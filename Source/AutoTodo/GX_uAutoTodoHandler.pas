///<summary>
/// This unit is based on the AutoTodoInserter wizard written by Peter Laman
/// and published on CodeCentral at
/// http://cc.embarcadero.com/codecentral/ccweb.exe/listing?id=22336
/// (Article on http://edn.embarcadero.com/article/32709)
///
/// The licence states "Commercial use requires permission", but I found no
/// way to contact him to get a permission. So for now I am assuming that he
/// has no objection.
/// @Peter: If you read this, please contact me through
/// http://blog.dummzeuch.de or https://plus.google.com/108684844040122144755 </summary>
unit GX_uAutoTodoHandler;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  TAutoTodoHandler = class
  private
    FTodoUser: string;
    FTextToInsert: string;
    function GenerateInsertText(const ProcName, BeginKey, EndKey, ProcType: string): string;
  public
    class function GetDefaultTextToInsert: string;
    class procedure GetPlaceholders(_sl: TStrings);
    constructor Create;
    destructor Destroy; override;
    ///<summary>
    /// Generates a list of string inserts for all empty
    /// begin/end blocks. The objects part contains the original source positions
    /// where to insert (0-based)  </summary>
    procedure Execute(const Source: string; List: TStrings);
    ///<summary>
    /// Adds to-do items for all empty begin/end blocks in the given string. Calls Execute. </summary>
    function Test(const Source: string): string;
    property TodoUser: string read FTodoUser write FTodoUser;
    property TextToInsert: string read FTextToInsert write FTextToInsert;
  end;

implementation

uses
  GX_GenericUtils;

const
  // Unfortunately this is strictly no longer true, because e.g. Umlauts can also be used as
  // identifiers, but most programmers will not do that.
  IdChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  SepChars = [#9, #32, #13, #10];

type
  TRoutine = record
    ProcType: string;
    PrevPos: Integer;
    NextPos: Integer;
    PrevName: string;
    NextName: string;
  end;

  TStringArray = array of string;

  TKeywordPair = record
    BeginKey: string;
    EndKeys: TStringArray;
  end;

  TKeywordPairArray = array of TKeywordPair;

{ TAutoTodoHandler }

class function TAutoTodoHandler.GetDefaultTextToInsert: string;
const
  DefaultTextToInsert = '//TODO 5 -o{UserName} -cEmpty Code Block: {ProcName} ({BeginKey}/{EndKey} in {ProcType})';
begin
  Result := DefaultTextToInsert;
end;

class procedure TAutoTodoHandler.GetPlaceholders(_sl: TStrings);
begin
  Assert(Assigned(_sl));
  _sl.Add('Username');
  _sl.Add('ProcName');
  _sl.Add('BeginKey');
  _sl.Add('EndKey');
  _sl.Add('ProcType');
end;

constructor TAutoTodoHandler.Create;
begin
  inherited;
  FTodoUser := GetCurrentUser;
  FTextToInsert := GetDefaultTextToInsert;
end;

destructor TAutoTodoHandler.Destroy;
begin
  inherited;
end;

procedure UnCommentSourceCode(var SourceCode: string; const ClearStrings: Boolean);
var
  i: Integer;
  Ch: Char;
  Prev: Char;
  State: (sCode, sAccolade, sParentAsterisk, sDoubleSlash, sQuotedString);
begin
  Prev := #0;
  State := sCode;
  for i := 1 to Length(SourceCode) do begin
    Ch := SourceCode[i];
    case State of
      sCode:
        case Ch of
          '{': begin
              State := sAccolade;
              SourceCode[i] := #32;
            end;
          '''': begin
              if ClearStrings then begin
                State := sQuotedString;
                SourceCode[i] := #32;
              end;
            end;
          '*': if Prev = '(' then begin
              State := sParentAsterisk;
              SourceCode[i] := #32;
              SourceCode[i - 1] := #32;
            end;
          '/': if Prev = '/' then begin
              State := sDoubleSlash;
              SourceCode[i] := #32;
              SourceCode[i - 1] := #32;
            end;
        end;
      sAccolade: begin
          if not CharInSet(SourceCode[i], [#13, #10]) then
            SourceCode[i] := #32;
          if Ch = '}' then
            State := sCode;
        end;

      sParentAsterisk: begin
          if not CharInSet(SourceCode[i], [#13, #10]) then
            SourceCode[i] := #32;
          if (Prev = '*') and (Ch = ')') then
            State := sCode;
        end;

      sDoubleSlash: begin
          if Ch = #13 then
            State := sCode
          else
            SourceCode[i] := #32;
        end;

      sQuotedString: begin
          SourceCode[i] := #32;
          if Ch = '''' then
            State := sCode;
        end;
    end;
    Prev := Ch;
  end; // for i
end;

function IsSubstrHere(const SubStr, s: string; idx: Integer): Boolean;
var
  Len: Integer;
  SubL: Integer;
  i: Integer;
begin
  i := 1;
  Len := Length(s);
  SubL := Length(SubStr);
  Result := True;
  while idx <= Len do begin
    if i > SubL then
      Exit;
    if SubStr[i] <> s[idx] then begin
      Result := False;
      Exit;
    end;
    Inc(idx);
    Inc(i);
  end;
  Result := False;
end;

function FindKeyPairBegin(const KeyPairs: TKeywordPairArray;
  const s: string; var StartPos: Integer; out PairIndex: Integer): Boolean;
var
  Lo: Integer;
  Hi: Integer;
  KeyIdx: Integer;
  Len: Integer;
begin
  Lo := Low(KeyPairs);
  Hi := High(KeyPairs);
  Len := Length(s);
  while StartPos <= Len do begin
    for KeyIdx := Lo to Hi do
      if IsSubstrHere(KeyPairs[KeyIdx].BeginKey, s, StartPos) then begin
        Result := True;
        PairIndex := KeyIdx;
        Exit;
      end;
    Inc(StartPos);
  end; // while StartPos <= L
  Result := False;
end;

function FindId(const Id, s: string; StartPos: Integer): Integer;
var
  Done: Boolean;
  After: Integer;
begin
  repeat
    Result := Pos(Id, Copy(s, StartPos, MaxInt));
    if Result > 0 then
      Inc(Result, StartPos - 1);
    After := Result + Length(Id);
    Done := (Result = 0)
      or ((Result = 1) or not CharInSet(s[Result - 1], IdChars))
      and ((After > Length(s)) or not CharInSet(s[After], IdChars));
    if not Done then
      StartPos := Result + 1;
  until Done;
end;

function FindKeyPairEnd(const KeyPairs: TKeywordPairArray;
  const s: string; var StartPos: Integer; out EndKeywordIndex: Integer;
  const PairIndex: Integer): Boolean;
var
  i: Integer;
  Len: Integer;
begin
  Len := Length(s);
  while StartPos <= Len do begin
    for i := 0 to Length(KeyPairs[PairIndex].EndKeys) - 1 do begin
      if IsSubstrHere(KeyPairs[PairIndex].EndKeys[i], s, StartPos) then begin
        Result := True;
        EndKeywordIndex := i;
        Exit;
      end;
    end;
    Inc(StartPos);
  end; // while StartPos <= L
  Result := False;
end;

function TAutoTodoHandler.GenerateInsertText(const ProcName, BeginKey, EndKey, ProcType: string): string;
begin
  Result := FTextToInsert;
  Result := StringReplace(Result, '{UserName}', FTodoUser, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{ProcName}', ProcName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{BeginKey}', BeginKey, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{EndKey}', EndKey, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{ProcType}', ProcType, [rfReplaceAll, rfIgnoreCase]);
end;

function TAutoTodoHandler.Test(const Source: string): string;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Execute(Source, sl);
    Result := Source;
    for i := sl.Count - 1 downto 0 do
      Insert(sl[i], Result, Integer(sl.Objects[i]) + 1);
  finally
    sl.Free;
  end;
end;

procedure TAutoTodoHandler.Execute(const Source: string; List: TStrings);
var
  LowSource: string;
  BegPos: Integer;
  Procs: array of TRoutine;
  KeyPairs: TKeywordPairArray;

  procedure CheckRoutine(var R: TRoutine);
  var
    i: Integer;
    j: Integer;
    Len: Integer;
  begin
    while R.NextPos < BegPos do begin
      R.PrevPos := R.NextPos;
      R.PrevName := R.NextName;
      R.NextPos := FindId(R.ProcType, LowSource, BegPos + 1);
      if R.NextPos = 0 then begin
        R.NextPos := MaxInt; // No further searching
        R.NextName := '';
        Exit;
      end;

      // We found a routine, determine its name
      Len := Length(LowSource);
      i := R.NextPos + Length(R.ProcType);
      while (i <= Len) and CharInSet(LowSource[i], SepChars) do
        Inc(i);
      j := i; // Start of routine name
      while (i <= Len) and CharInSet(LowSource[i], IdChars + ['.']) do
        Inc(i);

      // Take the name from the mixed case original source string }
      R.NextName := Copy(Source, j, i - j);
    end; // while R.NextPos < BegPos
  end; // CheckRoutine

  function GetIndentOf(const Pos: Integer): string;
  var
    Len: Integer;
    i: Integer;
  begin
    Len := Pos;
    while (Len > 0) and (LowSource[Len] <> #10) do
      Dec(Len);
    Len := (Pos - Len - 1);
    SetLength(Result, Len);
    for i := 1 to Len do
      Result[i] := #32;
  end; // GetIndentOf

  procedure AddToProcs(const ProcType: string);
  var
    idx: Integer;
  begin
    idx := Length(Procs);
    SetLength(Procs, idx + 1);
    Procs[idx].ProcType := ProcType;
    Procs[idx].PrevPos := 0;
    Procs[idx].NextPos := 0;
    Procs[idx].PrevName := '';
    Procs[idx].NextName := '';
  end;

  procedure AddToKeyPairs(const BeginKey, EndKey: string; const SecondEndKey: string = '');
  var
    idx: Integer;
  begin
    idx := Length(KeyPairs);
    SetLength(KeyPairs, idx + 1);
    KeyPairs[idx].BeginKey := BeginKey;
    if SecondEndKey = '' then
      SetLength(KeyPairs[idx].EndKeys, 1)
    else begin
      SetLength(KeyPairs[idx].EndKeys, 2);
      KeyPairs[idx].EndKeys[1] := SecondEndKey;
    end;
    KeyPairs[idx].EndKeys[0] := EndKey;
  end;

const
  SInherited = 'inherited;';
var
  EndPos: Integer;
  CodePos: Integer;
  FirstCR: Integer;
  LastCR: Integer;
  idx: Integer;
  EmptyBlock: Boolean;
  CurProc: Integer;
  Pri: Integer;
  KeyPairIndex: Integer;
  EndKeywordIndex: Integer;
  Todo: string;
begin // TAutoTodoHandler.Execute
  List.BeginUpdate;
  try
    List.Clear;
    LowSource := LowerCase(Source);
    UnCommentSourceCode(LowSource, True);
    BegPos := FindId('implementation', LowSource, 1);
    if BegPos = 0 then
      Exit; // There is no implementation part

    SetLength(Procs, 0);
    AddToProcs('class procedure');
    AddToProcs('class function');
    AddToProcs('procedure');
    AddToProcs('function');
    AddToProcs('constructor');
    AddToProcs('destructor');
    AddToProcs('class operator');

    SetLength(KeyPairs, 0);
    AddToKeyPairs('begin', 'end');
    AddToKeyPairs('try', 'finally', 'except');
    AddToKeyPairs('finally', 'end');
    AddToKeyPairs('except', 'end');
    AddToKeyPairs('repeat', 'until');
    AddToKeyPairs('then', 'else');
    AddToKeyPairs('of', 'end', 'else'); //Case of
    AddToKeyPairs('else', 'end');

    for Pri := Low(Procs) to High(Procs) do
      CheckRoutine(Procs[Pri]);
    Inc(BegPos);
    repeat
      if not FindKeyPairBegin(KeyPairs, LowSource, BegPos, KeyPairIndex) then
        Exit;
      if BegPos = 0 then
        Exit;

      // Determine the routine that owns the begin/end block
      for Pri := Low(Procs) to High(Procs) do
        CheckRoutine(Procs[Pri]);

      CodePos := BegPos + Length(KeyPairs[KeyPairIndex].BeginKey);
      EndPos := CodePos;
      if not FindKeyPairEnd(KeyPairs, LowSource, EndPos, EndKeywordIndex, KeyPairIndex) then
        Exit;

      // Check if this begin/end block is empty
      EmptyBlock := True;
      FirstCR := 0;
      LastCR := 0;
      idx := CodePos;
      while idx < EndPos do begin
        if CharInSet(Source[idx], ['i', 'I']) and
          SameText(Copy(Source, idx, Length(SInherited)), SInherited) then begin
          Inc(idx, Length(SInherited) - 1);
          if (idx < EndPos) and (Source[idx] = ';') then
            Inc(idx);
          CodePos := idx;
        end else if not CharInSet(Source[idx], SepChars) then begin
          EmptyBlock := False;
          break;
        end else if Source[idx] = #13 then begin
          if FirstCR = 0 then
            FirstCR := idx;
          LastCR := idx;
        end;
        Inc(idx);
      end; // while I < EndPos

      // If this is an empty block, insert the TODO
      if EmptyBlock then begin
        // Determine current routine
        CurProc := 0;
        for Pri := Succ(Low(Procs)) to High(Procs) do
          if Procs[Pri].PrevPos > Procs[CurProc].PrevPos then
            CurProc := Pri;

        Todo := GenerateInsertText(
          Procs[CurProc].PrevName,
          KeyPairs[KeyPairIndex].BeginKey,
          KeyPairs[KeyPairIndex].EndKeys[EndKeywordIndex],
          Procs[CurProc].ProcType);

        if FirstCR = 0 then begin
          // No line break between begin/end -> insert two
          Todo := #13#10
            + GetIndentOf(BegPos) + #32#32 + Todo + '???'#13#10
            + GetIndentOf(BegPos);
          idx := EndPos; // Insert position
        end else begin
          // There are line breaks between begin/end
          idx := LastCR; // Insert position
          Todo := GetIndentOf(BegPos) + #32#32 + Todo;

          if FirstCR = LastCR then
            Todo := #13#10 + Todo;
        end;
        List.AddObject(Todo, TObject(idx - 1));
      end; // if EmptyBlock
      BegPos := CodePos;
    until False;
  finally
    List.EndUpdate;
  end;
end; // TAutoTodoHandler.Execute

end.
