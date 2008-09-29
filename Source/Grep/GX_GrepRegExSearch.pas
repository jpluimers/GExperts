unit GX_GrepRegExSearch;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils, Classes, RegExpr, GX_GenericUtils, GX_OtaUtils;

type
  TFoundEvent = procedure(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString) of object;

  TSearcher = class(TObject)
  private
    FPattern: TGXUnicodeString;
    FData: TGXUnicodeStringList;
    FOnFound: TFoundEvent;
    FRegEx: TRegExpr;
    FCaseSensitive: Boolean;
    FWholeWord: Boolean;
    FRegularExpression: Boolean;
    FFileName: string;
    procedure SearchLineRegEx(LineNo: Integer);
    procedure SearchLineRaw(LineNo: Integer);
    procedure SetFileName(const Value: string);
    function CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Pattern: TGXUnicodeString read FPattern write FPattern;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property RegularExpression: Boolean read FRegularExpression write FRegularExpression;
    property WholeWord: Boolean read FWholeWord write FWholeWord;
    property FileName: string read FFileName write SetFileName;
    procedure Execute; overload;
  end;

implementation

{ TSearcher }

function TSearcher.CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
var
  PrevChar: TGXUnicodeChar;
  NextChar: TGXUnicodeChar;
  FirstMatchChar: TGXUnicodeChar;
  LastMatchChar: TGXUnicodeChar;
begin
  PrevChar := ' ';
  NextChar := ' ';
  if StartCol >= 2 then
    PrevChar := Line[StartCol - 1];
  if Length(Line) > EndCol then
    NextChar := Line[EndCol + 1];
  FirstMatchChar := Line[StartCol];
  LastMatchChar := Line[EndCol];
  Result := (((not IsCharIdentifier(PrevChar)) or (not IsCharIdentifier(FirstMatchChar)))
    and ((not IsCharIdentifier(NextChar)) or (not IsCharIdentifier(LastMatchChar))));
end;

constructor TSearcher.Create;
begin
  FRegEx := TRegExpr.Create;
end;

destructor TSearcher.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TSearcher.Execute;
var
  i: Integer;
begin
  if not Assigned(FData) then
    raise Exception.Create('Data to search not provided');
  if IsEmpty(Pattern) then
    raise Exception.Create('Search pattern is empty');

  if RegularExpression then
  begin
    FRegEx.ModifierI := not CaseSensitive;
    FRegEx.Expression := Pattern;
    FRegEx.Compile;
  end;

  for i := 0 to FData.Count - 1 do
  begin
    if RegularExpression then
      SearchLineRegEx(i)
    else
      SearchLineRaw(i);
  end;
end;

procedure TSearcher.SearchLineRaw(LineNo: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
  StartIndex: Integer;
  MatchPos: Integer;

  procedure GetNextMatch;
  begin
    if CaseSensitive then
      MatchPos := PosFrom(Pattern, Line, StartIndex)
    else
      MatchPos := CaseInsensitivePosFrom(Pattern, Line, StartIndex);
  end;

begin
  Line := FData[LineNo];
  StartIndex := 1;
  GetNextMatch;
  while MatchPos > 0 do
  begin
    StartCol := MatchPos;
    EndCol := MatchPos + Length(Pattern) - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0);
    if (not WholeWord) or (CheckWholeWord(Line, StartCol, EndCol)) then
      if Assigned(FOnFound) then
        FOnFound(LineNo + 1, StartCol, EndCol, Line);
    StartIndex := StartCol + 1;
    GetNextMatch;
  end;
end;

procedure TSearcher.SearchLineRegEx(LineNo: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
begin
  Line := FData[LineNo];
  if FRegEx.Exec(Line) then repeat
  begin
    //if FRegEx.SubExprMatchCount > 0 then
    //  raise Exception.Create('Subexpression searches are not supported');
    StartCol := FRegEx.MatchPos[0];
    EndCol := StartCol + FRegEx.MatchLen[0] - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0);
    if WholeWord then
      if not CheckWholeWord(Line, StartCol, EndCol) then
        Continue;
    if Assigned(FOnFound) then
      FOnFound(LineNo + 1, StartCol, EndCol, Line);
  end until not FRegEx.ExecNext;
end;

procedure TSearcher.SetFileName(const Value: string);
begin
  FFileName := Value;
  if not Assigned(FData) then
    FData := TGXUnicodeStringList.Create
  else
    FData.Clear;
  GxOtaLoadFileToUnicodeStrings(FFileName, FData);
end;

end.

