unit GX_eFindDelimiter;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ToolsAPI, GX_EditorExpert;

type
  TSourceLanguage = (ltPas, ltCpp);

type
  TBaseDelimiterExpert = class(TEditorExpert)
  private
    LastActionBackward: Boolean;
    ThreeDelimiters: Boolean;
  public
    procedure DoDelimiterAction(Editor: IOTASourceEditor;
                                Offset: Integer;
                                SChar, EChar: TOTACharPos); virtual; abstract;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

  TLocateDelimiter = class(TBaseDelimiterExpert)
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure DoDelimiterAction(Editor: IOTASourceEditor;
                                Offset: Integer;
                                SChar, EChar: TOTACharPos); override;
    function GetHelpString: string; override;
  end;

type
  TMoveToDelimiter = class(TBaseDelimiterExpert)
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure DoDelimiterAction(Editor: IOTASourceEditor;
                                Offset: Integer;
                                SChar, EChar: TOTACharPos); override;
    function GetHelpString: string; override;
  end;

implementation

uses
  SysUtils, Windows, Dialogs,
  mwPasTokenList, mwBCBTokenList,
  GX_EditReader, GX_GenericUtils, GX_OtaUtils;

resourcestring
  SDelimiterHelpPrefix =
    '  This expert enables you to quickly ';

  SDelimiterHelpSuffix =  ' a matching beginning/ending delimiter for the following Delphi '+
   'tokens: begin, try, case, repeat, for, with, while, asm, do, if, then, else, class, record, '+
   'array, interface, implementation, uses, private, protected, public, published, until, '+
   'end, try, finally, except, (/), and [/], .' +sLineBreak+
   '  It also supports the following C++ tokens: {/}, (/), and [/]';

  SDelimiterMoveToMessage = 'move to';
  SDelimiterLocateMessage = 'locate';

  SDelimiterUsage =
    sLineBreak +
    'The following steps are taken to match delimiters:' + sLineBreak +
    ' - Beginning at the current cursor position, the expert looks to the left '+
    'on the current line for a delimiter token such as "begin".'+ sLineBreak +
    ' - If a delimiter token is found, the expert scans for the matching '+
    'token, such as "end" in the above instance.' + sLineBreak +
    ' - If no delimiter is found to the left, as in the case "if|(i=0)" (where ''|'' represents the cursor), the ' +
    'expert attempts to find a delimiter token to the right of the current '+
    'cursor position. In the mentioned example, this will identify the opening '+
    'parenthesis, and an attempt is made to locate the closing parenthesis.';

  SNotValidIdentifier = '"%s" is not a supported delimiter.';
  SNoMatchingEndFound = 'No matching closing delimiter was found.';

{ TBaseDelimiterExpert }

function GetFileContent(out FileContent: string; const FileName: string;
  var EditorLine: string): Integer;
var
  EditRead: TEditReader;
  CharPos: TOTACharPos;
begin
  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData.
  EditRead := TEditReader.Create(FileName);
  try
    FileContent := EditRead.GetText;

    CharPos :=EditRead.GetCurrentCharPos;
    Result := LinePosToCharPos(Point(CharPos.CharIndex + 1, CharPos.Line), FileContent);
  finally
    FreeAndNil(EditRead);
  end;
end;

{ TODO 4 -oAnyone -cCleanup:
    TBaseDelimiterExpert.Execute method is far too complex.
    It needs to be broken up into some more manageable chunks. }

procedure TBaseDelimiterExpert.Execute(Sender: TObject);
resourcestring
  SPasOrCFilesOnly = 'This expert is for use in .pas, .dpr, .inc, .cpp, .c, .h or .hpp files only';
  SCouldNotGetSourceBuffer = 'Could not get source code from editor buffer. Is a special selection active?';
const
  Offsets: array[0..2] of Integer=(0, -1, 1);
var
  FileContent: string;
  C: Integer;

  SPos: Integer;
  EPos: Integer;
  Point: TPoint;
  SChar: TOTACharPos;
  EChar: TOTACharPos;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;

  TextToken: string;
  EditorLine: string;
  FileName: string;

  Language: TSourceLanguage;
  Backward: Boolean;
  Offset: Integer;

  function ExecutePas: Boolean;
  var
    I: Integer;
    Parser: TPasTokenList;
    StartToken: TTokenKind;
    ExtraStartTokens: set of TTokenKind;
    EndTokens: set of TTokenKind;
  begin
    ExtraStartTokens := [];
    Result := False;
    Parser := TPasTokenList.Create;
    try
      Parser.SetOrigin(@FileContent[1], Length(FileContent));
      if Parser.Origin = nil then
      begin
        MessageDlg(SCouldNotGetSourceBuffer, mtError, [mbOK], 0);
        Exit;
      end;

      for i := 0 to 2 do
      begin
        // We try current token, previous token and finally next token
        Parser.RunIndex := Parser.PositionToIndex(SPos) + Offsets[i];
        StartToken := Parser.RunID;
        case Parser.RunID of
          tkBegin, tkCase, tkAsm:
            begin
              EndTokens := [tkEnd];
              Backward := False;
              Break;
            end;

          tkTry:
            begin
              if ThreeDelimiters then
                EndTokens := [tkFinally, tkExcept]
              else
                EndTokens := [tkEnd];

              Backward := False;
              Break;
            end;

          tkWith, tkWhile, tkThen, tkDo, tkIf, tkElse, tkFor:
            begin
              EndTokens := []; // We figure this out later based on context
              Backward := False;
              Break;
            end;

          tkFinally, tkExcept:
            if LastActionBackward then
            begin
              EndTokens := [tkTry];
              if Parser.RunID = tkExcept then
                ExtraStartTokens := [tkFinally]
              else
                ExtraStartTokens := [tkExcept];
              Backward := True;
              Break;
            end
            else
            begin
              EndTokens := [tkEnd];
              Backward := False;
              Break;
            end;

          tkRoundOpen:
            begin
              EndTokens := [tkRoundClose];
              Backward := False;
              Break;
            end;

          tkSquareOpen:
            begin
              EndTokens := [tkSquareClose];
              Backward := False;
              Break;
            end;

          tkRepeat:
            begin
              EndTokens := [tkUntil];
              Backward := False;
              Break;
            end;

          tkClass, tkRecord:
            begin
              Parser.NextNonJunk;
              if Parser.RunID in [tkSemicolon, tkOf] then // Forward declaration or class of Foo;
                EndTokens := [tkSemicolon]
              else
                EndTokens := [tkEnd];
              Parser.PreviousNonJunk;
              Backward := False;
              Break;
            end;

          tkArray:
          begin
            Backward := False;
            EndTokens := [tkConst, tkSemiColon, tkRoundClose];
            Break;
          end;

          tkInterface:
            begin
              Backward := False;

              Parser.NextNonJunk;
              if Parser.RunID = tkSemicolon then // Forward declaration
              begin
                EndTokens := [tkSemicolon];
                Parser.PreviousNonJunk;
              end
              else
              begin
                Parser.PreviousNonJunk; // Back to the interface
                Parser.PreviousNonJunk; // Previous token
                if Parser.RunID = tkEqual then // Interface declaration
                  EndTokens := [tkEnd]
                else
                  EndTokens := [tkImplementation]; // interface section
                Parser.NextNonJunk;
              end;
              Break;
            end;

          tkImplementation:
            begin
              EndTokens := [tkInterface];
              Backward := True;
              Break;
            end;

          tkUses:
            begin
              EndTokens := [tkSemiColon];
              Backward:= False;
              Break;
            end;

          tkEnd:
            begin
              if ThreeDelimiters then
                EndTokens := [tkBegin, tkCase, tkRecord, tkClass, tkAsm, tkFinally, tkExcept]
              else
                EndTokens := [tkBegin, tkCase, tkRecord, tkClass, tkAsm, tkTry];

              Backward := True;
              Break;
            end;

          tkPrivate, tkProtected, tkPublic, tkPublished:
            begin
              EndTokens := [tkEnd, tkPrivate, tkProtected, tkPublic, tkPublished];
              Backward := False;
              Break;
            end;

          tkRoundClose:
            begin
              EndTokens := [tkRoundOpen];
              Backward := True;
              Break;
            end;

          tkSquareClose:
            begin
              EndTokens := [tkSquareOpen];
              Backward := True;
              Break;
            end;

          tkUntil:
            begin
              EndTokens := [tkRepeat];
              Backward := True;
              Break;
            end;
          else
            if i = 0 then begin
              TextToken := Parser.RunToken;
              if TextToken = CRLF then
                TextToken := '<cr><lf>'
              else
                TextToken := HackBadEditorStringToNativeString(TextToken)
            end else if i = 2 then
            begin
              MessageDlg(Format(SNotValidIdentifier, [TextToken]), mtError, [mbOK], 0);
              Exit;
            end;
        end; // case
      end; // for

      c := 1;

      if Backward then
      begin
        SPos := Parser.RunPosition + Length(Parser.RunToken);

        while (c > 0) and (Parser.RunIndex > 0) do
        begin
          Parser.PreviousNonJunk;
          if (Parser.RunID = StartToken) or (Parser.RunID in ExtraStartTokens) then
            Inc(c);
          if Parser.RunID in EndTokens then
          begin
            if StartToken = tkImplementation then
            begin
              Parser.PreviousNonJunk;
              if Parser.RunID <> tkEqual then
                Dec(c);
              Parser.NextNonJunk;
            end
            else
              Dec(c);
          end;
        end;

        EPos := Parser.RunPosition;
        Offset := 0;
      end
      else // Forward scan
      begin
        SPos := Parser.RunPosition;

        // If/then/else jump to the end of the if/else action statement or the next matching else
        // TODO 3 -cBug -oAnyone: Nested else/if without a begin/end can confuse the parser here
        if StartToken in [tkIf, tkThen, tkElse] then
        begin
          // An else can not stop at itself
          if Parser.RunID = tkElse then
            Parser.Next;
          while (not (Parser.RunID in [tkElse, tkSemicolon, tkBegin, tkCase, tkTry, tkNull])) and (EndTokens = []) do
          begin
            Parser.NextNonJunk;
            case Parser.RunID  of
              tkNull:
                Exit;
              tkBegin, tkCase, tkTry:
                EndTokens := [tkEnd];
              tkSemiColon:
                begin
                  EndTokens := [tkSemiColon];
                  Parser.Previous; // Since this is the terminator, skip back
                end;
              tkElse:
                begin
                  EndTokens := [tkElse];
                  Parser.Previous;
                end;
            end;
          end;
        end;

        // A with/while/do/for might have to scan from there to tkEnd or tkSemicolon
        if StartToken in [tkWith, tkWhile, tkDo, tkFor] then
        begin
          while (not (Parser.RunID in [tkSemicolon, tkBegin, tkCase, tkTry, tkNull])) and (EndTokens = []) do
          begin
            Parser.NextNonJunk;
            case Parser.RunID  of
              tkNull:
                Exit;
              tkBegin, tkCase, tkTry:
                EndTokens := [tkEnd];
              tkSemiColon:
                begin
                  EndTokens := [tkSemiColon];
                  Parser.Previous; // Since this is the terminator, skip back
                end;
            end;
          end;
        end;

        while (c > 0) and (Parser.RunID <> tkNull) do
        begin
          Parser.NextNonJunk;
          case StartToken of
            tkBegin, tkCase, tkExcept, tkFinally, tkWith, tkWhile, tkThen, tkDo, tkIf, tkElse, tkFor, tkRecord:
              begin
                if Parser.RunID in [tkBegin, tkTry, tkCase, tkAsm] then
                  Inc(c);
              end;

            tkTry:
              begin
                if ThreeDelimiters then
                begin
                  if Parser.RunID = StartToken then
                    Inc(c);
                end
                else if Parser.RunID in [tkBegin, tkTry, tkCase, tkAsm] then
                  Inc(c);
              end;

            tkRoundOpen, tkSquareOpen, tkRepeat:
              begin
                if Parser.RunID = StartToken then
                  Inc(c);
              end;

            tkArray:
              begin
                if Parser.RunID = tkRoundOpen then
                  Inc(c);
              end;
          end;

          if Parser.RunID in EndTokens then
          begin
            if StartToken = tkArray then
            begin
              if C > 1 then
              begin
                if not (Parser.RunID in [tkSemicolon, tkConst]) then
                  Dec(c);
              end
              else
                Dec(c); // End the checks
            end
            else
              Dec(c);
          end;
        end;

        if Parser.RunID = tkNull then
          Exit;
        EPos := Parser.RunPosition + Length(Parser.RunToken);
        Offset := Length(Parser.RunToken);
      end;

      if c = 0 then
      begin
        if ThreeDelimiters then
          LastActionBackward := Backward
        else
          LastActionBackward := not LastActionBackward;
      end;

      Result := True;
    finally
      FreeAndNil(Parser);
    end;
  end;

  function ExecuteCpp: Boolean;
  var
    I: Integer;
    CParser: TBCBTokenList;
    CStartToken: TCTokenKind;
    CEndToken: TCTokenKind;
  begin
    Result := False;
    CEndToken := ctknull;
    CParser := TBCBTokenList.Create;
    try
      CParser.SetOrigin(@FileContent[1], Length(FileContent));
      if CParser.Origin = nil then
      begin
        MessageDlg(SCouldNotGetSourceBuffer, mtError, [mbOK], 0);
        Exit;
      end;

      for i := 0 to 2 do
      begin
        // We try current token, previous token and finally next token
        CParser.RunIndex := CParser.PositionToIndex(SPos) + Offsets[i];
        CStartToken := CParser.RunID;

        case CParser.RunID of
          ctkbraceopen:
            begin
              CEndToken := ctkbraceclose;
              Break;
            end;
          ctkroundopen:
            begin
              CEndToken := ctkroundclose;
              Break;
            end;
          ctksquareopen:
            begin
              CEndToken := ctksquareclose;
              Break;
            end;
          ctkbraceclose:
            begin
              CEndToken := ctkbraceopen;
              Break;
            end;
          ctkroundclose:
            begin
              CEndToken := ctkroundopen;
              Break;
            end;
          ctksquareclose:
            begin
              CEndToken := ctksquareopen;
              Break;
            end;
          else
            if i = 0 then
              TextToken := HackBadEditorStringToNativeString(CParser.RunToken)
            else if i = 2 then
            begin
              MessageDlg(Format(SNotValidIdentifier, [TextToken]), mtError, [mbOK], 0);
              Exit;
            end;
        end; // case
      end;

      c := 1;

      if CParser.RunID in [ctkbraceopen, ctkroundopen, ctksquareopen] then
      begin
        SPos := CParser.RunPosition;

        while (c > 0) and (CParser.RunID <> ctknull) do
        begin
          if CParser.RunIndex = CParser.Count - 1 then
            Break;
          CParser.Next;
          if CStartToken = CParser.RunID then
            Inc(c);
          if CParser.RunID = CEndToken then
            Dec(c);
        end;

        EPos := CParser.RunPosition + Length(CParser.RunToken);
        Offset := Length(CParser.RunToken);
      end
      else
      begin
        SPos := CParser.RunPosition + Length(CParser.RunToken);

        while (c > 0) and (CParser.RunIndex > 0 )do
        begin
          CParser.Previous;
          if CStartToken = CParser.RunID then
            Inc(c);
          if CParser.RunID = CEndToken then
            Dec(c);
        end;

        EPos := CParser.RunPosition;
        Offset := 0;
      end;

      Result := True;
    finally
      FreeAndNil(CParser);
    end;
  end;

begin
  FileName := GxOtaGetTopMostEditBufferFileName;

  if IsCpp(FileName) or IsC(FileName) or IsH(FileName) then
    Language := ltCpp
  else if IsDprOrPas(FileName) or IsInc(FileName) then
    Language := ltPas
  else
    raise Exception.Create(SPasOrCFilesOnly);

  Module := GxOtaGetCurrentModule;
  SourceEditor := GxOtaGetCurrentSourceEditor;
  if (not Assigned(SourceEditor)) or (not Assigned(Module)) then
    Exit;

  SPos := GetFileContent(FileContent, SourceEditor.FileName, EditorLine);

  if Language = ltPas then
  begin
    if not ExecutePas then
      Exit;
  end
  else
    if not ExecuteCpp then
      Exit;

  if c <> 0 then
  begin
    MessageDlg(SNoMatchingEndFound, mtInformation, [mbOK], 0);
    Exit;
  end;

  // A matching delimiter was found
  Point := CharPosToLinePos(SPos + 1, FileContent);
  SChar.Line := Point.Y;
  SChar.CharIndex := Point.X - 1;
  Point := CharPosToLinePos(EPos + 1, FileContent);
  EChar.Line := Point.Y;
  EChar.CharIndex := Point.X - 1;
  DoDelimiterAction(SourceEditor, Offset, SChar, EChar);
end;

function TBaseDelimiterExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TMoveToDelimiter }

constructor TMoveToDelimiter.Create;
begin
  inherited Create;
  ThreeDelimiters := True;
end;

procedure TMoveToDelimiter.DoDelimiterAction(Editor: IOTASourceEditor;
  Offset: Integer; SChar, EChar: TOTACharPos);
var
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView(Editor);

  EditView.ConvertPos(False, EditPos, EChar);

  EditPos.Col := EditPos.Col - Offset;
  if EditPos.Col < 1 then
    EditPos.Col := 1;

  EditView.CursorPos := EditPos;
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

function TMoveToDelimiter.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + VK_RIGHT;
end;

function TMoveToDelimiter.GetDisplayName: string;
resourcestring
  SMoveToExpertName = 'Move to Matching Delimiter';
begin
  Result := SMoveToExpertName;
end;

function TMoveToDelimiter.GetHelpString: string;
begin
  Result := SDelimiterHelpPrefix + SDelimiterMoveToMessage +
    SDelimiterHelpSuffix + SDelimiterUsage;
end;

class function TMoveToDelimiter.GetName: string;
begin
  Result := 'MoveToDelimiter';
end;

{ TLocateDelimiter }

constructor TLocateDelimiter.Create;
begin
  inherited Create;
  ThreeDelimiters := False;
end;

procedure TLocateDelimiter.DoDelimiterAction(Editor: IOTASourceEditor;
  Offset: Integer; SChar, EChar: TOTACharPos);
var
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView(Editor);
  EditView.ConvertPos(False, EditPos, SChar);
  EditView.CursorPos := EditPos;

  if (EChar.Line > SChar.Line) or ((EChar.Line = SChar.Line) and (EChar.CharIndex > SChar.CharIndex)) then
    GxOtaSelectBlock(Editor, SChar, EChar)
  else
    GxOtaSelectBlock(Editor, EChar, SChar);
end;

function TLocateDelimiter.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + VK_LEFT;
end;

function TLocateDelimiter.GetDisplayName: string;
resourcestring
  SLocateDelimiterName = 'Locate Matching Delimiter';
begin
  Result := SLocateDelimiterName;
end;

function TLocateDelimiter.GetHelpString: string;
begin
  Result := SDelimiterHelpPrefix + SDelimiterLocateMessage +
    SDelimiterHelpSuffix + SDelimiterUsage;
end;

class function TLocateDelimiter.GetName: string;
begin
  Result := 'LocateDelimiter';
end;

initialization
  RegisterEditorExpert(TLocateDelimiter);
  RegisterEditorExpert(TMoveToDelimiter);
end.

