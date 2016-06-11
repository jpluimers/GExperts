// Editor expert for expanding a macro template
unit GX_MacroTemplatesExpert;

interface

uses
  Classes, ToolsAPI, GX_EditorExpert, GX_MacroTemplates,
  GX_MacroFile, GX_ConfigurationInfo, GX_Actions, GX_MacroExpandNotifier;

type
  TMacroTemplatesExpert = class;

  TExpandTemplateSettings = class(TTemplateSettings)
  public
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Reload; override;
  end;

  TInsertTemplateEvent = procedure(Sender: TObject; const ACode: string) of object;

  TTemplateShortCut = class
  protected
    FAction: IGxAction;
    FCode: string;
    FOnExecute: TInsertTemplateEvent;
    FOnUpdate: TNotifyEvent;
    procedure HandleExecute(Sender: TObject);
    procedure HandleUpdate(Sender: TObject);
    procedure SetAction(Value: IGxAction);
  public
    destructor Destroy; override;
    property Code: string read FCode write FCode;
    property Action: IGxAction read FAction write SetAction;
    property OnExecute: TInsertTemplateEvent read FOnExecute write FOnExecute;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TMacroTemplatesExpert = class(TEditorExpert)
  private
    FMacroFile: TMacroFile;
    FSettings: TExpandTemplateSettings;
    FShortCuts: TList;
    FExpandNotifier: IMacroExpandNotifier;
    procedure AddUsesToUnit(ATemplateObject: TMacroObject);
    function AddLeadingSpaces(const AString: string;
      ASpacesCount: Integer; AInsertCol: Integer): string;
    function PrefixLines(const Lines, Prefix: string; AStartLine: Integer = 1): string;
    function PrepareWhitePrefix(AInsertCol: Integer): string;
    procedure PrepareNewCursorPos(var VTemplateText: string;
      AInsertPos: TOTAEditPos; var VNewPos: TOTAEditPos; ConsiderPipe: Boolean);
    procedure ClearTemplShortCuts;
    procedure AddTemplShortCut(const AName: string; AShortCut: TShortCut);
    procedure TemplateActionExecute(Sender: TObject; const ACode: string);
    function OffsetToEditPos(AOffset: Integer): TOTAEditPos;
    procedure PrepareTemplateText(var VText: string;
      ACurrentPos: TOTAEditPos; AAfterLen: Integer;
      var InsertOffset: Integer; var InsertPos: TOTAEditPos);
    procedure HandleExpandTemplate(const AName, AText: string; var Accepted: Boolean);
    procedure RegisterExpandNotifier;
    procedure RemoveExpandNotifier;
    procedure ConfigAutoExpand;
  protected
    function GetTemplate(const ATemplateName: string; APromptForUnknown: Boolean = True): Integer;
    procedure SetNewCursorPos(ANewEditPos: TOTAEditPos);
    function CalculateNewCursorPos(const AString: string; ACursorPos: TOTAEditPos): TOTAEditPos;
    procedure CalculateInsertPos(AInsertOption: TTemplateInsertPos;
      var VInsertOffset: Integer; var VInsertPos: TOTAEditPos);
    function GetProgrammerInfo(var VInfo: TProgrammerInfo): Boolean;
    function CharPosToEditPos(ACharPos: TOTACharPos): TOTAEditPos;
    procedure LoadTemplates;
    procedure ExpandTemplate(const AForcedCode: string = '');
    procedure CreateDefaultTemplates;
    procedure SetAutoExpandActive(Value: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;

    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;

    procedure ReloadSettings;
    property Settings: TExpandTemplateSettings read FSettings;
    property MacroFile: TMacroFile read FMacroFile;
  end;

function GetExpandMacroExpert: TMacroTemplatesExpert;
function GetExpandMacroExpertReq: TMacroTemplatesExpert;

implementation

uses
  SysUtils, Forms, Dialogs,
  GX_GenericUtils, GX_MacroParser, GX_OtaUtils,
  GX_ActionBroker, GX_MacroSelect, GX_UsesManager;

resourcestring
  SFormOpenError = 'Config window already open ! (minimized ?)'#10 +
    'Cannot open it this way';

type
  TMacroDetails = record
    Name: string;
    Desc: string;
    ImplUnit: string;
    InsertPos: TTemplateInsertPos;
    Text: string;
  end;

const
  CommonConfigurationKey = 'Common';
  ShortCutIdent = 'ShortCut';
  // Registry sections
  ProgrammerIDKey = 'ProgammerID';
  ProgrammerNameIdent = 'ProgrammerName';
  ProgrammerInitialsIdent = 'ProgrammerInitials';
  ExpandWithCharIdent = 'ExpandWithChar';
  ExpandDelayIdent = 'ExpandDelay';

{$IFDEF GX_BCB}
  CommentBegin = '/*';
  CommentEnd = '*/';
{$ELSE GX_DELPHI}
  CommentBegin = '{';
  CommentEnd = '}';
{$ENDIF GX_DELPHI}

  DefaultMacros: array[0..10] of TMacroDetails = (
    ( // 0
      Name: '_';
      Desc: 'embed _() for gettext()';
      ImplUnit: 'gnugettext';
      InsertPos: tipCursorPos;
      Text:
         '_(%SELECTION%|)';
    ),
    ( // 1
      Name: 'ph';
      Desc: 'Procedure Header';
      ImplUnit: '';
      InsertPos: tipLineStart;
      Text:
        CommentBegin + '-----------------------------------------------------------------------------' +sLineBreak+
        '  Procedure: %PROCNAME%' +sLineBreak+
        '  Author:    %USER%' +sLineBreak+
        '  Date:      %DAY%-%MONTHSHORTNAME%-%YEAR%' +sLineBreak+
        '  Arguments: %ARGUMENTS%' +sLineBreak+
        '  Result:    %RESULT%' +sLineBreak+
        '-----------------------------------------------------------------------------' + CommentEnd + sLineBreak + sLineBreak;
    ),
    ( // 2
      Name: 'uh';
      Desc: 'Unit Header';
      ImplUnit: '';
      InsertPos: tipUnitStart;
      Text:
        CommentBegin + '-----------------------------------------------------------------------------' +sLineBreak+
        ' Unit Name: %UNIT%' +sLineBreak+
        ' Author:    %USER%' +sLineBreak+
        ' Date:      %DAY%-%MONTHSHORTNAME%-%YEAR%' +sLineBreak+
        ' Purpose:' +sLineBreak+
        ' History:' +sLineBreak+
        '-----------------------------------------------------------------------------' + CommentEnd + sLineBreak + sLineBreak;
    ),
    ( // 3
      Name: 'sd';
      Desc: 'Send Debug Message';
      ImplUnit: 'DbugIntf';
      InsertPos: tipCursorPos;
      Text: '{$IFOPT D+} SendDebug(''|''); {$ENDIF}';
    ),
    ( // 4
      Name: 'sdme';
      Desc: 'Send Debug Method Enter/Exit';
      ImplUnit: 'DbugIntf';
      InsertPos: tipLineStart;
      Text:
        '  %BEFORE%SendMethodEnter(''%METHODCLASS%'');' + sLineBreak +
        '  try' + sLineBreak +
        '    |' + sLineBreak +
        '  finally' + sLineBreak +
        '    SendMethodExit(''%METHODCLASS%'');' + sLineBreak +
        '  end;' + sLineBreak;
    ),
    ( // 5
      Name: 'xdoc';
      Desc: 'XML Method Documentation';
      InsertPos: tipCursorPos;
      Text:
        '/// <summary>' + sLineBreak +
        '/// |' + sLineBreak +
        '/// </summary>' + sLineBreak +
        '/// %BEGINPARAMLIST%<param name="%PARAMNAME%"></param>' + sLineBreak +
        '/// %ENDPARAMLIST%<returns>%RESULT%</returns>';
    ),
    ( // 6
      Name: 'begin';
      Desc: 'Embed begin/end';
      InsertPos: tipCursorPos;
      Text:
         'begin' + sLineBreak +
         '  %SELECTION%|' + sLineBreak +
         'end;';
    ),
    ( // 7
      Name: 'tryf';
      Desc: 'Embed try/finally';
      InsertPos: tipCursorPos;
      Text:
        'try' + sLineBreak +
        '  %SELECTION%|' + sLineBreak +
        'finally' + sLineBreak +
        'end;';
    ),
    ( // 8
      Name: 'for';
      Desc: 'For loop (basic)';
      InsertPos: tipCursorPos;
      Text: 'for | := 0 to  do';
    ),
    ( // 9
      Name: 'fori';
      Desc: 'For loop with counter/embedding';
      InsertPos: tipCursorPos;
      Text:
        'for i := 0 to |.Count - 1 do' + sLineBreak +
        'begin' + sLineBreak +
        '  %SELECTION%' + sLineBreak +
        'end;';
    ),
    ( // 10
      Name: 'while';
      Desc: 'Embed while/do/begin/end loop';
      InsertPos: tipCursorPos;
      Text:
        'while | do' + sLineBreak +
        'begin' + sLineBreak +
        '  %SELECTION%' + sLineBreak +
        'end;';
    )
    );


var
  GlobalExpandMacroExpert: TMacroTemplatesExpert = nil;

procedure RegisterExpandMacroExpert(AExpert: TMacroTemplatesExpert);
begin
  if GlobalExpandMacroExpert = nil then
    GlobalExpandMacroExpert := AExpert;
end;

procedure UnregisterExpandMacroExpert(AExpert: TMacroTemplatesExpert);
begin
  if GlobalExpandMacroExpert = AExpert then
    GlobalExpandMacroExpert := nil;
end;

function GetExpandMacroExpert: TMacroTemplatesExpert;
begin
  Result := GlobalExpandMacroExpert;
end;

function GetExpandMacroExpertReq: TMacroTemplatesExpert;
resourcestring
  SNoExpandMacroExpert = 'Macro Templates expert not found !';
begin
  Result := GetExpandMacroExpert;
  if Result = nil then
    raise Exception.Create(SNoExpandMacroExpert);
end;

// Replaces a template in editor's buffer with a given template code
procedure InsertTemplateIntoEditor(const ATemplateName, ATemplateCode: string;
  ATemplateOffset, AInsertOffset: Integer);
var
  CodeLen: Integer;
  InsertOff: Integer;
begin
  CodeLen := Length(ATemplateName);
  if ATemplateOffset = AInsertOffset then
  begin
    InsertOff := AInsertOffset + CodeLen;
    GxOtaInsertTextIntoEditorAtPos(ATemplateCode, InsertOff);
    // Delete template code (done after the insert to avoid problems)
    if CodeLen > 0 then
      GxOtaDeleteTextFromPos(ATemplateOffset, CodeLen);
  end
  else
  begin
    InsertOff := AInsertOffset;
    if CodeLen > 0 then
      GxOtaDeleteTextFromPos(ATemplateOffset, CodeLen);
    GxOtaInsertTextIntoEditorAtPos(ATemplateCode, InsertOff);
  end;
end;

destructor TTemplateShortCut.Destroy;
begin
  FAction := nil;
  inherited;
end;

procedure TTemplateShortCut.HandleExecute(Sender: TObject);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Sender, FCode);
end;

procedure TTemplateShortCut.HandleUpdate(Sender: TObject);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Sender);
end;

procedure TTemplateShortCut.SetAction(Value: IGxAction);
begin
  FAction := Value;
  if Assigned(FAction) then
  begin
    FAction.OnExecute := HandleExecute;
    FAction.OnUpdate := HandleUpdate;
  end;
end;

{ TMacroTemplatesExpert }

constructor TMacroTemplatesExpert.Create;
begin
  Assert(GlobalExpandMacroExpert = nil, 'Expand Macro Expert already created!');
  inherited Create;

  RegisterExpandMacroExpert(Self);
  RegisterProgrammerInfoProc(Self.GetProgrammerInfo);

  FShortCuts := TList.Create;

  FSettings := TExpandTemplateSettings.Create(Self);
  try
    FSettings.FMacroFileName := BuildFileName(ConfigInfo.ConfigPath, DefaultMacroFileName);

    FMacroFile := TMacroFile.Create;
    try
      FMacroFile.FileName := FSettings.MacroFileName;
      LoadTemplates;
    except
      FreeAndNil(FMacroFile);
      raise;
    end;
  except
    FreeAndNil(FSettings);
    raise;
  end;
end;

destructor TMacroTemplatesExpert.Destroy;
begin
  RemoveExpandNotifier;
  UnregisterExpandMacroExpert(Self);
  FreeAndNil(FSettings);
  FreeAndNil(FMacroFile);
  ClearTemplShortCuts;
  FreeAndNil(FShortCuts);
  inherited Destroy;
end;

procedure TMacroTemplatesExpert.Configure;
var
  fmMacroTemplates: TfmMacroTemplates;
begin
  if FSettings.Form <> nil then
    MessageDlg(SFormOpenError, mtInformation, [mbOk], 0)
  else
  begin
    fmMacroTemplates := TfmMacroTemplates.Create(nil);
    try
      ConvertBitmapToIcon(GetBitmap, fmMacroTemplates.Icon);
      fmMacroTemplates.Settings := FSettings;
      fmMacroTemplates.ShowModal;
    except // Free only on error (we free on close otherwise)
      FreeAndNil(fmMacroTemplates);
      raise;
    end;
  end;
end;

// Load the saved macro templates
procedure TMacroTemplatesExpert.LoadTemplates;
var
  i: Integer;
begin
  FMacroFile.LoadFromFile;
  if FMacroFile.MacroCount < 1 then
    CreateDefaultTemplates;
  ClearTemplShortCuts;
  for i := 0 to FMacroFile.MacroCount - 1 do
    if FMacroFile[i].ShortCut <> EmptyShortCut then
      AddTemplShortCut(FMacroFile[i].Name, FMacroFile[i].ShortCut);
end;

procedure TMacroTemplatesExpert.ClearTemplShortCuts;
var
  Obj: TObject;
begin
  while FShortCuts.Count > 0 do
  begin
    Obj := FShortCuts[FShortCuts.Count - 1];
    FShortCuts.Delete(FShortCuts.Count - 1);
    FreeAndNil(Obj);
  end;
end;

procedure TMacroTemplatesExpert.AddTemplShortCut(const AName: string; AShortCut: TShortCut);
const
  EditorExpertPrefix = 'EditorExpert';   // Do not localize.
  TemplateActionPrefix = 'Macro template: ';
var
  NewActionName: string;
  NewGxAction: IGxAction;
  NewShortCut: TTemplateShortCut;
begin
  if AName = '' then
    Exit;

  NewActionName := EditorExpertPrefix + GetName + AName;

  NewGxAction := GxActionBroker.RequestAction(NewActionName, GetBitmap);
  NewGxAction.Caption := TemplateActionPrefix + AName;
  NewGxAction.ShortCut := AShortCut;

  NewShortCut := TTemplateShortCut.Create;
  NewShortCut.Code := AName;
  NewShortCut.Action := NewGxAction;
  NewShortCut.OnExecute := TemplateActionExecute;
  NewShortCut.OnUpdate := ActionOnUpdate;

  FShortCuts.Add(NewShortCut);
end;

procedure TMacroTemplatesExpert.TemplateActionExecute(Sender: TObject; const ACode: string);
begin
  ExpandTemplate(ACode);
end;

// Prepare white-space prefix for a template that is not inserted into
// the first column.  AInsertCol is one-based.
function TMacroTemplatesExpert.PrepareWhitePrefix(AInsertCol: Integer): string;
var
  EditView: IOTAEditView;
  Idx: Integer;
begin
  Result := '';
  EditView := GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;

  Result := GxOtaGetPreceedingCharactersInLine(EditView);

  // Find the token start, if there is one under the cursor
  Idx := AInsertCol - 1;

  Result := Copy(Result, 1, Idx);

  // Peplace all non-whitespace characters
  for Idx := 1 to Length(Result) do
    if not (IsCharWhiteSpace(Result[Idx])) then
      Result[Idx] := ' ';
end;

function TMacroTemplatesExpert.AddLeadingSpaces(const AString: string;
  ASpacesCount: Integer; AInsertCol: Integer): string;
var
  WhiteString: string;
begin
  if ASpacesCount < 1 then
  begin
    Result := AString;
    Exit;
  end;

  WhiteString := PrepareWhitePrefix(AInsertCol);
  Result := PrefixLines(AString, WhiteString);
end;

function TMacroTemplatesExpert.PrefixLines(const Lines, Prefix: string; AStartLine: Integer): string;
var
  LastEOLFound: Boolean;
  i: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Lines;
    LastEOLFound := HasTrailingEOL(Lines);
    for i := AStartLine to SL.Count - 1 do
      SL.Strings[i] := Prefix + SL.Strings[i];

    Result := SL.Text;
    // Workaround for TStrings limitation - it always adds
    // an EOL when using the Text property
    if (not LastEOLFound) and HasTrailingEOL(Result) then
      RemoveLastEOL(Result);
  finally
    FreeAndNil(SL);
  end;
end;

// Expand the macros in the template text, embed/delete selection, etc.
procedure TMacroTemplatesExpert.PrepareTemplateText(var VText: string;
  ACurrentPos: TOTAEditPos; AAfterLen: Integer; var InsertOffset: Integer;
  var InsertPos: TOTAEditPos);
var
  PosAfter: TOTAEditPos;
  DoParseSource: Boolean;
  ReplaceSelection: Boolean;
  BlockStart, BlockEnd: TOTAEditPos;
  SelStart, SelLen: Integer;
  OffsetPos: Integer;
  LeadingText: string;
  EOLOnEndOfTemp, EOLOnEndOfSel: Boolean;
begin
  PosAfter := ACurrentPos;
  PosAfter.Col := PosAfter.Col + AAfterLen;
  Inc(PosAfter.Col);
  OffsetPos := 0;

  LeadingText := '';
  EOLOnEndOfSel := False;
  EOLOnEndOfTemp := False;

  ReplaceSelection := StrContains('%SELECTION%', VText, False);
  if ReplaceSelection then
  begin
    LeadingText := GxOtaGetLeadingWhiteSpaceInSelection;
    EOLOnEndOfTemp := HasTrailingEOL(VText);
    EOLOnEndOfSel := HasTrailingEOL(GxOtaGetCurrentSelection);
  end;

  DoParseSource := GxOtaGetCurrentSyntaxHighlighter = gxpPAS;
  // Cursor moving is disabled for now, since it clears %SELECTION% in Delphi 7
  //SetNewCursorPos(PosAfter);
  try
    VText := ReplaceStrings(VText, DoParseSource);
  finally
    //SetNewCursorPos(ACurrentPos);
  end;

  if ReplaceSelection and GxOtaGetSelection(nil, BlockStart, BlockEnd, SelStart, SelLen) then
  begin
    if (GxOtaCompareEditPos(BlockStart, ACurrentPos)<0) or
       (GxOtaCompareEditPos(BlockEnd, ACurrentPos)<0)
    then
    begin
      if (InsertOffset >= SelStart) and (InsertOffset < SelStart + SelLen) then
        OffsetPos := SelStart - InsertOffset
      else
        OffsetPos := -SelLen;

      if OffsetPos<>0 then
        InsertOffset := InsertOffset + OffsetPos;
    end;

    // Clear old selection
    GxOtaDeleteSelection;
  end;

  if OffsetPos <> 0 then
    InsertPos := OffsetToEditPos(InsertOffset);

  if ReplaceSelection then
  begin
    VText := PrefixLines(VText, LeadingText, 0);

    if (not EOLOnEndOfSel) and EOLOnEndOfTemp then
      RemoveLastEOL(VText);
  end;
end;

function TMacroTemplatesExpert.CharPosToEditPos(ACharPos: TOTACharPos): TOTAEditPos;
var
  EditView: IOTAEditView;
begin
  Result.Col := 1;
  Result.Line := 1;

  EditView := GxOtaGetTopMostEditView;
  if not Assigned(EditView)then
    Exit;

  EditView.ConvertPos(False, Result, ACharPos);
end;

function TMacroTemplatesExpert.OffsetToEditPos(AOffset: Integer): TOTAEditPos;
var
  EditView: IOTAEditView;
  CharPos: TOTACharPos;
begin
  EditView := GxOtaGetTopMostEditView;
  if not Assigned(EditView)then
    raise Exception.Create('No edit view present');

  CharPos := GxOtaGetCharPosFromPos(AOffset, EditView);
  EditView.ConvertPos(False, Result, CharPos);
end;

procedure TMacroTemplatesExpert.Execute(Sender: TObject);
begin
  ExpandTemplate;
end;

procedure TMacroTemplatesExpert.ExpandTemplate(const AForcedCode: string);
var
  TemplateName: string;
  TemplateText: string;
  TemplateIdx: Integer;
  InsertOffset: Integer;
  CodeOffset: Integer;
  AfterLen: Integer;
  InsertPos, CurrentPos: TOTAEditPos; // In characters
  NewCursorPos: TOTAEditPos; // Standard cursor position
  CharPos: TOTACharPos;
  TemplateObject: TMacroObject;
  InsertWhiteCnt: Integer;
  CodeForced, CodeInEditor: Boolean;
  CursorPos: TOTAEditPos;
  TemplateContainsPipe: Boolean;
begin
  // Retrieve the template from the editor window
  GxOtaGetCurrentIdentEx(TemplateName, CodeOffset, InsertPos, CurrentPos, AfterLen);

  // CodeOffset: Offset of the identifier in the character stream (Integer)
  // InsertPos:  Position where the identifier starts (line/col, 1-based)
  // CurrentPos: Current caret position (line/col)
  // AfterLen:   Length of the part of the identifier that is after cursor

  // AForcedCode is set when a macro's shortcut is pressed or when the space expand
  // notifier triggers (though the macro text is pre-deleted in this case)
  if AForcedCode <> '' then
  begin
    TemplateName := AForcedCode;
    AfterLen := 0;
    CodeForced := True;
  end
  else
    CodeForced := False;
  // TODO 3 -oAnyone -cBug: Fix when pressing Shift+Alt+T or the template's shortcut with a non-template identifier under the cursor, but choosing another template to insert inserts the text before the identifier

  // Locate the named template, or prompt for one if it does not exist
  TemplateIdx := GetTemplate(TemplateName, not CodeForced);
  if TemplateIdx < 0 then
    Exit
  else
  begin
    // Figure out how to make this work without messing up offsets with multi-line selections
    //if not StrContains('%SELECTION%', TemplateText) then
    //  GxOtaDeleteSelection;

    TemplateObject := FMacroFile.MacroItems[TemplateIdx];
    TemplateText := TemplateObject.Text;
    TemplateContainsPipe := StrContains('|', TemplateText);
    // Convert to CRLF on Windows
    if sLineBreak = #13#10 then
      TemplateText := AdjustLineBreaks(TemplateText);

    // Insert the macro into the editor
    if TemplateText <> '' then
    begin
      CodeInEditor := (CurrentPos.Col + 1 <> InsertPos.Col) and (not CodeForced);

      if CodeForced and (TemplateObject.InsertPos = tipCursorPos) then
      begin
        // For when the template is to be inserted directly at the end of a non-template-name identifier via a shortcut
        // This was causing problems with templates like raise and is disabled
        //CodeOffset := CodeOffset + Length(TemplateName);
        //InsertPos := CurrentPos;
        //InsertPos.Col := InsertPos.Col + 1;
      end;

      InsertOffset := CodeOffset;
      // This does nothing for tipCursorPos:
      CalculateInsertPos(TemplateObject.InsertPos, InsertOffset, InsertPos);
      // Expand the macros in the template text, embed/delete selection, etc.
      PrepareTemplateText(TemplateText, CurrentPos, AfterLen, InsertOffset, InsertPos);

      if TemplateObject.InsertPos = tipCursorPos then
      begin
        InsertWhiteCnt := InsertPos.Col - 1; // insert pos is one-based
        if InsertWhiteCnt > 0 then
          TemplateText := AddLeadingSpaces(TemplateText, InsertWhiteCnt, InsertPos.Col)
        else
        begin
          if CodeInEditor then
            CursorPos := InsertPos // start of code in editor
          else
            CursorPos := GxOtaGetCurrentEditPos;

          if CursorPos.Col > 1 then // Trim trailing blanks is on, and the user is not in the first column
            TemplateText := PrefixLines(TemplateText, StringOfChar(' ', CursorPos.Col - 1), 0);
        end;
      end;

      PrepareNewCursorPos(TemplateText, InsertPos, NewCursorPos, TemplateContainsPipe);

      if CodeForced or (not SameText(TemplateName, TemplateObject.Name)) then
        InsertTemplateIntoEditor('', TemplateText, CodeOffset, InsertOffset)
      else
        InsertTemplateIntoEditor(TemplateName, TemplateText, CodeOffset, InsertOffset);

      CharPos.CharIndex := NewCursorPos.Col - 1;
      CharPos.Line := NewCursorPos.Line;

      CurrentPos := CharPosToEditPos(CharPos);
      SetNewCursorPos(CurrentPos);
      AddUsesToUnit(TemplateObject);
    end;
  end;
end;

procedure TMacroTemplatesExpert.AddUsesToUnit(ATemplateObject: TMacroObject);
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    AddUnits(ATemplateObject.PrivUnits, True);
    AddUnits(ATemplateObject.PubUnits, False);
  finally
    Free;
  end;
end;

function TMacroTemplatesExpert.GetTemplate(const ATemplateName: string;
  APromptForUnknown: Boolean): Integer;
begin
  Result := FMacroFile.IndexOf(ATemplateName);
  if (Result < 0) and APromptForUnknown then
    Result := GetTemplateFromUser(ATemplateName, FMacroFile);
end;

function TMacroTemplatesExpert.GetHelpString: string;
begin
  Result := '  This expert expands templates that can include %-delimited macros.  ' +
    'It can be used in similar way to the IDE code templates, but is much more powerful because you can define custom shortcuts, insert positions, and include text-replacement macros.  ' +
    'The supported macros are available via the context menu in the template editor.  See the help file for more complete documentation.';
end;

procedure TMacroTemplatesExpert.InternalLoadSettings(Settings: TExpertSettings);
var
  SettingStorage: TMacroTemplatesIni;
begin
  SettingStorage := TMacroTemplatesIni.Create;
  try
    FSettings.ProgrammerName := SettingStorage.ReadString(ProgrammerIDKey,
      ProgrammerNameIdent, EmptyString);
    if FSettings.ProgrammerName = EmptyStr then
      FSettings.ProgrammerName := GetCurrentUser;
    FSettings.ProgrammerInitials := SettingStorage.ReadString(ProgrammerIDKey,
      ProgrammerInitialsIdent, EmptyString);
    if FSettings.ProgrammerInitials = EmptyString then
      FSettings.ProgrammerInitials := GetInitials(FSettings.ProgrammerName);
    ShortCut := SettingStorage.ReadInteger(CommonConfigurationKey, ShortCutIdent, ShortCut);
    FSettings.ExpandWithChar := SettingStorage.ReadBool(CommonConfigurationKey, ExpandWithCharIdent, FSettings.ExpandWithChar);
    FSettings.ExpandDelay := SettingStorage.ReadInteger(CommonConfigurationKey, ExpandDelayIdent, -1);
    ConfigAutoExpand;
  finally
    FreeAndNil(SettingStorage);
  end;
end;

procedure TMacroTemplatesExpert.InternalSaveSettings(Settings: TExpertSettings);
var
  SettingStorage: TMacroTemplatesIni;
begin
  SettingStorage := TMacroTemplatesIni.Create;
  try
    SettingStorage.WriteInteger(CommonConfigurationKey, ShortCutIdent, ShortCut);
    SettingStorage.WriteString(ProgrammerIDKey, ProgrammerNameIdent, FSettings.ProgrammerName);
    SettingStorage.WriteString(ProgrammerIDKey, ProgrammerInitialsIdent, FSettings.ProgrammerInitials);
    SettingStorage.WriteBool(CommonConfigurationKey, ExpandWithCharIdent, FSettings.ExpandWithChar);
    if FSettings.ExpandDelay >= 0 then // Non-default
      SettingStorage.WriteInteger(CommonConfigurationKey, ExpandDelayIdent, FSettings.ExpandDelay);
  finally
    FreeAndNil(SettingStorage);
  end;
end;

// Prepare the new cursor position after insertion of a template
// Fix the template's text if a cursor indicator was found
procedure TMacroTemplatesExpert.PrepareNewCursorPos(var VTemplateText: string;
  AInsertPos: TOTAEditPos; var VNewPos: TOTAEditPos; ConsiderPipe: Boolean);
var
  CursorMarkIndex: Integer;
begin
  // Calculate new cursor offset if it is assumed in macro code
  CursorMarkIndex := Pos('|', VTemplateText);
  if (CursorMarkIndex > 0) and ConsiderPipe then
  begin // This does not account for any embedded pipes in a %SELECTION%
    VNewPos := CalculateNewCursorPos(VTemplateText, AInsertPos);
    Delete(VTemplateText, CursorMarkIndex, 1);
  end
  else
    VNewPos := AInsertPos;
end;

procedure TMacroTemplatesExpert.SetNewCursorPos(ANewEditPos: TOTAEditPos);
resourcestring
  SNoEditView = 'No edit view (you have found a bug!)';
var
  EditView: IOTAEditView;
begin
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
    EditView.CursorPos := ANewEditPos
  else
    raise Exception.Create(SNoEditView);
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

// Calculate the new cursor postition using the old cursor
// position and position cursor marker "|" in AString
function TMacroTemplatesExpert.CalculateNewCursorPos(const AString: string;
  ACursorPos: TOTAEditPos): TOTAEditPos;
var
  i: Integer;
  MarkIndex: Integer;
  SL: TStringList;
begin
  Result := ACursorPos;
  SL := TStringList.Create;
  try
    SL.Text := AString;
    for i := 0 to SL.Count - 1 do
    begin
      MarkIndex := Pos('|', SL.Strings[i]);
      if MarkIndex > 0 then
      begin
        Result.Line := ACursorPos.Line + i;
        if i > 0 then
          Result.Col := MarkIndex
        else
          Result.Col := ACursorPos.Col + MarkIndex - 1;
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

// Calculate the insert position based on AInsertOption
procedure TMacroTemplatesExpert.CalculateInsertPos(AInsertOption: TTemplateInsertPos;
  var VInsertOffset: Integer; var VInsertPos: TOTAEditPos);
begin
  case AInsertOption of
    tipUnitStart:
      begin
        VInsertOffset := 0;
        VInsertPos.Line := 1;
        VInsertPos.Col := 1;
      end;
    tipLineStart:
      begin
        VInsertOffset := VInsertOffset - VInsertPos.Col + 1;
        VInsertPos.Col := 1;
      end;
    tipLineEnd:
      begin
        // TODO 4 -oAnyone -cFeature: Complete line end template support
      end;
    // Otherwise, use the existing/current insert position
  end;
end;

procedure TMacroTemplatesExpert.ReloadSettings;
begin
  LoadSettings;
  LoadTemplates;
end;

procedure TMacroTemplatesExpert.ConfigAutoExpand;
var
  DelayMS: Integer;
begin
  SetAutoExpandActive(FSettings.ExpandWithChar);

  if FSettings.ExpandWithChar then
  begin
    DelayMS := FSettings.ExpandDelay * ExpandDelayNumerator;
    if DelayMS < 0 then
      DelayMS := DefExpandDelay
    else if DelayMS < MinExpandDelay then
      DelayMS := MinExpandDelay;

    FExpandNotifier.SetExpandDelay(DelayMS);
  end;  
end;

function TMacroTemplatesExpert.GetProgrammerInfo(var VInfo: TProgrammerInfo): Boolean;
begin
  VInfo.FullName := FSettings.ProgrammerName;
  VInfo.Initials := FSettings.ProgrammerInitials;
  Result := True;
end;

function TMacroTemplatesExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('T');
end;

function TMacroTemplatesExpert.GetDisplayName: string;
resourcestring
  SMacroDisplayName = 'Expand Macro Template';
begin
  Result := SMacroDisplayName;
end;

class function TMacroTemplatesExpert.GetName: string;
begin
  Result := 'MacroTemplates';
end;

procedure TMacroTemplatesExpert.CreateDefaultTemplates;
var
  Macro: TMacroObject;
  i: Integer;
  Default: TMacroDetails;
begin
  MacroFile.Clear;
  for i := Low(DefaultMacros) to High(DefaultMacros) do
  begin
    Default := DefaultMacros[i];
    Macro := TMacroObject.Create(Default.Name);
    Macro.Desc := Default.Desc;
    Macro.Text := Default.Text;
    Macro.InsertPos := Default.InsertPos;
    Macro.PrivUnits.Text := Default.ImplUnit;
    MacroFile.AddMacro(Macro);
  end;
  if PrepareDirectoryForWriting(ExtractFileDir(MacroFile.FileName)) then
    MacroFile.SaveToFile(MacroFile.FileName);
end;

procedure TMacroTemplatesExpert.SetAutoExpandActive(Value: Boolean);
begin
  if Value then
    RegisterExpandNotifier
  else
    RemoveExpandNotifier;
end;

procedure TMacroTemplatesExpert.RegisterExpandNotifier;
begin
  if not Assigned(FExpandNotifier) then
    FExpandNotifier := PrepareNewExpandMacroNotifier(Self.HandleExpandTemplate);
end;

procedure TMacroTemplatesExpert.RemoveExpandNotifier;
begin
  if Assigned(FExpandNotifier) then
  begin
    (FExpandNotifier as IMacroExpandNotifier).Detach;
    FExpandNotifier := nil;
  end;
end;

procedure TMacroTemplatesExpert.HandleExpandTemplate(const AName, AText: string; var Accepted: Boolean);
var
  TemplateIdx: Integer;
  TemplateOffset: Integer;
  TextLen: Integer;
begin
  Accepted := False;

  TemplateIdx := GetTemplate(AName, False);
  if TemplateIdx < 0 then
    Exit;

  TextLen := Length(AText);
  TemplateOffset := GxOtaGetCurrentEditBufferPos - TextLen;

  if TemplateOffset < 0 then
    Exit;

  GxOtaDeleteByteFromPos(TemplateOffset, TextLen);
  ExpandTemplate(AName);

  Accepted := True;
end;

{ TExpandTemplateSettings }

procedure TExpandTemplateSettings.LoadSettings;
begin
  (FOwner as TMacroTemplatesExpert).LoadSettings;
end;

procedure TExpandTemplateSettings.SaveSettings;
begin
  (FOwner as TMacroTemplatesExpert).SaveSettings;
end;

procedure TExpandTemplateSettings.Reload;
begin
  (FOwner as TMacroTemplatesExpert).ReloadSettings;
end;

initialization
  RegisterEditorExpert(TMacroTemplatesExpert);

end.

