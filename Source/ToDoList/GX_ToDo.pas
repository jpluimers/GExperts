unit GX_ToDo;

{$I GX_CondDefine.inc}

interface

uses Forms, Messages, Classes, ActnList, Menus, ImgList,
  Controls, ComCtrls, ToolWin, ToolsAPI,
  GX_IdeDock, GX_Experts, GX_OtaUtils, GX_ConfigurationInfo, Graphics, Actions;

const
  UM_RESIZECOLS = WM_USER + 523;

resourcestring
  SCritical = 'Critical';
  SHigh = 'High';
  SNormal = 'Normal';
  SLow = 'Low';
  SLowest = 'Lowest';  
  SInfo = 'Info';
  SDone = 'Done';

type
  TToDoPriority = (tpCritical, tpHigh, tpNormal, tpLow, tpLowest, tpInfo, tpDone);

var
  PriorityText: array[Low(TToDoPriority)..High(TToDoPriority)] of string =
    (SCritical, SHigh, SNormal, SLow, SLowest, SInfo, SDone);

type
  TTokenInfo = class(TObject)
  private
    FToken: string;
    FPriority: TToDoPriority;
  public
    property Token: string read FToken write FToken;
    property Priority: TToDoPriority read FPriority write FPriority;
  end;

  TTokenList = class(TStringList)
  private
    procedure LoadFromSettings(ExpSettings: TExpertSettings);
    procedure SaveToSettings(ExpSettings: TExpertSettings);
    procedure AddToken(const Token: string; Priority: TToDoPriority);
  public
    destructor Destroy; override;
  end;

  TDirList = class(TStringList)
    procedure LoadFromSettings(ExpSettings: TExpertSettings);
    procedure SaveToSettings(ExpSettings: TExpertSettings);
  end;

  TToDoInfo = class(TObject)
  private
    Owner: string;
    ToDoClass: string;
    //
    Priority: TToDoPriority;
    Raw: string;
    Display: string;
    FileName: string;
    LineNo: Integer;
    function NumericPriority: Integer;
  end;

  TToDoScanType = (tstProject, tstOpenFiles, tstDirectory, tstProjectGroup);
  TParsePasFileCallback = procedure(const FileName: string; const SComment, EComment: string;
    const CommentStr: string; LineNumber: Integer) of object;

  TfmToDo = class(TfmIdeDockForm)
    StatusBar: TStatusBar;
    lvToDo: TListView;
    Popup: TPopupMenu;
    mitGoto: TMenuItem;
    mitRefresh: TMenuItem;
    mitPrint: TMenuItem;
    mitConfigure: TMenuItem;
    mitCopyToClipboard: TMenuItem;
    Actions: TActionList;
    actFileRefresh: TAction;
    actEditGoto: TAction;
    actFilePrint: TAction;
    actOptionsConfigure: TAction;
    actHelpHelp: TAction;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnSep1: TToolButton;
    tbnGoto: TToolButton;
    tbnSep2: TToolButton;
    tbnPrint: TToolButton;
    tbnSep3: TToolButton;
    tbnConfigure: TToolButton;
    tbnSep4: TToolButton;
    tbnHelp: TToolButton;
    mitSep2: TMenuItem;
    mitSep1: TMenuItem;
    actEditCopy: TAction;
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvToDoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvToDoColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvToDoEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lvToDoCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure actFilePrintUpdate(Sender: TObject);
    procedure actEditGotoExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actOptionsConfigureExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvTodoCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FIsFirstActivation: Boolean;
    FDataList: TList;
    FNotifier: TBaseIdeNotifier;
    FSortAscending: Boolean;
    FColumnIndex: Integer;
    FProjectFileName: string;
    FScannedFiles: TStringList;
    function GetSelectedItem: TToDoInfo;
    procedure UMResizeCols(var Msg: TMessage); message UM_RESIZECOLS;
    procedure RefreshTodoList;
    procedure ClearDataListAndListView;
    procedure ParseComment(const FileName: string; const SComment, EComment: string;
      const CommentStr: string; LineNumber: Integer);
    procedure LoadFile(const FileName: string);
    procedure EnumerateFilesByDirectory;
    procedure EnumerateProjectFiles(Project: IOTAProject);
    procedure EnumerateProjects;
    procedure SaveSettings;
    procedure LoadSettings;
    function ConfigurationKey: string;
    function PriorityToImageIndex(Priority: TToDoPriority): Integer;
    function NumericPriorityToGXPriority(const PriorityStr: string): TToDoPriority;
    procedure ParsePasFile(const _Filename: string; const _Content: string; _Callback: TParsePasFileCallback);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TToDoExpert = class(TGX_Expert)
  private
    FScanType: TToDoScanType;
    FDirsToScan: string;
    FRecurseDirScan: Boolean;
    FTokenList: TTokenList;
    FShowTokens: Boolean;
    FAddMessage: Boolean;
    FHideOnGoto: Boolean;
    FDirectoryHistoryList: TDirList;
    FFont: TFont;
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
  end;

var
  fmToDo: TfmToDo;
  ToDoExpert: TToDoExpert;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Dialogs, Clipbrd, Windows,
  mPasLex, mwBCBTokenList,
  GX_GxUtils, GX_GenericUtils, GX_EditReader,
  GX_ToDoOptions, GX_SharedImages, Math;

resourcestring
  SParsingError = 'A parsing error occurred in file %s.' + sLineBreak;

type
  TToDoNotifier = class(TBaseIdeNotifier)
  private
    FToDoForm: TfmToDo;
  public
    constructor Create(Owner: TfmToDo);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

{ TTokenList }

procedure TTokenList.LoadFromSettings(ExpSettings: TExpertSettings);
resourcestring
  SWhiteSpaceWarning = 'GExperts found the "%s" To Do List token with leading and/or trailing spaces.' + sLineBreak +
    sLineBreak +
    'Use of such tokens is deprecated; please correct your list of tokens ' +
    'in the To Do List configuration dialog to take this into account.';

  SLeadingDollarWarning = 'GExperts found the "%s" To Do List token with a leading $ character.' + sLineBreak +
    sLineBreak +
    'Use of such tokens is no longer allowed.' + sLineBreak +
    sLineBreak +
    'Please correct your list of tokens in the To Do List configuration dialog ' +
    'as soon as possible to take this into account.';
var
  i: Integer;
  TokenInfo: TTokenInfo;
  TempTokenText: string;
  ListSettings: TExpertSettings;
begin
  ExpSettings.ReadSection('Tokens', Self);
  ListSettings := ExpSettings.CreateExpertSettings('Tokens');
  try
    for i := 0 to Count - 1 do
    begin
      // Sanity checks of tokens
      TempTokenText := Self[i];
      if TempTokenText <> Trim(TempTokenText) then
      begin
        MessageDlg(Format(SWhiteSpaceWarning, [TempTokenText]), mtWarning, [mbOK], 0);
        TempTokenText := Trim(TempTokenText);
      end;

      if (Length(TempTokenText) > 0) and (TempTokenText[1] = '$') then
      begin
        MessageDlg(Format(SLeadingDollarWarning, [TempTokenText]), mtWarning, [mbOK], 0);
      end;

      TokenInfo := TTokenInfo.Create;
      TokenInfo.Token := Self[i];
      TokenInfo.Priority := TToDoPriority(ListSettings.ReadInteger(TokenInfo.Token, 1));
      Objects[i] := TokenInfo;
    end;
  finally
    FreeAndNil(ListSettings);
  end;
  if Count = 0 then
  begin
    // No tokens found, create a default list of tokens
    AddToken('TODO', tpNormal);
    AddToken('#ToDo1', tpHigh);
    AddToken('#ToDo2', tpNormal);
    AddToken('#ToDo3', tpLow);
    AddToken('INFO', tpInfo);
    AddToken('DONE', tpDone);
  end;
end;

procedure TTokenList.SaveToSettings(ExpSettings: TExpertSettings);
var
  i: Integer;
  ListSettings: TExpertSettings;
begin
  // Do not localize any of the below items.
  ExpSettings.EraseSection('Tokens');
  ListSettings := ExpSettings.CreateExpertSettings('Tokens');
  try
    for i := 0 to Count - 1 do
      ListSettings.WriteInteger(Self[i], Ord(TTokenInfo(Objects[i]).Priority));
  finally
    FreeAndNil(ListSettings);
  end;
end;

procedure TTokenList.AddToken(const Token: string; Priority: TToDoPriority);
var
  TokenInfo: TTokenInfo;
begin
  TokenInfo := TTokenInfo.Create;
  TokenInfo.Token := Token;
  TokenInfo.Priority := Priority;

  AddObject(Token, TokenInfo);
end;

destructor TTokenList.Destroy;
var
  i: Integer;
begin
  //{$IFOPT D+} SendDebug('TTokenList.Destroy'); {$ENDIF}
  for i := Count - 1 downto 0 do
    Objects[i].Free;

  inherited Destroy;
end;

{ TToDoNotifier }

constructor TToDoNotifier.Create(Owner: TfmToDo);
begin
  inherited Create;
  FToDoForm := Owner;
end;

procedure TToDoNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of

    ofnActiveProjectChanged:
      begin
        if not SameFileName(FileName, FToDoForm.FProjectFileName) then
        begin
          FToDoForm.ClearDataListAndListView;
          if FToDoForm.Visible then
            FToDoForm.RefreshTodoList
          else
            FToDoForm.FIsFirstActivation := True;
        end;
      end;

    ofnFileClosing:
      begin
        if (ToDoExpert.FScanType = tstProject) and (SameFileName(FileName, FToDoForm.FProjectFileName)) then
          FToDoForm.ClearDataListAndListView
        else if GxOtaGetCurrentProject = nil then
          FToDoForm.ClearDataListAndListView;
      end;
  end;
end;

{zTODO INFO Make sure this works}
{zTODO 1 -oAnyone -cCriticalBug: Priority 1 test}

procedure TfmToDo.ClearDataListAndListView;
var
  i: Integer;
begin
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
      TToDoInfo(FDataList.Items[i]).Free;
    FDataList.Clear;
  end;
  lvToDo.Items.Clear;
end;

procedure TfmToDo.EnumerateProjects;
var
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  ProjectGroup := GxOtaGetProjectGroup;
  if not Assigned(ProjectGroup) then
    Exit;
  for i := 0 to ProjectGroup.ProjectCount - 1 do
    EnumerateProjectFiles(ProjectGroup.Projects[i]);
end;

procedure TfmToDo.EnumerateProjectFiles(Project: IOTAProject);

  procedure ScanFile(const FileName: string);
  begin
    if IsKnownSourceFile(FileName) or IsToDo(FileName) then
    begin
      try
        if ToDoExpert.FScanType <> tstOpenFiles then
          LoadFile(FileName)
        else if GxOtaIsFileOpen(FileName) then
          LoadFile(FileName);
      except
        on E: Exception do
        begin
          GxLogAndShowException(E, Format(SParsingError, [FileName]));
          // Swallow exception
        end;
      end;
    end;
  end;

var
  i: Integer;
  ModuleInfo: IOTAModuleInfo;
begin
  if not Assigned(Project) then
    Exit;
  if ToDoExpert.FScanType <> tstProject then
    FProjectFileName := ''
  else
    FProjectFileName := GxOtaGetCurrentProjectFileName;

  ScanFile(GxOtaGetProjectFileName(Project, True));
  for i := 0 to Project.GetModuleCount-1 do
  begin
    ModuleInfo := Project.GetModule(i);
    Assert(Assigned(ModuleInfo));
    ScanFile(ModuleInfo.FileName);
  end;
end;

{#todo2 test this carefully}

{#todo3 Another test}

// What are SComment and EComment passed in for?
// If we want trimming, then a Boolean variable should
// be passed in, clearly saying so.

procedure TfmToDo.ParseComment(const FileName: string; const SComment, EComment: string;
  const CommentStr: string; LineNumber: Integer);
var
  i, j, k, n, m, TokenPos, NextCharPos: Integer;
  Info: TToDoInfo;
  ParsingString: string;
  OptionChar: Char;
  CListItem: TListItem;
begin
  for i := 0 to ToDoExpert.FTokenList.Count - 1 do
  begin
    n := AnsiCaseInsensitivePos(ToDoExpert.FTokenList[i], CommentStr);
    if n > 1 then
    begin
      // We found a token that looks like a TODO comment. Now
      // verify that it *is* one: either a white-space or the
      // comment token need to be right in front of the TODO item

      // Remove comment characters
      ParsingString := CommentStr;
      Delete(ParsingString, 1, Length(SComment));
      // Remove white-space left and right
      ParsingString := Trim(ParsingString);

      // The TODO token should be at the beginning of the comment
      TokenPos := AnsiCaseInsensitivePos(ToDoExpert.FTokenList[i], ParsingString);
      if TokenPos <> 1 then
        Continue;

      // The TODO token should be followed by a non-alphanumeric character
      NextCharPos := TokenPos + Length(ToDoExpert.FTokenList[i]);
      if (NextCharPos <= Length(ParsingString)) and IsCharAlphaNumeric(ParsingString[NextCharPos]) then
        Continue;

      // Token found in comment line
      Info := TToDoInfo.Create;
      Info.Priority := TTokenInfo(ToDoExpert.FTokenList.Objects[i]).Priority;

      // Remove token from string
      Delete(ParsingString, 1, Length(ToDoExpert.FTokenList[i]));
      ParsingString := Trim(ParsingString);

      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(ParsingString) do
      begin
        if not IsCharNumeric(ParsingString[j + 1]) then
          Break;
        Inc(j);
      end;
      if j > 0 then begin
        if not (Info.Priority in [tpInfo, tpDone]) then
          Info.Priority := NumericPriorityToGXPriority(Copy(ParsingString, 1, j));
        Delete(ParsingString, 1, j);
        ParsingString := TrimLeft(ParsingString);
      end;

      { zTODO -oTestCase: -cIssue <-- test case for colon }
      { zTODO DONE -oTestCase: -cIssue <-- test case for DONE }
      // Delete everything being with a possible trailing colon:
      j := Pos(':', ParsingString);
      if j > 0 then
        Delete(ParsingString, j, Length(ParsingString))
      else
        ParsingString := '';

      { zTODO -cSomething -oTestCase: <-- test case for -o switch }
      { zTODO blah -cSomething -oTestCase: <-- this is a deliberately broken item }
      { zTODO -oTestCase -cTest -c switch: <-- test case for -c switch }
      { zTODO -oTestCase -cTest -c switch: <-- another test case for -c switch }
      { zTODO -oTestCase -cTest -zWhoops -c switch: <-- -z switch }
      { zTODO -oTestCase -cTest -z-z-z-z -------------- -c switch: <-- try to break it }
      { zTODO -oTestCase -cTe-st : <-- hyphen test }
      //zTOdo
      { zTODO -oTestCase
          -cMultiline
          <-- Multiline test }
      // Identify owner of TODO item (-o)
      // Identify class of TODO item (-c)
{$IFNDEF GX_VER310_up}
      OptionChar := #0; // Initialize to make compiler happy - redundant
{$ENDIF}
      while StrContains('-', ParsingString) do
      begin
        if Length(ParsingString) > 1 then
        begin
          OptionChar := UpCase(ParsingString[2]);
          Delete(ParsingString, 1, 2);
        end
        else
          Break;

        // Find char immediately preceding the next option switch
        j := Pos('-', ParsingString) - 1;
        if j > 0 then
          k := j
        else
          k := Length(ParsingString);

        case OptionChar of
          'O': Info.Owner := Trim(Copy(ParsingString, 1, k));
          'C': Info.ToDoClass := Trim(Copy(ParsingString, 1, k));
        end;

        // Delete everything up to, but not including, the
        // next option switch
        Delete(ParsingString, 1, j);
      end;

      Info.Raw := CommentStr;

      // Handle multi-line to do comments
      ParsingString := StringReplace(CommentStr, CRLF, ' ', [rfReplaceAll]);

      if not ToDoExpert.FShowTokens then
        n := n + Length(ToDoExpert.FTokenList[i]);
      if EComment <> '' then // Trim end-comment token.
        m := AnsiCaseInsensitivePos(EComment, ParsingString) - 1
      else
        m := Length(ParsingString);
      if m < 1 then
        m := Length(ParsingString);
      // The +1 is necessary to match IDE's line numbering
      Info.Display := Copy(ParsingString, n, (m - n) + 1);
      // Delete -C and -O options from ToDo text
      if Pos(' -C', UpperCase(Info.Display)) > 0 then
        Delete(Info.Display, Pos(' -C', UpperCase(Info.Display)), Length(Info.ToDoClass) + 3);
      if Pos(' -O', UpperCase(Info.Display)) > 0 then
        Delete(Info.Display, Pos(' -O', UpperCase(Info.Display)), Length(Info.Owner) + 3);
      // Remove excess whitespace
      Info.Display := Trim(CompressWhiteSpace(Info.Display));
      // Identify numeric priority (if there is one)
      j := 0;
      while j < Length(Info.Display) do
      begin
        if not IsCharNumeric(Info.Display[j + 1]) then
          Break;
        Inc(j);
      end;
      Delete(Info.Display, 1, j);
      Info.Display := TrimLeft(Info.Display);
      if StrBeginsWith(':', Info.Display) then
      begin
        Delete(Info.Display, 1, 1);
        Info.Display := Trim(Info.Display);
      end;
      
      Info.LineNo := LineNumber;
      Info.FileName := FileName;
      FDataList.Add(Info);
      CListItem := lvToDo.Items.Add;
      ClistItem.SubItems.Add(Info.ToDoClass);
      ClistItem.SubItems.Add(Info.Owner);
      ClistItem.SubItems.Add(Info.Display);
      ClistItem.SubItems.Add(ExtractFileName(Info.FileName));
      ClistItem.SubItems.Add(IntToStr(Info.LineNo));
      CListItem.Data := Info;
      CListItem.ImageIndex := PriorityToImageIndex(Info.Priority);

      Assert(Assigned(ToDoExpert));
      if ToDoExpert.FAddMessage then
        GxOtaWriteToolMessage(Info.FileName, Info.Display, ToDoExpert.FTokenList[i], Info.LineNo, 1);

      Break; // Comment line parsed, stop searching for more To Do items in that line
    end;
  end;
end;

procedure TfmToDo.LoadFile(const FileName: string);
var
  CParser: TBCBTokenList;
  FileContent: String;
  IsCPPModule: Boolean;
  InternalEditReader: TEditReader;
  HeaderFile: string;
begin
  if FScannedFiles.IndexOf(FileName) >= 0 then
    Exit;
  FScannedFiles.Add(FileName);
  StatusBar.SimpleText := ExtractFileName(FileName);
  StatusBar.Repaint;

  {$IFOPT D+}SendDebug('Loading: ' + FileName); {$ENDIF}
  if not (IsKnownSourceFile(FileName) or IsToDo(FileName) or IsInc(FileName)) then
    Exit;
  IsCPPModule := IsCppSourceModule(FileName);

  try
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    InternalEditReader := TEditReader.Create(FileName);
    try
      FileContent := InternalEditReader.GetText;
    finally
      FreeAndNil(InternalEditReader);
    end;

    if IsCPPModule then
    begin
      CParser := TBCBTokenList.Create;
      try
        CParser.SetOrigin(@FileContent[1], Length(FileContent));
        while CParser.RunID <> ctknull do
        begin
          case CParser.RunID of
            ctkansicomment: begin
              { TODO -oanyone -ccheck :
                Maybe we can also easily support multi line todos here?
                If the C parser handles them as the Pascal parser does,
                that shouldn't be too difficult }
              ParseComment(FileName, '/*', '*/', CParser.RunToken, CParser.PositionAtLine(CParser.RunPosition));
            end;
            ctkslashesComment: ParseComment(FileName, '//', '', CParser.RunToken, CParser.PositionAtLine(CParser.RunPosition));
          end;
          if CParser.RunIndex = CParser.Count - 1 then
            Break;
          CParser.Next;
        end;
      finally
        FreeAndNil(CParser);
      end;

      if (ExtractUpperFileExt(FileName) = '.CPP') then
      begin
        HeaderFile := ChangeFileExt(FileName, '.h');
        if GxOtaFileOrModuleExists(HeaderFile) then
          LoadFile(HeaderFile);
      end;
    end
    else
    begin
      ParsePasFile(FileName, FileContent, ParseComment);
    end;
  finally
    Self.Update;
  end;
end;

procedure TfmToDo.ParsePasFile(const _Filename: string; const _Content: string;
  _Callback: TParsePasFileCallback);
var
  Parser: TmwPasLex;
  CommentLineNo: Integer;
  CommentText: string;
begin
  Parser := TmwPasLex.Create;
  try
    Parser.Origin := @_Content[1];
    while Parser.TokenID <> tkNull do begin
          { TODO 4 -oAnyone -cIssue: move deleting of the comment
            tokens out of the parser }
      case Parser.TokenID of
        tkBorComment: begin
            CommentLineNo := Parser.LineNumber + 1;
            CommentText := '';
            while Parser.TokenID in [tkCRLFCo, tkBorComment] do begin
              CommentText := CommentText + Parser.Token;
              Parser.Next;
            end;
            _Callback(_FileName, '{', '}', CommentText, CommentLineNo);
          end;
        tkAnsiComment: begin
            CommentLineNo := Parser.LineNumber + 1;
            CommentText := '';
            while Parser.TokenID in [tkCRLFCo, tkAnsiComment] do begin
              CommentText := CommentText + Parser.Token;
              Parser.Next;
            end;
            _Callback(_FileName, '(*', '*)', CommentText, CommentLineNo);
          end;
        tkSlashesComment: begin
              { TODO -oanyone -ccheck :
                Do we also want to support multi line todos that start with // ?
                The parser does not support that very well in contrast to the multi line
                borland and ansi comments }
            _Callback(_FileName, '//', '', Parser.Token, Parser.LineNumber + 1);
          end;
      end;
      Parser.Next;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

{#todo1 DONE yet another test}

{#todo5 and yet another test}

{#todo6 finally, one more test}

procedure TfmToDo.FormActivate(Sender: TObject);
begin
  if FIsFirstActivation then
  begin
    FIsFirstActivation := False;
    RefreshTodoList;
  end;
  PostMessage(Self.Handle, UM_RESIZECOLS, 0, 0);
end;

procedure TfmToDo.RefreshTodoList;
var
  AListItem: TListItem;
  Cursor: IInterface;
begin
  Cursor := TempHourGlassCursor;
  try
    ToDoExpert.FTokenList.CustomSort(SortStringListByLength);
    FScannedFiles.Clear;
    ClearDataListAndListView;
    lvToDo.Items.BeginUpdate;
    try
      case ToDoExpert.FScanType of
        tstProject,
        tstOpenFiles:
          EnumerateProjectFiles(GxOtaGetCurrentProject);

        tstProjectGroup:
          EnumerateProjects;

        tstDirectory:
          begin
            // If expert is instructed to process files by directory.
            if Trim(ToDoExpert.FDirsToScan) <> '' then
              EnumerateFilesByDirectory;
          end;
      end;
    finally
      lvToDo.Items.EndUpdate;
    end;
  finally
    StatusBar.SimpleText := '';
    lvToDo.AlphaSort;
    if lvToDo.Items.Count > 0 then
    begin
      AListItem := lvToDo.Items[0];
      lvToDo.Selected := AListItem;
      lvToDo.ItemFocused := AListItem;
    end
    else
      actEditGoto.Enabled := False;
  end;
end;

procedure TfmToDo.actFilePrintUpdate(Sender: TObject);
begin
  inherited;

  (Sender as TAction).Enabled := (lvToDo.Items.Count > 0);
end;

type
  TMatchChecker = class
  private
    FSelectedItem: TToDoInfo;
    FClosestLineMatch: Integer;
    FLineMatchDifference: integer;
  public
    constructor Create(_SelectedItem: TToDoInfo);
    procedure CheckComment(const FileName: string; const SComment, EComment: string;
      const CommentStr: string; LineNumber: Integer);
      property ClosestLineMatch: Integer read FClosestLineMatch;
  end;

{ TMatchChecker }

constructor TMatchChecker.Create(_SelectedItem: TToDoInfo);
begin
  inherited Create;
  FSelectedItem := _SelectedItem;
  FClosestLineMatch := -1;
  FLineMatchDifference := MaxInt;
end;

procedure TMatchChecker.CheckComment(const FileName, SComment, EComment, CommentStr: string;
  LineNumber: Integer);
begin
  if SameText(CommentStr, FSelectedItem.Raw) then begin
    // Look for a matching todo comment with the smallest absolute distance
    // from the line where we found the original comment when last scanning
    if (FClosestLineMatch = -1) or (Abs(FSelectedItem.LineNo - LineNumber + 1) <= FLineMatchDifference) then
    begin
      FClosestLineMatch := LineNumber;
      FLineMatchDifference := Abs(FSelectedItem.LineNo - FClosestLineMatch);
    end;
  end;
end;

procedure TfmToDo.actEditGotoExecute(Sender: TObject);
var
  CParser: TBCBTokenList;
  FileContent: string;
  SelectedItem: TToDoInfo;
  InternalEditReader: TEditReader;
  ClosestLineMatch: Integer;
  LineMatchDifference: Integer;
  IsCPPModule: Boolean;
  Cursor: IInterface;
  MatchChecker: TMatchChecker;
begin
  inherited;

  ClosestLineMatch := -1;
  LineMatchDifference := MaxInt;

  SelectedItem := GetSelectedItem;
  if SelectedItem = nil then Exit;

  IsCPPModule := IsCppSourceModule(SelectedItem.FileName);

  if not GxOtaIsFileOpen(SelectedItem.FileName) then
    GxOtaOpenFile(SelectedItem.FileName);

  Cursor := TempHourGlassCursor;
  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  InternalEditReader := TEditReader.Create(SelectedItem.FileName);
  try
    FileContent := InternalEditReader.GetText;
    if IsCppModule then
    begin
      CParser := TBCBTokenList.Create;
      try
        CParser.SetOrigin(@FileContent[1], Length(FileContent));
        while CParser.RunID <> ctknull do
        begin
          if CParser.RunID in [ctkansicomment, ctkslashescomment] then
            if SameText(CParser.RunToken, SelectedItem.Raw) then
            begin
              // Look for a matching todo comment with the smallest absolute distance
              // from the line where we found the original comment when last scanning
              if (ClosestLineMatch = -1) or (Abs(SelectedItem.LineNo - CParser.PositionAtLine(CParser.RunPosition)) <= LineMatchDifference) then
              begin
                ClosestLineMatch := CParser.PositionAtLine(CParser.RunPosition) - 1;
                LineMatchDifference := Abs(SelectedItem.LineNo - ClosestLineMatch);
              end;
            end;
          if CParser.RunIndex = CParser.Count - 1 then
            Break;
          CParser.Next;
          Application.ProcessMessages;
        end;
      finally
        FreeAndNil(CParser);
      end;
    end
    else
    begin
      MatchChecker := TMatchChecker.Create(SelectedItem);
      try
        ParsePasFile('EditBuffer', FileContent, MatchChecker.CheckComment);
        ClosestLineMatch := MatchChecker.ClosestLineMatch;
      finally
        FreeAndNil(MatchChecker);
      end;
    end;
    if ClosestLineMatch > -1 then begin
      // we found a match, goto that line
      InternalEditReader.GotoOffsetLine(ClosestLineMatch);
    end else begin
      // no match found (matching does not work with multi line comments (yet))
      // so we goto the line we stored originally
      InternalEditReader.GotoOffsetLine(SelectedItem.LineNo);
    end;
  finally
    FreeAndNil(InternalEditReader);
  end;
  if ToDoExpert.FHideOnGoto then
    Self.Hide;
end;

procedure TfmToDo.actFilePrintExecute(Sender: TObject);
resourcestring
  STodoListForProject = 'To Do List for Project: %s';
  STodoListForDirectory = 'To Do List for Directory: %s';
  STodoItems = 'To Do Items';
var
  RichEdit: TRichEdit;
  i: Integer;
begin
  inherited;

  if lvToDo.Items.Count = 0 then
    Exit;
  //#TODO2 make this a nicely formatted page
  RichEdit := TRichEdit.Create(Self);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Self;
    RichEdit.Clear;
    RichEdit.Lines.Add('');
    with RichEdit.SelAttributes do
    begin
      Style := [fsBold];
      Size := 14;
    end;

    case ToDoExpert.FScanType of
      tstProject,
      tstOpenFiles:
        RichEdit.Lines.Add(Format(STodoListForProject, [GxOtaGetCurrentProjectName]));

      tstProjectGroup:
        RichEdit.Lines.Add(Format(STodoListForProject, [ExtractFileName(GxOtaGetProjectGroupFileName)]));

      tstDirectory:
        RichEdit.Lines.Add(Format(STodoListForDirectory, [ToDoExpert.FDirsToScan]));
    end;

    for i := 0 to FDataList.Count - 1 do
    begin
      with TToDoInfo(FDataList.Items[i]) do
      begin
        with RichEdit.SelAttributes do
        begin
        (*
          case Priority of
            tpHigh   : Style := [fsBold];
            tpMed : Style := [];
            tpLow    : Style := [fsItalic];
          end;
        *)
          Style := [];
          Size := 10;
        end;
        RichEdit.Lines.Add(ExtractFileName(FileName) + ' (' + IntToStr(LineNo) + ')' +
          #09 + PriorityText[Priority] + #9 + Display);
      end;
    end;
    RichEdit.Print(SToDoItems);
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure TfmToDo.actFileRefreshExecute(Sender: TObject);
begin
  inherited;

  RefreshTodoList;
end;

procedure TfmToDo.actEditCopyExecute(Sender: TObject);
var
  ClipText: TStrings;
  i: Integer;
begin
  inherited;

  ClipText := TStringList.Create;
  try
    for i := 0 to lvToDo.Items.Count - 1 do
    begin
      ClipText.Add(IntToStr(lvToDo.Items[i].ImageIndex) + #9 +
        lvToDo.Items[i].SubItems[0] + #9 +
        lvToDo.Items[i].SubItems[1] + #9 +
        lvToDo.Items[i].SubItems[2] + #9 +
        lvToDo.Items[i].SubItems[3] + #9 +
        lvToDo.Items[i].SubItems[4]);
    end;
    Clipboard.AsText := ClipText.Text;
  finally
    FreeAndNil(ClipText);
  end;
end;

procedure TfmToDo.actHelpHelpExecute(Sender: TObject);
begin
  inherited;

  GxContextHelp(Self, 24);
end;

procedure TfmToDo.actOptionsConfigureExecute(Sender: TObject);
begin
  inherited;

  ToDoExpert.Configure;
end;

constructor TfmToDo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetToolbarGradient(ToolBar);

  if Assigned(ToDoExpert) then
      ToDoExpert.SetFormIcon(Self);

  FIsFirstActivation := True;
  FColumnIndex := -1;
  FSortAscending := True;

  FDataList := TList.Create;
  FScannedFiles := TStringList.Create;

  FNotifier := TToDoNotifier.Create(Self);
  FNotifier.AddNotifierToIDE;

  LoadSettings;
end;

destructor TfmToDo.Destroy;
begin
  SaveSettings;
  FNotifier.RemoveNotifierFromIDE;
  FNotifier := nil;

  ClearDataListAndListView;

  FreeAndNil(FDataList);
  FreeAndNil(FScannedFiles);

  inherited Destroy;

  fmToDo := nil;
end;

function TfmToDo.GetSelectedItem: TToDoInfo;
begin
  Result := nil;
  if lvToDo.Visible then
    if lvToDo.Selected <> nil then
      Result := TToDoInfo(lvToDo.Selected.Data);
end;

procedure TfmToDo.lvToDoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  actEditGoto.Enabled := (Item <> nil);
end;

procedure TfmToDo.EnumerateFilesByDirectory;
var
  FileMaskList: TStringList;

  procedure DirScan(Dir: string);
  var
    Search: TSearchRec;
    Result: Integer;
    i: Integer;
  begin
    //{$IFOPT D+}SendDebug('DirSearch on:' +Dir+' Mask: '+Mask);{$ENDIF}
    Dir := AddSlash(Dir);

    // First do sub-directories if option is selected
    if ToDoExpert.FRecurseDirScan then
    begin
      Result := FindFirst(Dir + AllFilesWildCard, faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) <> 0 then
          begin
            if (Search.Name <> '.') and (Search.Name <> '..') then
              DirScan(Dir + Search.Name);
          end;
          Result := FindNext(Search);
        end;
      finally
        SysUtils.FindClose(Search);
      end;
    end;

    for i := 0 to FileMaskList.Count - 1 do
    begin
      Result := FindFirst(Dir + Trim(FileMaskList.Strings[i]), faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) = 0 then
          begin
            try
              LoadFile(Dir + Search.Name);
            except
              on E: Exception do
              begin
                GxLogAndShowException(E, Format(SParsingError, [Dir + Search.Name]));
                // Swallow exception
              end;
            end;
            Application.ProcessMessages;
          end;
          Result := FindNext(Search);
        end;
      finally
        SysUtils.FindClose(Search);
      end;
    end;
  end;

var
  FileMask: string;
  i: Integer;
  Dirs: TStringList;
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist';
begin
  FileMask := '*.todo';
  if GxOtaHaveDelphiSupport then
    FileMask := FileMask + ';*.pas;*.dpr;*.inc';
  if GxOtaHaveCPPSupport then
    FileMask := FileMask + ';*.cpp;*.hpp;*.h';

  for i := 1 to Length(FileMask) do
    if CharInSet(FileMask[i], [';', ',']) then
      FileMask[i] := #13;

  FileMaskList := TStringList.Create;
  try
    FileMaskList.Text := FileMask;

    Dirs := TStringList.Create;
    try
      AnsiStrTok(ToDoExpert.FDirsToScan, ';', Dirs);
      for i := 0 to Dirs.Count - 1 do
      begin
        Dirs[i] := ExpandFileName(Dirs[i]);
        if not DirectoryExists(Dirs[i]) then
          raise Exception.CreateFmt(SSpecifiedDirectoryDoesNotExist, [Dirs[i]]);
        DirScan(Dirs[i]);
      end;
    finally
      FreeAndNil(Dirs);
    end;
  finally
    FreeAndNil(FileMaskList);
  end;
end;

procedure TfmToDo.lvToDoEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmToDo.lvToDoColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index = FColumnIndex then
    FSortAscending := not FSortAscending
  else
    FColumnIndex := Column.Index;
  lvToDo.AlphaSort;
end;

procedure TfmToDo.lvToDoCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  case FColumnIndex of
    -1: Compare := 0;
    0: Compare := TToDoInfo(Item1.Data).NumericPriority - TToDoInfo(Item2.Data).NumericPriority;
    5: try
        // Odd bug workaround
        if ((Item1.SubItems.Count > 2) and (Item2.SubItems.Count > 2)) then
          Compare := StrToInt(Item1.SubItems[FColumnIndex - 1]) -
            StrToInt(Item2.SubItems[FColumnIndex - 1])
        else
          Compare := 0;
      except
        on E: Exception do
        begin
          Compare := AnsiCompareStr(Item1.SubItems[FColumnIndex - 1],
            Item2.SubItems[FColumnIndex - 1]);
        end;
      end;
  else
    Compare := AnsiCompareStr(Item1.SubItems[FColumnIndex - 1], Item2.SubItems[FColumnIndex - 1]);
  end;
  if not FSortAscending then Compare := -Compare;
end;

procedure TfmToDo.lvTodoCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Odd(Item.Index) then
    lvToDo.Canvas.Brush.Color := clWindow
  else
    lvToDo.Canvas.Brush.Color := clWindow or ($0A0A0A);
  if TToDoInfo(Item.Data).Priority = tpDone then
    lvToDo.Canvas.Font.Color := clGrayText;
end;

procedure TfmToDo.SaveSettings;
begin
  // Do not localize any of the below items
  with TGExpertsSettings.Create do
  try
    WriteBool(ConfigurationKey, 'SortAscending', FSortAscending);
    WriteInteger(ConfigurationKey, 'SortColumn', FColumnIndex);
    SaveForm(Self, ConfigurationKey + '\Window', [fsSize]);
  finally
    Free;
  end;
end;

procedure TfmToDo.LoadSettings;
begin
  // Do not localize any of the below items
  with TGExpertsSettings.Create do
  try
    FSortAscending := ReadBool(ConfigurationKey, 'SortAscending', True);
    FColumnIndex := ReadInteger(ConfigurationKey, 'SortColumn', 0);
    LoadForm(Self, ConfigurationKey + '\Window', [fsSize]);
  finally
    Free;
  end;
end;

procedure TfmToDo.FormResize(Sender: TObject);
var
  NewWidth: Integer;
begin
  NewWidth :=
    lvToDo.ClientWidth
    - lvToDo.Columns[0].Width
    - lvToDo.Columns[1].Width
    - lvToDo.Columns[2].Width
    - lvToDo.Columns[4].Width
    - lvToDo.Columns[5].Width;

  if NewWidth < 10 then
    NewWidth := 10;

  lvToDo.Columns[3].Width := NewWidth;
end;

procedure TfmToDo.UMResizeCols(var Msg: TMessage);
begin
  lvToDo.Columns.BeginUpdate;
  try
    FormResize(Self);
  finally
    lvToDo.Columns.EndUpdate;
  end;
end;

procedure TfmToDo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

function TfmToDo.ConfigurationKey: string;
begin
  Result := TToDoExpert.ConfigurationKey;
end;

{ TToDoExpert }

procedure TToDoExpert.Execute(Sender: TObject);
begin
  if fmToDo = nil then
  begin
    fmToDo := TfmToDo.Create(nil);
    fmToDo.lvToDo.Font.Assign(FFont);
  end;
  IdeDockManager.ShowForm(fmToDo);
end;

{ TODO 3 -cIssue -oAnyone: This process needs to be cleaned up.  Why store the configuration
 settings in the expert and the form?  It only makes updating both
 copies harder. }
procedure TToDoExpert.Configure;
var
  TodoListOptionsUnchanged: Boolean;

  function HasChanged(Value: boolean): boolean;
  begin
    Result := not Value;
    if Result then
      TodoListOptionsUnchanged := False;
  end;

var
  Dlg: TfmToDoOptions;
  OldScanType: TToDoScanType;
begin
  TodoListOptionsUnchanged := True;

  Dlg := TfmToDoOptions.Create(nil);
  try
    FTokenList.Sort;
    Dlg.lstTokens.Items.Assign(FTokenList);
    Dlg.cbShowTokens.Checked := FShowTokens;
    Dlg.cbAddMessage.Checked := FAddMessage;
    Dlg.cbHideOnGoto.Checked := FHideOnGoto;
    Dlg.cboDirectories.Items.Assign(FDirectoryHistoryList);
    case FScanType of
      tstProject:
        Dlg.radScanProj.Checked := True;
      tstProjectGroup:
        Dlg.radScanProjGroup.Checked := True;
      tstOpenFiles:
        Dlg.radScanOpen.Checked := True;
      tstDirectory:
        Dlg.radScanDir.Checked := True;
    end;
    Dlg.chkInclude.Checked := FRecurseDirScan;
    Dlg.cboDirectories.Text := FDirsToScan;
    Dlg.btnFont.Font := FFont;

    if Dlg.ShowModal = mrOk then
    begin
      // Add directory to FDirectoryHistoryList
      AddMRUString(Dlg.cboDirectories.Text, FDirectoryHistoryList, True);

      if HasChanged(FTokenList.Equals(Dlg.lstTokens.Items)) then
        FTokenList.Assign(Dlg.lstTokens.Items);
      if HasChanged(FShowTokens = Dlg.cbShowTokens.Checked) then
        FShowTokens := Dlg.cbShowTokens.Checked;
      if HasChanged(FAddMessage = Dlg.cbAddMessage.Checked) then
        FAddMessage := Dlg.cbAddMessage.Checked;
      if HasChanged(FHideOnGoto = Dlg.cbHideOnGoto.Checked) then
        FHideOnGoto := Dlg.cbHideOnGoto.Checked;

      OldScanType := FScanType;
      if Dlg.radScanProj.Checked then
        FScanType := tstProject
      else if Dlg.radScanProjGroup.Checked then
        FScanType := tstProjectGroup
      else
      if Dlg.radScanOpen.Checked then
        FScanType := tstOpenFiles
      else
      if Dlg.radScanDir.Checked then
        FScanType := tstDirectory;
      HasChanged(FScanType = OldScanType);

      if HasChanged(FRecurseDirScan = Dlg.chkInclude.Checked) then
        FRecurseDirScan := Dlg.chkInclude.Checked;
      if HasChanged(SameFileName(FDirsToScan, Dlg.cboDirectories.Text)) then
        FDirsToScan := Dlg.cboDirectories.Text;

      FFont.Assign(Dlg.btnFont.Font);
      if Assigned(fmTodo) then
        fmTodo.lvTodo.Font := FFont;

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;

  if not TodoListOptionsUnchanged then
  begin
    if Assigned(fmToDo) then
      fmToDo.RefreshTodoList;
  end;
end;

constructor TToDoExpert.Create;
begin
  Assert(ToDoExpert = nil);
  inherited Create;
  ToDoExpert := Self;
  FTokenList := TTokenList.Create;
  FHideOnGoto := True;
  FScanType := tstProject;
  FDirectoryHistoryList := TDirList.Create;

  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 9;
  FFont.Style := [];
  FFont.Color := clBlack;
end;

destructor TToDoExpert.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(fmToDo);
  FreeAndNil(FDirectoryHistoryList);
  FreeAndNil(FTokenList);

  ToDoExpert := nil;
  inherited Destroy;
end;

function TToDoExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&To Do List';
begin
  Result := SMenuCaption;
end;

class function TToDoExpert.GetName: string;
begin
  Result := 'ToDoList'; // Do not localize.
end;

procedure TToDoExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);

  // Do not localize
  FTokenList.LoadFromSettings(Settings);
  FDirectoryHistoryList.LoadFromSettings(Settings);

  FShowTokens := Settings.ReadBool('ShowTokens', False);
  FAddMessage := Settings.ReadBool('AddMessage', False);
  FHideOnGoto := Settings.ReadBool('HideOnGoto', False);
  FScanType := TToDoScanType(Settings.ReadEnumerated('ScanType', TypeInfo(TToDoScanType), Ord(tstProject)));
  fDirsToScan := Settings.ReadString('DirToScan', '');
  fRecurseDirScan := Settings.ReadBool('RecurseDirScan', False);
  Settings.LoadFont('Font', FFont);

  if Active then
    IdeDockManager.RegisterDockableForm(TfmToDo, fmToDo, 'fmToDo');
end;

procedure TToDoExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);

  // Do not localize
  FTokenList.SaveToSettings(Settings);
  FDirectoryHistoryList.SaveToSettings(Settings);
  Settings.WriteBool('ShowTokens', FShowTokens);
  Settings.WriteBool('AddMessage', FAddMessage);
  Settings.WriteBool('HideOnGoto', FHideOnGoto);
  Settings.WriteEnumerated('ScanType', TypeInfo(TToDoScanType), Ord(FScanType));
  Settings.WriteString('DirToScan', FDirsToScan);
  Settings.WriteBool('RecurseDirScan', FRecurseDirScan);
  Settings.SaveFont('Font', FFont);
end;

procedure TToDoExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to initialize here
    else
      FreeAndNil(fmToDo);
  end;
end;

{ TDirList }

procedure TDirList.LoadFromSettings(ExpSettings: TExpertSettings);
begin
  ExpSettings.ReadSection('DirList', Self);
end;

procedure TDirList.SaveToSettings(ExpSettings: TExpertSettings);
var
  i: Integer;
  ListSettings: TExpertSettings;
begin
  // Do not localize any of the below items.
  ExpSettings.EraseSection('DirList');
  ListSettings := ExpSettings.CreateExpertSettings('DirList');
  try
    for i := 0 to Count - 1 do
      ListSettings.WriteString(Self[i], '');
  finally
    FreeAndNil(ListSettings);
  end;
end;

function TfmToDo.PriorityToImageIndex(Priority: TToDoPriority): Integer;
begin
  if Priority = tpInfo then
    Result := ImageIndexInfo
  else if Priority = tpDone then
    Result := ImageIndexCheck
  else
    Result := ImageIndexToDoPriority + Ord(Priority);
end;

function TfmToDo.NumericPriorityToGXPriority(const PriorityStr: string): TToDoPriority;
var
  IntPriority: integer;
begin
  Result := tpNormal;
  if TryStrToInt(PriorityStr, IntPriority) then begin
    case IntPriority of
      1: Result := tpCritical;
      2: Result := tpHigh;
      3: Result := tpNormal;
      4: Result := tpLow;
      5: Result := tpLowest;
    end;
  end;
end;

{ TToDoInfo }

function TToDoInfo.NumericPriority: Integer;
begin
  Result := Ord(Priority);
end;

initialization
  RegisterGX_Expert(TToDoExpert);
end.

