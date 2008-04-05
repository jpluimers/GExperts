unit GX_CodeLib;

{$I GX_CondDefine.inc}

//TODO 3 -cIssue -oAnyone: Handle invalid filenames with \/, or use Topic attribute as the display text?

interface

uses
  GX_Experts, GX_EnhancedEditor,
  Forms, Controls, StdActns, Classes, ActnList,
  Dialogs, Menus, ComCtrls, ToolWin, ExtCtrls, GpStructuredStorage,
  GX_GenericUtils;

type
  TSearchRecord = record
    Text: WideString;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
  end;

  TCodeLayout = (clSide, clTop);

  TGXStorageFile = class(TObject)
  private
    FStorage: IGpStructuredStorage;
    FFile: TStream;
    FFileName: WideString;
    function GetFileText: WideString;
    procedure SetFileText(const Value: WideString);
    procedure AssertFileIsOpen;
    procedure AssertValidFolderName(FolderName: WideString);
    procedure AssertValidFileName(FileName: WideString);
    procedure AssertExistingObjectName(const ObjectName: WideString);
    function GetFileInfo: IGpStructuredFileInfo;
  public
    constructor Create(const FileName: WideString);
    procedure CompactStorage;
    procedure CloseStorage;
    procedure SaveStorage;

    procedure OpenFile(const FullPath: WideString); overload;
    procedure OpenFile(const FolderName, FileName: WideString); overload;
    procedure Delete(const ObjectName: WideString);
    procedure Move(const OldName, NewName: WideString);
    procedure CloseFile;
    procedure CreateFolder(const FolderName: WideString);
    function FolderExists(const FolderName: WideString): Boolean;
    function FileExists(const FileName: WideString): Boolean;
    function ObjectExists(const ObjectName: WideString): Boolean;

    procedure ListFiles(const FolderName: WideString; Files: TStrings);
    procedure ListFolders(const FolderName: WideString; Files: TStrings);

    property FileText: WideString read GetFileText write SetFileText;
    function AttributeAsString(const AttrName: WideString; const Default: WideString = ''): WideString;
    procedure SetAttribute(const AttrName: WideString; const Value: WideString); overload;
    procedure SetAttribute(const AttrName: WideString; const Value: Integer); overload;
    procedure SetObjectAttribute(const ObjectName, AttrName, AttrValue: WideString);
    function GetObjectAttribute(const ObjectName, AttrName: WideString): WideString;
  end;

  TfmCodeLib = class(TForm)
    StatusBar: TStatusBar;
    Splitter: TSplitter;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileNew: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    mitFilePrint: TMenuItem;
    mitFileSep3: TMenuItem;
    mitFileExit: TMenuItem;
    mitEdit: TMenuItem;
    mitEditPaste: TMenuItem;
    mitEditCopy: TMenuItem;
    mitEditCut: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFileDelete: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    pnlView: TPanel;
    pmTopics: TPopupMenu;
    mitTreeNew: TMenuItem;
    mitTreeDelete: TMenuItem;
    pmCode: TPopupMenu;
    mitEditorCut: TMenuItem;
    mitEditorCopy: TMenuItem;
    mitEditorPaste: TMenuItem;
    mitEditorSep2: TMenuItem;
    mitEditorHighlighting: TMenuItem;
    mitPascal: TMenuItem;
    mitCPP: TMenuItem;
    mitEditSep1: TMenuItem;
    mitEditCopyFromIde: TMenuItem;
    mitEditPasteFromIde: TMenuItem;
    mitEditorSep1: TMenuItem;
    mitEditorCopyFromDelphi: TMenuItem;
    mitEditorPasteIntoDelphi: TMenuItem;
    mitEditSep2: TMenuItem;
    mitEditFind: TMenuItem;
    mitEditFindNext: TMenuItem;
    mitFileSep1: TMenuItem;
    mitNone: TMenuItem;
    mitFileNewRootFolder: TMenuItem;
    mitFileNewFolder: TMenuItem;
    mitFileNewSnippet: TMenuItem;
    mitTreeNewRootFolder: TMenuItem;
    mitTreeNewFolder: TMenuItem;
    mitTreeNewSnippet: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsOptions: TMenuItem;
    mitTreeMakeRoot: TMenuItem;
    mitEditSep3: TMenuItem;
    mitEditExpandAll: TMenuItem;
    mitEditContractAll: TMenuItem;
    mitHTML: TMenuItem;
    mitSQL: TMenuItem;
    tvTopics: TTreeView;
    Actions: TActionList;
    actDelete: TAction;
    actNewRootFolder: TAction;
    actNewFolder: TAction;
    actNewSnippet: TAction;
    actMakeRoot: TAction;
    actPrinterSetup: TAction;
    actPrint: TAction;
    actExit: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditCopyFromIde: TAction;
    actEditPasteToIde: TAction;
    actEditFind: TAction;
    actEditFindNext: TAction;
    actExpandAll: TAction;
    actContractAll: TAction;
    ToolBar: TToolBar;
    tbnNewFolder: TToolButton;
    tbnNewSnippet: TToolButton;
    tbnDelete: TToolButton;
    tbnCut: TToolButton;
    tbnSep1: TToolButton;
    tbnCopy: TToolButton;
    tbnPaste: TToolButton;
    tbnSep2: TToolButton;
    tbnCopyIde: TToolButton;
    tbnPasteIde: TToolButton;
    tbnSep3: TToolButton;
    tbnExpandAll: TToolButton;
    tbnContractAll: TToolButton;
    tbnSep4: TToolButton;
    tbnFind: TToolButton;
    actOptions: TAction;
    actHelpAbout: TAction;
    actHelpContents: TAction;
    actHelpHelp: TAction;
    tbnFindNext: TToolButton;
    actSyntaxNone: TAction;
    actSyntaxPascal: TAction;
    actSyntaxCpp: TAction;
    actSyntaxHtml: TAction;
    actSyntaxSql: TAction;
    actEditRename: TAction;
    mitTreeRename: TMenuItem;
    mitFileSep2: TMenuItem;
    actCompactStorage: TAction;
    CompactStorage1: TMenuItem;
    procedure CodeTextChange(Sender: TObject);
    procedure tvTopicsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure DeleteExecute(Sender: TObject);
    procedure PrinterSetupExecute(Sender: TObject);
    procedure PrintExecute(Sender: TObject);
    procedure ExitExecute(Sender: TObject);
    procedure CutExecute(Sender: TObject);
    procedure CopyExecute(Sender: TObject);
    procedure PasteExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure CopyFromIdeExecute(Sender: TObject);
    procedure PasteToIdeExecute(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindNextExecute(Sender: TObject);
    procedure tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ExpandAllExecute(Sender: TObject);
    procedure ContractAllExecute(Sender: TObject);
    procedure HelpExecute(Sender: TObject);
    procedure HelpContentsExecute(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure NewSnippetExecute(Sender: TObject);
    procedure NewRootFolderExecute(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure tvTopicsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OptionsExecute(Sender: TObject);
    procedure tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure MakeRootExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure GenericSyntaxHighlightingExecute(Sender: TObject);
    procedure tvTopicsDblClick(Sender: TObject);
    procedure actEditRenameExecute(Sender: TObject);
    procedure tvTopicsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure tvTopicsCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure actCompactStorageExecute(Sender: TObject);
    function GetUniqueTopicName(ParentNode: TTreeNode; Folder: Boolean): WideString;
  private
    FModified: Boolean;
    FSearch: TSearchRecord;
    FLayout: TCodeLayout;
    FStoragePath: WideString;
    FCodeText: TGxEnhancedEditor;
    FCurrentSyntaxMode: TGXSyntaxHighlighter;
    CodeDB: TGXStorageFile;
    function OpenStorage(const StorageFile: WideString): TGXStorageFile;
    procedure CloseDB(ClearFileName: Boolean = False);
    procedure InitializeTreeView;
    procedure SaveRecord;
    procedure DoSearch(First: Boolean);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure SetModified(New: Boolean);
    procedure SetLayout(New: TCodeLayout);
    procedure SortNodes;
    procedure SetupSyntaxHighlightingControl;
    function IsCodeSnippet(Node: TTreeNode): Boolean;
    function IsFolder(Node: TTreeNode): Boolean;
    function ConfigurationKey: string;
    procedure AddNewNode(Folder: Boolean);
    function GetNodePath(Node: TTreeNode): WideString;
    function GetNodeParentPath(Node: TTreeNode): WideString;
    procedure AssertValidFileName(const FileName: WideString);
    function HaveSelectedNode: Boolean;
    function SelectedNodeFullName: WideString;
    function SelectedCaption: WideString;
    procedure AssertSelectedNode;
    function FolderSelected: Boolean;
    procedure AddFolder(ParentNode: TTreeNode);
    procedure AddCode;
    property Modified: Boolean read FModified write SetModified;
    property StoragePath: WideString read FStoragePath write FStoragePath;
    procedure SetNodeAttribute(Node: TTreeNode; const AttrName, AttrValue: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Layout: TCodeLayout read FLayout write SetLayout;
  end;

  TCodeLibExpert = class(TGX_Expert)
  protected
    FCodeLibForm: TfmCodeLib;
    procedure SetActive(New: Boolean); override;
    procedure CreateCodeLibForm;
  public
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

procedure ShowCodeLib; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

{$R *.dfm}

// The built-in IDE parser/scanner has problems
// with a few conditional defines we use.
// Make it happy with code that *it* sees,
// but is never compiled by the real compiler.
{$UNDEF IdeParserPacifier_NEVERCOMPILED}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Windows, Clipbrd,
  GX_CodeSrch, GX_CodeOpt, GX_GxUtils,
  GX_OtaUtils, GX_IdeUtils,
  GX_GExperts, GX_ConfigurationInfo, GX_MessageBox, GX_SharedImages;

const
  DefaultFileName = 'CodeLibrarian.fs';
  CodeLibPathSep = '\';
  AttrTopic = 'Topic';
  AttrLanguage = 'Language';

resourcestring
  SCouldNotCreateStorage = 'Could not create Code Librarian storage file.';

type
  TReadOnlyCodeLibFileMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function MakeFileName(const FolderName, FileName: WideString): WideString;
begin
  Result := FolderName;
  if Result = '' then
    Result := CodeLibPathSep
  else
    Result := AddSlash(Result);
  if FileName <> '' then
  begin
    if FileName[1] = CodeLibPathSep then
      Result := Result + Copy(FileName, 2, 9999999)
    else
      Result := Result + FileName;
  end;
end;

function TfmCodeLib.OpenStorage(const StorageFile: WideString): TGXStorageFile;
begin
  Result := TGXStorageFile.Create(StorageFile);
  {$IFOPT D+}SendDebug('Created new CodeLib storage file: ' + StorageFile);{$ENDIF}
end;

procedure TfmCodeLib.InitializeTreeView;

  procedure LoadTreeView(Node: TTreeNode);
  var
    RNode: TTreeNode;
    Folders: TStringList;
    Files: TStringList;
    i: Integer;
    RootPath: WideString;
  begin
    Folders := TStringList.Create;
    Files := TStringList.Create;
    try
      RootPath := GetNodePath(Node);
      CodeDB.ListFolders(RootPath, Folders);
      for i := 0 to Folders.Count - 1 do
      begin
        RNode := tvTopics.Items.AddChild(Node, CodeDB.GetObjectAttribute(MakeFileName(RootPath, Folders[i]), AttrTopic));
        RNode.ImageIndex := ImageIndexClosedFolder;
        RNode.SelectedIndex := ImageIndexOpenFolder;
        LoadTreeView(RNode);
      end;
      CodeDB.ListFiles(RootPath, Files);
      for i := 0 to Files.Count - 1 do
      begin
        RNode := tvTopics.Items.AddChild(Node, CodeDB.GetObjectAttribute(MakeFileName(RootPath, Files[i]), AttrTopic));
        RNode.ImageIndex := ImageIndexDocument;
        RNode.SelectedIndex := ImageIndexDocument;
      end;
    finally
      FreeAndNil(Files);
      FreeAndNil(Folders);
    end;
  end;

begin
  tvTopics.SortType := stNone;
  tvTopics.Items.BeginUpdate;
  try
    tvTopics.Items.Clear;
    LoadTreeView(nil);
    tvTopics.Selected := nil;
  finally
    tvTopics.SortType := stText;
    tvTopics.Items.EndUpdate;
  end;
  {$IFOPT D+}SendDebug('Finished creating tree view nodes');{$ENDIF}
end;

procedure TfmCodeLib.SetLayout(New: TCodeLayout);
begin
  if FLayout <> New then
  begin
    FLayout := New;
    case FLayout of
      clSide:
        begin
          tvTopics.Align := alLeft;
          tvTopics.Width := Self.Width div 2;
          Splitter.Align := alLeft;
          if Splitter.Left < tvTopics.Left then
            Splitter.Left := Self.Width;
          Splitter.Cursor := crHSplit;
          FCodeText.Align := AlClient;
        end;
      clTop:
        begin
          tvTopics.Align := alTop;
          tvTopics.Height := Self.Height div 2;
          Splitter.Align := alTop;
          if Splitter.Top < tvTopics.Top then
            Splitter.Top := Self.Height;
          FCodeText.Align := AlClient;
          Splitter.Cursor := crVSplit;
        end;
    end;
  end;
end;

procedure TfmCodeLib.AddFolder(ParentNode: TTreeNode);
var
  NewNode: TTreeNode;
  NewFolder: WideString;
  NewTopic: WideString;
begin
  NewTopic := GetUniqueTopicName(ParentNode, True);
  NewFolder := AddSlash(GetNodePath(ParentNode)) + NewTopic;
  CodeDB.AssertValidFolderName(NewFolder);
  CodeDB.CreateFolder(NewFolder);
  Assert(CodeDB.FolderExists(NewFolder));
  NewNode := tvTopics.Items.AddChild(ParentNode, NewTopic);
  SetNodeAttribute(NewNode, AttrTopic, NewTopic);
  NewNode.ImageIndex := ImageIndexClosedFolder;
  NewNode.SelectedIndex := ImageIndexOpenFolder;
  SortNodes;
  tvTopics.Selected := NewNode;
  tvTopics.Selected.EditText;
end;

procedure TfmCodeLib.SetModified(New: Boolean);
resourcestring
  SModified = 'Modified';
begin
  FModified := New;
  if FModified then
    StatusBar.Panels[1].Text := SModified
  else
    StatusBar.Panels[1].Text := ''; // No need to localize.
end;

procedure TfmCodeLib.AddCode;
var
  Node: TTreeNode;
  NewFileName: WideString;
  TopicName: WideString;
begin
  if not HaveSelectedNode then
    Exit;
  TopicName := GetUniqueTopicName(tvTopics.Selected, False);
  NewFileName := AddSlash(SelectedNodeFullName) + TopicName;
  CodeDB.OpenFile(NewFileName);
  Assert(CodeDB.FileExists(NewFileName));
  CodeDB.SetAttribute(AttrTopic, TopicName);
  if mitPascal.Checked then
    CodeDB.SetAttribute(AttrLanguage, 'P')
  else
    CodeDB.SetAttribute(AttrLanguage, 'C');
  CodeDB.SaveStorage;

  Node := tvTopics.Items.AddChild(tvTopics.Selected, TopicName);
  Node.ImageIndex := ImageIndexDocument;
  Node.SelectedIndex := ImageIndexDocument;

  SortNodes;
  tvTopics.Selected := Node;
  tvTopics.Selected.EditText;
end;

procedure TfmCodeLib.CodeTextChange(Sender: TObject);
begin
  Modified := FCodeText.Modified;
end;

procedure TfmCodeLib.tvTopicsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;
  // Do not alter value of AllowChange.
end;

procedure TfmCodeLib.SaveRecord;
var
  LangType: WideString;
begin
  // This is called from the destructor where
  // we may be in a forced clean-up state due
  // to an exception in the constructor.
  if ExceptObject <> nil then
    Exit;

  Modified := False;
  if not HaveSelectedNode then
    Exit;

  SetNodeAttribute(tvTopics.Selected, AttrTopic, SelectedCaption);
  if FolderSelected then
  begin
    Assert(CodeDB.FolderExists(GetNodePath(tvTopics.Selected)));
  end
  else
  begin
    CodeDB.FileText :=  FCodeText.Lines.Text;

    if mitPascal.Checked then
      LangType := 'P'
    else if mitCPP.Checked then
      LangType := 'C'
    else if mitHTML.Checked then
      LangType := 'H'
    else if mitSQL.Checked then
      LangType := 'S'
    else
      LangType := 'N';
    CodeDB.SetAttribute(AttrLanguage, LangType);

    CodeDB.SaveStorage;
  end;
end;

procedure TfmCodeLib.tvTopicsEdited(Sender: TObject; Node: TTreeNode; var S: string);
var
  NewFileName: WideString;
begin
  Modified := True;
  AssertValidFileName(S);
  NewFileName := AddSlash(GetNodeParentPath(Node)) + S;
  if CodeDB.FileExists(NewFileName) or CodeDB.FolderExists(NewFileName) then
    raise Exception.CreateFmt('An item named %s already exists.', [S]);
  SetNodeAttribute(Node, AttrTopic, S);
  CodeDB.Move(GetNodePath(Node), NewFileName);
  SortNodes;
end;

procedure TfmCodeLib.tvTopicsChange(Sender: TObject; Node: TTreeNode);
var
  LangType: WideString;
begin
  try
    if (Node <> nil) and (IsCodeSnippet(Node)) then
    begin
      CodeDB.OpenFile(GetNodePath(Node));
      FCodeText.Lines.BeginUpdate;
      try
        LangType := CodeDB.AttributeAsString(AttrLanguage, 'P');
        if LangType = 'N' then  // Do not localize.
        begin
          // This is raw text
          mitNone.Checked := True;
          FCodeText.Highlighter := gxpNone;
        end
        else
        if LangType = 'C' then  // Do not localize.
        begin
          // This is CPP source code
          mitCPP.Checked := True;
          FCodeText.Highlighter := gxpCPP;
        end
        else
        if LangType = 'H' then  // Do not localize.
        begin
          // This is HTML source code
          mitHTML.Checked := True;
          FCodeText.Highlighter := gxpHTML;
        end
        else
        if LangType = 'S' then  // Do not localize.
        begin
          // This is SQL code
          mitSQL.Checked := True;
          FCodeText.Highlighter := gxpSQL;
        end
        else
        begin
          // This is Object Pascal source code.
          mitPascal.Checked := True;
          FCodeText.Highlighter := gxpPAS;
        end;
        FCodeText.Lines.Text := CodeDB.FileText;
      finally
        FCodeText.Lines.EndUpdate;
      end;
      FCodeText.ReadOnly := False;
    end
    else
      FCodeText.Lines.Clear;

    Modified := False;
  except
    on E: Exception do
      GxLogAndShowException(E);
  end;
end;

procedure TfmCodeLib.DeleteExecute(Sender: TObject);
resourcestring
  SSnippet = 'snippet';
  SFolder = 'folder';
  SConfirmDelete = 'Delete this %s?';
var
  NodeType: WideString;
begin
  if not HaveSelectedNode then
    Exit;
  if FolderSelected then
    NodeType := SFolder
  else
    NodeType := SSnippet;
  if MessageDlg(Format(SConfirmDelete, [NodeType]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    CodeDB.Delete(SelectedNodeFullName);
    tvTopics.Selected.Delete;
  end;
end;

procedure TfmCodeLib.PrinterSetupExecute(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;

procedure TfmCodeLib.PrintExecute(Sender: TObject);
resourcestring
  RS_PRINTTITLE = 'GExperts';
begin
  if tvTopics.Selected <> nil then
    FCodeText.Print(tvTopics.Selected.Text)
  else
    FCodeText.Print(RS_PRINTTITLE);
end;

procedure TfmCodeLib.ExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmCodeLib.CutExecute(Sender: TObject);
begin
  FCodeText.CutToClipboard;
end;

procedure TfmCodeLib.CopyExecute(Sender: TObject);
begin
  FCodeText.CopyToClipboard;
end;

procedure TfmCodeLib.PasteExecute(Sender: TObject);
begin
  if not FCodeText.ReadOnly then
    FCodeText.PasteFromClipBoard
end;

procedure TfmCodeLib.HelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

resourcestring
  SNotForFormFiles = 'Copy/Paste is not allowed in form files.';

procedure TfmCodeLib.CopyFromIdeExecute(Sender: TObject);
var
  FileName: WideString;
begin
  FileName := GxOtaGetCurrentSourceFile;
  if IsForm(FileName) then
    raise Exception.Create(SNotForFormFiles);

  FCodeText.SelText := GxOtaGetCurrentSelection;
end;

procedure TfmCodeLib.PasteToIdeExecute(Sender: TObject);
var
  FileName: WideString;
begin
  FileName := GxOtaGetCurrentSourceFile;
  if IsForm(FileName) then
    raise Exception.Create(SNotForFormFiles);

  GxOtaInsertTextIntoEditor(FCodeText.Text);
  Hide;
end;

procedure TfmCodeLib.FindExecute(Sender: TObject);
begin
  try
    with TfmCodeSearch.Create(nil) do
    try
      if ShowModal = mrOk then
      begin
        FSearch.Text := edSearch.Text;
        FSearch.CaseSensitive := cbCaseSensitive.Checked;
        FSearch.WholeWord := cbWholeWord.Checked;
        DoSearch(True);
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
      GxLogAndShowException(E);
  end;
end;

procedure TfmCodeLib.DoSearch(First: Boolean);

  function DoMatch(const Text: WideString): Integer;
  var
    MatchPos: Integer;
  begin
    Result := -1;

    if FSearch.CaseSensitive then
      MatchPos := AnsiPos(FSearch.Text, Text)
    else
      MatchPos := AnsiCaseInsensitivePos(FSearch.Text, Text);

    if (MatchPos > 0) and FSearch.WholeWord then
    begin
      // If the previous character is alphabetic, there isn't a match
      if MatchPos > 1 then
        if IsCharAlphaW(Text[MatchPos - 1]) then
          Exit;
      // If the next character is alphabetic, we didn't find a word match
      if MatchPos + Length(FSearch.Text) <= Length(Text) then
        if IsCharAlphaW(Text[MatchPos + Length(FSearch.Text)]) then
          Exit;
    end;
    Result := MatchPos;
  end;

var
  Node: TTreeNode;
  Match: Integer;
  InTopic: Boolean;
  FirstLoop: Boolean;
  NodePath: WideString;
  IsSnippet: Boolean;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      Node := nil;
      if not First then
      begin
        //if ActiveControl = FCodeText then
          Node := tvTopics.Selected
        //else
        //  Node := tvTopics.Selected.GetNext;
      end;
      if First or (Node = nil) then
        Node := tvTopics.Items.GetFirstNode;
      Match := 0;
      InTopic := False;
      FirstLoop := True;
      while Node <> nil do
      begin
        NodePath := GetNodePath(Node);
        IsSnippet := IsCodeSnippet(Node);
        if IsSnippet then
          CodeDB.OpenFile(NodePath)
        else
          CodeDB.CloseFile;
        //{$IFOPT D+}SendDebug('Starting search from '+Node.Text+ ' for '+IntToStr(Integer(Node.Data)));{$ENDIF}
        begin
          //{$IFOPT D+}SendDebug('Found the key: '+IntToStr(Integer(Node.Data)));{$ENDIF}
          if FirstLoop and IsSnippet and (FCodeText.Focused) and (Length(FCodeText.SelText) > 0) then
          begin
            InTopic := False;
            Match := DoMatch(Copy(CodeDB.FileText, FCodeText.SelStart + Length(FCodeText.SelText) + 1, 999999));
            //{$IFOPT D+}SendDebug('InterText search found '+FSearch.Text+' at '+IntToStr(Match)+' in '+Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + FCodeText.SelLength, 999999));{$ENDIF}
            if Match > 0 then
            begin
              //{$IFOPT D+}SendDebug('Found a match at position '+IntToStr(Match)+'("'+Copy(Copy(CodeDB.FieldByName('Code').AsString, GetByteSelStart + FCodeText.SelLength, 999999), 1, 15)+'")'+' SelStart = '+IntToStr(FCodeText.SelStart)+' SelLength = '+IntToStr(FCodeText.SelLength));{$ENDIF}
              //FCodeText.Perform(EM_LINEFROMCHAR, FCodeText.SelStart, 0);
              Match := Match + FCodeText.SelStart + Length(FCodeText.SelText);
              //{$IFOPT D+}SendDebug('Matched Text: "'+Copy(CodeDB.FieldByName('Code').AsString, Match, 12)+'"');{$ENDIF}
            end;
          end
          else // Search the complete topic and code text
          begin
            if not FirstLoop then
            begin
              InTopic := True;
              Match := DoMatch(Node.Text);
            end;
            //{$IFOPT D+}SendDebug('Topic match on '+CodeDB.FieldByName(AttrTopic).AsString+' returned '+IntToStr(Match));{$ENDIF}
            if (Match = 0) and IsSnippet then
            begin
              Match := DoMatch(CodeDB.FileText);
              InTopic := False;
              //{$IFOPT D+}SendDebug('Code match on '+CodeDB.FieldByName('Code').AsString+' returned '+IntToStr(Match));{$ENDIF}
            end;
          end;
          if Match > 0 then Break;
        end;
        Node := Node.GetNext;
        FirstLoop := False;
      end;
      if Node = nil then
        SysUtils.Beep;
      if Match > 0 then
      begin
        //{$IFOPT D+}SendDebug('Found a match!  InTopic: '+BooleanText(InTopic));{$ENDIF}
        //{$IFOPT D+}SendDebug('Match Text: '+Copy(FCodeText.Lines.Text, Match, 10));{$ENDIF}
        tvTopics.Selected := Node;
        if InTopic then
          tvTopics.SetFocus
        else
        begin
          FCodeText.SetFocus;
          Dec(Match);
          FCodeText.SetSelection(Match, Length(FSearch.Text));
          //{$IFOPT D+}SendDebug('Focused Text: '+Copy(FCodeText.Lines.Text, Match - 1, 10));{$ENDIF}
        end;
      end;
    except
      on E: Exception do
        GxLogAndShowException(E);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmCodeLib.FindNextExecute(Sender: TObject);
begin
  if FSearch.Text <> '' then
    DoSearch(False)
  else
    actEditFind.Execute;
end;

procedure TfmCodeLib.tvTopicsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DestNode: TTreeNode;
begin
  try
    if tvTopics.Selected = nil then
      Exit;
    DestNode := tvTopics.GetNodeAt(X, Y);
    if (DestNode = nil) or (DestNode = tvTopics.Selected) then
      Exit;
    if IsCodeSnippet(DestNode) or DestNode.HasAsParent(tvTopics.Selected) then
      Exit;
    CodeDB.Move(SelectedNodeFullName, AddSlash(GetNodePath(DestNode)) + SelectedCaption);
    CodeDB.SaveStorage;
    tvTopics.Selected.MoveTo(DestNode, naAddChild);
    DestNode.AlphaSort;
  except
    on E: Exception do
      GxLogAndShowException(E);
  end;
end;

procedure TfmCodeLib.tvTopicsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Sender);
end;

procedure TfmCodeLib.ExpandAllExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Expand(True);
    Node := Node.GetNextSibling;
  end;
end;

procedure TfmCodeLib.ContractAllExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  // OnChanging doesn't fire under Delphi 5 when calling Collapse below
  if (tvTopics.Selected <> nil) and Modified then
    SaveRecord;

  Node := tvTopics.Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;

  // OnChange doesn't fire under Delphi 5 when calling Collapse above
  tvTopicsChange(tvTopics, tvTopics.Selected);
end;

procedure TfmCodeLib.SaveSettings;
var
  Settings: TGExpertsSettings;
  BaseKey: string;
begin
  // Do not localize any of the following lines.
  BaseKey := ConfigInfo.GExpertsIdeRootRegistryKey;

  Settings := TGExpertsSettings.Create(BaseKey);
  try
    Settings.WriteInteger(ConfigurationKey, 'Left', Left);
    Settings.WriteInteger(ConfigurationKey, 'Top', Top);
    Settings.WriteInteger(ConfigurationKey, 'Width', Width);
    Settings.WriteInteger(ConfigurationKey, 'Height', Height);
    Settings.WriteInteger(ConfigurationKey, 'Layout', Ord(Layout));
    if Layout = clSide then
      Settings.WriteInteger(ConfigurationKey, 'Splitter', tvTopics.Width)
    else
      Settings.WriteInteger(ConfigurationKey, 'Splitter', tvTopics.Height);
    Settings.WriteString(ConfigurationKey, 'StoragePath', StoragePath);
  finally
    FreeAndNil(Settings);
  end;

  BaseKey := AddSlash(BaseKey) + ConfigurationKey;
  Settings := TGExpertsSettings.Create(BaseKey);
  try
    RegSaveFont(Settings, 'Editor', FCodeText.Font);
    RegSaveFont(Settings, 'TreeView', tvTopics.Font);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmCodeLib.LoadSettings;
var
  Settings: TGExpertsSettings;
  BaseKey: string;
begin
  // Do not localize any of the following lines.
  BaseKey := ConfigInfo.GExpertsIdeRootRegistryKey;

  Settings := TGExpertsSettings.Create(BaseKey);
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
    Width := Settings.ReadInteger(ConfigurationKey, 'Width', Width);
    Height := Settings.ReadInteger(ConfigurationKey, 'Height', Height);
    Layout := TCodeLayout(Settings.ReadInteger(ConfigurationKey, 'Layout', 0));
    if Layout = clSide then
      tvTopics.Width := Settings.ReadInteger(ConfigurationKey, 'Splitter', tvTopics.Width)
    else
      tvTopics.Height := Settings.ReadInteger(ConfigurationKey, 'Splitter', tvTopics.Height);
    StoragePath := Settings.ReadString(ConfigurationKey, 'StoragePath', StoragePath);
  finally
    FreeAndNil(Settings);
  end;

  BaseKey := AddSlash(BaseKey) + ConfigurationKey;
  Settings := TGExpertsSettings.Create(BaseKey);
  try
    RegLoadFont(Settings, 'Editor', FCodeText.Font);
    RegLoadFont(Settings, 'TreeView', tvTopics.Font);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmCodeLib.HelpExecute(Sender: TObject);
begin
{$IFNDEF STANDALONE}
  GxContextHelp(Self, 17);
{$ENDIF STANDALONE}
end;

procedure TfmCodeLib.HelpContentsExecute(Sender: TObject);
begin
{$IFNDEF STANDALONE}
  GxContextHelpContents(Self);
{$ENDIF STANDALONE}
end;

procedure TfmCodeLib.StatusBarResize(Sender: TObject);
begin
  with StatusBar do
    Panels[0].Width := Width - Panels[1].Width;
end;

procedure TfmCodeLib.NewRootFolderExecute(Sender: TObject);
begin
  AddFolder(nil);
end;

procedure TfmCodeLib.NewSnippetExecute(Sender: TObject);
begin
  AddNewNode(False);
end;

procedure TfmCodeLib.NewFolderExecute(Sender: TObject);
begin
  AddNewNode(True);
end;

procedure TfmCodeLib.AddNewNode(Folder: Boolean);
begin
  if Folder then
    AddFolder(tvTopics.Selected)
  else
    AddCode;
end;

procedure TfmCodeLib.tvTopicsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F2) and (tvTopics.Selected <> nil) then
    tvTopics.Selected.EditText;
end;

procedure TfmCodeLib.OptionsExecute(Sender: TObject);
var
  Dlg: TfmCodeOptions;
begin
  Dlg := TfmCodeOptions.Create(nil);
  try
    with Dlg do
    begin
      edPath.Text := StoragePath;
      if Layout = clSide then
        rbSide.Checked := True
      else
        rbTop.Checked := True;
      {$IFOPT D+}SendDebug('Setting fcTreeView.Text to '+tvTopics.Font.Name);{$ENDIF}
      fcTreeView.ItemIndex := fcTreeView.Items.IndexOf(tvTopics.Font.Name);
      {$IFOPT D+}SendDebug('fcTreeView.Text is '+fcTreeView.Text);{$ENDIF}
      udTreeView.Position := tvTopics.Font.Size;
      fcEditor.ItemIndex := fcEditor.Items.IndexOf(FCodeText.Font.Name);
      udEditor.Position := FCodeText.Font.Size;
    end;

    if Dlg.ShowModal = mrOk then
    begin
      if (StoragePath <> Dlg.edPath.Text) then
      begin
        if CodeDB <> nil then
          CloseDB(True);

        FreeAndNil(CodeDB);
        tvTopics.Items.Clear;
        FCodeText.Clear;
        StoragePath := AddSlash(Dlg.edPath.Text);
        CodeDB := OpenStorage(StoragePath + DefaultFileName);
        if CodeDB = nil then
        begin
          MessageDlg(SCouldNotCreateStorage, mtError, [mbOK], 0);
          Exit;
        end;
        InitializeTreeView;
      end;
      if Dlg.rbSide.Checked then
        Layout := clSide
      else
        Layout := clTop;

      with tvTopics.Font do
      begin
        Name := Dlg.fcTreeView.Text;
        Size := Trunc(StrToInt(Dlg.eTreeView.Text));
      end;
      with FCodeText.Font do
      begin
        Name := Dlg.fcEditor.Text;
        Size := Trunc(StrToInt(Dlg.eEditor.Text));
      end;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmCodeLib.tvTopicsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  AutoScroll := True;
end;

procedure TfmCodeLib.tvTopicsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  AutoScroll := False;
end;

procedure TfmCodeLib.MakeRootExecute(Sender: TObject);
var
  Sel: TTreeNode;
begin
  Sel := tvTopics.Selected;
  if ((Sel <> nil) and (Sel.Level > 0) and (not IsCodeSnippet(Sel))) then
  begin
    CodeDB.Move(SelectedNodeFullName, CodeLibPathSep + SelectedCaption);
    tvTopics.Selected.MoveTo(nil, naAdd);
    Modified := True;
    SortNodes;
  end;
end;

procedure TfmCodeLib.FormHide(Sender: TObject);
begin
  if FModified then
    SaveRecord;
  CloseDB;
end;

procedure TfmCodeLib.FormShow(Sender: TObject);
begin
  // CodeDB.Open;
end;

procedure TfmCodeLib.SortNodes;
begin
  tvTopics.AlphaSort;
end;

procedure TfmCodeLib.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  HaveEditorSelection: Boolean;
  HaveSelectedNode: Boolean;
  SnippetIsSelected: Boolean;
begin
  HaveEditorSelection := Length(FCodeText.SelText) > 0;
  actEditCut.Enabled := HaveEditorSelection;
  actEditCopy.Enabled := HaveEditorSelection;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FCodeText.ReadOnly));

  HaveSelectedNode  := tvTopics.Selected <> nil;
  SnippetIsSelected := HaveSelectedNode and IsCodeSnippet(tvTopics.Selected);

  actEditPasteToIde.Enabled := SnippetIsSelected and not IsStandalone;
  actEditCopyFromIde.Enabled := SnippetIsSelected and not IsStandalone;
  actDelete.Enabled := HaveSelectedNode;
  actEditRename.Enabled := HaveSelectedNode;

  if not HaveSelectedNode then
  begin
    actMakeRoot.Enabled := False;
    actNewSnippet.Enabled := False;
  end
  else
  begin
    actMakeRoot.Enabled := (not (tvTopics.Selected.Level = 0)) and (not SnippetIsSelected);
    actNewSnippet.Enabled := not SnippetIsSelected;
    actNewFolder.Enabled := not SnippetIsSelected;
  end;

  actSyntaxNone.Checked := (FCurrentSyntaxMode = gxpNone);
  actSyntaxPascal.Checked := (FCurrentSyntaxMode = gxpPAS);
  actSyntaxCpp.Checked := (FCurrentSyntaxMode = gxpCPP);
  actSyntaxHtml.Checked := (FCurrentSyntaxMode = gxpHTML);
  actSyntaxSql.Checked := (FCurrentSyntaxMode = gxpSQL);
  FCodeText.Enabled := SnippetIsSelected;
  FCodeText.ReadOnly := not SnippetIsSelected;

  Handled := True;
end;

procedure TfmCodeLib.GenericSyntaxHighlightingExecute(Sender: TObject);
begin
  Modified := True;

  if Sender = actSyntaxNone then
    FCurrentSyntaxMode := gxpNone
  else if Sender = actSyntaxPascal then
    FCurrentSyntaxMode := gxpPAS
  else if Sender = actSyntaxCpp then
    FCurrentSyntaxMode := gxpCPP
  else if Sender = actSyntaxHtml then
    FCurrentSyntaxMode := gxpHTML
  else if Sender = actSyntaxSql then
    FCurrentSyntaxMode := gxpSQL
  else
    raise Exception.Create('Internal error selecting language');

  FCodeText.Highlighter := FCurrentSyntaxMode;
end;

procedure TfmCodeLib.SetupSyntaxHighlightingControl;
begin
  FCurrentSyntaxMode := gxpPas;
  FCodeText := TGXEnhancedEditor.Create(Self);
  FCodeText.Highlighter := FCurrentSyntaxMode;
  FCodeText.Align := alClient;
  FCodeText.PopupMenu := pmCode;
  FCodeText.OnChange := CodeTextChange;
  FCodeText.Parent := pnlView;
  FCodeText.ReadOnly := True;
end;

constructor TfmCodeLib.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetNonModalFormPopupMode(Self);
  SetToolbarGradient(ToolBar);
  SetDefaultFont(Self);
  SetupSyntaxHighlightingControl;

  Screen.Cursor := crHourglass;
  try
    CodeDB := nil;
    {$IFOPT D+}SendDebug('Setting CodeLib storage path');{$ENDIF}
    StoragePath := AddSlash(ConfigInfo.ConfigPath);
    {$IFOPT D+}SendDebug('Storage path: ' + StoragePath);{$ENDIF}
    FLayout := clSide;

    FModified := False;
    CenterForm(Self);
    {$IFOPT D+}SendDebug('Loading CodeLib settings');{$ENDIF}
    LoadSettings;
    {$IFOPT D+}SendDebug('Opening CodeLib storage');{$ENDIF}
    CodeDB := OpenStorage(StoragePath + DefaultFileName); // do not localize
    if CodeDB = nil then
    begin
      MessageDlg(SCouldNotCreateStorage, mtError, [mbOK], 0);
      Exit;
    end;
    {$IFOPT D+}SendDebug('Opened storage file');{$ENDIF}
    InitializeTreeView;
    mitPascal.Checked := True;
    FModified := False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

destructor TfmCodeLib.Destroy;
begin
  if FModified then
    SaveRecord;

  {$IFOPT D+}SendDebug('Saving code librarian settings');{$ENDIF}
  SaveSettings;

  {$IFOPT D+}SendDebug('Freeing CodeDB');{$ENDIF}
  CloseDB(True);
  FreeAndNil(CodeDB);

  inherited;
end;

procedure TfmCodeLib.tvTopicsDblClick(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
  begin
    if IsCodeSnippet(tvTopics.Selected) then
      actEditPasteToIde.Execute;
  end;
end;

procedure TfmCodeLib.actEditRenameExecute(Sender: TObject);
begin
  if tvTopics.Selected <> nil then
    tvTopics.Selected.EditText
end;

procedure TfmCodeLib.tvTopicsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  // RightClickSelect in Delphi 5/6 is totally useless, so this is a workaround
  if Button = mbRight then
  begin
    Node := tvTopics.GetNodeAt(X, Y);
    if Node <> nil then
      tvTopics.Selected := Node;
  end;
end;

procedure TfmCodeLib.CloseDB(ClearFileName: Boolean);
begin
  if CodeDB <> nil then
    CodeDB.CloseStorage;
end;

procedure TfmCodeLib.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actExit.Execute;
  end;
end;

procedure TfmCodeLib.tvTopicsCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := lstrcmp(PChar(Node1.Text), PChar(Node2.Text));
  if IsCodeSnippet(Node1) and (not IsCodeSnippet(Node2)) then
    Compare := 1
  else if IsCodeSnippet(Node2) and (not IsCodeSnippet(Node1)) then
    Compare := -1;
end;

function TfmCodeLib.GetNodeParentPath(Node: TTreeNode): WideString;
begin
  Assert(Assigned(Node));
  Result := CodeLibPathSep;
  if Assigned(Node.Parent) then
    Result := GetNodePath(Node.Parent);
end;

procedure TfmCodeLib.actCompactStorageExecute(Sender: TObject);
begin
  CodeDB.CompactStorage;
  InitializeTreeView;
end;

function TfmCodeLib.IsCodeSnippet(Node: TTreeNode): Boolean;
begin
  Assert(Assigned(Node));
  Result := Node.ImageIndex = ImageIndexDocument;
end;

function TfmCodeLib.ConfigurationKey: string;
begin
  Result := TCodeLibExpert.ConfigurationKey;
end;

procedure TfmCodeLib.AssertValidFileName(const FileName: WideString);
resourcestring
  SlashError = 'The slash characters are not allowed in topic names';
  EmptyError = 'Blank names are not allowed';
begin
  if (Pos('/', FileName) > 0) or (Pos('\', FileName) > 0) then
    raise Exception.Create(SlashError);
  if FileName = '' then
    raise Exception.Create(EmptyError);
end;

function TfmCodeLib.GetNodePath(Node: TTreeNode): WideString;

  function GetPath(const Prefix: WideString; Node: TTreeNode): WideString;
  begin
    if not Assigned(Node) then
      Result := Prefix
    else
      Result := MakeFileName(GetPath(Prefix, Node.Parent), Node.Text);
  end;

begin
  Result := CodeLibPathSep;
  if not Assigned(Node) then
    Exit;
  Result := GetPath(Result, Node);
end;

function TfmCodeLib.HaveSelectedNode: Boolean;
begin
  Result := Assigned(tvTopics.Selected);
end;

function TfmCodeLib.SelectedNodeFullName: WideString;
begin
  AssertSelectedNode;
  Result := GetNodePath(tvTopics.Selected);
end;

function TfmCodeLib.SelectedCaption: WideString;
begin
  AssertSelectedNode;
  Result := tvTopics.Selected.Text;
end;

procedure TfmCodeLib.AssertSelectedNode;
begin
  if not HaveSelectedNode then
    raise Exception.Create('No selected node');
end;

function TfmCodeLib.FolderSelected: Boolean;
begin
  AssertSelectedNode;
  Result := IsFolder(tvTopics.Selected);
end;

function TfmCodeLib.IsFolder(Node: TTreeNode): Boolean;
begin
  Assert(Assigned(Node));
  Result := Node.ImageIndex = ImageIndexClosedFolder;
end;

procedure TfmCodeLib.SetNodeAttribute(Node: TTreeNode; const AttrName, AttrValue: WideString);
var
  ObjectName: WideString;
begin
  Assert(Assigned(Node));
  ObjectName := GetNodePath(Node);
  CodeDB.AssertExistingObjectName(ObjectName);
  CodeDB.SetObjectAttribute(ObjectName, AttrName, AttrValue);
end;

{ TReadOnlyCodeLibDBMessage }

function TReadOnlyCodeLibFileMessage.GetMessage: string;
resourcestring
  ReadOnlyCodeLibDBMsg = 'Your code librarian storage file is read only. ' +
    'Any changes you make will not be saved.';
begin
  Result := ReadOnlyCodeLibDBMsg;
end;

{ TCodeLibExpert }

destructor TCodeLibExpert.Destroy;
begin
  FreeAndNil(FCodeLibForm);
  inherited;
end;

procedure TCodeLibExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    else
    begin
      if Assigned(FCodeLibForm) then
      begin
        if FCodeLibForm.Visible then
          FCodeLibForm.Close;

        FreeAndNil(FCodeLibForm);
      end;
    end;
  end;
end;

function TCodeLibExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Code &Librarian';
begin
  Result := SMenuCaption;
end;

class function TCodeLibExpert.GetName: string;
begin
  Result := 'CodeLibrarian';
end;

procedure TCodeLibExpert.Click(Sender: TObject);
resourcestring
  SSetConfigPath = 'You must set the configuration path in the GExperts Options dialog for the Code Librarian to work.';
begin
  {$IFOPT D+}SendDebug('Activating CodeLib expert');{$ENDIF}
  if ConfigInfo.ConfigPath = '' then
  begin
    MessageDlg(SSetConfigPath, mtInformation, [mbOK], 0);
    Exit;
  end;
  CreateCodeLibForm;
  if FCodeLibForm.WindowState = wsMinimized then
    FCodeLibForm.WindowState := wsNormal;
  {$IFOPT D+}SendDebug('Showing CodeLib form');{$ENDIF}
  FCodeLibForm.Show;
end;

function TCodeLibExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TCodeLibExpert.CreateCodeLibForm;
begin
  {$IFOPT D+}SendDebug('Creating CodeLib form');{$ENDIF}
  if FCodeLibForm = nil then
  begin
    FCodeLibForm := TfmCodeLib.Create(nil);
    SetFormIcon(FCodeLibForm);
  end;
end;

procedure ShowCodeLib;
var
  CodeLibStandAlone: TCodeLibExpert;
begin
  {$IFOPT D+} SendDebug('Showing CodeLib expert'); {$ENDIF}
  CodeLibStandAlone := nil;
  InitSharedResources;
  try
    {$IFOPT D+} SendDebug('Created CodeLib window'); {$ENDIF}
    CodeLibStandAlone := TCodeLibExpert.Create;
    CodeLibStandAlone.LoadSettings;
    CodeLibStandAlone.CreateCodeLibForm;
    CodeLibStandAlone.FCodeLibForm.ShowModal;
    CodeLibStandAlone.SaveSettings;
  finally
    FreeAndNil(CodeLibStandAlone);
    FreeSharedResources;
  end;
end;

{ TGXStorageFile }

procedure TGXStorageFile.AssertFileIsOpen;
begin
  if not Assigned(FFile) then
    raise Exception.Create('No file is currently open');
  if FFileName = '' then
    raise Exception.Create('The file name is blank');
end;

procedure TGXStorageFile.AssertValidFileName(FileName: WideString);
begin
  if FileName = '' then
    raise Exception.Create('The file name can not be blank');
  if FileName[1] = CodeLibPathSep then
    raise Exception.Create('The file name can not start with a slash: ' + FileName);
end;

procedure TGXStorageFile.AssertValidFolderName(FolderName: WideString);
begin
  if FolderName = '' then
    raise Exception.Create('FolderName can not be blank');
  if FolderName[1] <> CodeLibPathSep then
    raise Exception.Create('FolderName must start with a slash');
end;

function TGXStorageFile.AttributeAsString(const AttrName, Default: WideString): WideString;
var
  FileInfo: IGpStructuredFileInfo;
begin
  Result := Default;
  FileInfo := GetFileInfo;
  Result := FileInfo.GetAttribute(AttrName);
end;

procedure TGXStorageFile.CloseFile;
begin
  FreeAndNil(FFile);
end;

procedure TGXStorageFile.CloseStorage;
begin
  //FreeAndNil(FStorage);?
end;

constructor TGXStorageFile.Create(const FileName: WideString);
begin
  inherited Create;
  FStorage := CreateStructuredStorage;
  if SysUtils.FileExists(FileName) then
    FStorage.Initialize(FileName, fmOpenReadWrite)
  else
    FStorage.Initialize(FileName, fmCreate or fmOpenReadWrite);
end;

procedure TGXStorageFile.CreateFolder(const FolderName: WideString);
begin
  FStorage.CreateFolder(FolderName);
end;

procedure TGXStorageFile.Delete(const ObjectName: WideString);
begin
  Assert(Length(ObjectName) > 0);
  CloseFile;
  FStorage.Delete(ObjectName);
end;

function TGXStorageFile.FileExists(const FileName: WideString): Boolean;
begin
  Result := FStorage.FileExists(FileName);
end;

function TGXStorageFile.FolderExists(const FolderName: WideString): Boolean;
begin
  Result := FStorage.FolderExists(FolderName);
end;

function TGXStorageFile.GetFileInfo: IGpStructuredFileInfo;
begin
  AssertFileIsOpen;
  Result := FStorage.GetFileInfo(FFileName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('The file %s does not exist to get the file info', [FFileName]);
end;

function TGXStorageFile.GetFileText: WideString;
var
  StringStream: TStringStream;
begin
  AssertFileIsOpen;
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(FFile, 0);
    Result := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TGXStorageFile.ListFiles(const FolderName: WideString; Files: TStrings);
begin
  AssertValidFolderName(FolderName);
  FStorage.FileNames(FolderName, Files);
end;

procedure TGXStorageFile.ListFolders(const FolderName: WideString; Files: TStrings);
begin
  AssertValidFolderName(FolderName);
  FStorage.FolderNames(FolderName, Files);
end;

procedure TGXStorageFile.Move(const OldName, NewName: WideString);
begin
  if OldName = NewName then
    Exit;
  FStorage.Move(OldName, NewName);
  if SameFileName(FFileName, OldName) then
    OpenFile(NewName);
end;

procedure TGXStorageFile.OpenFile(const FolderName, FileName: WideString);
begin
  AssertValidFolderName(FolderName);
  AssertValidFileName(FileName);
  OpenFile(MakeFileName(FolderName, FileName));
end;

procedure TGXStorageFile.OpenFile(const FullPath: WideString);
begin
  FreeAndNil(FFile);
  AssertValidFolderName(FullPath);
  Assert(Length(FullPath) > 0);
  FFile := FStorage.OpenFile(FullPath, fmCreate or fmOpenReadWrite);
  FFileName := FullPath;
end;

procedure TGXStorageFile.CompactStorage;
begin
  FreeAndNil(FFile);
  FStorage.Compact;
end;

procedure TGXStorageFile.SaveStorage;
begin
  //FStorage.Save;
end;

procedure TGXStorageFile.SetAttribute(const AttrName: WideString; const Value: Integer);
begin
  SetAttribute(AttrName, IntToStr(Value));
end;

procedure TGXStorageFile.SetAttribute(const AttrName: WideString; const Value: WideString);
var
  FileInfo: IGpStructuredFileInfo;
begin
  AssertFileIsOpen;
  FileInfo := GetFileInfo;
  FileInfo.Attribute[AttrName] := Value;
end;

procedure TGXStorageFile.SetFileText(const Value: WideString);
var
  StringStream: TStringStream;
begin
  AssertFileIsOpen;
  StringStream := TStringStream.Create(Value);
  try
    FFile.Size := StringStream.Size;
    FFile.Position := 0;
    FFile.CopyFrom(StringStream, 0);
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TGXStorageFile.SetObjectAttribute(const ObjectName, AttrName, AttrValue: WideString);
var
  FileInfo: IGpStructuredFileInfo;
begin
  FileInfo := FStorage.FileInfo[ObjectName];
  Assert(Assigned(FileInfo));
  FileInfo.Attribute[AttrName] := AttrValue;
end;

function TGXStorageFile.GetObjectAttribute(const ObjectName, AttrName: WideString): WideString;
var
  FileInfo: IGpStructuredFileInfo;
begin
  AssertExistingObjectName(ObjectName);
  FileInfo := FStorage.FileInfo[ObjectName];
  Assert(Assigned(FileInfo));
  Result := FileInfo.Attribute[AttrName];
end;

procedure TGXStorageFile.AssertExistingObjectName(const ObjectName: WideString);
begin
  Assert(ObjectExists(ObjectName), ObjectName + ' does not exist');
end;

function TfmCodeLib.GetUniqueTopicName(ParentNode: TTreeNode; Folder: Boolean): WideString;
resourcestring
  FolderPrefix = 'Folder ';
  SnippetPrefix = 'Snippet ';
var
  i: Integer;
  TopicName: WideString;
  ParentPath: WideString;
  TestName: WideString;
begin
  if Folder then
    TopicName := FolderPrefix
  else
    TopicName := SnippetPrefix;
  ParentPath := AddSlash(GetNodePath(ParentNode));
  for i := 1 to 101 do
  begin
    TestName := TopicName + IntToStr(i);
    CodeDB.AssertValidFolderName(ParentPath + TestName);
    if not CodeDB.ObjectExists(ParentPath + TestName) then
    begin
      Result := TestName;
      Exit;
    end;
  end;
  raise Exception.Create('Unable to find a unique folder name under ' + ParentPath);
end;

function TGXStorageFile.ObjectExists(const ObjectName: WideString): Boolean;
begin
  Result := FileExists(ObjectName) or FolderExists(ObjectName);
end;

initialization
  RegisterGX_Expert(TCodeLibExpert);

end.

