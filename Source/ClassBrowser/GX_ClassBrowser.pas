unit GX_ClassBrowser;

// This unit is NOT fully compatible with C++Builder (we don't parse C++ code)
// Is there an easy way to add visual display of multiple interface inheritance?

interface

{$I GX_CondDefine.inc}

uses
  GX_ClassMgr, GX_ClassParsing, GX_Experts, GX_OtaUtils, GX_EnhancedEditor,
  Classes, Controls, Forms, Dialogs, ActnList, ToolWin, ToolsAPI,
  Graphics, Menus, ExtCtrls, ComCtrls, ImgList, GX_BaseForm;

type
  TInfoViewMode = (vmList, vmTree);

  TfmClassBrowser = class(TfmBaseForm)
    StatusBar: TStatusBar;
    Splitter1: TSplitter;
    pnlData: TPanel;
    pcMain: TPageControl;
    tshMembers: TTabSheet;
    tshInherit: TTabSheet;
    tshCode: TTabSheet;
    lvInfo: TListView;
    Splitter2: TSplitter;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileAdd: TMenuItem;
    mitFileRemove: TMenuItem;
    mitFileSep1: TMenuItem;
    mitPrint: TMenuItem;
    mitFilePrintClassReport: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitView: TMenuItem;
    mitViewList: TMenuItem;
    mitViewTree: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsOptions: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    pmBrowser: TPopupMenu;
    pmiBrowserRefresh: TMenuItem;
    pmiBrowserSep1: TMenuItem;
    pmiBrowserProperties: TMenuItem;
    scInherit: TScrollBox;
    dlgPrinterSetup: TPrinterSetupDialog;
    pmInfo: TPopupMenu;
    mitFilePrintClassHierarchy: TMenuItem;
    pmiBrowserGotoClass: TMenuItem;
    pmiDetailsGoto: TMenuItem;
    mitViewUnitNames: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpContents: TMenuItem;
    mitEdit: TMenuItem;
    mitEditCopy: TMenuItem;
    mitViewSep2: TMenuItem;
    mitViewDetails: TMenuItem;
    pnlMethod: TPanel;
    tvBrowse: TTreeView;
    Actions: TActionList;
    actFileAdd: TAction;
    actFileRemove: TAction;
    actFilePrintClassReport: TAction;
    actFilePrintClassHierarchy: TAction;
    actFilePrinterSetup: TAction;
    actFileExit: TAction;
    actEditCopy: TAction;
    actViewList: TAction;
    actViewTree: TAction;
    actViewUnitNames: TAction;
    actViewClassProperties: TAction;
    mitViewSep1: TMenuItem;
    actOptionsOptions: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    ControlBar: TControlBar;
    tbMain: TToolBar;
    tbnAdd: TToolButton;
    tbnRemove: TToolButton;
    tbnSep1: TToolButton;
    tbnRefresh: TToolButton;
    tbnSep2: TToolButton;
    tbnFind: TToolButton;
    mitFileRefresh: TMenuItem;
    mitEditFind: TMenuItem;
    actEditFind: TAction;
    actFileRefresh: TAction;
    tbKinds: TToolBar;
    tbVisibility: TToolBar;
    tbnConstants: TToolButton;
    tbnMethods: TToolButton;
    tbnTypes: TToolButton;
    tbnVariables: TToolButton;
    tbnProperties: TToolButton;
    tbnPrivate: TToolButton;
    tbnProtected: TToolButton;
    tbnPublic: TToolButton;
    tbnPublished: TToolButton;
    actEditGotoMember: TAction;
    actEditGotoClass: TAction;
    actEditFindNext: TAction;
    mitEditFindNext: TMenuItem;
    actViewConstants: TAction;
    actViewMethods: TAction;
    actViewTypes: TAction;
    actViewVariables: TAction;
    actViewProperties: TAction;
    actViewPrivate: TAction;
    actViewProtected: TAction;
    actViewPublic: TAction;
    actViewPublished: TAction;
    procedure tvBrowseChange(Sender: TObject; Node: TTreeNode);
    procedure pnlDataResize(Sender: TObject);
    procedure lvInfoChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure pcMainChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actFilePrintClassReportExecute(Sender: TObject);
    procedure actFilePrintClassHierarchyExecute(Sender: TObject);
    procedure actFileAddExecute(Sender: TObject);
    procedure actFileRemoveExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actGenericViewNewFilterExecute(Sender: TObject);
    procedure actViewClassPropertiesExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOptionsOptionsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFilePrinterSetupExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actViewListExecute(Sender: TObject);
    procedure actViewTreeExecute(Sender: TObject);
    procedure actViewUnitNamesExecute(Sender: TObject);
    procedure actEditGotoMemberExecute(Sender: TObject);
    procedure actEditGotoClassExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure tvBrowseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actEditFindNextExecute(Sender: TObject);
    procedure tvBrowseDblClick(Sender: TObject);
  private
    FProjectNotifier: TBaseIdeNotifier;
    FIsFirstInvocation: Boolean;
    FViewUnitNames: Boolean;
    FInfoViewMode: TInfoViewMode;
    FClassList: TClassList;
    fmProgress: TfmClassParsing;
    FPrimitiveTop: Boolean;
    FStayInPackage: Boolean;
    FParseRecursing: Boolean;
    FAutomaticallyHideBrowser: Boolean;
    FLoadProject: Boolean;
    FStartingDir: string;
    FCurrentCodePaneFile: string;
    FClassHierarchyFontSize: Integer;
    FClassHierarchyBoxWidth: Integer;
    FClassHierarchyBoxSpace: Integer;
    FClassHierarchyFont: string;
    FFilters: array[0..8] of Boolean;
    FLastFind: string;

    procedure EndParse(Sender: TObject);
    procedure ParseFile(Sender: TObject; const FileName: string; FileIndex, FileCount: Integer);
    procedure LoadObjects(Item: TClassItem; ONode: TTreeNode);
    procedure LoadList(OInfo: TBrowseClassInfoCollection);
    procedure LoadClassList(Item: TClassItem; ONode: TTreeNode);
    procedure LoadClassTree(Item: TClassItem; ONode: TTreeNode);
    function GetMethodString(M: TBrowseMethodInfoItem): string;
    function CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
    procedure LoadCode;
    procedure GetInheritedList(List: TStrings; const StartClassName: string);
    procedure DrawInheritance;
    procedure DrawResize;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadAllObjects;
    procedure PrintClassReportBuiltIn;
    procedure PrintClassBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
    procedure PrintClassHierarchyBuiltIn;
    procedure PrintClassDiagramBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas; BoxSize, VSpace: Integer);
    procedure RefreshNode;
    procedure AddProject;
    procedure RemoveProject;
    procedure ClickInheritancePanel(Sender: TObject);
    function FilterTab(const Source: string): string;
    procedure ViewBrowserDetails;
    procedure SetInfoViewMode(const Value: TInfoViewMode);
    procedure FindFromNode(const Text: string; Node: TTreeNode);
    procedure FiltersToActions;
  private
    FMethodText: TGxEnhancedEditor;
    FCodeText: TGxEnhancedEditor;
    FLastProject: string;
    procedure SetupEditorControls;
    function Images: TImageList;
    function ConfigurationKey: string;
    procedure UpdateListFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ClassList: TClassList read FClassList;
  end;

  TClassProjectNotifier = class(TBaseIdeNotifier)
  private
    fmClassBrowser: TfmClassBrowser;
  public
    constructor Create(Form: TfmClassBrowser);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

  TClassExpert = class(TGX_Expert)
  private
    fmClassBrowser: TfmClassBrowser;
  protected
    procedure SetActive(New: Boolean); override;
  public
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function IsDefaultActive: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, StdCtrls, Printers,
  GX_VerDepConst, GX_ClassIdentify, GX_ConfigurationInfo, GX_EditReader,
  GX_ClassProp, GX_ClassOptions, GX_ClassReport, GX_GExperts,
  GX_GxUtils, GX_GenericUtils, GX_SharedImages, GX_IdeUtils, Math;

function ExpertClassKey: string;
begin
  if IsStandAlone then
    Result := '\Software\GExperts\'
  else
    Result := ConfigInfo.GExpertsIdeRootRegistryKey;
end;

{ TClassProjectNotifier }

constructor TClassProjectNotifier.Create(Form: TfmClassBrowser);
begin
  inherited Create;
  fmClassBrowser := Form;
end;

procedure TClassProjectNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    ofnActiveProjectChanged:
      begin
        with Self.fmClassBrowser do
        begin
          if not SameText(FileName, FLastProject) then
          begin
            RemoveProject;
            if Visible then
              AddProject
            else
              FLoadProject := True;
          end;
        end;
      end;
    ofnFileClosing:
      if AnsiCompareText(FileName, fmClassBrowser.FLastProject) = 0 then
        fmClassBrowser.RemoveProject;
  end;
end;

procedure ClearBitmap(Bitmap: Graphics.TBitmap);
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Height := 0;
    Bitmap.Width := 0;
  end;
end;

{ TfmClassBrowser }

procedure TfmClassBrowser.AddProject;
var
  Node: TTreeNode;
  Item: TClassItem;
  ProjectName: string;
begin
  ProjectName := GxOtaGetCurrentProjectFileName;
  if AnsiCompareText(FLastProject, ProjectName) = 0 then
    Exit;
  FLastProject := ProjectName;
  Node := tvBrowse.Items.Add(nil, ExtractFileName(FLastProject));
  Node.ImageIndex := ImageIndexClosedFolder;
  Node.SelectedIndex := ImageIndexOpenFolder;
  Item := ClassList.Add;
  with Item do
  begin
    Item.IsProject := True;
    Item.Directory := ExtractFilePath(ProjectName);
    Item.Name := ExtractPureFileName(ProjectName);
    if FileExists(Item.GenerateFilename(Item.Directory)) then
      Item.LoadFromFile(Item.Directory)
    else
      Item.Load;
  end;
  Node.Data := Item;
  LoadObjects(Item, Node);
end;

resourcestring
  SClassesParsed = '%d classes parsed in %g seconds';
  SSelectClassFirst = 'Please select a class in the tree first';

function GetClassesParsedText(ClassCount: Cardinal; Ticks: Cardinal): string;
begin
  Result := Format(SClassesParsed, [ClassCount, Ticks / 1000]);
end;

{ TfmClassBrowser }

procedure TfmClassBrowser.RemoveProject;
var
  ONode: TTreeNode;
  Node: TTreeNode;
  Item: TClassItem;
begin
  FLastProject := '';
  if tvBrowse.Items.Count = 0 then
    Exit;
  Node := tvBrowse.Items[0];
  if Node = nil then
    Exit;
  while Assigned(Node) do
  begin
    Item := TClassItem(Node.Data);
    if Item.IsProject then
    begin
      Item.SaveToFile(Item.Directory);
      FreeAndNil(Item);
      ONode := Node.GetNextSibling;
      FreeAndNil(Node);
      Node := ONode;
    end
    else
      Node := Node.GetNextSibling;
  end;
end;

procedure TfmClassBrowser.LoadAllObjects;
var
  i: Integer;
  Node: TTreeNode;
  AClassItem: TClassItem;
begin
  tvBrowse.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    tvBrowse.Selected := nil;
    tvBrowse.Items.Clear;
    for i := 0 to ClassList.Count-1 do
    begin
      AClassItem := ClassList.Items[i];
      Node := tvBrowse.Items.AddObject(nil, AClassItem.Name, AClassItem);
      Node.ImageIndex := ImageIndexClosedFolder;
      Node.SelectedIndex := ImageIndexOpenFolder;
      LoadObjects(AClassItem, Node);
    end;
  finally
    tvBrowse.Items.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.LoadObjects(Item: TClassItem; ONode: TTreeNode);
resourcestring
  SSorting = 'Sorting...';
begin
  tvBrowse.Items.BeginUpdate;
  Screen.Cursor := crHourglass;
  tvBrowse.SortType := stNone;
  try
    tvBrowse.Selected := nil;
    // while ONode.Count > 0 do
    //  ONode.Item[0].Free;
    ONode.DeleteChildren;
    case FInfoViewMode of
      vmList: LoadClassList(Item, ONode);
      vmTree: LoadClassTree(Item, ONode);
    else
      Assert(False, 'Unknown view mode!');
    end;
    ONode.Expand(False);
  finally
    StatusBar.SimpleText := SSorting;
    StatusBar.Repaint;
    tvBrowse.SortType := ComCtrls.stText;
    Screen.Cursor := crDefault;
    tvBrowse.Items.EndUpdate;
    StatusBar.SimpleText := '';
  end;
end;

procedure TfmClassBrowser.LoadClassTree(Item: TClassItem; ONode: TTreeNode);
var
  TempClassList: TStrings;
  Node: TTreeNode;
  i: Integer;

  procedure AddObjects(INode: TTreeNode);
  var
    j: Integer;
    ANode: TTreeNode;
    NodeText: string;
    AClassItem: TBrowseClassInfoCollection;
  begin
    j := TempClassList.Count-1;
    while j >= 0 do
    begin
      AClassItem := TBrowseClassInfoCollection(TempClassList.Objects[j]);

      if SameText(AClassItem.ObjectDerivedFrom, TBrowseClassInfoCollection(INode.Data).Name) then
      begin
        if FViewUnitNames then
        begin
          with AClassItem do
            NodeText := SourceName + '.' + Name;
        end
        else
          NodeText := AClassItem.Name;

        ANode := tvBrowse.Items.AddChildObject(INode, NodeText, AClassItem);
        ANode.ImageIndex := ImageIndexGear;
        ANode.SelectedIndex := ImageIndexGear;

        TempClassList.Delete(j);

        AddObjects(ANode);
        j := TempClassList.Count-1;
      end
      else
        Dec(j);
    end;
  end;

var
  NodeText: string;
  ClassInfoCollItem: TBrowseClassInfoCollection;
begin
  TempClassList := TStringList.Create;
  try
    for i := 0 to Item.ClassCount - 1 do
    begin
      ClassInfoCollItem := Item.ClassItem[i];
      TempClassList.AddObject(ClassInfoCollItem.Name, ClassInfoCollItem);
    end;

    // First add root items.
    i := TempClassList.Count-1;
    while i >= 0 do
    begin
      ClassInfoCollItem := TBrowseClassInfoCollection(TempClassList.Objects[i]);

      if Item.ObjectByName(ClassInfoCollItem.ObjectDerivedFrom) = nil then
      begin
        // This is a root item - ObjectDerivedFrom = nil --> no ancestor.
        if FViewUnitNames then
        begin
          with ClassInfoCollItem do
            NodeText := SourceName + '.' + Name;
        end
        else
          NodeText := ClassInfoCollItem.Name;

        Node := tvBrowse.Items.AddChildObject(ONode, NodeText, ClassInfoCollItem);
        Node.ImageIndex := ImageIndexGear;
        Node.SelectedIndex := ImageIndexGear;

        TempClassList.Delete(i);

        AddObjects(Node);
        i := TempClassList.Count-1;
      end
      else
        Dec(i);
    end;

    Assert(TempClassList.Count = 0, 'Bad algorithm building tree');

  finally
    FreeAndNil(TempClassList);
  end;
end;

procedure TfmClassBrowser.LoadClassList(Item: TClassItem; ONode: TTreeNode);
var
  INode: TTreeNode;
  i: Integer;
  NodeText: string;
begin
  for i := 0 to Item.ClassCount-1 do
  begin
    if FViewUnitNames then
    begin
      with Item.ClassItem[i] do
        NodeText := SourceName + '.' + Name;
    end
    else
      NodeText := Item.ClassItem[i].Name;

    INode := tvBrowse.Items.AddChildObject(ONode, NodeText, Item.ClassItem[i]);
    INode.ImageIndex := ImageIndexGear;
    INode.SelectedIndex := ImageIndexGear;
  end;
end;

procedure TfmClassBrowser.ParseFile(Sender: TObject; const FileName: string;
  FileIndex, FileCount: Integer);
resourcestring
  SParsingProgress = 'Parsing classes in %s ...';
begin
{$DEFINE DoShowProgressForm}
{$IFDEF DoShowProgressForm}
  if fmProgress = nil then
  begin
    fmProgress := TfmClassParsing.Create(Self);
    with fmProgress do
    begin
      Progress.Position := 0;
      Progress.Min := 0;
      Progress.Max := FileCount;
      Show;
    end;
  end;
  if Assigned(fmProgress) then
  begin
    fmProgress.lblParsing.Caption := Format(SParsingProgress, [ExtractFileName(FileName)]);
    fmprogress.Progress.Position := FileIndex;
  end;
  Application.ProcessMessages;
{$ENDIF DoShowProgressForm}
end;

procedure TfmClassBrowser.EndParse(Sender: TObject);
begin
  FreeAndNil(fmProgress);
end;

procedure TfmClassBrowser.LoadList(OInfo: TBrowseClassInfoCollection);

  procedure SetSubItemImage(const ListItem: TListItem; SubItem, Value: Integer);
  begin
    ListItem.SubItemImages[SubItem] := Value;
  end;

var
  AMethod: TBrowseMethodInfoItem;
  i: Integer;
  ListItem: TListItem;
begin
  lvInfo.Items.BeginUpdate;
  try
    lvInfo.Items.Clear;

    FMethodText.Clear;
    for i := 0 to OInfo.Count-1 do
    begin
      AMethod := OInfo.Items[i];
      if CheckFilter(AMethod) then
      begin
        listItem := lvInfo.Items.Add;
        ListItem.Caption := '';
        ListItem.ImageIndex := Ord(AMethod.MethodDeclare) + ImageIndexVisibility;
        ListItem.SubItems.Add('');
        SetSubItemImage(ListItem, 0, Ord(AMethod.MethodType) + ImageIndexMemberType);
        ListItem.SubItems.Add('');
        if AMethod.cVirtual then
          SetSubItemImage(ListItem, 1, ImageIndexCheck);
        ListItem.SubItems.Add('');
        if AMethod.cAbstract then
          SetSubItemImage(ListItem, 2, ImageIndexCheck);
        ListItem.SubItems.Add('');
        if AMethod.cOverride then
          SetSubItemImage(ListItem, 3, ImageIndexCheck);
        ListItem.SubItems.Add(CompressWhiteSpace(AMethod.DName));
        ListItem.Data := AMethod;
      end;
    end;
  finally
    lvInfo.Items.EndUpdate;
  end;
end;

function TfmClassBrowser.GetMethodString(M: TBrowseMethodInfoItem): string;
begin
  Result := IntToStr(Ord(M.MethodDeclare)) + #9 + IntToStr(Ord(M.MethodType)) + #9 +
            IntToStr(Ord(M.cVirtual)) + #9 + IntToStr(Ord(M.cAbstract)) + #9 +
            IntToStr(Ord(M.cOverride)) + #9 + FilterTab(M.DName);
end;

procedure TfmClassBrowser.tvBrowseChange(Sender: TObject; Node: TTreeNode);
resourcestring
  SSourceModule = 'Source module: %s  (%d ms)';
var
  OInfo: TBrowseClassInfoCollection;
  TimeSpent: DWORD;
begin
  Screen.Cursor := crHourglass;
  try
    if Node = nil then
      actFileRemove.Enabled := False
    else
      actFileRemove.Enabled := (Node.Level = 0);
    lvInfo.Items.BeginUpdate;
    lvInfo.Items.Clear;
    lvInfo.Items.EndUpdate;
    if Assigned(tvBrowse.Selected) and Assigned(tvBrowse.Selected.Data) then
    begin
      if tvBrowse.Selected.Level > 0 then
      begin
        OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
        if not OInfo.IsLoaded then OInfo.LoadMethods;
        TimeSpent := GetTickCount;
        if pcMain.ActivePage = tshCode then LoadCode;
        if pcMain.ActivePage = tshInherit then DrawInheritance;
        TimeSpent := GetTickCount - TimeSpent;
        LoadList(OInfo);
        StatusBar.SimpleText := Format(SSourceModule, [OInfo.SourceName, TimeSpent]);
      end
      else
      begin
        StatusBar.SimpleText := '';
        lvInfo.Items.BeginUpdate;
        lvInfo.Items.Clear;
        lvInfo.Items.EndUpdate;
        FMethodText.Clear;
        FCodeText.Clear;
        FCurrentCodePaneFile := '';
        while scInherit.ControlCount > 0 do
          scInherit.Controls[0].Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  pnlDataResize(Self);
end;

function TfmClassBrowser.CheckFilter(MInfo: TBrowseMethodInfoItem): Boolean;
begin
  Result := True;
  case MInfo.MethodType of
    ctConstant:   Result := actViewConstants.Checked;
    ctMethod:     Result := actViewMethods.Checked;
    ctType:       Result := actViewTypes.Checked;
    ctVariable:   Result := actViewVariables.Checked;
    ctProperty:   Result := actViewProperties.Checked;
  end;
  if Result then
    case MInfo.MethodDeclare of
      cdPrivate:   Result := actViewPrivate.Checked;
      cdProtected: Result := actViewProtected.Checked;
      cdPublic:    Result := actViewPublic.Checked;
      cdPublished: Result := actViewPublished.Checked;
    end;
end;

procedure TfmClassBrowser.pnlDataResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
  with lvInfo do
  begin
    if lvInfo.ClientWidth > 50 then
    begin
      Columns[5].Width := Max(lvInfo.ClientWidth - Columns[0].Width - Columns[1].Width -
        Columns[2].Width - Columns[3].Width - Columns[4].Width, 0);
    end;
  end;
  tshMembers.Width := pcMain.ActivePage.Width;
  tshMembers.Height := pcMain.ActivePage.Height;
  DrawResize;
end;

procedure TfmClassBrowser.DrawResize;
var
  i: Integer;
  W: Integer;
  ShapeLeft: Integer;
  L: Integer;
begin
  SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(False), 0);
  try
    tshInherit.Width := pcMain.ActivePage.Width;
    tshInherit.Height := pcMain.ActivePage.Height;
    scInherit.Width := tshInherit.Width;
    scInherit.Height := tshInherit.Height;
    W := scInherit.Width - 20;
    if W > 300 then W := 300;
    L := (scInherit.Width - W) div 2;
    ShapeLeft := L + (W div 2) - 2;
    for i := 0 to scInherit.ControlCount - 1 do
      if scInherit.Controls[i] is TPanel then
        with TPanel(scInherit.Controls[i]) do
          SetBounds(L, Top, W, Height)
      else
        if scInherit.Controls[i] is TShape then
          with TShape(scInherit.Controls[i]) do
            SetBounds(ShapeLeft, Top, 2, 20);
  finally
    SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(True), 0);
    for i := 0 to scInherit.ControlCount - 1 do
       scInherit.Controls[i].Invalidate;
    scInherit.Invalidate;
  end;
end;

procedure TfmClassBrowser.lvInfoChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  InfoItem: TBrowseMethodInfoItem;
begin
  FMethodText.Text := '';
  if Assigned(lvInfo.Selected) then
  begin
    with lvInfo do
      InfoItem := TBrowseMethodInfoItem(lvInfo.Selected.Data);
    if Assigned(InfoItem) then
    begin
      FMethodText.Text := CompressWhiteSpace(FilterTab(InfoItem.DName));
      StatusBar.SimpleText := CompressWhiteSpace(FilterTab(FMethodText.Text));
    end;
  end;
end;

procedure TfmClassBrowser.LoadCode;

  procedure LoadCodePane(const F: string);
  var
    Lines: TGXUnicodeStringList;
  begin
    if FCurrentCodePaneFile = F then
    begin
      if FCodeText.LineCount > 2 then
        Exit;
    end
    else
      FCurrentCodePaneFile := F;

    Lines := TGXUnicodeStringList.Create;
    try
      GxOtaLoadFileToUnicodeStrings(F, Lines);
      FCodeText.Text := Lines.Text;
    finally
      FreeAndNil(Lines);
    end;
  end;

var
  LineNumber: Integer;
  SourceFileName: string;
  MInfo: TBrowseMethodInfoItem;
begin
  Screen.Cursor := crHourglass;
  FCodeText.BeginUpdate;
  try
    if tvBrowse.Selected = nil then Exit;
    if tvBrowse.Selected.Level = 0 then Exit;
    SourceFileName := TBrowseClassInfoCollection(tvBrowse.Selected.Data).FileName;
    LineNumber := -1;
    if lvInfo.Selected = nil then
      LineNumber := TBrowseClassInfoCollection(tvBrowse.Selected.Data).RefreshLineNo
    else
    begin
      MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
      case MInfo.MethodType of
        ctConstant,
        ctType,
        ctVariable,
        ctProperty:   LineNumber := MInfo.GetInterfaceLine;
        ctMethod:
          begin
            if MInfo.cAbstract then
              LineNumber := MInfo.GetInterfaceLine
            else
              LineNumber := MInfo.GetImplementationLine;
          end;
      end;
    end;

    LoadCodePane(SourceFileName);

    if LineNumber >= 0 then
    begin
      FCodeText.TopLine := LineNumber + 1;
      FCodeText.CaretXY := Point(1, LineNumber);
    end;
  finally
    FCodeText.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tshCode then
    LoadCode
  else
    if pcMain.ActivePage = tshInherit then
      DrawInheritance;
end;

procedure TfmClassBrowser.RefreshNode;
var
  Ticks: DWORD;
  Item: TClassItem;
begin
  Screen.Cursor := crHourglass;
  try
    Ticks := GetTickCount;
    Item := TClassItem(tvBrowse.Selected.Data);
    Item.Recurse := FParseRecursing;
    Item.Load;
    LoadObjects(Item, tvBrowse.Selected);
    StatusBar.SimpleText := GetClassesParsedText(Item.ClassCount, GetTickCount - Ticks);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.GetInheritedList(List: TStrings; const StartClassName: string);
var
  LastVisitedLevel: Integer;
  FirstNode: TTreeNode;

  function GetInherited(const Comp: string): string;
  var
    OInfo: TBrowseClassInfoCollection;
    Node: TTreeNode;
  begin
    Result := '';
    Node := FirstNode;
    while Assigned(Node) do
    begin
      if Node.Level > LastVisitedLevel then
      begin
        if SameText(TBrowseClassInfoCollection(Node.Data).Name, Comp) then
        begin
          OInfo := TBrowseClassInfoCollection(Node.Data);
          Result := OInfo.DerivedFrom;
          // We could instead recurse the treeview backwards to get
          // this information if we are in "tree" mode.
          if FStayInPackage then
            LastVisitedLevel := Node.Level;
          Break;
        end;
      end;
      Node := Node.GetNext;
    end;
  end;

var
  InheritingFromClassName: string;
  CommaPosition: Integer;
begin
  FirstNode := tvBrowse.Items.GetFirstNode;
  LastVisitedLevel := FirstNode.Level;

  InheritingFromClassName := GetInherited(StartClassName);
  while InheritingFromClassName <> '' do
  begin
    List.Add(InheritingFromClassName);
    CommaPosition := Pos(',', InheritingFromClassName);
    // With multiple ancestors we want to search for the first one.
    if CommaPosition > 0 then
      InheritingFromClassName := Copy(InheritingFromClassName, 1, CommaPosition - 1);

    InheritingFromClassName := GetInherited(InheritingFromClassName);
  end;
end;

procedure TfmClassBrowser.ClickInheritancePanel(Sender: TObject);
//var
//  Panel: TPanel;
begin //FI:W519
  // Panel := Sender as TPanel;
  { TODO -oAnyone : Jump to the class declaration, or treeview node? }
end;

procedure TfmClassBrowser.DrawInheritance;
var
  ClassControlLeft: Integer;
  ClassControlWidth: Integer;

  procedure AddPanel(const PanelText: string; PanelPosition: Integer);
  var
    APanel: TPanel;
  begin
    APanel := TPanel.Create(scInherit);
    APanel.Visible := True;
    APanel.Parent := scInherit;
    APanel.OnClick := ClickInheritancePanel;
    SendMessage(APanel.Handle, WM_SETREDRAW, Integer(True), 0);
    APanel.BorderWidth := 1;
    APanel.FullRepaint := False;
    APanel.Caption := PanelText;
    APanel.BevelInner := bvLowered;
    APanel.SetBounds(ClassControlLeft, (PanelPosition * 50) + 10, ClassControlWidth, 30);

    // Assumes that form and panel have the same font
    if Self.Canvas.TextWidth(Caption) > APanel.ClientWidth - (2 * APanel.BorderWidth) then
    begin
      APanel.ShowHint := True;
      APanel.Hint := Caption;
    end;
  end;

  procedure AddShape(LeftPos: Integer; Index: Integer);
  begin
    with TShape.Create(scInherit) do
    begin
      Visible := True;
      Parent := scInherit;
      SetBounds(LeftPos, ((Index - 1) * 50) + 40, 2, 20);
    end;
  end;

var
  InheritList: TStrings;
  ClassIndex: Integer;
  ShapeLeft: Integer;
  DrawClassName: string;
  i: Integer;
  EntryCount: Integer;
begin
  if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then Exit;

  // Get the list of ancestor classes.
  InheritList := TStringList.Create;
  Screen.Cursor := crHourglass;
  try
    DrawClassName := (TObject(tvBrowse.Selected.Data) as TBrowseClassInfoCollection).Name;
    InheritList.Add(DrawClassName);
    GetInheritedList(Inheritlist, DrawClassName);

    scInherit.DisableAutoRange;
    SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(False), 0);
    try
      while scInherit.ControlCount > 0 do
        scInherit.Controls[0].Free;

      // Set up parameters for drawing the respective class items.
      ClassControlWidth := scInherit.Width - 20;
      if ClassControlWidth > 300 then ClassControlWidth := 300;

      ClassControlLeft := (scInherit.Width - ClassControlWidth) div 2;
      ShapeLeft := ClassControlLeft + (ClassControlWidth div 2) - 2;

      EntryCount := InheritList.Count-1;

      if FPrimitiveTop then
        ClassIndex := 0
      else
        ClassIndex := EntryCount;

      for i := EntryCount downto 0 do
      begin
        AddPanel(InheritList.Strings[ClassIndex], i);

        if i > 0 then
          AddShape(ShapeLeft, i);

        if FPrimitiveTop then
          Inc(ClassIndex)
        else
          Dec(ClassIndex);
      end;

    finally
      scInherit.EnableAutoRange;
      SendMessage(scInherit.Handle, WM_SETREDRAW, Integer(True), 0);

      for i := 0 to scInherit.ControlCount-1 do
        scInherit.Controls[i].Invalidate;
      scInherit.Invalidate;
    end;

  finally
    FreeAndNil(InheritList);
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.SaveSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
  i: Integer;
begin
  // Do not localize any of the following lines.
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create(ExpertClassKey);
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    Settings.SaveForm(Self, AddSlash(ConfigurationKey) + 'Window');
    Settings.WriteInteger(AddSlash(ConfigurationKey) + 'Window', 'Split', tvBrowse.Width);
    ExpSettings.WriteInteger('ViewMode', Ord(FInfoViewMode));
    ExpSettings.WriteBool('PrimitiveTop', FPrimitiveTop);
    ExpSettings.WriteBool('StayInPackage', FStayInPackage);
    ExpSettings.WriteBool('ParseRecursing', FParseRecursing);
    ExpSettings.WriteBool('AutomaticallyHideBrowser', FAutomaticallyHideBrowser);
    ExpSettings.WriteBool('UnitNames', FViewUnitNames);
    ExpSettings.WriteInteger('ClassHierarchyFontSize', FClassHierarchyFontSize);
    ExpSettings.WriteInteger('ClassHierarchyBoxWidth', FClassHierarchyBoxWidth);
    ExpSettings.WriteInteger('ClassHierarchyBoxSpace', FClassHierarchyBoxSpace);
    ExpSettings.WriteString('ClassHierarchyFont', 'Arial');
    for i := Low(FFilters) to High(FFilters) do
      ExpSettings.WriteBool(Format('Filter%d', [i]), FFilters[i]);

    ExpSettings.SaveFont('TreeFont', tvBrowse.Font);
    ExpSettings.SaveFont('ListFont', lvInfo.Font);
    ExpSettings.SaveFont('EditorFont', FCodeText.Font);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TfmClassBrowser.LoadSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
  i: Integer;
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;

  // Do not localize any of the following lines.
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create(ExpertClassKey);
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    Settings.LoadForm(Self, AddSlash(ConfigurationKey) + 'Window');
    tvBrowse.Width := Settings.ReadInteger(AddSlash(ConfigurationKey) + 'Window', 'Split', tvBrowse.Width);
    if tvBrowse.Width = 0 then
      tvBrowse.Width := 100;
    FInfoViewMode := TInfoViewMode(ExpSettings.ReadInteger('ViewMode', Ord(FInfoViewMode)));
    FViewUnitNames := ExpSettings.ReadBool('UnitNames', False);
    FClassHierarchyFontSize := ExpSettings.ReadInteger('ClassHierarchyFontSize', 8);
    FClassHierarchyBoxWidth := ExpSettings.ReadInteger('ClassHierarchyBoxWidth', 25);
    FClassHierarchyBoxSpace := ExpSettings.ReadInteger('ClassHierarchyBoxSpace', 10);
    FClassHierarchyFont := ExpSettings.ReadString('ClassHierarchyFont', 'Arial');
    if IsStandAlone then
      ClassList.StoragePath := AddSlash(ExtractFilePath(Application.ExeName))
    else begin
      ClassList.StoragePath := AddSlash(ConfigInfo.ConfigPath + ClassBrowserStorageFolder);
      FPrimitiveTop := ExpSettings.ReadBool('PrimitiveTop', FPrimitiveTop);
      FStayInPackage := ExpSettings.ReadBool('StayInPackage', FStayInPackage);
      FParseRecursing := ExpSettings.ReadBool('ParseRecursing', FParseRecursing);
      FAutomaticallyHideBrowser := ExpSettings.ReadBool('AutomaticallyHideBrowser', FAutomaticallyHideBrowser);
      for i := Low(FFilters) to High(FFilters) do
        FFilters[i] := ExpSettings.ReadBool(Format('Filter%d', [i]), True);

      ExpSettings.LoadFont('TreeFont', tvBrowse.Font);
      ExpSettings.LoadFont('ListFont', lvInfo.Font);
      ExpSettings.LoadFont('EditorFont', FCodeText.Font);
      FMethodText.Font.Assign(FCodeText.Font);
    end;
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
  FiltersToActions;
end;

procedure TfmClassBrowser.FiltersToActions;
begin
  actViewConstants.Checked := FFilters[0];
  actViewMethods.Checked := FFilters[1];
  actViewTypes.Checked := FFilters[2];
  actViewVariables.Checked := FFilters[3];
  actViewProperties.Checked := FFilters[4];
  actViewPrivate.Checked:= FFilters[5];
  actViewProtected.Checked := FFilters[6];
  actViewPublic.Checked := FFilters[7];
  actViewPublished.Checked := FFilters[8];
  UpdateListFilter;
end;

procedure TfmClassBrowser.PrintClassReportBuiltIn;
resourcestring
  SClassReport = 'Class Report';
var
  OInfo: TBrowseClassInfoCollection;
begin
  if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
    Exit;
  OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
  Screen.Cursor := crHourglass;
  Printer.Title := SClassReport;
  Printer.BeginDoc;
  try
    PrintClassBuiltIn(OInfo, Printer.Canvas);
  finally
    Printer.EndDoc;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.PrintClassBuiltIn(OInfo: TBrowseClassInfoCollection; ACanvas: TCanvas);
resourcestring
  SName = 'Name';
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;
var
  Row: Integer;
  i: Integer;
  List: TStrings;
  MInfo: TBrowseMethodInfoItem;
  FontHeight: Integer;
  ColumnWidth: Integer;
  BitmapSize: Integer;
  Bitmap: Graphics.TBitmap;

  procedure PrintHeader;
  begin
    with ACanvas do
    begin
      Font.Style := [fsBold];
      TextOut(PR_OffsetX, Row, 'Vi');
      TextOut(PR_OffsetX + ColumnWidth, Row, 'Ty');
      TextOut(PR_OffsetX + ColumnWidth * 2, Row, 'Vr');
      TextOut(PR_OffsetX + ColumnWidth * 3, Row, 'Ab');
      TextOut(PR_OffsetX + ColumnWidth * 4, Row, 'Ov');
      TextOut(PR_OffsetX + ColumnWidth * 6, Row, SName);
      MoveTo(PR_OffsetX, Row + FontHeight + 1);
      LineTo(PR_OffsetX + Printer.PageWidth - (2 * PR_OffsetX), Row + FontHeight + 1);
      Row := Row + FontHeight + 4;
      Font.Style := [];
    end;
  end;

  procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: Graphics.TBitmap);
  var
    BitmapHeader: pBitmapInfo;
    BitmapImage: Pointer;
    HeaderSize: DWord;
    ImageSize: DWord;
  begin
    GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage, ImageSize);
    try
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(Canvas.Handle,
                    DestRect.Left, DestRect.Top,     // Destination Origin
                    DestRect.Right  - DestRect.Left, // Destination Width
                    DestRect.Bottom - DestRect.Top,  // Destination Height
                    0, 0,                            // Source Origin
                    Bitmap.Width, Bitmap.Height,     // Source Width & Height
                    BitmapImage,
                    TBitmapInfo(BitmapHeader^),
                    DIB_RGB_COLORS,
                    SRCCOPY);
    finally
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    end;
  end;

resourcestring
  SClass = 'Class: ';
  SAncestor = 'Ancestor: ';
  SUnit = 'Unit: ';
begin
  Row := PR_OffSetY;
  with ACanvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 12;
    Font.Style := [fsBold];
    TextOut(PR_OffsetX, Row, SClass + OInfo.Name);
    Row := Row + ACanvas.TextHeight(SAllAlphaNumericChars) + 5;
    Font.Size := 10;
    FontHeight := ACanvas.TextHeight(SAllAlphaNumericChars);
    ColumnWidth:= ACanvas.TextWidth('WW');
    BitmapSize := FontHeight - 1;
    TextOut(PR_OffsetX, Row, SAncestor + OInfo.DerivedFrom);
    Row := Row + FontHeight + 5;
    TextOut(PR_OffsetX, Row, SUnit + OInfo.SourceName);
    Row := Row + FontHeight + 10;
    PrintHeader;
    Font.Style := [];
    List := TStringList.Create;
    try
      Bitmap := Graphics.TBitmap.Create;
      try
        for i := 0 to OInfo.Count - 1 do
          List.AddObject(GetMethodString(OInfo.Items[i]), OInfo.Items[i]);
        for i := 0 to List.Count - 1 do
        begin
          MInfo := TBrowseMethodInfoItem(List.Objects[i]);
          ClearBitmap(Bitmap);
          Images.GetBitmap(Ord(MInfo.MethodDeclare) + ImageIndexVisibility, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX, Row, PR_OffsetX + BitmapSize, Row + BitmapSize), Bitmap);
          ClearBitmap(Bitmap);
          Images.GetBitmap(Ord(MInfo.MethodType) + ImageIndexMemberType, Bitmap);
          PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth, Row, PR_OffsetX + ColumnWidth + BitmapSize, Row + BitmapSize), Bitmap);

          if MInfo.cVirtual then
          begin
            ClearBitmap(Bitmap);
            Images.GetBitmap(ImageIndexCheck, Bitmap);
            PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*2, Row, PR_OffsetX + ColumnWidth*2 + BitmapSize, Row + BitmapSize), Bitmap);
          end;
          if MInfo.cAbstract then
          begin
            ClearBitmap(Bitmap);
            Images.GetBitmap(ImageIndexCheck, Bitmap);
            PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*3, Row, PR_OffsetX + ColumnWidth*3 + BitmapSize, row + BitmapSize), Bitmap);
          end;
          if MInfo.cOverride then
          begin
            ClearBitmap(Bitmap);
            Images.GetBitmap(ImageIndexCheck, Bitmap);
            PrintBitmap(ACanvas, Rect(PR_OffsetX + ColumnWidth*4, Row, PR_OffsetX + ColumnWidth*4 + BitmapSize, row + BitmapSize), Bitmap);
          end;

          TextOut(PR_OffsetX + ColumnWidth*6, Row, FilterTab(MInfo.DName));
          Row := Row + FontHeight + 1;
          if Row + ((FontHeight + 1) * 3) > Printer.PageHeight then
          begin
            Printer.NewPage;
            Row := PR_OffsetY;
            PrintHeader;
          end;
        end;
      finally
        FreeAndNil(Bitmap);
      end;
    finally
      FreeAndNil(List);
    end;
  end;
end;

procedure TfmClassBrowser.FormActivate(Sender: TObject);
resourcestring
  SLoadingClasses = 'Loading stored classes...';
  SLoadingProject = 'Loading current project...';
begin
  if FIsFirstInvocation then
  begin
    FIsFirstInvocation := False;
    Application.ProcessMessages;
    Screen.Cursor := crHourglass;
    try
      StatusBar.SimpleText := SLoadingClasses;
      StatusBar.Repaint;
      ClassList.LoadFromFile;
      LoadAllObjects;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  if (not IsStandAlone) and FLoadProject then
  begin
    StatusBar.SimpleText := SLoadingProject;
    StatusBar.Repaint;
    FLoadProject := False;
    RemoveProject;
    Application.ProcessMessages;
    AddProject;
  end;
end;

procedure TfmClassBrowser.PrintClassDiagramBuiltIn(OInfo: TBrowseClassInfoCollection;
    ACanvas: TCanvas; BoxSize, VSpace: Integer);
const
  PR_OffsetX = 30;
  PR_OffsetY = 20;

var
  x, y: Integer;
  BoxW, BoxH: Integer;
  IndentX, IndentY: Integer;
  PageNum: Integer;

  procedure PrintClassSquare(Info: TBrowseClassInfoCollection; Level: Integer; var Py: Integer);
  var
    Rect: TRect;
    Node: TTreeNode;
    np: Integer;
    op: Integer;
    OldBrushColor: TColor;
  begin
    if y + BoxH + 20 > Printer.PageHeight then
    begin
      Printer.NewPage;
      y := PR_OffsetY;
      Py := 0;
      Inc(PageNum);
    end;
    x := PR_OffsetX + IndentX * Level;
    Rect := Classes.Rect(x, y, x + BoxW, y + BoxH);
    OldBrushColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := clBlack;
    ACanvas.FrameRect(Rect);
    ACanvas.Brush.Color := OldBrushColor;
    np := Rect.Bottom;
    op := PageNum;
    if Level > 0 then
      with ACanvas do
      begin
        MoveTo(Rect.Left, Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Rect.Top + (BoxH div 2));
        LineTo(Rect.Left - (IndentX div 2), Py);
      end;
    Inc(Rect.Left, 1);
    Inc(Rect.Top, 1);
    Dec(Rect.Bottom, 1);
    Dec(Rect.Right, 1);
    ACanvas.TextRect(Rect, x + 4, y + 4, Info.Name);
    y := y + BoxH + IndentY;
    Node := tvBrowse.Items[0];
    StatusBar.SimpleText := Info.Name;
    while Assigned(Node) do
    begin
      if op <> PageNum then
        np := 0;
      Application.ProcessMessages;
      if Node.Level > 0 then
        if SameText(TBrowseClassInfoCollection(Node.Data).ObjectDerivedFrom, Info.Name) then
          PrintClassSquare(TBrowseClassInfoCollection(Node.Data), Level + 1, np);
      Node := Node.GetNext;
    end;
  end;

var
  p, i: Integer;
  st: string;
begin
  PageNum := 1;
  x := PR_OffsetX;
  y := PR_OffsetY;
  st := '';
  for i := 1 to BoxSize do //FI:W528
    st := st + 'W';
  BoxW := ACanvas.TextWidth(st) + 8;
  BoxH := ACanvas.TextHeight(st) + 8;
  IndentX := (BoxW div 2);
  IndentY := VSpace;
  p := 0;
  PrintClassSquare(OInfo, 0, p);
end;

procedure TfmClassBrowser.PrintClassHierarchyBuiltIn;
var
  OInfo: TBrowseClassInfoCollection;
  Dlg: TfmClassReport;
resourcestring
  SClassReport = 'Class Hierarchy Report';
begin
  if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
    raise Exception.Create(SSelectClassFirst);
  Dlg := TfmClassReport.Create(nil);
  try
    Dlg.udFontSize.Position := FClassHierarchyFontSize;
    Dlg.udBoxSize.Position := FClassHierarchyBoxWidth;
    Dlg.udBoxSpacing.Position := FClassHierarchyBoxSpace;
    Dlg.cbxFont.ItemIndex := Dlg.cbxFont.Items.IndexOf(FClassHierarchyFont);

    if Dlg.ShowModal = mrCancel then
      Exit;
    OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);

    Screen.Cursor := crHourglass;
    Printer.Title := SClassReport;
    Printer.BeginDoc;
    try
      with Printer.Canvas do
      begin
        Font.Name := Dlg.cbxFont.Text;
        Font.Size := Trunc(Dlg.udFontSize.Position);
        Font.Style := [];
      end;
      PrintClassDiagramBuiltIn(OInfo, Printer.Canvas, Trunc(Dlg.udBoxSize.Position),
        Trunc(Dlg.udBoxSpacing.Position));
    finally
      Printer.EndDoc;
      Screen.Cursor := crDefault;
    end;

    FClassHierarchyFontSize := Dlg.udFontSize.Position;
    FClassHierarchyBoxWidth := Dlg.udBoxSize.Position;
    FClassHierarchyBoxSpace := Dlg.udBoxSpacing.Position;
    FClassHierarchyFont     := Dlg.cbxFont.Text;

  finally
    FreeAndNil(Dlg);
  end;
end;

function TfmClassBrowser.FilterTab(const Source: string): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Source) do
    if Result[i] = #9 then
      Result[i] := #32;
end;

constructor TfmClassBrowser.Create(AOwner: TComponent);
begin
  inherited;
  SetToolbarGradient(tbMain);
  SetToolbarGradient(tbKinds);
  SetToolbarGradient(tbVisibility);

  SetNonModalFormPopupMode(Self);
  FStartingDir := ExtractFilePath(Application.ExeName);
  FIsFirstInvocation := True;
  FLoadProject := True;

  SetupEditorControls;

  FPrimitiveTop := False;
  FStayInPackage := False;
  FParseRecursing := False;
  FAutomaticallyHideBrowser := True;
  FInfoViewMode := vmTree;

  FClassList := TClassList.Create;
  FClassList.OnParseFile := ParseFile;
  FClassList.OnEndParse := EndParse;

  LoadSettings;

  if not IsStandAlone then
  begin
    FProjectNotifier := TClassProjectNotifier.Create(Self);
    FProjectNotifier.AddNotifierToIDE;
  end;
end;

destructor TfmClassBrowser.Destroy;
begin
  SaveSettings;

  if not IsStandAlone then
  begin
    FProjectNotifier.RemoveNotifierFromIDE;
    FProjectNotifier := nil;
  end;

  FreeAndNil(FClassList);

  inherited Destroy;
end;

procedure TfmClassBrowser.FindFromNode(const Text: string; Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;
  while Assigned(Node) do
  begin
    if (Node.Level > 0) and (CaseInsensitivePos(Text, TBrowseClassInfoCollection(Node.Data).Name) > 0) then
    begin
      tvBrowse.Selected := Node;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

function TfmClassBrowser.Images: TImageList;
begin
  Result := GetSharedImageList;
end;

function TfmClassBrowser.ConfigurationKey: string;
begin
  Result := TClassExpert.ConfigurationKey;
end;

{ TClassExpert }

procedure TClassExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to do here.
    else
    begin
      if Assigned(fmClassBrowser) then
      begin
        if fmClassBrowser.Visible then
          fmClassBrowser.Close;

        FreeAndNil(fmClassBrowser);
      end;
    end;
  end;
end;

destructor TClassExpert.Destroy;
begin
  FreeAndNil(fmClassBrowser);

  inherited Destroy;
end;

function TClassExpert.GetActionCaption: string;
resourcestring
  SClassBrowserMenuCaption = 'Class Bro&wser';
begin
  Result := SClassBrowserMenuCaption;
end;

class function TClassExpert.GetName: string;
begin
  Result := 'ClassBrowser'; // do not localize
end;

procedure TClassExpert.Execute(Sender: TObject);
begin
  if fmClassBrowser = nil then
  begin
    fmClassBrowser := TfmClassBrowser.Create(nil);
    SetFormIcon(fmClassBrowser);
  end;
  if fmClassBrowser.WindowState = wsMinimized then
    fmClassBrowser.WindowState := wsNormal;
  fmClassBrowser.Show;
end;

function TClassExpert.HasConfigOptions: Boolean;
begin
  HasConfigOptions := False;
end;

function TClassExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningCPPBuilder;
end;

procedure TfmClassBrowser.actFilePrintClassReportExecute(Sender: TObject);
begin
  PrintClassReportBuiltIn;
end;

procedure TfmClassBrowser.actFilePrintClassHierarchyExecute(
  Sender: TObject);
begin
  PrintClassHierarchyBuiltIn;
end;

procedure TfmClassBrowser.actFileAddExecute(Sender: TObject);
var
  New, SDir: string;
  Node: TTreeNode;
  Ticks: DWORD;
  Item: TClassItem;
begin
  SDir := FStartingDir;
  if not GetDirectory(SDir) then
    Exit;
  FStartingDir := SDir;
  New := ExtractFileName(SDir);
  if not TfmClassIdentify.Execute(Self, New, FParseRecursing) then
    Exit;
  begin
    Node := tvBrowse.Items.Add(nil, New);
    Node.ImageIndex := ImageIndexClosedFolder;
    Node.SelectedIndex := ImageIndexOpenFolder;
    Ticks := GetTickCount;
    Item := ClassList.Add;
    Item.IsProject := False;
    Item.Directory := SDir;
    Item.Name := New;
    Item.Recurse := FParseRecursing;
    Item.Load;
    ClassList.SaveToFile(False);
    Node.Data := Pointer(Item);
    LoadObjects(Item, Node);
    StatusBar.SimpleText := GetClassesParsedText(Item.ClassCount, GetTickCount - Ticks);
    StatusBar.Repaint;
  end;
end;

procedure TfmClassBrowser.actFileRemoveExecute(Sender: TObject);
var
  Item: TClassItem;
begin
  if tvBrowse.Selected = nil then
  begin
    actFileRemove.Enabled := False;
    Exit;
  end;
  Item := TClassItem(tvBrowse.Selected.Data);
  tvBrowse.Selected.Free;
  FreeAndNil(Item);
  ClassList.SaveToFile(False);
  if tvBrowse.Selected = nil then
    actFileRemove.Enabled := False;
end;

procedure TfmClassBrowser.actFileRefreshExecute(Sender: TObject);
begin
  if tvBrowse.Selected = nil then
    Exit;
  if tvBrowse.Selected.Level = 0 then
    RefreshNode
  else
  begin
    TBrowseClassInfoCollection(tvBrowse.Selected.Data).LoadMethods;
    tvBrowseChange(tvBrowse, tvBrowse.Selected);
  end;
  ClassList.SaveToFile(False);
end;

procedure TfmClassBrowser.actEditFindExecute(Sender: TObject);
resourcestring
  SFindClass = 'Find Class';
  SEnterClassName = 'Enter a full or partial class name to find';
var
  Find: string;
begin
  if tvBrowse.Items.Count = 0 then Exit;
  Find := InputBox(SFindClass, SEnterClassName, '');
  if Find = '' then Exit;
  FLastFind := Find;
  FindFromNode(FLastFind, tvBrowse.Items[0]);
end;

procedure TfmClassBrowser.actGenericViewNewFilterExecute(Sender: TObject);
var
  SendingAction: TCustomAction;
begin
  SendingAction := Sender as TCustomAction;
  Assert(Assigned(SendingAction));
  SendingAction.Checked := not SendingAction.Checked;

  UpdateListFilter;
end;

procedure TfmClassBrowser.UpdateListFilter;
var
  OInfo: TBrowseClassInfoCollection;
begin
  if (tvBrowse.Selected = nil) or (tvBrowse.Selected.Level = 0) then
    Exit;
  Self.Cursor := crHourglass;
  try
    OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
    LoadList(OInfo);
    StatusBar.SimpleText := OInfo.SourceName + ': ' + IntToStr(OInfo.LineNo);
  finally
    Self.Cursor := crDefault;
  end;
end;

procedure TfmClassBrowser.SetupEditorControls;
begin
  FMethodText := TGxEnhancedEditor.Create(Self);
  FCodeText := TGxEnhancedEditor.Create(Self);

  FMethodText.Parent := pnlMethod;
  FMethodText.Align := alClient;
  FMethodText.ReadOnly := True;
  FMethodText.Highlighter := gxpPas;
  FCodeText.Parent := tshCode;
  FCodeText.Align := alClient;
  FCodeText.ReadOnly := True;
  FCodeText.Highlighter := gxpPas;
end;

procedure TfmClassBrowser.ViewBrowserDetails;
var
  OInfo: TBrowseClassInfoCollection;
begin
  if tvBrowse.Selected = nil then Exit;
  if tvBrowse.Selected.Level = 0 then Exit;
  OInfo := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
  with TfmClassProp.Create(Self) do
  try
    edtClassName.Text := OInfo.Name;
    edtDerivedFrom.Text := OInfo.DerivedFrom;
    mmoFileName.Text := OInfo.FileName;
    edtLineNo.Text := IntToStr(OInfo.LineNo);
    edtUnit.Text := OInfo.SourceName;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmClassBrowser.actViewClassPropertiesExecute(Sender: TObject);
begin
  if (ActiveControl = tvBrowse) then
    ViewBrowserDetails;
end;

procedure TfmClassBrowser.actHelpHelpExecute(Sender: TObject);
begin
  if not IsStandAlone then
    GxContextHelp(Self, 5);
end;

procedure TfmClassBrowser.actHelpContentsExecute(Sender: TObject);
begin
  if not IsStandAlone then
    GxContextHelpContents(Self);
end;

procedure TfmClassBrowser.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmClassBrowser.actOptionsOptionsExecute(Sender: TObject);
var
  Dlg: TfmClassOptions;
  i: Integer;
  Tag: Integer;
  gbxFilters: TGroupBox;
begin
  Dlg := TfmClassOptions.Create(nil);
  try
    Dlg.cbTreeView.ItemIndex := Dlg.cbTreeView.Items.IndexOf(tvBrowse.Font.Name);
    Dlg.cbListView.ItemIndex := Dlg.cbListView.Items.IndexOf(lvInfo.Font.Name);
    Dlg.cbEditor.ItemIndex := Dlg.cbEditor.Items.IndexOf(FCodeText.Font.Name);
    Dlg.udTree.Position := tvBrowse.Font.Size;
    Dlg.udList.Position := lvInfo.Font.Size;
    Dlg.udEditor.Position := FCodeText.Font.Size;
    Dlg.cbTop.Checked := FPrimitiveTop;
    Dlg.cbStayInPackage.Checked := FStayInPackage;
    Dlg.cbParseRecursing.Checked := FParseRecursing;
    Dlg.cbAutoHide.Checked := FAutomaticallyHideBrowser;
    { Set Filters }
    gbxFilters := Dlg.gbxFilters;
    for i := 0 to gbxFilters.ControlCount - 1 do
      if gbxFilters.Controls[i] is TCheckBox then
      begin
        Tag := TCheckBox(gbxFilters.Controls[i]).Tag;
        TCheckBox(gbxFilters.Controls[i]).Checked := FFilters[Tag];
      end;
    if Dlg.ShowModal = mrOk then
    begin
      for i := 0 to gbxFilters.ControlCount - 1 do
        if gbxFilters.Controls[i] is TCheckBox then
        begin
          Tag := TCheckBox(gbxFilters.Controls[i]).Tag;
          FFilters[Tag] := TCheckBox(gbxFilters.Controls[i]).Checked;
        end;
      tvBrowse.Font.Name := Dlg.cbTreeView.Text;
      lvInfo.Font.Name := Dlg.cbListView.Text;
      FCodeText.Font.Name := Dlg.cbEditor.Text;
      FCodeText.Font.Size := Trunc(Dlg.udEditor.Position);
      FMethodText.Font.Name := Dlg.cbEditor.Text;
      FMethodText.Font.Size := Trunc(Dlg.udEditor.Position);
      tvBrowse.Font.Size := Trunc(Dlg.udTree.Position);
      lvInfo.Font.Size := Trunc(Dlg.udList.Position);
      FPrimitiveTop := Dlg.cbTop.Checked;
      FStayInPackage := Dlg.cbStayInPackage.Checked;
      FParseRecursing := Dlg.cbParseRecursing.Checked;
      FAutomaticallyHideBrowser := Dlg.cbAutoHide.Checked;
      SaveSettings;
      FiltersToActions;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmClassBrowser.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmClassBrowser.actFilePrinterSetupExecute(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;

procedure TfmClassBrowser.actEditCopyExecute(Sender: TObject);
begin
  if (ActiveControl is TGxEnhancedEditor) then
    TGxEnhancedEditor(ActiveControl).CopyToClipboard;
end;

procedure TfmClassBrowser.SetInfoViewMode(const Value: TInfoViewMode);
begin
  tvBrowse.Selected := nil;

  if Value = FInfoViewMode then
    Exit;

  FInfoViewMode := Value;
  LoadAllObjects;
end;

procedure TfmClassBrowser.actViewListExecute(Sender: TObject);
begin
  SetInfoViewMode(vmList);
end;

procedure TfmClassBrowser.actViewTreeExecute(Sender: TObject);
begin
  SetInfoViewMode(vmTree);
end;

procedure TfmClassBrowser.actViewUnitNamesExecute(Sender: TObject);
begin
  FViewUnitNames := not FViewUnitNames;
  LoadAllObjects;
end;

procedure TfmClassBrowser.actEditGotoMemberExecute(Sender: TObject);
var
  Line: Integer;
  SourceFile: string;
  MInfo: TBrowseMethodInfoItem;
begin
  if (tvBrowse.Selected = nil) or (lvInfo.Selected = nil) then
    Exit;
  Line := 0;

  MInfo := TBrowseMethodInfoItem(lvInfo.Selected.Data);
  case MInfo.MethodType of
    ctConstant,
    ctType,
    ctVariable,
    ctProperty:    Line := MInfo.GetInterfaceLine;
    ctMethod:
      begin
        if MInfo.cAbstract then
          Line := MInfo.GetInterfaceLine
        else
          Line := MInfo.GetImplementationLine;
      end;
  end; // case
  Inc(Line);
  SourceFile := TBrowseClassInfoCollection(tvBrowse.Selected.Data).FileName;

  GxOtaGoToFileLine(SourceFile, Line);

  if FAutomaticallyHideBrowser then
    Self.Hide;
end;

procedure TfmClassBrowser.actEditGotoClassExecute(Sender: TObject);
var
  ClassInfos: TBrowseClassInfoCollection;
begin
  if tvBrowse.Selected = nil then
    Exit;
  ClassInfos := TBrowseClassInfoCollection(tvBrowse.Selected.Data);
  GxOtaGoToFileLine(ClassInfos.FileName, ClassInfos.RefreshLineNo + 1);
  if FAutomaticallyHideBrowser then
    Self.Hide;
end;

procedure TfmClassBrowser.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actEditCopy.Enabled := (ActiveControl is TGxEnhancedEditor);
  actEditGotoMember.Enabled := Assigned(lvInfo.Selected);
  actEditGotoClass.Enabled := Assigned(tvBrowse.Selected) and (TObject(tvBrowse.Selected.Data) is TBrowseClassInfoCollection);
  actEditFindNext.Enabled := FLastFind <> '';
  actViewClassProperties.Enabled := (actEditGotoClass.Enabled and tvBrowse.Focused);
  actFileAdd.Enabled := (ClassList.StoragePath <> '');
  actFileRemove.Enabled := Assigned(tvBrowse.Selected) and (tvBrowse.Selected.Level = 0);
  actViewList.Checked := (FInfoViewMode = vmList);
  actViewTree.Checked := (FInfoViewMode = vmTree);
  actViewUnitNames.Checked := FViewUnitNames;
  actViewProperties.Enabled := Assigned(tvBrowse.Selected) or Assigned(lvInfo.Selected);
end;

procedure TfmClassBrowser.tvBrowseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  // RightClickSelect in Delphi 5/6 is totally useless, so this is a workaround
  if Button = mbRight then
  begin
    Node := tvBrowse.GetNodeAt(X, Y);
    if Node <> nil then
      tvBrowse.Selected := Node;
  end;
end;

procedure TfmClassBrowser.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actFileExit.Execute;
  end;
end;

procedure TfmClassBrowser.actEditFindNextExecute(Sender: TObject);
var
  FindNode: TTreeNode;
begin
  FindNode := tvBrowse.Selected;
  if Assigned(FindNode) then
    FindNode := FindNode.GetNext;
  if (not Assigned(FindNode)) and (tvBrowse.Items.Count > 0) then
    FindNode := tvBrowse.Items[0];
  if Assigned(FindNode) then
    FindFromNode(FLastFind, FindNode);
end;

procedure TfmClassBrowser.tvBrowseDblClick(Sender: TObject);
begin
  actEditGotoClass.Execute;
end;

initialization
  RegisterGX_Expert(TClassExpert);
end.

