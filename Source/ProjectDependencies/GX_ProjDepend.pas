unit GX_ProjDepend;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, ComCtrls, ActnList, Menus, ToolWin, ExtCtrls,
  Forms, GX_Experts, GX_OtaUtils, GX_ConfigurationInfo, Dialogs, GX_BaseForm;

type
  TfmProjDepend = class(TfmBaseForm)
    StatusBar: TStatusBar;
    tvUnits: TTreeView;
    Splitter: TSplitter;
    pnlPageControlHost: TPanel;
    pcData: TPageControl;
    tshUnitUses: TTabSheet;
    lvUnitUses: TListView;
    tshUsedBy: TTabSheet;
    lvUsedBy: TListView;
    tshIndirect: TTabSheet;
    lvIndirect: TListView;
    pmTreeview: TPopupMenu;
    pmList: TPopupMenu;
    mitOpenUnitTree: TMenuItem;
    mitOpenUnitList: TMenuItem;
    N1: TMenuItem;
    mitIndirectUnitProperties: TMenuItem;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileRefresh: TMenuItem;
    mitFileExit: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitHelpAbout: TMenuItem;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnAbort: TToolButton;
    tbnSep1: TToolButton;
    tbnHelp: TToolButton;
    actFileRefresh: TAction;
    actFileAbort: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    actFileExit: TAction;
    mitHelpContents: TMenuItem;
    mitFileAbort: TMenuItem;
    actOpenUnitTree: TAction;
    mitFileSep1: TMenuItem;
    actOpenUnitList: TAction;
    actViewIndirectUnitProperties: TAction;
    actFileExport: TAction;
    actFileFilter: TAction;
    tbnSep2: TToolButton;
    tbnExport: TToolButton;
    tbnSep3: TToolButton;
    tbnFilter: TToolButton;
    dlgSave: TSaveDialog;
    mitFileExport: TMenuItem;
    mitFileFilter: TMenuItem;
    mitFileSep2: TMenuItem;
    procedure tvUnitsExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure pnlPageControlHostResize(Sender: TObject);
    procedure tvUnitsChange(Sender: TObject; Node: TTreeNode);
    procedure pcDataChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvUnitsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvIndirectCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOpenUnitTreeExecute(Sender: TObject);
    procedure actOpenUnitListExecute(Sender: TObject);
    procedure actViewIndirectUnitPropertiesExecute(Sender: TObject);
    procedure tvUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure actFileExportExecute(Sender: TObject);
    procedure actFileFilterExecute(Sender: TObject);
  private
    ProjectNotifier: TBaseIdeNotifier;
    FLastProject: string;
    FDoRefreshList: Boolean;
    FUnitList: TStringList;
    FFileList: TStringList;
    FSearchInProgress: Boolean;
    FAbortSignalled: Boolean;
    FFilterList: TStringList;
    UnitUsesSortColumn: Integer;
    UsedBySortColumn: Integer;
    IndirectSortColumn: Integer;
    procedure ShowUnitsUsed;
    procedure ShowUsedBy;
    procedure IndirectDepend;
    function GetFileName(const UName: string): string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OpenUnit(const UnitName: string);
    function LoadFileDepend(const FileName, UnitName, FormName: string): Boolean;
    procedure LoadFileDependencies;
    procedure BuildUses;
    procedure ClearFileList;
    procedure ClearUnitList;
    procedure UpdateFormActions;
    procedure ExportAllDependencies;
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TDependExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure SetActive(New: Boolean); override;
  private
    FScanEntireUnit: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Configure; override;
    procedure Click(Sender: TObject); override;
    function IsDefaultActive: Boolean; override;
  end;

var
  fmProjDepend: TfmProjDepend = nil;
  DependExpert: TDependExpert = nil;
  RootNode: TTreeNode = nil;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, ToolsAPI,
  mPasLex,
  GX_EditReader, GX_ProjDependProp, GX_GExperts, GX_ProjDependFilter,
  GX_GenericUtils, GX_GxUtils, GX_SharedImages, GX_IdeUtils, Math;

type
  TProjectNotifier = class(TBaseIdeNotifier)
  private
    fmDepend: TfmProjDepend;
  public
    constructor Create(Owner: TfmProjDepend);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
  end;

constructor TProjectNotifier.Create(Owner: TfmProjDepend);
begin
  inherited Create;
  fmDepend := Owner;
end;

procedure TProjectNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // Is this auto-update a good thing?  Should the user just click Refresh?
  if NotifyCode = ofnActiveProjectChanged then
  begin
    // Don't refresh if the project hasn't actually changed
    if GxOtaGetCurrentProjectFileName = fmDepend.FLastProject then
      Exit;

    fmDepend.ClearFileList;
    fmDepend.ClearUnitList;

    if fmDepend.Visible then
      fmDepend.BuildUses
    else
      fmDepend.FDoRefreshList := True;
  end;
end;

procedure TfmProjDepend.ClearFileList;
var
  i: Integer;
begin
  if FFileList <> nil then
  begin
    for i := 0 to FFileList.Count-1 do
      FFileList.Objects[i].Free;
    FFileList.Clear;
  end;
end;

procedure TfmProjDepend.ClearUnitList;
var
  i: Integer;
begin
  if FUnitList <> nil then
  begin
    for i := 0 to FUnitList.Count-1 do
      FUnitList.Objects[i].Free;
    FUnitList.Clear;
  end;
end;

function TfmProjDepend.LoadFileDepend(const FileName, UnitName, FormName: string): Boolean;
var
  EditRead: TEditReader;
  FileContent: string;
  Parser: TmwPasLex;
  nUses: Integer;
  Index: Integer;
  Node: TTreeNode;
  UList: TStringList;
  UInfo: TUnitInfo;
  UnitIdentifier: string;
begin
  Result := True;
  if FileName = GxOtaGetCurrentProjectFileName then
    Exit;

  UpdateFormActions;
  Application.ProcessMessages;

  if FAbortSignalled then
  begin
    Result := False;
    Exit;
  end;

  nUses := 0;
  UList := nil;

  StatusBar.SimpleText := FileName;
  StatusBar.Repaint;

  if not IsDprOrPas(FileName) then
    Exit;

  if FormName = '' then
    Index := ImageIndexUnit
  else
    Index := ImageIndexWindow;

  UInfo := TUnitInfo.Create;
  UInfo.SourceName := UnitName;
  UInfo.FileName := FileName;
  FFileList.AddObject(UnitName, UInfo);

  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  try
    EditRead := TEditReader.Create(FileName);
    try
      FileContent := EditRead.GetText;
    finally
      FreeAndNil(EditRead);
    end;
  except
    on E: Exception do
    begin
      // Warn, but skip project files that don't exist (dcu only, etc.?)
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
      Exit;
    end;
  end;
  Parser := TmwPasLex.Create;
  try
    Parser.Origin := @FileContent[1];
    while not (Parser.TokenID in [tkUnit, tkNull, tkLibrary, tkProgram]) do
    begin
      Parser.NextNoJunk;
    end;
    if Parser.TokenID in [tkUnit, tkLibrary, tkProgram] then
    begin
      Parser.NextNoJunk;
      if Parser.TokenID = tkIdentifier then
      begin
        UnitIdentifier := Parser.GetDottedIdentifierAtPos(True);
        Node := tvUnits.Items.AddChild(RootNode, UnitIdentifier);
        Node.HasChildren := True;
        Node.ImageIndex := Index;
        Node.SelectedIndex := Index;
        UList := TStringList.Create;
        FUnitList.AddObject(UnitIdentifier, UList);
      end;
    end;
    if UList = nil then
      Exit;
    while Parser.TokenID <> tkNull do
    begin
      if Parser.TokenID = tkUses then
      begin
        Inc(nUses);
        Parser.NextNoJunk;
        while not (Parser.TokenID in [tkSemiColon, tkNull]) do
        begin
          if Parser.TokenID = tkIdentifier then
            UList.Add(Parser.GetDottedIdentifierAtPos(True));
          Parser.NextNoJunk;
          if Parser.TokenID = tkIn then
            while not (Parser.TokenID in [tkSemiColon, tkComma, tkNull]) do
              Parser.NextNoJunk;
        end;
      end;
      if (DependExpert <> nil) and (not DependExpert.FScanEntireUnit) and
         (nUses >= 2) then
      begin
        Break;
      end;
      Parser.NextNoJunk;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TfmProjDepend.LoadFileDependencies;
var
  i: Integer;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  ContinueProcessing: Boolean;
begin
  CurrentProject := GxOtaGetCurrentProject;
  if not Assigned(CurrentProject) then
    Exit;

  for i := 0 to CurrentProject.GetModuleCount-1 do
  begin
    ModuleInfo := CurrentProject.GetModule(i);
    UpdateFormActions;
    Application.ProcessMessages;
    ContinueProcessing := LoadFileDepend(ModuleInfo.FileName, ModuleInfo.Name, ModuleInfo.FormName);
    if not ContinueProcessing then
      Break;
  end;
end;

procedure TfmProjDepend.BuildUses;
resourcestring
  SParsingUnits = 'Parsing units...';
var
  Cursor: IInterface;
begin
  ClearFileList;
  ClearUnitList;

  FSearchInProgress := True;
  FAbortSignalled := False;

  tvUnits.Items.BeginUpdate;
  Cursor := TempHourGlassCursor;
  try
    // Clear current scroll box.
    lvIndirect.Items.Clear;
    tvUnits.Items.Clear;

    // Start parsing.
    StatusBar.SimpleText := SParsingUnits;
    StatusBar.Repaint;

    RootNode := tvUnits.Items.Add(nil, ExtractFileName(GxOtaGetCurrentProjectFileName));

    RootNode.ImageIndex := ImageIndexWindows;
    RootNode.SelectedIndex := ImageIndexWindows;

    LoadFileDependencies;

    RootNode.Expand(False);
    tvUnits.AlphaSort;

    FLastProject := GxOtaGetCurrentProjectFileName;
  finally
    tvUnits.Items.EndUpdate;

    FSearchInProgress := False;
    FAbortSignalled := False;
  end;
end;

function TfmProjDepend.GetFileName(const UName: string): string;
var
  UInfo: TUnitInfo;
  i: Integer;
begin
  Result := '';
  i := FFileList.IndexOf(UName);
  if i >= 0 then
  begin
    UInfo := TUnitInfo(FFileList.Objects[i]);
    Result := UInfo.FileName;
  end;
end;

procedure TfmProjDepend.tvUnitsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  i, j, k: Integer;
  UseUnitsList: TStringList;
  CNode: TTreeNode;
begin
  AllowExpansion := True;
  if Node = RootNode then
    Exit;

  while Node.Count > 0 do
    Node.Item[0].Free;

  i := FUnitList.IndexOf(Node.Text);
  if i >= 0 then
  begin
    UseUnitsList := TStringList(FUnitList.Objects[i]);
    for j := 0 to UseUnitsList.Count - 1 do
    begin
      CNode := tvUnits.Items.AddChild(Node, UseUnitsList.Strings[j]);
      CNode.SelectedIndex := ImageIndexUnit;
      CNode.ImageIndex := ImageIndexUnit;
      k := FUnitList.IndexOf(UseUnitsList.Strings[j]);
      CNode.HasChildren := ((k > -1) and (TStringList(FUnitList.Objects[k]).Count > 0));
    end;
    if UseUnitsList.Count = 0 then
      Node.HasChildren := False;
  end
  else
    Node.HasChildren := False;
end;

procedure TfmProjDepend.pnlPageControlHostResize(Sender: TObject);
begin
  with lvUnitUses do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
  with lvUsedBy do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
  with lvIndirect do
    Columns[1].Width := Max(ClientWidth - Columns[0].Width, 0);
end;

procedure TfmProjDepend.tvUnitsChange(Sender: TObject; Node: TTreeNode);
begin
  pcDataChange(pcData);
end;

procedure TfmProjDepend.pcDataChange(Sender: TObject);
begin
  if pcData.ActivePage = tshUnitUses then
    ShowUnitsUsed
  else
  if pcData.ActivePage = tshUsedBy then
    ShowUsedBy
  else
  if pcData.ActivePage = tshIndirect then
    IndirectDepend;

  // A VCL5 bug prevents headers from drawing right unless we poke around here
  pnlPageControlHost.Width := pnlPageControlHost.Width - 1;
end;

procedure TfmProjDepend.ShowUnitsUsed;
var
  i, j: Integer;
  List: TStringList;
  ListItem: TListItem;
begin
  lvUnitUses.Items.BeginUpdate;
  try
    lvUnitUses.Items.Clear;

    if tvUnits.Selected = nil then
      Exit;

    i := FUnitList.IndexOf(tvUnits.Selected.Text);
    if i >= 0 then
    begin
      List := TStringList(FUnitList.Objects[i]);
      for j := 0 to List.Count - 1 do
      begin
        if FFilterList.IndexOf(List.Strings[j]) = -1 then
        begin
          ListItem := lvUnitUses.Items.Add;
          ListItem.Caption := List.Strings[j];
          ListItem.SubItems.Add(GetFileName(List.Strings[j]));
        end;
      end;
    end;
  finally
    lvUnitUses.Items.EndUpdate;
  end;
end;

procedure TfmProjDepend.ShowUsedBy;
var
  i, j: Integer;
  List: TStringList;
  ListItem: TListItem;
begin
  lvUsedBy.Items.BeginUpdate;
  try
    lvUsedBy.Items.Clear;

    if tvUnits.Selected = nil then
      Exit;

    for i := 0 to FUnitList.Count - 1 do
    begin
      List := TStringList(FUnitList.Objects[i]);
      for j := 0 to List.Count - 1 do
      begin
        if SameText(tvUnits.Selected.Text, List.Strings[j]) then
        begin
          ListItem := lvUsedBy.Items.Add;
          ListItem.Caption := FUnitList.Strings[i];
          ListItem.SubItems.Add(GetFileName(FUnitList.Strings[i]));
        end;
      end;
    end;
  finally
    lvUsedBy.Items.EndUpdate;
  end;
end;

// This routine is in desperate need of optimization
procedure TfmProjDepend.IndirectDepend;

  function UsedUnitIsInCommaSeparatedList(const UsedUnit, CommaList: string): Boolean;
  var
    List: TStrings;
  begin
    List := TStringList.Create;
    try
      List.CommaText := CommaList;

      Result := (List.IndexOf(UsedUnit) >= 0);
    finally
      FreeAndNil(List);
    end;
  end;

  procedure AddListViewUsesEntries(const UsingUnit, UsedUnit: string);
  var
    i: Integer;
    ExistingUnitList: string;
    ListItem: TListItem;
  begin
    if FFilterList.IndexOf(UsingUnit) <> -1 then
      Exit;

    i := lvIndirect.Items.Count-1;
    while i >= 0 do
    begin
      if SameText(UsingUnit, lvIndirect.Items[i].Caption) then
        Break;
      Dec(i);
    end;

    if i < 0 then
    begin
      ListItem := lvIndirect.Items.Add;
      ListItem.Caption := UsingUnit;
      ListItem.SubItems.Add(UsedUnit);
    end
    else
    begin
      ExistingUnitList := lvIndirect.Items[i].SubItems[0];
      if not UsedUnitIsInCommaSeparatedList(UsedUnit, ExistingUnitList) then
      begin
        ExistingUnitList := ExistingUnitList + ', ' + UsedUnit;
        lvIndirect.Items[i].SubItems[0] := ExistingUnitList;
      end;
    end;
  end;

  procedure AddItems(ProcessedUnitsList: TStrings; const UnitName: string);
  var
    i: Integer;
    UseUnitsList: TStringList;
  begin
    i := FUnitList.IndexOf(UnitName);
    if i >= 0 then
    begin
      UseUnitsList := TStringList(FUnitList.Objects[i]);

      for i := 0 to UseUnitsList.Count-1 do
      begin
        AddListViewUsesEntries(UseUnitsList.Strings[i], UnitName);

        if ProcessedUnitsList.IndexOf(UseUnitsList.Strings[i]) < 0 then
        begin
          ProcessedUnitsList.Add(UseUnitsList.Strings[i]);
          AddItems(ProcessedUnitsList, UseUnitsList.Strings[i]);
        end;
      end;
    end;
  end;

var
  List: TStrings;
  Cursor: IInterface;
begin
  if tvUnits.Selected = nil then
    Exit;

  Cursor := TempHourGlassCursor;
  lvIndirect.Items.Clear;
  List := TStringList.Create;
  try
    lvIndirect.Items.BeginUpdate;
    try
      AddItems(List, tvUnits.Selected.Text);
    finally
      lvIndirect.Items.EndUpdate;
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmProjDepend.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.SaveForm(Self, ConfigurationKey + '\Window');
    Settings.WriteInteger(ConfigurationKey + '\Window', 'Splitter', tvUnits.Width);
    Settings.WriteString(ConfigurationKey, 'ExcludedFiles', FFilterList.CommaText);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmProjDepend.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.LoadForm(Self, ConfigurationKey + '\Window');
    tvUnits.Width := Settings.ReadInteger(ConfigurationKey + '\Window', 'Splitter', tvUnits.Width);
    FFilterList.CommaText := Settings.ReadString(ConfigurationKey, 'ExcludedFiles', '');
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmProjDepend.OpenUnit(const UnitName: string);
resourcestring
  SFileDoesNotExist = '%s does not exist';
  SOpenError = 'Unable to open unit ';
var
  i: Integer;
  CurrentFileName: string;
  ActionServices: IOTAActionServices;
begin
  i := FFileList.IndexOf(UnitName);
  if i >= 0 then
  begin
    CurrentFileName := TUnitInfo(FFileList.Objects[i]).FileName;
    if FileExists(CurrentFileName) then
    begin
      ActionServices := BorlandIDEServices as IOTAActionServices;
      Assert(Assigned(ActionServices));
      if not ActionServices.OpenFile(CurrentFileName) then
        MessageDlg(Format(SOpenError, [CurrentFileName]), mtInformation, [mbOK], 0);
    end
    else
      MessageDlg(Format(SFileDoesNotExist, [CurrentFileName]), mtError, [mbOK], 0);
  end
  else
    if not GxOtaOpenFileFromPath(UnitName + '.pas') then
      MessageDlg(SOpenError + UnitName, mtInformation, [mbOK], 0);
end;

procedure TfmProjDepend.FormActivate(Sender: TObject);
begin
  if FDoRefreshList then
  begin
    FDoRefreshList := False;
    BuildUses;
  end;
end;

procedure TfmProjDepend.tvUnitsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmProjDepend.lvColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Sender = lvUnitUses then
    UnitUsesSortColumn := Column.Index;
  if Sender = lvUsedBy then
    UsedBySortColumn   := Column.Index;
  if Sender = lvIndirect then
    IndirectSortColumn := Column.Index;

  (Sender as TCustomListView).AlphaSort;
end;

procedure TfmProjDepend.lvIndirectCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
  SortColumn: Integer;
begin
  SortColumn := 0;

  if Sender = lvUnitUses then
    SortColumn := UnitUsesSortColumn;
  if Sender = lvUsedBy then
    SortColumn := UsedBySortColumn;
  if Sender = lvIndirect then
    SortColumn := IndirectSortColumn;

  if not (SortColumn in [0, 1]) then
    SortColumn := 0;

  if SortColumn = 0 then
    Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
  begin
    ix := SortColumn - 1;
    Compare := AnsiCompareText(Item1.SubItems[ix], Item2.SubItems[ix]);
  end;
end;

constructor TfmProjDepend.Create(AOwner: TComponent);
begin
  inherited;
  SetToolbarGradient(ToolBar);
  SetNonModalFormPopupMode(Self);
  FFilterList := TStringList.Create;

  pcData.ActivePageIndex := 0;

  FUnitList := TStringList.Create;
  FFileList := TStringList.Create;

  UnitUsesSortColumn := -1;
  IndirectSortColumn := -1;
  UsedBySortColumn := -1;

  CenterForm(Self);

  LoadSettings;

  ProjectNotifier := TProjectNotifier.Create(Self);
  ProjectNotifier.AddNotifierToIDE;
end;

destructor TfmProjDepend.Destroy;
begin
  SaveSettings;

  ClearUnitList;
  ClearFileList;
  FreeAndNil(FUnitList);
  FreeAndNil(FFileList);
  FreeAndNil(FFilterList);

  ProjectNotifier.RemoveNotifierFromIDE;
  ProjectNotifier := nil; // freed by IDE

  inherited Destroy;

  fmProjDepend := nil;
end;

procedure TfmProjDepend.actFileRefreshExecute(Sender: TObject);
begin
  BuildUses;
end;

procedure TfmProjDepend.actFileAbortExecute(Sender: TObject);
begin
  FAbortSignalled := True;
end;

procedure TfmProjDepend.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmProjDepend.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 6);
end;

procedure TfmProjDepend.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmProjDepend.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmProjDepend.actOpenUnitTreeExecute(Sender: TObject);
begin
  if tvUnits.Selected <> nil then
    OpenUnit(tvUnits.Selected.Text);
end;

procedure TfmProjDepend.actOpenUnitListExecute(Sender: TObject);

  procedure CallOpenUnit(lv: TListView);
  begin
    if lv.Selected <> nil  then
      OpenUnit(lv.Selected.Caption);
  end;

begin
  if pcData.ActivePage = tshUnitUses then
    CallOpenUnit(lvUnitUses)
  else
  if pcData.ActivePage = tshUsedBy then
    CallOpenUnit(lvUsedBy)
  else
  if pcData.ActivePage = tshIndirect then
    CallOpenUnit(lvIndirect);
end;

procedure TfmProjDepend.actViewIndirectUnitPropertiesExecute(
  Sender: TObject);
begin
  if lvIndirect.Selected = nil then
    Exit;

  with TfmProjDependProp.Create(nil) do
  try
    laFileName.Caption := lvIndirect.Selected.Caption;
    lbxSource.Items.CommaText := lvIndirect.Selected.SubItems[0];
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmProjDepend.tvUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Don't allow Grey-'*' for expand all, since we might have circular dependencies
  if Key = VK_MULTIPLY then
    Key := 0;
end;

procedure TfmProjDepend.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  UpdateFormActions;
end;

procedure TfmProjDepend.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    Key := #0;
    actFileExit.Execute;
  end;
end;

procedure TfmProjDepend.UpdateFormActions;
begin
  actOpenUnitTree.Enabled := (tvUnits.Selected <> nil);
  actViewIndirectUnitProperties.Enabled := (pcData.ActivePage = tshIndirect);
  actFileRefresh.Enabled := not FSearchInProgress;
  actFileAbort.Enabled := FSearchInProgress;
  actFileExport.Enabled := not FSearchInProgress;
  actFileFilter.Enabled := not FSearchInProgress;
end;

procedure TfmProjDepend.actFileExportExecute(Sender: TObject);
begin
  ExportAllDependencies;
end;

procedure TfmProjDepend.actFileFilterExecute(Sender: TObject);
begin
  with TfmProjDependFilter.Create(nil) do
  try
    UnitList := FFilterList;
    if ShowModal = mrOk then
    begin
      FFilterList.Assign(UnitList);
      pcDataChange(pcData);
    end;
  finally
    Free;
  end;
end;

procedure TfmProjDepend.ExportAllDependencies;

  procedure AddListViewUsesEntries(ExportList: TStrings; const BaseUnit, UsingUnit, UsedUnit: string);
  var
    i: Integer;
    Str: string;
  begin
    if FFilterList.IndexOf(UsingUnit) <> -1 then
      Exit;

    i := ExportList.Count - 1;
    while i >= 0 do
    begin
      Str := ExportList[i];
      Str := Copy(Str, Pos(',', Str) + 1, MaxInt);
      if SameText(UsingUnit, Str) then
        Break;
      Dec(i);
    end;

    if i < 0 then
      ExportList.Add(BaseUnit + ',' + UsingUnit);
  end;

  procedure AddItems(ProcessedUnitsList, ExportList: TStrings; const BaseUnit, UnitName: string);
  var
    i, j: Integer;
    UseUnitsList: TStringList;
  begin
    i := FUnitList.IndexOf(UnitName);
    if i >= 0 then
    begin
      // Get list of items used by this project module, and recursively add for each one
      UseUnitsList := TStringList(FUnitList.Objects[i]);
      for j := 0 to UseUnitsList.Count-1 do
      begin
        AddListViewUsesEntries(ExportList, BaseUnit, UseUnitsList.Strings[j], UnitName);
        if ProcessedUnitsList.IndexOf(UseUnitsList.Strings[j]) < 0 then
        begin
          ProcessedUnitsList.Add(UseUnitsList.Strings[j]);
          AddItems(ProcessedUnitsList, ExportList, BaseUnit, UseUnitsList.Strings[j]);
        end;
      end;
    end;
  end;

var
  List: TStringList;
  OutputData: TStringList;
  TempData: TStringList;
  i: Integer;
  Cursor: IInterface;
begin
  List := nil;
  TempData := nil;
  OutputData := TStringList.Create;
  try
    if dlgSave.Execute then
    begin
      Cursor := TempHourGlassCursor;
      TempData := TStringList.Create;
      List := TStringList.Create;
      for i := 0 to tvUnits.Items.Count - 1 do
      begin
        TempData.Clear;
        AddItems(List, TempData, tvUnits.Items[i].Text, tvUnits.Items[i].Text);
        OutputData.AddStrings(TempData);
      end;
      OutputData.SaveToFile(dlgSave.FileName);
    end;
  finally
    FreeAndNil(List);
    FreeAndNil(TempData);
    FreeAndNil(OutputData);
  end;
end;

function TfmProjDepend.ConfigurationKey: string;
begin
  Result := TDependExpert.ConfigurationKey;
end;

{ TDependExpert }

constructor TDependExpert.Create;
begin
  inherited Create;
  DependExpert := Self;
end;

destructor TDependExpert.Destroy;
begin
  DependExpert := nil;

  FreeAndNil(fmProjDepend);

  inherited Destroy;
end;

procedure TDependExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here.
    else
      FreeAndNil(fmProjDepend);
  end;
end;

function TDependExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Project &Dependencies';
begin
  Result := SMenuCaption;
end;

class function TDependExpert.GetName: string;
begin
  Result := 'ProjectDependencies';
end;

procedure TDependExpert.Configure;
resourcestring
  SConfigureExplanation = 'Do you want Project Dependencies to scan the entire unit for uses clauses?' + sLineBreak +
                          sLineBreak +
                          'This is slower, but will also find additional, unusual uses clauses that have been '+
                          'conditionally defined.';
begin
  FScanEntireUnit := (MessageDlg(SConfigureExplanation, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TDependExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  Settings.WriteBool(ConfigurationKey, 'ScanEntireUnit', FScanEntireUnit);
end;

procedure TDependExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  FScanEntireUnit := Settings.ReadBool(ConfigurationKey, 'ScanEntireUnit', False);
end;

procedure TDependExpert.Click(Sender: TObject);
begin
  if fmProjDepend = nil then
  begin
    fmProjDepend := TfmProjDepend.Create(nil);
    SetFormIcon(fmProjDepend);
    fmProjDepend.BuildUses;
  end;
  if fmProjDepend.WindowState = wsMinimized then
    fmProjDepend.WindowState := wsNormal;
  fmProjDepend.Show;
end;

function TDependExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningCPPBuilder;
end;

procedure TDependExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaHaveCurrentProject;
end;

initialization
  RegisterGX_Expert(TDependExpert);
end.

