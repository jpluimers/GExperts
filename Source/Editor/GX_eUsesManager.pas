unit GX_eUsesManager;

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, Forms, Menus, ComCtrls,
  ExtCtrls, ActnList, Dialogs, StdCtrls,
  GX_ConfigurationInfo, GX_EditorExpert, GX_GenericUtils, GX_BaseForm;

type
  TUsesExpert = class(TEditorExpert)
  private
    FFavoriteUnits: TStringList;
    FSingleActionMode: Boolean;
    FAvailTabIndex: Integer;
    procedure InternalExecute;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

type
  TfmUsesManager = class(TfmBaseForm)
    pnlUnits: TPanel;
    Splitter: TSplitter;
    pmuUses: TPopupMenu;
    mitInterfaceDelete: TMenuItem;
    mitUsesMoveToImplementation: TMenuItem;
    pmuAvail: TPopupMenu;
    mitAvailAddToImpl: TMenuItem;
    mitAvailAddToIntf: TMenuItem;
    pnlUses: TPanel;
    pcUnits: TPageControl;
    tabProject: TTabSheet;
    pnlProject: TPanel;
    lbxProject: TListBox;
    tabCommon: TTabSheet;
    pnlCommon: TPanel;
    lbxCommon: TListBox;
    tabFavorite: TTabSheet;
    pnlFavorite: TPanel;
    lbxFavorite: TListBox;
    pcUses: TPageControl;
    tabInterface: TTabSheet;
    pnlInterface: TPanel;
    lbxInterface: TListBox;
    tabImplementation: TTabSheet;
    pnlImplementation: TPanel;
    lbxImplementation: TListBox;
    mitUsesOpenUnit: TMenuItem;
    mitIntfSep1: TMenuItem;
    pnlFooter: TPanel;
    pnlFavFooter: TPanel;
    btnFavoriteAddToInterface: TButton;
    btnFavoriteAddToImplementation: TButton;
    btnFavoriteAddToFavorites: TButton;
    btnFavoriteDeleteFromFavorites: TButton;
    pnlProjFooter: TPanel;
    btnProjectAddToInterface: TButton;
    btnProjectAddToImplementation: TButton;
    pnlCommonFooter: TPanel;
    btnCommonAddToInterface: TButton;
    btnCommonAddToImplementation: TButton;
    dlgOpen: TOpenDialog;
    ActionList: TActionList;
    actUsesDelete: TAction;
    actIntfMoveToImpl: TAction;
    actFavDelete: TAction;
    actFavAdd: TAction;
    actAvailAddToImpl: TAction;
    actAvailAddToIntf: TAction;
    pnlImplFooter: TPanel;
    btnImplDelete: TButton;
    btnImplMoveToIntf: TButton;
    pnlIntfFooter: TPanel;
    btnIntfDelete: TButton;
    btnIntfMoveToImpl: TButton;
    actImplMoveToIntf: TAction;
    actUsesOpenUnit: TAction;
    tabSearchPath: TTabSheet;
    pnlSearchPathFooter: TPanel;
    btnSearchPathAddToIntf: TButton;
    btnSearchPathAddToImpl: TButton;
    pnlSearchPath: TPanel;
    lbxSearchPath: TListBox;
    pnlAvailableHeader: TPanel;
    edtFilter: TEdit;
    lblFilter: TLabel;
    mitAvailAddToFav: TMenuItem;
    mitAvailDelFromFav: TMenuItem;
    mitAvailSep1: TMenuItem;
    chkSingleActionMode: TCheckBox;
    actAvailOpenUnit: TAction;
    mitAvailSep2: TMenuItem;
    mitAvailOpenUnit: TMenuItem;
    actUsesAddToFavorites: TAction;
    mitUsesMoveToInterface: TMenuItem;
    mitUsesAddToFavorites: TMenuItem;
    lblUnits: TPanel;
    lblUses: TPanel;
    pnlButtonsRight: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbxImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxInterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxUsedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acUsesDeleteExecute(Sender: TObject);
    procedure actIntfMoveToImplExecute(Sender: TObject);
    procedure actImplMoveToIntfExecute(Sender: TObject);
    procedure actFavDeleteExecute(Sender: TObject);
    procedure actFavAddExecute(Sender: TObject);
    procedure actAvailAddToIntfExecute(Sender: TObject);
    procedure actAvailAddToImplExecute(Sender: TObject);
    procedure lbxInterfaceDblClick(Sender: TObject);
    procedure lbxAvailDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxImplementationDblClick(Sender: TObject);
    procedure actUsesOpenUnitExecute(Sender: TObject);
    procedure lbxAvailDblClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actAvailOpenUnitExecute(Sender: TObject);
    procedure actUsesAddToFavoritesExecute(Sender: TObject);
  private
    FAliases: TStringList;
    FFindThread: TFileFindThread;
    procedure GetCommonFiles;
    procedure GetProjectFiles;
    procedure AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean);
    procedure AddToIntfSection(const UnitName: string);
    procedure DeleteFromIntfSection(const UnitName: string);
    procedure DeleteFromImplSection(const UnitName: string);
    procedure OpenUnit(const UnitName: string);
    procedure ReadUsesList;
    function ApplyAlias(const UnitName: string): string;
    procedure AddListToIntfSection(ListBox: TObject);
    procedure AddListToImplSection(ListBox: TObject; RemoveFromInterface: Boolean);
    procedure AddListToFavorites(ListBox: TListBox);
    function HaveSelectedItem(ListBox: TListBox): Boolean;
    procedure DeleteItemIndex(ListBox: TListBox; FromInterface: Boolean);
    procedure DeleteSelected(ListBox: TListBox; FromInterface: Boolean);
    procedure MoveSelected(Src, Dest: TListBox; ToInterface: Boolean);
    function GetAvailableSourceListBox: TListBox;
    function GetUsesSourceListBox: TListBox;
    procedure SearchPathReady;
    procedure DeleteFromFavorites(const Item: string);
    procedure AddToFavorites(const Item: string);
    procedure FilterVisibleUnits;
    procedure SelectFirstItemInLists;
    procedure SaveChanges;
    procedure CloseIfInSingleActionMode;
  protected
    FProjectUnits: TStringList;
    FCommonUnits: TStringList;
    FFavoriteUnits: TStringList;
    FSearchPathUnits: TStringList;
    UsesExpert: TUsesExpert;
  end;

implementation

uses
  SysUtils, Messages, Windows, Graphics, ToolsAPI,
  GX_OtaUtils, GX_IdeUtils, GX_UsesManager;

{$R *.dfm}

{ TUsesExpert }

constructor TUsesExpert.Create;
begin
  inherited;
  ShortCut := scShift + scAlt + Ord('U');
  FFavoriteUnits := TStringList.Create;
  LoadSettings;
end;

destructor TUsesExpert.Destroy;
begin
  SaveSettings;
  FreeAndNil(FFavoriteUnits);
  inherited;
end;

procedure TUsesExpert.Execute(Sender: TObject);
begin
  InternalExecute;
end;

function TUsesExpert.GetDisplayName: string;
resourcestring
  SUsesExpert = 'Uses Clause Manager';
begin
  Result := SUsesExpert;
end;

procedure TUsesExpert.GetHelpString(List: TStrings);
resourcestring
  SUsesExpertHelp =
    '  This expert is designed to help you manage the uses clauses of your Delphi files.  ' +
    'You can delete and move units between the interface and implementation sections of ' +
    'the file you are editing using the buttons or drag and drop.  You can also add units from:'#13 +
    '  - The effective search path (project root, project search paths, IDE library paths)'#13 +
    '  - The current project'#13 +
    '  - Delphi''s common VCL/RTL units'#13 +
    '  - A user-defined favorite units list'#13 +
    '  The filter control allows case-insensitive filtering of the available unit lists.'#13 +
    '  There is a "Single action/quick add mode" checkbox that automatically closes ' +
    'the form when you perform an action on a uses clause or automatically adds the ' +
    'selected units to the active uses clause when the OK button is selected.';
begin
  List.Text := SUsesExpertHelp;
end;

class function TUsesExpert.GetName: string;
begin
  Result := 'UsesClauseMgr';
end;

function TUsesExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TUsesExpert.InternalExecute;
var
  Form: TfmUsesManager;
  Bitmap: TBitmap;
begin
  AssertIsPasOrInc(GxOtaGetCurrentSourceFile);
  Form := TfmUsesManager.Create(Application);
  try
    Bitmap := GetBitmap;
    if Assigned(Bitmap) then
      ConvertBitmapToIcon(Bitmap, Form.Icon);

    Form.FFavoriteUnits.Assign(FFavoriteUnits);
    Form.UsesExpert := Self;
    Form.chkSingleActionMode.Checked := FSingleActionMode;
    if (FAvailTabIndex >= 0) and (FAvailTabIndex < Form.pcUnits.PageCount) then
      Form.pcUnits.ActivePageIndex := FAvailTabIndex;
    if Form.ShowModal = mrOk then
    begin
      FFavoriteUnits.Assign(Form.FFavoriteUnits);
      FSingleActionMode := Form.chkSingleActionMode.Checked;
      FAvailTabIndex := Form.pcUnits.ActivePageIndex;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TUsesExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FFavoriteUnits.CommaText := Settings.ReadString(ConfigurationKey, 'Favorites', '');
  FSingleActionMode := Settings.ReadBool(ConfigurationKey, 'SingleActionMode', False);
  FAvailTabIndex := Settings.ReadInteger(ConfigurationKey, 'AvailTabIndex', 0);
end;

procedure TUsesExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  Settings.WriteString(ConfigurationKey, 'Favorites', FFavoriteUnits.CommaText);
  Settings.WriteBool(ConfigurationKey, 'SingleActionMode', FSingleActionMode);
  Settings.WriteInteger(ConfigurationKey, 'AvailTabIndex', FAvailTabIndex);
end;

{ TfmEditUsesExpert }

procedure TfmUsesManager.GetProjectFiles;
var
  IProject: IOTAProject;
  IModuleInfo: IOTAModuleInfo;
  i: Integer;
  FileName: string;
begin
  IProject := GxOtaGetCurrentProject;
  if not Assigned(IProject) then
    Exit;
  for i := 0 to IProject.GetModuleCount - 1 do
  begin
    IModuleInfo := IProject.GetModule(i);
    Assert(IModuleInfo <> nil);

    FileName := IModuleInfo.FileName;
    // We don't want blank names, packages, etc.
    if IsPas(FileName) then
      FProjectUnits.Add(ExtractPureFileName(FileName));
  end;
end;

procedure TfmUsesManager.GetCommonFiles;
var
  Found: Integer;
  SearchRec: TSearchRec;
begin
  // Read all dcu files from the $(DELPHI)\lib directory
  Found := SysUtils.FindFirst(AddSlash(ExtractFilePath(GetIdeRootDirectory)) +
    'lib' + PathDelim + '*.dcu', $3F, SearchRec);
  try
    while Found = 0 do
    begin
      if not ((SearchRec.Attr and faDirectory) = faDirectory) then
        FCommonUnits.Add(ExtractPureFileName(SearchRec.Name));
      Found := SysUtils.FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TfmUsesManager.FormCreate(Sender: TObject);
begin
  FProjectUnits := TStringList.Create;
  FCommonUnits := TStringList.Create;
  FFavoriteUnits := TStringList.Create;
  FSearchPathUnits := TStringList.Create;
  FAliases := TStringList.Create;

  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.pas');
  FFindThread.FileMasks.Add('*.dcu');
  GxOtaGetEffectiveLibraryPath(FFindThread.SearchDirs);
  FFindThread.OnFindComplete := SearchPathReady;
  FFindThread.StartFind;

  pcUnits.ActivePageIndex := 0;
  pcUses.ActivePageIndex := 0;
  GxOtaGetUnitAliases(FAliases);
  GetCommonFiles;
  GetProjectFiles;
  ReadUsesList;
end;

procedure TfmUsesManager.FormDestroy(Sender: TObject);
begin
  FFindThread.OnFindComplete := nil;
  FFindThread.Terminate;
  FreeAndNil(FAliases);
  FreeAndNil(FFindThread);
  FreeAndNil(FProjectUnits);
  FreeAndNil(FCommonUnits);
  FreeAndNil(FFavoriteUnits);
  FreeAndNil(FSearchPathUnits);
end;

procedure TfmUsesManager.AddListToIntfSection(ListBox: TObject);
var
  i: Integer;
begin
  if ListBox is TListBox then
    for i := 0 to TListBox(ListBox).Items.Count - 1 do
      if TListBox(ListBox).Selected[i] then
        AddToIntfSection(TListBox(ListBox).Items[i]);
end;

procedure TfmUsesManager.AddToIntfSection(const UnitName: string);
begin
  EnsureStringInList(lbxInterface.Items, UnitName);
  DeleteStringFromList(lbxImplementation.Items, UnitName);
end;

procedure TfmUsesManager.DeleteFromIntfSection(const UnitName: string);
begin
  DeleteStringFromList(lbxInterface.Items, UnitName);
end;

procedure TfmUsesManager.OpenUnit(const UnitName: string);
var
  FileName: string;
begin
  FileName := UnitName + '.pas';
  GxOtaOpenFileFromPath(FileName);
end;

procedure TfmUsesManager.AddListToImplSection(ListBox: TObject; RemoveFromInterface: Boolean);
var
  i: Integer;
begin
  if ListBox is TListBox then
    for i := 0 to TListBox(ListBox).Items.Count - 1 do
      if TListBox(ListBox).Selected[i] then
        AddToImplSection(TListBox(ListBox).Items[i], RemoveFromInterface);
end;

procedure TfmUsesManager.AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean);
begin
  if RemoveFromInterface then
    DeleteStringFromList(lbxInterface.Items, UnitName);
  if lbxInterface.Items.IndexOf(UnitName) = -1 then
    EnsureStringInList(lbxImplementation.Items, UnitName);
end;

procedure TfmUsesManager.DeleteFromImplSection(const UnitName: string);
begin
  DeleteStringFromList(lbxImplementation.Items, UnitName);
end;

function TfmUsesManager.ApplyAlias(const UnitName: string): string;
var
  i: Integer;
begin
  Result := UnitName;
  i := FAliases.IndexOfName(Result);
  if i <> -1 then
    Result := FAliases.Values[Result];
end;

procedure TfmUsesManager.ReadUsesList;
var
  i: Integer;
  UsesManager: TUsesManager;
begin
  lbxInterface.Clear;
  lbxImplementation.Clear;
  UsesManager := TUsesManager.Create(GxOtaGetCurrentSourceEditor);
  try
    for i := 0 to UsesManager.InterfaceUses.Count - 1 do
      lbxInterface.Items.Add(ApplyAlias(UsesManager.InterfaceUses.Items[i].Name));
    for i := 0 to UsesManager.ImplementationUses.Count - 1 do
      lbxImplementation.Items.Add(ApplyAlias(UsesManager.ImplementationUses.Items[i].Name));
    if not UsesManager.IsPositionBeforeImplementation(GxOtaGetCurrentEditBufferPos) then
      pcUses.ActivePage := tabImplementation
    else
      pcUses.ActivePage := tabInterface;
  finally
    FreeAndNil(UsesManager);
  end;
end;

procedure TfmUsesManager.lbxImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToImplSection(Source, False);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.lbxInterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToIntfSection(Source);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.lbxUsedDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbxProject) or (Source = lbxCommon) or
    (Source = lbxFavorite) or (Source = lbxSearchPath);
end;

procedure TfmUsesManager.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actUsesDelete.Enabled := HaveSelectedItem(GetUsesSourceListBox);
  actUsesAddToFavorites.Enabled := actUsesDelete.Enabled;
  actIntfMoveToImpl.Enabled := HaveSelectedItem(lbxInterface) and (pcUses.ActivePage = tabInterface);
  actImplMoveToIntf.Enabled := HaveSelectedItem(lbxImplementation)and (pcUses.ActivePage = tabImplementation);
  actUsesOpenUnit.Enabled := HaveSelectedItem(GetUsesSourceListBox);
  actAvailAddToImpl.Enabled := HaveSelectedItem(GetAvailableSourceListBox);
  actAvailAddToIntf.Enabled := actAvailAddToImpl.Enabled;
  actAvailOpenUnit.Enabled := actAvailAddToImpl.Enabled;
  actFavDelete.Enabled := HaveSelectedItem(lbxFavorite);
  actFavDelete.Visible := GetAvailableSourceListBox = lbxFavorite;
  actFavAdd.Visible := actFavDelete.Visible;
  if (ActiveControl = lbxInterface) or (ActiveControl = lbxImplementation) then
    actUsesDelete.ShortCut := VK_DELETE
  else
    actUsesDelete.ShortCut := 0;
end;

function TfmUsesManager.HaveSelectedItem(ListBox: TListBox): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(ListBox));
  Result := False;
  for i := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[i] then
    begin
      Result := True;
      Break;
    end;
end;

procedure TfmUsesManager.DeleteSelected(ListBox: TListBox; FromInterface: Boolean);
var
  i: Integer;
begin
  Assert(Assigned(ListBox));
  for i := ListBox.Items.Count - 1 downto 0 do
  begin
    if ListBox.Selected[i] then
    begin
      if FromInterface then
        DeleteFromIntfSection(ListBox.Items[i])
      else
        DeleteFromImplSection(ListBox.Items[i]);
    end;
  end;
end;

procedure TfmUsesManager.MoveSelected(Src, Dest: TListBox; ToInterface: Boolean);
var
  i: Integer;
  UnitName: string;
begin
  Assert(Assigned(Src) and Assigned(Dest));
  for i := Src.Items.Count - 1 downto 0 do
  begin
    if Src.Selected[i] then
    begin
      UnitName := Src.Items[i];

      if ToInterface then
        AddToIntfSection(UnitName)
      else
        AddToImplSection(UnitName, True);
    end;
  end;
end;

procedure TfmUsesManager.acUsesDeleteExecute(Sender: TObject);
begin
  DeleteSelected(GetUsesSourceListBox, GetUsesSourceListBox = lbxInterface);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actIntfMoveToImplExecute(Sender: TObject);
begin
  MoveSelected(lbxInterface, lbxImplementation, False);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actImplMoveToIntfExecute(Sender: TObject);
begin
  MoveSelected(lbxImplementation, lbxInterface, True);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actFavDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := lbxFavorite.Items.Count - 1 downto 0 do
    if lbxFavorite.Selected[i] then
      DeleteFromFavorites(lbxFavorite.Items[i]);
end;

procedure TfmUsesManager.actFavAddExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
begin
  dlgOpen.InitialDir := ExtractFilePath(GetIdeRootDirectory);
  if dlgOpen.Execute then
  begin
    for i := 0 to dlgOpen.Files.Count - 1 do
    begin
      FileName := ExtractPureFileName(dlgOpen.Files[i]);
      AddToFavorites(FileName);
    end;
  end;
end;

procedure TfmUsesManager.actAvailAddToIntfExecute(Sender: TObject);
var
  Src: TListBox;
  i: Integer;
begin
  Src := GetAvailableSourceListBox;
  for i := Src.Items.Count - 1 downto 0 do
    if Src.Selected[i] then
      AddToIntfSection(Src.Items[i]);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actAvailAddToImplExecute(Sender: TObject);
begin
  AddListToImplSection(GetAvailableSourceListBox, False);
  CloseIfInSingleActionMode;
end;

function TfmUsesManager.GetAvailableSourceListBox: TListBox;
begin
  Result := nil;
  if pcUnits.ActivePage = tabProject then
    Result := lbxProject
  else if pcUnits.ActivePage = tabCommon then
    Result := lbxCommon
  else if pcUnits.ActivePage = tabFavorite then
    Result := lbxFavorite
  else if pcUnits.ActivePage = tabSearchPath then
    Result := lbxSearchPath;
  Assert(Assigned(Result));
end;

procedure TfmUsesManager.lbxInterfaceDblClick(Sender: TObject);
begin
  DeleteItemIndex(lbxInterface, True);
end;

procedure TfmUsesManager.lbxImplementationDblClick(Sender: TObject);
begin
  DeleteItemIndex(lbxImplementation, False);
end;

procedure TfmUsesManager.DeleteItemIndex(ListBox: TListBox; FromInterface: Boolean);
begin
  if ListBox.ItemIndex > -1 then
  begin
    if FromInterface then
      DeleteFromIntfSection(ListBox.Items[ListBox.ItemIndex])
    else
      DeleteFromImplSection(ListBox.Items[ListBox.ItemIndex])
  end;
end;

procedure TfmUsesManager.lbxAvailDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbxInterface) or (Source = lbxImplementation);
end;

procedure TfmUsesManager.lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = lbxInterface) or (Source = lbxImplementation) then
  begin
    if Sender = lbxFavorite then
      AddListToFavorites(Source as TListBox)
    else
      actUsesDelete.Execute;
  end;
end;

procedure TfmUsesManager.actUsesOpenUnitExecute(Sender: TObject);
var
  Src: TListBox;
begin
  Src := GetUsesSourceListBox;
  if Src.ItemIndex > -1 then
    OpenUnit(Src.Items[Src.ItemIndex]);
end;

function TfmUsesManager.GetUsesSourceListBox: TListBox;
begin
  Result := nil;
  if pcUses.ActivePage = tabImplementation then
    Result := lbxImplementation
  else if pcUses.ActivePage = tabInterface then
    Result := lbxInterface;
  Assert(Assigned(Result));
end;

procedure TfmUsesManager.lbxAvailDblClick(Sender: TObject);
var
  Src: TListBox;
begin
  Src := GetAvailableSourceListBox;
  if Src.ItemIndex > -1 then
  begin
    if GetUsesSourceListBox = lbxImplementation then
      AddToImplSection(Src.Items[Src.ItemIndex], False)
    else
      AddToIntfSection(Src.Items[Src.ItemIndex]);
  end;
end;

procedure TfmUsesManager.SearchPathReady;
var
  PathFiles: TStringList;
  PathUnits: TStringList;
  i: Integer;
  FileName: string;
  IsDotNet: Boolean;

  procedure AddPathUnit(const FileName: string);
  begin
    if IsDotNet then
    begin
      if not IsDCU(FileName) then
        PathUnits.Add(ExtractPureFileName(FileName));
    end
    else begin
      if not IsDCUIL(FileName) then
        PathUnits.Add(ExtractPureFileName(FileName));
    end;
  end;

begin
  IsDotNet := GxOtaCurrentProjectIsDotNet;
  PathFiles := nil;
  PathUnits := TStringList.Create;
  try
    PathFiles := TStringList.Create;
    PathUnits.Sorted := True;
    PathUnits.Duplicates := dupIgnore;
    FFindThread.LockResults;
    try
      PathFiles.Assign(FFindThread.Results);
    finally
      FFindThread.ReleaseResults;
    end;
    for i := 0 to PathFiles.Count - 1 do
      AddPathUnit(PathFiles[i]);
    GxOtaGetProjectFileNames(GxOtaGetCurrentProject, PathFiles);
    for i := 0 to PathFiles.Count - 1 do
    begin
      FileName := PathFiles[i];
      if IsPas(FileName) then
        AddPathUnit(FileName);
    end;
    FSearchPathUnits.Assign(PathUnits);
  finally
    FreeAndNil(PathUnits);
    FreeAndNil(PathFiles);
  end;
  FilterVisibleUnits;
  lbxSearchPath.Color := clWindow;
  lbxSearchPath.Enabled := True;
end;

procedure TfmUsesManager.DeleteFromFavorites(const Item: string);
begin
  DeleteStringFromList(FFavoriteUnits, Item);
  DeleteStringFromList(lbxFavorite.Items, Item);
end;

procedure TfmUsesManager.AddToFavorites(const Item: string);
begin
  EnsureStringInList(FFavoriteUnits, Item);
  EnsureStringInList(lbxFavorite.Items, Item);
end;

procedure TfmUsesManager.edtFilterChange(Sender: TObject);
begin
  FilterVisibleUnits;
end;

procedure TfmUsesManager.FilterVisibleUnits;
var
  Filter: string;
begin
  Filter := Trim(edtFilter.Text);
  FilterStringList(FFavoriteUnits, lbxFavorite.Items, Filter, False);
  FilterStringList(FProjectUnits, lbxProject.Items, Filter, False);
  FilterStringList(FCommonUnits, lbxCommon.Items, Filter, False);
  FilterStringList(FSearchPathUnits, lbxSearchPath.Items, Filter, False);
  SelectFirstItemInLists;
end;

procedure TfmUsesManager.edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ListBox: TListBox;
begin
  if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
  begin
    ListBox := GetAvailableSourceListBox;
    if ListBox.Items.Count > 1 then
      ListBox.Perform(WM_KEYDOWN, Key, 0)
    else if ListBox.Items.Count = 1 then
      ListBox.Selected[0] := True;
    Key := 0;
  end;
end;

procedure TfmUsesManager.SelectFirstItemInLists;

  procedure SelectBestItem(ListBox: TListBox);
  var
    Filter: string;
    MatchIndex: Integer;
  begin
    if ListBox.Items.Count > 0 then
    begin
      Filter := Trim(edtFilter.Text);
      MatchIndex := ListBox.Items.IndexOf(Filter);
      if MatchIndex = -1 then
        MatchIndex := 0;

      ListBox.Selected[MatchIndex] := True;
    end;
  end;


begin
  SelectBestItem(lbxCommon);
  SelectBestItem(lbxFavorite);
  SelectBestItem(lbxSearchPath);
  SelectBestItem(lbxProject);
end;

procedure TfmUsesManager.btnOKClick(Sender: TObject);
var
  ListBox: TListBox;
begin
  if chkSingleActionMode.Checked then
  begin
    ListBox := GetAvailableSourceListBox;
    if pcUses.ActivePage = tabInterface then
      AddListToIntfSection(ListBox)
    else
      AddListToImplSection(ListBox, False);
  end;
  SaveChanges;
end;

procedure TfmUsesManager.SaveChanges;
var
  i: Integer;
  UnitName: string;
  Units: TStringList;
begin
  FFavoriteUnits.Assign(lbxFavorite.Items);

  Units := TStringList.Create;
  try
    GetInterfaceUnits(Units);
    for i := 0 to Units.Count - 1 do
    begin
      UnitName := Units[i];
      if lbxInterface.Items.IndexOf(UnitName) = -1 then
        RemoveUnitFromInterface(UnitName);
    end;

    GetImplementationUnits(Units);
    for i := 0 to Units.Count  - 1 do
    begin
      UnitName := Units[i];
      if lbxImplementation.Items.IndexOf(UnitName) = -1 then
        RemoveUnitFromImplementation(UnitName);
    end;
  finally
    FreeAndNil(Units);
  end;

  for i := 0 to lbxInterface.Items.Count - 1 do
  begin
    UnitName := lbxInterface.Items[i];
    case GetUsesStatus(UnitName) of
      usNonExisting, usImplementation:
        UseUnitInInterface(UnitName);
    end;
  end;

  for i := 0 to lbxImplementation.Items.Count - 1 do
  begin
    UnitName := lbxImplementation.Items[i];
    case GetUsesStatus(UnitName) of
      usNonExisting, usInterface:
        UseUnitInImplementation(UnitName);
    end;
  end;
end;

procedure TfmUsesManager.CloseIfInSingleActionMode;
begin
  if chkSingleActionMode.Checked then
  begin
    SaveChanges;
    ModalResult := mrOk;
  end;
end;

procedure TfmUsesManager.FormShow(Sender: TObject);
begin
  FilterVisibleUnits;
end;

procedure TfmUsesManager.actAvailOpenUnitExecute(Sender: TObject);
var
  Src: TListBox;
begin
  Src := GetAvailableSourceListBox;
  if Src.ItemIndex > -1 then
    OpenUnit(Src.Items[Src.ItemIndex]);
end;

procedure TfmUsesManager.actUsesAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(GetUsesSourceListBox);
end;

procedure TfmUsesManager.AddListToFavorites(ListBox: TListBox);
var
  i: Integer;
begin
  Assert(Assigned(ListBox));
  for i := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[i] then
      AddToFavorites(ListBox.Items[i]);
end;

initialization
  RegisterEditorExpert(TUsesExpert);
end.

