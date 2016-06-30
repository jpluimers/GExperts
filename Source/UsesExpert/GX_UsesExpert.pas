unit GX_UsesExpert;

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, Forms, Menus, ComCtrls,
  ExtCtrls, ActnList, Actions, Dialogs, StdCtrls,
  GX_ConfigurationInfo, GX_Experts, GX_GenericUtils, GX_BaseForm,
  GX_KbdShortCutBroker;

type
  TUsesExpert = class(TGX_Expert)
  private
    FFavoriteUnits: TStringList;
    FSingleActionMode: Boolean;
    FAvailTabIndex: Integer;
    FReplaceFileUseUnit: Boolean;
    FOrigFileAddUnitExecute: TNotifyEvent;
    procedure InternalExecute;
    function FindAction(out _Action: TBasicAction): Boolean;
  protected
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
    function GetBitmapFileName: string; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    // Do any delayed setup after the IDE is done initializing
    procedure AfterIDEInitialized; override;
    // Various methods that will be called
    // at appropriate times
    procedure Configure; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
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
    actOpenUnit: TAction;
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
    btnOpen: TButton;
    pnlUsesBottom: TPanel;
    btnAddDots: TButton;
    btnRemoveDots: TButton;
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
    procedure actOpenUnitExecute(Sender: TObject);
    procedure lbxAvailDblClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actUsesAddToFavoritesExecute(Sender: TObject);
    procedure pmuAvailPopup(Sender: TObject);
    procedure btnRemoveDotsClick(Sender: TObject);
    procedure btnAddDotsClick(Sender: TObject);
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
    function GetLbxForOpen: TListBox;
    procedure OpenSelectedUnit(ListBox: TListBox);
    procedure lbxInterfaceFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxImplementationFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxFavoriteFilesDropped(_Sender: TObject; _Files: TStrings);
  protected
    FProjectUnits: TStringList;
    FCommonUnits: TStringList;
    FFavoriteUnits: TStringList;
    FSearchPathUnits: TStringList;
    FUsesExpert: TUsesExpert;
  end;

implementation

uses
  SysUtils, Messages, Windows, Graphics, ToolsAPI,
  GX_OtaUtils, GX_IdeUtils, GX_UsesManager, GX_dzVclUtils,
  GX_UsesExpertOptions, GX_MessageBox;

{$R *.dfm}

{ TUsesExpert }

constructor TUsesExpert.Create;
begin
  inherited;
  FFavoriteUnits := TStringList.Create;
  LoadSettings;
end;

destructor TUsesExpert.Destroy;
var
  act: TBasicAction;
begin
  SaveSettings;

  if Assigned(FOrigFileAddUnitExecute) then
    if FindAction(act) then
      act.OnExecute := FOrigFileAddUnitExecute;

  FreeAndNil(FFavoriteUnits);
  inherited;
end;

procedure TUsesExpert.AfterIDEInitialized;
var
  act: TBasicAction;
begin
  inherited;

  if FReplaceFileUseUnit then begin
    if FindAction(act) then begin
      FOrigFileAddUnitExecute := act.OnExecute;
      act.OnExecute := Self.Execute;
    end;
  end;
end;

function TUsesExpert.FindAction(out _Action: TBasicAction): Boolean;
var
  MainMenu: TMainMenu;
  mi: TMenuItem;
begin
  Result := False;
  MainMenu := GxOtaGetIdeMainMenu;
  if not Assigned(MainMenu) then
    Exit;
  if not TMainMenu_FindMenuItem(MainMenu, 'FileUseUnitItem', mi) or not Assigned(mi.Action) then
    Exit;
  if mi.Action.Name = 'FileUseUnitCommand' then begin
    _Action := mi.Action;
    Result := True;
  end;
end;

procedure TUsesExpert.Execute(Sender: TObject);
begin
  InternalExecute;
end;

function TUsesExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('U');
end;

function TUsesExpert.GetActionCaption: string;
resourcestring
  SUsesExpert = '&Uses Clause Manager ...';
begin
  Result := SUsesExpert;
end;

//procedure TUsesExpert.GetHelpString(List: TStrings);
//resourcestring
//  SUsesExpertHelp =
//    '  This expert is designed to help you manage the uses clauses of your Delphi files.  ' +
//    'You can delete and move units between the interface and implementation sections of ' +
//    'the file you are editing using the buttons or drag and drop.  You can also add units from:'#13 +
//    '  - The effective search path (project root, project search paths, IDE library paths)'#13 +
//    '  - The current project'#13 +
//    '  - Delphi''s common VCL/RTL units'#13 +
//    '  - A user-defined favorite units list'#13 +
//    '  The filter control allows case-insensitive filtering of the available unit lists.'#13 +
//    '  There is a "Single action/quick add mode" checkbox that automatically closes ' +
//    'the form when you perform an action on a uses clause or automatically adds the ' +
//    'selected units to the active uses clause when the OK button is selected.';
//begin
//  List.Text := SUsesExpertHelp;
//end;

class function TUsesExpert.GetName: string;
begin
  Result := 'UsesClauseMgr';
end;

function TUsesExpert.GetBitmapFileName: string;
begin
  Result := ClassName;
end;

function TUsesExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TUsesExpert.Configure;
var
  act: TBasicAction;
  Found: boolean;
begin
  Found := FindAction(act);
  if TfmUsesExpertOptions.Execute(Application, Found, FSingleActionMode, FReplaceFileUseUnit) then begin
    SaveSettings;
    if Found then begin
      if FReplaceFileUseUnit then begin
        if not Assigned(FOrigFileAddUnitExecute) then begin
          FOrigFileAddUnitExecute := act.OnExecute;
          act.OnExecute := Execute;
        end;
      end else begin
        act.OnExecute := FOrigFileAddUnitExecute;
        FOrigFileAddUnitExecute := nil;
      end;
    end;
  end;
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
    Form.FUsesExpert := Self;
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

procedure TUsesExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FFavoriteUnits.CommaText := Settings.ReadString('Favorites', '');
  FSingleActionMode := Settings.ReadBool('SingleActionMode', False);
  FReplaceFileUseUnit := Settings.ReadBool('ReplaceFileUseUnit', False);
  FAvailTabIndex := Settings.ReadInteger('AvailTabIndex', 0);
end;

procedure TUsesExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;
  Settings.WriteString('Favorites', FFavoriteUnits.CommaText);
  Settings.WriteBool('SingleActionMode', FSingleActionMode);
  Settings.WriteBool('ReplaceFileUseUnit', FReplaceFileUseUnit);
  Settings.WriteInteger('AvailTabIndex', FAvailTabIndex);
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
    AddSlash('lib') {$IFDEF GX_VER220_up} + AddSlash('Win32') + AddSlash('Release') {$ENDIF} + '*.dcu', $3F, SearchRec);
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

  TWinControl_ActivateDropFiles(lbxInterface, lbxInterfaceFilesDropped);
  TWinControl_ActivateDropFiles(lbxImplementation, lbxImplementationFilesDropped);
  TWinControl_ActivateDropFiles(lbxFavorite, lbxFavoriteFilesDropped);
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

procedure TfmUsesManager.lbxInterfaceFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToIntfSection(ChangeFileExt(ExtractFileName(s), ''));
  end;
end;

procedure TfmUsesManager.lbxImplementationFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToImplSection(ChangeFileExt(ExtractFileName(s), ''), True);
  end;
end;


procedure TfmUsesManager.lbxFavoriteFilesDropped(_Sender: TObject; _Files: TStrings);
var
  i: Integer;
  s: string;
begin
  for i := 0 to _Files.Count - 1 do begin
    s := _Files[i];
    AddToFavorites(ChangeFileExt(ExtractFileName(s), ''));
  end;
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

procedure TfmUsesManager.pmuAvailPopup(Sender: TObject);
begin
  inherited;
  if pcUses.ActivePage= tabInterface then
    mitAvailAddToIntf.Default := True
  else
    mitAvailAddToImpl.Default := True;
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
var
  AvailableSourceListBox: TListBox;
  ActiveLBHasSelection: Boolean;
  UsesSourceListBox: TListBox;
  UsesIsInterface: Boolean;
begin
  AvailableSourceListBox := GetAvailableSourceListBox;
  UsesSourceListBox := GetUsesSourceListBox;
  ActiveLBHasSelection := HaveSelectedItem(AvailableSourceListBox);
  UsesIsInterface := (pcUses.ActivePage = tabInterface);

  actUsesDelete.Enabled := HaveSelectedItem(UsesSourceListBox);
  actUsesAddToFavorites.Enabled := actUsesDelete.Enabled;
  actIntfMoveToImpl.Enabled := HaveSelectedItem(lbxInterface) and UsesIsInterface;
  actImplMoveToIntf.Enabled := HaveSelectedItem(lbxImplementation) and not UsesIsInterface;

  actAvailAddToImpl.Enabled := ActiveLBHasSelection;
  actAvailAddToIntf.Enabled := ActiveLBHasSelection;
  actFavAdd.Enabled := ActiveLBHasSelection or (pcUnits.ActivePage = tabFavorite);
  actFavDelete.Enabled := HaveSelectedItem(lbxFavorite);
  actFavDelete.Visible := AvailableSourceListBox = lbxFavorite;
  if (ActiveControl = lbxInterface) or (ActiveControl = lbxImplementation) then
    actUsesDelete.ShortCut := VK_DELETE
  else
    actUsesDelete.ShortCut := 0;

  actOpenUnit.Enabled := HaveSelectedItem(GetLbxForOpen);
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
  Src: TListBox;
begin
  if pcUnits.ActivePage = tabFavorite then
  begin
    dlgOpen.InitialDir := ExtractFilePath(GetIdeRootDirectory);
    if dlgOpen.Execute then
    begin
      for i := 0 to dlgOpen.Files.Count - 1 do
      begin
        FileName := ExtractPureFileName(dlgOpen.Files[i]);
        AddToFavorites(FileName);
      end;
    end
  end
  else begin
    FileName := '';
    Src := GetAvailableSourceListBox;
    for i := Src.Items.Count - 1 downto 0 do
    begin
      if Src.Selected[i] then
      begin
        FileName := Src.Items[i];
        AddToFavorites(FileName);
      end;
    end;
    edtFilter.Text := '';
    pcUnits.ActivePage := tabFavorite;
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
  if IsCtrlDown then
    OpenSelectedUnit(lbxInterface)
  else
    DeleteItemIndex(lbxInterface, True);
end;

procedure TfmUsesManager.lbxImplementationDblClick(Sender: TObject);
begin
  if IsCtrlDown then
    OpenSelectedUnit(lbxImplementation)
  else
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

function TfmUsesManager.GetLbxForOpen: TListBox;
begin
  if ActiveControl = lbxImplementation then
    Result := lbxImplementation
  else if ActiveControl = lbxInterface then
    Result := lbxInterface
  else
    Result := GetAvailableSourceListBox;
end;

procedure TfmUsesManager.OpenSelectedUnit(ListBox: TListBox);
var
  UnitName: string;
begin
  if TListBox_GetSelected(ListBox, UnitName) then begin
    OpenUnit(UnitName);
    ModalResult := mrCancel;
  end;
end;

procedure TfmUsesManager.actOpenUnitExecute(Sender: TObject);
begin
  OpenSelectedUnit(GetLbxForOpen);
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
  UnitName: string;
begin
  Src := GetAvailableSourceListBox;
  if TListBox_GetSelected(Src, UnitName) then begin
    if IsCtrlDown then begin
      OpenUnit(UnitName);
      ModalResult := mrCancel;
    end else begin
      if GetUsesSourceListBox = lbxImplementation then
        AddToImplSection(UnitName, False)
      else
        AddToIntfSection(UnitName);
    end;
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
  i: Integer;
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    i := pcUnits.ActivePageIndex + 1;
    if i = pcUnits.PageCount then i := 0;
      pcUnits.ActivePageIndex := i;
  end
  else begin
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

type
  TShowAddDotsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;


{ TShowAddDotsMessage }

function TShowAddDotsMessage.GetMessage: string;
resourcestring
  SConfirmRemoveDots =
    'This will try to add namespace qualifiers to all unit names ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"Registry" will be changed to "System.Win.Registry".' + sLineBreak
    + sLineBreak
    + 'This is meant to update projects to newer Delphi versions.' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmRemoveDots;
end;

procedure TfmUsesManager.btnAddDotsClick(Sender: TObject);
var
  DefaultNamespace: string;
  NameSpaces: TStringList;

  procedure AddDotsTo(lb: TListBox);
  var
    UnitIdx: Integer;
    UnitName: string;
    p: Integer;
    s: string;
    NsIdx: Integer;
  begin
    for UnitIdx := 0 to lb.Count - 1 do begin
      UnitName := lb.Items[UnitIdx];
      p := Pos('.', UnitName);
      if p = 0 then begin
        for NsIdx := 0 to NameSpaces.Count - 1 do begin
          s := NameSpaces[NsIdx] + '.' + UnitName;
          if FSearchPathUnits.IndexOf(s) <> -1 then begin
            UnitName := s;
            break;
          end;
        end;
      end;
      lb.Items[UnitIdx] := UnitName;
    end;
  end;

var
  CurrentUnitName: string;
  p: Integer;
begin
  if ShowGxMessageBox(TShowAddDotsMessage) <> mrYes then
    Exit;

  NameSpaces := TStringList.Create;
  try
    GxOtaGetProjectNamespaces(DefaultNamespace, NameSpaces);
    CurrentUnitName := GxOtaGetCurrentSourceFile;
    if CurrentUnitName <> '' then begin
      // remove .pas
      CurrentUnitName := ChangeFileExt(ExtractFilename(CurrentUnitName), '');
      p := Pos('.', CurrentUnitName);
      if p > 0 then begin
        CurrentUnitName := Copy(CurrentUnitName, 1, p - 1);
        NameSpaces.Insert(0, CurrentUnitName);
      end;
      AddDotsTo(lbxInterface);
      AddDotsTo(lbxImplementation);
    end;
  finally
    FreeAndNil(NameSpaces);
  end;
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

type
  TShowRemoveDotsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowRemoveDotsMessage }

function TShowRemoveDotsMessage.GetMessage: string;
resourcestring
  SConfirmRemoveDots =
    'This will remove the namespace qualifiers from all unit names ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"System.Win.Registry" will be shortended to "Registry".' + sLineBreak
    + sLineBreak
    + 'This is meant to keep backwards compatibility with older Delphi versions.' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmRemoveDots;
end;

procedure TfmUsesManager.btnRemoveDotsClick(Sender: TObject);

  procedure RemoveDotsfrom(lb: TListBox);
  var
    i: Integer;
    s: string;
    p: Integer;
  begin
    for i := 0 to lb.Count - 1 do begin
      s := lb.Items[i];
      p := Pos('.', s);
      while p > 0 do begin
        s := Copy(s, p + 1, 255);
        p := Pos('.', s);
      end;
      lb.Items[i] := s;
    end;
  end;

begin
  if ShowGxMessageBox(TShowRemoveDotsMessage) <> mrYes then
    Exit;

  RemoveDotsfrom(lbxInterface);
  RemoveDotsfrom(lbxImplementation);
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
  RegisterGX_Expert(TUsesExpert);
end.

