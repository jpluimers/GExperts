unit GX_UsesExpert;

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, Forms, Menus, ComCtrls,
  ExtCtrls, ActnList, Actions, Dialogs, StdCtrls, Grids,
  GX_ConfigurationInfo, GX_Experts, GX_GenericUtils, GX_BaseForm,
  GX_KbdShortCutBroker, GX_UnitExportsParser;

type
  TUsesExpert = class(TGX_Expert)
  private
    FSingleActionMode: Boolean;
    FAvailTabIndex: Integer;
    FReplaceFileUseUnit: Boolean;
    FOrigFileAddUnitExecute: TNotifyEvent;
    FReadMap: Boolean;
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
    mitUsesDelete: TMenuItem;
    mitUsesMove: TMenuItem;
    pmuAvail: TPopupMenu;
    mitAvailAddToUses: TMenuItem;
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
    mitUsesSep1: TMenuItem;
    mitUsesSep2: TMenuItem;
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
    actUsesMove: TAction;
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
    actUsesUnAlias: TAction;
    mitUsesUnalias: TMenuItem;
    tabIdentifiers: TTabSheet;
    sgIdentifiers: TStringGrid;
    pnlIdentifiersFooter: TPanel;
    btnIdentifiersAddToIntf: TButton;
    btnIdentifiersAddToImpl: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbxImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxInterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxUsedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acUsesDeleteExecute(Sender: TObject);
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
    procedure btnRemoveDotsClick(Sender: TObject);
    procedure btnAddDotsClick(Sender: TObject);
    procedure actUsesUnAliasExecute(Sender: TObject);
  private
    FAliases: TStringList;
    FFindThread: TFileFindThread;
    FUnitExportParserThread: TUnitExportParserThread;
    FCurrentIdentIdx: Integer;
    // maintains a list unit name mappings from "originally used" to "currently used"
    // this is necessary to put units which have been switched between using prefixes and
    // not in the correct place of the unit list.
    FOldToNewUnitNameMap: TStringList;
    procedure GetCommonFiles;
    procedure GetProjectFiles;
    function TryGetMapFiles: Boolean;
    procedure AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean);
    procedure AddToIntfSection(const UnitName: string);
    procedure DeleteFromIntfSection(const UnitName: string);
    procedure DeleteFromImplSection(const UnitName: string);
    procedure OpenUnit(const UnitName: string);
    procedure ReadUsesList;
    function ApplyAlias(const UnitName: string): string;
    procedure UnAlias(AStrings: TStrings);
    procedure AddListToIntfSection(ListBox: TObject);
    procedure AddListToImplSection(ListBox: TObject; RemoveFromInterface: Boolean);
    procedure AddListToFavorites(ListBox: TListBox);
    function HaveSelectedItem(ListBox: TListBox): Boolean;
    procedure DeleteItemIndex(ListBox: TListBox; FromInterface: Boolean);
    procedure DeleteSelected(ListBox: TListBox; FromInterface: Boolean);
    procedure MoveSelected(Src, Dest: TListBox; ToInterface: Boolean);
    function TryGetAvailableSourceListBox(out lbx: TListBox): Boolean;
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
    procedure actUsesMoveToImplExecute(Sender: TObject);
    procedure actUsesMoveToIntExecute(Sender: TObject);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OnExportParserFinished(_Sender: TObject);
  protected
    FProjectUnits: TStringList;
    FCommonUnits: TStringList;
    FFavoriteUnits: TStringList;
    FSearchPathUnits: TStringList;
    FFavUnitsExports: TStringList;
    FUsesExpert: TUsesExpert;
  public
    constructor Create(_Owner: TComponent; _UsesExpert: TUsesExpert); reintroduce;
  end;

implementation

uses
  SysUtils, Messages, Windows, Graphics, ToolsAPI,
  GX_OtaUtils, GX_IdeUtils, GX_UsesManager, GX_dzVclUtils, GX_dzMapFileReader, GX_dzFileUtils,
  GX_UsesExpertOptions, GX_MessageBox, StrUtils, Math;

{$R *.dfm}

{ TUsesExpert }

constructor TUsesExpert.Create;
begin
  inherited;
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
  if TfmUsesExpertOptions.Execute(Application, Found, FReadMap, FSingleActionMode, FReplaceFileUseUnit) then begin
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
begin
  AssertIsPasOrInc(GxOtaGetCurrentSourceFile);
  Form := TfmUsesManager.Create(Application, Self);
  try
    Form.LoadSettings;
    Form.chkSingleActionMode.Checked := FSingleActionMode;
    if (FAvailTabIndex >= 0) and (FAvailTabIndex < Form.pcUnits.PageCount) then
      Form.pcUnits.ActivePageIndex := FAvailTabIndex;

    if Form.ShowModal = mrOk then
    begin
      Form.SaveSettings;
      FSingleActionMode := Form.chkSingleActionMode.Checked;
      FAvailTabIndex := Form.pcUnits.ActivePageIndex;

      IncCallCount;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TUsesExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FSingleActionMode := Settings.ReadBool('SingleActionMode', False);
  FReplaceFileUseUnit := Settings.ReadBool('ReplaceFileUseUnit', False);
  FReadMap := Settings.ReadBool('ReadMap', True);
  FAvailTabIndex := Settings.ReadInteger('AvailTabIndex', 0);
end;

procedure TUsesExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;
  Settings.WriteBool('SingleActionMode', FSingleActionMode);
  Settings.WriteBool('ReplaceFileUseUnit', FReplaceFileUseUnit);
  Settings.WriteBool('ReadMap', FReadMap);
  Settings.WriteInteger('AvailTabIndex', FAvailTabIndex);
end;

{ TfmUsesManager }

constructor TfmUsesManager.Create(_Owner: TComponent; _UsesExpert: TUsesExpert);
var
  Bitmap: TBitmap;
begin
  FUsesExpert := _UsesExpert;
  inherited Create(_Owner);

  Bitmap := FUsesExpert.GetBitmap;
  if Assigned(Bitmap) then
    ConvertBitmapToIcon(Bitmap, Icon);

  sgIdentifiers.Cells[0, 0] := 'Identifier';
  sgIdentifiers.Cells[1, 0] := 'Unit';
end;

procedure TfmUsesManager.GetProjectFiles;
var
  IProject: IOTAProject;
  IModuleInfo: IOTAModuleInfo;
  i: Integer;
  FileName: string;
begin
  FProjectUnits.Clear;
  if not FUsesExpert.FReadMap or not TryGetMapFiles then begin
    IProject := GxOtaGetCurrentProject;
    if not Assigned(IProject) then
      Exit;
    for i := 0 to IProject.GetModuleCount - 1 do begin
      IModuleInfo := IProject.GetModule(i);
      Assert(IModuleInfo <> nil);

      FileName := IModuleInfo.FileName;
      // We don't want blank names, packages, etc.
      if IsPas(FileName) then
        FProjectUnits.Add(ExtractPureFileName(FileName));
    end;
  end;
end;

function TfmUsesManager.TryGetMapFiles: boolean;
var
  Project: IOTAProject;
  ProjectFilename: string;
  Reader: TMapFileReader;
  OutputDir: string;
  MapFile: string;
begin
  Result := False;
  Project := GxOtaGetCurrentProject;
  if not Assigned(Project) then
    Exit;
  OutputDir := GxOtaGetProjectOutputDir(Project);
  ProjectFilename := GxOtaGetProjectFileName(Project);
  MapFile := AddSlash(OutputDir) + ExtractFilename(ProjectFilename);
  MapFile := ChangeFileExt(MapFile, '.map');
  MapFile := TFileSystem.ExpandFileNameRelBaseDir(MapFile, ExtractFileDir(ProjectFilename));
  if not FileExists(MapFile) then
    Exit;
  Reader := TMapFileReader.Create(MapFile);
  try
    FProjectUnits.Assign(Reader.Units);
    Result := (FProjectUnits.Count > 0)
  finally
    FreeAndNil(Reader);
  end;
end;

procedure TfmUsesManager.GetCommonFiles;
var
  Found: Integer;
  SearchRec: TSearchRec;
begin
  // Read all dcu files from the $(DELPHI)\lib directory (for XE+ use the Win32\Release subdir)
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
  FFavUnitsExports := TStringList.Create;
  FAliases := TStringList.Create;
  FOldToNewUnitNameMap := TStringList.Create;
  FCurrentIdentIdx := -1;

  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.pas');
  FFindThread.FileMasks.Add('*.dcu');
  GxOtaGetEffectiveLibraryPath(FFindThread.SearchDirs);
  FFindThread.OnFindComplete := SearchPathReady;
  FFindThread.StartFind;

  pcUnits.ActivePage := tabSearchPath;
  pcUses.ActivePage := tabInterface;
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
  if Assigned(FFindThread) then begin
    FFindThread.OnFindComplete := nil;
    FFindThread.Terminate;
  end;
  if Assigned(FUnitExportParserThread) then begin
    FUnitExportParserThread.OnTerminate := nil;
    FUnitExportParserThread.Terminate;
  end;
  FreeAndNil(FOldToNewUnitNameMap);
  FreeAndNil(FAliases);
  FreeAndNil(FFindThread);
  FreeAndNil(FUnitExportParserThread);
  FreeAndNil(FProjectUnits);
  FreeAndNil(FCommonUnits);
  FreeAndNil(FFavoriteUnits);
  FreeAndNil(FSearchPathUnits);
  FreeAndNil(FFavUnitsExports);
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

const
  ALIAS_PREFIX = ' (-> ';

function TfmUsesManager.ApplyAlias(const UnitName: string): string;
var
  i: Integer;
begin
  Result := UnitName;
  i := FAliases.IndexOfName(Result);
  if i <> -1 then
    Result := UnitName + ALIAS_PREFIX + FAliases.Values[Result] + ')';
end;

procedure TfmUsesManager.UnAlias(AStrings: TStrings);
var
  i: Integer;
  s: string;
  p: Integer;
begin
  for i := AStrings.Count - 1 downto 0 do begin
    s := AStrings[i];
    p := Pos(' ', s);
    if p > 0 then begin
      s := Copy(s, 1, p - 1);
      AStrings[i] := s;
    end;
  end;
end;

procedure TfmUsesManager.ReadUsesList;
var
  i: Integer;
  UsesManager: TUsesManager;
  Ident: string;
  IdentOffset: Integer;
  StartPos: TOTAEditPos;
  CurrentPos: TOTAEditPos;
  AfterLen: Integer;
  lbx: TListBox;
  tab: TTabSheet;
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
      tab := tabImplementation
    else
      tab := tabInterface;

    GxOtaGetCurrentIdentEx(Ident, IdentOffset, StartPos, CurrentPos, AfterLen);
    if Ident <> '' then begin
      lbx := nil;
      case UsesManager.isPositionInUsesList(IdentOffset) of
        puInterface: begin
            if UsesManager.GetUsesStatus(Ident) = usInterface then begin
              tab := tabInterface;
              lbx := lbxInterface;
            end;
          end;
        puImplementation: begin
            if UsesManager.GetUsesStatus(Ident) = usImplementation then begin
              tab := tabImplementation;
              lbx := lbxImplementation;
            end;
          end;
      end;
      pcUses.ActivePage := tab;
      if Assigned(lbx) then 
        FCurrentIdentIdx := lbx.Items.IndexOf(Ident);
    end;
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
resourcestring
  SUsesMoveToInterface = '&Move to Interface';
  SUsesMoveToImplementation = '&Move to Implementation';
var
  AvailableSourceListBox: TListBox;
  ActiveLBHasSelection: Boolean;
  UsesSourceListBox: TListBox;
  UsesIsInterface: Boolean;
begin
  // todo: This no longer works correctly: If the Identifiers grid is active
  //       TryGetAvailableSourceListBox returns false and the actions don't
  //       get enabled / disabled as they should, e.g. the Delete action stays
  //       disabled even when switching from an empty to a non empty uses list.
  if not TryGetAvailableSourceListBox(AvailableSourceListBox) then
    Exit; //==>
  UsesSourceListBox := GetUsesSourceListBox;
  ActiveLBHasSelection := HaveSelectedItem(AvailableSourceListBox);
  UsesIsInterface := (pcUses.ActivePage = tabInterface);

  actUsesDelete.Enabled := HaveSelectedItem(UsesSourceListBox);

  actUsesAddToFavorites.Enabled := actUsesDelete.Enabled;
  if UsesIsInterface then begin
      actUsesMove.Caption := SUsesMoveToImplementation;
      actUsesMove.OnExecute := actUsesMoveToImplExecute;
      actUsesMove.Enabled := HaveSelectedItem(lbxInterface);
      mitAvailAddToUses.Action := actAvailAddToIntf;
  end else begin
      actUsesMove.Caption := SUsesMoveToInterface;
      actUsesMove.OnExecute := actUsesMoveToIntExecute;
      actUsesMove.Enabled := HaveSelectedItem(lbxImplementation);
      mitAvailAddToUses.Action := actAvailAddToImpl;
  end;

  actAvailAddToImpl.Enabled := ActiveLBHasSelection;
  actAvailAddToIntf.Enabled := ActiveLBHasSelection;
  actFavAdd.Enabled := ActiveLBHasSelection or (pcUnits.ActivePage = tabFavorite);
  actFavDelete.Enabled := HaveSelectedItem(lbxFavorite);
  actFavDelete.Visible := AvailableSourceListBox = lbxFavorite;

  if (ActiveControl = lbxInterface) or (ActiveControl = lbxImplementation) then
    actUsesDelete.ShortCut := VK_DELETE
  else
    actUsesDelete.ShortCut := 0;

  if ActiveControl = lbxFavorite then
    actFavDelete.ShortCut := VK_DELETE
  else
    actFavDelete.ShortCut := 0;

  actOpenUnit.Enabled := HaveSelectedItem(GetLbxForOpen);
end;

function TfmUsesManager.HaveSelectedItem(ListBox: TListBox): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(ListBox) then
    Exit; //==>

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

procedure TfmUsesManager.actUsesMoveToImplExecute(Sender: TObject);
begin
  MoveSelected(lbxInterface, lbxImplementation, False);
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actUsesMoveToIntExecute(Sender: TObject);
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
    if not TryGetAvailableSourceListBox(src) then
      Exit; //==>
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
  if TryGetAvailableSourceListBox(src) then begin
    for i := Src.Items.Count - 1 downto 0 do
      if Src.Selected[i] then
        AddToIntfSection(Src.Items[i]);
  end else begin
    // no listbox, so it must be the Identifiers StringGrid
    i := sgIdentifiers.Row;
    if (i < sgIdentifiers.FixedRows) or (i >= sgIdentifiers.RowCount) then
      Exit; //==>
    AddToIntfSection(sgIdentifiers.Cells[1, i]);
  end;
  CloseIfInSingleActionMode;
end;

procedure TfmUsesManager.actAvailAddToImplExecute(Sender: TObject);
var
  i: Integer;
  Src: TListBox;
begin
  if TryGetAvailableSourceListBox(Src) then begin
    AddListToImplSection(Src, False);
  end else begin
    // no listbox, so it must be the Identifiers StringGrid
    i := sgIdentifiers.Row;
    if (i < sgIdentifiers.FixedRows) or (i >= sgIdentifiers.RowCount) then
      Exit; //==>
    AddToImplSection(sgIdentifiers.Cells[1, i], False);
  end;
  CloseIfInSingleActionMode;
end;

function TfmUsesManager.TryGetAvailableSourceListBox(out lbx: TListBox): Boolean;
begin
  lbx := nil;
  if pcUnits.ActivePage = tabProject then
    lbx := lbxProject
  else if pcUnits.ActivePage = tabCommon then
    lbx := lbxCommon
  else if pcUnits.ActivePage = tabFavorite then
    lbx := lbxFavorite
  else if pcUnits.ActivePage = tabSearchPath then
    lbx := lbxSearchPath;
  Result := Assigned(lbx);
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
  else begin
    if not TryGetAvailableSourceListBox(Result) then begin
      Result := nil;
    end;
  end;
end;

procedure TfmUsesManager.OpenSelectedUnit(ListBox: TListBox);
var
  UnitName: string;
  r: Integer;
begin
  if not Assigned(ListBox) then begin
    // no listbox -> identifier string grid
    r := sgIdentifiers.Row;
    if (r < sgIdentifiers.FixedRows) or (r >= sgIdentifiers.RowCount) then
      Exit; //==>
    UnitName := sgIdentifiers.Cells[1, r];
  end else begin
    if not TListBox_GetSelected(ListBox, UnitName) then
      Exit; //==>
  end;
  OpenUnit(UnitName);
  ModalResult := mrCancel;
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
  if not TryGetAvailableSourceListBox(Src) then
    Exit; //==>
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
  if not Assigned(FFindThread) then begin
    // If it is not assigned, something went wrong in the form's constructor
    // which freed the thread but a synchronise call was still waiting to be executed.
    Exit; //==>
  end;
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
  i: Integer;
  Identifier: string;
  UnitName: string;
  Filter: string;
  FixedRows: Integer;
  cnt: Integer;
begin
  Filter := Trim(edtFilter.Text);
  FilterStringList(FFavoriteUnits, lbxFavorite.Items, Filter, False);
  FilterStringList(FProjectUnits, lbxProject.Items, Filter, False);
  FilterStringList(FCommonUnits, lbxCommon.Items, Filter, False);
  FilterStringList(FSearchPathUnits, lbxSearchPath.Items, Filter, False);
  SelectFirstItemInLists;

  FixedRows := sgIdentifiers.FixedRows;
  sgIdentifiers.RowCount := FixedRows + Max(1, FFavUnitsExports.Count);
  sgIdentifiers.Cells[0, FixedRows] := '';
  sgIdentifiers.Cells[1, FixedRows] := '';
  cnt := 0;
  for i := 0 to FFavUnitsExports.Count - 1 do begin
    Identifier := FFavUnitsExports[i];
    if (Filter = '') or StrBeginsWith(Filter, Identifier, False) then begin
      sgIdentifiers.Cells[0, FixedRows + cnt] := Identifier;
      UnitName := PChar(FFavUnitsExports.Objects[i]);
      sgIdentifiers.Cells[1, FixedRows + cnt] := UnitName;
      Inc(cnt);
    end;
  end;
  sgIdentifiers.RowCount := FixedRows + Max(1, cnt);
  TGrid_Resize(sgIdentifiers, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmUsesManager.edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ListBox: TListBox;
  i: Integer;
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    i := pcUnits.ActivePageIndex + 1;
    if i = pcUnits.PageCount then
      i := 0;
    pcUnits.ActivePageIndex := i;
  end else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then begin
    i := pcUnits.ActivePageIndex - 1;
    if i < 0 then
      i := pcUnits.PageCount - 1;
    pcUnits.ActivePageIndex := i;
  end else begin
    if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
    begin
      if TryGetAvailableSourceListBox(ListBox) then begin
        if ListBox.Items.Count > 1 then
          ListBox.Perform(WM_KEYDOWN, Key, 0)
        else if ListBox.Items.Count = 1 then
          ListBox.Selected[0] := True;
        Key := 0;
      end else begin
        // no listbox, so ist must be the identifiers StringGrid
        sgIdentifiers.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
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

procedure TfmUsesManager.LoadSettings;
var
  Settings: TGExpertsSettings;
  Paths: TStringList;
  sl: TStringList;
  i: Integer;
  fn: string;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    FFavoriteUnits.Sorted := False;
    FFavoriteUnits.CommaText := Settings.ReadString(TUsesExpert.ConfigurationKey, 'Favorites', '');
  finally
    FreeAndNil(Settings);
  end;
  FFavoriteUnits.Sorted := True;

  Paths := nil;
  sl := TStringList.Create;
  try
    Paths := TStringList.Create;
    GxOtaGetAllPossiblePaths(Paths);
    for i := 0 to FFavoriteUnits.Count - 1 do begin
      if GxOtaTryFindPathToFile(FFavoriteUnits[i] + '.pas', fn, Paths) then
        sl.Add(fn);
    end;
    if sl.Count > 0 then
      FUnitExportParserThread := TUnitExportParserThread.Create(sl, OnExportParserFinished);
  finally
    FreeAndNil(Paths);
    FreeAndNil(sl);
  end;
end;

procedure TfmUsesManager.OnExportParserFinished(_Sender: TObject);
var
  IdentIdx: Integer;
  cnt: Integer;
  FixedRows: Integer;
  UnitName: string;
  Identifier: string;
  sl: TStrings;
  Idx: Integer;
begin
  if not Assigned(FUnitExportParserThread) then
    Exit; //==>

  FixedRows := sgIdentifiers.FixedRows;
  cnt := 0;
  sl := FUnitExportParserThread.Identifiers;
  sgIdentifiers.RowCount := FixedRows + max(1, sl.Count);
  sgIdentifiers.Cells[0, FixedRows] := '';
  sgIdentifiers.Cells[1, FixedRows] := '';
  for IdentIdx := 0 to sl.Count - 1 do begin
    Identifier := sl[IdentIdx];
    UniqueString(Identifier);
    UnitName := PChar(sl.Objects[IdentIdx]);
      // make sure the string is valid and not freed in the thread
    if FFavoriteUnits.Find(UnitName, Idx) then begin
      UnitName := FFavoriteUnits[Idx];
      FFavUnitsExports.AddObject(Identifier, Pointer(PChar(UnitName)));
      sgIdentifiers.Cells[0, FixedRows + cnt] := Identifier;
      sgIdentifiers.Cells[1, FixedRows + cnt] := UnitName;
      Inc(cnt);
    end;
  end;
  sgIdentifiers.RowCount := FixedRows + Max(1, cnt);
  TGrid_Resize(sgIdentifiers, [roUseGridWidth, roUseAllRows]);
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
    OrigUnitName: string;
    NewUnitName: string;
    s: string;
    NsIdx: Integer;
    SearchPathUnitsIndex: Integer;
  begin
    for UnitIdx := 0 to lb.Count - 1 do begin
      OrigUnitName := lb.Items[UnitIdx];
      for NsIdx := 0 to NameSpaces.Count - 1 do begin
        s := NameSpaces[NsIdx] + '.' + OrigUnitName;
        SearchPathUnitsIndex := FSearchPathUnits.IndexOf(s);
        if SearchPathUnitsIndex <> -1 then begin
          NewUnitName := FSearchPathUnits[SearchPathUnitsIndex];
          if OrigUnitName <> NewUnitName then begin
            FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
            lb.Items[UnitIdx] := NewUnitName;
          end;
          Break;
        end;
      end;
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
    if not TryGetAvailableSourceListBox(ListBox) then
      Exit; //==>
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
    OrigUnitName: string;
    NewUnitName: string;
    p: Integer;
  begin
    for i := 0 to lb.Count - 1 do begin
      OrigUnitName := lb.Items[i];
      NewUnitName := OrigUnitName;
      p := Pos('.', NewUnitName);
      while p > 0 do begin
        NewUnitName := Copy(NewUnitName, p + 1, 255);
        p := Pos('.', NewUnitName);
      end;
      if NewUnitName <> OrigUnitName then begin
        FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
        lb.Items[i] := NewUnitName;
      end;
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
  OldUnitName: string;
  NewUnitName: string;
  Units: TStringList;
  NewToOldUnitNameMap: TStringList;
begin
  UnAlias(lbxInterface.Items);
  UnAlias(lbxImplementation.Items);

  NewToOldUnitNameMap := nil;
  Units := TStringList.Create;
  try
    GetInterfaceUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if lbxInterface.Items.IndexOf(OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (lbxInterface.Items.IndexOf(NewUnitName) = -1) then
          RemoveUnitFromInterface(OldUnitName);
      end;
    end;

    GetImplementationUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if lbxImplementation.Items.IndexOf(OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (lbxImplementation.Items.IndexOf(NewUnitName) = -1) then
          RemoveUnitFromImplementation(OldUnitName);
      end;
    end;

    NewToOldUnitNameMap := TStringList.Create;
    for i := 0 to FOldToNewUnitNameMap.Count - 1 do begin
      OldUnitName := FOldToNewUnitNameMap.Names[i];
      NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
      NewToOldUnitNameMap.Values[NewUnitName] := OldUnitName;
    end;

    for i := 0 to lbxInterface.Items.Count - 1 do begin
      NewUnitName := lbxInterface.Items[i];
      OldUnitName := NewToOldUnitNameMap.Values[NewUnitName];
      if OldUnitName = NewUnitName then
        OldUnitName := '';
      case GetUsesStatus(NewUnitName) of
        usNonExisting: begin
            if OldUnitName = '' then begin
              UseUnitInInterface(NewUnitName);
            end else begin
              case GetUsesStatus(OldUnitName) of
                usNonExisting: begin
                    UseUnitInInterface(NewUnitName);
                  end;
                usImplementation: begin
                    RemoveUnitFromImplementation(OldUnitName);
                    UseUnitInInterface(NewUnitName);
                  end;
                usInterface: begin
                    ReplaceUnitInInterface(OldUnitName, NewUnitName);
                  end;
              end;
            end;
          end;
        usInterface: begin
            // the new unit name is already in the interface uses
            // there is a slim chance that the old one is also used
            if OldUnitName <> '' then begin
              case GetUsesStatus(OldUnitName) of
                usImplementation:
                  RemoveUnitFromImplementation(OldUnitName);
                usInterface:
                  RemoveUnitFromInterface(OldUnitName);
              end;
            end;
          end;
        usImplementation: begin
            // also removes it from the implementation uses
            UseUnitInInterface(NewUnitName);
            // the new unit name is now in the interface uses
            // there is a slim chance that the old one is also used
            if OldUnitName <> '' then begin
              case GetUsesStatus(OldUnitName) of
                usImplementation:
                  RemoveUnitFromImplementation(OldUnitName);
                usInterface:
                  RemoveUnitFromInterface(OldUnitName);
              end;
            end;
          end;
      end; // case New
    end; // end for interface

    for i := 0 to lbxImplementation.Items.Count - 1 do begin
      NewUnitName := lbxImplementation.Items[i];
      OldUnitName := NewToOldUnitNameMap.Values[NewUnitName];
      if OldUnitName = NewUnitName then
        OldUnitName := '';
      case GetUsesStatus(NewUnitName) of
        usNonExisting: begin
            if OldUnitName = '' then begin
              UseUnitInImplementation(NewUnitName);
            end else begin
              case GetUsesStatus(OldUnitName) of
                usNonExisting: begin
                    UseUnitInImplementation(NewUnitName);
                  end;
                usInterface: begin
                    RemoveUnitFromInterface(OldUnitName);
                    UseUnitInImplementation(NewUnitName);
                  end;
                usImplementation: begin
                    ReplaceUnitInImplementation(OldUnitName, NewUnitName);
                  end;
              end;
            end;
          end;
        usInterface: begin
            // also removes it from the interface uses
            UseUnitInImplementation(NewUnitName);
            // the new unit name is now in the implementation uses
            // there is a slim chance that the old one is also used
            if OldUnitName <> '' then begin
              case GetUsesStatus(OldUnitName) of
                usImplementation:
                  RemoveUnitFromImplementation(OldUnitName);
                usInterface:
                  RemoveUnitFromInterface(OldUnitName);
              end;
            end;
          end;
        usImplementation: begin
            // the new unit name is already in the implementation uses
            // there is a slim chance that the old one is also used
            if OldUnitName <> '' then begin
              case GetUsesStatus(OldUnitName) of
                usImplementation:
                  RemoveUnitFromImplementation(OldUnitName);
                usInterface:
                  RemoveUnitFromInterface(OldUnitName);
              end;
            end;
          end;
      end;
    end;
  finally
    FreeAndNil(NewToOldUnitNameMap);
    FreeAndNil(Units);
  end;
end;

procedure TfmUsesManager.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteString(TUsesExpert.ConfigurationKey, 'Favorites', FFavoriteUnits.CommaText);
  finally
    FreeAndNil(Settings);
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
var
  lbx: TListBox;
begin
  FilterVisibleUnits;
  lbx := GetUsesSourceListBox;
  if FCurrentIdentIdx <> -1 then
    lbx.Selected[FCurrentIdentIdx] := True
  else begin
    lbx.ClearSelection;
    lbx.ItemIndex := -1;
  end;
end;

procedure TfmUsesManager.actUsesAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(GetUsesSourceListBox);
end;

type
  TShowUnaliasMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowUnaliasMessage }

function TShowUnaliasMessage.GetMessage: string;
resourcestring
  SConfirmUnalias =
    'This will replace aliases with the actual unit ' + sLineBreak
    + 'in both uses clauses.' + sLineBreak
    + sLineBreak
    + 'Example:' + sLineBreak
    + '"dbiTypes" will be replaced by "BDE" '  + sLineBreak
    + '(if default aliases are in effect).' + sLineBreak
    + sLineBreak
    + 'Do you want to proceed?';
begin
  Result := SConfirmUnalias;
end;

procedure TfmUsesManager.actUsesUnAliasExecute(Sender: TObject);

  procedure ReplaceByAlias(AStrings: TStrings);
  var
    i: Integer;
    s: string;
    p: Integer;
  begin
    for i := AStrings.Count - 1 downto 0 do begin
      s := AStrings[i];
      p := Pos(ALIAS_PREFIX, s);
      if p > 0 then begin
        p := p + Length(ALIAS_PREFIX);
        s := Copy(s, p, Length(s) - p);
        AStrings[i] := s;
      end;
    end;
  end;

begin
  if ShowGxMessageBox(TShowUnaliasMessage) <> mrYes then
    Exit;

  ReplaceByAlias(lbxInterface.Items);
  ReplaceByAlias(lbxImplementation.Items);
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

