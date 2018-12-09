unit GX_UsesExpert;

interface

{$I 'GX_CondDefine.inc'}

uses
  Classes, Controls, Forms, Menus, ComCtrls,
  ExtCtrls, ActnList, Actions, Dialogs, StdCtrls, Grids, Types,
  GX_ConfigurationInfo, GX_Experts, GX_GenericUtils, GX_BaseForm,
  GX_KbdShortCutBroker, GX_UnitExportsParser, GX_dzCompilerAndRtlVersions;

{$IFOPT D+}
{$IF RTLVersion > RtlVersionDelphiXE}
// System.Diagnostics, which exports TStopWatch, was added to the RTL in DelphiXE2
{$DEFINE DO_TIMING}
{$IFEND}
{$ENDIF}

type
  TPageControl = class(ComCtrls.TPageControl)
  end;

type
  TStringGrid = class(Grids.TStringGrid)
  private
    function GetAssociatedList: TStrings;
    procedure SetAssociatedList(const Value: TStrings);
  published
  public
    property AssociatedList: TStrings read GetAssociatedList write SetAssociatedList;
  end;

type
  TUsesExpert = class(TGX_Expert)
  private
    FAvailTabIndex: Integer;
    FReplaceFileUseUnit: Boolean;
    FParseAll: Boolean;
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
    pm_Intf: TPopupMenu;
    pm_Impl: TPopupMenu;
    m_IntfDelete: TMenuItem;
    m_ImplDelete: TMenuItem;
    m_IntfMove: TMenuItem;
    m_ImplMove: TMenuItem;
    pmuAvail: TPopupMenu;
    mitAvailAddToUses: TMenuItem;
    pnlUses: TPanel;
    pcUnits: TPageControl;
    tabProject: TTabSheet;
    pnlProject: TPanel;
    sg_Project: TStringGrid;
    tabCommon: TTabSheet;
    pnlCommon: TPanel;
    sg_Common: TStringGrid;
    tabFavorite: TTabSheet;
    pnlFavorite: TPanel;
    sg_Favorite: TStringGrid;
    p_Interface: TPanel;
    p_InterfaceTitle: TPanel;
    sg_Interface: TStringGrid;
    p_Implementation: TPanel;
    p_ImplementationTitle: TPanel;
    sg_Implementation: TStringGrid;
    m_IntfOpenUnit: TMenuItem;
    m_ImplOpenUnit: TMenuItem;
    m_IntfSep1: TMenuItem;
    m_IntfSep2: TMenuItem;
    m_ImplSep: TMenuItem;
    m_ImplSep2: TMenuItem;
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
    actImplDelete: TAction;
    actIntfDelete: TAction;
    actIntfMove: TAction;
    actImplMove: TAction;
    actFavDelUnit: TAction;
    actAvailAddToFav: TAction;
    actAvailAddToImpl: TAction;
    actAvailAddToIntf: TAction;
    b_DeleteFromIntf: TButton;
    b_DeleteFromImpl: TButton;
    b_MoveToImpl: TButton;
    b_MoveToIntf: TButton;
    actOpenUnit: TAction;
    tabSearchPath: TTabSheet;
    pnlSearchPathFooter: TPanel;
    btnSearchPathAddToIntf: TButton;
    btnSearchPathAddToImpl: TButton;
    pnlSearchPath: TPanel;
    sg_SearchPath: TStringGrid;
    pnlAvailableHeader: TPanel;
    edtIdentifierFilter: TEdit;
    edtUnitFilter: TEdit;
    lblFilter: TLabel;
    mitAvailAddToFav: TMenuItem;
    mitAvailDelFromFav: TMenuItem;
    mitAvailSep1: TMenuItem;
    mitAvailSep2: TMenuItem;
    mitAvailOpenUnit: TMenuItem;
    actIntfAddToFavorites: TAction;
    actImplAddToFavorites: TAction;
    m_IntfAddToFavorites: TMenuItem;
    m_ImplAddToFavorites: TMenuItem;
    lblUnits: TPanel;
    lblUses: TPanel;
    pnlButtonsRight: TPanel;
    btnCancel: TButton;
    actOK: TAction;
    btnOK: TButton;
    btnOpen: TButton;
    pnlUsesBottom: TPanel;
    btnAddDots: TButton;
    btnRemoveDots: TButton;
    actUnAlias: TAction;
    m_IntfUnalias: TMenuItem;
    m_ImplUnAlias: TMenuItem;
    tabIdentifiers: TTabSheet;
    sg_Identifiers: TStringGrid;
    pnlIdentifiersFooter: TPanel;
    btnIdentifiersAddToIntf: TButton;
    btnIdentifiersAddToImpl: TButton;
    actFocusInterface: TAction;
    actFocusImplementation: TAction;
    tim_Progress: TTimer;
    btnAddSearchPathlToFavorites: TButton;
    btnAddProjectToFavorites: TButton;
    btnAddRtlToFavorites: TButton;
    pm_Favorite: TPopupMenu;
    mi_FavAddToImpl: TMenuItem;
    mi_FavAddtoIntf: TMenuItem;
    actFavAddUnit: TAction;
    N1: TMenuItem;
    mi_FavAddUnit: TMenuItem;
    mi_FavDelUnit: TMenuItem;
    N2: TMenuItem;
    OpenUnit1: TMenuItem;
    mi_AvailAddToIntf: TMenuItem;
    actAvailAddAllToFav: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sg_ImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_InterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_InterfaceDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure sg_ImplementationDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actImplDeleteExecute(Sender: TObject);
    procedure actIntfDeleteExecute(Sender: TObject);
    procedure actFavDelUnitExecute(Sender: TObject);
    procedure actAvailAddToFavExecute(Sender: TObject);
    procedure actAvailAddToIntfExecute(Sender: TObject);
    procedure actAvailAddToImplExecute(Sender: TObject);
    procedure sg_InterfaceDblClick(Sender: TObject);
    procedure lbxAvailDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sg_ImplementationDblClick(Sender: TObject);
    procedure actOpenUnitExecute(Sender: TObject);
    procedure lbxAvailDblClick(Sender: TObject);
    procedure edtIdentifierFilterChange(Sender: TObject);
    procedure edtIdentifierFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtUnitFilterChange(Sender: TObject);
    procedure edtUnitFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actOKExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actIntfAddToFavoritesExecute(Sender: TObject);
    procedure actImplAddToFavoritesExecute(Sender: TObject);
    procedure btnRemoveDotsClick(Sender: TObject);
    procedure btnAddDotsClick(Sender: TObject);
    procedure actIntfUnAliasExecute(Sender: TObject);
    procedure tabIdentifiersResize(Sender: TObject);
    procedure pcUnitsChange(Sender: TObject);
    procedure sg_MouseDownForDragging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pcUnitsResize(Sender: TObject);
    procedure pcUsesResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pnlUsesResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure pnlAvailableHeaderResize(Sender: TObject);
    procedure sg_UsedDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sg_AvailDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure edtIdentifierFilterEnter(Sender: TObject);
    procedure edtIdentifierFilterExit(Sender: TObject);
    procedure edtUnitFilterEnter(Sender: TObject);
    procedure edtUnitFilterExit(Sender: TObject);
    procedure actImplMoveExecute(Sender: TObject);
    procedure actIntfMoveExecute(Sender: TObject);
    procedure pnlUsesBottomResize(Sender: TObject);
    procedure actFocusInterfaceExecute(Sender: TObject);
    procedure actFocusImplementationExecute(Sender: TObject);
    procedure tim_ProgressTimer(Sender: TObject);
    procedure actFavAddUnitExecute(Sender: TObject);
    procedure actAvailAddAllToFavExecute(Sender: TObject);
  private
    FLeftRatio: Double;
    FAliases: TStringList;
    FFindThread: TFileFindThread;
    FUnitExportParserThread: TUnitExportParserThread;
    // maintains a list unit name mappings from "originally used" to "currently used"
    // this is necessary to put units which have been switched between using prefixes and
    // not in the correct place of the unit list.
    FOldToNewUnitNameMap: TStringList;
    procedure GetCommonFiles;
    procedure GetProjectFiles;
    function TryGetMapFiles: Boolean;
    function AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean): Integer;
    function AddToIntfSection(const UnitName: string): Integer;
    procedure DeleteFromIntfSection(const UnitName: string);
    procedure DeleteFromImplSection(const UnitName: string);
    procedure OpenUnit(const UnitName: string);
    procedure ReadUsesList;
    function ApplyAlias(const UnitName: string): string;
    procedure UnAlias(sg: TStringGrid);
    procedure AddListToIntfSection(sg: TObject);
    procedure AddListToImplSection(sg: TObject; RemoveFromInterface: Boolean);
    procedure AddListToFavorites(sg: TStringGrid);
    function HaveSelectedItem(sg: TStringGrid): Boolean;
    procedure DeleteItemIndex(sg: TStringGrid; FromInterface: Boolean);
    procedure DeleteSelected(sg: TStringGrid);
    procedure MoveSelected(Src, Dest: TStringGrid; ToInterface: Boolean);
    function GetAvailableSourceList: TStringGrid;
    procedure SearchPathReady;
    procedure DeleteFromFavorites(const Item: string);
    procedure AddToFavorites(const Item: string);
    procedure FilterVisibleUnits;
    procedure FilterIdentifiers;
    procedure SelectFirstItemInLists;
    procedure SaveChanges;
    function GetListForOpen: TStringGrid;
    procedure OpenSelectedUnit(List: TStringGrid);
    procedure lbxInterfaceFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxImplementationFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure lbxFavoriteFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure LoadFavorites;
    procedure SaveFavorites;
    procedure OnExportParserFinished(_Sender: TObject);
    procedure ResizeIdentiferGrid;
    procedure SwitchUnitsTab(_Direction: Integer);
    function AddToStringGrid(sg: TStringGrid; const UnitName: string): Integer;
    procedure DeleteFromStringGrid(sg: TStringGrid; const UnitName: string);
    function IndexInStringGrid(sg: TStringGrid; const UnitName: string): integer;
    procedure FilterStringGrid(Filter: string; List: TStrings; sg: TStringGrid);
    procedure DrawStringGridCell(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
      _State: TGridDrawState; _Focused: Boolean);
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

{$R *.dfm}

uses
  SysUtils, Messages, Windows, Graphics, StrUtils, Math, ToolsAPI,
  GX_OtaUtils, GX_IdeUtils, GX_UsesManager, GX_dzVclUtils, GX_dzMapFileReader, GX_dzFileUtils,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_UsesExpertOptions, GX_MessageBox, GX_dzOsUtils;


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
  if TfmUsesExpertOptions.Execute(Application, Found, FReadMap, FReplaceFileUseUnit, FParseAll) then begin
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
    if (FAvailTabIndex >= 0) and (FAvailTabIndex < Form.pcUnits.PageCount) then begin
      Form.pcUnits.ActivePageIndex := FAvailTabIndex;
      Form.pcUnits.Change;
    end;

    if Form.ShowModal = mrOk then
    begin
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
  FReplaceFileUseUnit := Settings.ReadBool('ReplaceFileUseUnit', False);
  FReadMap := Settings.ReadBool('ReadMap', True);
  FAvailTabIndex := Settings.ReadInteger('AvailTabIndex', 0);
  FParseAll := Settings.ReadBool('ParseAll', True);
end;

procedure TUsesExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;
  Settings.WriteBool('ReplaceFileUseUnit', FReplaceFileUseUnit);
  Settings.WriteBool('ReadMap', FReadMap);
  Settings.WriteInteger('AvailTabIndex', FAvailTabIndex);
  Settings.WriteBool('ParseAll', FParseAll);
end;

{ TfmUsesManager }

constructor TfmUsesManager.Create(_Owner: TComponent; _UsesExpert: TUsesExpert);
var
  Bitmap: TBitmap;
begin
{$IFOPT D+}
  SendDebug('Creating UsesManager form');
{$ENDIF D+}
  FUsesExpert := _UsesExpert;
  inherited Create(_Owner);

  DoubleBuffered := True;
  pnlUnits.DoubleBuffered := True;
  pnlUses.DoubleBuffered := True;

  FLeftRatio :=  pnlUses.Width / ClientWidth;

  Bitmap := FUsesExpert.GetBitmap;
  if Assigned(Bitmap) then
    ConvertBitmapToIcon(Bitmap, Icon);

  sg_Identifiers.Cells[0, 0] := 'Identifier';
  sg_Identifiers.Cells[1, 0] := 'Unit';
  sg_Identifiers.Cells[0, 1] := 'searching ...';

  TStringGrid_AdjustRowHeight(sg_Interface);
  TStringGrid_AdjustRowHeight(sg_Implementation);
  TStringGrid_AdjustRowHeight(sg_SearchPath);
  TStringGrid_AdjustRowHeight(sg_Project);
  TStringGrid_AdjustRowHeight(sg_Favorite);
  TStringGrid_AdjustRowHeight(sg_Identifiers);
end;

procedure TfmUsesManager.GetProjectFiles;
var
  IProject: IOTAProject;
  IModuleInfo: IOTAModuleInfo;
  i: Integer;
  FileName: string;
begin
{$IFOPT D+}
  SendDebug('Reading project files');
{$ENDIF D+}
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
{$IFOPT D+}
  SendDebug('Done reading project files');
{$ENDIF D+}
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
{$IFOPT D+}
  SendDebug('Reading common files');
{$ENDIF D+}
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
{$IFOPT D+}
  SendDebug('Done reading common files');
{$ENDIF D+}
end;

procedure TfmUsesManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if ModalResult = mrOk then
    SaveFavorites;
end;

procedure TfmUsesManager.FormCreate(Sender: TObject);

  function RetrieveEditorBlockSelection: string;
  var
    Temp: string;
    i: Integer;
  begin
    Temp := GxOtaGetCurrentSelection;
    // Only use the currently selected text if the length is between 1 and 80
    if (Length(Trim(Temp)) >= 1) and (Length(Trim(Temp)) <= 80) then
    begin
      i := Min(Pos(#13, Temp), Pos(#10, Temp));
      if i > 0 then
        Temp := Copy(Temp, 1, i - 1);
    end else
      Temp := '';
    Result := Temp;
  end;

var
  Selection: string;
  Paths: TStringList;
begin
  TControl_SetMinConstraints(Self);
  pnlUses.Constraints.MinWidth := pnlUses.Width;
   
  FProjectUnits := TStringList.Create;
  sg_Project.AssociatedList := FProjectUnits;

  FCommonUnits := TStringList.Create;
  sg_Common.AssociatedList := FCommonUnits;

  FFavoriteUnits := TStringList.Create;
  sg_Favorite.AssociatedList := FFavoriteUnits;

  FSearchPathUnits := TStringList.Create;
  sg_SearchPath.AssociatedList := FSearchPathUnits;

  FFavUnitsExports := TStringList.Create;
  FAliases := TStringList.Create;
  FOldToNewUnitNameMap := TStringList.Create;

  Paths := TStringList.Create;
  try
    GxOtaGetAllPossiblePaths(Paths);
    if FUsesExpert.FParseAll then begin
{$IFOPT D+}
        SendDebug('Running UnitExportParser thread to get identifiers from all units in search path');
{$ENDIF D+}
        FUnitExportParserThread := TUnitExportParserThread.Create(nil, Paths,
          ConfigInfo.ConfigPath + 'UsesExpertCache', OnExportParserFinished);
        tim_Progress.Enabled := True;
    end else begin
      LoadFavorites;

      if FFavoriteUnits.Count = 0 then begin
        sg_Identifiers.Cells[0, 1] := 'no favorites selected';
      end else begin
{$IFOPT D+}
        SendDebug('Running UnitExportParser thread to get identifiers from favorites');
{$ENDIF D+}
        FUnitExportParserThread := TUnitExportParserThread.Create(FFavoriteUnits, Paths,
          ConfigInfo.ConfigPath + 'UsesExpertCache', OnExportParserFinished);
        tim_Progress.Enabled := True;
      end;
    end;
  finally
    FreeAndNil(Paths);
  end;

  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.pas');
  FFindThread.FileMasks.Add('*.dcu');
  GxOtaGetEffectiveLibraryPath(FFindThread.SearchDirs);
  FFindThread.OnFindComplete := SearchPathReady;
  FFindThread.StartFind;
{$IFOPT D+}
  SendDebug('Started SearchPath FindThread');
{$ENDIF D+}

  pcUnits.ActivePage := tabSearchPath;
  GxOtaGetUnitAliases(FAliases);
  GetCommonFiles;
  GetProjectFiles;
  ReadUsesList;

  TWinControl_ActivateDropFiles(sg_Interface, lbxInterfaceFilesDropped);
  TWinControl_ActivateDropFiles(sg_Implementation, lbxImplementationFilesDropped);
  TWinControl_ActivateDropFiles(sg_Favorite, lbxFavoriteFilesDropped);

  Selection := RetrieveEditorBlockSelection;
  if Trim(Selection) = '' then begin
    try
      Selection := GxOtaGetCurrentIdent;
    except
       // if access violation created
      on E: Exception do
        Selection := '';
    end;
  end;
  edtIdentifierFilter.Text := Selection;
  edtIdentifierFilter.SelectAll;
end;

procedure TfmUsesManager.FormDestroy(Sender: TObject);
begin
  tim_Progress.Enabled := False;

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

procedure TfmUsesManager.FormResize(Sender: TObject);
begin
  pnlUses.Width := Round(FLeftRatio * ClientWidth);
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

procedure TfmUsesManager.AddListToIntfSection(sg: TObject);
var
  i: Integer;
  grid: TStringGrid;
  col: Integer;
begin
  if sg is TStringGrid then begin
    grid := TStringGrid(sg);
    col := grid.ColCount - 1;
    for i := grid.Selection.Top to grid.Selection.Bottom do
      AddToIntfSection(grid.Cells[col, i]);
  end;
end;

function TfmUsesManager.AddToStringGrid(sg: TStringGrid; const UnitName: string): Integer;
var
  i: Integer;
  cnt: Integer;
  Found: Boolean;
begin
  Result := -1;
  Found := False;
  cnt := sg.RowCount;
  for i := sg.FixedRows to cnt - 1 do begin
    if SameText(sg.Cells[0, i], UnitName) then begin
      Result := i;
      Found := True;
      Break;
    end;
  end;
  if not found then begin
    if (cnt = sg.FixedRows + 1) and (sg.Cells[0, sg.FixedRows] = '') then
      cnt :=sg.FixedRows;
    sg.RowCount := cnt + 1;
    sg.Cells[0, cnt] := UnitName;
    Result := cnt;
  end;
  sg.Row:= Result;
end;

procedure TfmUsesManager.DeleteFromStringGrid(sg: TStringGrid; const UnitName: string);
var
  i: Integer;
  cnt: Integer;
  j: Integer;
begin
  cnt := sg.RowCount;
  for i := sg.FixedRows to cnt - 1 do begin
    if SameText(sg.Cells[0, i], UnitName) then begin
      for j := i + 1 to cnt - 1 do
        sg.Cells[0, j - 1] := sg.Cells[0, j];
      Dec(cnt);
      TGrid_SetNonfixedRowCount(sg, cnt);
      if cnt = 0 then
        sg.Cells[0, sg.FixedRows] := '';
      Exit; //==>
    end;
  end;
end;

function TfmUsesManager.AddToIntfSection(const UnitName: string): Integer;
begin
  Result := AddToStringGrid(sg_Interface, UnitName);
  DeleteFromStringGrid(sg_Implementation, UnitName);
end;

procedure TfmUsesManager.DeleteFromIntfSection(const UnitName: string);
begin
  DeleteFromStringGrid(sg_Interface, UnitName);
end;

procedure TfmUsesManager.OpenUnit(const UnitName: string);
var
  FileName: string;
begin
  FileName := UnitName + '.pas';
  GxOtaOpenFileFromPath(FileName);
end;

procedure TfmUsesManager.pcUnitsChange(Sender: TObject);
begin
  if pcUnits.ActivePage = tabIdentifiers then begin
    edtIdentifierFilter.Visible := True;
    TWinControl_SetFocus(edtIdentifierFilter);
    edtUnitFilter.Visible := False;
  end else begin
    edtUnitFilter.Visible := True;
    TWinControl_SetFocus(edtUnitFilter);
    edtIdentifierFilter.Visible := False;
  end;
end;

procedure TfmUsesManager.pcUnitsResize(Sender: TObject);
begin
  TGrid_Resize(sg_SearchPath, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Project, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Common, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Favorite, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmUsesManager.pcUsesResize(Sender: TObject);
begin
  TGrid_Resize(sg_Interface, [roUseGridWidth, roUseAllRows]);
  TGrid_Resize(sg_Implementation, [roUseGridWidth, roUseAllRows]);
end;

procedure TfmUsesManager.pnlAvailableHeaderResize(Sender: TObject);
begin
  edtUnitFilter.Left := 0;
  edtUnitFilter.Width := pnlAvailableHeader.Width;
  edtIdentifierFilter.Left := 0;
  edtIdentifierFilter.Width := pnlAvailableHeader.Width;
end;

procedure TfmUsesManager.pnlUsesBottomResize(Sender: TObject);
var
  w: Integer;
begin
  w := (pnlUsesBottom.ClientWidth - 3 * 8) div 4;
  b_DeleteFromIntf.Width := w;
  b_MoveToImpl.Width := w;
  b_MoveToImpl.Left := w + 8;
  b_MoveToIntf.Width := w;
  b_MoveToIntf.Left := 2 * (w + 8);
  b_DeleteFromImpl.Width := w;
  b_DeleteFromImpl.Left := 3 * (w + 8);

  btnAddDots.Width := 2 * w + 8;
  btnRemoveDots.Width := 2 * w + 8;
  btnRemoveDots.Left := 2 * (w + 8);
end;

procedure TfmUsesManager.pnlUsesResize(Sender: TObject);
begin
  p_Interface.Width := pnlUses.ClientWidth div 2;
end;

procedure TfmUsesManager.AddListToImplSection(sg: TObject; RemoveFromInterface: Boolean);
var
  i: Integer;
  col: Integer;
  grid: TStringGrid;
begin
  if sg is TStringGrid then begin
    grid := TStringGrid(sg);
    col := grid.ColCount - 1;
    for i := grid.Selection.Top to grid.Selection.Bottom do
      AddToImplSection(grid.Cells[col, i], RemoveFromInterface);
  end;
end;

function TfmUsesManager.AddToImplSection(const UnitName: string; RemoveFromInterface: Boolean): Integer;
begin
  if RemoveFromInterface then
    DeleteFromStringGrid(sg_Interface, UnitName);
  Result := AddToStringGrid(sg_Implementation, UnitName);
end;

procedure TfmUsesManager.DeleteFromImplSection(const UnitName: string);
begin
  DeleteFromStringGrid(sg_Implementation, UnitName);
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

procedure TfmUsesManager.UnAlias(sg: TStringGrid);
var
  i: Integer;
  s: string;
  p: Integer;
begin
  for i := sg.RowCount - 1 downto sg.FixedRows do begin
    s := sg.cells[0, i];
    p := Pos(' ', s);
    if p > 0 then begin
      s := Copy(s, 1, p - 1);
      sg.cells[0, i] := s;
    end;
  end;
end;

function TfmUsesManager.IndexInStringGrid(sg: TStringGrid; const UnitName: string): integer;
var
  col: Integer;
  i: Integer;
begin
  col := sg.ColCount - 1;
  for i := sg.FixedRows to sg.RowCount - 1 do begin
    if SameText(sg.Cells[col, i], UnitName) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TfmUsesManager.ReadUsesList;
var
  i: Integer;
  UsesManager: TUsesManager;
  sl: TStringList;
begin
{$IFOPT D+}
  SendDebug('Reading uses lists');
{$ENDIF D+}
  TStringGrid_Clear(sg_Interface);
  TStringGrid_Clear(sg_Implementation);

  sl := nil;
  UsesManager := TUsesManager.Create(GxOtaGetCurrentSourceEditor);
  try
    sl := TStringList.Create;
    UsesManager.InterfaceUses.AssignTo(sl);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
      sl[i] := ApplyAlias(sl[i]);
    TStringGrid_AssignCol(sg_Interface, 0, sl);
    TGrid_Resize(sg_Interface, [roUseGridWidth, roUseAllRows]);

    UsesManager.ImplementationUses.AssignTo(sl);
    sl.Sort;
    for i := 0 to sl.Count - 1 do
      sl[i] := ApplyAlias(sl[i]);
    TStringGrid_AssignCol(sg_Implementation, 0, sl);
    TGrid_Resize(sg_Implementation, [roUseGridWidth, roUseAllRows]);
  finally
    FreeAndNil(sl);
    FreeAndNil(UsesManager);
  end;
{$IFOPT D+}
  SendDebug('Done reading uses lists');
{$ENDIF D+}
end;

procedure TfmUsesManager.sg_ImplementationDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToImplSection(Source, True);
end;

procedure TfmUsesManager.sg_InterfaceDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  AddListToIntfSection(Source);
end;

procedure TfmUsesManager.sg_ImplementationDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Project) or (Source = sg_Common) or
    (Source = sg_Favorite) or (Source = sg_SearchPath) or (Source = sg_Interface);
end;

procedure TfmUsesManager.sg_InterfaceDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Project) or (Source = sg_Common) or
    (Source = sg_Favorite) or (Source = sg_SearchPath) or (Source = sg_Implementation);
end;

procedure TfmUsesManager.DrawStringGridCell(_sg: TStringGrid; const _Text: string; const _Rect: TRect;
  _State: TGridDrawState; _Focused: Boolean);
var
  cnv: TCanvas;
begin
  cnv := _sg.Canvas;
  if _Text = '' then
    cnv.Brush.Color := _sg.Color
  else begin
    if gdSelected in _State then begin
      if not _Focused then begin
        cnv.Brush.Color := clDkGray;
        // I would have used clHighlightText but that becomes unreadable when theming is active
        cnv.Font.Color := clWhite;
      end;
    end;
  end;
  cnv.FillRect(_Rect);
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
end;

procedure TfmUsesManager.sg_UsedDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, sg.Focused);
end;

procedure TfmUsesManager.sg_MouseDownForDragging(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  sg: TStringGrid;
  SourceCol: Integer;
  SourceRow: Integer;
begin
  if button = mbLeft then begin
    sg := Sender as TStringGrid;
    // Convert mouse coordinates X, Y to col and row indices
    sg.MouseToCell(X, Y, SourceCol, SourceRow);
    // Allow dragging only if a non fixed cell was clicked
    if (SourceCol >= sg.FixedCols) and (SourceRow >= sg.FixedCols) then begin
      // Begin dragging after mouse has moved 16 pixels
      sg.BeginDrag(False, 16);
    end;
  end;
end;

procedure TfmUsesManager.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);

  procedure SetShortcut(_Shortcut: TShortCut; _act: TCustomAction);
  var
    i: Integer;
    act: TCustomAction;
  begin
    for i := 0 to ActionList.ActionCount-1 do begin
      act := ActionList.Actions[i] as TCustomAction;
      if act.ShortCut = _Shortcut then
        act.ShortCut := 0;
    end;
    if Assigned(_act) then
      _act.ShortCut := _Shortcut;
  end;

var
  HasSelectedItem: Boolean;
  AvailableSourceList: TStringGrid;
  ActiveLBHasSelection: Boolean;
begin
  HasSelectedItem :=     HaveSelectedItem(sg_Interface);
  actIntfMove.Enabled :=HasSelectedItem;
  actIntfDelete.Enabled := HasSelectedItem;
  actIntfAddToFavorites.Enabled := HasSelectedItem;

  HasSelectedItem := HaveSelectedItem(sg_Implementation);
  actImplMove.Enabled := HasSelectedItem;
  actImplDelete.Enabled := HasSelectedItem;
  actImplAddToFavorites.Enabled := HasSelectedItem;

  AvailableSourceList := GetAvailableSourceList;
  ActiveLBHasSelection := HaveSelectedItem(AvailableSourceList);

  actAvailAddToImpl.Enabled := ActiveLBHasSelection;
  actAvailAddToIntf.Enabled := ActiveLBHasSelection;
  actAvailAddToFav.Enabled := ActiveLBHasSelection;
  actFavDelUnit.Enabled := HaveSelectedItem(sg_Favorite);

  if ActiveControl = sg_Interface then begin
    SetShortcut(VK_INSERT, actIntfAddToFavorites);
    SetShortcut(VK_DELETE, actIntfDelete);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), actIntfMove);
  end else if ActiveControl = sg_Implementation then begin
    SetShortcut(VK_INSERT, actImplAddToFavorites);
    SetShortcut(VK_DELETE, actImplDelete);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), actImplMove);
  end else if ActiveControl = sg_Favorite then begin
    SetShortcut(VK_INSERT, actFavAddUnit);
    SetShortcut(VK_DELETE, actFavDelUnit);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), nil);
  end else begin
    SetShortcut(VK_INSERT, nil);
    SetShortcut(VK_DELETE, nil);
    SetShortcut(ShortCut(Ord('M'), [ssCtrl]), nil);
  end;

  actOpenUnit.Enabled := HaveSelectedItem(GetListForOpen);
end;

function TfmUsesManager.HaveSelectedItem(sg: TStringGrid): Boolean;
begin
  Result := False;
  if not Assigned(sg) then
    Exit; //==>

  Result := sg.Selection.Bottom - sg.Selection.Top >= 0;
end;

procedure TfmUsesManager.DeleteSelected(sg: TStringGrid);
var
  i: Integer;
  col: Integer;
begin
  Assert(Assigned(sg));
  col := sg.ColCount - 1;
  for i := sg.Selection.Bottom downto sg.Selection.Top do begin
    DeleteFromStringGrid(sg, sg.Cells[col, i])
  end;
end;

procedure TfmUsesManager.MoveSelected(Src, Dest: TStringGrid; ToInterface: Boolean);
var
  i: Integer;
  col: Integer;
  UnitName: string;
begin
  Assert(Assigned(Src) and Assigned(Dest));
  col := Src.ColCount - 1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do
  begin
    UnitName := Src.Cells[col, i];

    if ToInterface then
      AddToIntfSection(UnitName)
    else
      AddToImplSection(UnitName, True);
    end;
end;

procedure TfmUsesManager.actFavDelUnitExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := sg_Favorite.Selection.Bottom downto sg_Favorite.Selection.Top do
    DeleteFromFavorites(sg_Favorite.Cells[0, i]);
end;

procedure TfmUsesManager.actFocusImplementationExecute(Sender: TObject);
begin
  TWinControl_SetFocus(sg_Implementation);
end;

procedure TfmUsesManager.actFocusInterfaceExecute(Sender: TObject);
begin
  TWinControl_SetFocus(sg_Interface);
end;

procedure TfmUsesManager.actFavAddUnitExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
begin
  dlgOpen.InitialDir := ExtractFilePath(GetIdeRootDirectory);
  if dlgOpen.Execute then begin
    for i := 0 to dlgOpen.Files.Count - 1 do begin
      FileName := ExtractPureFileName(dlgOpen.Files[i]);
      AddToFavorites(FileName);
    end;
  end
end;

procedure TfmUsesManager.actAvailAddAllToFavExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
  Src: TStringGrid;
begin
  FileName := '';
  Src := GetAvailableSourceList;
  for i := Src.FixedRows to Src.RowCount - 1 do begin
    FileName := Src.Cells[0, i];
    if FileName <> '' then
      AddToFavorites(FileName);
  end;
  edtUnitFilter.Text := '';
  pcUnits.ActivePage := tabFavorite;
  pcUnits.Change;
end;

procedure TfmUsesManager.actAvailAddToFavExecute(Sender: TObject);
var
  i: Integer;
  FileName: string;
  Src: TStringGrid;
begin
  FileName := '';
  Src := GetAvailableSourceList;
  for i := Src.Selection.Bottom downto Src.Selection.Top do begin
    FileName := Src.Cells[0, i];
    if FileName <> '' then
      AddToFavorites(FileName);
  end;
  edtUnitFilter.Text := '';
  pcUnits.ActivePage := tabFavorite;
  pcUnits.Change;
end;

procedure TfmUsesManager.actAvailAddToIntfExecute(Sender: TObject);
var
  Src: TStringGrid;
  i: Integer;
  Col: Integer;
  Row: Integer;
begin
  src := GetAvailableSourceList;
  Col := src.ColCount - 1;
  Row := -1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do
    Row := AddToIntfSection(Src.Cells[Col, i]);
  if Row <> -1 then
    sg_Interface.Row := Row;
end;

procedure TfmUsesManager.actAvailAddToImplExecute(Sender: TObject);
var
  i: Integer;
  Src: TStringGrid;
  Col: Integer;
  Row: Integer;
begin
  src := GetAvailableSourceList;
  Col := src.ColCount - 1;
  Row := -1;
  for i := Src.Selection.Bottom downto Src.Selection.Top do begin
    Row := AddToImplSection(Src.Cells[col, i], True);
  end;
  if Row <> -1 then
    sg_Implementation.Row := Row;
end;

function TfmUsesManager.GetAvailableSourceList: TStringGrid;
begin
  Result := nil;
  if pcUnits.ActivePage = tabProject then
    Result := sg_Project
  else if pcUnits.ActivePage = tabCommon then
    Result := sg_Common
  else if pcUnits.ActivePage = tabFavorite then
    Result := sg_Favorite
  else if pcUnits.ActivePage = tabSearchPath then
    Result := sg_SearchPath
  else if pcUnits.ActivePage = tabIdentifiers then
    Result := sg_Identifiers;
  Assert(Assigned(Result));
end;

procedure TfmUsesManager.sg_InterfaceDblClick(Sender: TObject);
begin
  if IsCtrlDown then
    OpenSelectedUnit(sg_Interface)
  else
    DeleteItemIndex(sg_Interface, True);
end;

procedure TfmUsesManager.sg_AvailDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
  GridFocused: Boolean;
begin
  GridFocused := sg.Focused or edtUnitFilter.Focused or edtIdentifierFilter.Focused;
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, GridFocused);
end;

procedure TfmUsesManager.sg_ImplementationDblClick(Sender: TObject);
begin
  if IsCtrlDown then
    OpenSelectedUnit(sg_Implementation)
  else
    DeleteItemIndex(sg_Implementation, False);
end;

procedure TfmUsesManager.DeleteItemIndex(sg: TStringGrid; FromInterface: Boolean);
var
  col: Integer;
  row: Integer;
begin
  row := sg.row;
  if (row >= sg.FixedRows) and (row < sg.RowCount) then
    begin
      col := sg.ColCount - 1;
      if FromInterface then
        DeleteFromIntfSection(sg.Cells[col, row])
      else
        DeleteFromImplSection(sg.Cells[col, row]);
    end;
end;

procedure TfmUsesManager.lbxAvailDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = sg_Interface) or (Source = sg_Implementation);
end;

procedure TfmUsesManager.lbxAvailDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Sender = sg_Favorite then begin
    AddListToFavorites(Source as TStringGrid)
  end;
  if Source = sg_Interface then begin
    actIntfDelete.Execute;
  end else if Source = sg_Implementation then begin
    actImplDelete.Execute;
  end;
end;

function TfmUsesManager.GetListForOpen: TStringGrid;
begin
  if ActiveControl = sg_Implementation then
    Result := sg_Implementation
  else if ActiveControl = sg_Interface then
    Result := sg_Interface
  else
    Result :=  GetAvailableSourceList;
end;

procedure TfmUsesManager.OpenSelectedUnit(List: TStringGrid);
var
  UnitName: string;
  col: Integer;
begin
  Assert(Assigned(List));

  col := List.ColCount-1;
  UnitName := List.Cells[col, List.row];
  OpenUnit(UnitName);
  ModalResult := mrCancel;
end;

procedure TfmUsesManager.actOpenUnitExecute(Sender: TObject);
begin
  OpenSelectedUnit(GetListForOpen);
end;

procedure TfmUsesManager.lbxAvailDblClick(Sender: TObject);
var
  Src: TStringGrid;
  col: Integer;
  UnitName: string;
begin
  Src := GetAvailableSourceList;
  Assert(Assigned(Src));
  col := Src.ColCount - 1;
  UnitName := Src.Cells[col, Src.row];
  if IsCtrlDown then begin
    OpenUnit(UnitName);
    ModalResult := mrCancel;
  end else begin
    AddToImplSection(UnitName, True)
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
{$IFOPT D+}
  SendDebug('SarchPath is ready');
{$ENDIF D+}
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
{$IFOPT D+}
  SendDebugFmt('Found %d files in SarchPath', [PathFiles.Count]);
{$ENDIF D+}
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
  sg_SearchPath.Color := clWindow;
  sg_SearchPath.Enabled := True;
end;

procedure TfmUsesManager.DeleteFromFavorites(const Item: string);
begin
  DeleteStringFromList(FFavoriteUnits, Item);
  DeleteFromStringGrid(sg_Favorite, Item);
end;

procedure TfmUsesManager.AddToFavorites(const Item: string);
begin
  EnsureStringInList(FFavoriteUnits, Item);
  AddToStringGrid(sg_Favorite, Item);
end;

procedure TfmUsesManager.edtIdentifierFilterChange(Sender: TObject);
begin
  FilterIdentifiers;
end;

procedure TfmUsesManager.edtIdentifierFilterEnter(Sender: TObject);
begin
  sg_Identifiers.Invalidate;
end;

procedure TfmUsesManager.edtIdentifierFilterExit(Sender: TObject);
begin
  sg_Identifiers.Invalidate;
end;

procedure TfmUsesManager.SplitterMoved(Sender: TObject);
begin
  FLeftRatio :=  pnlUses.Width / ClientWidth;
end;

procedure TfmUsesManager.SwitchUnitsTab(_Direction: Integer);
var
  i: Integer;
begin
  i := pcUnits.ActivePageIndex + _Direction;
  if i = pcUnits.PageCount then
    i := 0
  else if i < 0 then
    i := pcUnits.PageCount - 1;
  pcUnits.ActivePageIndex := i;
  pcUnits.Change;
end;

procedure TfmUsesManager.edtIdentifierFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    SwitchUnitsTab(1);
  end else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then begin
    SwitchUnitsTab(-1);
  end else begin
    if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
    begin
      sg_Identifiers.Perform(WM_KEYDOWN, Key, 0);
      Key := 0;
    end;
  end;
end;

procedure TfmUsesManager.edtUnitFilterChange(Sender: TObject);
begin
  FilterVisibleUnits;
end;

procedure TfmUsesManager.edtUnitFilterEnter(Sender: TObject);
begin
  sg_Project.Invalidate;
  sg_Common.Invalidate;
  sg_Favorite.Invalidate;
  sg_SearchPath.Invalidate;
end;

procedure TfmUsesManager.edtUnitFilterExit(Sender: TObject);
begin
  sg_Project.Invalidate;
  sg_Common.Invalidate;
  sg_Favorite.Invalidate;
  sg_SearchPath.Invalidate;
end;

procedure TfmUsesManager.FilterStringGrid(Filter: string; List: TStrings; sg: TStringGrid);
var
  FilterList: TStrings;
begin
  FilterList := TStringList.Create;
  try
    FilterStringList(List, FilterList, Filter, False);
    TStringGrid_AssignCol(sg, 0, FilterList);
    TGrid_Resize(sg, [roUseGridWidth, roUseAllRows]);
  finally
    FreeAndNil(FilterList);
  end;
end;

procedure TfmUsesManager.FilterVisibleUnits;
var
  Filter: string;
begin
  Filter := Trim(edtUnitFilter.Text);
  FilterStringGrid(Filter, FFavoriteUnits, sg_Favorite);
  FilterStringGrid(Filter, FProjectUnits, sg_Project);
  FilterStringGrid(Filter, FCommonUnits, sg_Common);
  FilterStringGrid(Filter, FSearchPathUnits, sg_SearchPath);

  SelectFirstItemInLists;
end;

procedure TfmUsesManager.FilterIdentifiers;
var
  i: Integer;
  Identifier: string;
  UnitName: string;
  Filter: string;
  FixedRows: Integer;
  cnt: Integer;
begin
{$IFOPT D+}
  SendDebug('Filtering identifiers');
{$ENDIF D+}
  Filter := Trim(edtIdentifierFilter.Text);
  FixedRows := sg_Identifiers.FixedRows;
  TStringGrid_Clear(sg_Identifiers);
  TGrid_SetNonfixedRowCount(sg_Identifiers, FFavUnitsExports.Count);
  cnt := 0;
  for i := 0 to FFavUnitsExports.Count - 1 do begin
    Identifier := FFavUnitsExports[i];
    if (Filter = '') or StartsText(Filter, Identifier) then begin
      sg_Identifiers.Cells[0, FixedRows + cnt] := Identifier;
      UnitName := PChar(FFavUnitsExports.Objects[i]);
      sg_Identifiers.Cells[1, FixedRows + cnt] := UnitName;
      Inc(cnt);
    end;
  end;
  TGrid_SetNonfixedRowCount(sg_Identifiers, cnt);
{$IFOPT D+}
  SendDebug('Done filtering identifiers');
{$ENDIF D+}
  ResizeIdentiferGrid;
end;

procedure TfmUsesManager.ResizeIdentiferGrid;
begin
  TGrid_Resize(sg_Identifiers, [roUseGridWidth, roUseAllRows]);
  TGrid_RestrictToGridWdith(sg_Identifiers, [1]);
end;

procedure TfmUsesManager.edtUnitFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  sg: TStringGrid;
begin
  if (Key = VK_TAB) and (Shift = [ssCtrl]) then
  begin
    SwitchUnitsTab(1);
  end else if (Key = VK_TAB) and (Shift = [ssCtrl, ssShift]) then begin
    SwitchUnitsTab(-1);
  end else begin
    if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then
    begin
      sg := GetAvailableSourceList;
      Assert(Assigned(sg));
      if sg.RowCount > sg.FixedRows then
        sg.Perform(WM_KEYDOWN, Key, 0)
      else
        sg.Row := sg.FixedRows;
      Key := 0;
    end;
  end;
end;

procedure TfmUsesManager.SelectFirstItemInLists;

  procedure SelectBestItem(sg: TStringGrid);
  var
    Filter: string;
    MatchIndex: Integer;
  begin
    if sg.RowCount > sg.FixedRows then
    begin
      Filter := Trim(edtUnitFilter.Text);
      MatchIndex := IndexInStringGrid(sg, Filter);
      if MatchIndex = -1 then
        MatchIndex := sg.FixedRows;
      sg.Row := MatchIndex;
    end;
  end;

begin
  SelectBestItem(sg_Common);
  SelectBestItem(sg_Favorite);
  SelectBestItem(sg_SearchPath);
  SelectBestItem(sg_Project);
end;

procedure TfmUsesManager.tabIdentifiersResize(Sender: TObject);
begin
  ResizeIdentiferGrid;
end;

procedure TfmUsesManager.tim_ProgressTimer(Sender: TObject);
begin
  if Assigned(FUnitExportParserThread) then begin
    sg_Identifiers.Cells[1, 1] := IntToStr(FUnitExportParserThread.Identifiers.Count);
  end;
end;

procedure TfmUsesManager.LoadFavorites;
var
  fn: string;
begin
{$IFOPT D+}
  SendDebug('Loading favorites');
{$ENDIF D+}
  FFavoriteUnits.Sorted := False;
  FFavoriteUnits.Clear;
  fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
  if FileExists(fn) then
    FFavoriteUnits.LoadFromFile(fn);
  FFavoriteUnits.Sorted := True;
{$IFOPT D+}
  SendDebugFmt('Done loading %d favorites', [FFavoriteUnits.Count]);
{$ENDIF D+}
end;

procedure TfmUsesManager.OnExportParserFinished(_Sender: TObject);
var
  IdentIdx: Integer;
  FixedRows: Integer;
  UnitName: string;
  Identifier: string;
  sl: TStrings;
  Idx: Integer;
begin
  tim_Progress.Enabled := False;
  if not Assigned(FUnitExportParserThread) then
    Exit; //==>

  FixedRows := sg_Identifiers.FixedRows;
  sl := FUnitExportParserThread.Identifiers;

{$IFOPT D+}
  SendDebugFmt('UnitExportParser finished, found %d identifiers', [sl.Count]);
{$IFDEF DO_TIMING}
  SendDebugFmt('UnitExportParser loaded %d units', [FUnitExportParserThread.LoadedUnitsCount]);
  SendDebugFmt('UnitExportParser parsed %d units', [FUnitExportParserThread.ParsedUnitsCount]);
  SendDebugFmt('UnitExportParser searching time %d ms', [FUnitExportParserThread.SearchingTimeMS]);
  SendDebugFmt('UnitExportParser loading time %d ms', [FUnitExportParserThread.LoadingTimeMS]);
  SendDebugFmt('UnitExportParser inserting time %d ms', [FUnitExportParserThread.InsertingTimeMS]);
  SendDebugFmt('UnitExportParser parsing time %d ms', [FUnitExportParserThread.ParsingTimeMS]);
  SendDebugFmt('UnitExportParser processing time %d ms', [FUnitExportParserThread.ProcessingTimeMS]);
  SendDebugFmt('UnitExportParser sorting time %d ms', [FUnitExportParserThread.SortingTimeMS]);
  SendDebugFmt('UnitExportParser total time %d ms', [FUnitExportParserThread.TotalTimeMS]);
{$ENDIF}
{$ENDIF D+}

{$IFOPT D+}
  SendDebug('Preprocessing identifiers');
{$ENDIF D+}
  sg_Identifiers.RowCount := FixedRows + 1;
  sg_Identifiers.Cells[0, FixedRows] := '';
  sg_Identifiers.Cells[1, FixedRows] := '';
  for IdentIdx := 0 to sl.Count - 1 do begin
    Identifier := sl[IdentIdx];
    UniqueString(Identifier);
    UnitName := PChar(sl.Objects[IdentIdx]);
    // make sure the string is valid and not freed in the thread
    if FSearchPathUnits.Find(UnitName, Idx) then begin
      UnitName := FSearchPathUnits[Idx];
      FFavUnitsExports.AddObject(Identifier, Pointer(PChar(UnitName)));
    end;
  end;
{$IFOPT D+}
  SendDebug('Done preprocessing identifiers');
{$ENDIF D+}
  FilterIdentifiers;
end;

type
  TShowAddDotsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TShowAddDotsMessage }

function TShowAddDotsMessage.GetMessage: string;
resourcestring
  SConfirmAddDots =
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
  Result := SConfirmAddDots;
end;

procedure TfmUsesManager.btnAddDotsClick(Sender: TObject);
var
  DefaultNamespace: string;
  NameSpaces: TStringList;

  procedure AddDotsTo(sg: TStringGrid);
  var
    UnitIdx: Integer;
    OrigUnitName: string;
    NewUnitName: string;
    s: string;
    NsIdx: Integer;
    SearchPathUnitsIndex: Integer;
  begin
    for UnitIdx := sg.FixedRows to sg.RowCount - 1 do begin
      OrigUnitName := sg.Cells[0 ,UnitIdx];
      for NsIdx := 0 to NameSpaces.Count - 1 do begin
        s := NameSpaces[NsIdx] + '.' + OrigUnitName;
        SearchPathUnitsIndex := FSearchPathUnits.IndexOf(s);
        if SearchPathUnitsIndex <> -1 then begin
          NewUnitName := FSearchPathUnits[SearchPathUnitsIndex];
          if OrigUnitName <> NewUnitName then begin
            FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
            sg.Cells[0, UnitIdx] := NewUnitName;
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
      AddDotsTo(sg_Interface);
      AddDotsTo(sg_Implementation);
    end;
  finally
    FreeAndNil(NameSpaces);
  end;
end;

procedure TfmUsesManager.actOKExecute(Sender: TObject);
begin
  SaveChanges;
  ModalResult := mrOk;
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

  procedure RemoveDotsfrom(sg: TStringGrid);
  var
    i: Integer;
    OrigUnitName: string;
    NewUnitName: string;
    p: Integer;
  begin
    for i := sg.FixedRows to sg.RowCount - 1 do begin
      OrigUnitName := sg.Cells[0, i];
      NewUnitName := OrigUnitName;
      p := Pos('.', NewUnitName);
      while p > 0 do begin
        NewUnitName := Copy(NewUnitName, p + 1, 255);
        p := Pos('.', NewUnitName);
      end;
      if NewUnitName <> OrigUnitName then begin
        FOldToNewUnitNameMap.Values[OrigUnitName] := NewUnitName;
        sg.Cells[0, i] := NewUnitName;
      end;
    end;
  end;

begin
  if ShowGxMessageBox(TShowRemoveDotsMessage) <> mrYes then
    Exit;

  RemoveDotsfrom(sg_Interface);
  RemoveDotsfrom(sg_Implementation);
end;

procedure TfmUsesManager.SaveChanges;
var
  i: Integer;
  OldUnitName: string;
  NewUnitName: string;
  Units: TStringList;
  NewToOldUnitNameMap: TStringList;
begin
  UnAlias(sg_Interface);
  UnAlias(sg_Implementation);

  NewToOldUnitNameMap := nil;
  Units := TStringList.Create;
  try
    GetInterfaceUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if IndexInStringGrid(sg_Interface, OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (IndexInStringGrid(sg_Interface, NewUnitName) = -1) then
          RemoveUnitFromInterface(OldUnitName);
      end;
    end;

    GetImplementationUnits(Units);
    for i := 0 to Units.Count - 1 do begin
      OldUnitName := Units[i];
      if IndexInStringGrid(sg_Implementation, OldUnitName) = -1 then begin
        NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
        if (NewUnitName = '') or (IndexInStringGrid(sg_Implementation, NewUnitName) = -1) then
          RemoveUnitFromImplementation(OldUnitName);
      end;
    end;

    NewToOldUnitNameMap := TStringList.Create;
    for i := 0 to FOldToNewUnitNameMap.Count - 1 do begin
      OldUnitName := FOldToNewUnitNameMap.Names[i];
      NewUnitName := FOldToNewUnitNameMap.Values[OldUnitName];
      NewToOldUnitNameMap.Values[NewUnitName] := OldUnitName;
    end;

    for i := sg_Interface.FixedRows to sg_Interface.RowCount - 1 do begin
      NewUnitName := sg_Interface.Cells[0, i];
      if (NewUnitName <> '') and not SameText(NewUnitName, 'System') then begin
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
      end;
    end; // end for interface

    for i := sg_Implementation.FixedRows to sg_Implementation.RowCount - 1 do begin
      NewUnitName := sg_Implementation.Cells[0, i];
      if (NewUnitName <> '') and not SameText(NewUnitName, 'System') then begin
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
    end;
  finally
    FreeAndNil(NewToOldUnitNameMap);
    FreeAndNil(Units);
  end;
end;

procedure TfmUsesManager.SaveFavorites;
var
  fn: string;
begin
  // Do not localize.
  fn := ConfigInfo.ConfigPath + 'FavoriteUnits.txt'; // do not localize
  FFavoriteUnits.SaveToFile(fn);
end;

procedure TfmUsesManager.FormShow(Sender: TObject);

procedure SelectInGrid(_sg: TStringGrid; const _Unit: string);
  var
    Idx: Integer;
  begin
    Idx := IndexInStringGrid(_sg, _Unit);
    if Idx <> -1 then
      _sg.row := Idx
    else
      _sg.row := 0;
  end;

var
  s: string;
begin
  FilterVisibleUnits;

  s := edtIdentifierFilter.Text;
  SelectInGrid(sg_Interface, s);
  SelectInGrid(sg_Implementation, s);
end;

procedure TfmUsesManager.actImplMoveExecute(Sender: TObject);
begin
  MoveSelected(sg_Implementation, sg_Interface, True);
end;

procedure TfmUsesManager.actImplAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(sg_Implementation);
end;

procedure TfmUsesManager.actImplDeleteExecute(Sender: TObject);
begin
  DeleteSelected(sg_Implementation);
end;

procedure TfmUsesManager.actIntfAddToFavoritesExecute(Sender: TObject);
begin
  AddListToFavorites(sg_Interface);
end;

procedure TfmUsesManager.actIntfDeleteExecute(Sender: TObject);
begin
  DeleteSelected(sg_Interface);
end;

procedure TfmUsesManager.actIntfMoveExecute(Sender: TObject);
begin
  MoveSelected(sg_Interface, sg_Implementation, False);
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

procedure TfmUsesManager.actIntfUnAliasExecute(Sender: TObject);

  procedure ReplaceByAlias(sg: TStringGrid; AlsoSearch: TStringList);
  var
    i: Integer;
    s: string;
    p: Integer;
    sl: TStringList;
    FixedRows: Integer;
  begin
    sl := TStringList.Create;
    try
      for i := sg.FixedRows to sg.RowCount - 1  do begin
        s := sg.Cells[0, i];
        p := Pos(ALIAS_PREFIX, s);
        if p > 0 then begin
          p := p + Length(ALIAS_PREFIX);
          s := Copy(s, p, Length(s) - p);
        end;
        // only if the unit is not already in the list or in AlsoSearch, add it
        if (AlsoSearch.IndexOf(s) = -1) and (sl.IndexOf(s) = -1) then
          sl.Add(s);
      end;
      FixedRows := sg.FixedRows;
      for i := 0 to sl.Count - 1 do
        sg.Cells[0, i + FixedRows] := sl[i];
      TGrid_SetNonfixedRowCount(sg, sl.Count);
      if sl.Count = 0 then
        sg.Cells[0, FixedRows] := '';
    finally
      FreeAndNil(sl);
    end;
  end;

var
  IntSl: TStringList;
begin
  if ShowGxMessageBox(TShowUnaliasMessage) <> mrYes then
    Exit;

  IntSl := TStringList.Create;
  try
    // IntSl is empty
    ReplaceByAlias(sg_Interface, IntSl);

    // fill IntSl with the units from interface list
    TStringGrid_GetCol(sg_Interface, 0, IntSl);
    IntSl.Sort;
    ReplaceByAlias(sg_Implementation, IntSl);
  finally
    FreeAndNil(IntSl);
  end;
end;

procedure TfmUsesManager.AddListToFavorites(sg: TStringGrid);
var
  i: Integer;
  col: Integer;
begin
  Assert(Assigned(sg));
  col := sg.ColCount - 1;
  for i := sg.Selection.Top to sg.Selection.Bottom do
    AddToFavorites(sg.Cells[col, i]);
end;

{ TStringGrid }

function TStringGrid.GetAssociatedList: TStrings;
begin
  Result := TStrings(Tag);
end;

procedure TStringGrid.SetAssociatedList(const Value: TStrings);
begin
  Tag := Integer(Value);
end;

initialization
  RegisterGX_Expert(TUsesExpert);
end.

