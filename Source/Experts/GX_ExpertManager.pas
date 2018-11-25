unit GX_ExpertManager;

{$I GX_CondDefine.inc}

// Main menu item accelerators are not drawn under W2K because
// this form is modal.  TMenuToolBar might fix this.

interface

uses
  DropTarget, DropSource,
  Windows, Classes, ImgList, Controls, Forms, Dialogs,
  Menus, ComCtrls, ActnList, ToolWin, GX_Experts, GX_BaseForm;

type
  TExpertManagerExpert = class;

  TAddExpertToRegistryResult = (aerOK, aerDuplicate, aerNoExpert);

  TfmExpertManager = class(TfmBaseForm)
    lvExperts: TListView;
    pmItems: TPopupMenu;
    mitEnableExpert: TMenuItem;
    mitDisableExpert: TMenuItem;
    mitPopSep1: TMenuItem;
    mitRemoveExpert: TMenuItem;
    StatusBar: TStatusBar;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnEnable: TToolButton;
    tbnDisable: TToolButton;
    tbnSep1: TToolButton;
    tbnAdd: TToolButton;
    tbnRemove: TToolButton;
    tbnSep2: TToolButton;
    tbnHelp: TToolButton;
    actExpertEnable: TAction;
    actExpertDisable: TAction;
    actExpertAdd: TAction;
    actExpertRemove: TAction;
    actFileExit: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    ilStateImages: TImageList;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitHelp: TMenuItem;
    mitFileExit: TMenuItem;
    mitExpert: TMenuItem;
    mitExpertEnable: TMenuItem;
    mitExpertDisable: TMenuItem;
    mitSep1: TMenuItem;
    mitExpertAdd: TMenuItem;
    mitExpertRemove: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitSep2: TMenuItem;
    mitHelpAbout: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure actExpertEnableExecute(Sender: TObject);
    procedure actExpertDisableExecute(Sender: TObject);
    procedure actExpertAddExecute(Sender: TObject);
    procedure actExpertRemoveExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actExpertDisableUpdate(Sender: TObject);
    procedure actExpertEnableUpdate(Sender: TObject);
    procedure actExpertRemoveUpdate(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FExpertManager: TExpertManagerExpert;
    FFileDrop: TDropFileTarget;
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure AddExperts(ExpertList: TStrings);
    procedure UpdateControlsState;
    procedure RefreshExpertListControl;
    function ConfirmIfGExperts(const FileName: string): Boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
    destructor Destroy; override;
  end;

  TExpertManagerExpert = class(TGX_Expert)
  private
    FInitialExperts: TStrings;
    FInitialDisabledExperts: TStrings;

    class function GetExpertsRegKeyName(IsEnabled: Boolean; var Section: string): string;
    class procedure GetExperts(Experts: TStrings; IsEnabled: Boolean);

    class procedure MoveExpertRegistryKey(const ExpertName, FromBase, FromSection: string;
      const ToBase, ToSection: string);

    class function AddExpertToRegistry(const ExpertName, FileName: string): TAddExpertToRegistryResult;
    class procedure EnableExpertInRegistry(const ExpertName: string);
    class procedure DisableExpertInRegistry(const ExpertName: string);
    class procedure RemoveExpertFromRegistry(const ExpertName: string; IsEnabled: Boolean);

  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;

    property InitialExperts: TStrings read FInitialExperts;
    property InitialDisabledExperts: TStrings read FInitialDisabledExperts;
  end;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure InstallGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure RemoveGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, CommCtrl,
  GX_GExperts, GX_GxUtils, GX_GenericUtils, GX_OtaUtils,
  GX_ConfigurationInfo, GX_MessageBox, GX_SharedImages, GX_IdeUtils,
  GX_VerDepConst, GX_dzVclUtils;

type
  TGxExpertState = (gesCurrentlyEnabled, gesNextTimeEnabled,
                    gesCurrentlyDisabled, gesNextTimeDisabled,
                    gesInvalid);


  TShowDisableCurrentMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

const
  FormSaveKey = '\Window';

function GetListItemState(ListItem: TListItem): TGxExpertState;
begin
  if (ListItem <> nil) and (ListItem.StateIndex in [0..3]) then
    Result := TGxExpertState(ListItem.StateIndex)
  else
    Result := gesInvalid;
end;

{ TfmExpertManager }

procedure TfmExpertManager.actExpertEnableExecute(Sender: TObject);
var
  GxExpertState: TGxExpertState;
begin
  Assert(lvExperts.Selected <> nil);

  GxExpertState := GetListItemState(lvExperts.Selected);
  if (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]) then
    FExpertManager.DisableExpertInRegistry(lvExperts.Selected.Caption)
  else
    FExpertManager.EnableExpertInRegistry(lvExperts.Selected.Caption);
  UpdateControlsState;
end;

procedure TfmExpertManager.actExpertDisableExecute(Sender: TObject);
begin
  Assert(lvExperts.Selected <> nil);

  if not ConfirmIfGExperts(lvExperts.Selected.SubItems[0]) then
    Exit;

  FExpertManager.DisableExpertInRegistry(lvExperts.Selected.Caption);
  UpdateControlsState;
end;

procedure TfmExpertManager.actExpertAddExecute(Sender: TObject);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    if ShowOpenDialog('Select expert DLL', 'dll', Files, 'IDE Experts (*.dll)|*.dll') then
      Self.AddExperts(Files);
  finally
    FreeAndNil(Files);
  end;
end;

procedure TfmExpertManager.actExpertRemoveExecute(Sender: TObject);
resourcestring
  SConfirmRemoval = 'Are you sure you want to remove the selected expert?';
var
  ItemIsEnabled: Boolean;
begin
  Assert(lvExperts.Selected <> nil);

  if MessageDlg(SConfirmRemoval, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if not ConfirmIfGExperts(lvExperts.Selected.SubItems[0]) then
      Exit;

    ItemIsEnabled := GetListItemState(lvExperts.Selected) in [gesCurrentlyEnabled, gesNextTimeEnabled];
    FExpertManager.RemoveExpertFromRegistry(lvExperts.Selected.Caption, ItemIsEnabled);
    UpdateControlsState;
  end;
end;

procedure TfmExpertManager.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmExpertManager.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 7);
end;

procedure TfmExpertManager.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmExpertManager.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

constructor TfmExpertManager.Create(AOwner: TComponent);
begin
  raise Exception.Create('Call constructor CreateWithManager');
  inherited;
end;

constructor TfmExpertManager.CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
begin
  FExpertManager := Manager;
  inherited Create(AOwner);

  TControl_SetMinConstraints(Self);

  SetToolbarGradient(ToolBar);
  SetNonModalFormPopupMode(Self);

  FFileDrop := TDropFileTarget.Create(nil);
  FFileDrop.OnDrop := DropFiles;
  FFileDrop.Dragtypes := [dtCopy, dtMove, dtLink];
  FFileDrop.ShowImage := True;
  FFileDrop.Register(lvExperts);

  LoadSettings;
  RefreshExpertListControl;
end;

destructor TfmExpertManager.Destroy;
begin
  FExpertManager := nil;
  SaveSettings;

  FFileDrop.Unregister;
  FreeAndNil(FFileDrop);

  inherited Destroy;
end;

procedure TfmExpertManager.RefreshExpertListControl;
var
  i: Integer;
  ListItem: TListItem;
  ExpertList: TStringList;
begin
  lvExperts.Items.BeginUpdate;
  try
    lvExperts.Items.Clear;
    ExpertList := TStringList.Create;
    try
      FExpertManager.GetExperts(ExpertList, True); // Get enabled experts
      for i := 0 to ExpertList.Count-1 do
      begin
        ListItem := lvExperts.Items.Add;
        ListItem.Caption := ExpertList.Names[i];
        ListItem.SubItems.Add(ExpertList.Values[ExpertList.Names[i]]);
        // Check if the expert was in registry when we started this expert
        if FExpertManager.InitialExperts.Values[ExpertList.Names[i]] <> '' then
          ListItem.StateIndex := Ord(gesCurrentlyEnabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeEnabled);
      end;

      ExpertList.Clear;

      FExpertManager.GetExperts(ExpertList, False); // Get disabled experts
      for i := 0 to ExpertList.Count-1 do
      begin
        ListItem := lvExperts.Items.Add;
        ListItem.Caption := ExpertList.Names[i];
        ListItem.SubItems.Add(ExpertList.Values[ExpertList.Names[i]]);

        if FExpertManager.InitialDisabledExperts.Values[ ExpertList.Names[i]] <> '' then
          ListItem.StateIndex := Ord(gesCurrentlyDisabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeDisabled);
      end;
    finally
      FreeAndNil(ExpertList);
    end;
  finally
    lvExperts.Items.EndUpdate;
  end;
end;

function TfmExpertManager.ConfigurationKey: string;
begin
  Result := 'ExpertManager';
end;

function TfmExpertManager.ConfirmIfGExperts(const FileName: string): Boolean;
begin
  Result := True;

  if SameFileName(FileName, ThisDllName) then
    Result := (ShowGxMessageBox(TShowDisableCurrentMessage) = mrYes);
end;

procedure TfmExpertManager.UpdateControlsState;
var
  CurrentItemCaption: string;
  Item: TListItem;
begin
  CurrentItemCaption := '';
  if lvExperts.Selected <> nil then
    CurrentItemCaption := lvExperts.Selected.Caption;
  try
    RefreshExpertListControl;
  finally
    if CurrentItemCaption <> '' then
    begin
      Item := lvExperts.FindCaption(0, CurrentItemCaption, False, True, False);
      if Item <> nil then
        Item.Selected := True;
    end;
  end;
end;

procedure TfmExpertManager.AddExperts(ExpertList: TStrings);
resourcestring
  SCouldNotAddExpertDupe = '%s could not be added as an expert.' + sLineBreak
                          + sLineBreak
                          + 'The module you chose is already loaded as an expert DLL.';
  SCouldNotAddExpertInvalid = '%s could not be added as an expert.' + sLineBreak
                            + sLineBreak
                            + 'The module you chose probably is not a valid expert DLL.';
var
  i: Integer;
  ExpertName: string;
  ExpertEntry: string;
begin
  if ExpertList = nil then
    Exit;

  for i := 0 to ExpertList.Count -1 do
  begin
    ExpertEntry := ExpertList[i];

    ExpertName := ChangeFileExt(ExtractFileName(ExpertEntry), '');
    case FExpertManager.AddExpertToRegistry(ExpertName, ExpertEntry) of
      aerOK:
        UpdateControlsState;
      aerDuplicate:
        MessageDlg(Format(SCouldNotAddExpertDupe, [ExpertEntry]), mtError, [mbOK], 0);
      aerNoExpert:
        MessageDlg(Format(SCouldNotAddExpertInvalid, [ExpertEntry]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfmExpertManager.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  AddExperts(FFileDrop.Files);
end;

procedure TfmExpertManager.LoadSettings;
var
  Settings: TGExpertsSettings;
  SaveKey: string;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    SaveKey := ConfigurationKey + FormSaveKey;
    Settings.LoadForm(Self, SaveKey);
    //lvExperts.Columns[0].Width := Settings.ReadInteger(SaveKey, 'Col1Width', lvExperts.Columns[0].Width);
    //lvExperts.Columns[1].Width := Settings.ReadInteger(SaveKey, 'Col2Width', lvExperts.Columns[1].Width);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmExpertManager.SaveSettings;
var
  Settings: TGExpertsSettings;
  SaveKey: string;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    SaveKey := ConfigurationKey + FormSaveKey;
    Settings.SaveForm(Self, SaveKey);
    //Settings.WriteInteger(SaveKey, 'Col1Width', lvExperts.Columns[0].Width);
    //Settings.WriteInteger(SaveKey, 'Col2Width', lvExperts.Columns[1].Width);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmExpertManager.FormResize(Sender: TObject);
begin
  if lvExperts.HandleAllocated then begin
    ListView_SetColumnWidth(lvExperts.Handle, 0, ColumnTextWidth);
    ListView_SetColumnWidth(lvExperts.Handle, 1, ColumnHeaderWidth);
  end;
end;

{ TExpertManagerExpert }

constructor TExpertManagerExpert.Create;
begin
  inherited Create;

  FInitialExperts := TStringList.Create;
  FInitialDisabledExperts := TStringList.Create;
  GetExperts(FInitialExperts, True);
  GetExperts(FInitialDisabledExperts, False);
end;

destructor TExpertManagerExpert.Destroy;
begin
  // Nothing has been added to the ancestor's SaveSettings
  //SaveSettings;

  FreeAndNil(FInitialExperts);
  FreeAndNil(FInitialDisabledExperts);

  inherited Destroy;
end;

function TExpertManagerExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Expert Manager...';
begin
  Result := SMenuCaption;
end;

class function TExpertManagerExpert.GetName: string;
begin
  Result := 'ExpertManager'; // Do not localize.
end;

procedure TExpertManagerExpert.Execute;
var
  Dlg: TfmExpertManager;
begin
  Dlg := TfmExpertManager.CreateWithManager(nil, Self);
  try
    SetFormIcon(Dlg);
    if Dlg.lvExperts.Items.Count > 0 then
    begin
      Dlg.lvExperts.Selected := Dlg.lvExperts.Items[0];
      Dlg.lvExperts.ItemFocused := Dlg.lvExperts.Items[0];
    end;
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

class procedure TExpertManagerExpert.RemoveExpertFromRegistry(const ExpertName: string; IsEnabled: Boolean);
var
  SectionName: string;
  ExpertSetting: TGExpertsSettings;
begin
  ExpertSetting := TGExpertsSettings.Create(GetExpertsRegKeyName(IsEnabled, SectionName));
  try
    ExpertSetting.DeleteKey(SectionName, ExpertName);
  finally
    FreeAndNil(ExpertSetting);
  end;
end;

class function TExpertManagerExpert.AddExpertToRegistry(const ExpertName, FileName: string): TAddExpertToRegistryResult;
var
  RegIni: TGExpertsSettings;
  ExpertList: TStringList;
  i: Integer;
begin
  {$IFOPT D+} SendDebug('Adding Expert: '+FileName); {$ENDIF}

  // set Result here because otherwise Delphi 7 complains (wrongly) that it might not be set
  Result := aerOK;

  // Make sure that this particular expert is not already loaded.
  ExpertList := TStringList.Create;
  try
    GetExperts(ExpertList, True); // Get enabled experts
    for i := 0 to ExpertList.Count - 1 do
    begin
      if ExpertList.Values[ExpertList.Names[i]] = FileName then
      begin
        Result := aerDuplicate;
        Exit;
      end;
    end;
  finally
    FreeAndNil(ExpertList);
  end;

  if not IsValidExpertDll(FileName) then begin
    Result := aerNoExpert;
    Exit;
  end;

  RegIni := TGExpertsSettings.Create(ConfigInfo.IdeRootRegistryKey);
  try
    RegIni.WriteString('Experts', ExpertName, FileName); // Do not localize.
  finally
    FreeAndNil(RegIni);
  end;
end;

// Function to move a value from one registry key to another
class procedure TExpertManagerExpert.MoveExpertRegistryKey(const ExpertName,
  FromBase, FromSection: string; const ToBase, ToSection: string);
var
  SettingFrom: TGExpertsSettings;
  SettingTo: TGExpertsSettings;

  ExpertPath: string;
begin
  SettingTo := nil;
  SettingFrom := TGExpertsSettings.Create(FromBase);
  try
    SettingTo := TGExpertsSettings.Create(ToBase);

    ExpertPath := SettingFrom.ReadString(FromSection, ExpertName, '');
    if ExpertPath <> '' then
      SettingTo.WriteString(ToSection, ExpertName, ExpertPath);

    SettingFrom.DeleteKey(FromSection, ExpertName);
  finally
    FreeAndNil(SettingTo);
    FreeAndNil(SettingFrom);
  end;
end;

class procedure TExpertManagerExpert.EnableExpertInRegistry(const ExpertName: string);
var
  BasePathEnabled: string;
  BasePathDisabled: string;

  SectionNameEnabled: string;
  SectionNameDisabled: string;
begin
  BasePathEnabled := GetExpertsRegKeyName(True, SectionNameEnabled);
  BasePathDisabled := GetExpertsRegKeyName(False, SectionNameDisabled);

  MoveExpertRegistryKey(ExpertName, BasePathDisabled, SectionNameDisabled,
    BasePathEnabled, SectionNameEnabled);
end;

class procedure TExpertManagerExpert.DisableExpertInRegistry(const ExpertName: string);
var
  BasePathEnabled: string;
  BasePathDisabled: string;

  SectionNameEnabled: string;
  SectionNameDisabled: string;
begin
  BasePathEnabled := GetExpertsRegKeyName(True, SectionNameEnabled);
  BasePathDisabled := GetExpertsRegKeyName(False, SectionNameDisabled);

  MoveExpertRegistryKey(ExpertName, BasePathEnabled, SectionNameEnabled,
    BasePathDisabled, SectionNameDisabled);
end;

class function TExpertManagerExpert.GetExpertsRegKeyName(IsEnabled: Boolean; var Section: string): string;
const
  // Do not localize.
  EnabledExpertRegistryLocation = 'Experts';
  DisabledExpertRegistryLocationSection = 'DisabledExperts';
begin
  if IsEnabled then
  begin
    Result := ConfigInfo.IdeRootRegistryKey;
    Section := EnabledExpertRegistryLocation;
  end
  else
  begin
    Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + ConfigurationKey;
    Section := DisabledExpertRegistryLocationSection;
  end;
end;

class procedure TExpertManagerExpert.GetExperts(Experts: TStrings; IsEnabled: Boolean);
var
  SectionName: string;
begin
  with TGExpertsSettings.Create(GetExpertsRegKeyName(IsEnabled, SectionName)) do
  try
    ReadSectionValues(SectionName, Experts);
  finally
    Free;
  end;
end;

procedure InstallGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer);
var
  Succeeded: Boolean;
begin
  try
    Succeeded := (TExpertManagerExpert.AddExpertToRegistry('GExperts', ThisDllName) = aerOK);
  except
    Succeeded := False;
  end;
  if Succeeded then
    ShowMessage(Format('GExperts has been registered for use in %s', [IDEEnglishName]))
  else begin
    ShowMessage(Format('GExperts could not be registered for use in %s.'#13#10
      + 'Starting the GExperts Expert Manager.', [IDEEnglishName]));
    ShowExpertManager;
  end;
end;

procedure RemoveGExperts(Handle: HWND; InstHandle: HINST; CmdLine: PAnsiChar; CmdShow: Integer);
var
  Succeeded: Boolean;
begin
  try
    TExpertManagerExpert.RemoveExpertFromRegistry('GExperts', true);
    Succeeded := True;
  except
    Succeeded := False;
  end;
  if Succeeded then
    ShowMessage('GExperts has been removed')
  else begin
    ShowMessage('GExperts could not be removed.'#13#10
      + 'Starting the GExperts Expert Manager.');
    ShowExpertManager;
  end;
end;

// Code used by ExpertManager executable - DLL exports

var
  ExpMgr: TExpertManagerExpert = nil;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
begin
  {$IFOPT D+}SendDebug('Showing expert manager'); {$ENDIF}
  ExpMgr := nil;
  InitSharedResources;
  try
    ExpMgr := TExpertManagerExpert.Create;
    ExpMgr.Execute(nil);
  finally
    FreeAndNil(ExpMgr);
    FreeSharedResources;
  end;
end;

function TExpertManagerExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TExpertManagerExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

{ TShowDisableCurrentMessage }

function TShowDisableCurrentMessage.GetMessage: string;
resourcestring
  SConfirmGExpertsDisable = 'You are about to disable or remove the GExperts DLL ' +
                            'that contains the expert you are currently using.' + sLineBreak +
                            sLineBreak +
                            'If you proceed, you will not be able to use this Expert Manager '+
                            'the next time the IDE is started.' + sLineBreak +
                            sLineBreak +
                            'Do you want to proceed?';
begin
  Result := SConfirmGExpertsDisable;
end;


procedure TfmExpertManager.actExpertDisableUpdate(Sender: TObject);
var
  GxExpertState: TGxExpertState;
  ActionIsEnabled: Boolean;
begin
  ActionIsEnabled := False;

  if lvExperts.Selected <> nil then
  begin
    GxExpertState := GetListItemState(lvExperts.Selected);
    if (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]) then
      ActionIsEnabled := True;
  end;

  (Sender as TCustomAction).Enabled := ActionIsEnabled;
end;

procedure TfmExpertManager.actExpertEnableUpdate(Sender: TObject);
var
  GxExpertState: TGxExpertState;
  ActionIsEnabled: Boolean;
begin
  ActionIsEnabled := False;

  if lvExperts.Selected <> nil then
  begin
    GxExpertState := GetListItemState(lvExperts.Selected);
    if (GxExpertState in [gesCurrentlyDisabled, gesNextTimeDisabled]) then
      ActionIsEnabled := True;
  end;

  (Sender as TCustomAction).Enabled := ActionIsEnabled;
end;

procedure TfmExpertManager.actExpertRemoveUpdate(Sender: TObject);
begin
  (Sender as TCustomAction).Enabled := (lvExperts.Selected <> nil);
end;

procedure TfmExpertManager.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
resourcestring
  SCurrentlyDisabled = 'Expert DLL is currently disabled and not loaded';
  SCurrentlyEnabled  = 'Expert DLL is currently enabled and active';
  SNextTimeEnabled   = 'Expert DLL will be active next time IDE is started';
  SNextTimeDisabled  = 'Expert DLL will be inactive next time IDE is started';

  function GetDescriptiveState(State: TGxExpertState): string;
  begin
    case State of
      gesCurrentlyDisabled: Result := SCurrentlyDisabled;
      gesCurrentlyEnabled:  Result := SCurrentlyEnabled;
      gesNextTimeDisabled:  Result := SNextTimeDisabled;
      gesNextTimeEnabled:   Result := SNextTimeEnabled;
    else
      Result := '';
    end;
  end;

var
  GxExpertState: TGxExpertState;
begin
  if lvExperts.Selected <> nil then
  begin
    GxExpertState := GetListItemState(lvExperts.Selected);
    StatusBar.SimpleText := GetDescriptiveState(GxExpertState);
  end
  else
    StatusBar.SimpleText := '';

  Handled := False;
end;

procedure TfmExpertManager.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // TODO 3 -oAnyone -cIssue: Closing with ESC doesn't undo any changes the user made
  if Key = VK_ESCAPE then
  begin
    actFileExit.Execute;
    Key := 0;
  end;
end;

procedure TfmExpertManager.FormShow(Sender: TObject);
begin
  // Works around a bug in Delphi 5 under XP where the menu paints white
  MainMenu.Images := GetSharedImageList;
end;

initialization
  RegisterGX_Expert(TExpertManagerExpert);
end.

