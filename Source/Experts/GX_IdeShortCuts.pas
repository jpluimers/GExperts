unit GX_IdeShortCuts;

{$I GX_CondDefine.inc}

// Original author: Stefan Hoffmeister <stefan.hoffmeister@econos.de>

interface

uses
  Classes, Controls, Forms, StdCtrls, Menus, ExtCtrls, ComCtrls,
  GX_Experts, GX_OtaUtils, GX_BaseForm;

type
  TfmIdeShortCuts = class(TfmBaseForm)
    MainMenu: TMainMenu;
    lblMenuStruc: TLabel;
    edtMenuStructure: TEdit;
    lblMenuItemName: TLabel;
    edtMenuItemName: TEdit;
    chkUseShortCut: TCheckBox;
    lblShortCut: TLabel;
    pnlControls: TPanel;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btOK: TButton;
    btCancel: TButton;
    btnHelp: TButton;
    hkShortCut: THotKey;
    procedure MenuClick(Sender: TObject);
    procedure ShortCutChange(Sender: TObject);
    procedure chkUseShortCutClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    procedure ReadFromRegistryCFG;
    procedure InitializeForm;
    procedure LoadSettings;
    procedure SaveSettings;
    function WindowConfigurationKey: string;
    function ShortCutConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TShortCutExpert = class(TGX_Expert)
  private
    FOldShortCuts: TStringList;
    FPackageNotifier: TBaseIdeNotifier;
    FUpdateTimer: TTimer;
    FUpdateCount: Integer;
    FProcessNotifier: TBaseDebuggerNotifier;

    procedure OnUpdateTimer(Sender: TObject);
    procedure ReadFromRegistryIDE;
    procedure ResetShortCuts;
    procedure InitializeShortCutExpert;
    procedure FinalizeShortCutExpert;
  protected
    procedure SetActive(New: Boolean); override;
    procedure QueueReinitializeShortcuts;
  public
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, ToolsAPI,
  GX_ConfigurationInfo, GX_GxUtils, GX_GenericUtils, ActnList, GX_dzVclUtils;

type
  TPackageLoadingNotifier = class(TBaseIdeNotifier)
  private
    FShortCutExpert: TShortCutExpert;
  public
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    constructor Create(const AClient: TShortCutExpert);
    destructor Destroy; override;
  end;

  TProcessNotifier = class(TBaseDebuggerNotifier)
  private
    FShortCutExpert: TShortCutExpert;
  public
    constructor Create(const AClient: TShortCutExpert);
    procedure ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); override;
    procedure ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); override;
    destructor Destroy; override;
  end;

function FindMenuByName(AMenuItem: TMenuItem; const Name: string): TMenuItem;
var
  j: Integer;
  TempItem: TMenuItem;
begin
  Assert(Assigned(AMenuItem));

  Result := nil;

  if Length(Name) = 0 then
    Exit;

  for j := 0 to AMenuItem.Count - 1 do
  begin
    TempItem := AMenuItem.Items[j];

    Assert(Assigned(TempItem));
    if SameText(TempItem.Name, Name) then
      Result := TempItem
    else
      Result := FindMenuByName(TempItem, Name);

    if Result <> nil then
      Break;
  end;
end;

{ TfmIdeShortCuts }

procedure TfmIdeShortCuts.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 23);
end;

procedure TfmIdeShortCuts.btOKClick(Sender: TObject);
var
  Settings: TGExpertsSettings;

  procedure WriteToRegistry(AMenuItem: TMenuItem);
  var
    j: Integer;
    ChildItem: TMenuItem;
  begin
    for j := 0 to AMenuItem.Count - 1 do
    begin
      ChildItem := AMenuItem.Items[j];

      if ChildItem.Checked then
        Settings.WriteInteger(ShortCutConfigurationKey, ChildItem.Name, ChildItem.ShortCut);

      WriteToRegistry(ChildItem);
    end;
  end;

var
  RegValues: TStrings;
begin
  // Store settings in registry.
  Settings := TGExpertsSettings.Create;
  try
    RegValues := TStringList.Create;
    try
      Settings.EraseSection(ShortCutConfigurationKey);
      WriteToRegistry(MainMenu.Items);
    finally
      FreeAndNil(RegValues);
    end;
  finally
    FreeAndNil(Settings);
  end;
  ModalResult := mrOk;
end;

constructor TfmIdeShortCuts.Create(AOwner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  LoadSettings;
end;

destructor TfmIdeShortCuts.Destroy;
begin
  SaveSettings;
  inherited;
end;

procedure TfmIdeShortCuts.InitializeForm;

  // Fill the form's menu bar with a copy of the IDE's menu bar
  procedure FillMenuBar(ASource, ADest: TMenuItem);
  var
    i: Integer;
    ANewItem: TMenuItem;
    ChildItem: TMenuItem;
  begin
    try
      ADest.Name := ASource.Name;
    except // Ignore menu items with duplicate names, such as with ModelMaker 6.2 for D7
      FreeAndNil(ADest);
      Exit;
    end;
    ADest.Caption := ASource.Caption;
    if False then // Change the conditional here to see the action name in the captions
    begin
      if Assigned(ASource.Action) then
      begin
        ADest.Caption := ADest.Caption + ' (';
        if Assigned(ASource.Action.Owner) then
          ADest.Caption := ADest.Caption + ASource.Action.Owner.Name + '.';
        ADest.Caption := ADest.Caption + ASource.Action.Name + ')';
      end;
    end;
    ADest.ShortCut := ASource.ShortCut;
    ADest.Tag := ASource.ShortCut;
    ADest.Clear;
    ADest.OnClick := MenuClick;
    for i := 0 to ASource.Count - 1 do
    begin
      ChildItem := ASource.Items[i];
      if ChildItem.Visible and (Length(Trim(ChildItem.Name)) > 0)
        and not SameText(ChildItem.Name, 'GExperts')
        and not SameText(ChildItem.Name, 'WindowsMenu')
        and not SameText(ChildItem.Name, 'GExpert_Menu') then
      begin
        ANewItem := TMenuItem.Create(ADest.GetParentMenu);
        ADest.Add(ANewItem);
        FillMenuBar(ASource.Items[i], ANewItem);
      end;
    end;
  end;

begin
  LoadSettings;
  MainMenu.Items.Clear;
  FillMenuBar(GxOtaGetIdeMainMenu.Items, MainMenu.Items);
  ReadFromRegistryCFG;
  Menu := MainMenu;
end;

procedure TfmIdeShortCuts.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.LoadForm(Self, WindowConfigurationKey, [fsSize, fsPosition]);
  finally
    FreeAndNil(Settings);
  end;
end;

// When the user chooses a menu item, show the item's name
procedure TfmIdeShortCuts.MenuClick(Sender: TObject);

  function MenuHierarchy(AMenuItem: TMenuItem): string;
  const
    Separator = ' | ';
  begin
    Result := StripHotKey(AMenuItem.Caption);
    if (AMenuItem.Parent <> nil) and
       (AMenuItem.Parent <> MainMenu.Items) then
    begin
      Result := MenuHierarchy(AMenuItem.Parent) + Separator + Result;
    end;
  end;

var
  ClickedItem: TMenuItem;
begin
  ClickedItem := (Sender as TMenuItem);
  edtMenuItemName.Text := ClickedItem.Name;
  edtMenuStructure.Text := MenuHierarchy(ClickedItem);
  chkUseShortCut.Enabled :=
    (Length(ClickedItem.Name) > 0) and (ClickedItem.Count = 0)
    and (ClickedItem.Name <> 'FileClosedFilesItem');
  chkUseShortCut.OnClick := nil;
  hkShortCut.OnChange := nil;
  try
    if TShortCut(ClickedItem.Tag) <> ClickedItem.ShortCut then
    begin
      chkUseShortCut.Checked := True;
      hkShortCut.Enabled := True;
      hkShortCut.HotKey := ClickedItem.ShortCut;
    end
    else
    begin
      chkUseShortCut.Checked := False;
      hkShortCut.Enabled := False;
      hkShortCut.HotKey := 0;
      hkShortCut.HotKey := ClickedItem.ShortCut; //FI:W508 - Assignment has side effects
    end;
  finally
    chkUseShortCut.OnClick := chkUseShortCutClick;
    hkShortCut.OnChange := ShortCutChange;
  end;
end;

// Read shortcut settings from the registry
// into our local configuration menu.
procedure TfmIdeShortCuts.ReadFromRegistryCFG;
var
  i: Integer;
  RegValues: TStrings;
  Settings: TGExpertsSettings;
  AMenuItem: TMenuItem;
  AMenuItemName: string;
begin
  RegValues := nil;
  Settings := TGExpertsSettings.Create;
  try
    RegValues := TStringList.Create;
    Settings.ReadSection(ShortCutConfigurationKey, RegValues);
    for i := 0 to RegValues.Count - 1 do
    begin
      AMenuItemName := RegValues[i];
      AMenuItem := FindMenuByName(MainMenu.Items, AMenuItemName);
      if AMenuItem <> nil then
      begin
        try
          AMenuItem.ShortCut := TShortCut(Settings.ReadInteger(ShortCutConfigurationKey, AMenuItemName, 0));
          AMenuItem.Checked := True;
        except
          on E: EConvertError do { ignore };
        end;
      end;
    end;
  finally
    FreeAndNil(RegValues);
    FreeAndNil(Settings);
  end;
end;

procedure TfmIdeShortCuts.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.SaveForm(Self, WindowConfigurationKey, [fsSize, fsPosition]);
  finally
    FreeAndNil(Settings);
  end;
end;

function TfmIdeShortCuts.ShortCutConfigurationKey: string;
begin
  Result := TShortCutExpert.ConfigurationKey;
end;

function TfmIdeShortCuts.WindowConfigurationKey: string;
begin
  Result := TShortCutExpert.ConfigurationKey + PathDelim + 'Window';
end;

procedure TfmIdeShortCuts.FormShow(Sender: TObject);
begin
  InitializeForm;
end;

procedure TfmIdeShortCuts.chkUseShortCutClick(Sender: TObject);
var
  AItem: TMenuItem;
begin
  hkShortCut.OnChange := nil;
  try
    if chkUseShortCut.Checked then
      hkShortCut.Enabled := True
    else
    begin
      hkShortCut.Enabled := False;
      AItem := FindMenuByName(MainMenu.Items, edtMenuItemName.Text);
      AItem.ShortCut := TShortCut(AItem.Tag);
      AItem.Checked := False;
    end;
  finally
    hkShortCut.OnChange := ShortCutChange;
  end;
end;

procedure TfmIdeShortCuts.ShortCutChange(Sender: TObject);
var
  AItem: TMenuItem;

  function ShortCutsAreEqual: Boolean;
  begin
    Result := (TShortCut(AItem.Tag) = hkShortCut.HotKey);
  end;

begin
  AItem := FindMenuByName(MainMenu.Items, edtMenuItemName.Text);
  if not Assigned(AItem) then
    raise Exception.Create('Menu item not found: ' + edtMenuItemName.Text);
  if ShortCutsAreEqual then
  begin
    AItem.ShortCut := TShortCut(AItem.Tag);
    AItem.Checked := False;
  end
  else
  begin
    AItem.ShortCut := hkShortCut.HotKey;
    AItem.Checked := True;
  end;
end;

{ TShortCutExpert }

procedure TShortCutExpert.Execute(Sender: TObject);
var
  Dlg: TfmIdeShortCuts;
begin
  Dlg := TfmIdeShortCuts.Create(nil);
  try
    // Restore old shortcuts - to show "original" settings.
    ResetShortCuts;
    SetFormIcon(Dlg);

    if Dlg.ShowModal = mrOk then
    begin
      // Apply new shortcut settings.  The form writes to the registry itself.
      ReadFromRegistryIDE;
      IncCallCount;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

destructor TShortCutExpert.Destroy;
begin
  FinalizeShortCutExpert;

  inherited;
end;

procedure TShortCutExpert.FinalizeShortCutExpert;
begin
  if Assigned(FUpdateTimer) then
    FUpdateTimer.Enabled := False;
  FreeAndNil(FUpdateTimer);
  if Assigned(FPackageNotifier) then
    FPackageNotifier.RemoveNotifierFromIDE;
  if Assigned(FProcessNotifier) then
    FProcessNotifier.RemoveNotifierFromIDE;

  // The IDE destroys the notifier for us (tested in D5/D7)
  FPackageNotifier := nil;
  FProcessNotifier := nil;

  // Restore old shortcut settings
  ResetShortCuts;

  FreeAndNil(FOldShortCuts);
end;

function TShortCutExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&IDE Menu Shortcuts...';
begin
  Result := SMenuCaption;
end;

class function TShortCutExpert.GetName: string;
begin
  Result := 'IDEMenuShortCuts'; // Do not localize.
end;

function TShortCutExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TShortCutExpert.InitializeShortCutExpert;
begin
  Assert(FOldShortCuts = nil);
  FOldShortCuts := TStringList.Create;
  FOldShortCuts.Sorted := True;
  FOldShortCuts.Duplicates := dupError;

  ReadFromRegistryIDE;

  FPackageNotifier := TPackageLoadingNotifier.Create(Self);
  FPackageNotifier.AddNotifierToIDE;

  FProcessNotifier := TProcessNotifier.Create(Self);
  FProcessNotifier.AddNotifierToIDE;

  FUpdateTimer := TTimer.Create(nil);
  FUpdateTimer.Enabled := False;
  FUpdateTimer.Interval := 5000;
  FUpdateTimer.OnTimer := OnUpdateTimer;
end;

procedure TShortCutExpert.QueueReinitializeShortcuts;
begin
  // Restart the timer delay before updating the shortcuts.
  // This prevents us from updating constantly during startup, etc.
  FUpdateTimer.Enabled := False;
  FUpdateCount := 0;
  FUpdateTimer.Enabled := True;
end;

// Read shortcut settings from the registry and apply to the menu
procedure TShortCutExpert.ReadFromRegistryIDE;
var
  i: Integer;
  RegValues: TStrings;
  Settings: TGExpertsSettings;
  AMenuItem: TMenuItem;
  AMenuItemName: string;
  OldShortCutIdx: Integer;
  OldShortCut: TShortCut;
  NewShortCut: TShortCut;
  IDEMainMenu: TMainMenu;
begin
  if Application.Terminated then
    Exit;

  RegValues := nil;
  {$IFOPT D+} SendDebug('Checking for and applying customized IDE shortcuts'); {$ENDIF}
  Settings := TGExpertsSettings.Create;
  try
    RegValues := TStringList.Create;
    IDEMainMenu := GxOtaGetIdeMainMenu;
    if not Assigned(IDEMainMenu) then
      Exit;

    Settings.ReadSection(ConfigurationKey, RegValues);
    {$IFOPT D+} if RegValues.Count > 0 then SendDebug('Setting shortcuts for ' + Trim(RegValues.Text)); {$ENDIF}
    for i := 0 to RegValues.Count - 1 do
    begin
      AMenuItemName := RegValues[i];
      {$IFOPT D+} SendDebug('Looking for ' + AMenuItemName); {$ENDIF}
      AMenuItem := FindMenuByName(IDEMainMenu.Items, AMenuItemName);
      if AMenuItem <> nil then
      begin
        {$IFOPT D+} SendDebug('Found ' + AMenuItemName); {$ENDIF}
        // Store the old shortcut in a list
        OldShortCut := AMenuItem.ShortCut;
        try
          NewShortCut := Settings.ReadInteger(ConfigurationKey, AMenuItemName, 0);
          AMenuItem.ShortCut := NewShortCut;
          if AMenuItem.Action is TCustomAction then
            (AMenuItem.Action as TCustomAction).Shortcut := NewShortCut;
          {$IFOPT D+} SendDebug('Set ShortCut to ' + ShortCutToText(AMenuItem.ShortCut)); {$ENDIF}

          Assert(Assigned(FOldShortCuts), 'FOldShortCuts not assigned');
          if not FOldShortCuts.Find(AMenuItemName, OldShortCutIdx) then // If it is not in list
            FOldShortCuts.AddObject(AMenuItemName, TObject(OldShortCut)); // store it
          {$IFOPT D+} SendDebug('Done adding item to FOldShortCuts'); {$ENDIF}
        except
          on E: EConvertError do { ignore };
        end;
      end;
    end;
  finally
    FreeAndNil(RegValues);
    FreeAndNil(Settings);
  end;
  {$IFOPT D+} SendDebug('Done setting IDE shortcuts'); {$ENDIF}
end;

procedure TShortCutExpert.ResetShortCuts;
var
  i: Integer;
  AMenuItem: TMenuItem;
  IDEMainMenu: TMainMenu;
  OldShortCut: TShortCut;
begin
  if Application.Terminated then
    Exit;
  IDEMainMenu := GxOtaGetIdeMainMenu;
  if not Assigned(IDEMainMenu) then
    Exit;
  if Assigned(FOldShortCuts) then
  begin
    for i := 0 to FOldShortCuts.Count - 1 do
    begin
      AMenuItem := FindMenuByName(IDEMainMenu.Items, FOldShortCuts.Strings[i]);
      if AMenuItem <> nil then
      begin
        OldShortCut := TShortCut(FOldShortCuts.Objects[i]);
        AMenuItem.ShortCut := OldShortCut;
        if (AMenuItem.Action is TCustomAction) then
          (AMenuItem.Action as TCustomAction).ShortCut := OldShortCut;
      end;
    end;
  end;
end;

procedure TShortCutExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);

    if New then
    begin
      InitializeShortCutExpert;
    end
    else
    begin
      // IDE ShortCuts is modal, so the form will not exist here
      // Remove the modified shortcuts and restore the defaults
      FinalizeShortCutExpert;
    end;
  end;
end;

procedure TShortCutExpert.OnUpdateTimer(Sender: TObject);
begin
  Inc(FUpdateCount);
  {$IFOPT D+} SendDebug('IDE shortcut update timer expired, calling ReadFromRegistryIDE.  Update Count: ' + IntToStr(FUpdateCount)); {$ENDIF}
  if FUpdateCount >= 3 then
    FUpdateTimer.Enabled := False;
  if Application.Terminated then
    Exit;
  ReadFromRegistryIDE;
  {$IFOPT D+} SendDebug('Done processing IDE shortcut updates'); {$ENDIF}
end;

{ TPackageLoadingNotifier }

constructor TPackageLoadingNotifier.Create(const AClient: TShortCutExpert);
begin
  inherited Create;

  Assert(Assigned(AClient));
  FShortCutExpert := AClient;
end;

destructor TPackageLoadingNotifier.Destroy;
begin
  FShortCutExpert.FPackageNotifier := nil;
  inherited;
end;

procedure TPackageLoadingNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // Re-assign shortcuts if a package (which may contain
  // experts with hotkeys) or project has been loaded.  BDS 2006 resets shortcuts
  // in lots of situations, but this detects most of them.
  if NotifyCode in [ofnPackageInstalled, ofnActiveProjectChanged] then
    FShortCutExpert.QueueReinitializeShortcuts;
end;

{ TProcessNotifier }

constructor TProcessNotifier.Create(const AClient: TShortCutExpert);
begin
  inherited Create;
  Assert(Assigned(AClient));
  FShortCutExpert := AClient;
end;

destructor TProcessNotifier.Destroy;
begin
  FShortCutExpert.FProcessNotifier := nil;
  inherited;
end;

procedure TProcessNotifier.ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  inherited;
  FShortCutExpert.QueueReinitializeShortcuts;
end;

procedure TProcessNotifier.ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  inherited;
  FShortCutExpert.QueueReinitializeShortcuts;
end;

initialization
  RegisterGX_Expert(TShortCutExpert);

end.

