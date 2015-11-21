unit GX_IdeEnhance;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, ComCtrls, Menus,
  GX_MultiLinePalette, GX_MultilineHost, GX_IdeFormEnhancer;

type
  TIdeEnhancements = class(TObject)
  private
    // Fonts
    FOIFont: TFont;
    FOldOIFont: TFont;
    FOIFontEnabled: Boolean;
    FOICustomFontNames: Boolean;
    // File saving
    FAutoSave: Boolean;
    FAutoSaveInterval: Integer;
    // Menus
    {$IFDEF VER150} // Delphi 7 only
    procedure SetCPTabSelectButton(Value: Boolean);
    procedure TabSelectButtonClick(Sender: TObject);
    {$ENDIF VER150}
    procedure SetAutoSave(const Value: Boolean);
    procedure SetAutoSaveInterval(const Value: Integer);
    procedure SetOIFont(Value: TFont);
    procedure SetOIFontEnabled(Value: Boolean);
    procedure OIFontChange(Sender: TObject);
    procedure SetOICustomFontNames(const Value: Boolean);
  private
    // Component palette
    FCPMultiLine: Boolean;
    FCPHotTracking: Boolean;
    FCPAsButtons: Boolean;
    FCPRaggedRight: Boolean;
    FCPScrollOpposite: Boolean;
    FCPFlatButtons: Boolean;
    FCPTabsInPopup: Boolean;
    FCPTabsInPopupAlphaSort: Boolean;
    FOldCPPopupEvent: TNotifyEvent;
    FCPFontEnabled: Boolean;
    FCPFont: TFont;
    FOldCPFont: TFont;

    FMultiLineTabDockHostManager: TGxMultiLineTabDockHostsManager;
    FMultiLineTabManager: TMultiLineTabManager;

    procedure InstallMultiLineComponentTabs;
    procedure RemoveMultiLineComponentTabs;
    procedure AddTabsToPopup(Sender: TObject);
    procedure DeleteCPPopupMenuItems(Popup: TPopupMenu);
    procedure SetActiveTab(Sender: TObject);
    procedure SetCPMultiLine(Value: Boolean);
    procedure SetCPAsButtons(Value: Boolean);
    procedure SetCPTabsInPopup(Value: Boolean);
    procedure SetCPTabsInPopupAlphaSort(Value: Boolean);
    procedure InstallMultiLineHostTabs;
    procedure RemoveMultiLineHostTabs;
    function GetDefaultMultiLineTabDockHost: Boolean;
    procedure SetDefaultMultiLineTabDockHost(const Value: Boolean);
    function GetMultiLineTabDockHost: Boolean;
    procedure SetMultiLineTabDockHost(const Value: Boolean);
    procedure SetCPFont(Value: TFont);
    procedure SetCPFontEnabled(Value: Boolean);
    procedure CPFontChange(Sender: TObject);
    procedure SetCPFlatButtons(const Value: Boolean);
    procedure SetCPRaggedRight(const Value: Boolean);
    procedure SetCPScrollOpposite(const Value: Boolean);
    procedure Remove;
    function ConfigurationKey: string;
    procedure SetEnhanceIDEForms(const Value: Boolean);
    function GetEnhanceIDEForms: Boolean;
    function GetEnhanceSearchPath: Boolean;
    procedure SetEnhanceSearchPath(const Value: Boolean);
    function GetEnhanceToolProperties: Boolean;
    procedure SetEnhanceToolProperties(const Value: Boolean);
    procedure SetEnhanceSearchPathAggressive(const Value: Boolean);
    function GetEnhanceSearchPathAggressive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure LoadSettings;
    procedure SaveSettings;

    // IDE
    property EnhanceIDEForms: Boolean read GetEnhanceIDEForms write SetEnhanceIDEForms;
    // Search path
    property EnhanceSearchPath: Boolean read GetEnhanceSearchPath write SetEnhanceSearchPath;
    property EnhanceSearchPathAggressive: Boolean read GetEnhanceSearchPathAggressive write SetEnhanceSearchPathAggressive;
    // Tool Options dialog
    property EnhanceToolProperties: Boolean read GetEnhanceToolProperties write SetEnhanceToolProperties;
    // Fonts
    property OIFontEnabled: Boolean read FOIFontEnabled write SetOIFontEnabled;
    property OIFont: TFont read FOIFont;
    property OICustomFontNames: Boolean read FOICustomFontNames write SetOICustomFontNames;
    // File saving
    property AutoSave: Boolean read FAutoSave write SetAutoSave;
    property AutoSaveInterval: Integer read FAutoSaveInterval write SetAutoSaveInterval;
    property CPFontEnabled: Boolean read FCPFontEnabled write SetCPFontEnabled;
    property CPFont: TFont read FCPFont;
    // Component palette
    property CPMultiLine: Boolean read FCPMultiLine write SetCPMultiLine;
    property CPHotTracking: Boolean read FCPHotTracking write FCPHotTracking;
    property CPAsButtons: Boolean read FCPAsButtons write SetCPAsButtons;
    property CPFlatButtons: Boolean read FCPFlatButtons write SetCPFlatButtons;
    property CPScrollOpposite: Boolean read FCPScrollOpposite write SetCPScrollOpposite;
    property CPRaggedRight: Boolean read FCPRaggedRight write SetCPRaggedRight;
    property CPTabsInPopup: Boolean read FCPTabsInPopup write SetCPTabsInPopup;
    property CPTabsInPopupAlphaSort: Boolean read FCPTabsInPopupAlphaSort write SetCPTabsInPopupAlphaSort;
    // Multi-line tab dock host
    property MultiLineTabDockHost: Boolean read GetMultiLineTabDockHost write SetMultiLineTabDockHost;
    property DefaultMultiLineTabDockHost: Boolean read GetDefaultMultiLineTabDockHost write SetDefaultMultiLineTabDockHost;
  end;

function IdeEnhancements: TIdeEnhancements;
procedure FreeIdeEnhancements;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  {$IFDEF MSWINDOWS} VCLEditors, {$ENDIF MSWINDOWS}
  {$IFDEF VER150} Controls, Buttons, {$ENDIF VER150}
  SysUtils, Forms,
  GX_GenericUtils, GX_GxUtils, GX_IdeUtils, GX_OtaUtils, GX_ConfigurationInfo, 
  GX_IdeSearchPathEnhancer, GX_IdeProjectOptionsEnhancer,
  GX_IdeToolPropertiesEnhancer;

{ TIdeEnhancements }

constructor TIdeEnhancements.Create;
begin
  {$IFOPT D+} SendDebug('TIdeEnhancements.Create'); {$ENDIF}
  inherited Create;

  if IsStandAlone then
    Exit;

  FOIFont := TFont.Create;
  FOIFont.OnChange := OIFontChange;

  FCPFont := TFont.Create;
  FCPFont.OnChange := CPFontChange;
end;

procedure TIdeEnhancements.Initialize;
begin
  Assert(Application.MainForm <> nil, 'No MainForm found');

  {$IFOPT D+} SendDebug('Installing IDE Enhancements and loading settings'); {$ENDIF}
  LoadSettings;
  {$IFOPT D+} SendDebug('Loaded IDE Enhancement settings'); {$ENDIF}

  if CPMultiLine then
    InstallMultiLineComponentTabs;
end;

procedure TIdeEnhancements.Remove;
begin
  EnhanceIDEForms := False;
  // MultiLine component palette
  CPMultiLine := False;
  CPAsButtons := False;
  CPHotTracking := False;
  CPTabsInPopup := False;
  CPTabsInPopupAlphaSort := False;
  // Fonts
  CPFontEnabled := False;
  RemoveMultiLineComponentTabs;
  RemoveMultiLineHostTabs;
  OIFontEnabled := False;
  // Don't call SaveSettings after this point
end;

procedure TIdeEnhancements.SetEnhanceIDEForms(const Value: Boolean);
begin
  if Value then
    TIDEFormEnhancements.SetEnabled(True)
  else
    TIDEFormEnhancements.SetEnabled(False);
end;

procedure TIdeEnhancements.SetEnhanceSearchPath(const Value: Boolean);
begin
  TGxIdeSearchPathEnhancer.SetEnabled(Value);
  TGxIdeProjectOptionsEnhancer.SetEnabled(Value);
end;

procedure TIdeEnhancements.SetEnhanceSearchPathAggressive(const Value: Boolean);
begin
  TGxIdeSearchPathEnhancer.SetAggressive(Value);
end;

procedure TIdeEnhancements.SetEnhanceToolProperties(const Value: Boolean);
begin
  TGxIdeToolPropertiesEnhancer.SetEnabled(Value);
end;

destructor TIdeEnhancements.Destroy;
begin
  if IsStandAlone then
    Exit;

  Remove;

  FreeAndNil(FOIFont);
  FreeAndNil(FCPFont);

  inherited Destroy;
end;

procedure TIdeEnhancements.LoadSettings;
var
  Settings: TGExpertsSettings;
  Key: string;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  // do not localize any of the below items
  Settings := TGExpertsSettings.Create;
  with Settings do
    try
      EnhanceIDEForms := ReadBool(ConfigurationKey, 'EnhanceIDEForms', False);
      EnhanceSearchPath := ReadBool(ConfigurationKey, 'EnhanceSearchPath', False);
      EnhanceSearchPathAggressive := ReadBool(ConfigurationKey, 'EnhanceSearchPathAggressive', False);
      EnhanceToolProperties := ReadBool(ConfigurationKey, 'EnhanceToolProperties', False);
      // File saving
      AutoSave := ReadBool(ConfigurationKey, 'AutoSave', False);
      AutoSaveInterval := ReadInteger(ConfigurationKey, 'AutoSaveInterval', 5);
      // Fonts
      Key := AddSlash(ConfigurationKey);
      RegLoadFont(Settings, Key + 'OIFont', OIFont);
      OIFontEnabled := ReadBool(ConfigurationKey, 'EnableOIFont', False);
      OICustomFontNames := ReadBool(ConfigurationKey, 'OICustomFontNames', False);

      // Component palette
      CPFontEnabled := ReadBool(ConfigurationKey, 'EnableCPFont', False);
      RegLoadFont(Settings, Key + 'CPFont', CPFont);
      CPMultiLine := ReadBool(ConfigurationKey, 'CPMultiLine', False);
      CPScrollOpposite := ReadBool(ConfigurationKey, 'CPScrollOpposite', False);
      CPRaggedRight := ReadBool(ConfigurationKey, 'CPRaggedRight', False);
      CPFlatButtons := ReadBool(ConfigurationKey, 'CPFlatButtons', False);
      CPAsButtons := ReadBool(ConfigurationKey, 'CPAsButtons', False);
      CPTabsInPopup := ReadBool(ConfigurationKey, 'CPTabsInPopup', False);
      CPTabsInPopupAlphaSort := ReadBool(ConfigurationKey, 'CPTabsInPopupAlphaSort', False);
      CPHotTracking := ReadBool(ConfigurationKey, 'CPHotTracking', False);

      // MultiLine tab dock host
      MultiLineTabDockHost := ReadBool(ConfigurationKey, 'MultiLineTabDockHost', False);
      DefaultMultiLineTabDockHost := ReadBool(ConfigurationKey, 'DefaultMultiLineTabDockHost', True);
    finally
      FreeAndNil(Settings);
    end;
end;

procedure TIdeEnhancements.SaveSettings;
var
  Settings: TGExpertsSettings;
  Key: string;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  // do not localize any of the below items
  Settings := TGExpertsSettings.Create;
  with Settings do
    try
      WriteBool(ConfigurationKey, 'EnhanceIDEForms', EnhanceIDEForms);
      WriteBool(ConfigurationKey, 'EnhanceSearchPath', EnhanceSearchPath);
      WriteBool(ConfigurationKey, 'EnhanceSearchPathAggressive', EnhanceSearchPathAggressive);
      WriteBool(ConfigurationKey, 'EnhanceToolProperties', EnhanceToolProperties);

      // File saving
      WriteBool(ConfigurationKey, 'AutoSave', AutoSave);
      WriteInteger(ConfigurationKey, 'AutoSaveInterval', AutoSaveInterval);
      // Fonts
      WriteBool(ConfigurationKey, 'EnableOIFont', OIFontEnabled);
      WriteBool(ConfigurationKey, 'OICustomFontNames', OICustomFontNames);

      Key := AddSlash(ConfigurationKey);
      RegSaveFont(Settings, Key + 'OIFont', OIFont);

      // Component palette
      RegSaveFont(Settings, Key + 'CPFont', CPFont);
      WriteBool(ConfigurationKey, 'EnableCPFont', CPFontEnabled);
      WriteBool(ConfigurationKey, 'CPTabsInPopupAlphaSort', CPTabsInPopupAlphaSort);
      WriteBool(ConfigurationKey, 'CPTabsInPopup', CPTabsInPopup);
      WriteBool(ConfigurationKey, 'CPMultiLine', CPMultiLine);
      WriteBool(ConfigurationKey, 'CPScrollOpposite', CPScrollOpposite);
      WriteBool(ConfigurationKey, 'CPRaggedRight', CPRaggedRight);
      WriteBool(ConfigurationKey, 'CPHotTracking', CPHotTracking);
      WriteBool(ConfigurationKey, 'CPAsButtons', CPAsButtons);
      WriteBool(ConfigurationKey, 'CPFlatButtons', CPFlatButtons);

      // MultiLine tab dock host
      WriteBool(ConfigurationKey, 'MultiLineTabDockHost', MultiLineTabDockHost);
      WriteBool(ConfigurationKey, 'DefaultMultiLineTabDockHost', DefaultMultiLineTabDockHost);
    finally
      FreeAndNil(Settings);
    end;
end;

procedure TIdeEnhancements.AddTabsToPopup(Sender: TObject);
var
  CPPopupMenu: TPopupMenu;

  procedure AddPopupMenuItems;
  var
    StartInsertingAt: Integer;
    i: Integer;
    Menu: TMenuItem;
    TabNames: TStringList;
    TabControl: TTabControl;
  begin
    Menu := TMenuItem.Create(nil);
    Menu.Caption := '-';
    Menu.Tag := -1;
    Menu.Name := 'GX_PopupSeparator';
    CPPopupMenu.Items.Add(Menu);

    StartInsertingAt := CPPopupMenu.Items.Count;

    TabControl := GetComponentPaletteTabControl;
    if TabControl <> nil then
    begin
      TabNames := TStringList.Create;
      try
        for i := 0 to TabControl.Tabs.Count - 1 do
          TabNames.AddObject(TabControl.Tabs[i], TObject(i));
        if CPTabsInPopupAlphaSort then
          TabNames.Sort;
        for i := 0 to TabControl.Tabs.Count - 1 do
        begin
          Menu := TMenuItem.Create(nil);
          Menu.Caption := TabNames[i];
          Menu.Tag := -1;
          Menu.Name := 'GX_Palette' + IntToStr(Integer(TabNames.Objects[i]));
          Menu.RadioItem := True;
          Menu.GroupIndex := 99;
          Menu.Checked := Integer(TabNames.Objects[i]) = TabControl.TabIndex;
          Menu.OnClick := SetActiveTab;
          // This allows a max of 20 tabs per column.  Not perfect, but
          // still nicer than menu items disappearing off the screen.
          if (i > 0) and ((StartInsertingAt + i - 1) mod 20 = 0) then
            Menu.Break := mbBarBreak;
          CPPopupMenu.Items.Add(Menu)
        end;
      finally
        FreeAndNil(TabNames);
      end;
    end;
  end;

begin
  if (Sender = nil) or (not (Sender is TPopupMenu)) then
    Exit;
  CPPopupMenu := TPopupMenu(Sender);
  DeleteCPPopupMenuItems(CPPopupMenu);
  if Assigned(FOldCPPopupEvent) then
    FOldCPPopupEvent(Sender);
  AddPopupMenuItems;
end;

procedure TIdeEnhancements.DeleteCPPopupMenuItems(Popup: TPopupMenu);
var
  i: Integer;
  Menu: TMenuItem;
begin
  i := 0;
  while i <= Popup.Items.Count - 1 do
  begin
    if Popup.Items[i].Tag = -1 then
    begin
      Menu := Popup.Items[i];
      Popup.Items.Delete(i);
      FreeAndNil(Menu);
    end
    else
      Inc(i);
  end;
end;

procedure TIdeEnhancements.SetActiveTab(Sender: TObject);
var
  TabControl: TTabControl;
  Tab: string;
  i: Integer;
begin
  TabControl := GetComponentPaletteTabControl;
  if TabControl <> nil then
  begin
    Tab := TMenuItem(Sender).Caption;

    // Compensate for AutoHotKeys
    Tab := StringReplace(Tab, '&', '', [rfReplaceAll]);

    for i := 0 to TabControl.Tabs.Count - 1 do
      if TabControl.Tabs[i] = Tab then
      begin
        TabControl.TabIndex := i;
        TabControl.OnChange(TabControl);
        Break;
      end;
  end;
end;

procedure TIdeEnhancements.SetAutoSave(const Value: Boolean);
begin
  // Do something here
  FAutoSave := Value;
end;

procedure TIdeEnhancements.SetAutoSaveInterval(const Value: Integer);
begin
  // Do something here
  FAutoSaveInterval := Value;
end;

{ --- Window menu enhancement --- }

procedure TIdeEnhancements.SetCPMultiLine(Value: Boolean);
var
  CPTabControl: TTabControl;
begin
  {$IFOPT D+} SendDebug('Setting multiline palette to ' + BooleanText(Value)); {$ENDIF}
  if FCPMultiLine <> Value then
  begin
    FCPMultiLine := Value;
    CPTabControl := GetComponentPaletteTabControl;
    if CPTabControl = nil then
    begin
      {$IFOPT D+} SendDebug('Unable to reset OldCPResizeHandler (no tab control)'); {$ENDIF}
      Exit;
    end;
    if FCPMultiLine then
      InstallMultiLineComponentTabs
    else
      RemoveMultiLineComponentTabs;
  end;
end;

procedure TIdeEnhancements.SetCPFlatButtons(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPFlatButtons <> Value then
  begin
    FCPFlatButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    if CPAsButtons then
    begin
      if FCPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end;
  end;
end;

procedure TIdeEnhancements.SetCPAsButtons(Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPAsButtons <> Value then
  begin
    FCPAsButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    {$IFOPT D+} SendDebug('Removing CP Buttons'); {$ENDIF}
    if Value then
    begin
      if CPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end
    else
      TabControl.Style := tsTabs;
  end;
end;

procedure TIdeEnhancements.SetCPRaggedRight(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPRaggedRight <> Value then
  begin
    FCPRaggedRight := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    TabControl.RaggedRight := FCPRaggedRight;
  end;
end;

procedure TIdeEnhancements.SetCPScrollOpposite(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  TabControl := GetComponentPaletteTabControl;
  if Assigned(TabControl) then
  begin
    if Value <> TabControl.ScrollOpposite then
      TabControl.ScrollOpposite := Value;
    FCPScrollOpposite := TabControl.ScrollOpposite;
  end;
end;

procedure TIdeEnhancements.SetCPTabsInPopupAlphaSort(Value: Boolean);
begin
  if FCPTabsInPopupAlphaSort <> Value then
    FCPTabsInPopupAlphaSort := Value;
end;

{$IFDEF VER150}
procedure TIdeEnhancements.TabSelectButtonClick(Sender: TObject);
var
  Button: TSpeedButton;
  MainForm: TCustomForm;
begin
  if Sender is TSpeedButton then
  begin
    Button := TSpeedButton(Sender);
    MainForm := GetIdeMainForm;
    if not Assigned(Button.PopupMenu) and Assigned(MainForm) then
    begin
      Button.PopupMenu := TPopupMenu.Create(MainForm);
      Button.PopupMenu.OnPopup := AddTabsToPopup;
    end;
    if Assigned(Button.PopupMenu) then
      Button.PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TIdeEnhancements.SetCPTabSelectButton(Value: Boolean);
const
  ButtonName = 'GXTabSelectButton';
var
  MainForm: TCustomForm;
  Button: TSpeedButton;
begin
  if Value then
  begin // Create CP select button
    MainForm := GetIdeMainForm;
    if not Assigned(MainForm) then
      Exit;
    if MainForm.FindComponent(ButtonName) <> nil then
      Exit;

    Button := TSpeedButton.Create(MainForm);
    Button.Align := alRight;
    Button.Width := 18;
    Button.Caption := '...';
    Button.Name := ButtonName;
    Button.Parent := GetComponentPaletteTabControl;
    Button.OnClick := TabSelectButtonClick;
  end
  else begin // Remove CP select button
    MainForm := GetIdeMainForm;
    if not Assigned(MainForm) then
      Exit;

    Button := TSpeedButton(MainForm.FindComponent(ButtonName));
    FreeAndNil(Button);
  end;
end;

procedure TIdeEnhancements.SetCPTabsInPopup(Value: Boolean);
begin
  if FCPTabsInPopup <> Value then
  begin
    FCPTabsInPopup := Value;
    SetCPTabSelectButton(Value);
  end;
end;

{$ELSE not VER150}

procedure TIdeEnhancements.SetCPTabsInPopup(Value: Boolean);
var
  CPPopupMenu: TPopupMenu;
begin
  if FCPTabsInPopup <> Value then
  begin
    FCPTabsInPopup := Value;
    CPPopupMenu := GetComponentPalettePopupMenu;
    if CPPopupMenu = nil then
      Exit;
    if Value then
    begin
      FOldCPPopupEvent := CPPopupMenu.OnPopup;
      CPPopupMenu.OnPopup := AddTabsToPopup;
    end
    else
      CPPopupMenu.OnPopup := FOldCPPopupEvent;
  end;
end;

{$ENDIF not VER150}

procedure TIdeEnhancements.SetOIFont(Value: TFont);
var
  OIForm: TCustomForm;
begin
  OIForm := GetObjectInspectorForm;
  if OIForm <> nil then
  begin
    if FOldOIFont = nil then
    begin
      FOldOIFont := TFont.Create;
      FOldOIFont.Assign(OIForm.Font);
    end;
    OIForm.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetOIFontEnabled(Value: Boolean);
begin
  if FOIFontEnabled <> Value then
  begin
    FOIFontEnabled := Value;
    if Value then
      SetOIFont(OIFont)
    else if FOldOIFont <> nil then
    begin
      SetOIFont(FOldOIFont);
      FreeAndNil(FOldOIFont);
    end;
  end;
end;

procedure TIdeEnhancements.OIFontChange(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('OI font changed'); {$ENDIF}
  if OIFontEnabled then
    SetOIFont(OIFont);
end;

procedure TIdeEnhancements.SetCPFont(Value: TFont);
var
  CPTabControl: TTabControl;
begin
  CPTabControl := GetComponentPaletteTabControl;
  if CPTabControl <> nil then
  begin
    if FOldCPFont = nil then
    begin
      FOldCPFont := TFont.Create;
      FOldCPFont.Assign(CPTabControl.Font);
    end;
    CPTabControl.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetCPFontEnabled(Value: Boolean);
begin
  if FCPFontEnabled <> Value then
  begin
    FCPFontEnabled := Value;
    if Value then
      SetCPFont(CPFont)
    else if FOldCPFont <> nil then
    begin
      SetCPFont(FOldCPFont);
      FreeAndNil(FOldCPFont);
    end;
  end;
end;

procedure TIdeEnhancements.CPFontChange(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('CP font changed'); {$ENDIF}
  if CPFontEnabled then
    SetCPFont(CPFont);
end;

procedure TIdeEnhancements.InstallMultiLineComponentTabs;
begin
  if GetComponentPaletteTabControl = nil then
    Exit;

  if FMultiLineTabManager = nil then
    FMultiLineTabManager := TMultiLineTabManager.Create(GetIdeMainForm);
end;

procedure TIdeEnhancements.RemoveMultiLineComponentTabs;
begin
  FreeAndNil(FMultiLineTabManager);
end;

procedure TIdeEnhancements.SetDefaultMultiLineTabDockHost(const Value: Boolean);
begin
  GX_MultilineHost.DefaultToMultiLine := Value;
end;

function TIdeEnhancements.GetDefaultMultiLineTabDockHost: Boolean;
begin
  Result := GX_MultilineHost.DefaultToMultiLine;
end;

function TIdeEnhancements.GetEnhanceIDEForms: Boolean;
begin
  Result := TIDEFormEnhancements.GetEnabled;
end;

function TIdeEnhancements.GetEnhanceSearchPath: Boolean;
begin
  Result := TGxIdeSearchPathEnhancer.GetEnabled;
end;

function TIdeEnhancements.GetEnhanceSearchPathAggressive: Boolean;
begin
  Result := TGxIdeSearchPathEnhancer.GetAggressive;
end;

function TIdeEnhancements.GetEnhanceToolProperties: Boolean;
begin
   Result := TGxIdeToolPropertiesEnhancer.GetEnabled;
end;

function TIdeEnhancements.GetMultiLineTabDockHost: Boolean;
begin
  Result := (FMultiLineTabDockHostManager <> nil);
end;

procedure TIdeEnhancements.SetMultiLineTabDockHost(const Value: Boolean);
begin
  if Value then
    InstallMultiLineHostTabs
  else
    RemoveMultiLineHostTabs;
end;

procedure TIdeEnhancements.InstallMultiLineHostTabs;
begin
  if MultilineTabDockHostPossible and (FMultiLineTabDockHostManager = nil) then
    FMultiLineTabDockHostManager := TGxMultiLineTabDockHostsManager.Create;
end;

procedure TIdeEnhancements.RemoveMultiLineHostTabs;
begin
  FreeAndNil(FMultiLineTabDockHostManager);
end;

procedure TIdeEnhancements.SetOICustomFontNames(const Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  FontNamePropertyDisplayFontNames := Value;
  {$ENDIF MSWINDOWS}
  FOICustomFontNames := Value;
end;

function TIdeEnhancements.ConfigurationKey: string;
begin
  Result := 'IDEEnhancements';
end;

// Internal Singleton management code

var
  PrivateIdeEnhancements: TIdeEnhancements = nil;
  CanCreate: Boolean = True;

function IdeEnhancements: TIdeEnhancements;
begin
  {$IFOPT D+} SendDebug('Calling IdeEnhancements'); {$ENDIF D+}
  Assert(CanCreate, 'CanCreate not set');

  if PrivateIdeEnhancements = nil then
    PrivateIdeEnhancements := TIdeEnhancements.Create;

  Result := PrivateIdeEnhancements;
end;

procedure FreeIdeEnhancements;
begin
  {$IFOPT D+} SendDebug('FreeIdeEnhancements'); {$ENDIF D+}
  CanCreate := False;

  FreeAndNil(PrivateIdeEnhancements);
end;

initialization
  {$IFOPT D+} SendDebug('Initializing IDE enhancements unit'); {$ENDIF D+}

finalization
  FreeIdeEnhancements;
end.

