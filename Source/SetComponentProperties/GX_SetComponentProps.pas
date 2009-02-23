// Set Component Properties Expert
// Original Author: Robert Wachtel (rwachtel@gmx.de)

unit GX_SetComponentProps;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, ToolsAPI, GX_OtaUtils;

type
  TSetComponentPropsNotifier = class(TBaseIdeNotifier, IOTAIDENotifier50)
  private
    FOutputMessages: TStringList;
    procedure AddMessageToList(const sModuleFileName, sFormat: string; const Args: array of const);
    procedure CheckAndSetComponent(const ModuleFileName: string; Component: IOTAComponent);
    procedure CheckChildComponents(RootComponent: IOTAComponent; const ModuleFileName: string);
  protected
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); reintroduce; overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); reintroduce; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSetComponentPropsSettings = class(TObject)
  private
    FComponents: TStringList;
    FGxSetComponentPropsNotifier: TSetComponentPropsNotifier;
    FProperties: TStringList;
    FPropertyTypes: TStringList;
    FSimulate: Boolean;
    FValues: TStringList;
    FVerbose: Boolean;
    FOnlyOpenFiles: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNotifierToIDE: Boolean;
    class procedure FreeMe;
    class function GetInstance: TSetComponentPropsSettings;
    procedure RemoveNotifierFromIDE;
    property Components: TStringList read FComponents;
    property Properties: TStringList read FProperties;
    property PropertyTypes: TStringList read FPropertyTypes;
    property Simulate: Boolean read FSimulate write FSimulate;
    property Values: TStringList read FValues;
    property Verbose: Boolean read FVerbose write FVerbose;
    property OnlyOpenFiles: Boolean read FOnlyOpenFiles write FOnlyOpenFiles;
  end;

implementation

uses
  SysUtils, TypInfo,
  GX_Experts, Gx_GenericUtils, GX_ConfigurationInfo,
  GX_SetComponentPropsConfig, GX_SetComponentPropsStatus;

type
  TSetComponentPropsExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    function IsDefaultActive: Boolean; override;
  end;

var
  GxSetComponentPropsSettings: TSetComponentPropsSettings = nil;

{TSetComponentPropsExpert}

// Get an instance of the settings container (TSetComponentPropsSettings)
constructor TSetComponentPropsExpert.Create;
begin
  inherited;
  TSetComponentPropsSettings.GetInstance; // Get an instance of the settings container
end;

// Be sure to free the settings conatiner (TSetComponentPropsSettings)
destructor TSetComponentPropsExpert.Destroy;
begin
  Active := False; // Prevent re-creating TSetComponentPropsSettings later when setting Active=False
  TSetComponentPropsSettings.FreeMe;
  inherited;
end;

// When the menu item is clicked, open the configuration dialog
procedure TSetComponentPropsExpert.Click(Sender: TObject);
begin
  Configure;
end;

// Action taken when user clicks the Configure button on the Experts tab of menu item GExperts/GExperts Configuration...
procedure TSetComponentPropsExpert.Configure;
begin
  with TfmSetComponentPropsConfig.Create(nil) do
  begin
    try
      SetSettings;
      if ShowModal = mrOK then
        GetSettings;
    finally
      Release;
    end;
  end;
end;

// Returns the string displayed on the GExperts menu item
function TSetComponentPropsExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Set Component Properties...';
begin
  Result := SMenuCaption;
end;

// Used to determine the unique keyword used to save the active state and shortcut into the registry
class function TSetComponentPropsExpert.GetName: string;
begin
  Result := 'SetComponentProperties';
end;

// This expert should have a configure button in the configuration dialog
function TSetComponentPropsExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

// This expert does not have a visible menu item in the GExperts top level menu
function TSetComponentPropsExpert.HasMenuItem: Boolean;
begin
  Result := False;
end;

// Gets the expert settings from the registry
procedure TSetComponentPropsExpert.InternalLoadSettings(Settings: TGExpertsSettings);
var
  Instance: TSetComponentPropsSettings;
begin
  inherited;
  Instance := TSetComponentPropsSettings.GetInstance;
  Instance.Simulate := Settings.ReadBool(ConfigurationKey, 'Simulate', True);
  Instance.Verbose := Settings.ReadBool(ConfigurationKey, 'Verbose', True);
  Settings.ReadStrings(Instance.Components, ConfigurationKey + '\Components', 'Components');
  Settings.ReadStrings(Instance.Properties, ConfigurationKey + '\Properties', 'Properties');
  Settings.ReadStrings(Instance.Values, ConfigurationKey + '\Values', 'Values');
  Settings.ReadStrings(Instance.PropertyTypes, ConfigurationKey + '\Property Types', 'PropertyTypes');
  Assert((Instance.Components.Count = Instance.Properties.Count) and
    (Instance.Components.Count = Instance.Values.Count));
end;

// Saves the expert settings to the registry
procedure TSetComponentPropsExpert.InternalSaveSettings(Settings: TGExpertsSettings);
var
  Instance: TSetComponentPropsSettings;
begin
  inherited;
  Instance := TSetComponentPropsSettings.GetInstance;
  Settings.WriteBool(ConfigurationKey, 'Simulate', Instance.Simulate);
  Settings.WriteBool(ConfigurationKey, 'Verbose', Instance.Verbose);
  Settings.WriteStrings(Instance.Components, ConfigurationKey + '\Components', 'Components');
  Settings.WriteStrings(Instance.Properties, ConfigurationKey + '\Properties', 'Properties');
  Settings.WriteStrings(Instance.Values, ConfigurationKey + '\Values', 'Values');
  Settings.WriteStrings(Instance.PropertyTypes, ConfigurationKey + '\Property Types', 'PropertyTypes');
end;

// Called to clean up the expert when it is disabled at runtime or destroyed on shutdown.
procedure TSetComponentPropsExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited;
    if New then
      TSetComponentPropsSettings.GetInstance.AddNotifierToIDE
    else
      TSetComponentPropsSettings.GetInstance.RemoveNotifierFromIDE;
  end;
end;

{TGxSetComponentPropsSettings}

// Initialize all settings
constructor TSetComponentPropsSettings.Create;
begin
  inherited;
  FComponents := TStringList.Create;
  FProperties := TStringList.Create;
  FValues := TStringList.Create;
  FPropertyTypes := TStringList.Create;
  FSimulate := True;
  FVerbose := True;
  FOnlyOpenFiles := True;
  FGxSetComponentPropsNotifier := nil;
end;

// Free all lists and the notifier in the IDE
destructor TSetComponentPropsSettings.Destroy;
begin
  RemoveNotifierFromIDE;
  FreeAndNil(FComponents);
  FreeAndNil(FProperties);
  FreeAndNil(FValues);
  FreeAndNil(FPropertyTypes);
  inherited;
end;

// Register the notifier in the IDE
function TSetComponentPropsSettings.AddNotifierToIDE: Boolean;
begin
  if Assigned(FGxSetComponentPropsNotifier) then
    RemoveNotifierFromIDE;
  FGxSetComponentPropsNotifier := TSetComponentPropsNotifier.Create;
  Result := FGxSetComponentPropsNotifier.AddNotifierToIDE;
end;

// Class procedure to Free the settings container
class procedure TSetComponentPropsSettings.FreeMe;
begin
  FreeAndNil(GxSetComponentPropsSettings);
end;

// Class function to get a singleton instance of the settings container
class function TSetComponentPropsSettings.GetInstance: TSetComponentPropsSettings;
begin
  if not Assigned(GxSetComponentPropsSettings) then
    GxSetComponentPropsSettings := TSetComponentPropsSettings.Create;
  Result := GxSetComponentPropsSettings;
end;

// Remove the notifier from the IDE and set the var to nil
procedure TSetComponentPropsSettings.RemoveNotifierFromIDE;
begin
  if Assigned(FGxSetComponentPropsNotifier) then
  begin
    FGxSetComponentPropsNotifier.RemoveNotifierFromIDE;
    FGxSetComponentPropsNotifier := nil;
  end;
end;

{TSetComponentPropsNotifier}

// Create a list for the output messages
constructor TSetComponentPropsNotifier.Create;
begin
  inherited;
  FOutputMessages := TStringList.Create;
end;

// Free the output messages list before destroying
destructor TSetComponentPropsNotifier.Destroy;
begin
  FreeAndNil(FOutputMessages);
  inherited;
end;

// Add a message to the output message list
procedure TSetComponentPropsNotifier.AddMessageToList(const
  sModuleFileName, sFormat: string; const Args: array of const);
begin
  FOutputMessages.Add(sModuleFileName + '|' + Format(sFormat, Args));
end;

// After compilation, add any pending mesages to the message view
procedure TSetComponentPropsNotifier.AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
var
  sFileName, sMessage: string;
begin
  if (IsCodeInsight = False) and TSetComponentPropsSettings.GetInstance.Verbose then
  begin
    if FOutputMessages.Count > 0 then
    begin
      GxOtaWriteTitleMessage('Properties set before compilation by GExperts Set Component Properties:');
      if TSetComponentPropsSettings.GetInstance.Simulate then
        GxOtaWriteTitleMessage('Simulation Mode: No property changes are being made');
    end;
    while FOutputMessages.Count > 0 do
    begin
      sFileName := Copy(FOutputMessages[0], 1, Pos('|', FOutputMessages[0]) - 1);
      sMessage := Copy(FOutputMessages[0], Pos('|', FOutputMessages[0]) + 1, MaxInt);
      GxOtaWriteToolMessage(sFileName, sMessage, '', 0, 0);
      FOutputMessages.Delete(0);
    end;
  end;
end;

// Iterate all modules of the current project before compiling and
// look for components and properties to be set
procedure TSetComponentPropsNotifier.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
var
  ModuleInfo: IOTAModuleInfo;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
  IndexProjectModules: Integer;
  CurrentEditor: IOTAEditor;
  Settings: TSetComponentPropsSettings;
  FileName: string;
begin
  FOutputMessages.Clear;

  Settings := TSetComponentPropsSettings.GetInstance;
  if IsCodeInsight = False then
  begin
    try
      CurrentEditor := GxOtaGetCurrentEditor;
      if Settings.Verbose then
        TfmSetComponentPropsStatus.GetInstance.Show;

      // Iterate all modules of the current project and get forms and datamodules
      for IndexProjectModules := 0 to Pred(Project.GetModuleCount) do
      begin
        ModuleInfo := Project.GetModule(IndexProjectModules);
        FileName := ModuleInfo.FileName;
        if Settings.Verbose then
          TfmSetComponentPropsStatus.GetInstance.ProcessedFile := FileName;
        if (not IsDcp(FileName)) and (FileName <> '') then
        begin
          if Settings.OnlyOpenFiles then
          begin
            if not GxOtaIsFileOpen(FileName) then
              Continue;
          end;
          try
            Module := ModuleInfo.OpenModule;
            if Assigned(Module) then
            begin
              FormEditor := GxOtaGetFormEditorFromModule(Module);
              if Assigned(FormEditor) then
              begin
                RootComponent := FormEditor.GetRootComponent;
                if Assigned(RootComponent) then
                  CheckChildComponents(RootComponent, ModuleInfo.FileName);
              end;
            end;
          except
            on EFOpenError do
            begin
              // Ignore exceptions about non-existing modules
            end;
          else
            begin
              // Other exceptions are re-raised
              raise;
            end;
          end;
        end;
      end;
      if Assigned(CurrentEditor) then
        CurrentEditor.Show;
    finally
      TfmSetComponentPropsStatus.ReleaseMe;
    end;
  end;
end;

// Check a given component for properties to be set
procedure TSetComponentPropsNotifier.CheckAndSetComponent(const
  ModuleFileName: string; Component: IOTAComponent);
var
  NativeComponent: TComponent;
  IndexComponents: Integer;
  cName, cClass, cProperty, cValue: string;
  Settings: TSetComponentPropsSettings;
  PropType: TTypeKind;
  CurrentValue: string;
  NormalizedValue: string;
begin
  NativeComponent := GxOtaGetNativeComponent(Component);
  Settings := TSetComponentPropsSettings.GetInstance;
  if Assigned(NativeComponent) then
  begin
    IndexComponents := 0;
    while IndexComponents < Settings.Components.Count do
    begin
      cName := NativeComponent.Name;
      cClass := Settings.Components[IndexComponents];
      cProperty := Settings.Properties[IndexComponents];
      cValue := Settings.Values[IndexComponents];
      cValue := AnsiDequotedStr(cValue, #39);
      if (cValue= #39#39) then
        cValue := '';

      if InheritsFromClass(NativeComponent.ClassType, cClass) then
      begin
        PropType := Component.GetPropTypeByName(cProperty);
        if PropType = tkUnknown then
          AddMessageToList(ModuleFileName, 'Unknown property name %s for class %s', [cProperty, cClass])
        else
        begin
          try
            CurrentValue := GxOtaGetComponentPropertyAsString(Component, cProperty);
            NormalizedValue := GxOtaNormalizePropertyValue(Component, cProperty, cValue);
            if CurrentValue <> NormalizedValue then
            begin
              if Settings.Verbose then
                AddMessageToList(ModuleFileName, '%s %s: Setting %s to %s', [cClass, cName, cProperty, cValue]);

              if not Settings.Simulate then
                if (not GxOtaSetComponentPropertyAsString(Component, cProperty, cValue)) then
                  AddMessageToList(ModuleFileName, '%s %s: Setting %s to %s failed', [cClass, cName, cProperty, cValue]);
            end;
          except
            on E: Exception do
              AddMessageToList(ModuleFileName, '%s %s: Setting %s to %s failed with exception %s', [cClass, cName, cProperty, cValue, QuotedStr(E.Message)]);
          end;
        end;
      end;
      Inc(IndexComponents);
    end;
  end;
end;

// Iterate all child components of a given RootComponent
procedure TSetComponentPropsNotifier.CheckChildComponents(
  RootComponent: IOTAComponent; const ModuleFileName: string);
var
  IndexComponents: Integer;
  Component: IOTAComponent;
begin
  if Assigned(RootComponent) then
  begin
    // Iterate over the immediate child components
    for IndexComponents := 0 to Pred(RootComponent.GetComponentCount) do
    begin
      Component := RootComponent.GetComponent(IndexComponents);
      if Assigned(Component) then
        CheckAndSetComponent(ModuleFileName, Component);
    end;
  end;
end;

function TSetComponentPropsExpert.IsDefaultActive: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TSetComponentPropsExpert);

end.

