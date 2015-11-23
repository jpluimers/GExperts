unit GX_EditorEnhancements;

{$I GX_CondDefine.inc}

interface

uses
  GX_EditorFormServices, GX_ConfigurationInfo, Classes, Controls,
  GX_HideNavbar;

type
  TConfigurationSheet = (cfgEditor, cfgToolBar, cfgEditorExperts);

  { TEditorEnhancements needs to descend from TComponent as we install
    a Notification hook that informs the object of disappearing editor
    toolbars. }
type
  TEditorEnhancements = class(TComponent)
  private
    FHideNavigationToolbarExpert: IHideNavigationToolbarExpert;
    FToolBarAlign: TAlign;
    FToolBarActionsList: TStringList;
    FToolBarList: TList;
    FButtons: Boolean;
    FButtonsFlat: Boolean;
    FMultiLine: Boolean;
    FHotTrack: Boolean;
    FToolBarVisible: Boolean;
    FEnabled: Boolean;
    FMiddleButtonClose: Boolean;
    FHideNavbar: Boolean;
    procedure AddToolBar;
    procedure RemoveToolBar;
    procedure EditorFormListener(EventCode: TEditFormEventCode; EditFormProxy: IGxEditFormProxy);
    procedure SetToolBarActionsList(const Value: TStrings);
    function GetToolBarActionsList: TStrings;
    procedure LoadSettings;
    procedure LoadToolBarSettings(const Settings: TGExpertsSettings);
    procedure SaveToolBarSettings(const Settings: TGExpertsSettings);

    function ShowConfigurationDialog(ActiveSheet: TConfigurationSheet): TModalResult;
    procedure SetEnabled(const Value: Boolean);
    procedure Install;
    procedure Remove;
    procedure SetHideNavbar(const _Value: Boolean);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveSettings;
    property Enabled: Boolean read FEnabled write SetEnabled;

    // ToolBar code
    procedure ApplyToolBarSettings;
    property ToolBarActionsList: TStrings read GetToolBarActionsList write SetToolBarActionsList;
    property ToolBarAlign: TAlign read FToolBarAlign write FToolBarAlign;
    property ToolBarVisible: Boolean read FToolBarVisible write FToolBarVisible;

    function ShowToolBarConfigurationDialog: TModalResult;
    function ShowEditorConfigurationDialog: TModalResult;

    // Tab properties
    property MultiLine: Boolean read FMultiLine write FMultiLine;
    property MiddleButtonClose: Boolean read FMiddleButtonClose write FMiddleButtonClose;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property Buttons: Boolean read FButtons write FButtons;
    property ButtonsFlat: Boolean read FButtonsFlat write FButtonsFlat;

    property HideNavbar: Boolean read FHideNavbar write SetHideNavbar;
  end;

function EditorEnhancements: TEditorEnhancements;
procedure FreeEditorEnhancements;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, GX_ActionBroker, {$ENDIF}
  SysUtils, ExtCtrls, ComCtrls, Forms,
  GX_GenericUtils, GX_OtaUtils, GX_Configure, GX_Toolbar, GX_ToolbarConfig,
  GX_ToolBarDropDown, GX_GxUtils, GX_IdeUtils;

const
  ToolBarActionsKey = 'EditorEnhancements' +PathDelim+ 'ToolBarActions';
  ToolBarActionsCountValue = 'Count';
  ToolBarActionsActionPrefixValue = 'Action';

{ TEditorEnhancements }

procedure TEditorEnhancements.AddToolBar;
const
  GX_ToolBarComponentName = 'GX_ToolBar'; // Do not localize.
var
  i: Integer;
  EditorFormServices: IGxEditorFormServices;
  EditorFormProxy: IGxEditFormProxy;
  EditorForm: TCustomForm;
  ToolBar: TGXToolBar;

  EditControl: TControl;
  EditControlPanel: TPanel;
begin
  // Create list of toolbars on demand
  if not Assigned(FToolBarList) then
    FToolBarList := TList.Create;
  EditControlPanel := nil;

  EditorFormServices := GxEditorFormServices;
  for i := 0 to EditorFormServices.EditorFormProxyCount - 1 do
  begin
    EditorFormProxy := EditorFormServices.EditorFormProxies[i];
    Assert(Assigned(EditorFormProxy));

    EditorForm := EditorFormProxy.EditorForm;
    Assert(Assigned(EditorForm));

    if EditorFormProxy.IsSourceEditor and (not Assigned(EditorForm.FindComponent(GX_ToolBarComponentName))) then
    begin
      ToolBar := TGXToolBar.Create(EditorForm);
      FToolBarList.Add(ToolBar);
      ToolBar.FreeNotification(Self);

      ToolBar.Align := FToolBarAlign;

      ToolBar.Name := GX_ToolBarComponentName;

      EditControl := EditorFormProxy.EditControl;
      Assert(Assigned(EditControl));

      if RunningDelphi8OrGreater then begin
        // There are toolbar focus problems when the Parent is EditorPanel in D8+
        if Assigned(EditControl.Parent) then
          EditControlPanel := EditControl.Parent.Parent as TPanel; // CodePanel
      end
      else
        EditControlPanel := EditControl.Parent as TPanel; // EditorPanel

      if not Assigned(EditControlPanel) then
        Exit;

      ToolBar.Parent := EditControlPanel;
      ToolBar.Images := GxOtaGetIdeImageList;

      ToolBar.BringToFront;

      ToolBar.RecreateToolBarButtons;

      // Finally make the toolbar visible depending on settings
      ToolBar.Visible := ToolBarVisible;

      ToolBar.SetEditorControls;
    end;
  end;
end;

procedure TEditorEnhancements.ApplyToolBarSettings;
var
  i: Integer;
  GExpertsToolBar: TGXToolBar;
begin
  // Immediately exit if there are no toolbars around
  if not Assigned(FToolBarList) then
    Exit;

  {$IFOPT D+} SendDebug(Format('Applying settings to %d toolbars', [FToolBarList.Count])); {$ENDIF}
  for i := 0 to FToolBarList.Count - 1 do
  begin
    GExpertsToolBar := FToolBarList.Items[i];

    {$IFOPT D+} SendDebug('Hiding GExpertsToolBar'); {$ENDIF}
    GExpertsToolBar.Visible := False;

    {$IFOPT D+} SendDebug('Aligning GExpertsToolBar'); {$ENDIF}
    GExpertsToolBar.Align := FToolBarAlign;
    GExpertsToolBar.RecreateToolBarButtons;

    {$IFOPT D+} SendDebug('Setting GExpertsToolBar.Visible to ' + BooleanText(ToolBarVisible)); {$ENDIF}
    GExpertsToolBar.Visible := ToolBarVisible;

    GExpertsToolBar.SetEditorControls;
  end;

  {$IFOPT D+} SendDebug('Successfully applied toolbar settings'); {$ENDIF}
end;

function TEditorEnhancements.ConfigurationKey: string;
begin
  Result := 'EditorEnhancements';
end;

constructor TEditorEnhancements.Create(AOwner: TComponent);
begin
  // There have been some rather inexplicable problems (AVs)
  // if AOwner <> nil; simply take no risks - we don't need an owner

  Assert(AOwner = nil);

  inherited Create(AOwner);

  Name := 'GX_EditEnhance';

  FHideNavigationToolbarExpert := CreateHideNavigationToolbarExpert;

  // Editor tab control
  FHotTrack := True;

  // Toolbar settings
  FToolBarVisible := True;
  FToolBarAlign := alTop;

  FToolBarActionsList := TStringList.Create;
  InitializeGXToolBarDropdowns;

  LoadSettings;
end;

destructor TEditorEnhancements.Destroy;
begin
  {$IFOPT D+} SendDebug('Destroying Editor Enhancements'); {$ENDIF}
  Remove;

  FHideNavigationToolbarExpert := Nil;

  FreeAndNil(FToolBarActionsList);

  {$IFOPT D+} SendDebug('Editor Enhancements Destroyed'); {$ENDIF}

  inherited Destroy;
end;

procedure TEditorEnhancements.EditorFormListener(
  EventCode: TEditFormEventCode; EditFormProxy: IGxEditFormProxy);
begin
  {$IFOPT D+} SendDebug('Got notification of editor form change'); {$ENDIF}
  if EventCode = efAfterCreate then
    AddToolBar;
end;

function TEditorEnhancements.GetToolBarActionsList: TStrings;
begin
  Result := FToolBarActionsList;
end;

procedure TEditorEnhancements.Install;
begin
  if not EditorEnhancementsPossible then 
    Exit;
  AddToolBar;

  GxEditorFormServices.AddListener(EditorFormListener);
end;

procedure TEditorEnhancements.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  Assert(ConfigInfo <> nil);

  Settings := TGExpertsSettings.Create;
  try
    LoadToolBarSettings(Settings);

    MiddleButtonClose := Settings.ReadBool(ConfigurationKey, 'MiddleButtonClose', MiddleButtonClose);
    MultiLine := Settings.ReadBool(ConfigurationKey, 'MultiLine', MultiLine);
    HotTrack := Settings.ReadBool(ConfigurationKey, 'HotTrack', HotTrack);
    Buttons := Settings.ReadBool(ConfigurationKey, 'Buttons', Buttons);
    ButtonsFlat := Settings.ReadBool(ConfigurationKey, 'ButtonsFlat', ButtonsFlat);
    ToolBarVisible := Settings.ReadBool(ConfigurationKey, 'ToolBarVisible', ToolBarVisible);
    HideNavbar := Settings.ReadBool(ConfigurationKey, 'HideNavbar', HideNavbar);
    FToolBarAlign := TAlign(Settings.ReadInteger(ConfigurationKey, 'ToolBarAlign', Ord(ToolBarAlign)));
    Assert(FToolBarAlign in [Low(TAlign)..High(TAlign)]);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEditorEnhancements.LoadToolBarSettings(const Settings: TGExpertsSettings);
const
  // Do not localize default action names.
  DefaultToolBarActions: array[0..4] of string =
    ('EditCutCommand', 'EditCopyCommand', 'EditPasteCommand', 'EditUndoCommand',
     'EditRedoCommand');
var
  i: Integer;
  ActionData: string;
  ToolBarButtonCount: Integer;
begin
  ToolBarButtonCount := Settings.ReadInteger(ToolBarActionsKey, ToolBarActionsCountValue, 0);
  for i := 0 to ToolBarButtonCount-1 do
  begin
    ActionData := Settings.ReadString(ToolBarActionsKey,
      Format('%s%d', [ToolBarActionsActionPrefixValue, i]), SeparatorMenuItemString);
    // Close just causes AVs, so we don't allow it
    if not StrContains('FileClose', ActionData) then
      FToolBarActionsList.Add(ActionData);
  end;

  if (ToolBarButtonCount = 0) and (not IsStandAlone) then
  begin
    for i := Low(DefaultToolBarActions) to High(DefaultToolBarActions) do
    begin
    {$IFOPT D+}
      Assert(Assigned(GxActionBroker.FindAction(DefaultToolBarActions[i])),
             'Could not locate default IDE action named ' + DefaultToolBarActions[i]);
    {$ENDIF D+}
      FToolBarActionsList.Add(DefaultToolBarActions[i]);
    end;

    SaveToolBarSettings(Settings);
  end;
end;

procedure TEditorEnhancements.Notification(Component: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  if Operation = opRemove then
  begin
    Assert(Assigned(FToolBarList));
    i := FToolBarList.IndexOf(Component);
    if i >= 0 then
      FToolBarList.Delete(i);
  end;

  inherited Notification(Component, Operation);
end;

procedure TEditorEnhancements.Remove;
begin
  if not EditorEnhancementsPossible then 
    Exit;

  GxEditorFormServices.RemoveListener(EditorFormListener);
  RemoveToolBar;
end;

procedure TEditorEnhancements.RemoveToolBar;
var
  GExpertsToolBar: TGXToolBar;
begin
  if not Assigned(FToolBarList) then
    Exit;

  {$IFOPT D+} SendDebug(Format('Editor Enhancements: Removing %d toolbar(s)', [FToolBarList.Count])); {$ENDIF}
  // Note: Since we used FreeNotification on the toolbar
  // to instruct it to notify ourselves of its very own
  // deletion, GExpertsToolBar.Free will essentially
  // call back into Notifcation where the toolbar is
  // removed from the FToolBarList (.Delete).
  // Hence the slightly weird structure of this loop.
  while FToolBarList.Count > 0 do
  begin
    GExpertsToolBar := FToolBarList.Items[0];
    FreeAndNil(GExpertsToolBar);
    {$IFOPT D+} SendDebug('Editor Enhancements: Successfully freed a toolbar'); {$ENDIF}
  end;

  {$IFOPT D+} SendDebug('Freeing toolbar list'); {$ENDIF}
  FreeAndNil(FToolBarList);
end;

procedure TEditorEnhancements.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    SaveToolBarSettings(Settings);

    Settings.WriteBool(ConfigurationKey, 'MiddleButtonClose', MiddleButtonClose);
    Settings.WriteBool(ConfigurationKey, 'MultiLine', MultiLine);
    Settings.WriteBool(ConfigurationKey, 'HotTrack', HotTrack);
    Settings.WriteBool(ConfigurationKey, 'Buttons', Buttons);
    Settings.WriteBool(ConfigurationKey, 'ButtonsFlat', ButtonsFlat);
    Settings.WriteBool(ConfigurationKey, 'ToolBarVisible', ToolBarVisible);
    Settings.WriteBool(ConfigurationKey, 'HideNavbar', HideNavbar);
    Settings.WriteInteger(ConfigurationKey, 'ToolBarAlign', Ord(ToolBarAlign));
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEditorEnhancements.SaveToolBarSettings(const Settings: TGExpertsSettings);
var
  i: Integer;
begin
  Assert(Assigned(Settings));
  Assert(Assigned(FToolBarActionsList));

  Settings.WriteInteger(ToolBarActionsKey, ToolBarActionsCountValue, FToolBarActionsList.Count);

  for i := 0 to FToolBarActionsList.Count - 1 do
    Settings.WriteString(ToolBarActionsKey,
      Format('%s%d', [ToolBarActionsActionPrefixValue, i]), FToolBarActionsList[i]);
end;

procedure TEditorEnhancements.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if FEnabled then
    Install
  else
    Remove;
end;

procedure TEditorEnhancements.SetHideNavbar(const _Value: Boolean);
begin
  FHideNavbar := _Value;
  FHideNavigationToolbarExpert.SetVisible(not FHideNavbar);
end;

procedure TEditorEnhancements.SetToolBarActionsList(const Value: TStrings);
begin
  Assert(Assigned(FToolBarActionsList));
  if Assigned(Value) then
    FToolBarActionsList.Assign(Value)
  else
    FToolBarActionsList.Clear;
end;

function TEditorEnhancements.ShowConfigurationDialog(ActiveSheet: TConfigurationSheet): TModalResult;
var
  Dlg: TfmConfiguration;
  CurrentTabSheet: TTabSheet;
  i: Integer;
begin
  Dlg := TfmConfiguration.Create(nil);
  try
    Assert(Ord(High(TConfigurationSheet)) = 1);

    case ActiveSheet of
      cfgEditor:  Dlg.pcConfig.ActivePage := Dlg.tshEditor;
    end;

    // Hide non-essential tab sheets.
    for i := 0 to Dlg.pcConfig.PageCount - 1 do
    begin
      CurrentTabSheet := Dlg.pcConfig.Pages[i];
      CurrentTabSheet.TabVisible := (CurrentTabSheet = Dlg.tshEditor);
    end;

    { TODO 4 -oAnyone -cIssue:
    This is horribly convoluted. From TEditorEnhancements we call that
    dialog which, if mrOk, sets properties in TEditorEnhancements which
    in turn will set properties on the toolbar and elsewhere.
    Note that amazingly ShowConfigurationDialog here only ever is triggered
    by the toolbar and by the editor local context menu appendix.
    Probably ShowConfigurationDialog should be a member of, say GX_Toolbar?}
    Result := Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TEditorEnhancements.ShowEditorConfigurationDialog: TModalResult;
begin
  Result := ShowConfigurationDialog(cfgEditor);
end;

function TEditorEnhancements.ShowToolBarConfigurationDialog: TModalResult;
var
  Dialog: TfmToolbarConfig;
begin
  Dialog := TfmToolbarConfig.Create(nil);
  try
    Dialog.ToolBarActions := FToolBarActionsList;
    Result := Dialog.ShowModal;
    if Result = mrOk then
      FToolBarActionsList.Assign(Dialog.ToolBarActions);
  finally
    FreeAndNil(Dialog);
  end;

  if Result = mrOk then
  begin
    SaveSettings;
    ApplyToolBarSettings;
  end;
end;

// ****************************************************************************

var
  PrivateEditorEnhancements: TEditorEnhancements;
  CanCreate: Boolean = True;

function EditorEnhancements: TEditorEnhancements;
begin
  Assert(CanCreate, 'CanCreate must be true in EditorEnhancements');

  if PrivateEditorEnhancements = nil then
    PrivateEditorEnhancements := TEditorEnhancements.Create(nil);

  Result := PrivateEditorEnhancements;
end;

procedure FreeEditorEnhancements;
begin
  {$IFOPT D+} SendDebug('FreeEditorEnhancements CanCreate: ' + BooleanText(CanCreate)); {$ENDIF}
  CanCreate := False;

  FreeAndNil(PrivateEditorEnhancements);
end;

initialization
  CanCreate := True;

finalization
  FreeEditorEnhancements;

end.

