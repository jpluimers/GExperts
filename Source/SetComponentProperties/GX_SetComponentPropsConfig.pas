// Configuration dialog for Set Component Properties
// Original Author: Robert Wachtel (rwachtel@gmx.de)

unit GX_SetComponentPropsConfig;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, ActnList, Menus, StdCtrls, ComCtrls;

type
  TfmSetComponentPropsConfig = class(TForm)
    actnAddPropertyType: TAction;
    actnAddRule: TAction;
    actnClearRules: TAction;
    actnDefaultPropertyTypes: TAction;
    actnDefaultRules: TAction;
    actnDeletePropertyType: TAction;
    actnDeleteRule: TAction;
    actnHelp: TAction;
    Actions: TActionList;
    actnModifyPropertyType: TAction;
    btnAddRule: TButton;
    btnCancel: TButton;
    btnDeleteRule: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    chkSimulate: TCheckBox;
    chkVerbose: TCheckBox;
    cbxComponents: TComboBox;
    cbxProperty: TComboBox;
    edtValue: TEdit;
    gbxOptions: TGroupBox;
    gbxPropertyToSet: TGroupBox;
    itmAdd: TMenuItem;
    itmClearRules: TMenuItem;
    itmDefaultRules: TMenuItem;
    itmDefaultTypes: TMenuItem;
    itmDelete: TMenuItem;
    itmModify: TMenuItem;
    itmN1: TMenuItem;
    lblComponent: TLabel;
    lblGrid: TLabel;
    lblProperty: TLabel;
    lblPropertyTpes: TLabel;
    lblValue: TLabel;
    lbxPropertyTypes: TListBox;
    pmuProperties: TPopupMenu;
    pnuPropertyTypes: TPopupMenu;
    lvProperties: TListView;
    chkOnlyOpenFiles: TCheckBox;
    procedure actnAddPropertyTypeExecute(Sender: TObject);
    procedure actnAddRuleExecute(Sender: TObject);
    procedure actnClearRulesExecute(Sender: TObject);
    procedure actnDefaultPropertyTypesExecute(Sender: TObject);
    procedure actnDefaultRulesExecute(Sender: TObject);
    procedure actnDeletePropertyTypeExecute(Sender: TObject);
    procedure actnDeleteRuleExecute(Sender: TObject);
    procedure actnHelpExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actnModifyPropertyTypeExecute(Sender: TObject);
    procedure chkSimulateClick(Sender: TObject);
    procedure chkVerboseClick(Sender: TObject);
    procedure cbxPropertyEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvPropertiesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure chkOnlyOpenFilesClick(Sender: TObject);
  private
    FActualComponent: string;
    FActualPropertyNames: TStringList;
    FActualPropertyTypes: TStringList;
    FSimulateOnly: Boolean;
    FVerbose: Boolean;
    FOnlyOpenFiles: Boolean;
    function AddComponentItem(const ComponentName, ComponentProperty, ComponentValue: string): Integer;
    function GetRowIndex(const ComponentName, ComponentProperty: string): Integer;
    procedure SetDefaultComponents;
    procedure SetDefaultPropertyTypes;
    procedure SetSimulateOnly(const Value: Boolean);
    procedure SetVerbose(const Value: Boolean);
    procedure SetOnlyOpenFiles(const Value: Boolean);
  public
    procedure GetSettings;
    procedure SetSettings;
    property SimulateOnly: Boolean read FSimulateOnly write SetSimulateOnly;
    property Verbose: Boolean read FVerbose write SetVerbose;
    property OnlyOpenFiles: Boolean read FOnlyOpenFiles write SetOnlyOpenFiles;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Dialogs,
  GX_OtaUtils, GX_GenericUtils, GX_SetComponentProps, GX_GxUtils;

// New property type input
procedure TfmSetComponentPropsConfig.actnAddPropertyTypeExecute(Sender: TObject);
resourcestring
  rsAddPropertyTypeCaption = 'New Property Type';
var
  NewPropertyType: string;
begin
  NewPropertyType := InputBox(rsAddPropertyTypeCaption, '', '');
  if (NewPropertyType <> '') and (lbxPropertyTypes.Items.IndexOf(NewPropertyType) < 0) then
    lbxPropertyTypes.Items.Add(NewPropertyType);
end;

// A new or modificated rule should be saved
procedure TfmSetComponentPropsConfig.actnAddRuleExecute(Sender: TObject);
var
  RowIndex: Integer;
  ComponentName, ComponentProperty, ComponentValue: string;
begin
  ComponentName := Copy(cbxComponents.Text + #$20, 1, Pos(#$20, cbxComponents.Text + #$20) - 1);
  ComponentProperty := Copy(cbxProperty.Text + #$20, 1, Pos(#$20, cbxProperty.Text + #$20) - 1);
  ComponentValue := edtValue.Text;

  // Look for component name and property in grid
  RowIndex := GetRowIndex(ComponentName, ComponentProperty);

  // Component/property already in grid, so modify existing item
  if RowIndex > -1 then
    lvProperties.Items[RowIndex].SubItems[1] := edtValue.Text
  else // New rule
    RowIndex := AddComponentItem(ComponentName, ComponentProperty, ComponentValue);

  // Try to select the item
  if RowIndex > -1 then
    lvProperties.Items.Item[RowIndex].Selected := True;
end;

// Clear rules
procedure TfmSetComponentPropsConfig.actnClearRulesExecute(Sender: TObject);
begin
  if MessageDlg('This will delete all rules. Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes then
    lvProperties.Items.Clear;
end;

// Default property types is selcted
procedure TfmSetComponentPropsConfig.actnDefaultPropertyTypesExecute(Sender: TObject);
begin
  SetDefaultPropertyTypes;
end;

// Default rules
procedure TfmSetComponentPropsConfig.actnDefaultRulesExecute(Sender: TObject);
begin
  if MessageDlg('This will delete all rules and set them to default values.  Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes then
    SetDefaultComponents;
end;

// Delete the selected property type(s)
procedure TfmSetComponentPropsConfig.actnDeletePropertyTypeExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := Pred(lbxPropertyTypes.Items.Count) downto 0 do
    if lbxPropertyTypes.Selected[i] then
      lbxPropertyTypes.Items.Delete(i);
end;

// Delete the selected rule
procedure TfmSetComponentPropsConfig.actnDeleteRuleExecute(Sender: TObject);
var
  RowIndex: Integer;
  ComponentName, ComponentProperty: string;
begin
  ComponentName := Copy(cbxComponents.Text + #$20, 1, Pos(#$20, cbxComponents.Text + #$20) - 1);
  ComponentProperty := Copy(cbxProperty.Text + #$20, 1, Pos(#$20, cbxProperty.Text + #$20) - 1);

  // Look for component name/property in the grid
  RowIndex := GetRowIndex(ComponentName, ComponentProperty);

  if RowIndex > -1 then
    lvProperties.Items.Delete(RowIndex);

  cbxComponents.Text := '';
  cbxProperty.Text := '';
  edtValue.Text := '';
end;

procedure TfmSetComponentPropsConfig.actnHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 47);
end;

// Enable/disable menu items
procedure TfmSetComponentPropsConfig.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actnAddRule.Enabled := (cbxComponents.Text <> '') and (cbxProperty.Text <> '') and (edtValue.Text <> '');
  actnDeleteRule.Enabled := (cbxComponents.Text <> '') and (cbxProperty.Text <> '') and (edtValue.Text <> '');
  actnDeletePropertyType.Enabled := (lbxPropertyTypes.SelCount > 0);
  actnModifyPropertyType.Enabled := (lbxPropertyTypes.SelCount = 1);
  Handled := True;
end;

// Modify property type
procedure TfmSetComponentPropsConfig.actnModifyPropertyTypeExecute(Sender: TObject);
resourcestring
  rsModifyPropertyTypeCaption = 'Modify Property Type';
var
  ModifyPropertyType: string;
  IndexPropertyTypes: Integer;
begin
  for IndexPropertyTypes := Pred(lbxPropertyTypes.Items.Count) downto 0 do
  begin
    if lbxPropertyTypes.Selected[IndexPropertyTypes] then
    begin
      ModifyPropertyType := lbxPropertyTypes.Items[IndexPropertyTypes];
      if InputQuery(rsModifyPropertyTypeCaption, '', ModifyPropertyType) then
      begin
        if ModifyPropertyType <> lbxPropertyTypes.Items[IndexPropertyTypes] then
        begin
          if (lbxPropertyTypes.Items.IndexOf(ModifyPropertyType) < 0) and (ModifyPropertyType <> '') then
            lbxPropertyTypes.Items[IndexPropertyTypes] := ModifyPropertyType
          else
            lbxPropertyTypes.Items.Delete(IndexPropertyTypes);
        end;
      end;
    end;
  end;
end;

// Add a rule
function TfmSetComponentPropsConfig.AddComponentItem(const ComponentName,
  ComponentProperty, ComponentValue: string): Integer;
begin
  with lvProperties.Items.Add do begin
    Caption := ComponentName;
    SubItems.Add(ComponentProperty);
    SubItems.Add(ComponentValue);
  end;
  Result := GetRowIndex(ComponentName, ComponentProperty);
end;

// Simulate checkbox is clicked
procedure TfmSetComponentPropsConfig.chkSimulateClick(Sender: TObject);
begin
  FSimulateOnly := chkSimulate.Checked;
end;

// Verbose checkbox is clicked
procedure TfmSetComponentPropsConfig.chkVerboseClick(Sender: TObject);
begin
  FVerbose := chkVerbose.Checked;
end;

procedure TfmSetComponentPropsConfig.chkOnlyOpenFilesClick(Sender: TObject);
begin
  FOnlyOpenFiles := chkOnlyOpenFiles.Checked;
end;

// When entering the Property combobox all properties of the actual selected component are retrieved
procedure TfmSetComponentPropsConfig.cbxPropertyEnter(Sender: TObject);
var
  IndexPropertyNames: Integer;
  OldSelectedProperty: string;
begin
  FActualComponent := cbxComponents.Text;
  FActualPropertyNames.Clear;
  FActualPropertyTypes.Clear;

  if GetClass(FActualComponent) <> nil then
    GetPropertyNames(GetClass(FActualComponent), FActualPropertyNames, FActualPropertyTypes, lbxPropertyTypes.Items);

  OldSelectedProperty := cbxProperty.Text;

  cbxProperty.Clear;

  for IndexPropertyNames := 0 to Pred(FActualPropertyNames.Count) do
    cbxProperty.Items.Add(FActualPropertyNames[IndexPropertyNames] + #$20 + '(' + FActualPropertyTypes[IndexPropertyNames] + ')');

  cbxProperty.Text := OldSelectedProperty;
end;

procedure TfmSetComponentPropsConfig.FormCreate(Sender: TObject);
begin
  GxOtaGetInstalledComponentList(cbxComponents.Items, False);
  FActualComponent := '';
  FActualPropertyNames := TStringList.Create;
  FActualPropertyTypes := TStringList.Create;
end;

procedure TfmSetComponentPropsConfig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FActualPropertyNames);
  FreeAndNil(FActualPropertyTypes);
end;

// Look for a given ComponentName and ComponentProperty in the listview and give back the RowIndex
function TfmSetComponentPropsConfig.GetRowIndex(const ComponentName, ComponentProperty: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Pred(lvProperties.Items.Count) do
  begin
    if SameText(ComponentName, lvProperties.Items.Item[i].Caption) and SameText(ComponentProperty, lvProperties.Items.Item[i].SubItems[0]) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

// Save all settings in the expert setting container (TGxSetComponentPropsSettings)
procedure TfmSetComponentPropsConfig.GetSettings;
var
  i: Integer;
  Settings: TSetComponentPropsSettings;
begin
  Settings := TSetComponentPropsSettings.GetInstance;
  Settings.Components.Clear;
  Settings.Properties.Clear;
  Settings.Values.Clear;

  for i := 0 to Pred(lvProperties.Items.Count) do
  begin
    Settings.Components.Add(lvProperties.Items[i].Caption);
    Settings.Properties.Add(lvProperties.Items[i].SubItems[0]);
    Settings.Values.Add(lvProperties.Items[i].SubItems[1]);
  end;

  Settings.PropertyTypes.Assign(lbxPropertyTypes.Items);

  Settings.Simulate := SimulateOnly;
  Settings.Verbose := Verbose;
  Settings.OnlyOpenFiles := OnlyOpenFiles;
end;

procedure TfmSetComponentPropsConfig.SetDefaultComponents;
const
  rsDefaultValues   : array[0..2, 0..2] of string = (
    ('TADOConnection', 'TDatabase', 'TDataSet'),
    ('Connected',      'Connected', 'Active'),
    ('False',          'False',     'False'));
var
  i: Integer;
begin
  lvProperties.Items.Clear;
  for i := Low(rsDefaultValues) to High(rsDefaultValues) do
    AddComponentItem(rsDefaultValues[0, i], rsDefaultValues[1, i], rsDefaultValues[2, i]);
end;

procedure TfmSetComponentPropsConfig.SetDefaultPropertyTypes;
resourcestring
  rsDefaultPropertyTypes = 'Boolean,Integer,String,TCaption';
begin
  lbxPropertyTypes.Items.CommaText := rsDefaultPropertyTypes;
end;

// Simulate value is set
procedure TfmSetComponentPropsConfig.SetSimulateOnly(const Value: Boolean);
begin
  if FSimulateOnly <> Value then
  begin
    FSimulateOnly := Value;
    chkSimulate.Checked := Value;
  end;
end;

procedure TfmSetComponentPropsConfig.SetOnlyOpenFiles(const Value: Boolean);
begin
  if FOnlyOpenFiles <> Value then
  begin
    FOnlyOpenFiles := Value;
    chkOnlyOpenFiles.Checked := Value;
  end;
end;

// Verbose value is set
procedure TfmSetComponentPropsConfig.SetVerbose(const Value: Boolean);
begin
  if FVerbose <> Value then
  begin
    FVerbose := Value;
    chkVerbose.Checked := Value;
  end;
end;

// Load all settings from the expert setting container (TGxSetComponentPropsSettings)
procedure TfmSetComponentPropsConfig.SetSettings;
var
  i: Integer;
  Settings: TSetComponentPropsSettings;
begin
  Settings := TSetComponentPropsSettings.GetInstance;
  lvProperties.Items.Clear;
  for i := 0 to Pred(Settings.Components.Count) do
    AddComponentItem(Settings.Components[i], Settings.Properties[i], Settings.Values[i]);

  lbxPropertyTypes.Items.Assign(Settings.PropertyTypes);

  SimulateOnly := Settings.Simulate;
  Verbose := Settings.Verbose;
  OnlyOpenFiles := Settings.OnlyOpenFiles;

  if lvProperties.Items.Count = 0 then
    SetDefaultComponents;
  if lbxPropertyTypes.Items.Count = 0 then
    SetDefaultPropertyTypes;
end;

//  After selecting a cell in the listview the "Rules" groupbox is filled with data
procedure TfmSetComponentPropsConfig.lvPropertiesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Assigned(Item) and (Change = ctState) then
  begin
    cbxComponents.Text := Item.Caption;
    cbxProperty.Text := Item.SubItems[0];
    edtValue.Text := Item.SubItems[1];
  end;
end;

end.

