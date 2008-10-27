// Original Author: Piotr Likus
// Replace Components dialog with details of a single property mapping
unit GX_ReplaceCompMapDets;

interface

uses
  Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls, ActnList, ImgList,
  ComCtrls, ToolWin, GX_SharedImages, GX_ReplaceCompData;

type
  TDataAction = (daUnknown, daInsert, daEdit, daDelete);

  TfmReplaceCompMapDets = class(TForm)
    Actions: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    actFirst: TAction;
    actPrior: TAction;
    actNext: TAction;
    actLast: TAction;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnApply: TButton;
    btnClose: TButton;
    pnlHeader: TPanel;
    pnlToolbar: TPanel;
    ToolBar: TToolBar;
    tbnFirst: TToolButton;
    tbnPrior: TToolButton;
    tbnNext: TToolButton;
    tbnLast: TToolButton;
    tbnSep: TToolButton;
    tbnAdd: TToolButton;
    tbnDelete: TToolButton;
    bvlPos: TBevel;
    lblPos: TLabel;
    lblPosition: TLabel;
    pnlDetails: TPanel;
    gbxMappingDetails: TGroupBox;
    pnlEdits: TPanel;
    lblGroup: TLabel;
    lblSourceComp: TLabel;
    lblDestComp: TLabel;
    lblSourceProp: TLabel;
    lblDestProp: TLabel;
    cbxGroupName: TComboBox;
    cbxSourceClassName: TComboBox;
    cbxDestClassName: TComboBox;
    cbxSourcePropName: TComboBox;
    cbxDestPropName: TComboBox;
    chkBiDirEnabled: TCheckBox;
    chkDisabled: TCheckBox;
    chkUseConstValue: TCheckBox;
    edtConstValue: TEdit;
    chkLogValues: TCheckBox;
    chkLogNonDef: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnFirstClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbxSourceClassNameChange(Sender: TObject);
    procedure cbxDestClassNameChange(Sender: TObject);
    procedure chkUseConstValueClick(Sender: TObject);
    procedure chkLogValuesClick(Sender: TObject);
  private
    FItem: TCompRepMapItem;
    FDataAction: TDataAction;
    FConfigData: TReplaceCompData;
    FGroup: TCompRepMapGroupItem;
    FModified: Boolean;
    procedure LoadGroupList;
    procedure LoadItem;
    procedure UpdateBtns;
    procedure UpdateGroupSelection;
    procedure FillClassList(List: TStrings);
    procedure TrySetCombo(Combo: TComboBox; const Value: string);
    procedure UpdateNavBar;
    procedure LoadProps(AClassName: string; Items: TStrings);
    procedure SaveItem;
    procedure ShowError(const Msg: string);
    procedure ValidateItem;
    procedure DeleteItem(Item: TCompRepMapItem);
    procedure CheckChangeItem(DeleteOnNew: Boolean = False);
    procedure ExecFirst;
    function GetCurrentGroup: TCompRepMapGroupItem;
    procedure RefreshItem;
    procedure SetItem(NewItem: TCompRepMapItem);
    procedure ExecPrior;
    procedure ExecNext;
    procedure ExecLast;
    function CreateItem: TCompRepMapItem;
    procedure ExecAdd;
    procedure ExecDelete;
    procedure FillDefaults;
    procedure BeginEdit;
    procedure BeginInsert;
    procedure DoDelete;
    procedure SetComboColor(Combo: TComboBox; Color: TColor);
    procedure SetEditsColor(Color: TColor);
    procedure ExecApply;
    procedure TryFocus(AControl: TWinControl);
    procedure ShowRequiredError(const FieldName: string);
    procedure UpdateItemStatus;
    procedure LoadSettings;
    procedure SaveSettings;
    function ConfigurationKey: string;
    procedure SetModified;
  public
    constructor Create(Owner: TComponent; ConfigData: TReplaceCompData;
      Item: TCompRepMapItem; DataAction: TDataAction); reintroduce;
    function Execute: Boolean;
  end;

implementation

uses
  SysUtils, Dialogs,
  GX_OtaUtils, GX_GenericUtils, GX_ReplaceCompUtils, GX_ConfigurationInfo;

{$R *.dfm}

function GetEditColor(Enabled: Boolean): TColor;
begin
  if Enabled then
    Result := clWindow
  else
    Result := clBtnFace;  
end;

{ TfmReplaceCompMapDets }

constructor TfmReplaceCompMapDets.Create(Owner: TComponent; ConfigData: TReplaceCompData;
  Item: TCompRepMapItem; DataAction: TDataAction);
begin
  inherited Create(Owner);
  SetToolbarGradient(ToolBar);
  Assert(Assigned(ConfigData));
  Assert(Assigned(Item));
  FConfigData := ConfigData;
  FItem := Item;
  FGroup := Item.Group;
  FDataAction := DataAction;
  LoadSettings;
end;

procedure TfmReplaceCompMapDets.FormShow(Sender: TObject);
begin
  cbxGroupName.Text := '';
  LoadGroupList;
  FillClassList(cbxSourceClassName.Items);
  FillClassList(cbxDestClassName.Items);
  LoadItem;
  UpdateBtns;
end;

procedure TfmReplaceCompMapDets.LoadGroupList;
var
  i: Integer;
begin
  cbxGroupName.Items.Clear;
  cbxGroupName.Items.BeginUpdate;
  try
    for i := 0 to FConfigData.MapGroupList.Count-1 do
      cbxGroupName.Items.Add(FConfigData.MapGroupList[i].Name);
  finally
    cbxGroupName.Items.EndUpdate;
  end;
end;

procedure TfmReplaceCompMapDets.UpdateGroupSelection;
begin
  if Assigned(FItem) then
    cbxGroupName.ItemIndex := cbxGroupName.Items.IndexOf(FItem.GroupName)
end;

procedure TfmReplaceCompMapDets.FillClassList(List: TStrings);
begin
  GxOtaGetInstalledComponentList(List, False);
end;

procedure TfmReplaceCompMapDets.SetComboColor(Combo: TComboBox; Color: TColor);
begin
  Combo.Color := Color;
end;

procedure TfmReplaceCompMapDets.SetEditsColor(Color: TColor);
begin
  SetComboColor(cbxGroupName, Color);
  SetComboColor(cbxSourceClassName, Color);
  SetComboColor(cbxDestClassName, Color);
  SetComboColor(cbxSourcePropName, Color);
  SetComboColor(cbxDestPropName, Color);
  edtConstValue.Color := Color;
end;

procedure TfmReplaceCompMapDets.UpdateItemStatus;
var
  Idx, ItemCnt: Integer;
begin
  if Assigned(FItem) then
  begin
    if Assigned(FItem.Group) then
    begin
      FGroup := FItem.Group;
      ItemCnt := FGroup.Items.Count;
      Idx := FItem.Index + 1;
    end
    else
    begin
      ItemCnt := 1;
      Idx := 1;
    end;

    lblPosition.Caption := IntToStr(Idx)+' / '+IntToStr(ItemCnt);
    SetEditsColor(GetEditColor(True));
    pnlEdits.Enabled := True;
    edtConstValue.Color := GetEditColor(chkUseConstValue.Checked);
    edtConstValue.ReadOnly := not chkUseConstValue.Checked;
    chkLogNonDef.Enabled := chkLogValues.Checked;
    cbxSourcePropName.Enabled := not chkUseConstValue.Checked;
    cbxSourcePropName.Color := GetEditColor(cbxSourcePropName.Enabled);
    chkBiDirEnabled.Enabled := not chkUseConstValue.Checked;
  end
  else
  begin
    lblPosition.Caption := '0 / 0';
    SetEditsColor(GetEditColor(False));
    pnlEdits.Enabled := False;
  end;
end;

procedure TfmReplaceCompMapDets.LoadItem;
begin
  UpdateGroupSelection;
  if Assigned(FItem) then
  begin
    TrySetCombo(cbxSourceClassName, FItem.SourceClassName);
    TrySetCombo(cbxDestClassName, FItem.DestClassName);
    LoadProps(FItem.SourceClassName, cbxSourcePropName.Items);
    LoadProps(FItem.DestClassName, cbxDestPropName.Items);
    cbxSourcePropName.Text := FItem.SourcePropName;
    cbxDestPropName.Text := FItem.DestPropName;
    chkDisabled.Checked := FItem.Disabled;
    chkBiDirEnabled.Checked := FItem.BiDirEnabled;
    chkUseConstValue.Checked := FItem.UseConstValue;
    edtConstValue.Text := FItem.ConstValue;
    chkLogValues.Checked := FItem.LogValuesEnabled;
    chkLogNonDef.Checked := FItem.LogOnlyDefValuesEnabled;
  end
  else
  begin
    TrySetCombo(cbxSourceClassName, '');
    TrySetCombo(cbxDestClassName, '');
    LoadProps('', cbxSourcePropName.Items);
    LoadProps('', cbxDestPropName.Items);
    cbxSourcePropName.Text := '';
    cbxDestPropName.Text := '';

    chkDisabled.Checked := CompRepDefDisabled;
    chkBiDirEnabled.Checked := CompRepDefBiDirEnabled;
    chkUseConstValue.Checked := CompRepDefUseConstValue;
    edtConstValue.Text := '';
    chkLogValues.Checked := CompRepDefLogValuesEnabled;
    chkLogNonDef.Checked := CompRepDefLogOnlyDefValuesEnabled;
  end;
  UpdateItemStatus;
end;


procedure TfmReplaceCompMapDets.LoadProps(AClassName: string; Items: TStrings);
begin
  if Trim(AClassName) = '' then
    Items.Clear
  else
    GetPropertyNames(AClassName, Items);
end;

procedure TfmReplaceCompMapDets.TrySetCombo(Combo: TComboBox; const Value: string);
begin
  Combo.ItemIndex := Combo.Items.IndexOf(Value);
  if Combo.ItemIndex = -1 then
    Combo.Text := Value;
end;

procedure TfmReplaceCompMapDets.UpdateBtns;
begin
  UpdateNavBar;
  btnApply.Enabled := Assigned(FItem);
end;

procedure TfmReplaceCompMapDets.UpdateNavBar;
var
  Index: Integer;
begin
  if (not Assigned(FItem)) or (not Assigned(FItem.Group)) then
  begin
    actFirst.Enabled := False;
    actPrior.Enabled := False;
    actNext.Enabled := False;
    actLast.Enabled := False;
    actAdd.Enabled := Assigned(FGroup);
    actDelete.Enabled := False;
  end
  else
  begin
    Index := FItem.Index;
    actFirst.Enabled := (Index > 0);
    actPrior.Enabled := (Index > 0);
    actNext.Enabled := (Index < FItem.Group.Items.Count - 1);
    actLast.Enabled := (Index < FItem.Group.Items.Count - 1);
    actAdd.Enabled := True;
    actDelete.Enabled := (Index >= 0);
  end;
end;

procedure TfmReplaceCompMapDets.btnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfmReplaceCompMapDets.ExecApply;
begin
  SaveItem;
  if FDataAction = daInsert then
    FDataAction := daEdit;
  UpdateItemStatus;
  UpdateBtns;
end;

procedure TfmReplaceCompMapDets.btnApplyClick(Sender: TObject);
begin
  ExecApply;
  SetModified;
end;

procedure TfmReplaceCompMapDets.ShowError(const Msg: string);
begin
  MessageDlg(Msg, mtError, [mbOK], 0);
end;

procedure TfmReplaceCompMapDets.TryFocus(AControl: TWinControl);
begin
  if AControl.CanFocus and AControl.Enabled then
    AControl.SetFocus;
end;

procedure TfmReplaceCompMapDets.ShowRequiredError(const FieldName: string);
resourcestring
  SNoFieldValue = 'have to be filled-in!';
begin
  ShowError(FieldName+' '+SNoFieldValue);
end;

procedure TfmReplaceCompMapDets.ValidateItem;
resourcestring
  SGroupName = 'Group name';
  SSourceClass = 'Source class';
  SSourceProp = 'Source property';
  SDestClass = 'Destination class';
  SDestProp = 'Destination property';
begin
  if Trim(cbxGroupName.Text) = '' then
  begin
    TryFocus(cbxGroupName);
    ShowRequiredError(SGroupName);
    Abort;
  end;
  if Trim(cbxSourceClassName.Text) = '' then
  begin
    TryFocus(cbxSourceClassName);
    ShowRequiredError(SSourceClass);
    Abort;
  end;
  if Trim(cbxDestClassName.Text) = '' then
  begin
    TryFocus(cbxDestClassName);
    ShowRequiredError(SDestClass);
    Abort;
  end;
  if (Trim(cbxSourcePropName.Text) = '') and (not chkUseConstValue.Checked) then
  begin
    TryFocus(cbxSourcePropName);
    ShowRequiredError(SSourceProp);
    Abort;
  end;
  if Trim(cbxDestPropName.Text) = '' then
  begin
    TryFocus(cbxDestPropName);
    ShowRequiredError(SDestProp);
    Abort;
  end;
end;

procedure TfmReplaceCompMapDets.SaveItem;
begin
  ValidateItem;
  FItem.Group := FConfigData.PrepareMapGroup(cbxGroupName.Text);
  FGroup := FItem.Group;
  FItem.SourceClassName := cbxSourceClassName.Text;
  FItem.SourcePropName := cbxSourcePropName.Text;
  FItem.DestClassName := cbxDestClassName.Text;
  FItem.DestPropName := cbxDestPropName.Text;
  FItem.Disabled := chkDisabled.Checked;
  FItem.BiDirEnabled := chkBiDirEnabled.Checked;
  FItem.UseConstValue := chkUseConstValue.Checked;
  FItem.ConstValue := edtConstValue.Text;
  FItem.LogValuesEnabled := chkLogValues.Checked;
  FItem.LogOnlyDefValuesEnabled := chkLogNonDef.Checked;
end;

// Copy values from the old record to a new one
procedure TfmReplaceCompMapDets.FillDefaults;
begin
  FItem.Group := FConfigData.PrepareMapGroup(cbxGroupName.Text);
  FItem.SourceClassName := cbxSourceClassName.Text;
  FItem.SourcePropName := '';
  FItem.DestClassName := cbxDestClassName.Text;
  FItem.DestPropName := '';
  FItem.Disabled := chkDisabled.Checked;
  FItem.BiDirEnabled := chkBiDirEnabled.Checked;
  FItem.LogValuesEnabled := chkLogValues.Checked;
  FItem.LogOnlyDefValuesEnabled := chkLogNonDef.Checked;
end;

procedure TfmReplaceCompMapDets.DeleteItem(Item: TCompRepMapItem);
begin
  if Assigned(Item) then
  begin
    Item.Group := nil;
    FreeAndNil(Item);
  end;
end;

procedure TfmReplaceCompMapDets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CheckChangeItem(True);
  SetItem(nil);
  SaveSettings;
end;

procedure TfmReplaceCompMapDets.FormCreate(Sender: TObject);
begin
  SetToolbarGradient(ToolBar);
end;

procedure TfmReplaceCompMapDets.btnFirstClick(Sender: TObject);
begin
  ExecFirst;
end;

procedure TfmReplaceCompMapDets.CheckChangeItem(DeleteOnNew: Boolean);
resourcestring
  SConfirmSaveNew = 'You have created a new item. Do you want to save it?';
begin
  if Assigned(FItem) and (FDataAction = daInsert) then
  begin
    if DeleteOnNew then
      DoDelete
    else
    begin
      if MessageDlg(SConfirmSaveNew, mtConfirmation, [mbYes, mbNo], 0) = mrYes
      then
        ExecApply
      else
        DoDelete;
    end;
  end;
end;

function TfmReplaceCompMapDets.GetCurrentGroup: TCompRepMapGroupItem;
begin
  if Assigned(FItem) then
    Result := FItem.Group
  else
    Result := nil;
end;

procedure TfmReplaceCompMapDets.SetItem(NewItem: TCompRepMapItem);
begin
  CheckChangeItem;
  FItem := NewItem;
end;

procedure TfmReplaceCompMapDets.RefreshItem;
begin
  LoadItem;
  UpdateBtns;
end;


procedure TfmReplaceCompMapDets.ExecFirst;
begin
  if Assigned(FItem.Group) and (FItem.Group.Items.Count>0) and (FItem.Index>0) then
  begin
    SetItem(FItem.Group.Items[0]);
    BeginEdit;
    RefreshItem;
  end;
end;

procedure TfmReplaceCompMapDets.ExecPrior;
var
  Idx: Integer;
begin
  Idx := FItem.Index;
  if Assigned(FItem.Group) and (FItem.Group.Items.Count>0) and (Idx>0) then
  begin
    SetItem(FItem.Group.Items[Idx-1]);
    BeginEdit;
    RefreshItem;
  end;
end;

procedure TfmReplaceCompMapDets.ExecNext;
var
  Cnt, Idx: Integer;
begin
  Idx := FItem.Index;
  Cnt := FItem.Group.Items.Count;

  if Assigned(FItem.Group) and (Idx < Cnt-1) then
  begin
    SetItem(FItem.Group.Items[Idx+1]);
    BeginEdit;
    RefreshItem;
  end;
end;

procedure TfmReplaceCompMapDets.ExecLast;
var
  Cnt, Idx: Integer;
begin
  Idx := FItem.Index;
  Cnt := FItem.Group.Items.Count;

  if Assigned(FItem.Group) and (Idx < Cnt-1) then
  begin
    SetItem(FItem.Group.Items[Cnt-1]);
    BeginEdit;
    RefreshItem;
  end;
end;

procedure TfmReplaceCompMapDets.BeginEdit;
begin
  FDataAction := daEdit;
end;

procedure TfmReplaceCompMapDets.BeginInsert;
begin
  FDataAction := daInsert;
end;

procedure TfmReplaceCompMapDets.btnPriorClick(Sender: TObject);
begin
  ExecPrior;
end;

procedure TfmReplaceCompMapDets.btnNextClick(Sender: TObject);
begin
  ExecNext;
end;

procedure TfmReplaceCompMapDets.btnLastClick(Sender: TObject);
begin
  ExecLast;
end;

procedure TfmReplaceCompMapDets.btnAddClick(Sender: TObject);
begin
  ExecAdd;
end;

procedure TfmReplaceCompMapDets.btnDeleteClick(Sender: TObject);
resourcestring
  SConfirmDelete = 'Are you sure you want to delete selected item?';
begin
  if MessageDlg(SConfirmDelete, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes
  then
    ExecDelete;
end;

function TfmReplaceCompMapDets.CreateItem: TCompRepMapItem;
begin
  Result := TCompRepMapItem.Create;
end;

procedure TfmReplaceCompMapDets.ExecAdd;
begin
  CheckChangeItem;
  FItem := CreateItem;
  BeginInsert;
  FillDefaults;
  RefreshItem;
end;

procedure TfmReplaceCompMapDets.DoDelete;
begin
  DeleteItem(FItem);
  FItem := nil;
end;

procedure TfmReplaceCompMapDets.ExecDelete;
var
  Group: TCompRepMapGroupItem;
  Idx: Integer;
begin
  if not Assigned(FItem) then
    Exit;
  Group := GetCurrentGroup;
  Idx := FItem.Index;

  DoDelete;

  if not Assigned(Group) then
    Idx := -1
  else if Idx >= Group.Items.Count then
    Idx := Group.Items.Count - 1;

  if Idx > -1 then
    SetItem(Group.Items[Idx]);

  BeginEdit;
  RefreshItem;
  SetModified;
end;

procedure TfmReplaceCompMapDets.cbxSourceClassNameChange(Sender: TObject);
begin
  LoadProps(cbxSourceClassName.Text, cbxSourcePropName.Items);
end;

procedure TfmReplaceCompMapDets.cbxDestClassNameChange(Sender: TObject);
begin
  LoadProps(cbxDestClassName.Text, cbxDestPropName.Items);
end;

function TfmReplaceCompMapDets.ConfigurationKey: string;
begin
  Result := FConfigData.RootConfigurationKey + PathDelim + Self.ClassName+'.Window';
end;

procedure TfmReplaceCompMapDets.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmReplaceCompMapDets.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteInteger(ConfigurationKey, 'Left', Left);
    Settings.WriteInteger(ConfigurationKey, 'Top', Top);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmReplaceCompMapDets.chkUseConstValueClick(Sender: TObject);
begin
  if not chkUseConstValue.Checked then
    edtConstValue.Text := ''
  else // const
  begin
    cbxSourcePropName.Text := '';
    chkBiDirEnabled.Checked := False;
  end;  
  UpdateItemStatus;
end;

procedure TfmReplaceCompMapDets.chkLogValuesClick(Sender: TObject);
begin
  UpdateItemStatus;
end;

function TfmReplaceCompMapDets.Execute: Boolean;
begin
  FModified := False;
  ShowModal;
  Result := FModified;
end;

procedure TfmReplaceCompMapDets.SetModified;
begin
  FModified := True;
end;

end.

