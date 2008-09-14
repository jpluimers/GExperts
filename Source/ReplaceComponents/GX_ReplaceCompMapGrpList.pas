// Original Author: Piotr Likus
// Replace Components form that lists mapping groups
unit GX_ReplaceCompMapGrpList;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GX_ReplaceCompData;

type
  TfmReplaceCompMapGrpList = class(TForm)
    pnlButtons: TPanel;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnExport: TButton;
    btnImport: TButton;
    btnClear: TButton;
    pnlList: TPanel;
    lbxGroups: TListBox;
    pnlMain: TPanel;
    pnlFooter: TPanel;
    pnlFooterButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    dlgGetImportFile: TOpenDialog;
    dlgGetExportFile: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure lbxGroupsClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnImportClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure lbxGroupsDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FConfigData: TReplaceCompData;
    procedure LoadGroupList;
    procedure UpdateBtns;
    procedure ClearAllGroups;
    function SelectedGroupName: string;
    procedure DeleteGroup(const GroupName: string; RefreshEnabled: Boolean = True);
    procedure RefreshList;
    procedure DeleteGroups(GroupList: TStringList);
    procedure DeleteSelectedGroups;
    procedure GetSelectedGroups(GroupList: TStringList);
    procedure ExecEditGroup(const GroupName: string);
    procedure ShowError(const Msg: string);
    procedure ValidateNewName(OldName, NewName: string);
    function LocateGroup(const GroupName: string): Boolean;
    procedure ExecExportTo(const FileName: string);
    function ConfigurationKey: string;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create(Owner: TComponent; ConfigData: TReplaceCompData); reintroduce;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_GenericUtils, GX_ConfigurationInfo;

resourcestring
  SEnterNewName = 'Enter New Group Name';
  SName = 'Name';

{ TfmReplaceCompMapGrpList }

constructor TfmReplaceCompMapGrpList.Create(Owner: TComponent;
  ConfigData: TReplaceCompData);
begin
  inherited Create(Owner);
  FConfigData := ConfigData;
  LoadSettings;
end;

procedure TfmReplaceCompMapGrpList.FormShow(Sender: TObject);
begin
  LoadGroupList;
  UpdateBtns;
end;

procedure TfmReplaceCompMapGrpList.UpdateBtns;
var
  ItemSelectedFlag: Boolean;
  ItemSelectedCount: Integer;
begin
  ItemSelectedCount := lbxGroups.SelCount;
  ItemSelectedFlag := (ItemSelectedCount > 0);
  btnEdit.Enabled := (ItemSelectedCount = 1);
  btnDelete.Enabled := ItemSelectedFlag;
  btnExport.Enabled := ItemSelectedFlag;
  btnClear.Enabled := (lbxGroups.Items.Count > 0);
end;

procedure TfmReplaceCompMapGrpList.RefreshList;
begin
  LoadGroupList;
  UpdateBtns;
end;

procedure TfmReplaceCompMapGrpList.LoadGroupList;
var
  i: Integer;
begin
  lbxGroups.Clear;
  lbxGroups.Items.BeginUpdate;
  try
    for i := 0 to FConfigData.MapGroupList.Count-1 do
      lbxGroups.Items.Add(FConfigData.MapGroupList[i].Name);
  finally
    lbxGroups.Items.EndUpdate;
  end;
end;

procedure TfmReplaceCompMapGrpList.lbxGroupsClick(Sender: TObject);
begin
  UpdateBtns;
end;

procedure TfmReplaceCompMapGrpList.ClearAllGroups;
begin
  FConfigData.MapGroupList.Clear;
end;

procedure TfmReplaceCompMapGrpList.btnClearClick(Sender: TObject);
resourcestring
  SConfirmClear = 'Are you sure you want to delete all groups?';
begin
  if MessageDlg(SConfirmClear, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ClearAllGroups;
    RefreshList;
  end;  
end;

procedure TfmReplaceCompMapGrpList.GetSelectedGroups(GroupList: TStringList);
var
  i: Integer;
begin
  GroupList.Clear;
  for i := 0 to lbxGroups.Items.Count-1 do
    if lbxGroups.Selected[i] then
      GroupList.Add(lbxGroups.Items[i]);
end;

function TfmReplaceCompMapGrpList.SelectedGroupName: string;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  while i < lbxGroups.Items.Count do
  begin
    if lbxGroups.Selected[i] then
    begin
      Result := lbxGroups.Items[i];
      Break;
    end
    else
      Inc(i);
  end;
end;

procedure TfmReplaceCompMapGrpList.DeleteGroup(const GroupName: string; RefreshEnabled: Boolean);
var
  Idx: Integer;
begin
  Idx := FConfigData.MapGroupList.IndexOf(GroupName);
  if Idx > -1 then
  begin
    FConfigData.MapGroupList.Delete(Idx);
    if RefreshEnabled then RefreshList;
  end;
end;

procedure TfmReplaceCompMapGrpList.DeleteGroups(GroupList: TStringList);
var
  i: Integer;
begin
  if GroupList.Count > 0 then
  begin
    for i := 0 to GroupList.Count-1 do
      DeleteGroup(GroupList[i], False);
    RefreshList;
  end;
end;

procedure TfmReplaceCompMapGrpList.DeleteSelectedGroups;
var
  SelectedItems: TStringList;
begin
  SelectedItems := TStringList.Create;
  try
    GetSelectedGroups(SelectedItems);
    DeleteGroups(SelectedItems);
  finally
    FreeAndNil(SelectedItems);
  end;
end;

procedure TfmReplaceCompMapGrpList.ShowError(const Msg: string);
begin
  MessageDlg(Msg, mtError, [mbOK], 0);
end;

procedure TfmReplaceCompMapGrpList.btnDeleteClick(Sender: TObject);
resourcestring
  SConfirmDelete = 'Are you sure you want to delete selected group(s)?';
begin
  if MessageDlg(SConfirmDelete, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    DeleteSelectedGroups;
end;

procedure TfmReplaceCompMapGrpList.ExecEditGroup(const GroupName: string);
var
  GroupObject: TCompRepMapGroupItem;
  OldName, NewName: string;
begin
  OldName := GroupName;
  GroupObject := FConfigData.MapGroupList.FindObject(OldName) as TCompRepMapGroupItem;
  if Assigned(GroupObject) then
  begin
    NewName := OldName;
    if InputQuery(SEnterNewName, SName, NewName) then
    begin
      NewName := Trim(NewName);
      ValidateNewName(OldName, NewName);
      GroupObject.Name := NewName;
      RefreshList;
      LocateGroup(NewName);
    end;
  end;
end;

function TfmReplaceCompMapGrpList.LocateGroup(const GroupName: string): Boolean;
var
  Idx: Integer;
begin
  Idx := lbxGroups.Items.IndexOf(GroupName);
  Result := (Idx >= 0);
  if Result then
    lbxGroups.ItemIndex := Idx;
end;

procedure TfmReplaceCompMapGrpList.btnEditClick(Sender: TObject);
begin
  ExecEditGroup(SelectedGroupName);
end;

procedure TfmReplaceCompMapGrpList.ValidateNewName(OldName, NewName: string);
resourcestring
  SNameNonUnique = 'Name "%s" is not unique!';
  SNameRequired = 'Name of group can not be empty!';
var
  NewIdx, OldIdx: Integer;
begin
  if NewName = '' then
  begin
    ShowError(SNameRequired);
    Abort;
  end;

  if OldName = NewName then
    Exit;

  NewIdx := FConfigData.MapGroupList.IndexOf(NewName);
  OldIdx := FConfigData.MapGroupList.IndexOf(OldName);

  if (NewIdx >= 0) and (NewIdx <> OldIdx) then
  begin
    ShowError(Format(SNameNonUnique, [NewName]));
    Abort;
  end;
end;

procedure TfmReplaceCompMapGrpList.btnAddClick(Sender: TObject);
var
  NewName: string;
begin
  NewName := '';
  if InputQuery(SEnterNewName, SName, NewName) then
  begin
    NewName := Trim(NewName);
    ValidateNewName('', NewName);

    FConfigData.MapGroupList.Add(NewName);

    RefreshList;
    LocateGroup(NewName);
  end;
end;

function TfmReplaceCompMapGrpList.ConfigurationKey: string;
begin
  Result := FConfigData.RootConfigurationKey + PathDelim + Self.ClassName+'.Window';
end;

procedure TfmReplaceCompMapGrpList.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
    Width := Settings.ReadInteger(ConfigurationKey, 'Width', Width);
    Height := Settings.ReadInteger(ConfigurationKey, 'Height', Height);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmReplaceCompMapGrpList.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    if not (WindowState in [wsMinimized, wsMaximized]) then
    begin
      Settings.WriteInteger(ConfigurationKey, 'Left', Left);
      Settings.WriteInteger(ConfigurationKey, 'Top', Top);
      Settings.WriteInteger(ConfigurationKey, 'Width', Width);
      Settings.WriteInteger(ConfigurationKey, 'Height', Height);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmReplaceCompMapGrpList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TfmReplaceCompMapGrpList.btnImportClick(Sender: TObject);
begin
  if dlgGetImportFile.Execute then
  begin
    FConfigData.AppendFrom(dlgGetImportFile.FileName);
    RefreshList;
  end;
end;

procedure TfmReplaceCompMapGrpList.btnExportClick(Sender: TObject);
begin
  if dlgGetExportFile.Execute then
    ExecExportTo(dlgGetExportFile.FileName);
end;

procedure TfmReplaceCompMapGrpList.ExecExportTo(const FileName: string);
var
  GroupNames: TStringList;
begin
  GroupNames := TStringList.Create;
  try
    GetSelectedGroups(GroupNames);
    FConfigData.SaveTo(FileName, GroupNames);
  finally
    FreeAndNil(GroupNames);
  end;
end;

procedure TfmReplaceCompMapGrpList.lbxGroupsDblClick(Sender: TObject);
begin
  if btnEdit.Enabled then
    btnEdit.Click;
end;

procedure TfmReplaceCompMapGrpList.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
  begin
    for i := 0 to lbxGroups.Items.Count - 1 do
      lbxGroups.Selected[i] := True;
    Key := 0;
  end;
end;

end.

