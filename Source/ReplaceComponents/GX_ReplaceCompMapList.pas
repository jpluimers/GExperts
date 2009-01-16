// Original Author: Piotr Likus
// Replace Components mapping list window
unit GX_ReplaceCompMapList;

interface

uses
  Messages, Classes, Controls, Forms, ComCtrls, StdCtrls,
  ExtCtrls, Contnrs, ToolWin, ImgList, ActnList,
  GX_ReplaceCompData, GX_SharedImages, GX_ReplaceCompMapDets, GX_BaseForm;

const
  UM_UPDATECOLS = WM_USER + 632;

type
  TfmReplaceCompMapList = class(TfmBaseForm)
    pnlHeader: TPanel;
    lblGroup: TLabel;
    comGroupName: TComboBox;
    pnlMain: TPanel;
    pnlMappings: TPanel;
    lvMapItems: TListView;
    Actions: TActionList;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    tbrReplacement: TToolBar;
    tbnAdd: TToolButton;
    tbnEdit: TToolButton;
    tbnDelete: TToolButton;
    actOpenGroupList: TAction;
    pnlToolSep: TPanel;
    tbrGroups: TToolBar;
    tbnGroups: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure comGroupNameChange(Sender: TObject);
    procedure btnOpenGroupListClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure lvMapItemsDblClick(Sender: TObject);
    procedure lvMapItemsClick(Sender: TObject);
    procedure lvMapItemsColumnClick(Sender: TObject; Column: TListColumn);
  private
    FConfigData: TReplaceCompData;
    FSelectedGroup: string;
    FSortOnColumn: Integer; // counter from 1 (-1 means DESC)
    function ConfigurationKey: string;
    procedure LoadSettings;
    procedure SaveSettings;
    function SelectedGroupName: string;
    procedure LoadGroupList;
    procedure UpdateGroupSelection;
    procedure LoadMapList;
    procedure UpdateBtns;
    procedure UpdateColumnWidths;
    procedure UMUpdateCols(var Msg: TMessage); message UM_UPDATECOLS;
    function IsAllGroupSelected: Boolean;
    procedure AddMappingToListView(AMapItem: TCompRepMapItem);
    procedure RefreshAll;
    procedure ExecAdd(const GroupName: string);
    procedure ExecDelete;
    procedure ExecEdit(Item: TCompRepMapItem);
    function SelectedItem: TCompRepMapItem;
    function FindGroup(const GroupName: string): TCompRepMapGroupItem;
    function ExecDets(Item: TCompRepMapItem; DataAction: TDataAction): Boolean;
    procedure DeleteItem(Item: TCompRepMapItem);
    procedure RefreshItems;
    procedure LoadAll;
    procedure SortItems(ItemList: TObjectList; SortOnColumn: Integer);
    procedure AddItems(Group: TCompRepMapGroupItem; ItemList: TObjectList);
    procedure LoadItems(ItemList: TObjectList);
  public
    constructor Create(Owner: TComponent; ConfigData: TReplaceCompData); reintroduce;
  end;

implementation

uses
  SysUtils, Windows, Dialogs, Gx_GenericUtils, GX_ConfigurationInfo,
  GX_ReplaceCompMapGrpList;

{$R *.dfm}

resourcestring
  SAllItemsGroup = '< All groups >';

{ TfmReplaceCompMapList }

constructor TfmReplaceCompMapList.Create(Owner: TComponent; ConfigData: TReplaceCompData);
begin
  inherited Create(Owner);
  FConfigData := ConfigData;
  FSortOnColumn := 1;
  LoadSettings;
end;

function TfmReplaceCompMapList.ConfigurationKey: string;
begin
  Result := FConfigData.RootConfigurationKey + PathDelim + Self.ClassName+'.Window';
end;

procedure TfmReplaceCompMapList.LoadSettings;
var
  Settings: TGExpertsSettings;
  i: Integer;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
    Width := Settings.ReadInteger(ConfigurationKey, 'Width', Width);
    Height := Settings.ReadInteger(ConfigurationKey, 'Height', Height);
    FSelectedGroup := Settings.ReadString(ConfigurationKey, 'SelectedGrp', '');
    // note: values < 0 are better stored as string
    FSortOnColumn := StrToIntDef(Settings.ReadString(ConfigurationKey, 'SortOnColumn',
      IntToStr(FSortOnColumn)), FSortOnColumn);

    for i := 0 to lvMapItems.Columns.Count-1 do
      lvMapItems.Columns[i].Width :=
        Settings.ReadInteger(ConfigurationKey, 'Col'+IntToStr(i)+'.Width',
          lvMapItems.Columns[i].Width);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmReplaceCompMapList.SaveSettings;
var
  Settings: TGExpertsSettings;
  i: Integer;
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
    Settings.WriteString(ConfigurationKey, 'SelectedGrp', SelectedGroupName);
    // Note: Values < 0 are better stored as string
    Settings.WriteString(ConfigurationKey, 'SortOnColumn', IntToStr(FSortOnColumn));

    for i := 0 to lvMapItems.Columns.Count-1 do
      Settings.WriteInteger(ConfigurationKey, 'Col'+IntToStr(i)+'.Width',
        lvMapItems.Columns[i].Width);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmReplaceCompMapList.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TfmReplaceCompMapList.FormCreate(Sender: TObject);
begin
  SetToolbarGradient(tbrReplacement);
  SetToolbarGradient(tbrGroups);
end;

function TfmReplaceCompMapList.SelectedGroupName: string;
begin
  if comGroupName.ItemIndex >= 0 then
    Result := comGroupName.Items[comGroupName.ItemIndex]
  else
    Result := '';
end;

procedure TfmReplaceCompMapList.FormShow(Sender: TObject);
begin
  comGroupName.ItemIndex := -1;
  LoadAll;
end;

procedure TfmReplaceCompMapList.RefreshItems;
begin
  LoadMapList;
  UpdateBtns;
end;

procedure TfmReplaceCompMapList.LoadAll;
begin
  LoadGroupList;
  UpdateGroupSelection;
  RefreshItems;
end;

procedure TfmReplaceCompMapList.RefreshAll;
begin
  if (comGroupName.ItemIndex >= 0) and (Trim(comGroupName.Text)<>'') then
    FSelectedGroup := comGroupName.Text
  else
    FSelectedGroup := '';
  LoadAll;
end;

procedure TfmReplaceCompMapList.LoadGroupList;
var
  i: Integer;
begin
  comGroupName.Items.Clear;
  comGroupName.Items.BeginUpdate;
  try
    for i := 0 to FConfigData.MapGroupList.Count-1 do
      comGroupName.Items.Add(FConfigData.MapGroupList[i].Name);
    comGroupName.Items.Insert(0, SAllItemsGroup);
  finally
    comGroupName.Items.EndUpdate;
  end;
end;

procedure TfmReplaceCompMapList.UpdateGroupSelection;
begin
  if FSelectedGroup<>'' then
    comGroupName.ItemIndex := comGroupName.Items.IndexOf(FSelectedGroup)
  else if comGroupName.Items.Count > 0 then
    comGroupName.ItemIndex := 0;
end;

procedure TfmReplaceCompMapList.UpdateColumnWidths;
begin
  PostMessage(Self.Handle, UM_UPDATECOLS, 0, 0)
end;

function TfmReplaceCompMapList.IsAllGroupSelected: Boolean;
begin
  Result := (SelectedGroupName = SAllItemsGroup);
end;

procedure TfmReplaceCompMapList.LoadMapList;
var
  i, Idx: Integer;
  ItemList: TObjectList;
begin
  lvMapItems.Items.BeginUpdate;
  try
    lvMapItems.Items.Clear;

    ItemList := TObjectList.Create;
    try
      ItemList.OwnsObjects := False;

      if IsAllGroupSelected then
      begin
        for i := 0 to FConfigData.MapGroupList.Count-1 do
          AddItems(FConfigData.MapGroupList[i], ItemList);
      end
      else
      begin
        Idx := FConfigData.MapGroupList.IndexOf(SelectedGroupName);
        if Idx >= 0 then
          AddItems(FConfigData.MapGroupList[Idx], ItemList);
      end;

      SortItems(ItemList, FSortOnColumn);
      LoadItems(ItemList);
    finally
      FreeAndNil(ItemList);
    end;
    UpdateColumnWidths;
    lvMapItems.Invalidate;
  finally
    lvMapItems.Items.EndUpdate;
  end;
end;

procedure TfmReplaceCompMapList.AddItems(Group: TCompRepMapGroupItem;
  ItemList: TObjectList);
var
  i: Integer;
begin
  for i := 0 to Group.Items.Count-1 do
    ItemList.Add(Group.Items[i]);
end;

procedure TfmReplaceCompMapList.SortItems(ItemList: TObjectList; SortOnColumn: Integer);
var
  SortedItemList: TStringList;
  i: Integer;
  NativeItem: TCompRepMapItem;
  ColumnText: string;
  AbsSortColumn: Integer;
begin
  Assert(not ItemList.OwnsObjects);

  AbsSortColumn := Abs(SortOnColumn);

  SortedItemList := TStringList.Create;
  try
    SortedItemList.Sorted := True;
    SortedItemList.Duplicates := dupAccept;
    for i := 0 to ItemList.Count-1 do
    begin
      NativeItem := (ItemList[i] as TCompRepMapItem);

      case AbsSortColumn of
        2: ColumnText := NativeItem.SourceText;
        3: ColumnText := NativeItem.DestText;
        else
          ColumnText := NativeItem.GroupName;
      end;
      SortedItemList.AddObject(ColumnText, NativeItem);
    end;

    ItemList.Clear;

    if AbsSortColumn <> SortOnColumn then
    begin
      for i :=SortedItemList.Count-1 downto 0 do
        ItemList.Add(SortedItemList.Objects[i])
    end
    else
    begin
      for i := 0 to SortedItemList.Count-1 do
        ItemList.Add(SortedItemList.Objects[i]);
    end;

  finally
    FreeAndNil(SortedItemList);
  end;
end;

procedure TfmReplaceCompMapList.LoadItems(ItemList: TObjectList);
var
  i: Integer;
begin
  for i := 0 to ItemList.Count-1 do
    AddMappingToListView(ItemList[i] as TCompRepMapItem);
end;

procedure TfmReplaceCompMapList.AddMappingToListView(AMapItem: TCompRepMapItem);
var
  ListItem: TListItem;
begin
  ListItem := lvMapItems.Items.Add;
  ListItem.Caption := AMapItem.GroupName;
  ListItem.SubItems.Add(AMapItem.SourceText);
  ListItem.SubItems.Add(AMapItem.DestText);
  ListItem.Data := AMapItem;
end;

procedure TfmReplaceCompMapList.UpdateBtns;
begin
  actEdit.Enabled := (lvMapItems.SelCount = 1);
  actDelete.Enabled := (lvMapItems.SelCount > 0);
  actAdd.Enabled := (SelectedGroupName<>'');
end;

procedure TfmReplaceCompMapList.UMUpdateCols(var Msg: TMessage);
begin
  // Hacks to get the listview columns sizing right on startup
  lvMapItems.Width := lvMapItems.Width + 1;
end;

procedure TfmReplaceCompMapList.comGroupNameChange(Sender: TObject);
begin
  LoadMapList;
end;

procedure TfmReplaceCompMapList.btnOpenGroupListClick(Sender: TObject);
var
  Dlg: TfmReplaceCompMapGrpList;
begin
  Dlg := TfmReplaceCompMapGrpList.Create(nil, FConfigData);
  try
    Dlg.Icon := Self.Icon;
    if Dlg.ShowModal = mrOK then
    begin
      FConfigData.SaveData;
      RefreshAll;
    end
    else
    begin
      FConfigData.ReloadData;
      RefreshAll;
    end;  
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmReplaceCompMapList.actAddExecute(Sender: TObject);
begin
  if IsAllGroupSelected then
    ExecAdd('')
  else
    ExecAdd(SelectedGroupName);
end;

function TfmReplaceCompMapList.FindGroup(const GroupName: string): TCompRepMapGroupItem;
begin
  Result := FConfigData.MapGroupList.FindObject(GroupName) as TCompRepMapGroupItem;
end;

function TfmReplaceCompMapList.ExecDets(Item: TCompRepMapItem; DataAction: TDataAction): Boolean;
var
  Dlg: TfmReplaceCompMapDets;
begin
  Dlg := TfmReplaceCompMapDets.Create(nil, FConfigData, Item, DataAction);
  try
    Dlg.Icon := Self.Icon;
    Result := Dlg.Execute;
    if Result then
    begin
      FConfigData.SaveData;
      RefreshAll;
    end;  
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmReplaceCompMapList.ExecAdd(const GroupName: string);
var
  Item: TCompRepMapItem;
begin
  Item := TCompRepMapItem.Create;
  try
    if GroupName <> '' then
      Item.Group := FindGroup(GroupName);
    if ExecDets(Item, daInsert) then
      RefreshItems;
  except
    FreeAndNil(Item);
  end;
end;

procedure TfmReplaceCompMapList.ExecEdit(Item: TCompRepMapItem);
begin
  if ExecDets(Item, daEdit) then
    RefreshItems;
end;

procedure TfmReplaceCompMapList.DeleteItem(Item: TCompRepMapItem);
begin
  if Assigned(Item) then
  begin
    Item.Group.ExtractItem(Item);
    FreeAndNil(Item);
  end;  
end;

procedure TfmReplaceCompMapList.ExecDelete;
var
  i: Integer;
begin
  for i := lvMapItems.Items.Count-1 downto 0 do
    if lvMapItems.Items[i].Selected then
      DeleteItem(TCompRepMapItem(lvMapItems.Items[i].Data));
  ListViewDeleteSelected(lvMapItems);
end;

function TfmReplaceCompMapList.SelectedItem: TCompRepMapItem;
begin
  if lvMapItems.SelCount <> 1 then
    Result := nil
  else
    Result := TCompRepMapItem(lvMapItems.Selected.Data);
end;

procedure TfmReplaceCompMapList.actEditExecute(Sender: TObject);
var
  Item: TCompRepMapItem;
begin
  Item := SelectedItem;
  if Assigned(Item) then
    ExecEdit(Item);
end;

procedure TfmReplaceCompMapList.actDeleteExecute(Sender: TObject);
resourcestring
  SConfirmDelete = 'Are you sure you want to delete selected item(s)?';
begin
  if MessageDlg(SConfirmDelete, mtConfirmation, [mbYes, mbNo], 0) = mrYes
  then
  begin
    ExecDelete;
    RefreshItems;
  end;  
end;

procedure TfmReplaceCompMapList.lvMapItemsDblClick(Sender: TObject);
begin
  if actEdit.Enabled then
    actEdit.Execute;
end;

procedure TfmReplaceCompMapList.lvMapItemsClick(Sender: TObject);
begin
  UpdateBtns;
end;

procedure TfmReplaceCompMapList.lvMapItemsColumnClick(Sender: TObject; Column: TListColumn);
var
  i: Integer;
  Cursor: IInterface;
begin
  i := Column.Index;
  if i >= 0 then
  begin
    Cursor := TempHourGlassCursor;
    Inc(i);
    if Abs(FSortOnColumn) = i then
      FSortOnColumn := -FSortOnColumn
    else
      FSortOnColumn := i;
    RefreshAll;
  end;
end;

end.


