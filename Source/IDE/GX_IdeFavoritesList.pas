unit GX_IdeFavoritesList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_BaseForm,
  StdCtrls,
  ComCtrls;

type
  TOnEditFavoritesEntry = procedure(_Sender: TWinControl;
    var _Name, _Value: string; var _OK: Boolean) of object;

type
  Tf_GxIdeFavoritesList = class(TfmBaseForm)
    b_Close: TButton;
    lv_Favorites: TListView;
    b_Add: TButton;
    b_Edit: TButton;
    b_Delete: TButton;
    procedure b_AddClick(Sender: TObject);
    procedure b_EditClick(Sender: TObject);
    procedure b_DeleteClick(Sender: TObject);
    procedure lv_FavoritesDblClick(Sender: TObject);
  private
    FOnEdit: TOnEditFavoritesEntry;
    procedure SetData(const _Caption: string; _OnEdit: TOnEditFavoritesEntry; _Favorites: TStrings);
    procedure GetData(_Favorites: TStrings);
    procedure EditCurrent;
    function doOnEdit(var _Name, _Value: string): Boolean;
  public
    class procedure Execute(_Owner: TComponent; const _Caption: string;
      _OnEdit: TOnEditFavoritesEntry; var _Favorites: TStringList);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  GX_dzVclUtils;

{$R *.dfm}

{ Tf_GxIdeFavoritesList }

class procedure Tf_GxIdeFavoritesList.Execute(_Owner: TComponent; const _Caption: string;
  _OnEdit: TOnEditFavoritesEntry; var _Favorites: TStringList);
var
  frm: Tf_GxIdeFavoritesList;
begin
  frm := Tf_GxIdeFavoritesList.Create(_Owner);
  try
    frm.SetData(_Caption, _OnEdit, _Favorites);
    frm.ShowModal;
    frm.GetData(_Favorites);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_GxIdeFavoritesList.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

procedure Tf_GxIdeFavoritesList.b_AddClick(Sender: TObject);
var
  li: TListItem;
  FavName: string;
  FavPath: string;
begin
  FavName := 'New Entry';
  FavPath := '';
  if not doOnEdit(FavName, FavPath) then
    Exit;
  li := lv_Favorites.Items.Add;
  li.Caption := FavName;
  li.SubItems.Add(FavPath);
end;

procedure Tf_GxIdeFavoritesList.b_DeleteClick(Sender: TObject);
var
  li: TListItem;
begin
  li := lv_Favorites.Selected;
  if not Assigned(li) then
    Exit;
  if MessageDlg('Delete this entry?', mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    Exit;
  lv_Favorites.DeleteSelected;
end;

procedure Tf_GxIdeFavoritesList.b_EditClick(Sender: TObject);
begin
  EditCurrent;
end;

function Tf_GxIdeFavoritesList.doOnEdit(var _Name, _Value: string): Boolean;
begin
  Result := False;
  if Assigned(FOnEdit) then
    FOnEdit(Self, _Name, _Value, Result);
end;

procedure Tf_GxIdeFavoritesList.EditCurrent;
var
  li: TListItem;
  FavName: string;
  FavPath: string;
begin
  li := lv_Favorites.Selected;
  if not Assigned(li) then
    Exit;
  FavName := li.Caption;
  FavPath := li.SubItems[0];
  if not doOnEdit(FavName, FavPath) then
    Exit;
  li.Caption := FavName;
  li.SubItems[0] := FavPath;
end;

procedure Tf_GxIdeFavoritesList.GetData(_Favorites: TStrings);
var
  i: Integer;
  FavName: string;
  FavValue: string;
  li: TListItem;
begin
  for i := 0 to lv_Favorites.Items.Count - 1 do begin
    li := lv_Favorites.Items[i];
    FavName := li.Caption;
    FavValue := li.SubItems[0];
    _Favorites.Values[FavName] := FavValue;
  end;
end;

procedure Tf_GxIdeFavoritesList.lv_FavoritesDblClick(Sender: TObject);
begin
  EditCurrent;
end;

procedure Tf_GxIdeFavoritesList.SetData(const _Caption: string; _OnEdit: TOnEditFavoritesEntry;
  _Favorites: TStrings);
var
  i: Integer;
  FavName: string;
  FavValue: string;
  li: TListItem;
begin
  Caption := _Caption;
  FOnEdit := _OnEdit;
  for i := 0 to _Favorites.Count - 1 do begin
    FavName := _Favorites.Names[i];
    FavValue := _Favorites.Values[FavName];
    li := lv_Favorites.Items.Add;
    li.Caption := FavName;
    li.SubItems.Add(FavValue);
  end;
end;

end.
