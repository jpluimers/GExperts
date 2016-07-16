unit GX_IdeSearchPathFavorites;

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
  Tf_SarchPathFavorites = class(TfmBaseForm)
    l_Favorites: TLabel;
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
    procedure SetData(_Favorites: TStrings);
    procedure GetData(_Favorites: TStrings);
    procedure EditCurrent;
  public
    class procedure Execute(_Owner: TComponent; var _Favorites: TStringList);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  GX_dzSelectDirectoryFix,
  GX_IdeSearchPathFavoriteEdit,
  GX_dzVclUtils;

{$R *.dfm}

{ Tf_SarchPathFavorites }

class procedure Tf_SarchPathFavorites.Execute(_Owner: TComponent; var _Favorites: TStringList);
var
  frm: Tf_SarchPathFavorites;
begin
  frm := Tf_SarchPathFavorites.Create(_Owner);
  try
    frm.SetData(_Favorites);
    frm.ShowModal;
    frm.GetData(_Favorites);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_SarchPathFavorites.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

procedure Tf_SarchPathFavorites.b_AddClick(Sender: TObject);
var
  li: TListItem;
  FavName: string;
  FavPath: string;
begin
  FavName := 'New Entry';
  FavPath := '';
  if not Tf_IdeSearchPathFavoriteEdit.Execute(Self, FavName, FavPath) then
    Exit;
  li := lv_Favorites.Items.Add;
  li.Caption := FavName;
  li.SubItems.Add(FavPath);
end;

procedure Tf_SarchPathFavorites.b_DeleteClick(Sender: TObject);
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

procedure Tf_SarchPathFavorites.b_EditClick(Sender: TObject);
begin
  EditCurrent;
end;

procedure Tf_SarchPathFavorites.EditCurrent;
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
  if not Tf_IdeSearchPathFavoriteEdit.Execute(Self, FavName, FavPath) then
    Exit;
  li.Caption := FavName;
  li.SubItems[0] := FavPath;
end;

procedure Tf_SarchPathFavorites.GetData(_Favorites: TStrings);
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

procedure Tf_SarchPathFavorites.lv_FavoritesDblClick(Sender: TObject);
begin
  EditCurrent;
end;

procedure Tf_SarchPathFavorites.SetData(_Favorites: TStrings);
var
  i: Integer;
  FavName: string;
  FavValue: string;
  li: TListItem;
begin
  for i := 0 to _Favorites.Count - 1 do begin
    FavName := _Favorites.Names[i];
    FavValue := _Favorites.Values[FavName];
    li := lv_Favorites.Items.Add;
    li.Caption := FavName;
    li.SubItems.Add(FavValue);
  end;
end;

end.
