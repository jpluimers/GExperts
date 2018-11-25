unit GX_FavOptions;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  GX_BaseForm;

type
  TfmFavOptions = class(TfmBaseForm)
    gbxFavOptions: TGroupBox;
    chkConfirmFolderDelete: TCheckBox;
    chkExpandAllOnLoad: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkHideOnExecute: TCheckBox;
    chkShowPreview: TCheckBox;
    chk_InsertFavMenu: TCheckBox;
  private
    procedure SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean);
    procedure GetData(out _FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu: Boolean);
  public
    class function Execute(_Owner: TWinControl; var _FolderDelete, _ExpandAll: Boolean;
      var _ExecHide, _ShowPreview: Boolean;
      var _InsertFavMenu: Boolean): Boolean;
{$IFNDEF GX_VER150_up} // Delphi 7
    constructor Create(_Owner: TComponent); override;
{$ENDIF}
  end;

implementation

{$R *.dfm}

{ TfmFavOptions }

class function TfmFavOptions.Execute(_Owner: TWinControl; var _FolderDelete, _ExpandAll: Boolean;
  var _ExecHide, _ShowPreview: Boolean;
  var _InsertFavMenu: Boolean): Boolean;
var
  frm: TfmFavOptions;
begin
  frm := TfmFavOptions.Create(nil);
  try
    frm.SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview, _InsertFavMenu);
    end;
  finally
    FreeAndNil(frm);
  end;

end;

{$IFNDEF GX_VER150_up} // Delphi 7

constructor TfmFavOptions.Create(_Owner: TComponent);
begin
  inherited;
  chk_InsertFavMenu.Enabled := False;
  chk_InsertFavMenu.Caption := chk_InsertFavMenu.Caption + ' (not available in Delphi 6)';
end;
{$ENDIF}

procedure TfmFavOptions.GetData(out _FolderDelete, _ExpandAll, _ExecHide, _ShowPreview: Boolean;
  out _InsertFavMenu: Boolean);
begin
  _FolderDelete := chkConfirmFolderDelete.Checked;
  _ExpandAll := chkExpandAllOnLoad.Checked;
  _ExecHide := chkHideOnExecute.Checked;
  _ShowPreview := chkShowPreview.Checked;
  _InsertFavMenu := chk_InsertFavMenu.Checked;
end;

procedure TfmFavOptions.SetData(_FolderDelete, _ExpandAll, _ExecHide, _ShowPreview: Boolean;
  _InsertFavMenu: Boolean);
begin
  chkConfirmFolderDelete.Checked := _FolderDelete;
  chkExpandAllOnLoad.Checked := _ExpandAll;
  chkHideOnExecute.Checked := _ExecHide;
  chkShowPreview.Checked := _ShowPreview;
  chk_InsertFavMenu.Checked := _InsertFavMenu;
end;

end.
