unit GX_ClassIdentify;

interface

uses
  SysUtils, StdCtrls, Controls, Classes, Forms,
  GX_BaseForm;

type
  TfmClassIdentify = class(TfmBaseForm)
    gbxIdentifier: TGroupBox;
    lblNotes: TLabel;
    lblIdentifier: TLabel;
    edtID: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    chk_ScanRecursively: TCheckBox;
    procedure edtIDChange(Sender: TObject);
  private
    procedure GetData(out Identifier: string; out Recursive: Boolean);
    procedure SetData(const Identifier: string; Recursive: Boolean);
  public
    class function Execute(Owner: TWinControl; var Identifier: string; var Recursive: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmClassIdentify }

class function TfmClassIdentify.Execute(Owner: TWinControl; var Identifier: string;
  var Recursive: Boolean): Boolean;
var
  frm: TfmClassIdentify;
begin
  frm := TfmClassIdentify.Create(Owner);
  try
    frm.SetData(Identifier, Recursive);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(Identifier, Recursive);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmClassIdentify.GetData(out Identifier: string; out Recursive: Boolean);
begin
  Identifier := edtID.Text;
  Recursive := chk_ScanRecursively.Checked;
end;

procedure TfmClassIdentify.SetData(const Identifier: string; Recursive: Boolean);
begin
  edtID.Text := Identifier;
  chk_ScanRecursively.Checked := Recursive;
end;

procedure TfmClassIdentify.edtIDChange(Sender: TObject);
begin
  btnOK.Enabled := (edtID.Text <> '');
end;

end.
