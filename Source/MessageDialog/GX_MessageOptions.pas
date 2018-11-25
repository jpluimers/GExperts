unit GX_MessageOptions;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  GX_BaseForm,
  GX_MessageDialog;

type
  TfmMessageOptions = class(TfmBaseForm)
    lblCrLfPascal: TLabel;
    edtCrLfPascal: TEdit;
    lblCrLfCPP: TLabel;
    edtCrLfCPP: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    gbxCrLfString: TGroupBox;
    grpGnuGettext: TGroupBox;
    chkGetTextIndividual: TCheckBox;
    rbGgtFuncUnderscore: TRadioButton;
    rbGgtFuncGetText: TRadioButton;
  private
    procedure SetData(const _CrLfPascal: string; const _CrLfCPP: string;
      _GunGetTextFunction: TGnuGetTextFunction; _GnuGetTextIndividual: Boolean);
    procedure GetData(out _CrLfPascal: string; out _CrLfCPP: string;
      out _GunGetTextFunction: TGnuGetTextFunction; out _GnuGetTextIndividual: Boolean);
  public
    class function Execute(_Owner: TWinControl; var _CrLfPascal: string; var _CrLfCPP: string;
      var _GunGetTextFunction: TGnuGetTextFunction; var _GnuGetTextIndividual: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmMessageOptions }

class function TfmMessageOptions.Execute(_Owner: TWinControl; var _CrLfPascal, _CrLfCPP: string;
  var _GunGetTextFunction: TGnuGetTextFunction; var _GnuGetTextIndividual: Boolean): Boolean;
var
  frm: TfmMessageOptions;
begin
  frm := TfmMessageOptions.Create(_Owner);
  try
    frm.SetData(_CrLfPascal, _CrLfCPP, _GunGetTextFunction, _GnuGetTextIndividual);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_CrLfPascal, _CrLfCPP, _GunGetTextFunction, _GnuGetTextIndividual);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmMessageOptions.GetData(out _CrLfPascal, _CrLfCPP: string;
  out _GunGetTextFunction: TGnuGetTextFunction; out _GnuGetTextIndividual: Boolean);
begin
  _CrLfPascal := edtCrLfPascal.Text;
  _CrLfCPP := edtCrLfCPP.Text;
  if rbGgtFuncUnderscore.Checked then
    _GunGetTextFunction := ggtUnderscore
  else
    _GunGetTextFunction := ggtGetText;
  _GnuGetTextIndividual := chkGetTextIndividual.Checked;
end;

procedure TfmMessageOptions.SetData(const _CrLfPascal, _CrLfCPP: string;
  _GunGetTextFunction: TGnuGetTextFunction; _GnuGetTextIndividual: Boolean);
begin
  edtCrLfPascal.Text := _CrLfPascal;
  edtCrLfCPP.Text := _CrLfCPP;
  if _GunGetTextFunction = ggtUnderscore then
    rbGgtFuncUnderscore.Checked := True
  else
    rbGgtFuncGetText.Checked := True;
  chkGetTextIndividual.Checked := _GnuGetTextIndividual;
end;

end.
