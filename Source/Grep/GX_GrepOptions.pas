unit GX_GrepOptions;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmGrepOptions = class(TfmBaseForm)
    gbxOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepUseCurrentIdent: TCheckBox;
  public
    class function Execute(var UseCurrentIdent: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmGrepOptions }

class function TfmGrepOptions.Execute(var UseCurrentIdent: Boolean): Boolean;
var
  Dlg: TfmGrepOptions;
begin
  Dlg := TfmGrepOptions.Create(nil);
  try
    Dlg.chkGrepUseCurrentIdent.Checked := UseCurrentIdent;
    Result := (Dlg.ShowModal = mrOk);
    if Result then
    begin
      UseCurrentIdent := Dlg.chkGrepUseCurrentIdent.Checked;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

end.
