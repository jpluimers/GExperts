unit GX_AutoTodoDone;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TfmAutoTodoDone = class(TForm)
    lblMesssage: TLabel;
    btnOK: TButton;
    chkDontShowAgain: TCheckBox;
  public
    class function Execute(ATodoCount: Integer): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmAutoTodoDone }

class function TfmAutoTodoDone.Execute(ATodoCount: Integer): Boolean;
resourcestring
  DoneMessage = '%d comments have been inserted in empty code blocks.';
var
  Dialog: TfmAutoTodoDone;
begin
  Dialog := TfmAutoTodoDone.Create(nil);
  try
    Dialog.lblMesssage.Caption := Format(DoneMessage, [ATodoCount]);
    Dialog.ShowModal;
    Result := Dialog.chkDontShowAgain.Checked;
  finally
    FreeAndNil(Dialog);
  end;
end;

end.
