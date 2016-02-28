unit GX_AutoTodoDone;

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
  StdCtrls;

type
  TfmAutoTodoDone = class(TForm)
    l_Blurb: TLabel;
    b_Ok: TButton;
    chk_DontShowAgain: TCheckBox;
  private
  public
    class function Execute(ATodoCount: Integer): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmAutoTodoDone }

class function TfmAutoTodoDone.Execute(ATodoCount: Integer): Boolean;
resourcestring
  str_Done = '%d todos have been inserted.';
var
  frm: TfmAutoTodoDone;
begin
  frm := TfmAutoTodoDone.Create(nil);
  try
    frm.l_Blurb.Caption := Format(str_Done, [ATodoCount]);
    frm.ShowModal;
    Result := frm.chk_DontShowAgain.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

end.
