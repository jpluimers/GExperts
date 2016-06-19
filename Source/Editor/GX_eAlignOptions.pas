unit GX_eAlignOptions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmAlignOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    lblWhitespace: TLabel;
    edtWhitespace: TEdit;
    mmoTokens: TMemo;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  end;

implementation

{$R *.dfm}

procedure TfmAlignOptions.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key = Chr(27) then
    ModalResult := mrCancel;
end;

end.
