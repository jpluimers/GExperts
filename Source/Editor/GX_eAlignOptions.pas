unit GX_eAlignOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmAlignOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    lblWhitespace: TLabel;
    edtWhitespace: TEdit;
    mmoTokens: TMemo;
    procedure mmoTokensKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{$R *.dfm}

procedure TfmAlignOptions.mmoTokensKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key= VK_ESCAPE then
    ModalResult := mrCancel;
end;

end.
