unit GX_eAlignOptions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, StdCtrls, Forms;

type
  TfmAlignOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    lblWhitespace: TLabel;
    edtWhitespace: TEdit;
    mmoTokens: TMemo;
  end;

implementation

{$R *.dfm}

end.
