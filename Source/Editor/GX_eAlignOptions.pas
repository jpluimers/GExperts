unit GX_eAlignOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmAlignOptions = class(TfmBaseForm)
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
