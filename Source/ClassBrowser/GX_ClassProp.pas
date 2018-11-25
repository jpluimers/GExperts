unit GX_ClassProp;

interface

uses
  StdCtrls, Controls, ComCtrls, Classes, Forms, GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmClassProp = class(TfmBaseForm)
    pgeProperties: TPageControl;
    tabProps: TTabSheet;
    lblClassName: TLabel;
    edtClassName: TEdit;
    lblDerivedFrom: TLabel;
    edtDerivedFrom: TEdit;
    lblFileName: TLabel;
    lblLineNumber: TLabel;
    edtLineNo: TEdit;
    btnOK: TButton;
    mmoFileName: TMemo;
    lblUnit: TLabel;
    edtUnit: TEdit;
  end;


implementation

{$R *.dfm}

end.
