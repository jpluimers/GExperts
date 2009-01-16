unit GX_CodeSrch;

interface

uses
  Classes, Controls, Forms, StdCtrls, GX_BaseForm;

type
  TfmCodeSearch = class(TfmBaseForm)
    lblFind: TLabel;
    edSearch: TEdit;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbWholeWord: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

end.
