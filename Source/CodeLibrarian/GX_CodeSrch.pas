unit GX_CodeSrch;

interface

uses
  Classes, Controls, Forms, StdCtrls;

type
  TfmCodeSearch = class(TForm)
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
