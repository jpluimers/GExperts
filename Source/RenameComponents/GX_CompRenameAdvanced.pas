unit GX_CompRenameAdvanced;

interface

uses
  Classes, Controls, StdCtrls, Forms;

type
  TfmCompRenameAdvanced = class(TForm)
    lblComponentClass: TLabel;
    lblProperties: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    mmoPropertyNames: TMemo;
  end;

implementation

{$R *.dfm}

end.
