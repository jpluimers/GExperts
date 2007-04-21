unit GX_ClassIdentify;

interface

uses
  StdCtrls, Controls, Classes, Forms;

type
  TfmClassIdentify = class(TForm)
    gbxIdentifier: TGroupBox;
    lblNotes: TLabel;
    lblIdentifier: TLabel;
    edtID: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

end.
