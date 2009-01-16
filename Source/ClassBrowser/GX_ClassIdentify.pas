unit GX_ClassIdentify;

interface

uses
  StdCtrls, Controls, Classes, Forms, GX_BaseForm;

type
  TfmClassIdentify = class(TfmBaseForm)
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
