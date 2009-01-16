unit GX_CompRenameAdvanced;

interface

uses
  Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmCompRenameAdvanced = class(TfmBaseForm)
    lblComponentClass: TLabel;
    lblProperties: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    mmoPropertyNames: TMemo;
  end;

implementation

{$R *.dfm}

end.
