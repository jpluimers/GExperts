unit GX_GrepOptions;

interface

uses
  Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmGrepOptions = class(TfmBaseForm)
    gbxOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepUseCurrentIdent: TCheckBox;
  end;

implementation

{$R *.dfm}

end.
