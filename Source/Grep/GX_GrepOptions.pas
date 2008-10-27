unit GX_GrepOptions;

interface

uses
  Classes, Controls, StdCtrls, Forms;

type
  TfmGrepOptions = class(TForm)
    gbxOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepUseCurrentIdent: TCheckBox;
  end;

implementation

{$R *.dfm}

end.
