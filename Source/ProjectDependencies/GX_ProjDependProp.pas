unit GX_ProjDependProp;

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, Forms;

type
  TfmProjDependProp = class(TForm)
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    btnOK: TButton;
    lblFileName: TLabel;
    laFileName: TStaticText;
    lbxSource: TListBox;
    lblSource: TLabel;
  end;

implementation

{$R *.dfm}

end.
