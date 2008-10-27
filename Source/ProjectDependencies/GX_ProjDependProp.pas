unit GX_ProjDependProp;

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, Forms, ExtCtrls;

type
  TfmProjDependProp = class(TForm)
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    pnlContent: TPanel;
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    lblFileName: TLabel;
    lblSource: TLabel;
    laFileName: TStaticText;
    lbxSource: TListBox;
  end;

implementation

{$R *.dfm}

end.
