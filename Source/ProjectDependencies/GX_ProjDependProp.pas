unit GX_ProjDependProp;

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, Forms, ExtCtrls, GX_BaseForm;

type
  TfmProjDependProp = class(TfmBaseForm)
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
