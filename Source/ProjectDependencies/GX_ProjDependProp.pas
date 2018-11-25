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
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  GX_dzVclUtils;

{$R *.dfm}

{ TfmProjDependProp }

constructor TfmProjDependProp.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

end.
