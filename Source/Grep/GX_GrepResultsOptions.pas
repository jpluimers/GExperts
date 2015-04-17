unit GX_GrepResultsOptions;

interface

uses
  StdCtrls, Dialogs, Controls, ExtCtrls, Classes, Forms, ComCtrls, GX_BaseForm;

type
  TfmGrepResultsOptions = class(TfmBaseForm)
    gbxMatchList: TGroupBox;
    pnlListFont: TPanel;
    gbxMatchContext: TGroupBox;
    pnlContextFont: TPanel;
    lblContextLines: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    dlgGrepListFont: TFontDialog;
    dlgGrepContextFont: TFontDialog;
    pnlMatchLineColor: TPanel;
    dlgContextFontColor: TColorDialog;
    chkGrepMiddle: TCheckBox;
    chkGrepExpandAll: TCheckBox;
    edtContextLines: TEdit;
    udContextLines: TUpDown;
    chkGrepAutoHide: TCheckBox;
    procedure pnlContextFontClick(Sender: TObject);
    procedure pnlMatchLineColorClick(Sender: TObject);
    procedure pnlListFontClick(Sender: TObject);
  end;

implementation

uses Graphics;

{$R *.dfm}

procedure TfmGrepResultsOptions.pnlContextFontClick(Sender: TObject);
var
  MatchCol: TColor;
begin
  dlgGrepContextFont.Font.Assign(pnlContextFont.Font);
  if dlgGrepContextFont.Execute then begin
    MatchCol := pnlMatchLineColor.Font.Color;
    pnlContextFont.Font.Assign(dlgGrepContextFont.Font);
    pnlContextFont.Refresh;
    pnlMatchLineColor.Font.Assign(dlgGrepContextFont.Font);
    pnlMatchLineColor.Font.Color := MatchCol;
    pnlMatchLineColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlMatchLineColorClick(Sender: TObject);
begin
  dlgContextFontColor.Color := pnlMatchLineColor.Font.Color;
  if dlgContextFontColor.Execute then begin
    pnlMatchLineColor.Font.Color := dlgContextFontColor.Color;
    pnlMatchLineColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlListFontClick(Sender: TObject);
begin
  dlgGrepListFont.Font.Assign(pnlListFont.Font);
  if dlgGrepListFont.Execute then begin
    pnlListFont.Font.Assign(dlgGrepListFont.Font);
    pnlListFont.Refresh;
  end;
end;

end.
