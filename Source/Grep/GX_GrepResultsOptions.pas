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
    pnlContextMatchFontColor: TPanel;
    dlgContextMatchFontColor: TColorDialog;
    chkGrepMiddle: TCheckBox;
    chkGrepExpandAll: TCheckBox;
    edtContextLines: TEdit;
    udContextLines: TUpDown;
    gbxFoundList: TGroupBox;
    chkSaveFoundListSize: TCheckBox;
    chkSaveContextSize: TCheckBox;
    gbxListColors: TGroupBox;
    pnlListMatchTextColor: TPanel;
    chkDefaultListColors: TCheckBox;
    pnlListMatchBackgroundColor: TPanel;
    dlgListMatchTextColor: TColorDialog;
    dlgListMatchBackgroundColor: TColorDialog;
    pnlContextMacthLineFontColor: TPanel;
    dlgContextMatchLineFontColor: TColorDialog;
    chkGrepAutoHide: TCheckBox;
    chkGrepSaveResultListItems: TCheckBox;
    chkGrepExpandIf: TCheckBox;
    eExpandIfMatches: TEdit;
    eExpandIfFiles: TEdit;
    lblExpandIfMatches: TLabel;
    lblExpandIfFiles: TLabel;
    chkGrepExpandFew: TCheckBox;
    lblExpandFewLines: TLabel;
    eExpandFewLines: TEdit;
    procedure pnlContextFontClick(Sender: TObject);
    procedure pnlContextMatchFontColorClick(Sender: TObject);
    procedure pnlListFontClick(Sender: TObject);
    procedure pnlListMatchTextColorClick(Sender: TObject);
    procedure pnlListMatchBackgroundColorClick(Sender: TObject);
    procedure pnlContextMacthLineFontColorClick(Sender: TObject);
    procedure chkDefaultListColorsClick(Sender: TObject);
    procedure chkGrepExpandClick(Sender: TObject);
  private
    FExpandsChanging: Boolean;
  end;

implementation

uses Graphics;

{$R *.dfm}

procedure TfmGrepResultsOptions.pnlListFontClick(Sender: TObject);
var
  AColor: TColor;
begin
  dlgGrepListFont.Font.Assign(pnlListFont.Font);
  if dlgGrepListFont.Execute then
  begin
    pnlListFont.Font.Assign(dlgGrepListFont.Font);
    pnlListFont.Refresh;
    AColor := pnlListMatchTextColor.Font.Color;
    pnlListMatchTextColor.Font.Assign(dlgGrepListFont.Font);
    pnlListMatchTextColor.Font.Color := AColor;
    pnlListMatchTextColor.Refresh;
    pnlListMatchBackgroundColor.Font.Assign(dlgGrepListFont.Font);
    pnlListMatchBackgroundColor.Font.Color := AColor;
    pnlListMatchBackgroundColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlListMatchTextColorClick(Sender: TObject);
begin
  dlgListMatchTextColor.Color := pnlListMatchTextColor.Font.Color;
  if dlgListMatchTextColor.Execute then
  begin
    pnlListMatchTextColor.Font.Color := dlgListMatchTextColor.Color;
    pnlListMatchTextColor.Refresh;
    pnlListMatchBackgroundColor.Font.Color := dlgListMatchTextColor.Color;
    pnlListMatchBackgroundColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlListMatchBackgroundColorClick(Sender: TObject);
begin
  dlgListMatchBackgroundColor.Color := pnlListMatchBackgroundColor.Color;
  if dlgListMatchBackgroundColor.Execute then
  begin
    pnlListMatchBackgroundColor.Color := dlgListMatchBackgroundColor.Color;
    pnlListMatchBackgroundColor.Refresh;
    pnlListMatchTextColor.Color := dlgListMatchBackgroundColor.Color;
    pnlListMatchTextColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlContextFontClick(Sender: TObject);
var
  AColor: TColor;
begin
  dlgGrepContextFont.Font.Assign(pnlContextFont.Font);
  if dlgGrepContextFont.Execute then
  begin
    pnlContextFont.Font.Assign(dlgGrepContextFont.Font);
    pnlContextFont.Refresh;
    AColor := pnlContextMatchFontColor.Font.Color;
    pnlContextMatchFontColor.Font.Assign(dlgGrepContextFont.Font);
    pnlContextMatchFontColor.Font.Color := AColor;
    pnlContextMatchFontColor.Refresh;
    AColor := pnlContextMacthLineFontColor.Font.Color;
    pnlContextMacthLineFontColor.Font.Assign(dlgGrepContextFont.Font);
    pnlContextMacthLineFontColor.Font.Color := AColor;
    pnlContextMacthLineFontColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlContextMacthLineFontColorClick(Sender: TObject);
begin
  dlgContextMatchLineFontColor.Color := pnlContextMacthLineFontColor.Font.Color;
  if dlgContextMatchLineFontColor.Execute then
  begin
    pnlContextMacthLineFontColor.Font.Color := dlgContextMatchLineFontColor.Color;
    pnlContextMacthLineFontColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.pnlContextMatchFontColorClick(Sender: TObject);
begin
  dlgContextMatchFontColor.Color := pnlContextMatchFontColor.Font.Color;
  if dlgContextMatchFontColor.Execute then
  begin
    pnlContextMatchFontColor.Font.Color := dlgContextMatchFontColor.Color;
    pnlContextMatchFontColor.Refresh;
  end;
end;

procedure TfmGrepResultsOptions.chkDefaultListColorsClick(Sender: TObject);
begin
  if chkDefaultListColors.Checked then begin
    dlgListMatchTextColor.Color := pnlListMatchTextColor.Color;
    pnlListMatchTextColor.Enabled := False;
    pnlListMatchTextColor.Color := gbxListColors.Color;
    pnlListMatchTextColor.Font.Color := clGrayText;

    dlgListMatchBackgroundColor.Color := pnlListMatchBackgroundColor.Color;
    pnlListMatchBackgroundColor.Enabled := False;
    pnlListMatchBackgroundColor.Color := gbxListColors.Color;
    pnlListMatchBackgroundColor.Font.Color := clGrayText;
  end else begin
    pnlListMatchTextColor.Enabled := True;
    pnlListMatchTextColor.Color := dlgListMatchTextColor.Color;
//    pnlListMatchTextColor.Font.Color := gbxListColors.Font.Color;
    pnlListMatchBackgroundColor.Enabled := True;
    pnlListMatchBackgroundColor.Color := dlgListMatchBackgroundColor.Color;
//    pnlListMatchBackgroundColor.Font.Color := gbxListColors.Font.Color;
  end;
end;

procedure TfmGrepResultsOptions.chkGrepExpandClick(Sender: TObject);
begin
  if FExpandsChanging then
    Exit;

  FExpandsChanging := True;
  try
    chkGrepExpandAll.Checked := Sender = chkGrepExpandAll;
    chkGrepExpandIf.Checked := Sender = chkGrepExpandIf;
    chkGrepExpandFew.Checked := Sender = chkGrepExpandFew;

    lblExpandIfMatches.Enabled := chkGrepExpandIf.Checked;
    eExpandIfMatches.Enabled := chkGrepExpandIf.Checked;
    lblExpandIfFiles.Enabled := chkGrepExpandIf.Checked;
    eExpandIfFiles.Enabled := chkGrepExpandIf.Checked;

    lblExpandFewLines.Enabled := chkGrepExpandFew.Checked;
    eExpandFewLines.Enabled := chkGrepExpandFew.Checked;
  finally
    FExpandsChanging := False;
  end;
end;

end.
