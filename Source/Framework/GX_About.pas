unit GX_About;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls;

type
  TfmAbout = class(TForm)
    lblGExperts: TLabel;
    btnClose: TButton;
    lblVersion: TLabel;
    pnlLogo: TPanel;
    imgLogo: TImage;
    btnEmail: TButton;
    lblWebPage: TLabel;
    lblProjectLeader: TLabel;
    lblContributors: TLabel;
    lblErik: TLabel;
    lblWebSite: TLabel;
    pnlContributors: TPanel;
    lbxContributors: TListBox;
    lblPreRelease1: TLabel;
    lblPreRelease2: TLabel;
    procedure btnEmailClick(Sender: TObject);
    procedure lblWebPageClick(Sender: TObject);
  private
    procedure InitVersionInfoControls;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}
{$R GX_About.res}

uses
  SysUtils, Graphics,
  GX_GenericUtils, GX_FeedbackWizard;

procedure TfmAbout.btnEmailClick(Sender: TObject);
begin
  with TfmFeedbackWizard.Create(Application) do
    Show; // This form frees itself by setting caFree
  Close;
end;

procedure TfmAbout.lblWebPageClick(Sender: TObject);
begin
  GXShellExecute((Sender as TLabel).Caption, '', True);
end;

constructor TfmAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaultFont(Self);
  SetFontBold(lblContributors);
  SetFontBold(lblProjectLeader);
  SetFontBold(lblWebSite);
  SetFontBold(lblVersion);
  SetFontBold(lblGExperts);
  SetFontColor(lblPreRelease1, clRed);
  SetFontColor(lblPreRelease2, clRed);
  SetFontSize(lblGExperts, +4);
  SetFontSize(lblVersion, +4);
  SetFontUnderline(lblErik);
  SetFontUnderline(lblWebPage);
  SetFontColor(lblErik, clBlue);
  SetFontColor(lblWebPage, clBlue);


  imgLogo.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUT_WIZ');
  InitVersionInfoControls;
end;

procedure TfmAbout.InitVersionInfoControls;
resourcestring
  SVersion = 'Version';
  SUnknown = '<unknown>';
var
  Version: TVersionNumber;
begin
  try
    Version := GetFileVersionNumber(ThisDllName);
    lblVersion.Caption := Format('%s %d.%d%d', [SVersion, Version.Major, Version.Minor, Version.Release]);
  except
    lblVersion.Caption := Format('%s %s', [SVersion, SUnknown]);
  end;
  if Version.Build <> 0 then
    lblVersion.Caption := lblVersion.Caption + '.' + IntToStr(Version.Build);
end;

end.

