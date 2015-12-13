unit GX_About;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ExtCtrls, GX_BaseForm;

type
  TfmAbout = class(TfmBaseForm)
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
    lblPreRelease1: TLabel;
    lblPreRelease2: TLabel;
    mmoBuildDetails: TMemo;
    mmoContributors: TMemo;
    procedure btnEmailClick(Sender: TObject);
    procedure lblWebPageClick(Sender: TObject);
  private
    procedure InitVersionInfoControls;
  protected
    class function GetVersionStr: string;
    class function DoAddToAboutDialog: Integer; virtual;
    class function GetAboutIcon: HBITMAP; virtual;
    class function GetSplashIcon: HBITMAP; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // If you release an experimental GExperts, either
    // set gblAboutFormClass to your own descentant of this form or
    // call SetCustomBuildDetails and SetCustomBuildEmails to
    // describe your build and provide feedback email adresses.
    class procedure SetCustomBuildDetails(const Details: string);
    class procedure SetCustomBuildEmails(const ABugEmail, ASuggestionEmail: string);
    class procedure AddToSplashScreen;
    class procedure AddToAboutDialog;
    class procedure RemoveFromAboutDialog;
  end;

type
  TAboutFormClass = class of TfmAbout;

var
  gblAboutFormClass: TAboutFormClass;

implementation

{$R *.dfm}
{$R GX_About.res}

uses
  SysUtils, Graphics, ToolsApi,
  GX_GenericUtils, GX_FeedbackWizard;

const
  DefaultBugEmail = 'bugs@gexperts.org';  // Do not localize.
  DefaultSuggestionEmail = 'suggestions@gexperts.org'; // Do not localize.
var
  BuildDetails: string = '';
  BugEmail: string = DefaultBugEmail;
  SuggestionEmail: string = DefaultSuggestionEmail;

procedure TfmAbout.btnEmailClick(Sender: TObject);
begin
  TfmFeedbackWizard.Execute(Application, BugEmail, SuggestionEmail);
  Close;
end;

procedure TfmAbout.lblWebPageClick(Sender: TObject);
var
  Lbl: TLabel;
  URL: string;
begin
  Lbl := Sender as TLabel;
  URL := Lbl.Hint;
  if URL = '' then
    URL := Lbl.Caption;
  GXShellExecute(URL, '', True);
end;

constructor TfmAbout.Create(AOwner: TComponent);
begin
  inherited;
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
  SetFontColor(mmoBuildDetails, clRed);

  imgLogo.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUT_WIZ');
  InitVersionInfoControls;

  if NotEmpty(BuildDetails) then
  begin
    if (BugEmail = DefaultBugEmail) or (SuggestionEmail = DefaultSuggestionEmail) then
      btnEmail.Visible := False;
    mmoBuildDetails.Visible := True;
    mmoBuildDetails.Lines.Text := BuildDetails;
  end
  else
  begin
    if gblAboutFormClass = TfmAbout then
      btnEmail.Visible := True;
    mmoBuildDetails.Visible := False;
  end;
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

class procedure TfmAbout.SetCustomBuildDetails(const Details: string);
begin
  BuildDetails := Details;
end;

class procedure TfmAbout.SetCustomBuildEmails(const ABugEmail, ASuggestionEmail: string);
begin
  BugEmail := ABugEmail;
  SuggestionEmail := ASuggestionEmail;
end;

class function TfmAbout.GetAboutIcon: HBITMAP;
const
  GX_ABOUT_ICON32 = 'GX32';
begin
  Result := LoadBitmap(HInstance, GX_ABOUT_ICON32);
end;

class function TfmAbout.GetSplashIcon: HBITMAP;
const
  GX_ABOUT_ICON24 = 'GX24';
begin
  Result := LoadBitmap(HInstance, GX_ABOUT_ICON24);
end;

class function TfmAbout.GetVersionStr: string;
resourcestring
  SVersion = 'Version';
  SUnknown = '<unknown>';
var
  Version: TVersionNumber;
begin
  try
    Version := GetFileVersionNumber(ThisDllName);
    Result := Format('%s %d.%d%d', [SVersion, Version.Major, Version.Minor, Version.Release]);
  except
    Result := Format('%s %s', [SVersion, SUnknown]);
  end;
  if Version.Build <> 0 then
    Result := Result + '.' + IntToStr(Version.Build);
end;

class procedure TfmAbout.AddToSplashScreen;
begin
{$IFDEF GX_VER170_up}
  // Only Delphi 2005 and up support the splash screen services
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap('GExperts',
      GetSplashIcon, False, GetVersionStr);
{$ENDIF GX_VER170_up}
end;

class function TfmAbout.DoAddToAboutDialog: Integer;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
var
  AboutBoxServices: IOTAAboutBoxServices;
begin
  Result := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
  begin
    Result := AboutBoxServices.AddPluginInfo(
      'GExperts',
      'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
      + ' programmers by adding several features to the IDE.'
      + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
      + '(c) Erik Berry and the GExperts Team'#13#10
      + 'http://www.gexperts.org',
      GetAboutIcon,
      False,
      '', // leave this empty!
      GetVersionStr);
  end;
end;
{$ELSE not GX_VER170_up}
begin
  Result := -1;
end;
{$ENDIF not GX_VER170_up}

var
  FAboutPluginIndex: Integer;

class procedure TfmAbout.AddToAboutDialog;
begin
  FAboutPluginIndex := DoAddToAboutDialog;
end;

class procedure TfmAbout.RemoveFromAboutDialog;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
var
  AboutBoxServices: IOTAAboutBoxServices;
begin
  if FAboutPluginIndex <> -1 then
  begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
      AboutBoxServices.RemovePluginInfo(FAboutPluginIndex);
    FAboutPluginIndex := -1;
  end;
end;
{$ELSE not GX_VER170_up}
begin
end;
{$ENDIF not GX_VER170_up}

initialization
  TfmAbout.AddToSplashScreen;
  gblAboutFormClass := TfmAbout;

finalization
end.

