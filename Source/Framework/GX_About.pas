unit GX_About;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, GX_BaseForm;

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
    class function doAddToAboutDialog: integer; virtual;
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
  Windows, SysUtils, Graphics, ToolsApi,
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
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the splash screen services
var
  bmSplashScreen: HBITMAP;
{$ENDIF GX_VER170_up}
begin
{$IFDEF GX_VER170_up}
  if Assigned(SplashScreenServices) then begin
    bmSplashScreen := LoadBitmap(HInstance, 'SplashScreenBitMap');
    SplashScreenServices.AddPluginBitmap(
      'GExperts',
      bmSplashScreen,
      False,
      GetVersionStr);
  end;
{$ENDIF GX_VER170_up}
end;

class function TfmAbout.doAddToAboutDialog: integer;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
var
  bmSplashScreen: HBITMAP;
  AboutBoxServices: IOTAAboutBoxServices;
{$ENDIF GX_VER170_up}
begin
  Result := -1;
{$IFDEF GX_VER170_up}
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    bmSplashScreen := LoadBitmap(HInstance, 'SplashScreenBitMap');
    Result := AboutBoxServices.AddPluginInfo(
      'GExperts',
      'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
      + ' programmers by adding several features to the IDE.'
      + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
      + '(c) by Erik Berry and the GExperts Team'#13#10
      + 'http://www.gexperts.org',
      bmSplashScreen,
      False,
      GetVersionStr, 'Open Source');
  end;
{$ENDIF GX_VER170_up}
end;

var
  FAboutPluginIndex: Integer;

class procedure TfmAbout.AddToAboutDialog;
begin
  FAboutPluginIndex := doAddToAboutDialog;
end;

class procedure TfmAbout.RemoveFromAboutDialog;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
var
  AboutBoxServices: IOTAAboutBoxServices;
{$ENDIF GX_VER170_up}
begin
{$IFDEF GX_VER170_up}
  if FAboutPluginIndex <> -1 then
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
      AboutBoxServices.RemovePluginInfo(FAboutPluginIndex);
{$ENDIF GX_VER170_up}
end;

initialization
  TfmAbout.AddToSplashScreen;
  gblAboutFormClass := TfmAbout;
finalization
end.

