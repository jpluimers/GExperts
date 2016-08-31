unit GX_AboutExperimental;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_About,
  ExtCtrls,
  StdCtrls;

type
  TfmAboutExperimental = class(TfmAbout)
    l_CreatedBy: TLabel;
    l_DummzeuchDe: TLabel;
    l_Formatter: TLabel;
  protected
    class function DoAddToAboutDialog: integer; override;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  GX_GenericUtils;

{ TfmAboutExperimental }

constructor TfmAboutExperimental.Create(_Owner: TComponent);
begin
  inherited;

  SetFontUnderline(l_DummzeuchDe);
  SetFontColor(l_DummzeuchDe, clBlue);
  SetFontUnderline(l_Formatter);
  SetFontColor(l_Formatter, clBlue);
end;

class function TfmAboutExperimental.DoAddToAboutDialog: integer;
{$IFDEF GX_VER170_up}
// Only Delphi 2005 and up support the about box services
var
  AboutBoxServices: IOTAAboutBoxServices;
begin
  Result := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
    Result := AboutBoxServices.AddPluginInfo(
      'GExperts Experimental',
      'GExperts is a free set of tools built to increase the productivity of Delphi and C++Builder'
      + ' programmers by adding several features to the IDE.'
      + ' GExperts is developed as Open Source software and we encourage user contributions to the project.'#13#10
      + '(c) Erik Berry and the GExperts Team'#13#10
      + 'http://blog.dummzeuch.de',
      GetAboutIcon,
      False,
      '', // leave this empty!
      GetVersionStr + ' experimental');
  end;
end;
{$ELSE not GX_VER170_up}
begin
  Result := -1;
end;
{$ENDIF not GX_VER170_up}

initialization
  TfmAboutExperimental.SetCustomBuildEmails('gexperts@dummzeuch.de', 'gexperts@dummzeuch.de');
  gblAboutFormClass := TfmAboutExperimental;
end.

