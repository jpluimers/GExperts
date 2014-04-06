unit GX_IconMessageBox;

interface

uses
  GX_MessageBox;

type
  TShowMissingIconMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

implementation

uses SysUtils;

{ TShowMissingIconMessage }

function TShowMissingIconMessage.GetMessage: string;
resourcestring
  SBadIconFile =
    'Some of the default GExperts icons are missing.  Make sure that the icons ' +
    'exist in a directory called "Icons" underneath the main GExperts ' +
    'installation directory and is included in "GXIcons.rc".  The file missing is: %s';
begin
  Result := Format(SBadIconFile, [FData]);
end;

var
  ShownOnce: Boolean = False;

function TShowMissingIconMessage.ShouldShow: Boolean;
begin
  Result := not ShownOnce;
  ShownOnce := True;
end;


end.
