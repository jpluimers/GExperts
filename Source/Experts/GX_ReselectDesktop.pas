unit GX_ReselectDesktop;

interface

uses
  SysUtils,
  Classes,
  Forms,
  GX_Experts;

type
  TReselectDesktopExpert = class(TGX_Expert)
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetHelpString: string; override;
  end;

implementation

uses
  StdCtrls,
  Messages,
  Windows,
  GX_IdeUtils;

{ TReslectDesktopExpert }

type
  TComboBoxHack = class(TComboBox)
  end;

procedure TReselectDesktopExpert.Execute(Sender: TObject);
var
  cbDesktop: TComboBox;
begin
  if not TryGetDesktopCombo(cbDesktop) then
    Exit; //==>
  TComboBoxHack(cbDesktop).Click;
  IncCallCount;
end;

function TReselectDesktopExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Reselect Desktop';
begin
  Result := SMenuCaption;
end;

function TReselectDesktopExpert.GetHelpString: string;
resourcestring
  SReselectDesktopHelpString =
    '  This expert does the same as selecting the currently active desktop again'#13#10
    + '  so it saves one mouse click or several key strokes.'#13#10
    + '  It is meant to be put as an icon onto the Desktop toolbar.';
begin
  Result := SReselectDesktopHelpString;
end;

class function TReselectDesktopExpert.GetName: string;
begin
  Result := 'ReselectDesktop';
end;

function TReselectDesktopExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TReselectDesktopExpert);
end.
