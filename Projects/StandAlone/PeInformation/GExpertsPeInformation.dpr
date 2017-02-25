program GExpertsPeInformation;

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  Forms,
  GX_StandAloneLoadDLL in '..\..\..\Source\Framework\GX_StandAloneLoadDLL.pas';

procedure Main;
type
  TShowPeInfo = procedure(CmdLine: PAnsiChar); cdecl;
var
  ShowPeInfo: TShowPeInfo;
  s: AnsiString;
  Dll: IGExpertsDll;
begin
  try
    Dll := LoadAnyGExpertsDLL;
    ShowPeInfo := Dll.GetProcAddress('ShowPeInfo');
    s := ParamStr(1);
    ShowPeInfo(PAnsiChar(s));
    // it worked halt the program
    Halt(0);
  except
    on e: Exception do begin
      Application.ShowException(e);
    end;
  end;
end;

begin
  Main;
end.
