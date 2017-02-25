program CodeLibrarian;

{$R *_icon.res}

uses
  SysUtils,
  Forms,
  GX_StandAloneLoadDLL in '..\..\..\Source\Framework\GX_StandAloneLoadDLL.pas';

procedure Main;
type
  TShowCodeLib = procedure; // not cdecl, but it doesn't really matter since it dosn't have parameters.
var
  ShowCodeLib: TShowCodeLib;
  Dll: IGExpertsDll;
begin
  try
    Dll := LoadAnyGExpertsDLL;
    ShowCodeLib := Dll.GetProcAddress('ShowCodeLib');
    ShowCodeLib;
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
