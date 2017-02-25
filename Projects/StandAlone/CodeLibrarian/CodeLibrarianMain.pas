unit CodeLibrarianMain;

interface

procedure Main;

implementation

uses
  SysUtils,
  Classes,
  Forms,
  GX_StandAloneLoadDLL;

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

end.
