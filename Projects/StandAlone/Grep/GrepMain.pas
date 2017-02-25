unit GrepMain;

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
  TShowGrep = procedure;
var
  ShowGrep: TShowGrep;
  Dll: IGExpertsDll;
begin
  try
    Dll := LoadAnyGExpertsDLL;
    ShowGrep := Dll.GetProcAddress('ShowGrep');
    ShowGrep;
    // it worked halt the program
    Halt(0);
  except
    on e: Exception do begin
      Application.ShowException(e);
    end;
  end;
end;

end.
