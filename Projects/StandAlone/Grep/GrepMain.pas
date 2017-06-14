unit GrepMain;

interface

procedure Main;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  GX_StandAloneLoadDLL;

procedure Main;
const
{$IFDEF unicode}
  ShowGrepExName = 'ShowGrepExW';
{$ELSE}
  ShowGrepExName = 'ShowGrepExA';
{$ENDIF}
type
  TShowGrep = procedure;
{$IFDEF unicode}
  TShowGrepEx = procedure(const _Directory: PWideChar);
{$ELSE}
  TShowGrepEx = procedure(const _Directory: PAnsiChar);
{$ENDIF}
var
  ShowGrep: TShowGrep;
  ShowGrepEx: TShowGrepEx;
  Dll: IGExpertsDll;
begin
  try
    SetErrorMode(SEM_FAILCRITICALERRORS);
    Dll := LoadAnyGExpertsDLL;
    if ParamCount > 0 then begin
      ShowGrepEx := Dll.GetProcAddress(ShowGrepExName);
      ShowGrepEx(PChar(ParamStr(1)));
    end else begin
      ShowGrep := Dll.GetProcAddress('ShowGrep');
      ShowGrep;
    end;
    // it worked halt the program
    Halt(0);
  except
    on e: Exception do begin
      Application.ShowException(e);
    end;
  end;
end;

end.
