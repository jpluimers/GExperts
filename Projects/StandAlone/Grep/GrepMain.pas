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
type
  TShowGrep = procedure;
  TShowGrepEx = procedure(_HWnd: HWND; _HInst: HINST; _CmdLine: PAnsiChar; nCmdShow: Integer); stdcall;
var
  ShowGrep: TShowGrep;
  ShowGrepEx: TShowGrepEx;
  Dll: IGExpertsDll;
  Dir: AnsiString;
begin
  try
    SetErrorMode(SEM_FAILCRITICALERRORS);
    Dll := LoadAnyGExpertsDLL;
    if ParamCount > 0 then begin
      ShowGrepEx := Dll.GetProcAddress('ShowGrepEx');
      Dir := ParamStr(1);
      ShowGrepEx(0, Dll.ModuleHandle, PAnsiChar(Dir), SW_SHOWNORMAL);
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
