program ExpertManager;

{$R *.res}

uses
  Windows,
  SysUtils,
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

type
  TShowExpertManager = procedure;

var
  GExpertsDLLName: string;
  Handle: HMODULE;
  ShowExpertManager: TShowExpertManager;
begin
  if ParamCount > 0 then
    GExpertsDLLName := ParamStr(1)
  else
    GExpertsDLLName := GExpertsDll;

  Handle := SysUtils.SafeLoadLibrary(GExpertsDLLName);
  if Handle = 0 then
    raise Exception.CreateFmt('Could not load library %s.', [GExpertsDLLName]);
  @ShowExpertManager := GetProcAddress(Handle, 'ShowExpertManager');
  if not Assigned(ShowExpertManager) then
    raise Exception.CreateFmt('Could not find entry point "ShowExpertManager" in dll %s', [GExpertsDLLName]);
  ShowExpertManager;
end.

