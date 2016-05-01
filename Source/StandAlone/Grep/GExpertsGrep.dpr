program GExpertsGrep;

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs;

procedure LoadDll(const _GExpertsDLLName: string);
type
  TShowGrep = procedure;
var
  Handle: Cardinal;
  ShowGrep: TShowGrep;
begin
  Handle := SysUtils.SafeLoadLibrary(_GExpertsDLLName);
  if Handle = 0 then
    raise Exception.CreateFmt('Could not load library %s.', [_GExpertsDLLName]);
  @ShowGrep := GetProcAddress(Handle, 'ShowGrep');
  if not Assigned(ShowGrep) then
    raise Exception.CreateFmt('Could not find entry point "ShowGrep" in dll %s', [_GExpertsDLLName]);
  ShowGrep;
end;

procedure TryLoad(const _GExpertsDLLName: string);
begin
  try
    LoadDll(_GExpertsDLLName);
    // it worked halt the program
    Halt(0);
  except
    // ignore (and try the next dll)
  end;
end;

procedure Main;
begin
  TryLoad('GExpertsRS101.dll');
  TryLoad('GExpertsRS10.dll');
  TryLoad('GExpertsRSXE8.dll');
  TryLoad('GExpertsRSXE7.dll');
  TryLoad('GExpertsRSXE6.dll');
  TryLoad('GExpertsRSXE5.dll');
  TryLoad('GExpertsRSXE4.dll');
  TryLoad('GExpertsRSXE3.dll');
  TryLoad('GExpertsRSXE2.dll');
  TryLoad('GExpertsRSXE1.dll');
  TryLoad('GExpertsRS2010.dll');
  TryLoad('GExpertsRS2009.dll');
  TryLoad('GExpertsDelphi2007.dll');
  TryLoad('GExpertsBDS2006.dll');
  TryLoad('GExpertsDelphi2005.dll');
  TryLoad('GExpertsD8.dll');
  TryLoad('GExpertsD7.dll');
  TryLoad('GExpertsD6.dll');

  MessageDlg('Failed to load any of the possible GExperts DLLs', mtError, [mbOK], 0);
end;

begin
  Main;
end.
