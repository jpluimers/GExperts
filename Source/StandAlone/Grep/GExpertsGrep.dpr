program GExpertsGrep;

{$R *.res}

uses
  Windows,
  SysUtils,
  Classes,
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

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

procedure Main;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('GExpertsD6.dll');
    sl.Add('GExpertsD7.dll');
    sl.Add('GExpertsD8.dll');
    sl.Add('GExpertsDelphi2005.dll');
    sl.Add('GExpertsBDS2006.dll');
    sl.Add('GExpertsDelphi2007.dll');
    sl.Add('GExpertsRS2009.dll');
    sl.Add('GExpertsRS2010.dll');
    sl.Add('GExpertsRSXE1.dll');
    sl.Add('GExpertsRSXE2.dll');
    sl.Add('GExpertsRSXE3.dll');
    sl.Add('GExpertsRSXE4.dll');
    sl.Add('GExpertsRSXE5.dll');
    sl.Add('GExpertsRSXE6.dll');
    sl.Add('GExpertsRSXE7.dll');
    sl.Add('GExpertsRSXE8.dll');
    sl.Add('GExpertsRS10.dll');
    sl.Add('GExpertsRS101.dll');
    for i := sl.Count - 1 downto 0 do
      if FileExists(sl[i]) then begin
        LoadDll(sl[i]);
        Exit;
      end;
  finally
    FreeAndNil(sl);
  end;
end;

begin
  Main;
end.
