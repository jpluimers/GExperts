program ExpertManager;

uses
  Windows,
  SysUtils,
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas',
  GExpertsDllSelection in 'GExpertsDllSelection.pas' {frGEXpertsDllSelection};

begin
  TfrGEXpertsDllSelection.Execute;
end.
