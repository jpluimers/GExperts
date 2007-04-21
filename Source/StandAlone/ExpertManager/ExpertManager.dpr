program ExpertManager;

{$R *.res}

uses
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

procedure ShowExpertManager; external GExpertsDll;
begin
  ShowExpertManager;
end.

