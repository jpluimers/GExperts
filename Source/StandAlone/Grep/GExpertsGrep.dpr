program GExpertsGrep;

{$R *.res}

uses
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

procedure ShowGrep; external GExpertsDll;

begin
  ShowGrep;
end.
