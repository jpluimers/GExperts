program CodeLibrarian;

{$R *.res}

uses
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

procedure ShowCodeLib; external GExpertsDll;

begin
  ShowCodeLib;
end.
