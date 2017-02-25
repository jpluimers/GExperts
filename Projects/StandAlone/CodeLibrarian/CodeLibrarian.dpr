program CodeLibrarian;

{$R *_icon.res}

uses
  GX_VerDepConst in '..\..\Framework\GX_VerDepConst.pas';

procedure ShowCodeLib; external GExpertsDll;

begin
  ShowCodeLib;
end.
