unit testfile_ifthenelse;

interface

implementation

procedure test;
begin
  if True then
  begin
    Hallo;
  end
  else
  begin
    if true then
      Hallo
    else
      Hallo;
  end;
  if True then
    Hallo
  else
  begin
    Hallo;
  end;
end;

end.
