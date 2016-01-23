unit testfile_AngledBrackets;

interface

implementation

procedure test;
begin
  Assert(2<=(1 + 1));
  Assert(2<=1+1);
  Assert(1<1+1);
  Assert(1 < 1+1);
  Assert(1 < (1+1));
  Assert(1 <(1 + 1));
end;

end.
