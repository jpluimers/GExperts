unit testfile_CurlyBracesInWhile;

interface

implementation

procedure ConverFun(iPos: integer);
begin
  while true {and iPos in [
         5])} do
    ;
end;

end.