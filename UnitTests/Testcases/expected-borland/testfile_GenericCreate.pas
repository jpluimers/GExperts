unit testfile_GenericCreate;

interface

implementation

procedure Test;
var
  Arr: TArray<Integer>;
begin
  Arr := TArray<Integer>.Create(100, 101, 102);
end;

end.