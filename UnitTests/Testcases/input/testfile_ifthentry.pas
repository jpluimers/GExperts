unit testfile_ifthentry;

interface

implementation

procedure test;
begin
  if True then
    try
      dosomething;
    finally
      dosomethingelse;
    end;
end;

end.

