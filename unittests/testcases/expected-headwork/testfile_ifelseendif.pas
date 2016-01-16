unit testunit_ifelseendif;

interface

implementation

procedure hallo;
begin
  if test1 then
    begin
{$IFDEF level1}
      if test2 then
        begin
{$IFDEF level2}
        end;
{$ELSE}
        end;
{$ENDIF}
    end;
{$ELSE}
    end;
{$ENDIF}
end;

end.
