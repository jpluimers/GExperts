unit testfile_SlashCommentToCurly;

interface

implementation

procedure Test;
begin
  if 1 > 2 then // this comment is the problem
    begin
    end;
end;

end.