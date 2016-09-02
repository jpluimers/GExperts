unit testfile_CharList;

interface

implementation

function URLEscape(const S: string): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    if S[Idx] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.'] then
      Result := Result + S[Idx]
    else
      Result := Result + '%' + IntToHex(Ord(S[Idx]), 2);
  end;
end;

end.
