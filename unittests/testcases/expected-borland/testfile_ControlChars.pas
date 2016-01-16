program testfile_ControlChars;

const
  Str5 = ^m^j;
  Str6 = #13#10;
  Str7 = #13^J; // known bug: this gets uppercased wrongly
  Str8 = ^m#10;

begin
  Write('hello'^m^j);
end.
