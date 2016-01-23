unit testunit_compilerdirectives;

{$INCLUDE 'hallo'}
{$IFOPT r+}
{ do something }
{$ENDIF}

interface

(*$ifdef allow_bracket_start_directives*)
this_does_not_work_correctly;
(*$endif*)

implementation

end.

