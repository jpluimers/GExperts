unit Preview;
{*******************************
If you change any text in
this memo the AFTER memo
is updated.
*********************************}

interface
uses Windows, SysUtils, Classes,
  Graphics, Forms;

type
  TClass = class(TObject)
  private
    FField1,
    FField2: integer;
    procedure M1(S: integer); virtual;
  public
    function Foo: integer;
  end;

implementation

function Foo: integer;
begin
  Result := 0;
  case Result of
    1: Result := 2;
    else Result := 3;
  end;
end;

procedure TClass.M1(S, K: integer;
  S2, K2: Single);
  procedure SubProc;
  begin
    S := 9;
  end;
const
  J = 10;
  V = 12.5;
var
  J,
  I: Double;
begin
  if S > J then begin
    SubProc;
  end
  else begin
    S := Foo;
  end;
end;
end.

