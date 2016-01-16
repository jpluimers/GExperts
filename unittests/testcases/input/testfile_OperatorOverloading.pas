unit testfile_OperatorOverloading;

interface

type
  TMyClass = record
    // Addition of two operands of type TMyClass
    class operator Add(a, b: TMyClass): TMyClass;
    class operator Subtract(a, b: TMyClass): TMyclass; // Subtraction of type TMyClass
    class operator Implicit(a: Integer): TMyClass;
    class operator Implicit(a: TMyClass): Integer;
    class operator Explicit(a: Double): TMyClass;
  end;

implementation

{ TMyClass }

class operator TMyClass.Add(a, b: TMyClass): TMyClass;
begin

end;

class operator TMyClass.Explicit(a: Double): TMyClass;
begin

end;

class operator TMyClass.Implicit(a: Integer): TMyClass;
begin

end;

class operator TMyClass.Implicit(a: TMyClass): Integer;
begin

end;

class operator TMyClass.Subtract(a, b: TMyClass): TMyclass;
begin

end;

end.

