unit testunit_strictvisibility;

interface

type
  TSomeType = class
  strict private
    ThisIsNotAccessibleFromOutsideTheClass: integer;
  strict protected
    ThisIsNotAccessibleFromOutsideTheClassOrDescendants: integer;
  end;

implementation

end.
