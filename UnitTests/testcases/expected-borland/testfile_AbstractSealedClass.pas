unit testfile_AbstractSealedClass;

interface

type
  TSealedClass = class sealed(TObject)
  private
    FField: integer;
  public
    constructor Create;
  end;

type
  TAbstractClass = class abstract(TObject)
  private
    FField: integer;
  public
    constructor Create;
  end;

implementation

{ TSealedClass }

constructor TSealedClass.Create;
begin

end;

{ TAbstractClass }

constructor TAbstractClass.Create;
begin

end;

end.
