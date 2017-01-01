unit testfile_ClassInImplementation;

interface

implementation

type
  TDummy = class
  public
    constructor Create;
  end;

  { TDummy }

constructor TDummy.Create;
begin
  inherited Create;
end;

end.
