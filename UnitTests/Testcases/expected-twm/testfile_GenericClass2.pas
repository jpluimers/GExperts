unit testfile_GenericClass2;

interface

type
  TMyComplexClassList<T: TMyComplexClass, constructor> = class(TMyClassList<T>)
  private
  public
    constructor Create; override;
    procedure Assign(aSource: TMyClassList<T>); override;
  end;

implementation

constructor TMyComplexClassList.Create;
begin
  inherited;
end;

procedure TMyComplexClassList.Assign(aSource: TMyClassList<T>);
begin
end;

end.
