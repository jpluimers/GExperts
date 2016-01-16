unit testfile_GenericClass;

interface

type
  TGenericArray<T> = array of T;

type
  TMyData<T> = class(TObject)
  public
    function Add(const aExistingSet: TGenericArray<T>;
      const aNewValue: T): TGenericArray<T>;
  end;

implementation

{ TMyData<T> }

function TMyData<T>.Add(const aExistingSet: TGenericArray<T>;
  const aNewValue: T): TGenericArray<T>;
begin
  { You could append the new 
  value to the existing set and return it here. }
end;

end.