unit testfile_NestedClass;

interface

type
  TSomeEvent1 = procedure(_Sender: TObject) of object;


type
  TOuterClass = class
  public
    type
      TSomeEvent2 = procedure(_Sender: TObject) of object;
  public
  end;

implementation

end.
