unit testfile_Complex;

interface

uses uDataObject, Data.dbxJsonReflect, System.SysUtils, System.Classes;

type
  // COMPLEX OBJECT - Contains TStringList as property.
  TMyComplexClass = class(TMyClass)
  private
    aStringList: TStringList;
  public
    class procedure RegisterConverters(aMar: TJsonMarshal); override;
    class procedure RegisterReverters(aUnMar: TJSONUnMarshal); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(aSource: TMyClass); override;
    procedure SetupData; override;
    property pStringList: TStringList read aStringList write aStringList;
  end;

  TMyComplexClassList<T: TMyComplexClass, constructor> = class(TMyClassList<T>)
  private
  public
    constructor Create; override;
    procedure Assign(aSource: TMyClassList<T>); override;
  end;

var
  initComplexList: TMyComplexClassList<TMyComplexClass>;

implementation

{ TMyComplexClass }

procedure TMyComplexClass.Assign(aSource: TMyClass);
var
  aStr: String;
  aCC: TMyComplexClass;
begin
  inherited Assign(aSource);
  aStringList.Clear;
  aCC := aSource as TMyComplexClass;
  for aStr in aCC.aStringList do
    aStringList.Add(aStr);
end;

constructor TMyComplexClass.Create;
begin
  inherited Create;
  aStringList := TStringList.Create;
end;

destructor TMyComplexClass.Destroy;
begin
  aStringList.Free;
  inherited;
end;

class procedure TMyComplexClass.RegisterConverters(aMar: TJsonMarshal);
begin
  inherited;
  // register class specifik converters here.
  aMar.RegisterConverter(TStringList,
    function(Data: TObject): TListOfStrings
    var
      i, Count: integer;
    begin
      Count := TStringList(Data).Count;
      SetLength(Result, Count);
      for i := 0 to Count - 1 do
        Result[i] := TStringList(Data)[i];
    end);
end;

class procedure TMyComplexClass.RegisterReverters(aUnMar: TJSONUnMarshal);
begin
  inherited;
  // register class specific reverters here
  aUnMar.RegisterReverter(TStringList,
    function(Data: TListOfStrings): TObject
    var
      StrList: TStringList;
      Str: string;
    begin
      StrList := TStringList.Create;
      for Str in Data do
        StrList.Add(Str);
      Result := StrList;
    end);
end;

procedure TMyComplexClass.SetupData;
var
  i: integer;
begin
  inherited SetupData;
  for i := 0 to pInteger do
    aStringList.Add(IntToStr(pInteger) + ' - Text - ' + IntToStr(i));
end;

{ TMyComplexClassList<T> }

procedure TMyComplexClassList<T>.Assign(aSource: TMyClassList<T>);
begin
  inherited Assign(aSource);
end;

constructor TMyComplexClassList<T>.Create;
begin
  inherited Create;

end;

initialization

initComplexList := TMyComplexClassList<TMyComplexClass>.Create;

finalization

initComplexList.Free;

end.
