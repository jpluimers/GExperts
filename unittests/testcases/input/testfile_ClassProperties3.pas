unit testfile_ClassProperties;

interface

type
  TMyClass = class
  strict private
  class var
    FRed: Integer;
    FGreen: Integer;
    FBlue: Integer;
  public
    constructor Create;
    class property Red: Integer read FRed write FRed;
    class property Green: Integer read FGreen write FGreen;
    class property Blue: Integer read FBlue write FBlue;
  end;

implementation

constructor TMyClass.Create;
begin
  inherited;
end;

end.
