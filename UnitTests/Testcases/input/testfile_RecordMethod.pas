unit testfile_RecordMethod;

interface

type
  TMyRecord = record
    constructor Init;
    procedure trallala;
    procedure Diedeldum;
  end;

type
  TWithPrivate = record
  private
    FIntegerField: integer;
    FStringField: string;
  public
    constructor Init;
    procedure SomeProcedure;
    function SomeFunction: integer;
  end;

implementation

{ TMyRecord }

procedure TMyRecord.Diedeldum;
begin

end;

constructor TMyRecord.Init;
begin

end;

procedure TMyRecord.trallala;
begin

end;

{ TWithPrivate }

constructor TWithPrivate.Init;
begin

end;

function TWithPrivate.SomeFunction: integer;
begin

end;

procedure TWithPrivate.SomeProcedure;
begin

end;

end.

