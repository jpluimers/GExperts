unit testfile_NestedClass6;

interface

type
  TOuterClass = class
    type
      TInnerClass = class
        myInnerField: Integer;
        procedure innerProc;
      end;
    procedure outerProc1;
    procedure outerProc2;
  end;

implementation

{ TOuterClass }

procedure TOuterClass.outerProc1;
begin

end;

procedure TOuterClass.outerProc2;
begin

end;

{ TOuterClass.TInnerClass }

procedure TOuterClass.TInnerClass.innerProc;
begin

end;

end.
