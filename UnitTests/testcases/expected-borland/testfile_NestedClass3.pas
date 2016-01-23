unit testfile_NestedClass3;

interface

type
  TOuterClass = class
    procedure outerproc0;
    type
      TInnerClass = class
      private
        type
          TInnerClass2 = class
            myInnerField2: Integer;
            procedure innerProc2;
          end;
      public
        myInnerField: Integer;
        procedure innerProc;
      end;
    procedure outerProc1;
    procedure outerProc2;
  end;

implementation

{ TOuterClass }

procedure TOuterClass.outerProc0;
begin

end;

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

{ TOuterClass.TInnerClass.TInnerClass2 }

procedure TOuterClass.TInnerClass.TInnerClass2.innerProc2;
begin

end;

end.
