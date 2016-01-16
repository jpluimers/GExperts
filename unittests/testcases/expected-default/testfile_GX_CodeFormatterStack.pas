// defines a stack and a stackstack for the code formatter based on a pseudo template
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterStack;

interface

uses
  GX_CodeFormatterTypes;

type
  PStackRec = ^TStackRec;
  TStackRec = record
    RT: TReservedType;
    nInd: Integer;
  end;

const
  MaxStack = 150;
type
  TStackArray = array[0..MaxStack] of TStackRec;

type
  TCodeFormatterStack = class
  private
    FStack: TStackArray;
    FStackPtr: Integer;
    FNIndent: Integer;
    FProcLevel: Integer;
    function TopRec: PStackRec;
  public
    constructor Create;
    destructor Destroy; override;
    {: returns the topmost item from the stack without removing it }
    function GetTopType: TReservedType;
    function GetTopIndent: Integer;
    {: Check whether AType is somewhere on the stack }
    function HasType(AType: TReservedType): Boolean;
    function Pop: TReservedType;
    procedure Push(RType: TReservedType; IncIndent: Integer);
    {: returns True if the stack is empty }
    function IsEmpty: Boolean;
    {: clears the stack and returns the number of items that were left }
    function Clear: Integer;
    function Depth: Integer;
    function Clone: TCodeFormatterStack;
    property NIndent: Integer read FNIndent write FNIndent;
    property ProcLevel: Integer read FProcLevel write FProcLevel;
  end;

{$DEFINE STACK_TEMPLATE}
type
  _STACK_ITEM_ = TCodeFormatterStack;
const
  _MAX_DEPTH_ = 150;
{$INCLUDE DelforStackTemplate.tpl}

type
  TCodeFormatterStackStack = class(_STACK_)
  end;

implementation

{ TDelForStack }

constructor TCodeFormatterStack.Create;
begin
  inherited Create;
  FStackPtr := -1;
  FNIndent := 0;
  FProcLevel := 0;
end;

destructor TCodeFormatterStack.Destroy;
begin
  inherited;
end;

function TCodeFormatterStack.GetTopType: TReservedType;
begin
  if FStackPtr >= 0 then
    Result := TopRec.RT
  else
    Result := rtNothing;
end;

procedure TCodeFormatterStack.Push(RType: TReservedType; IncIndent: Integer);
begin
  Inc(FStackPtr);
  if FStackPtr > MaxStack then
    raise EFormatException.Create('Stack overflow');

  TopRec.RT := RType;
  TopRec.nInd := FNIndent;
  FNIndent := FNIndent + IncIndent;
end;

function TCodeFormatterStack.HasType(AType: TReservedType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FStackPtr do
    if FStack[I].RT = AType then begin
      Result := True;
      Exit;
    end;
end;

function TCodeFormatterStack.Pop: TReservedType;
begin
  if FStackPtr >= 0 then begin
    FNIndent := TopRec.nInd;
    if (TopRec.RT = rtProcedure) and (FProcLevel > 0) then
      Dec(FProcLevel);
    Result := TopRec^.RT;
    Dec(FStackPtr);
  end else begin
    FNIndent := 0;
    FProcLevel := 0;
    Result := rtNothing;
  end;
end;

function TCodeFormatterStack.TopRec: PStackRec;
begin
  Result := @FStack[FStackPtr];
end;

function TCodeFormatterStack.GetTopIndent: Integer;
begin
  if not IsEmpty then begin
    Result := TopRec.nInd;
    NIndent := Result;
  end else
    Result := NIndent;
end;

function TCodeFormatterStack.IsEmpty: Boolean;
begin
  Result := FStackPtr < 0;
end;

function TCodeFormatterStack.Clear: Integer;
begin
  Result := Depth;
  FStackPtr := -1;
  FNIndent := 0;
  { TODO -otwm -ccheck : Is this correct? }
  FProcLevel := 0;
end;

function TCodeFormatterStack.Depth: Integer;
begin
  Result := FStackPtr + 1;
end;

function TCodeFormatterStack.Clone: TCodeFormatterStack;
begin
  Result := TCodeFormatterStack.Create;
  Result.FStack := FStack;
  Result.FStackPtr := FStackPtr;
  Result.FNIndent := FNIndent;
  Result.FProcLevel := FProcLevel;
end;

{ TDelForStackStack }

{$INCLUDE DelforStackTemplate.tpl}

end.
