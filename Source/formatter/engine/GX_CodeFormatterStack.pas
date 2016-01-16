// defines a stack and a stackstack for the code formatter based on a pseudo template
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterStack;

{$I GX_CondDefine.inc}

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
  TCodeFormatterSegment = class
  private
    FStack: TStackArray;
    FStackPtr: Integer;
    FNIndent: Integer;
    FProcLevel: Integer;
    FGenericsElement: Boolean;
    function TopRec: PStackRec;
    procedure SetGenericsElement(_Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    {: returns the topmost item from the stack without removing it }
    function GetTopType: TReservedType;
    function GetTopIndent: Integer;
    {: like GetTopType, but takes an index, Idx = 0 is equivalent to GetTopType,
       Idx=1 returns the next etc. }
    function GetType(_Idx: Integer): TReservedType;
    {: Check whether _Type is somewhere on the stack }
    function HasType(_Type: TReservedType): Boolean;
    function Pop: TReservedType;
    procedure Push(_Type: TReservedType; _IncIndent: Integer);
    {: returns True if the stack is empty }
    function IsEmpty: Boolean;
    {: clears the stack and returns the number of items that were left }
    function Clear: Integer;
    function Depth: Integer;
    function Clone: TCodeFormatterSegment;
    property NIndent: Integer read FNIndent write FNIndent;
    property ProcLevel: Integer read FProcLevel write FProcLevel;
    property GenericsElement: Boolean read FGenericsElement write SetGenericsElement;
  end;

{$DEFINE STACK_TEMPLATE}
type
  _STACK_ITEM_ = TCodeFormatterSegment;
const
  _MAX_DEPTH_ = 150;
{$INCLUDE DelforStackTemplate.tpl}

type
  TCodeFormatterStack = class(_STACK_)
  end;

implementation

{ TCodeFormatterSegment }

constructor TCodeFormatterSegment.Create;
begin
  inherited Create;
  FStackPtr := -1;
  FNIndent := 0;
  FProcLevel := 0;
  FGenericsElement := False;
end;

destructor TCodeFormatterSegment.Destroy;
begin
  inherited;
end;

function TCodeFormatterSegment.GetTopType: TReservedType;
begin
  if FStackPtr >= 0 then
    Result := TopRec.RT
  else
    Result := rtNothing;
end;

function TCodeFormatterSegment.GetType(_Idx: Integer): TReservedType;
begin
  if FStackPtr >= _Idx then
    Result := FStack[FStackPtr - _Idx].RT
  else
    Result := rtNothing;
end;

procedure TCodeFormatterSegment.Push(_Type: TReservedType; _IncIndent: Integer);
begin
  Inc(FStackPtr);
  if FStackPtr > MaxStack then
    raise EFormatException.Create('Stack overflow');

  TopRec.RT := _Type;
  TopRec.nInd := FNIndent;
  FNIndent := FNIndent + _IncIndent;
end;

procedure TCodeFormatterSegment.SetGenericsElement(_Value: Boolean);
begin
  FGenericsElement := _Value;
end;

function TCodeFormatterSegment.HasType(_Type: TReservedType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FStackPtr do
    if FStack[I].RT = _Type then begin
      Result := True;
      Exit;
    end;
end;

function TCodeFormatterSegment.Pop: TReservedType;
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

function TCodeFormatterSegment.TopRec: PStackRec;
begin
  Result := @FStack[FStackPtr];
end;

function TCodeFormatterSegment.GetTopIndent: Integer;
begin
  if not IsEmpty then begin
    Result := TopRec.nInd;
    NIndent := Result;
  end else
    Result := NIndent;
end;

function TCodeFormatterSegment.IsEmpty: Boolean;
begin
  Result := FStackPtr < 0;
end;

function TCodeFormatterSegment.Clear: Integer;
begin
  Result := Depth;
  FStackPtr := -1;
  FNIndent := 0;
  FProcLevel := 0;
end;

function TCodeFormatterSegment.Depth: Integer;
begin
  Result := FStackPtr + 1;
end;

function TCodeFormatterSegment.Clone: TCodeFormatterSegment;
begin
  Result := TCodeFormatterSegment.Create;
  Result.FStack := FStack;
  Result.FStackPtr := FStackPtr;
  Result.FNIndent := FNIndent;
  Result.FProcLevel := FProcLevel;
end;

{ TCodeFormatterStack }

{$INCLUDE DelforStackTemplate.tpl}

end.

