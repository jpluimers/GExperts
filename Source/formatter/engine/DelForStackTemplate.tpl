{$IFNDEF STACK_TEMPLATE}
unit DelForStackTemplate;

interface

uses
  DelForTypes;

type
  _STACK_ITEM_ = TObject;

const
  _MAX_DEPTH_ = 150;

{$ENDIF STACK_TEMPLATE}

{$IFNDEF STACK_TEMPLATE_SECOND_PASS}

type
  _STACK_ARRAY_ = array[0.._MAX_DEPTH_] of _STACK_ITEM_;

type
  _STACK_ = class
  private
    FStack: _STACK_ARRAY_;
    FStackPtr: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(_Item: _STACK_ITEM_);
    function Pop: _STACK_ITEM_;
    function Top: _STACK_ITEM_;
    function IsEmpty: boolean;
  end;
{$ENDIF STACK_TEMPLATE_SECOND_PASS}

{$IFNDEF STACK_TEMPLATE}
implementation
{$DEFINE STACK_TEMPLATE_SECOND_PASS}
{$ENDIF STACK_TEMPLATE}

{$IFDEF STACK_TEMPLATE_SECOND_PASS}

{ _STACK_ }

constructor _STACK_.Create;
begin
  inherited Create;
  FStackPtr := -1;
end;

destructor _STACK_.Destroy;
begin
  while not IsEmpty do
    Pop.Free;
  inherited;
end;

procedure _STACK_.Push(_Item: _STACK_ITEM_);
begin
  Inc(FStackPtr);
  if FStackPtr > _MAX_DEPTH_ then
    raise EFormatException.Create('Stack overflow');

  FStack[FStackPtr] := _Item;
end;

function _STACK_.Pop: _STACK_ITEM_;
begin
  Assert(FStackPtr >= 0);

  Result := FStack[FStackPtr];
  Dec(FStackPtr);
end;

function _STACK_.Top: _STACK_ITEM_;
begin
  Assert(FStackPtr >= 0);

  Result := FStack[FStackPtr];
end;

function _STACK_.IsEmpty: boolean;
begin
  Result := FStackPtr < 0;
end;

{$ENDIF STACK_TEMPLATE_SECOND_PASS}

{$IFNDEF STACK_TEMPLATE}
{$WARNINGS off}
end.
{$ELSE STACK_TEMPLATE}

{$DEFINE STACK_TEMPLATE_SECOND_PASS}

{$ENDIF STACK_TEMPLATE}

