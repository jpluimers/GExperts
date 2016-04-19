// defines TPascalToken and descendants which are the intermediate format after parsing
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterTokens;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  StrUtils,
  TypInfo,
  GX_GenericUtils,
  GX_CodeFormatterTypes;

type
  {: TPascalToken is never instantiated }
  TPascalToken = class
  public
    function GetWordType: TWordType; virtual; abstract;
    procedure GetLength(var _Length: Integer); virtual; abstract;
    function Space(_Space: TSpace): Boolean; virtual;
    function GetReservedType: TReservedType; virtual; abstract;
    procedure SetReservedType(_rType: TReservedType); virtual;
    procedure SetSpace(_Space: TSpaceSet; _State: Boolean); virtual;
    {: @returns the object's content }
    function GetContent: TGXUnicodeString; virtual; abstract;
    (*: changes " // <comment>" to "{//<comment>}" used for preventing a
      begin
        moved from the next line to become commented out
        @param ACommChar is one of '{', '(' or '/' specifying the new comment type
        @returs True if token is a comment, False otherwise *)
    function ChangeComment(_CommChar: char): Boolean;
    function GetExpression(out _Expression: TGXUnicodeString): Boolean; virtual;
    procedure SetExpression(const _Expression: TGXUnicodeString); virtual;
    procedure SetExpressionCase(_Case: TCase); virtual;
    function GetExpressionCase: TCase; virtual;
    procedure SetOptions(_Options: TTokenOptions); virtual;
    function GetOptions: TTokenOptions; virtual;
    procedure AddOption(_Option: TTokenOption);
    function HasOption(_Option: TTokenOption): Boolean;

    function GetForDebug: TGXUnicodeString; virtual;

    property ExpressionCase: TCase read GetExpressionCase write SetExpressionCase;
    property ReservedType: TReservedType read GetReservedType write SetReservedType;
    property WordType: TWordType read GetWordType;
    property Content: TGXUnicodeString read GetContent;
    property Options: TTokenOptions read GetOptions write SetOptions;
  end;

  TLineFeed = class(TPascalToken)
  private
    FSpacePerIndent: Integer;
    FNoOfSpaces: Integer;
    FOldNoOfSpaces: Integer;
    FWrapped: Boolean;
  public
    constructor Create(_OldnSpaces: Integer; _SpacePerIndent: Integer);
    function GetWordType: TWordType; override;
    procedure SetIndent(_Value: Integer);
    procedure IncIndent(_Value: Integer);
    procedure GetLength(var _Length: Integer); override;
    function GetReservedType: TReservedType; override;

    function GetForDebug: TGXUnicodeString; override;

    {: @returns a TGXUnicodeString with FNoOfSpaces spaces }
    function GetContent: TGXUnicodeString; override;
    property NoOfSpaces: Integer read FNoOfSpaces write FNoOfSpaces;
    property OldNoOfSpaces: Integer read FOldNoOfSpaces write FOldNoOfSpaces;
    property Wrapped: Boolean read FWrapped write FWrapped;
  end;

  TExpression = class(TPascalToken)
  private
    FExpression: TGXUnicodeString;
    FWordType: TWordType;
    FSpaceType: TSpaceSet;
    FCaseType: TCase;
    FReservedType: TReservedType;
    FOptions: TTokenOptions;
  public
    constructor Create(_Type: TWordType; const _Expression: TGXUnicodeString);
    procedure CheckReserved;
    procedure SetSpace(_Space: TSpaceSet; _State: Boolean); override;
    procedure SetReservedType(_ReservedType: TReservedType); override;
    function Space(_Space: TSpace): Boolean; override;
    {: @returns <spaces><the expression><spaces> }
    function GetContent: TGXUnicodeString; override;
    procedure GetLength(var _Length: Integer); override;
    function GetWordType: TWordType; override;
    function GetReservedType: TReservedType; override;
    function GetExpression(out _Expression: TGXUnicodeString): Boolean; override;
    procedure SetExpression(const _Value: TGXUnicodeString); override;
    procedure SetExpressionCase(_Value: TCase); override;
    function GetExpressionCase: TCase; override;
    procedure SetOptions(_Value: TTokenOptions); override;
    function GetOptions: TTokenOptions; override;
    function GetForDebug: TGXUnicodeString; override;
  end;

  TAlignExpression = class(TExpression)
  private
    FAlignPos: Byte;
    FNoOfSpaces: Byte;
  public
    constructor Create(_Like: TExpression; _AlignPos: Byte);
    // Note: As a side effect, this adjusts FNoOfspaces
    procedure GetLength(var _Length: Integer); override;
    {: @returns <spaces><the expression><spaces> }
    function GetContent: TGXUnicodeString; override;
    property NoOfSpaces: byte read FNoOfSpaces write FNoOfSpaces;
  end;

implementation

{ TPascalToken }

function TPascalToken.GetExpression(out _Expression: TGXUnicodeString): Boolean;
begin
  Result := False;
end;

function TPascalToken.GetForDebug: TGXUnicodeString;
begin
  Result := ClassName;
end;

function TPascalToken.GetOptions: TTokenOptions;
begin
  Result := [];
end;

function TPascalToken.HasOption(_Option: TTokenOption): Boolean;
begin
  Result := _Option in Options;
end;

function TPascalToken.Space(_Space: TSpace): Boolean;
begin
  Result := False;
end;

procedure TPascalToken.SetExpression(const _Expression: TGXUnicodeString);
begin
end;

procedure TPascalToken.SetOptions(_Options: TTokenOptions);
begin
end;

procedure TPascalToken.SetExpressionCase(_Case: TCase);
begin
end;

function TPascalToken.GetExpressionCase: TCase;
begin
  Result := rfUnchanged;
end;

procedure TPascalToken.SetReservedType(_rType: TReservedType);
begin
end;

procedure TPascalToken.AddOption(_Option: TTokenOption);
begin
  Options := Options + [_Option];
end;

function TPascalToken.ChangeComment(_CommChar: char): Boolean;
var
  s: TGXUnicodeString;
begin
  Result := (Reservedtype = rtComment);
  if not Result then
    Exit;

  GetExpression(s);
  if Pos(_CommChar, s) <> 0 then
    Exit;

  case _CommChar of
    '{':
      if Pos('}', s) = 0 then
        s := '{' + s + '}';

    '(':
      if Pos('*)', s) = 0 then
        s := '(*' + s + '*)';

    '/':
      if Pos('//', s) <> 1 then
        s := '//' + s;
  end; // case

  SetExpression(s);
end;

procedure TPascalToken.SetSpace(_Space: TSpaceSet; _State: Boolean);
begin
  // do nothing
end;

{ TExpression }

constructor TExpression.Create(_Type: TWordType; const _Expression: TGXUnicodeString);
begin
  inherited Create;
  FWordType := _Type;
  FSpaceType := [];
  FCaseType := rfUnchanged;
  FReservedType := rtNothing;
  FExpression := _Expression;
  CheckReserved;
end;

procedure TExpression.CheckReserved;
var
  Expr: TGXUnicodeString;
  Directive: TGXUnicodeString;
  ResvdType: TReservedType;
begin
  SetReservedType(rtNothing);
  case WordType of
    wtCompDirective: begin
        GetExpression(Expr);

        if Copy(Expr, 1, 1) = '{' then
          Directive := Copy(Expr, 3, 999999)
        else if Copy(Expr, 1, 2) = '(*' then
          Directive := Copy(Expr, 4, 999999);

        Directive := LowerCase(Directive);

        if AnsiStartsStr('endif', Directive) then
          SetReservedType(rtCompEndif)
        else if AnsiStartsStr('else', Directive) then
          SetReservedType(rtCompElse)
        else if AnsiStartsStr('ifdef', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('ifopt', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('if ', Directive) then
          SetReservedType(rtCompIf)
        else if AnsiStartsStr('ifndef', Directive) then
          SetReservedType(rtCompIf);
      end;

    wtFullComment, wtFullOutComment, wtHalfStarComment,
      wtHalfOutComment, wtHalfComment:
      SetReservedType(rtComment);

    wtWord: begin
        GetExpression(Expr);
        Expr := LowerCase(Expr);
        if ReservedWordList.FindWord(Expr, ResvdType) then begin
          SetReservedType(ResvdType);
          Exit;
        end;
      end;

    wtOperator: begin
        if GetExpression(Expr) then begin
          if Length(Expr) = 1 then begin
            case Expr[1] of
              ':':
                SetReservedType(rtColon);
              '.':
                SetReservedType(rtDot);
              ';':
                SetReservedType(rtSemiColon);
              ',':
                SetReservedType(rtComma);
              ')':
                SetReservedType(rtRightBr);
              '(':
                SetReservedType(rtLeftBr);
              ']':
                SetReservedType(rtRightHook);
              '[':
                SetReservedType(rtLeftHook);
              '-':
                SetReservedType(rtMinus);
              '+':
                SetReservedType(rtPlus);
              '=':
                SetReservedType(rtEquals);
              '/', '*':
                SetReservedType(rtMathOper);
              '<', '>':
                SetReservedType(rtLogOper);
            end;
          end else if Length(Expr) = 2 then begin
            if Expr = '.)' then
              SetReservedType(rtRightHook)
            else if Expr = '(.' then
              SetReservedType(rtLeftHook)
            else if Expr = '..' then
              SetReservedType(rtDotDot)
            else if Expr = ':=' then
              SetReservedType(rtAssignOper)
            else if (Expr[1] = '<') or (Expr[1] = '>') then
              { if p in > < <> >=  <= = }
              SetReservedType(rtLogOper);
          end;
        end;
      end;
  end;
end;

procedure TExpression.SetExpression(const _Value: TGXUnicodeString);
begin
  FExpression := _Value
end;

procedure TExpression.SetOptions(_Value: TTokenOptions);
begin
  FOptions := _Value;
end;

function TExpression.GetWordType: TWordType;
begin
  Result := FWordType;
end;

function TExpression.GetExpression(out _Expression: TGXUnicodeString): Boolean;
begin
  _Expression := FExpression;
  Result := True;
end;

function TExpression.GetForDebug: TGXUnicodeString;

  function SpaceTypeToStr(_SpaceType: TSpaceSet): TGXUnicodeString;
  begin
    if (spBefore in _SpaceType) then
      Result := 'spBefore'
    else
      Result := '';
    if spAfter in _SpaceType then begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + 'spAfter';
    end;
  end;

var
  s: TGXUnicodeString;
begin
  s := 'Expression: ''' + FExpression + '''';
  s := s + ', WordType: ' + GetEnumName(TypeInfo(TWordType), Integer(FWordType));
  s := s + ', SpaceType: [' + SpaceTypeToStr(FSpaceType) + ']';
  s := s + ', CaseType: ' + GetEnumName(TypeInfo(TCase), Integer(FCaseType));
  s := s + ', ReservedType: ' + GetEnumName(TypeInfo(TReservedType), Integer(FReservedType));
  s := s + ', Options: [' + IfThen(FOptions = [], '', 'toFeedNewLine') + ']';
  Result := inherited GetForDebug + '(' + s + ')';
end;

function TExpression.Space(_Space: TSpace): Boolean;
begin
  Result := (_Space in FSpaceType);
end;

function TExpression.GetReservedType: TReservedType;
begin
  Result := FReservedType;
end;

procedure TExpression.SetSpace(_Space: TSpaceSet; _State: Boolean);
begin
  if _State then
    FSpaceType := FSpaceType + _Space
  else
    FSpaceType := FSpaceType - _Space
end;

procedure TExpression.SetExpressionCase(_Value: TCase);
begin
  FCaseType := _Value;
end;

function TExpression.GetExpressionCase: TCase;
begin
  Result := FCaseType;
end;

procedure TExpression.SetReservedType(_ReservedType: TReservedType);
begin
  FReservedType := _ReservedType;
end;

function TExpression.GetContent: TGXUnicodeString;
begin
  if Space(spBefore) then
    Result := GX_CodeFormatterTypes.Space
  else
    Result := '';

  Result := Result + FExpression;
  Result := AdjustCase(Result, ExpressionCase);

  if Space(spAfter) then
    Result := Result + GX_CodeFormatterTypes.Space;
end;

procedure TExpression.GetLength(var _Length: Integer);
begin
  _Length := _Length + Length(FExpression);
  if Space(spBefore) then
    inc(_Length);
  if Space(spAfter) then
    inc(_Length);
end;

function TExpression.GetOptions: TTokenOptions;
begin
  Result := FOptions;
end;

{ TLineFeed }

function TLineFeed.GetContent: TGXUnicodeString;
var
  Len: Integer;
begin
  if FWrapped then
    Len := FNoOfSpaces + FSpacePerIndent
  else
    Len := FNoOfSpaces;

  if (Len > 0) then
    Result := StringOfChar(GX_CodeFormatterTypes.Space, Len)
  else
    Result := '';
end;

function TLineFeed.GetForDebug: TGXUnicodeString;
var
  s: TGXUnicodeString;
begin
  s := 'SpacePerIndent: ' + IntTostr(FSpacePerIndent);
  s := s + ', NoOfSpaces: ' + IntToStr(FNoOfSpaces);
  s := s + ', OldNoOfSpaces: ' + IntToStr(FOldNoOfSpaces);
  s := s + ', Wrapped: ' + IfThen(FWrapped, 'true', 'false');

  Result := inherited GetForDebug + '(' + s + ')';
end;

procedure TLineFeed.GetLength(var _Length: Integer);
begin
  if FNoOfSpaces > 0 then
    _Length := FNoOfSpaces
  else
    _Length := 0;
end;

function TLineFeed.GetWordType: TWordType;
begin
  Result := wtLineFeed;
end;

function TLineFeed.GetReservedType: TReservedType;
begin
  Result := rtLineFeed;
end;

constructor TLineFeed.Create(_OldnSpaces: Integer; _SpacePerIndent: Integer);
begin
  inherited Create;
  FOldNoOfSpaces := _OldnSpaces;
  FSpacePerIndent := _SpacePerIndent;
  FWrapped := False;
  FNoOfSpaces := _OldnSpaces; { default not changed indent }
end;

procedure TLineFeed.IncIndent(_Value: Integer);
begin
  inc(FNoOfSpaces, _Value * FSpacePerIndent);
end;

procedure TLineFeed.SetIndent(_Value: Integer);
begin
  FNoOfSpaces := _Value * FSpacePerIndent;
end;

{ TAlignExpression }

constructor TAlignExpression.Create(_Like: TExpression; _AlignPos: Byte);
var
  s: TGXUnicodeString;
begin
  _Like.GetExpression(s);
  inherited Create(_Like.FWordType, s);
  FAlignPos := _AlignPos;
  FNoOfSpaces := 0;
  FSpaceType := _Like.FSpaceType;
  FCaseType := _Like.FCaseType;
  FReservedType := _Like.FReservedType;
end;

procedure TAlignExpression.GetLength(var _Length: Integer);
begin
  if _Length < FAlignPos then begin
    FNoOfSpaces := FAlignPos - _Length;
    _Length := FAlignPos;
    SetSpace([spBefore], False);
  end else
    FNoOfSpaces := 0;
  inherited GetLength(_Length);
end;

function TAlignExpression.GetContent: TGXUnicodeString;
begin
  if (FNoOfSpaces > 0) then
    Result := StringOfChar(GX_CodeFormatterTypes.Space, FNoOfSpaces)
  else
    Result := '';

  Result := Result + inherited GetContent;
end;

end.

