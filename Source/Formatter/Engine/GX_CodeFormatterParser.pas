// parses Delphi code into a token collection
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterParser;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils,
  GX_CodeFormatterTokenList,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterTokens;

type
  TIdentifiersList = class(TStringList)
  private
    FSettings: TCodeFormatterSettings;
  public
    constructor Create(const _Settings: TCodeFormatterSettings); reintroduce;
    function AddIdentifier(const _s: TGXUnicodeString): TGXUnicodeString;
  end;

type
  {: usage:
     Tokens := TCodeFormatterParser.Execute(Text, Settings); }
  TCodeFormatterParser = class
  private
    FSpacePerIndent: Integer;
    FStartCommentOut: TGXUnicodeString;
    FEndCommentOut: TGXUnicodeString;
  private
    FTokens: TPascalTokenList;
    FReadingAsm: Boolean;
    FAsmComment: TWordType;
    FPrevLine: TLineFeed;
    FPrevType: TWordType;
    FLeftPointBracket: Integer;
    FIdentifiers: TIdentifiersList;
    {: ReadAsm does very simple parsing only to find comments because this
       influences finding the matching "end". No formatting is done
       later to asm code }
    procedure ReadAsm(var _Buff: PGXUnicodeChar);
    function ReadHalfComment(out _Dest: TGXUnicodeString; var _Source: PGXUnicodeChar): TWordType;
    function ReadWord(out _Dest: TGXUnicodeString; var _Source: PGXUnicodeChar): TWordType;
    {: Adds a single line of text to the parse tree }
    procedure AddLine(_Buff: PGXUnicodeChar);
    procedure DoExecute(_Text: TGXUnicodeStringList);
  public
    constructor Create(_Settings: TCodeFormatterSettings);
    destructor Destroy; override;
    function DetachTokens: TPascalTokenList;
    {: parses the text and returns a parse tree }
    class function Execute(_Text: TGXUnicodeStringList; _Settings: TCodeFormatterSettings): TPascalTokenList;
  end;

implementation

uses
  GX_CodeFormatterUnicode;

class function TCodeFormatterParser.Execute(_Text: TGXUnicodeStringList; _Settings: TCodeFormatterSettings): TPascalTokenList;
var
  Parser: TCodeFormatterParser;
begin
  Parser := TCodeFormatterParser.Create(_Settings);
  try
    Parser.DoExecute(_Text);
    Result := Parser.DetachTokens;
  finally
    Parser.Free;
  end;
end;

constructor TCodeFormatterParser.Create(_Settings: TCodeFormatterSettings);
begin
  inherited Create;
  FSpacePerIndent := _Settings.SpacePerIndent;
  FStartCommentOut := _Settings.StartCommentOut;
  FEndCommentOut := _Settings.EndCommentOut;
  FLeftPointBracket := 0;
  FReadingAsm := False;
  FPrevLine := nil;
  FPrevType := wtNothing;
  FTokens := TPascalTokenList.Create;
  FIdentifiers := TIdentifiersList.Create(_Settings);
end;

destructor TCodeFormatterParser.Destroy;
begin
  FreeAndNil(FTokens);
  FreeAndNil(FIdentifiers);
  inherited;
end;

function TCodeFormatterParser.DetachTokens: TPascalTokenList;
begin
  Result := FTokens;
  FTokens := nil;
end;

procedure TCodeFormatterParser.DoExecute(_Text: TGXUnicodeStringList);
var
  i: Integer;
  s: TGXUnicodeString;
  Buff: PGXUnicodeChar;
begin
  for i := 0 to _Text.Count - 1 do begin
    s := _Text[i];
    Buff := PGXUnicodeChar(s);
    AddLine(Buff);
  end;
end;

procedure TCodeFormatterParser.AddLine(_Buff: PGXUnicodeChar);
var
  s: TGXUnicodeString;
begin
  FPrevLine := TLineFeed.Create(0, FSpacePerIndent);
  FTokens.Add(FPrevLine);

  if not Assigned(_Buff) or (_Buff^ = #0) then
    Exit;

  if FReadingAsm then
    ReadAsm(_Buff);

  while (_Buff^ <> #0) do begin
    if FPrevType in [wtHalfComment, wtHalfStarComment, wtHalfOutComment] then
      FPrevType := ReadHalfComment(s, _Buff)
    else
      FPrevType := ReadWord(s, _Buff);

    if FPrevType = wtSpaces then begin
      if (FPrevLine <> nil) and (FPrevLine.NoOfSpaces = 0) then begin
        FPrevLine.NoOfSpaces := Length(s);
        FPrevLine.OldNoOfSpaces := FPrevLine.NoOfSpaces;
      end;
    end else begin
      FTokens.Add(TExpression.Create(FPrevType, s));

      if FReadingAsm and (_Buff^ <> #0) then
        ReadAsm(_Buff);
    end;
  end;

  if FTokens.Count >= MaxCollectionSize - 100 then
    raise ECodeFormatter.Create('File to large to reformat')
end;

function PCharPlus(_p: PGXUnicodeChar; _Offset: integer): PGXUnicodeChar;
begin
  Result := _p;
  while _Offset > 0 do begin
    Inc(Result);
    Dec(_Offset);
  end;
  while _Offset < 0 do begin
    Dec(Result);
    Inc(_Offset);
  end;
end;

function PCharDiff(_p, _q: PGXUnicodeChar): integer;
begin
  Result := (Integer(_p) - Integer(_q)) div SizeOf(_p^);
end;

procedure TCodeFormatterParser.ReadAsm(var _Buff: PGXUnicodeChar);
var
  P: PGXUnicodeChar;
  FirstNonWhitespace: PGXUnicodeChar;
  s: TGXUnicodeString;
begin
  P := _Buff;
  FirstNonWhitespace := _Buff;

  while CharInSet(FirstNonWhitespace^, [Tab, Space]) do
    Inc(FirstNonWhitespace);

  while p^ <> #0 do begin
    case FAsmComment of
      wtHalfComment: begin
          if P^ = '}' then
            FAsmComment := wtWord;
          Inc(P);
        end;

      wtHalfStarComment: begin
          if (P^ = '*') and (PCharPlus(P, 1)^ = ')') then begin
            FAsmComment := wtWord;
            Inc(P);
          end;

          Inc(P);
        end;
    else // case
      if ((P = _Buff) or CharInSet(PCharPlus(p, -1)^, [' ', Tab]))
        and (StrLIComp(P, 'end', 3) = 0)
        and CharInSet(PCharPlus(P, 3)^, [#0, ';', ' ', Tab]) then begin // 'end' of assembler
        FReadingAsm := False;

        if FirstNonWhitespace <> P then begin
          SetString(s, FirstNonWhitespace, PCharDiff(p, FirstNonWhitespace) - 1);
          FTokens.Add(TExpression.Create(wtAsm, s));
        end;

        _Buff := P;
        Exit;
      end else if P^ = '{' then begin // '{' comment
        Inc(P);

        while (P^ <> '}') and (P^ <> #0) do
          Inc(P);

        if p^ = #0 then
          FAsmComment := wtHalfComment
        else
          Inc(P);
      end else if (P^ = '(') and (PCharPlus(P, 1)^ = '*') then begin // '(*' comment
        Inc(p, 2);
        while (P^ <> #0) and ((P^ <> '*') or (PCharPlus(P, 1)^ <> ')')) do
          Inc(P);
        if p^ = #0 then
          FAsmComment := wtHalfStarComment
        else
          Inc(P, 2);
      end else if (P^ = '/') and (PCharPlus(P, 1)^ = '/') then begin // '//' comment
        Inc(p, 2);
        while P^ <> #0 do begin
          Inc(P);
        end
      end else
        Inc(P);
    end; // case
  end; // while

  SetString(s, _Buff, StrLen(_Buff));
  FTokens.Add(TExpression.Create(wtAsm, s));
  _Buff := P;
end;

function TCodeFormatterParser.ReadWord(out _Dest: TGXUnicodeString; var _Source: PGXUnicodeChar): TWordType;
const
  IdentifierTerminators = [
    '+', '-', '*', '/', '=',
    '(', ')', '[', ']', '{', '}',
    ',', ';', ':', '.',
    '<', '>', '@', '^',
    Space, '''', Tab, #0];
  { TODO -otwm -cfixme : This allows strings like #a which is wrong, but there is no easy way to fix it. }
  StringControlChars = ['0'..'9', 'a'..'z', 'A'..'Z', '#', '^', '$'];
var
  P: PGXUnicodeChar;

  {: Reads a string literal, on exit p points behind the last character of the string
     @returns either wtString or wtErrorString }

  function ReadString: TWordType;
  begin
    Result := wtString;
    while True do begin
      Inc(P);
      case p^ of
        #0: begin
            // unterminated string (invalid Pascal code but we must deal with it anyway)
            Result := wtErrorString;
            Exit;
          end;

        #39: {// '} begin // we found either a quoted single quote or the end quote
            Inc(p);

            case p^ of
              #39: begin // skip quoted single quote
                  ; // do nothing, we just skipped it
                end;

              '#', '^': begin
                  // todo: this is not really correct code:
                  // 'hello'#^523 is not a valid string literal
                  while CharInSet(P^, StringControlChars) do
                    Inc(P);

                  if P^ <> #39 then begin
                    // we found the end of the string
                    Exit;
                  end;
                end;
            else
              // we found the ending quote
              Exit;
            end;
          end;
      end; // case
    end;

    // we never get here, exiting the function is done via several Exit statements
  end;

  {: Reads an identifier, on exit p points behind the last character of the identifier }

  procedure ReadIdentifier;
  begin
    Result := wtWord;
    { TODO -otwm -ccheck : Shouldn't that rather check for valid identifiers?
      Note that Delphi 2005 allows non ascii characters for identifiers, e.g. "Müller" is a valid
      identifier! }
    while not CharInSet(p^, IdentifierTerminators) do
      Inc(P);
  end;

  // this handles the formatting enabling / disabling strings
  // default: '{(*}' ... '{*)}'

  function CheckDisableComments(out AResult: TWordType): Boolean;
  var
    Len: Integer;
  begin
    Result := (FStartCommentOut <> '') and (FEndCommentOut <> '');
    if not Result then
      Exit;

    Len := Length(FStartCommentOut);
    Result := StrLIComp(P, FStartCommentOut, Len) = 0;

    if not Result then
      Exit;

    AResult := wtHalfOutComment;
    Inc(P, Len);
    Len := Length(FEndCommentOut);

    while P^ <> #0 do begin
      if StrLIComp(P, FEndCommentOut, Len) = 0 then begin
        Inc(P, Len - 1);
        AResult := wtFullOutComment;
        Break;
      end;

      Inc(P);
    end;
  end;

begin
  P := _Source;

  if CharInSet(P^, [Tab, Space]) then begin
    Result := wtSpaces;
    while CharInSet(P^, [Tab, Space]) do
      Inc(P);
  end else if CheckDisableComments(Result) then begin
  end else
    case P^ of
      '{': begin
          Result := wtHalfComment;

          while not CharInSet(P^, ['}', #0]) do
            Inc(P);

          if (P^ = '}') then begin
            Result := wtFullComment;
            if PCharPlus(_Source, 1)^ = '$' then
              Result := wtCompDirective;

            Inc(p);
          end;
        end;

      #39: begin
          // single quote '
          Result := ReadString;
        end;

      '^': begin // string starting with ^A or so or the ^ operator
          Inc(p);
          if CharInSet(P^, ['a'..'z', 'A'..'Z']) and CharInSet(PCharPlus(P, 1)^, [#39, '^', '#']) then begin
            Result := wtString;

            while CharInSet(P^, StringControlChars) do
              Inc(P);

            if P^ = #39 then
              Result := ReadString;
          end else begin
            Result := wtOperator;
          end;
        end;
      '+', '-', '*', '=',
        ')', '[', ']', '}',
        ',', ';',
        '@': begin
          Result := wtOperator;
          Inc(p);
        end;

      '<': begin
          Result := wtOperator;
          Inc(p);
          if CharInSet(p^, ['=', '>']) then // <= or <>
            Inc(p);
        end;

      '>', ':': begin
          Result := wtOperator;
          Inc(p);
          if p^ = '=' then // >= or :=
            Inc(P);
        end;

      '.': begin // .. or .) { TODO -otwm -ccheck : What about .9 for a float? It works, but why? }
          Result := wtOperator;
          Inc(p);
          case p^ of
            '.':
              Inc(P);
            ')': begin
                Dec(FLeftPointBracket);
                Inc(P);
              end;
          end;
        end;

      '(': begin // (. or (*
          Result := wtOperator;
          Inc(p);

          case p^ of
            '.': begin // (. (has precendence over .9, so '(.9)' is an error)
                Inc(FLeftPointBracket);
                Inc(P);
              end;

            '*': begin
                Inc(p);
                Result := wtHalfStarComment;

                while (P^ <> #0) and ((P^ <> '*') or (PCharPlus(P, 1)^ <> ')')) do
                  Inc(P);

                if p^ <> #0 then begin
                  Inc(P);
                  Result := wtFullComment;
                  if PCharPlus(_Source, 2)^ = '$' then
                    Result := wtCompDirective;

                  Inc(p);
                end;
              end;
          end; // case
        end;

      '/': begin // / or //
          if (PCharPlus(P, 1)^ = '/') then begin
            Result := wtFullComment;
            while P^ <> #0 do
              Inc(P);
          end else begin
            Result := wtOperator;
            Inc(p);
          end;
        end;

      '$': begin
          Result := wtHexNumber;
          Inc(P);

          while CharInSet(P^, ['0'..'9', 'A'..'F', 'a'..'f']) do
            Inc(P);
        end;
      '#': begin
          { TODO -otwm -cfixme :
            This is for upper casing hex numbers, but it is rather ugly.
            It also misses those embedded in strings 'bla'#1d'blub' and will change #10^j to #10^J }
          Result := wtHexNumber;

          while CharInSet(P^, StringControlChars) do
            Inc(P);

          if P^ = #39 then begin
            // single quote
            Result := ReadString;
          end;
        end;

      '0'..'9': begin
          Result := wtNumber;
          while CharInSet(P^, ['0'..'9', '.']) and not (StrLComp(P, '..', 2) = 0)
            and not ((FLeftPointBracket > 0) and (StrLComp(P, '.)', 2) = 0)) do
            Inc(P);

          if StrLIComp(P, 'E', 1) = 0 then
            if CharInSet(PCharPlus(P, 1)^, ['0'..'9', '-', '+']) then begin
              Inc(P, 2);
              while CharInSet(P^, ['0'..'9']) do
                Inc(P);
            end;
        end;
    else
      ReadIdentifier;
    end;

  SetString(_Dest, _Source, PCharDiff(P, _Source));

  // This is for changing the casing of identifiers without adding them to
  // the global list.
  if (Result <> wtString) and (not FReadingAsm) then
    _Dest := FIdentifiers.AddIdentifier(_Dest);

  if SameText(_Dest, AnsiString('asm')) then begin
    FReadingAsm := True;
    FAsmComment := wtWord;
  end;

  if (P^ = #0) then
    _Source := P
  else begin
    if CharInSet(P^, [Tab, Space]) then
      Inc(P);

    _Source := P;
  end;
end;

function TCodeFormatterParser.ReadHalfComment(out _Dest: TGXUnicodeString; var _Source: PGXUnicodeChar): TWordType;
var
  Len: Integer;
  P: PGXUnicodeChar;
  FirstNonSpace: PGXUnicodeChar;
  EndComment: TGXUnicodeString;
  EndCommentType: TWordType;
begin
  P := _Source;
  FirstNonSpace := _Source;

  while CharInSet(P^, [Tab, Space]) do
    Inc(P);

  if (FPrevLine <> nil) and (FPrevLine.NoOfSpaces = 0) then begin
    FPrevLine.NoOfSpaces := PCharDiff(P, _Source);
    FPrevLine.OldNoOfSpaces := PCharDiff(P, _Source);
    FirstNonSpace := p;
  end;

  Result := FPrevType;

  case FPrevType of
    wtHalfComment: begin
        EndComment := '}';
        EndCommentType := wtFullComment;
      end;

    wtHalfStarComment: begin
        EndComment := '*)';
        EndCommentType := wtFullComment;
      end;

    wtHalfOutComment: begin
        EndComment := FEndCommentOut;
        EndCommentType := wtFullOutComment;
      end;
  else
    raise ECodeFormatter.Create('internal error: ReadHalfComment should only be called if FPrevType in [wtHalfComment, wtHalfStarComment, wtHalfOutComment]');
  end;

  Len := Length(EndComment);
  while (P^ <> #0) do begin
    if StrLIComp(P, EndComment, Len) = 0 then begin
      Result := EndCommentType;
      Inc(P, Len);
      Break;
    end;

    Inc(P);
  end;

  SetString(_Dest, FirstNonSpace, PCharDiff(P, FirstNonSpace));

  _Source := P;
end;

{ TIdentifiersList }

function TIdentifiersList.AddIdentifier(const _s: TGXUnicodeString): TGXUnicodeString;
var
  WordIndex : Integer;
begin
  case FSettings.IdentifiersCase of
    rfLowerCase:
      Result := LowerCase(_s);
    rfUpperCase:
      Result := UpperCase(_s);
    rfFirstUp: begin
        Result := UpperCase(Copy(_s, 1, 1));
        Result := Result + LowerCase(Copy(_s, 2, Length(_s) - 1));
      end;
    rfUnchanged:
      Result := _s;
    rfFirstOccurrence:
       if not FSettings.CapNames.Find(_s, WordIndex) then
         Result := Strings[Add(_s)]
       else
         Result := FSettings.CapNames[WordIndex];
  end;
end;

constructor TIdentifiersList.Create(const _Settings: TCodeFormatterSettings);
begin
  inherited Create;
  FSettings := _Settings;
  CaseSensitive := False;
  Sorted := True;
  Duplicates := dupIgnore;
end;

end.

