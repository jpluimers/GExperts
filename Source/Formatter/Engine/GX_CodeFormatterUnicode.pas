unit GX_CodeFormatterUnicode;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  GX_GenericUtils;

function StrLIComp(_Str1: PGXUnicodeChar; const _Str2: string; _MaxLen: Integer): Integer; overload;
function StrLComp(_Str1: PGXUnicodeChar; const _Str2: string; _MaxLen: Integer): Integer; overload;

{$IFNDEF GX_VER200_up}
function CharInSet(c: TGXUnicodeChar; _Set: TSysCharSet): boolean; overload;
function StrLen(_p: PGXUnicodeChar): Integer; overload;
procedure SetString(var _s: TGXUnicodeString; _p: PGXUnicodeChar; _Len: integer); overload;
function UpCase(_c: TGXUnicodeChar): TGXUnicodeChar; overload;

{$ENDIF GX_VER200_up}

implementation


function StrLIComp(_Str1: PGXUnicodeChar; const _Str2: string; _MaxLen: Integer): Integer; overload;
var
  Counter: Integer;
  Str2: PChar;
begin
  Result := 0;
  if _MaxLen = 0 then
    Exit;
  Str2 := @_Str2[1];
  Counter := 0;
  repeat
    if (_Str1^ = #0) or (str2^ = #0) or  not SameText(_Str1^, str2^) then begin
      Result := Ord(_Str1^) - Ord(str2^);
      exit;
    end;
    Inc(_Str1);
    Inc(Str2);
    Inc(Counter);
  until Counter >= _MaxLen;
end;

function StrLComp(_Str1: PGXUnicodeChar; const _Str2: string; _MaxLen: Integer): Integer; overload;
var
  Counter: Integer;
  Str2: PChar;
begin
  Result := 0;
  if _MaxLen = 0 then
    Exit;
  Str2 := @_Str2[1];
  Counter := 0;
  repeat
    if (_Str1^ = #0) or (str2^ = #0) or (Ord(_Str1^) <> Ord(Str2^)) then begin
      Result := Ord(_Str1^) - Ord(str2^);
      exit;
    end;
    Inc(_Str1);
    Inc(Str2);
    Inc(Counter);
  until Counter >= _MaxLen;
end;

{$IFNDEF GX_VER200_up}

function CharInSet(c: TGXUnicodeChar; _Set: TSysCharSet): boolean; overload;
begin
  if Ord(c) > 256 then
    Result := false
  else
    Result := char(Ord(c) and $FF) in _Set;
end;

procedure SetString(var _s: TGXUnicodeString; _p: PGXUnicodeChar; _Len: integer);
begin
  SetLength(_s, _Len);
  Move(_p^, _s[1], _Len * SizeOf(_p^));
end;

function StrLen(_p: PGXUnicodeChar): Integer;
begin
  Result := 0;
  while _p^ <> #0 do begin
    Inc(Result);
    Inc(_p);
  end;
end;

function UpCase(_c: TGXUnicodeChar): TGXUnicodeChar; overload;
var
  s: string;
  us: TGXUnicodeString;
begin
  s := _c;
  us := UpperCase(s);
  Result := us[1];
end;

{$ENDIF GX_VER200_up}

end.
