unit GX_ProofreaderKeyboard;

{$I GX_CondDefine.inc}

interface

function CharsAreNearOnKeyboard(const a, b: Char): Boolean;

var
  KeysWithDistance1: array[1..50] of set of AnsiChar;
  KeyboardChars, ShiftKeyboardChars, AltGrKeyboardChars: string;

implementation

uses
  Windows, GX_GenericUtils, SysUtils;

function CharsAreNearOnKeyboard(const a, b: Char): Boolean;
var
  p: ShortInt;
  q: ShortInt;
begin
  p := Pos(a, KeyboardChars);
  if p = 0 then
    p := Pos(a, ShiftKeyboardChars);
  if p = 0 then
    p := Pos(a, AltGrKeyboardChars); // FS: added
  q := Pos(b, KeyboardChars);
  if q = 0 then
    q := Pos(b, ShiftKeyboardChars);
  if q = 0 then
    q := Pos(b, AltGrKeyboardChars); // FS: added

  Result := (p > 0) and (q > 0) and CharInSet(KeyboardChars[q], KeysWithDistance1[p]);
end;

var
  BarrierChars: set of AnsiChar;
  Barriers, NextChars: string[6]; // FS: added

(*
InitKeySets builds a map of the user's keyboard in memory.  It arranges
the key map such that is mirrors the keyboard device driver's picture of
the keyboard.  Before adding this function, the key location map was
hardcoded and the calculations for keys that are "next to each other"
were off for many people.

> In particular, I am interested in the first half - that is that part/loop
> where
>     j := VkKeyScan(Chr(i)); // char to virtual key

  This loop builds a KeyBoardLayout array that mirrors the physical
location of keys on the keyboard.  The first dimension in the array is
the shift state, and is one of (ksNormal, ksShift, ksAltGr).  The second
dimension is the physical row of the key on the keyboard.  The third and
last dimension is the physical column in that row for the key.

  To perform the above, we need to find the locale specific virtual key
code for each of the printable ASCII characters from 33 to 254 that can
be "next to" other keys.  The code then extracts the high and low order
bytes to get the shift state and the virtual key code to produce that
ASCII character.  MapVirtualKey(lo(j), 0) is what maps the virtual key
code to the keyboard driver's scan code to determine the physical
position of that key on the keyboard.  The case statement following
MapVirtualKey() checks the scan code and from that determines the "row"
the key is on for this keyboard, such that scan codes 1..13 are the
first row "QWERTYUIOP[]\" in most of our keyboards, 16..27 is row two,
or "ASDFGHJKL;'" on most keyboards, etc.  Scan codes 41 (` and ~), 43 (\
and |) and 86 (?) have their locations set specially and I can only
assume they are special cases that don't follow standard scan code
ordering.  The last if statement in the loop just makes sure we found a
useful mapping, and if we did, assign it to the KeyBoardLayout array if
the array spot isn't already filled.
*)

{$WARNINGS OFF}
procedure InitKeySets;

  procedure IncludeInSet(iKeySet: Integer; Key: AnsiChar);
  begin
    if Key <> #32 then
      KeysWithDistance1[iKeySet] := KeysWithDistance1[iKeySet] + [Key];
  end;
const
  ASCIIAlphaChars = ['A'..'Z', 'a'..'z'];
type
  TKeyState = (ksNormal, ksShift, ksAltGr);
var
  KeyBoardLayout: array[TKeyState] of array[1..4] of string[15];
  i, j, Row, Column: Smallint;
  aKeyState: TKeystate;
begin
  Column := 0;
  // Get layout as 4 rows of 11..13 keys in 3 states
  // keyboard state could be Normal, Shift or AltGr (CTRL+ALT)
  FillChar(KeyBoardLayout, SizeOf(KeyBoardLayout), #32);
  for aKeyState := Low(TKeyState) to High(TKeyState)  do
  begin
    KeyBoardLayout[aKeyState][1][0] := #13;
    KeyBoardLayout[aKeyState][2][0] := #12;
    KeyBoardLayout[aKeyState][3][0] := #12;
    KeyBoardLayout[aKeyState][4][0] := #11;
  end;

  for i := 33 to 254 do
  begin
    j := VkKeyScan(Chr(i)); // char to virtual key
    if Lo(j) > 0 then
    begin
      case Hi(j) of
        0: aKeystate := ksNormal;
        1: aKeystate := ksShift;
        6: aKeystate := ksAltGr; // (CTRL+ALT)
      else
        Continue; // ignore other shift states
      end;

      begin
        j := MapVirtualKey(lo(j), 0); // virtual key code to scan code
        Row := 0;
        case Lo(j) of
          1..13:
            begin
              Row := 1;
              Column := j;
            end;
          16..27:
            begin
              Row := 2;
              Column := j - 15;
            end;
          30..40:
            begin
              Row := 3;
              Column := j - 29;
            end;
          44..53:
            begin
              Row := 4;
              Column := j - 42;
            end;
          41:
            begin
              Row := 1;
              Column := 1;
            end;
          43:
            begin
              Row := 3;
              Column := 12;
            end;
          86:
            begin
              Row := 4;
              Column := 1;
            end;
        end;
        if (Row > 0) and (KeyBoardLayout[aKeyState][Row][Column] = #32) then
          KeyBoardLayout[aKeyState][Row][Column] := AnsiChar(i);
      end;
    end;
  end;

  KeyboardChars :=
    KeyBoardLayout[ksNormal][1] +
    KeyBoardLayout[ksNormal][2] +
    KeyBoardLayout[ksNormal][3] +
    KeyBoardLayout[ksNormal][4];

  ShiftKeyboardChars :=
    KeyBoardLayout[ksShift][1] +
    KeyBoardLayout[ksShift][2] +
    KeyBoardLayout[ksShift][3] +
    KeyBoardLayout[ksShift][4];

  AltGrKeyboardChars :=
    KeyBoardLayout[ksAltgr][1] +
    KeyBoardLayout[ksAltgr][2] +
    KeyBoardLayout[ksAltgr][3] +
    KeyBoardLayout[ksAltgr][4];

  // Get keys with distance 1
  for i := 1 to Length(KeyboardChars) do
  begin
    case i of

      1:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][2]);
        end;

      2..13:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i - 1]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 1]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 1]);
          if i > 2 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 2]);
        end;

      14..25:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][1][i + 2 - 13]);
          if i > 14 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i + 1 - 13]);
          if i > 14 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 1 - 13]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 13]);
        end;

      26..37:
        begin
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][2][i + 1 - 25]);
          if i > 26 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 1 - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i + 1 - 25]);
          if i > 26 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][4][i - 25]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][4][i + 1 - 25]);
        end;

      38..48:
        begin
          if i > 38 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 38]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][3][i - 37]);
          if i > 38 then
            IncludeInSet(i, KeyBoardLayout[ksNormal][4][i - 1 - 37]);
          IncludeInSet(i, KeyBoardLayout[ksNormal][4][i + 1 - 37]);
        end;
    end;
  end;

  // Get BarrierChars
  BarrierChars := [];
  Barriers := '';
  NextChars := '';
  for Row := 2 to 4 do
  begin
    for aKeyState := ksNormal to ksShift do
    begin
      Column := Length(KeyBoardLayout[aKeyState][Row]);
      while (Column > 0) and not (KeyBoardLayout[aKeyState][Row][Column] in ASCIIAlphaChars) do
        Dec(Column);
      if (Column > 0) and (Column < Length(KeyBoardLayout[aKeyState][Row])) then
      begin
        BarrierChars := BarrierChars + [KeyBoardLayout[aKeyState][Row][Column]];
        Barriers := Barriers + KeyBoardLayout[aKeyState][Row][Column];
        NextChars := NextChars + KeyBoardLayout[aKeyState][Row][Column + 1];
      end;
    end;
  end;
end;
{$WARNINGS ON}

initialization
  InitKeySets;

end.
