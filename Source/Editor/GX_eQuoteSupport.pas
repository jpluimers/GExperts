unit GX_eQuoteSupport;

interface

uses Classes, StrUtils, SysUtils;

procedure QuoteLines(Lines: TStrings; EndOfLine: string; IndentStart: Integer);
procedure UnquoteLines(Lines: TStrings; IndentStart: Integer = 0);

implementation

uses
  GX_GenericUtils;

function HasConcatOperators(s: string): Boolean;
begin
  Result := Pos('+', s) <> 0;
end;

function HasComplexCode(s: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(s) do begin
    if CharInSet(s[i], ['(', ')', '/', '{', '}']) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function PosFirstNonSpace(s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] <> ' ' then begin
      Result := i;
      Exit;
    end;
end;

function EscapeQuotes(s: string): string;
begin
  Result := StringReplace(s, '''', '''''', [rfReplaceAll]);
end;

function UnquoteLine(Input: string):string;
var
  p: Integer;
  len: Integer;
  ch: Char;
  Output: string;
  LastStringEnd: Integer;

  function GetNextCh: Boolean;
  begin
    Result := True;
    if p > len then
    begin
      ch := #0;
      Result := False;
    end
    else
    begin
      ch := Input[p];
      inc(p);
    end;
  end;

  function PeekNextCh: Char;
  begin
    if (p) > len then
      Result := #0
    else
      Result := Input[p];
  end;

  procedure GetBraceComment;
  begin
    repeat
      Output := Output + ch;
    until (ch = '}') or (not GetNextCh);
  end;

  procedure GetParenComment;
  begin
    repeat
      Output := Output + ch;
    until ((ch = '*') and (PeekNextCh = ')')) or (not GetNextCh);
    if ch = '*' then begin
      GetNextCh;
      Output := Output + ch;
    end;
  end;

  procedure GetSlashesComment;
  begin
    repeat
      Output := Output + ch;
    until (not GetNextCh);
  end;

  function GetString: Boolean;
  begin
    Result := False;
    if ch <> '''' then
      Exit;
    while GetNextCh do
    begin
      if ch = '''' then
        if PeekNextCh = '''' then
          GetNextCh
        else
          Break;
      Output := Output + ch;
      Result := True;
    end;
  end;

  procedure GetCodeString;
  begin
    Output := Output + ch;
    while GetNextCh do
    begin
      if ch = '''' then
        if PeekNextCh = '''' then
          GetNextCh
        else begin
          Output := Output + ch;
          Break;
        end;
      Output := Output + ch;
    end;
  end;

  procedure GetCode;
  var
    ParenCount: Integer;
  begin
    ParenCount := 0;
    while GetNextCh do
      case ch of
        '(':
          if PeekNextCh = '*' then
            GetParenComment
          else begin
            inc(ParenCount);
            Output := Output + ch;
          end;
        '{':
          GetBraceComment;
        '/':
          if PeekNextCh = '/' then
            GetSlashesComment;
        '''':
          if ParenCount > 0 then
            GetCodeString
          else
            Break;
        ')':
        begin
          if ParenCount > 0 then
            dec(ParenCount);
          Output := Output + ch;
        end
      else
        Output := Output + ch;
      end;
  end;

begin
  Output := '';
  p := 1;
  len := Length(Input);
  LastStringEnd := -1;

  while p <= len do
  begin
    GetCode;
    if GetString then
      LastStringEnd := Length(Output);
  end;

  // Trim off end of line code
  if LastStringEnd <> -1 then
    if (HasConcatOperators(Copy(Output, LastStringEnd+1, MaxInt))) and
       (not HasComplexCode(Copy(Output, LastStringEnd+1, MaxInt))) then
      Delete(Output, LastStringEnd+1, MaxInt);

  Result := TrimRight(Output);
end;

procedure QuoteLines(Lines: TStrings; EndOfLine: string; IndentStart: Integer);
var
  i, p: Integer;
  Indent: Integer;
begin
  Lines[0] := ''''+ TrimRight(Lines[0]) + EndOfLine;

  Indent := IndentStart;

  // Make sure quotes won't happen in the middle of some text
  for i := 1 to Lines.Count - 1 do
  begin
    p := PosFirstNonSpace(Lines[i]);
    if (p > 0) and (Indent > p) then
      Indent := p;
  end;

  // If lines are flush with margin, they probably need to be indented
  if Indent = 1 then
    for i := 1 to Lines.Count - 1 do
      Lines[i] := DupeString(' ', IndentStart-1) + '''' + EscapeQuotes(TrimRight(Lines[i])) + EndOfLine
  else
    for i := 1 to Lines.Count - 1 do
      if Lines[i] = '' then
        Lines[i] := DupeString(' ', Indent-1) + '''' + EndOfLine
      else
        Lines[i] := Copy(Lines[i], 1, Indent-1) + '''' + EscapeQuotes(Copy(TrimRight(Lines[i]), Indent, MaxInt)) + EndOfLine;
end;

procedure UnquoteLines(Lines: TStrings; IndentStart: Integer);
var
  i, p: Integer;
begin
  for i := 0 to Lines.Count - 1 do
    Lines[i] := UnquoteLine(TrimRight(Lines[i]));

  // Remove excessive indenting
  if IndentStart > 1 then begin
    for i := 1 to Lines.Count - 1 do
    begin
      p := PosFirstNonSpace(Lines[i]);
      if (p > 0) and (IndentStart > p) then
        IndentStart := p;
    end;
    for i := 1 to Lines.Count - 1 do
      Lines[i] := Copy(Lines[i], IndentStart, MaxInt);
  end;
end;

end.

