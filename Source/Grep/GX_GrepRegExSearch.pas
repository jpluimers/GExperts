unit GX_GrepRegExSearch;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils, Classes, RegExpr, GX_GenericUtils, GX_OtaUtils, StrUtils;

type
  TFoundEvent = procedure(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString) of object;

  TSearcher = class(TObject)
  private
    FPattern: TGXUnicodeString;
    FData: TGXUnicodeStringList;
    FOnFound: TFoundEvent;
    FRegEx: TRegExpr;
    FCaseSensitive: Boolean;
    FWholeWord: Boolean;
    FRegularExpression: Boolean;
    FFileName: string;
    FNoComments: Boolean;
    procedure SearchLineRegEx(LineStr: string; LineNo: Integer);
    procedure SearchLineRaw(LineStr: string; LineNo: Integer);
    procedure SetFileName(const Value: string);
    function CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Pattern: TGXUnicodeString read FPattern write FPattern;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property NoComments: Boolean read FNoComments write FNoComments;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property RegularExpression: Boolean read FRegularExpression write FRegularExpression;
    property WholeWord: Boolean read FWholeWord write FWholeWord;
    property FileName: string read FFileName write SetFileName;
    procedure Execute; overload;
  end;

implementation

{ TODO : The space replacement for comments needs to use a different, more obscure character such as ÷©®¡™‡|~‰±°•? so that we don't match as many false-positives when searcing for spaces }

{ TSearcher }

function TSearcher.CheckWholeWord(Line: TGXUnicodeString; StartCol, EndCol: Integer): Boolean;
var
  PrevChar: TGXUnicodeChar;
  NextChar: TGXUnicodeChar;
  FirstMatchChar: TGXUnicodeChar;
  LastMatchChar: TGXUnicodeChar;
begin
  PrevChar := ' ';
  NextChar := ' ';
  if StartCol >= 2 then
    PrevChar := Line[StartCol - 1];
  if Length(Line) > EndCol then
    NextChar := Line[EndCol + 1];
  FirstMatchChar := Line[StartCol];
  LastMatchChar := Line[EndCol];
  Result := (((not IsCharIdentifier(PrevChar)) or (not IsCharIdentifier(FirstMatchChar)))
    and ((not IsCharIdentifier(NextChar)) or (not IsCharIdentifier(LastMatchChar))));
end;

constructor TSearcher.Create;
begin
  FRegEx := TRegExpr.Create;
end;

destructor TSearcher.Destroy;
begin
  FreeAndNil(FRegEx);
  inherited;
end;

procedure TSearcher.Execute;
var
  i, index, index2: Integer;
  iParen, iCurly, iTick, iSlash : Integer;
  bInCurlyComment, bInParenComment, bInTick: Boolean;
  s, sTick: string;

  procedure RemoveTicks;
  var
    idx : Integer;
  begin
    // Now remove all comments in the line.
    bInTick := False;
    for idx := 1 to Length(s) do
    begin
      // If we run into a comment block, we need to stop removing the string literals.
      if ((not bInTick) and ((idx = iParen) or (idx = iCurly) or (idx = iSlash))) then
      begin
        break;
      end;

      // To here we're checking each character and removing whatever string literals we find.
      if s[idx] = #39 then
      begin
        bInTick := not bInTick; // comment was toggled
        s[idx] := ' '; // replace with a space
        sTick[idx] := #39; // save string literal
      end
      else if bInTick then
      begin
        sTick[idx] := s[idx]; // remember literal
        s[idx] := ' '; // replace with a space
      end;
    end;
  end;

  procedure RemoveComments(sStart, sEnd: string);
  begin
    // Now we need to check if the curly comment or the parenthesis comment has started.
    index := pos(sStart, s);

    // Did we end this type of comment in the same line?
    index2 := pos(sEnd, Copy(s, index + Length(sStart), Length(s)));

    if (index2 > 0) then
    begin
      // add the first index and position where real code would start again
      index2 := index2 + index + Length(sEnd) + Length(sStart) - 1;
      // replace with just spaces
      s := Copy(s, 1, index - 1) + DupeString(' ', index2 - index) + Copy(s, index2, Length(s));

      // Look for another.
      index := pos(sStart, s);
    end
    else
    begin
      // s starts a comment but doesn't end it.
      s := Copy(s, 1, index - 1);

      if SameText(sStart, '{') then
      begin
        bInCurlyComment := True;
      end
      else // if SameText(sStart, '(*') then
      begin
        bInParenComment := True;
      end;
    end;
  end;

begin
  if not Assigned(FData) then
    raise Exception.Create('Data to search not provided');
  if IsEmpty(Pattern) then
    raise Exception.Create('Search pattern is empty');

  if RegularExpression then
  begin
    FRegEx.ModifierI := not CaseSensitive;
    FRegEx.Expression := Pattern;
    FRegEx.Compile;
  end;

  bInCurlyComment := False;
  bInParenComment := False;

  for i := 0 to FData.Count - 1 do
  begin
    // Ensure we are not updating the original line by creating a unique copy.
    s := FData[i];
    UniqueString(s);
    // This string will store the string literals for adding back in once the parsing is done.
    sTick := DupeString(' ', Length(s));

    if NoComments then
    begin
      { Because of all the different ways of commenting, as well as the quote marks
        this is a fairly difficult task.  There is likely a better way to handle this,
        but this way seems to work! PRG Jan.11.2012 cantak@gmail.com }

      // First, if we are already in a type of comment, we need to see if we left it.
      if (bInCurlyComment) then
      begin
        index := pos('}', s);

        // If we found it, replace with empty space:
        // End of a comment line} CODE MAYBE
        //     [ spaces ]         CODE MAYBE
        if (index > 0) then
        begin
          s := DupeString(' ', i) + Copy(s, i + 1, Length(s));
          bInCurlyComment := False;
        end;
      end
      else if (bInParenComment) then
      begin
        index := pos('*)', s);
        if (index > 0) then
        begin
          s := DupeString(' ', i + 1) + Copy(s, i + 2, Length(s));
          bInParenComment := False;
        end;
      end;

      if ((bInCurlyComment) or (bInParenComment)) then
      begin
        // The whole line is irrelevant, and we don't even need the spacers anymore.
        // No search will be performed.
        s := EmptyStr;
      end;

      // Here's the conundrum.   We can have one line with mixed { comments{* <-- see here!
      // The double pipes eliminate to the end of the line, UNLESS they themselves are comments
      // caused from an earlier curly or paren/star combo.  Tick marks may make the comment
      // starters and stoppers irrelevant too.  So we always need to handle the FIRST
      // one in the line, and move on from there.
      iParen := pos('(*', s);
      iCurly := pos('{', s);
      iTick := pos(#39, s);
      iSlash := pos('//', s);

      // While any of these exists, we need to process the first one only.
      while ((iParen > 0) or (iCurly > 0) or (iTick > 0) or (iSlash > 0)) do
      begin
        if ((iParen > 0) and
            ((iParen < iCurly) or (iCurly = 0)) and
            ((iParen < iTick) or (iTick = 0)) and
            ((iParen < iSlash) or (iSlash = 0))) then
        begin
          // Parenthesis is first
          RemoveComments('(*', '*)');
        end
        // We already confirmed the parenthesis is not first to here.
        else if ((iCurly > 0) and
                 ((iCurly < iTick) or (iTick = 0)) and
                 ((iCurly < iSlash) or (iSlash = 0))) then
        begin
          RemoveComments('{', '}');
        end
        // Curly and parenthesis are NOT first to here
        else if ((iTick > 0) and
                 ((iTick < iSlash) or (iSlash = 0))) then
        begin
          RemoveTicks;
        end
        // Since the only possibility left is slash, we should hit it now
        else if (iSlash > 0) then
        begin
          s := Copy(s, 1, iSlash - 1);
        end
        else
        begin
          // This begin/end block is only here to make sure there's no infinite
          // loop about to happen.  Should not be possible to hit.
          Assert(False);
        end;

        // Reassign these for the line.
        iParen := pos('(*', s);
        iCurly := pos('{', s);
        iTick := pos(#39, s);
        iSlash := pos('//', s);
      end;

      // To here, s has had all string literals removed (only for parsing out comment
      // blocks. It's also had all comment blocks removed.  Add the string literals back.
      // Increase the length of s to its original size so the loop below does not fail.
      s := Copy(s + DupeString(' ', Length(sTick)), 1, Length(sTick));
      for index := 1 to Length(sTick) do
      begin
        if sTick[index] <> ' ' then
        begin
          s[index] := sTick[index];
        end;
      end;

      // Finally for the final parsing, and the search, we can shrink ending
      // spaces out of s.
      index := Length(s);
      while ((index > 0) and (s[index] = ' ')) do Dec(index);
      s := Copy(s, 1, index);
    end;

    // if s is empty we were entirely in a comment and we needed to skip this.
    if s <> EmptyStr then
    begin
      if RegularExpression then
        SearchLineRegEx(s, i)
      else
        SearchLineRaw(s, i);
    end;
  end;
end;

procedure TSearcher.SearchLineRaw(LineStr: string; LineNo: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
  StartIndex: Integer;
  MatchPos: Integer;

  procedure GetNextMatch;
  begin
    if CaseSensitive then
      MatchPos := PosFrom(Pattern, LineStr, StartIndex)
    else
      MatchPos := CaseInsensitivePosFrom(Pattern, LineStr, StartIndex);
  end;

begin
  { Added passed string called LineStr. Example:
    Line[i]: ShowMessage(' Test '); // Comment goes here
    LineStr: ShowMessage('      ');
    These ONLY differ if the user searches for NO COMMENTS.
    The search then finds index matches in LineStr, but highlights them
    using the original Line[i]. }

  Line := FData[LineNo];
  StartIndex := 1;
  GetNextMatch;
  while MatchPos > 0 do
  begin
    StartCol := MatchPos;
    EndCol := MatchPos + Length(Pattern) - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0);
    if (not WholeWord) or (CheckWholeWord(LineStr, StartCol, EndCol)) then
      if Assigned(FOnFound) then
        FOnFound(LineNo + 1, StartCol, EndCol, Line);
    StartIndex := StartCol + 1;
    GetNextMatch;
  end;
end;

procedure TSearcher.SearchLineRegEx(LineStr: string; LineNo: Integer);
var
  StartCol: Integer;
  EndCol: Integer;
  Line: TGXUnicodeString;
begin
  Line := FData[LineNo];
  if FRegEx.Exec(LineStr) then repeat
  begin
    //if FRegEx.SubExprMatchCount > 0 then
    //  raise Exception.Create('Subexpression searches are not supported');
    StartCol := FRegEx.MatchPos[0];
    EndCol := StartCol + FRegEx.MatchLen[0] - 1;
    Assert(StartCol > 0);
    Assert(EndCol > 0, 'Invalid regular expression match, try escaping any special characters using ''\''');
    if WholeWord then
      if not CheckWholeWord(LineStr, StartCol, EndCol) then
        Continue;
    if Assigned(FOnFound) then
      FOnFound(LineNo + 1, StartCol, EndCol, Line);
  end until not FRegEx.ExecNext;
end;

procedure TSearcher.SetFileName(const Value: string);
begin
  FFileName := Value;
  if not Assigned(FData) then
    FData := TGXUnicodeStringList.Create
  else
    FData.Clear;
  GxOtaLoadFileToUnicodeStrings(FFileName, FData);
end;

end.

