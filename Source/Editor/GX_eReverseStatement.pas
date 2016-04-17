unit GX_eReverseStatement;

interface

uses
  Classes, GX_eSelectionEditorExpert;

type
  TReverseStatementExpert = class(TSelectionEditorExpert)
  private
    FMadeChanges: Boolean;
    function ReverseOneLine(const S: string): string;
    function ReverseAssignment(var S: string): Boolean;
    function ReverseForLoop(var S: string): Boolean;
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

uses
  SysUtils, GX_EditorExpert, GX_OtaUtils, GX_GenericUtils;

{ TReverseStatementExpert }

constructor TReverseStatementExpert.Create;
begin
  inherited Create;
end;

function TReverseStatementExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scAlt + scShift + Ord('R');
end;

function TReverseStatementExpert.GetDisplayName: string;
resourcestring
  SReverseStatementName = 'Reverse Statement';
begin
  Result := SReverseStatementName;
end;

function TReverseStatementExpert.GetHelpString: string;
resourcestring
  SReverseStatementHelp =
    '  This expert reverses all assignment statements and the loop direction of ' +
    'for loops in a selected block of code.  ' + sLineBreak +
    'For example, a statement like "Foo := Bar;" would be changed into: "Bar := Foo;"  ' +
    'and "for i := 0 to 99 do" into: "for i := 99 downto 0 do".' + sLineBreak + sLineBreak +
    '  This expert supports both Delphi and C++ assignments, but only Delphi for loops. ' +
    'It expects all reversible statements to be contained on a single line.';
begin
  Result := SReverseStatementHelp;
end;

class function TReverseStatementExpert.GetName: string;
begin
  Result := 'ReverseStatement';
end;

function TReverseStatementExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TReverseStatementExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  i: Integer;
begin
  Assert(Assigned(Lines));
  FMadeChanges := False;
  for i := 0 to Lines.Count - 1 do
    Lines[i] := ReverseOneLine(Lines[i]);
  Result := FMadeChanges;
end;

function TReverseStatementExpert.ReverseOneLine(const S: string): string;
begin
  Result := S;
  if ReverseForLoop(Result) then
    Exit;
  if ReverseAssignment(Result) then
    Exit;
end;

function TReverseStatementExpert.ReverseAssignment(var S: string): Boolean;
var
  i: Integer;
  AssignOp: string;
  AssignPos: Integer;
  SemPos: Integer;
  StringBefore: string;
  StringAfter: string;
  TrailingString: string;
  SpaceBefore: string;
  SpaceAfter: string;
  LeadingSpace: string;
begin
  if IsCPPSourceModule(GxOtaGetCurrentSourceEditor.FileName) then
    AssignOp := '='
  else
    AssignOp := ':=';
  Result := False;
  if S = '' then
    Exit;
  AssignPos := Pos(AssignOp, S);
  SemPos := LastDelimiter(';', S);
  TrailingString := Copy(S, SemPos + 1, 9999);

  if (AssignPos > 1) and (SemPos > 3) and (Length(S) > AssignPos + 1) then
  begin
    if StrContains('//', S) then
      if Pos('//', S) < AssignPos then
        Exit;
    if StrContains('(*', S) then
      if Pos('(*', S) < AssignPos then
        Exit;
    if IsCPPSourceModule(GxOtaGetCurrentSourceEditor.FileName) then
    begin
      if StrContains('/*', S) then
        if Pos('/*', S) < AssignPos then
          Exit;
      if Pos(S, '==') = AssignPos then
        Exit;
    end
    else
    begin
      if StrContains('{', S) then
        if Pos('{', S) < AssignPos then
          Exit;
    end;
    i := 1;
    while IsCharWhitespace(S[i]) do
    begin
      LeadingSpace := LeadingSpace + S[i];
      Inc(i);
    end;
    StringBefore := Copy(S, i, AssignPos - i);
    i := AssignPos - 1;
    if StringBefore <> '' then
    begin
      while IsCharWhitespace(S[i]) do
      begin
        SpaceBefore := S[i] + SpaceBefore;
        SetLength(StringBefore, Length(StringBefore) - 1);
        Dec(i)
      end;
    end;
    i := AssignPos + Length(AssignOp);
    while IsCharWhitespace(S[i]) do
    begin
      SpaceAfter := SpaceAfter + S[i];
      Inc(i);
    end;
    StringAfter := Copy(S, i, SemPos - i);
    S := LeadingSpace + StringAfter + SpaceAfter + AssignOp +
      SpaceBefore + StringBefore + ';' + TrailingString;
    Result := True;
    FMadeChanges := True;
  end;
end;

function TReverseStatementExpert.ReverseForLoop(var S: string): Boolean;
const
  cForString = 'for ';
  cAssignString = ':=';
  cToString = ' to ';
  cDownToString = ' downto ';
  cDoString = ' do';
var
  Down: Boolean;
  iCurr, iPrev: Integer;
  LeadingString: string;
  TrailingString: string;
  StringBefore: string;
  StringAfter: string;

  // Copy part of S from iFrom including to iTo excluding:
  function CopyS(iFrom, iTo: Integer): string;
  begin
    Result := Copy(S, iFrom, iTo - iFrom);
  end;

  // Search Pat in S, starting from iPrev:
  function PosInSFromPrev(const Pat: string): Integer;
  begin
    Result := CaseInsensitivePosFrom(Pat, S, iPrev);
  end;

begin
  // STATE-comments: The two ^s show where iPrev and iCurr point to.
  // The sample input is:
  // LeadingStuff; for i := aaa to zzz do // trailing stuff

  Result := False;

  iPrev := 1;
  iCurr := PosInSFromPrev(cForString);
  if iCurr = 0 then
    Exit;
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //        ^             ^
  //      iPrev         iCurr

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cAssignString);
  if iCurr = 0 then
    Exit;
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //                      ^     ^
  Inc(iCurr, Length(cAssignString));
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //                      ^       ^
  LeadingString := CopyS(1, iCurr);

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cDownToString);
  Down := iCurr > 0;
  if not Down then
  begin
    iCurr := PosInSFromPrev(cToString);
    if iCurr = 0 then
      Exit;
  end;
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //                              ^   ^
  StringBefore := CopyS(iPrev, iCurr);
  if Down then
    Inc(iCurr, Length(cDownToString))
  else
    Inc(iCurr, Length(cToString));
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //                              ^       ^

  iPrev := iCurr;
  iCurr := PosInSFromPrev(cDoString);
  if iCurr = 0 then
    Exit;
  // STATE: LeadingStuff; for i := aaa to zzz do // trailing stuff
  //                                      ^  ^
  StringAfter := CopyS(iPrev, iCurr);
  TrailingString := CopyS(iCurr, Succ(Length(S)));

  if Down then
    S := cToString
  else
    S := cDownToString;
  S := TrimRight(LeadingString) + ' ' + Trim(StringAfter) + S +
    Trim(StringBefore) + ' ' + TrimLeft(TrailingString);
  Result := True;
  FMadeChanges := True;
end;

initialization
  RegisterEditorExpert(TReverseStatementExpert);
end.

