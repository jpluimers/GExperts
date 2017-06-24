// Defines several default settings
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
// Contributors:        Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code.

unit GX_CodeFormatterDefaultSettings;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings;

function BorlandDefaults: TCodeFormatterEngineSettings;
function DelForExDefaults: TCodeFormatterEngineSettings;

implementation

function DelForExDefaults: TCodeFormatterEngineSettings;
begin
  Result.AlignCommentPos := 40;
  Result.AlignComments := False;
  Result.AlignVar := False;
  Result.AlignVarPos := 20;
  Result.BlankProc := True;
  Result.BlankSubProc := False;
  Result.ChangeIndent := True;
  Result.CommentFunction := False;
  Result.CommentUnit := False;
  Result.ExceptSingle := False;
  Result.FeedAfterSemiColon := False;
  Result.FeedAfterThen := False;
  Result.FeedAfterVar := False;
  Result.FeedBeforeEnd := False;
  Result.FeedEachUnit := False;
  Result.FeedElseIf := False;
  Result.FeedRoundBegin := Unchanged;
  Result.FeedRoundTry := Unchanged;
  Result.FillNewWords := [];
  Result.IdentifiersCase := rfUnchanged;
  Result.IndentBegin := False;
  Result.IndentCaseElse := False;
  Result.IndentComments := False;
  Result.IndentCompDirectives := False;
  Result.IndentTry := True;
  Result.IndentTryElse := False;
  Result.NoFeedBeforeThen := False;
  Result.NoIndentElseIf := False;
  Result.NoIndentUsesComma := False;
  Result.RemoveDoubleBlank := False;
  Result.ReservedCase := rfLowerCase;
  Result.SpaceColon := [spAfter];
  Result.SpaceComma := [spAfter];
  Result.SpaceEqualOper := spBoth;
  Result.SpaceLeftBr := spNone;
  Result.SpaceLeftHook := spNone;
  Result.SpaceOperators := spBoth;
  Result.SpacePerIndent := 2;
  Result.SpaceRightBr := spNone;
  Result.SpaceRightHook := spNone;
  Result.SpaceSemiColon := [spAfter];
  Result.StandDirectivesCase := rfLowerCase;
  Result.UpperCompDirectives := True;
  Result.UpperNumbers := True;
  Result.WrapLines := False;
  Result.WrapPosition := 81;
  Result.EndCommentOut := '{*)}';
  Result.StartCommentOut := '{(*}';
end;

function BorlandDefaults: TCodeFormatterEngineSettings;
begin
  Result.AlignCommentPos := 40;
  Result.AlignComments := False;
  Result.AlignVar := False;
  Result.AlignVarPos := 20;
  Result.BlankProc := True;
  Result.BlankSubProc := False;
  Result.ChangeIndent := True;
  Result.CommentFunction := False;
  Result.CommentUnit := False;
  Result.ExceptSingle := False;
  Result.FeedAfterSemiColon := True;
  Result.FeedAfterThen := True;
  Result.FeedAfterVar := True;
  Result.FeedBeforeEnd := True;
  Result.FeedEachUnit := False;
  Result.FeedElseIf := False;
  Result.FeedRoundBegin := NewLine;
  Result.FeedRoundTry := NewLine;
  Result.FillNewWords := [];
  Result.IdentifiersCase := rfUnchanged;
  Result.IndentBegin := False;
  Result.IndentCaseElse := False;
  Result.IndentComments := True;
  Result.IndentCompDirectives := False;
  Result.IndentTry := True;
  Result.IndentTryElse := False;
  Result.NoFeedBeforeThen := True;
  Result.NoIndentElseIf := False;
  Result.NoIndentUsesComma := False;
  Result.RemoveDoubleBlank := True;
  Result.ReservedCase := rfLowerCase;
  Result.SpaceColon := [spAfter];
  Result.SpaceComma := [spAfter];
  Result.SpaceEqualOper := spBoth;
  Result.SpaceLeftBr := spNone;
  Result.SpaceLeftHook := spNone;
  Result.SpaceOperators := spBoth;
  Result.SpacePerIndent := 2;
  Result.SpaceRightBr := spNone;
  Result.SpaceRightHook := spNone;
  Result.SpaceSemiColon := [spAfter];
  Result.StandDirectivesCase := rfLowerCase;
  Result.UpperCompDirectives := True;
  Result.UpperNumbers := True;
  Result.WrapLines := True;
  Result.WrapPosition := 81;
  Result.EndCommentOut := '{*)}';
  Result.StartCommentOut := '{(*}';
end;

end.

