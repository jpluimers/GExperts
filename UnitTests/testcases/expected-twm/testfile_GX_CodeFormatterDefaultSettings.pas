// Defines several default settings
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterDefaultSettings;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_CodeFormatterTypes;

function BorlandDefaults: TSettings;

implementation

function BorlandDefaults: TSettings;
begin
//  Result.Wizard.CapFileName := '';
//  Result.Wizard.ToolPosition := 0;
//  Result.Wizard.Shortcut.First := 0;
//  Result.Wizard.Shortcut.Second := #0;

  Result.AlignCommentPos := 40;
  Result.AlignComments := False;
  Result.AlignVar := False;
  Result.AlignVarPos := 20;
  Result.BlankProc := True;
  Result.BlankSubProc := False;
  Result.ChangeIndent := True;
  Result.CommentFunction := False;
  Result.CommentUnit := False;
  Result.FeedAfterSemiColon := True;
  Result.FeedAfterThen := True;
  Result.FeedAfterVar := True;
  Result.FeedBeforeEnd := True;
  Result.FeedEachUnit := False;
  Result.FeedElseIf := False;
  Result.FeedRoundBegin := NewLine;
  Result.FillNewWords := fmUnchanged;
  Result.indentBegin := False;
  Result.IndentCaseElse := False;
  Result.IndentComments := True;
  Result.IndentCompDirectives := False;
  Result.IndentTry := True;
  Result.IndentTryElse := False;
  Result.NoFeedBeforeThen := True;
  Result.NoIndentElseIf := False;
  Result.RemoveDoubleBlank := True;
  Result.ReservedCase := rfLowerCase;
  Result.ShortCut := 0;
  Result.SpaceColon := spAfter;
  Result.SpaceComma := spAfter;
  Result.SpaceEqualOper := spBoth;
  Result.SpaceLeftBr := spNone;
  Result.SpaceLeftHook := spNone;
  Result.SpaceOperators := spBoth;
  Result.SpacePerIndent := 2;
  Result.SpaceRightBr := spNone;
  Result.SpaceRightHook := spNone;
  Result.SpaceSemiColon := spAfter;
  Result.StandDirectivesCase := rfLowerCase;
  Result.UpperCompDirectives := True;
  Result.UpperNumbers := True;
  Result.WrapLines := True;
  Result.WrapPosition := 81;
  StrCopy(Result.EndCommentOut, '{*)}');
  StrCopy(Result.StartCommentOut, '{(*}');
end;

end.
