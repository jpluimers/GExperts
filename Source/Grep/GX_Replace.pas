unit GX_Replace;

interface

uses Classes, GX_GrepBackend;

// Replace all matches in all files
function ReplaceAll(ResultList: TStrings; GrepSettings: TGrepSettings): Integer;
// Replace all matches in a single file
function ReplaceAllInFiles(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
// Replace all matches on a single line
function ReplaceLine(LineResult: TLineResult; GrepSettings: TGrepSettings): Integer;

implementation

uses SysUtils, Controls, Dialogs, ToolsAPI, GX_OtaUtils, GX_GenericUtils, RegExpr;

type
  ESkipFileReplaceException = class(Exception);

resourcestring
  SFileChangedAbort = '%s' + sLineBreak + 'has changed since it was searched.  Replacement aborted.'
    + sLineBreak + 'Expected: %s' + sLineBreak + 'Found: %s';
  SUnableToOpen  = 'Unable to open ';
  SNoOpenForms   = 'Replacing strings in open forms is not possible.  Please close the form first.';
  SFileSkipped   = 'The following file will be skipped:';

// Replaces the string between SPos and EPos with the replace string from TGrepSettings
function ReplacePatternInString(CurrentLine: TLineResult; GrepSettings: TGrepSettings): string;
var
  i: Integer;
  FindPos: Integer;
  FindLen: Integer;
  CurrentMatch: TMatchResult;
begin
  Result := CurrentLine.Line;
  for i := CurrentLine.Matches.Count - 1 downto 0 do
  begin
    CurrentMatch := CurrentLine.Matches.Items[i];
    FindPos := CurrentMatch.SPos;
    FindLen := CurrentMatch.EPos - CurrentMatch.SPos + 1;
    Delete(Result, FindPos, FindLen);
    Insert(GrepSettings.Replace, Result, FindPos);
    CurrentMatch.ShowBold := False;
  end;
end;

function ReplacePatternInStringWithRegEx(CurrentLine: TLineResult; GrepSettings: TGrepSettings; RegEx: TRegExpr): string;
var
  i: Integer;
begin
  Result := RegEx.Replace(CurrentLine.Line, GrepSettings.Replace, True);
  for i := CurrentLine.Matches.Count - 1 downto 0 do
    CurrentLine.Matches[i].ShowBold := False;
end;

function ReplaceAll(ResultList: TStrings; GrepSettings: TGrepSettings): Integer;
var
  i: Integer;
  Replaced: Integer;
begin
  Result := 0;
  for i := 0 to ResultList.Count - 1 do
  begin
    if ResultList.Objects[i] is TFileResult then
     begin
       Replaced := ReplaceAllInFiles(ResultList.Objects[i] as TFileResult, GrepSettings);
       Inc(Result, Replaced);
     end;
  end;
end;

function InternalReplace(LineMode: Boolean; ALineResult: TLineResult; AFileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
var
  InMemory: Boolean;
  TempString: string;
  MatchFile: string;
  TempFile: TGXUnicodeStringList;
  LineResult : TLineResult;
  Module: IOTAModule;
  EditWriter: IOTAEditWriter;
  SourceEditor: IOTASourceEditor;
  RegEx: TRegExpr;
  WasBinary: Boolean;

  procedure GetFileLines;
  begin
    if InMemory then
    begin
      if IsForm(MatchFile) then
        raise ESkipFileReplaceException.Create(SNoOpenForms);
      Module := GxOtaGetModule(GxOtaGetBaseModuleFileName(MatchFile));
      if not Assigned(Module) then
        raise Exception.Create(SUnableToOpen + MatchFile);
      SourceEditor := GxOtaGetSourceEditorFromModule(Module, MatchFile);
      if not Assigned(SourceEditor) then
        raise Exception.Create(SUnableToOpen + MatchFile);
      GxOtaLoadFileToUnicodeStrings(SourceEditor.FileName, TempFile, WasBinary);
    end
    else
      GxOtaLoadFileToUnicodeStrings(MatchFile, TempFile, WasBinary);
  end;

  procedure DoReplacement;
  var
    i: Integer;
    FileLine: string;
  begin
    if LineMode then
    begin
      i := ALineResult.LineNo;
      Assert(TempFile.Count >= (LineResult.LineNo - 1));
      FileLine := TempFile.Strings[LineResult.LineNo - 1];
      if LineResult.Line <> FileLine then
        raise Exception.CreateFmt(SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

      if GrepSettings.RegEx then
        TempString := ReplacePatternInStringWithRegEx(LineResult, GrepSettings, RegEx)
      else
        TempString := ReplacePatternInString(LineResult, GrepSettings);
      TempFile.Strings[i -1] := TempString;
      Inc(Result, LineResult.Matches.Count);
    end
    else
    begin
      for i := AFileResult.Count - 1 downto 0 do
      begin
        LineResult := AFileResult.Items[i];
        Inc(Result, LineResult.Matches.Count);
        Assert(TempFile.Count >= (LineResult.LineNo - 1));
        FileLine := TempFile.Strings[LineResult.LineNo - 1];
        if LineResult.Line <> FileLine then
          raise Exception.CreateFmt(SFileChangedAbort, [MatchFile, LineResult.Line, FileLine]);

      if GrepSettings.RegEx then
        TempString := ReplacePatternInStringWithRegEx(LineResult, GrepSettings, RegEx)
      else
        TempString := ReplacePatternInString(LineResult, GrepSettings);
        TempFile.Strings[LineResult.LineNo - 1] := TempString;
      end;
    end;
  end;

  procedure WriteResults;
  var
    FormFile: TFileStream;
    FormSource: TStringStream;
  begin
    if InMemory then
    begin
      EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
      EditWriter.DeleteTo(MaxInt);
      // RemoveLastEOL is necessary because TStringList.Text adds an extra CRLF on the end
      EditWriter.Insert(PAnsiChar(ConvertToIDEEditorString(RemoveTrailingEOL(TempFile.Text))));
      EditWriter := nil;
      Module := nil;
      SourceEditor := nil;
    end
    else
    begin
      if IsForm(MatchFile) and WasBinary then
      begin
        FormFile := nil;
        FormSource := TStringStream.Create(TempFile.Text);
        try
          FormSource.Seek(0, soFromBeginning);
          FormFile := TFileStream.Create(MatchFile, fmOpenWrite or fmShareDenyWrite);
          FormFile.Seek(0, soFromBeginning);
          ObjectTextToResource(FormSource, FormFile);
          FormFile.Size;
        finally
          FreeAndNil(FormSource);
          FreeAndNil(FormFile);
        end;
      end
      else
        TempFile.SaveToFile(MatchFile);
    end;
  end;

begin
  Result := 0;
  WasBinary := False;
  if LineMode then
  begin
    LineResult := ALineResult;
    MatchFile := TFileResult(LineResult.Collection).FileName;
  end
  else
    MatchFile := AFileResult.FileName;

  RegEx := nil;
  TempFile := TGXUnicodeStringList.Create;
  try
    if GrepSettings.RegEx then
    begin
      RegEx := TRegExpr.Create;
      RegEx.Expression := GrepSettings.Pattern;
      RegEx.ModifierG := True;
      RegEx.ModifierI := not GrepSettings.CaseSensitive;
      RegEx.Compile;
    end;

    InMemory := GxOtaIsFileOpen(MatchFile, True);
    try
      GetFileLines;
    except on E: ESkipFileReplaceException do
      begin
        E.Message := E.Message + sLineBreak + SFileSkipped +sLineBreak+ MatchFile;
        if MessageDlg(E.Message, mtWarning, [mbOK, mbCancel], 0) = mrCancel then
          Abort
        else
          Exit;
      end;
    end;
    DoReplacement;
    WriteResults;
  finally
    FreeAndNil(RegEx);
    FreeAndNil(TempFile);
  end;
end;

function ReplaceAllInFiles(FileResult: TFileResult; GrepSettings: TGrepSettings): Integer;
begin
  Result := InternalReplace(False, nil, FileResult, GrepSettings);
end;

function ReplaceLine(LineResult: TLineResult; GrepSettings: TGrepSettings): Integer;
begin
  Result := InternalReplace(True, LineResult, nil, GrepSettings);
end;

end.

