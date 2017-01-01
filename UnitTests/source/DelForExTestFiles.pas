unit DelForExTestFiles;

{$OPTIMIZATION off}

interface

uses
  Classes,
  SysUtils,
  TestFrameWork,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterEngine,
  GX_GenericUtils;

type
  TTestTestfiles = class(TTestCase)
  private
    FFormatter: TCodeFormatterEngine;
    procedure TrimTrailingCrLf(_sl: TGxUnicodeStringList);
    procedure TestFile(const _Filename: string; _AllowFailure: Boolean = False);
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; virtual; abstract;
    function GetResultDir: string; virtual; abstract;
    function GetConfigDirBS: string;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ExecuteDoubleClickAction; override;
  published
    procedure testAbstractSealedClass;
    procedure testAngledBrackets;
    procedure testArabic;
    procedure testAsm;
    procedure testAsmProblem1;
    procedure testAssemblerNewlines;
    procedure testCharList;
    procedure testClassVar;
    procedure testCommentEnd;
    procedure testCompilerDirectives;
    procedure testConstSet;
    procedure testConstSetWithComment;
    procedure testConstSetWithCommentAtEnd;
    procedure testControlChars;
    procedure testDisableFormatComment;
    procedure testDotFloat;
    procedure testElseAtEnd;
    procedure testEmptyProgram;
    procedure testEmptyStringAssignment;
    procedure testEmptyUnit;
    procedure testFormula;
    procedure testHashCharStrings;
    procedure testHexNumbers;
    procedure testIfdefs;
    procedure testIfElseendif;
    procedure testIfThenElse;
    procedure testIfThenTry;
    procedure testIndentComment;
    procedure testJustOpeningComment;
    procedure testJustOpeningStarCommentInAsm;
    procedure testConstVar;
    procedure testLargeFile;
    procedure testOperatorOverloading;
    procedure testQuotesError;
    procedure testRecordMethod;
    procedure testSingleElse;
    procedure testSlashCommentToCurly;
    procedure testStarCommentAtEol;
    procedure testStrictVisibility;
    procedure testStringWithSingleQuotes;
    procedure testTabBeforeEndInAsm;
    procedure testTripleQuotes;
    procedure testUnmatchedElse;
    procedure testUnmatchedEndif;
    procedure testUnmatchedIfdef;
    procedure testUnterminatedString;
    procedure testClassProperties;
    procedure testClassProperties2;
    procedure testClassProperties3;
    procedure testNestedClass;
    procedure testNestedClass2;
    procedure testNestedClass3;
    procedure testNestedClass4;
    procedure testNestedClass5;
    procedure testNestedClass6;
    procedure testClassInImplementation;
    procedure testGenericClass;
    procedure testGenericClass2;
    procedure testfileGenericCreate;
    procedure testfileFakeGenericCreate;
    procedure testUsesWithComment;
    procedure testTypeOf;
    procedure testNestedEventType;
    procedure TestUnicode;
    procedure testCurlyBracesInWhile;
    procedure testAnonymousCurrentlyFails;
    procedure testCurlyHalfCommentEndCurrentlyFails;
    procedure testIfThenElse2CurrentlyFails;
    procedure testComplexCurrentlyFails;
  end;

type
  TTestFilesHeadworkFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  published
  end;

type
  TTestFilesBorlandFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesDelforFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesDefaultFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesTwmFormatting = class(TTestTestfiles)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

type
  TTestFilesSpecial = class(TTestFilesTwmFormatting)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
    function GetResultDir: string; override;
  end;

implementation

uses
  Windows,
  ShellAPI,
  StrUtils,
  Dialogs,
  GX_CodeFormatterConfigHandler;

{ TTestTestfiles }

procedure TTestTestfiles.ExecuteDoubleClickAction;
const
  CURRENTLY_FAILS = 'CurrentlyFails';
var
  s: string;
  Filename: string;
  InFile: string;
  ExpectedFile: string;
  OutputFile: string;
  Params: string;
begin
  // 'testStarCommentAtEol'
  s := Self.FTestName;
  Assert(StartsText('test', s));
  s := Copy(s, 5);
  if EndsText(CURRENTLY_FAILS, s) then
    s := Copy(s, 1, Length(s) - Length(CURRENTLY_FAILS));

  Filename := 'testfile_' + s + '.pas';
  InFile := 'testcases\input\' + Filename;
  ExpectedFile := 'testcases\expected-' + GetResultDir + '\' + Filename;
  OutputFile := 'testcases\output-' + GetResultDir + '\' + Filename;
  Params := Format('"%s" "%s"', [ExpectedFile, OutputFile]);
  ShellExecute(0, '', PChar('C:\Program Files (x86)\Beyond Compare 3\bcompare.exe'),
    PChar(Params), '', SW_NORMAL);
end;

function TTestTestfiles.GetConfigDirBS: string;
begin
  Result := '..\release\';
end;

procedure TTestTestfiles.SetUp;
var
  Settings: TCodeFormatterEngineSettings;
begin
  inherited;
  Settings := GetFormatSettings;
  FFormatter := TCodeFormatterEngine.Create;
  FFormatter.Settings.Settings := Settings;
end;

procedure TTestTestfiles.TearDown;
begin
  inherited;
  FFormatter.Free;
end;

procedure TTestTestfiles.TrimTrailingCrLf(_sl: TGxUnicodeStringList);
var
  cnt: Integer;
begin
  cnt := _sl.Count;
  while cnt > 0 do begin
    if _sl[cnt - 1] <> '' then
      Exit;
    Dec(cnt);
    _sl.Delete(cnt);
  end;
end;

type
  EFileDoesNotExist = class(EAbort)

  end;

procedure TTestTestfiles.TestFile(const _Filename: string; _AllowFailure: Boolean);
var
  Filename: string;
  InFile: string;
  ExpectedFile: string;
  ExpectedText: TGxUnicodeStringList;
  st: TGxUnicodeStringList;
begin
  Filename := 'testfile_' + _Filename + '.pas';
  InFile := 'testcases\input\' + Filename;
  ExpectedFile := 'testcases\expected-' + GetResultDir + '\' + Filename;
  if not FileExists(InFile) then begin
//    ExpectedException := EFileDoesNotExist;
    raise EFileDoesNotExist.Create('Input file does not exist');
  end;
  if not FileExists(ExpectedFile) then begin
//    ExpectedException := EFileDoesNotExist;
    raise EFileDoesNotExist.Create('Expected file does not exist');
  end;

  ExpectedText := nil;
  st := TGxUnicodeStringList.Create;
  try
    st.LoadFromFile(InFile);
    ExpectedText := TGxUnicodeStringList.Create;
    ExpectedText.LoadFromFile(ExpectedFile);
    FFormatter.Execute(st);
    try
      TrimTrailingCrLf(ExpectedText);
      TrimTrailingCrLf(st);
// uncomment if you want to use e.g. BeyondCompare do the comparison
//      st.SaveToFile('testcases\output-' + GetResultDir + '\' + Filename);
      CheckEquals(ExpectedText.Text, st.Text, 'error in output');
    except
      st.SaveToFile('testcases\output-' + GetResultDir + '\' + Filename);
      if not _AllowFailure then
        raise;
    end;
  finally
    ExpectedText.Free;
    st.Free;
  end;
end;

procedure TTestTestfiles.testStarCommentAtEol;
begin
  TestFile('StarCommentAtEol');
end;

procedure TTestTestfiles.testSlashCommentToCurly;
begin
  TestFile('SlashCommentToCurly');
end;

procedure TTestTestfiles.testStrictVisibility;
begin
  TestFile('strictvisibility');
end;

procedure TTestTestfiles.testCompilerDirectives;
begin
  TestFile('compilerdirectives');
end;

procedure TTestTestfiles.testComplexCurrentlyFails;
begin
  TestFile('complex', True);
end;

procedure TTestTestfiles.testAssemblerNewlines;
begin
  TestFile('assemblernewline');
end;

procedure TTestTestfiles.testIfdefs;
begin
  TestFile('ifdefs');
end;

procedure TTestTestfiles.testUnmatchedIfdef;
begin
  TestFile('unmatchedifdef');
end;

procedure TTestTestfiles.testUnmatchedElse;
begin
  TestFile('unmatchedelse');
end;

procedure TTestTestfiles.testUnmatchedEndif;
begin
  TestFile('unmatchedendif');
end;

procedure TTestTestfiles.testIfElseendif;
begin
  TestFile('IfElseEndif');
end;

procedure TTestTestfiles.testIfThenElse;
begin
  TestFile('ifthenelse');
end;

procedure TTestTestfiles.testIfThenElse2CurrentlyFails;
begin
  TestFile('ifthenelse2', True);
end;

procedure TTestTestfiles.testIfThenTry;
begin
  TestFile('ifthentry');
end;

procedure TTestTestfiles.testDisableFormatComment;
begin
  TestFile('DisableFormatComment');
end;

procedure TTestTestfiles.testLargeFile;
begin
  TestFile('LargeFile');
end;

procedure TTestTestfiles.testNestedClass;
begin
  TestFile('NestedClass');
end;

procedure TTestTestfiles.testNestedClass2;
begin
  TestFile('NestedClass2');
end;

procedure TTestTestfiles.testNestedClass3;
begin
  TestFile('NestedClass3');
end;

procedure TTestTestfiles.testNestedClass4;
begin
  TestFile('NestedClass4');
end;

procedure TTestTestfiles.testNestedClass5;
begin
  TestFile('NestedClass5');
end;

procedure TTestTestfiles.testNestedClass6;
begin
  TestFile('NestedClass6');
end;

procedure TTestTestfiles.testNestedEventType;
begin
  TestFile('NestedEventType');
end;

procedure TTestTestfiles.testOperatorOverloading;
begin
  TestFile('OperatorOverloading');
end;

procedure TTestTestfiles.testTripleQuotes;
begin
  TestFile('triplequotes');
end;

procedure TTestTestfiles.testTypeOf;
begin
  TestFile('TypeOf');
end;

procedure TTestTestfiles.TestUnicode;
begin
  TestFile('unicode');
end;

procedure TTestTestfiles.testSingleElse;
begin
  TestFile('singleelse');
end;

procedure TTestTestfiles.testQuotesError;
begin
  TestFile('QuotesError');
end;

procedure TTestTestfiles.testRecordMethod;
begin
  TestFile('RecordMethod');
end;

procedure TTestTestfiles.testJustOpeningComment;
begin
  TestFile('OpeningCommentOnly');
end;

procedure TTestTestfiles.testElseAtEnd;
begin
  TestFile('ElseAtEnd');
end;

procedure TTestTestfiles.testJustOpeningStarCommentInAsm;
begin
  // I actually thought this would crash...
  TestFile('OpeningStarCommentInAsm');
end;

procedure TTestTestfiles.testTabBeforeEndInAsm;
begin
  TestFile('TabBeforeEndInAsm');
end;

procedure TTestTestfiles.testEmptyProgram;
begin
  TestFile('EmptyProgram');
end;

procedure TTestTestfiles.testEmptyUnit;
begin
  TestFile('EmptyUnit');
end;

procedure TTestTestfiles.testIndentComment;
begin
  TestFile('IndentComment');
end;

procedure TTestTestfiles.testUnterminatedString;
begin
  TestFile('UnterminatedString');
end;

procedure TTestTestfiles.testUsesWithComment;
begin
  TestFile('UsesWithComment');
end;

procedure TTestTestfiles.testStringWithSingleQuotes;
begin
  // note this actually contains a string with the TEXT #13#10:
  // >hello ' #13#10);<
  TestFile('StringWithSingleQuote');
end;

procedure TTestTestfiles.testEmptyStringAssignment;
begin
  TestFile('EmptyStringAssignment');
end;

procedure TTestTestfiles.testHashCharStrings;
begin
  TestFile('HashCharStrings');
end;

procedure TTestTestfiles.testDotFloat;
begin
  TestFile('DotFloat');
end;

procedure TTestTestfiles.testHexNumbers;
begin
  TestFile('HexNumbers');
end;

procedure TTestTestfiles.testConstSet;
begin
  TestFile('ConstSet');
end;

procedure TTestTestfiles.testConstSetWithComment;
begin
  TestFile('ConstSetWithComment');
end;

procedure TTestTestfiles.testConstSetWithCommentAtEnd;
begin
  TestFile('ConstSetWithCommentAtEnd');
end;

procedure TTestTestfiles.testControlChars;
begin
  TestFile('ControlChars');
end;

procedure TTestTestfiles.testConstVar;
begin
  TestFile('ConstVar');
end;

procedure TTestTestfiles.testfileGenericCreate;
begin
  TestFile('GenericCreate');
end;

procedure TTestTestfiles.testfileFakeGenericCreate;
begin
  TestFile('FakeGenericCreate');
end;

procedure TTestTestfiles.testFormula;
begin
  TestFile('Formula');
end;

procedure TTestTestfiles.testGenericClass;
begin
  TestFile('GenericClass');
end;

procedure TTestTestfiles.testGenericClass2;
begin
  TestFile('GenericClass2');
end;

procedure TTestTestfiles.testAbstractSealedClass;
begin
  TestFile('AbstractSealedClass');
end;

procedure TTestTestfiles.testAngledBrackets;
begin
  TestFile('AngledBrackets');
end;

procedure TTestTestfiles.testAnonymousCurrentlyFails;
begin
  TestFile('Anonymous', True);
end;

procedure TTestTestfiles.testArabic;
begin
  TestFile('Arabic');
end;

procedure TTestTestfiles.testAsm;
begin
  TestFile('asm');
end;

procedure TTestTestfiles.testAsmProblem1;
begin
  TestFile('AsmProblem1');
end;

procedure TTestTestfiles.testCharList;
begin
  TestFile('CharList');
end;

procedure TTestTestfiles.testClassInImplementation;
begin
  TestFile('ClassInImplementation');
end;

procedure TTestTestfiles.testClassProperties;
begin
  TestFile('ClassProperties');
end;

procedure TTestTestfiles.testClassProperties2;
begin
  TestFile('ClassProperties2');
end;

procedure TTestTestfiles.testClassProperties3;
begin
  TestFile('ClassProperties3');
end;

procedure TTestTestfiles.testClassVar;
begin
  TestFile('ClassVar');
end;

procedure TTestTestfiles.testCommentEnd;
begin
  TestFile('CommentEnd');
end;

procedure TTestTestfiles.testCurlyBracesInWhile;
begin
  TestFile('CurlyBracesInWhile');
end;

procedure TTestTestfiles.testCurlyHalfCommentEndCurrentlyFails;
begin
  TestFile('CurlyHalfCommentEnd', True);
end;

{ TTestFilesHeadworkFormatting }

function TTestFilesHeadworkFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(GetConfigDirBS + 'FormatterSettings-headwork.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesHeadworkFormatting.GetResultDir: string;
begin
  Result := 'headwork';
end;

{ TTestFilesBorlandFormatting }

function TTestFilesBorlandFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
begin
  Result := BorlandDefaults;
end;

function TTestFilesBorlandFormatting.GetResultDir: string;
begin
  Result := 'borland';
end;

{ TTestFilesDelforFormatting }

function TTestFilesDelforFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(GetConfigDirBS + 'FormatterSettings-DelForEx.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesDelforFormatting.GetResultDir: string;
begin
  Result := 'delforex';
end;

{ TTestFilesDefaultFormatting }

function TTestFilesDefaultFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesDefaultFormatting.GetResultDir: string;
begin
  Result := 'default'
end;

{ TTestFilesTwmFormatting }

function TTestFilesTwmFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
var
  Settings: TCodeFormatterSettings;
begin
  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(GetConfigDirBS + 'FormatterSettings-twm.ini', Settings);
    Result := Settings.Settings;
  finally
    Settings.Free;
  end;
end;

function TTestFilesTwmFormatting.GetResultDir: string;
begin
  Result := 'twm';
end;

{ TTestFilesSpecial }

function TTestFilesSpecial.GetFormatSettings: TCodeFormatterEngineSettings;
begin
  Result := inherited GetFormatSettings;
  Result.ExceptSingle := True;
end;

function TTestFilesSpecial.GetResultDir: string;
begin
  Result := 'special';
end;

initialization
  RegisterTest(TTestFilesDefaultFormatting.Suite);
  RegisterTest(TTestFilesHeadworkFormatting.Suite);
  RegisterTest(TTestFilesBorlandFormatting.Suite);
  RegisterTest(TTestFilesDelforFormatting.Suite);
  RegisterTest(TTestFilesTwmFormatting.Suite);
  RegisterTest(TTestFilesSpecial.Suite);
end.
