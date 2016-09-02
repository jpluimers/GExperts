unit DelForExTestOldNew;

{$OPTIMIZATION off}

interface

uses
  Windows,
  Classes,
  SysUtils,
  TestFrameWork,
  GX_CodeFormatterTypes,
  GX_CodeFormatterSettings,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterEngine,
  GX_GenericUtils;

type
  {Tests that compare the output of the original dll with the output of the current compile.}
  TTestCompareOldNew = class(TTestCase)
  private
    FLastActual: string;
    FSettings: TCodeFormatterEngineSettings;
    FNewFormatter: TCodeFormatterEngine;
    procedure CompareOldNew(const _Input, _Expected: TGxUnicodeString; const _Description: string); virtual;
    procedure TestFile(const _Filename: string);
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testJustOpeningComment;
    procedure testJustOpeningStarCommentInAsm;
    procedure TabBeforeEndInAsm;
    procedure testEmptyProgram;
    procedure testEmptyUnit;
    procedure testPreview; virtual;
    procedure testProblems; virtual;
    procedure testProblemRus;
    procedure testCompilerDirectives;
    procedure testDelforEngine;
    procedure testDelforTypes;
    procedure testDelForStack;
    procedure testoObjects;
    procedure testDelforexPackage9;
    procedure testDelforExpert;
    procedure testDelforBookmarks;
    procedure testDelforDefaultSettings;
    procedure testElseAtEnd;
    procedure testStrictVisibility; virtual;
    procedure testLargeUnit1; virtual;
    procedure testIndentComment; virtual;
    procedure testUnterminatedString; virtual;
    procedure testStringWithSingleQuotes;
    procedure testEmptyStringAssignment;
    procedure testHashCharStrings;
    // this crashes the original DLL (fixed in the new one)
//    procedure testLargeUnit2;
  end;

//type
//  TTestOldNewHeadworkFormatting = class(TTestCompareOldNew)
//  protected
//    function GetFormatSettings: TSettings; override;
//  published
//    procedure testPreview; override;
//    procedure testLargeUnit1; override;
//    procedure testIndentComment; override;
//    procedure testDelforexPackage9;
//  end;

type
  TTestOldNewBorlandFormatting = class(TTestCompareOldNew)
  protected
    function GetFormatSettings: TCodeFormatterEngineSettings; override;
  published
  end;

//type
//  TTestOldNewDelforFormatting = class(TTestCompareOldNew)
//  protected
//    function GetFormatSettings: TSettings; override;
//  published
//    procedure testLargeUnit1; override;
//    procedure testIndentComment; override;
//    procedure testDelforexPackage9;
//  end;

//type
//  TTestAlign = class(TTestOldNewDelforFormatting)
//  protected
//    function GetFormatSettings: TSettings; override;
//  published
//    procedure testPreview; override;
//  end;

//type
//  TTestCaptializationFile = class(TTestCompareOldNew)
//  private
//    {: This list can have one of 3 states:
//       1. it is empty -> the results are expected to be the same
//       2. it is not empty -> the results are expected to differ at the lines stored in the list
//       3. it is NIL -> the files are expected to differ }
//    FExpectedCapDifferenceLines: TList;
//  protected
//    function GetFormatSettings: TSettings; override;
//    procedure CheckCaptialization(const _Description: string); override;
//    procedure SetUp; override;
//    procedure TearDown; override;
//  published
//    procedure testProblems; override;
//    procedure testStrictVisibility; override;
//    procedure testLargeUnit1; override;
//    procedure testIndentComment; override;
//    procedure testDelforexPackage9;
//  end;

implementation

uses
  Dialogs;

procedure TTestCompareOldNew.SetUp;
begin
  FSettings := GetFormatSettings;
  FNewFormatter := TCodeFormatterEngine.Create;
  FNewFormatter.Settings.Settings := FSettings;
end;

procedure TTestCompareOldNew.TearDown;
begin
  FNewFormatter.Free;
end;

procedure TTestCompareOldNew.CompareOldNew(const _Input, _Expected: TGxUnicodeString; const _Description: string);
var
  st: TGxUnicodeStringList;
begin
  st := TGxUnicodeStringList.Create;
  try
    st.Text := _Input;
    Check(FNewFormatter.Execute(st), 'Error in formatter engine');
    FLastActual := st.Text;
    CheckEquals(_Expected, FLastActual, _Description + ' had unexpected differences')
  finally
    st.Free;
  end;
end;

procedure TTestCompareOldNew.testJustOpeningComment;
begin
  TestFile('OpeningCommentOnly');
end;

procedure TTestCompareOldNew.testElseAtEnd;
begin
  TestFile('ElseAtEnd');
end;

procedure TTestCompareOldNew.testJustOpeningStarCommentInAsm;
begin
  // I actually thought this would crash...
  TestFile('OpeningStarCommentInAsm');
end;

procedure TTestCompareOldNew.TabBeforeEndInAsm;
begin
  TestFile('TabBeforeEndInAsm');
end;

procedure TTestCompareOldNew.testEmptyProgram;
begin
  TestFile('EmptyProgram');
end;

procedure TTestCompareOldNew.testEmptyUnit;
begin
  TestFile('EmptyUnit');
end;

procedure TTestCompareOldNew.testIndentComment;
begin
  TestFile('IndentComment');
end;

procedure TTestCompareOldNew.testUnterminatedString;
begin
  TestFile('UnterminatedString');
end;

procedure TTestCompareOldNew.testStringWithSingleQuotes;
begin
  // note this actually contains a string with the TEXT #13#10:
  // >hello ' #13#10);<
  TestFile('StringWithSingleQuote');
end;

procedure TTestCompareOldNew.testEmptyStringAssignment;
begin
  TestFile('EmptyStringAssignment');
end;

procedure TTestCompareOldNew.testHashCharStrings;
begin
  TestFile('HashCharStrings');
end;

procedure TTestCompareOldNew.TestFile(const _Filename: string);
var
  InputStr: TStringList;
  ExpectedStr: TStringList;
  s: string;
begin
  InputStr := TStringList.Create;
  ExpectedStr := TStringList.Create;
  try
    s := 'unittests\testcases\input\testfile_' + _Filename + '.pas';
    InputStr.LoadFromFile(s);
    s := 'unittests\testcases\expected-' + ClassName + '\testfile_' + _Filename + '.pas';
    ExpectedStr.LoadFromFile(s);
    try
      CompareOldNew(InputStr.Text, ExpectedStr.Text, _Filename);
    except
      InputStr.Text := FLastActual;
      s := 'unittests\testcases\expected-' + ClassName + '\testfile_' + _Filename + '.out';
      InputStr.SaveToFile(s);
      raise;
    end;
  finally
    ExpectedStr.Free;
    InputStr.Free;
  end;
end;

procedure TTestCompareOldNew.testPreview;
begin
  TestFile('preview');
end;

procedure TTestCompareOldNew.testProblems;
begin
  TestFile('problems');
end;

procedure TTestCompareOldNew.testProblemRus;
begin
  TestFile('problemrus');
end;

procedure TTestCompareOldNew.testCompilerDirectives;
begin
  // the new engine also recognises '(*$' as directives
  // and since all configurations uppercase them, there are two differences
  TestFile('compilerdirectives');
end;

procedure TTestCompareOldNew.testStrictVisibility;
begin
  // the new dll does not indent "strict private" / "strict protected"
  TestFile('strictvisibility');
end;

procedure TTestCompareOldNew.testLargeUnit1;
begin
//  TestFile('unittests\testcases\input\testunit_xdom_3_1.pas');
end;

// this test crashes the old dll (access violation), the new one doesn't
//procedure TTestCompareOldNew.testLargeUnit2;
//begin
//  TestFile('unittests\testcases\input\testunit_VirtualTrees.pas');
//end;

procedure TTestCompareOldNew.testDelforEngine;
begin
  TestFile('GX_CodeFormatterEngine');
end;

procedure TTestCompareOldNew.testDelforTypes;
begin
  TestFile('GX_CodeFormatterTypes');
end;

procedure TTestCompareOldNew.testDelForStack;
begin
  TestFile('GX_CodeFormatterStack');
end;

procedure TTestCompareOldNew.testoObjects;
begin
  TestFile('GX_CollectionLikeLists');
end;

procedure TTestCompareOldNew.testDelforexPackage9;
begin
  TestFile('DelforexPackage9');
end;

procedure TTestCompareOldNew.testDelforExpert;
begin
  TestFile('DeforExpert');
end;

procedure TTestCompareOldNew.testDelforBookmarks;
begin
  TestFile('GX_CodeFormatterBookmarks');
end;

procedure TTestCompareOldNew.testDelforDefaultSettings;
begin
  TestFile('GX_CodeFormatterDefaultSettings');
end;

{ TTestHeadworkFormatting }

//function TTestOldNewHeadworkFormatting.GetFormatSettings: TSettings;
//begin
//  Result := HeadworkDefaults;
//end;
//
//procedure TTestOldNewHeadworkFormatting.testDelforexPackage9;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;
//
//procedure TTestOldNewHeadworkFormatting.testIndentComment;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;
//
//procedure TTestOldNewHeadworkFormatting.testLargeUnit1;
//begin
//  FExpectedDifferenceLines.Add(pointer(3887));
//  inherited;
//end;
//
//procedure TTestOldNewHeadworkFormatting.testPreview;
//begin
//  // headwork formatting is the only one that actually changes the preview file
//  TestFile('packagewiz\preview.pas');
//end;

{ TTestBorlandFormatting }

function TTestOldNewBorlandFormatting.GetFormatSettings: TCodeFormatterEngineSettings;
begin
  Result := BorlandDefaults;
end;

{ TTestOldNewDelforFormatting }

//function TTestOldNewDelforFormatting.GetFormatSettings: TSettings;
//begin
//  Result := DelforDefaults;
//end;
//
//procedure TTestOldNewDelforFormatting.testDelforexPackage9;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;
//
//procedure TTestOldNewDelforFormatting.testIndentComment;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;
//
//procedure TTestOldNewDelforFormatting.testLargeUnit1;
//begin
//  FExpectedDifferenceLines.Add(pointer(3883));
//  inherited;
//end;

{ TTestAlign }

//function TTestAlign.GetFormatSettings: TSettings;
//begin
//  Result := inherited GetFormatSettings;
//  Result.Parser.AlignComments := True;
//  Result.Parser.AlignVar := True;
//end;
//
//procedure TTestAlign.testPreview;
//begin
//  FExpectDifferentFromInput := True;
//  TestFile('packagewiz\preview.pas');
//end;

{ TTestCaptializationFile }

//procedure TTestCaptializationFile.CheckCaptialization(const _Description: string);
//var
//  OldCap: TStringList;
//  NewCap: TStringList;
//  OldCapFile: string;
//  NewCapFile: string;
//  NameOnly: string;
//begin
//  NameOnly := ExtractFileName(_Description);
//  OldCapFile := NameOnly + '-captialization-failed-' + ClassName + 'OldFormatter';
//  NewCapFile := NameOnly + '-captialization-failed-' + ClassName + 'NewFormatter';
//
//  FOrigFormatter.SaveCapFile(OldCapFile);
//  FNewFormatter.SaveCapFile(NewCapFile);
//
//  OldCap := TStringList.Create;
//  NewCap := TStringList.Create;
//  try
//    OldCap.LoadFromFile(OldCapFile);
//    NewCap.LoadFromFile(NewCapFile);
//    CompareLines(OldCap, NewCap, FExpectedCapDifferenceLines, 'captitalization file');
//    DeleteFile(OldCapFile);
//    DeleteFile(NewCapFile);
//  finally
//    NewCap.Free;
//    OldCap.Free;
//  end;
//  inherited;
//end;
//
//function TTestCaptializationFile.GetFormatSettings: TSettings;
//begin
//  Result := DelforDefaults;
//  Result.Parser.FillNewWords := fmAddNewWord;
//end;
//
//procedure TTestCaptializationFile.SetUp;
//begin
//  inherited;
//  FExpectedCapDifferenceLines := TList.Create;
//end;
//
//procedure TTestCaptializationFile.TearDown;
//begin
//  FExpectedCapDifferenceLines.Free;
//  inherited;
//end;
//
//procedure TTestCaptializationFile.testStrictVisibility;
//begin
//  FExpectedDifferenceLines.Add(pointer(6));
//  FExpectedDifferenceLines.Add(pointer(7));
//  FExpectedDifferenceLines.Add(pointer(8));
//  FExpectedDifferenceLines.Add(pointer(9));
//  FExpectedCapDifferenceLines.Add(pointer(1));
//  TestFile('unittests\testcases\input\testunit_strictvisibility.pas');
//end;
//
//procedure TTestCaptializationFile.testLargeUnit1;
//begin
//  FExpectedDifferenceLines.Add(pointer(3883));
//  FExpectedCapDifferenceLines.Add(pointer(1661));
//  inherited;
//end;
//
//procedure TTestCaptializationFile.testProblems;
//begin
//  FExpectedCapDifferenceLines.Add(pointer(22));
//  FExpectedCapDifferenceLines.Add(pointer(62));
//  inherited;
//end;
//
//procedure TTestCaptializationFile.testIndentComment;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;
//
//procedure TTestCaptializationFile.testDelforexPackage9;
//begin
//  FExpectDifferentFromInput := False;
//  inherited;
//end;

initialization
//  RegisterTest('OldNew', TTestOldNewHeadworkFormatting.Suite);
//  RegisterTest('OldNew', TTestOldNewBorlandFormatting.Suite);
//  RegisterTest('OldNew', TTestOldNewDelforFormatting.Suite);
//  RegisterTest('OldNew', TTestAlign.Suite);
//  RegisterTest('OldNew', TTestCaptializationFile.Suite);

//  RegisterTest('timing', TTestTimingHeadworkFormatting.Suite);
//  RegisterTest('timing', TTestTimingBorlandFormatting.Suite);
//  RegisterTest('timing', TTestTimingDelforFormatting.Suite);
end.
