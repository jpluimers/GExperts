// Simple types used in the code formatter
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
//                      Jens Borrisholt (Jens@borrisholt.dk) - Cleaning up the code, and making it aware of several language features

unit GX_CodeFormatterTypes;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils;

type
  ECodeFormatter = class(Exception);

type
  TWordType = (wtLineFeed, wtSpaces, wtHalfComment, wtHalfStarComment,
    wtHalfOutComment, wtFullComment, wtFullOutComment, wtString, wtErrorString,
    wtOperator, wtWord, wtNumber, wtHexNumber, wtNothing, wtAsm, wtCompDirective);

  EFormatException = class(Exception);

const
  Maxline = 1024; // the maximum line length of the Delphi editor
  NotPrintable = [#1..#8, #10..#14, #16..#19, #22..#31]; { not printable chars }
  Tab = #9;
  Space = ' ';
  MaxCollectionSize = Maxint div (SizeOf(Integer) * 2);

type
  {: determines a word's casing:
     * rfLowerCase = all lowercase
     * rfUpperCase = all uppercase
     * rfFirstUp = first character upper case, all other lowercase
     * rfUnchanged = do not change anything
     * rfFirstOccurrence = the first occurrence of the word
  }
  TCase = (rfLowerCase, rfUpperCase, rfFirstUp, rfUnchanged, rfFirstOccurrence);

  TSpace = (spBefore, spAfter);
  TSpaceSet = set of TSpace;

function SpaceSetToInt(_SpaceSet: TSpaceSet): Integer;
function IntToSpaceSet(_Value: Integer): TSpaceSet;

const
  spBoth = [spBefore, spAfter];
  spNone = [];

type
  TReservedType = (rtNothing, rtReserved, rtOper, rtDirective,
    rtIf, rtDo, rtWhile, rtOn, rtVar, rtType, rtProcedure, rtAsm, rtTry,
    rtExcept,
    rtEnd, rtBegin, rtCase, rtOf, rtLineFeed, rtColon, rtSemiColon,
    rtThen, rtClass, rtClassDecl, rtProgram, rtRepeat, rtUntil, rtRecord,
    rtVisibility, rtElse, rtIfElse, rtInterface, rtImplementation,
    rtLeftBr, rtRightBr, rtLeftHook, rtRightHook, rtMathOper, rtAssignOper,
    rtMinus, rtPlus,
    rtLogOper, rtEquals, rtForward, rtDefault, rtInitialization, rtComma,
    rtUses, rtProcDeclare, rtFuncDirective, rtAbsolute, rtComment, rtRecCase, rtDot,
    rtCompIf, rtDotDot,
    rtCompElse, rtCompEndif);

const
  NoReservedTypes = [rtNothing, rtComma, rtColon, rtLineFeed, rtDefault,
    rtFuncDirective, rtAbsolute, rtComment, rtLeftBr, rtRightBr, rtForward,
    rtCompIf, rtCompElse, rtCompEndif, rtVisibility];

  StandardDirectives = [rtDefault, rtAbsolute, rtVisibility, rtFuncDirective,
    rtAbsolute, rtForward];

type
  {: stores all known reserved words in lower case with their associated type,
     must be ordered perfectly on words!!
     NOTE: This is for Delphi 2005, there are some words that aren't reserved
           in earlier Delphi versions, maybe that should be configurable?
           That could be done by converting this list into a oObjects.TStrCollection
           which only contains those words that are known for the configured Delphi
           version. }
  TReservedWordList = class
  private
    FWords: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function FindWord(const _s: string; out _ReservedType: TReservedType): Boolean;
    procedure Add(const _s: string; _ReservedType: TReservedType);
  end;

var
  ReservedWordList: TReservedWordList = nil;

type
  {: Holds a special "option" for a tpascal token, some of them is just override a setting or behavior }
  TTokenOption = (toFeedNewLine);
  TTokenOptions = set of TTokenOption;

{: changes the string case as specified in aCase
   @param aStr is the input string
   @param aCase is a TCase specifying the desired case
   @returns the modified string }
function AdjustCase(const _str: TGXUnicodeString; _Case: TCase): TGXUnicodeString;

implementation

uses
  GX_CodeFormatterUnicode;

function AdjustCase(const _str: TGXUnicodeString; _Case: TCase): TGXUnicodeString;
var
  i: Integer;
begin
  case _Case of
    rfUpperCase:
      Result := UpperCase(_str);
    rfLowerCase:
      Result := LowerCase(_str);
    rfFirstUp: begin
        Result := LowerCase(_str);
        i := 1;
        while (Result[i] = Space) or (Result[i] = Tab) do
          Inc(i);
        Result[i] := UpCase(Result[i]);
      end;
  else
    // rfUnchanged and rfFirstOccurrence
    Result := _str;
  end;
end;

{ TKeywordColl }

function SpaceSetToInt(_SpaceSet: TSpaceSet): Integer;
begin
  Result := 0;
  Move(_SpaceSet, Result, SizeOf(_SpaceSet));
end;

function IntToSpaceSet(_Value: Integer): TSpaceSet;
begin
  Result := [];
  Move(_Value, Result, SizeOf(Result));
end;

{ TReservedWordList }

constructor TReservedWordList.Create;
begin
  inherited Create;
  FWords := TStringList.Create;
  FWords.Sorted := True;
  FWords.Duplicates := dupError;

  Add('absolute', rtAbsolute);
  Add('abstract', rtFuncDirective);
  Add('and', rtOper);
  Add('array', rtReserved);
  Add('as', rtOper);
  Add('asm', rtAsm);
  Add('assembler', rtFuncDirective);
  Add('automated', rtVisibility);
  Add('begin', rtBegin);
  Add('case', rtCase);
  Add('cdecl', rtFuncDirective);
  Add('class', rtClass);
  Add('const', rtVar);
  Add('constructor', rtProcedure);
  Add('contains', rtUses);
  Add('default', rtDefault);
  Add('deprecated', rtFuncDirective);
  Add('destructor', rtProcedure);
  Add('dispid', rtFuncDirective);
  Add('dispinterface', rtInterface);
  Add('div', rtOper);
  Add('do', rtDo);
  Add('downto', rtOper);
  Add('dynamic', rtFuncDirective);
  Add('else', rtElse);
  Add('end', rtEnd);
  Add('except', rtExcept);
  Add('export', rtFuncDirective);
  Add('exports', rtUses);
  Add('external', rtForward);
  Add('far', rtFuncDirective);
  Add('file', rtReserved);
  Add('final', rtDirective);
  Add('finalization', rtInitialization);
  Add('finally', rtExcept);
  Add('for', rtWhile);
  Add('forward', rtForward);
  Add('function', rtProcedure);
  Add('goto', rtReserved);
  Add('helper', rtReserved);
  Add('if', rtIf);
  Add('implementation', rtImplementation);
  Add('implements', rtFuncDirective);
  Add('in', rtOper);
  Add('index', rtFuncDirective);
  Add('inherited', rtReserved);
  Add('initialization', rtInitialization);
  Add('inline', rtFuncDirective);
  Add('interface', rtInterface);
  Add('is', rtOper);
  Add('label', rtVar);
  Add('library', rtFuncDirective);
  Add('message', rtFuncDirective);
  Add('mod', rtOper);
  Add('name', rtFuncDirective);
  Add('near', rtFuncDirective);
  Add('nil', rtReserved);
  Add('nodefault', rtFuncDirective);
  Add('not', rtOper);
  Add('object', rtClass);
  Add('of', rtOf);
  Add('on', rtOn);
  Add('operator', rtProcedure);
  Add('or', rtOper);
  Add('out', rtReserved);
  Add('overload', rtFuncDirective);
  Add('override', rtFuncDirective);
  Add('packed', rtReserved);
  Add('pascal', rtFuncDirective);
  Add('platform', rtFuncDirective);
  Add('private', rtVisibility);
  Add('procedure', rtProcedure);
  Add('program', rtProgram);
  Add('property', rtProcedure);
  Add('protected', rtVisibility);
  Add('public', rtVisibility);
  Add('published', rtVisibility);
  Add('raise', rtReserved);
  Add('read', rtFuncDirective);
  Add('readonly', rtFuncDirective);
  Add('record', rtRecord);
  Add('reference', rtReserved);
  Add('register', rtFuncDirective);
  Add('reintroduce', rtFuncDirective);
  Add('repeat', rtRepeat);
  Add('requires', rtUses);
  Add('resident', rtFuncDirective);
  Add('resourcestring', rtVar);
  Add('safecall', rtFuncDirective);
  Add('set', rtReserved);
  Add('shl', rtOper);
  Add('shr', rtOper);
  Add('static', rtFuncDirective);
  Add('sealed', rtDirective);
  Add('stdcall', rtFuncDirective);
  Add('stored', rtFuncDirective);
  Add('strict', rtVisibility);
  Add('string', rtReserved);
  Add('then', rtThen);
  Add('threadvar', rtVar);
  Add('to', rtOper);
  Add('try', rtTry);
  Add('type', rtType);
  Add('unit', rtProgram);
  Add('until', rtUntil);
  Add('uses', rtUses);
  Add('var', rtVar);
  Add('virtual', rtFuncDirective);
  Add('while', rtWhile);
  Add('with', rtWhile);
  Add('write', rtFuncDirective);
  Add('writeonly', rtFuncDirective);
  Add('xor', rtOper);
end;

destructor TReservedWordList.Destroy;
begin
  FreeAndNil(FWords);
  inherited;
end;

procedure TReservedWordList.Add(const _s: string; _ReservedType: TReservedType);
begin
  FWords.AddObject(LowerCase(_s), Pointer(Ord(_ReservedType)));
end;

function TReservedWordList.FindWord(const _s: string; out _ReservedType: TReservedType): Boolean;
var
  Idx: Integer;
begin
  Result := FWords.Find(LowerCase(_s), Idx);
  if Result then
    _ReservedType := TReservedType(GXNativeInt(FWords.Objects[Idx]));
end;

initialization
  ReservedWordList := TReservedWordList.Create;
finalization
  FreeAndNil(ReservedWordList);
end.

