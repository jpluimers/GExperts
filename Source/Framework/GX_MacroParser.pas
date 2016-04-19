// This unit is used to perform string substitution for string variables
// Function arguments, result, and username substitution by Ulrich Schulte and EB
// Cleanup and objects by Piotr Likus

unit GX_MacroParser;

interface

// This unit is NOT fully compatible with C++Builder
// We only have a Pascal parser so pass in DoParseSource = False if
// the current file is not Object Pascal source code

{$I GX_CondDefine.inc}

const
  InputVarName = 'INPUTVAR'; // name of input variable macro (do not localize)

type
  TBasicMacroReplacer = class
    function ReplaceStrings(const RawString: string; DoParseSource: Boolean): string; virtual; abstract;
  end;

  TBasicMacroLibrary = class
  protected
    function GetUnknownNameResult: string;
  public
    function ParseMacro(AMacroText: string; var VResult: string): Boolean; virtual; abstract;
  end;

function ReplaceStrings(const RawString: string; DoParseSource: Boolean): string;

procedure RegisterMacroLibrary(ALibrary: TBasicMacroLibrary);
procedure UnregisterMacroLibrary(ALibrary: TBasicMacroLibrary);
procedure RegisterMacroReplacer(AReplacer: TBasicMacroReplacer);
procedure UnregisterMacroReplacer(AReplacer: TBasicMacroReplacer);

implementation

uses
  Classes, SysUtils, Dialogs, Windows, Clipbrd,
  mPasLex, GX_GenericUtils, GX_OtaUtils, GX_EditReader;

resourcestring
  SUnknownNameResult = 'Not available';
  SNoneResult = 'None';

var
  InternalMacroLibList: TList = nil;
  InternalReplacer: TBasicMacroReplacer = nil;

type
  TMacroToken = (matMethodClass, matMethodProc, matClass, matUnit, matIdent, matInterface);

  TSourceScope = (ssBefore, ssAfter, ssEntire);

  { TBasicSourceParser }
  TBasicSourceParser = class
  private
    FLexer: TmwPasLex;
    FFileContent: string;
    FScope: TSourceScope;
  protected
    function GetFileName: string; virtual;
    function GetToken: string;
    function GetTokenID: TTokenKind;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitSource(AScope: TSourceScope); virtual;
    // Lexer methods
    procedure Next;
    procedure NextNoJunk;
    // Lexer properties
    property Token: string read GetToken;
    property TokenID: TTokenKind read GetTokenID;
    property Scope: TSourceScope read FScope;
  end;

  { TMacroSourceParser }
  TMacroSourceParser = class(TBasicSourceParser)
  private
    procedure GetClassMethodDetailsFromCurrentPosEx(var NestedClasses,
      ClassName, MethodName, Args, ResultType: string; ArgList: TLexArgList);
  protected
    function GetBeforeMode: Boolean;
    procedure GetClassMethodDetailsFromCurrentPos(var NestedClasses, ClassName, MethodName, Args, ResultType: string);
    procedure GetRelevantClassMethodDetails(var NestedClasses, ClassName, MethodName, Args, ResultType: string);
    function GoToNextMethodDefinition: Boolean;
    function GetDottedIdentifierAtPos: string;
    procedure GetArgList(ArgList: TLexArgList);
  public
    function GetUnitToken: string;
    function GetMethodProcToken(AddClass: Boolean): string;
    function GetClassOfCurrMethodToken: string;
    function GetInterfaceToken: string;
    function GetClassToken: string;
    function GetIdentToken(AFullName: Boolean): string;
    procedure GetNameArgsResult(var Name, Args, ResultType: string);
    property BeforeMode: Boolean read GetBeforeMode;
  end;

  { TMacroReplacer }
  TMacroReplacer = class(TBasicMacroReplacer)
  private
    function AddIndent(const AText: string; const AIndentText: string;
      AIncFirstLine: Boolean): string;
    function CalcLineIndent(const AText: string): Integer;
    function FormatParamList(const ASingleTemplate: string): string;
    procedure GetArgList(ArgList: TLexArgList);
    function FormatParamItem(const ATemplate: string; AItem: TLexArgItem): string;
    function ReformatMultiLineBlock(const AText, AIndentText: string): string;
  protected
    FParser: TMacroSourceParser;
    FDictionary: TStringList;
    FParseSource: Boolean;
    FBeforeMode: Boolean;
    function GetParserToken(AToken: TMacroToken): string;
    function GetReplaceString(StVar: string; const AIndentText: string): string;

    function GetClassName: string;
    function GetInterfaceName: string;
    function GetMethodClassName: string;
    function GetProcName: string;
    function GetUnitName: string;
    function GetUnitDir: string;
    function GetIdentName: string;
    function GetUserNameTag: string;
    function IsInputVarMacro(AMacro: string): Boolean;
    function ParseNewVarMacro(AVarMacro: string; var VVarName, VVarCaption: string): Boolean;
    function ProcessNewVar(AVarMacro: string; var VVarName, VVarValue: string): Boolean;
    procedure GetNameArgsResult(var Name, Args, ResultType: string);
    function SelectScope: TSourceScope;
    procedure SetBeforeMode(const Value: Boolean);
    function ReplaceByLibraries(const AMacroText: string; var VResult: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ReplaceStrings(const RawString: string; DoParseSource: Boolean): string; override;
    property BeforeMode: Boolean read FBeforeMode write SetBeforeMode;
  end;

procedure PrepareMacroLibList;
begin
  if InternalMacroLibList = nil then
    InternalMacroLibList := TList.Create;
end;

procedure UnprepareMacroLibList;
begin
  FreeAndNil(InternalMacroLibList);
end;

procedure RegisterMacroLibrary(ALibrary: TBasicMacroLibrary);
begin
  Assert(Assigned(InternalMacroLibList));
  InternalMacroLibList.Add(ALibrary);
end;

procedure UnregisterMacroLibrary(ALibrary: TBasicMacroLibrary);
begin
  if InternalMacroLibList <> nil then
    InternalMacroLibList.Remove(ALibrary);
end;

procedure RegisterMacroReplacer(AReplacer: TBasicMacroReplacer);
begin
  InternalReplacer := AReplacer;
end;

procedure UnregisterMacroReplacer(AReplacer: TBasicMacroReplacer);
begin
  if InternalReplacer = AReplacer then
    InternalReplacer := nil;
end;

function ReplaceStrings(const RawString: string; DoParseSource: Boolean):
  string;
var
  Replacer: TBasicMacroReplacer;
begin
  if InternalReplacer = nil then
    Replacer := TMacroReplacer.Create
  else
    Replacer := InternalReplacer;

  try
    Result := Replacer.ReplaceStrings(RawString, DoParseSource);
  finally
    if Replacer <> InternalReplacer then
      FreeAndNil(Replacer);
  end;
end;

constructor TBasicSourceParser.Create;
begin
  inherited;
  FLexer := TmwPasLex.Create;
end;

destructor TBasicSourceParser.Destroy;
begin
  inherited;
end;

function TBasicSourceParser.GetFileName: string;
begin
  Result := GxOtaGetCurrentSourceFile;
end;

procedure TBasicSourceParser.InitSource(AScope: TSourceScope);
var
  EditRead: TEditReader;
begin
  FScope := AScope;

  FFileContent := '';

  EditRead := TEditReader.Create(GetFileName);
  try
    if AScope = ssEntire then
      FFileContent := EditRead.GetText
    else if AScope = ssAfter then
      FFileContent := EditRead.GetTextFromPos
    else
      FFileContent := EditRead.GetTextToPos;
  finally
    FreeAndNil(EditRead);
  end;
  FLexer.Origin := @FFileContent[1];
end;

function TBasicSourceParser.GetToken: string;
begin
  Result := FLexer.Token;
end;

function TBasicSourceParser.GetTokenID: TTokenKind;
begin
  Result := FLexer.TokenID;
end;

procedure TBasicSourceParser.NextNoJunk;
begin
  FLexer.NextNoJunk;
end;

procedure TBasicSourceParser.Next;
begin
  FLexer.Next;
end;

function TMacroSourceParser.GetBeforeMode: Boolean;
begin
  Result := (Scope = ssBefore);
end;

// Get unit name from the source code
function TMacroSourceParser.GetUnitToken: string;
begin
  Result := SUnknownNameResult;
  while (TokenID <> tkNull) and (TokenID <> tkUnit) do
    NextNoJunk;
  if TokenID = tkUnit then
  begin
    NextNoJunk;
    Result := GetDottedIdentifierAtPos;
  end;
end;

// Get the method name after the current cursor position
function TMacroSourceParser.GetMethodProcToken(AddClass: Boolean): string;
var
  MethodName: string;
  LastClassName: string;
  FullClassName: string;
  Args: string;
  ResultType: string;
begin
  Result := SUnknownNameResult;
  GetRelevantClassMethodDetails(FullClassName, LastClassName, MethodName, Args, ResultType);
  if (LastClassName <> '') and AddClass then
    Result := LastClassName + '.' + MethodName
  else
    Result := MethodName;
end;

// Get class of method placed next to current cursor pos
function TMacroSourceParser.GetClassOfCurrMethodToken: string;
var
  FullClassName: string;
  LastClassName: string;
  MethodName: string;
  Args: string;
  ResultType: string;
begin
  Result := SNoneResult;
  if GoToNextMethodDefinition then
  begin
    GetClassMethodDetailsFromCurrentPos(FullClassName, LastClassName, MethodName, Args, ResultType);
    if LastClassName <> '' then
      Result := LastClassName;
  end;
end;

// Returns interface name placed inside interface declaration after cursor
function TMacroSourceParser.GetInterfaceToken: string;
var
  IntfName: string;
  IntfFound: Boolean;
begin
  Result := SUnknownNameResult;
  repeat
    IntfFound := False;
    repeat // search for 'identifier = interface'
      if TokenID = tkIdentifier then
      begin
        IntfName := Token;
        NextNoJunk;
        if TokenID <> tkEqual then
          Continue;
        NextNoJunk;
        if TokenID = tkInterface then
          IntfFound := True;
      end
      else
        NextNoJunk;
    until IntfFound or (TokenID = tkNull);
    if IntfFound then
      Result := IntfName;
  until (not BeforeMode) or (TokenID = tkNull);
end;

// Get class name placed next to current cursor pos
// retrieves class from class declaration or method definition header
function TMacroSourceParser.GetClassToken: string;
var
  ClassFound: Boolean;
  FoundClassName: string;
  InClass: Boolean;
begin
  InClass := False;
  Result := '';
  repeat
    ClassFound := False;
    repeat // search for 'identifier = class'
      if TokenID = tkIdentifier then
      begin
        FoundClassName := Token;
        NextNoJunk;
        if TokenID <> tkEqual then
          Continue;
        NextNoJunk;
        if TokenID = tkClass then
        begin
          ClassFound := True;
          InClass := True;
        end;
      end
      else if (TokenID in MethodMarkers) and (not InClass) then
      begin // find "procedure classname.methodname()"
        FoundClassName := GetClassOfCurrMethodToken;
        if (FoundClassName <> SUnknownNameResult) and (FoundClassName <> '') then
          ClassFound := True;
      end
      else
      begin
        if TokenID = tkEnd then
          InClass := False;
        NextNoJunk;
      end;
    until ClassFound or (TokenID = tkNull);
    if ClassFound then
      Result := FoundClassName;
  until (not BeforeMode) or (TokenID = tkNull);
  if Result = '' then
    Result := SUnknownNameResult;
end;

function TMacroSourceParser.GetIdentToken(AFullName: Boolean): string;
begin
  Result := GxOtaGetCurrentIdent;
  if Result = '' then
    Result := SUnknownNameResult;
end;

function TMacroReplacer.SelectScope: TSourceScope;
begin
  if FBeforeMode then
    Result := ssBefore
  else
    Result := ssAfter;
end;

function TMacroReplacer.GetParserToken(AToken: TMacroToken): string;
var
  Scope: TSourceScope;
begin
  Result := SUnknownNameResult;

  if AToken = matUnit then
    Scope := ssEntire
  else
    Scope := SelectScope;

  FParser.InitSource(Scope);
  with FParser do
  case AToken of
    matMethodClass:
      Result := GetMethodProcToken(True);
    matClass:
      Result := GetClassToken;
    matInterface:
      Result := GetInterfaceToken;
    matMethodProc:
      Result := GetMethodProcToken(False);
    matUnit:
      Result := GetUnitToken;
    matIdent:
      Result := GetIdentToken(True);
  else
    raise Exception.Create('Undefined token type in GetParserToken');
  end;
end;

// Class name from declaration or from implementation of method
function TMacroReplacer.GetClassName: string;
begin
  Result := GetParserToken(matClass);
end;

// Interface name from interface declaration
function TMacroReplacer.GetInterfaceName: string;
begin
  Result := GetParserToken(matInterface);
end;

// Class and method name from implementation of method
function TMacroReplacer.GetMethodClassName: string;
begin
  Result := GetParserToken(matMethodClass);
end;

// Procedure/method name from implementation of method
function TMacroReplacer.GetProcName: string;
begin
  Result := GetParserToken(matMethodProc);
end;

// Unit name from header of file
function TMacroReplacer.GetUnitName: string;
begin
  if not FParseSource then
    Result := ExtractPureFileName(GxOtaGetCurrentSourceFile)
  else
    Result := GetParserToken(matUnit);
end;

function TMacroReplacer.GetUnitDir: string;
begin
  Result := ExtractFileDir(GxOtaGetCurrentSourceFile);
end;

// First identifier after the cursor
function TMacroReplacer.GetIdentName: string;
begin
  Result := GetParserToken(matIdent);
end;

procedure TMacroSourceParser.GetNameArgsResult(var Name, Args, ResultType: string);
var
  FullClassName: string;
  LastClassName: string;
begin
  repeat
    if GoToNextMethodDefinition then
    begin
      Name := SUnknownNameResult;
      Args := SNoneResult;
      ResultType := SNoneResult;

      NextNoJunk; // Get the proc/class identifier
      GetClassMethodDetailsFromCurrentPos(FullClassName, LastClassName, Name, Args, ResultType);
      if LastClassName <> '' then
        Name := LastClassName + '.' + Name;
    end;
  until (not BeforeMode) or (TokenID = tkNull);
end;

procedure TMacroSourceParser.GetArgList(ArgList: TLexArgList);
var
  FullClassName: string;
  LastClassName: string;
  Name, Args, ResultType: string;
begin
  repeat
    if GoToNextMethodDefinition then
    begin
      Name := SUnknownNameResult;
      Args := SNoneResult;
      ResultType := SNoneResult;

      NextNoJunk; // Get the proc/class identifier

      ArgList.Clear;
      GetClassMethodDetailsFromCurrentPosEx(FullClassName, LastClassName, Name,
        Args, ResultType, ArgList);

      if LastClassName <> '' then
        Name := LastClassName + '.' + Name;
    end;
  until (not BeforeMode) or (TokenID = tkNull);
end;

const
  // Constants for Input variable macro
  InputVarSep = ',';
  InputVarPrevix = '%' + InputVarName + InputVarSep; // prefix for this macro

// Returns True if macro is an Input variable macro
function TMacroReplacer.IsInputVarMacro(AMacro: string): Boolean;
begin
  Result := StrContains(InputVarPrevix, AMacro);
end;

// Parses an input variable macro
// Returns True if parsing was successful
function TMacroReplacer.ParseNewVarMacro(AVarMacro: string; var VVarName, VVarCaption: string): Boolean;
var
  TokenList: TStringList;
begin
  Result := False;
  TokenList := TStringList.Create;
  try
    AnsiStrTok(AVarMacro, InputVarSep, TokenList);
    if TokenList.Count < 2 then
      Exit
    else
    begin
      VVarName := TokenList[1];
      if TokenList.Count > 2 then
      begin
        VVarCaption := TokenList[2];
        // skip '%' on the end
        VVarCaption := Copy(VVarCaption, 1, Length(VVarCaption) - 1);
      end
      else
      begin
        // skip '%' on the end
        VVarName := Copy(VVarName, 1, Length(VVarName) - 1);
        VVarCaption := VVarName;
      end;
    end;
  finally
    FreeAndNil(TokenList);
  end;
  Result := True;
end;

constructor TMacroReplacer.Create;
begin
  inherited;
  FParser := TMacroSourceParser.Create;
  FDictionary := TStringList.Create;
  FParseSource := False;
  FBeforeMode := False;
end;

destructor TMacroReplacer.Destroy;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FParser);
  inherited;
end;

function TMacroReplacer.ProcessNewVar(AVarMacro: string; var VVarName, VVarValue: string): Boolean;
var
  VarCaption: string;
begin
  Result := False;
  if not ParseNewVarMacro(AVarMacro, VVarName, VarCaption) then
    Exit;

  Result := InputQuery(VarCaption, VarCaption, VVarValue);
end;

procedure TMacroReplacer.GetNameArgsResult(var Name, Args, ResultType: string);
begin
  FParser.InitSource(SelectScope);
  FParser.GetNameArgsResult(Name, Args, ResultType);
end;

procedure TMacroReplacer.GetArgList(ArgList: TLexArgList);
begin
  FParser.InitSource(SelectScope);
  FParser.GetArgList(ArgList);
end;

procedure TMacroReplacer.SetBeforeMode(const Value: Boolean);
begin
  FBeforeMode := Value;
end;

function TMacroReplacer.GetUserNameTag: string;
begin
  Result := GetCurrentUser;
  if Trim(Result)='' then
    Result := SUnknownNameResult;
end;

// Add "indent text" before each line of specified text
// AIncFirstLine - including first line
function TMacroReplacer.AddIndent(const AText: string; const AIndentText: string;
  AIncFirstLine: Boolean): string;
var
  Lines: TStringList;
  IndentText: string;
  Idx, i: Integer;
begin
  if AIndentText = '' then
    Result := AText
  else
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := AText;

      if AIncFirstLine then
        Idx := 0
      else
        Idx := 1;

      IndentText := StringReplace(AIndentText, '|', '', [rfReplaceAll]);

      for i:=Idx to Lines.Count-1 do
        Lines[i] := IndentText + Lines[i];

      Result := Lines.Text;
      if not HasTrailingEOL(AText) then
        RemoveLastEOL(Result);
    finally
      FreeAndNil(Lines);
    end;
  end;
end;

// Prepare multi-line block of text to be inserted into the text editor
//  1. Move all lines to left (more or less)
//  2. Trim the text as a whole
//  3. Add indent text before each line (skipping the first one)
function TMacroReplacer.ReformatMultiLineBlock(const AText: string;
  const AIndentText: string): string;
var
  i: Integer;
  Lines: TStringList;
  MinIndex, MinSpaceCount: Integer;
begin
  Result := AText;

  if AText = '' then
    Result := AText
  else
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := AText;

      GxOtaExpandTabsInList(Lines);
      FindMinIndent(Lines, MinIndex, MinSpaceCount);

      if MinIndex < 0 then
        MinSpaceCount := 0;

      for i := 0 to Lines.Count-1 do
      begin
        if (i <> MinIndex) and (Trim(Lines[i]) <> '') then
          Lines[i] := LeftTrimNChars(Lines[i], [#32], MinSpaceCount);
      end;

      Result := Lines.Text;

      if not HasTrailingEOL(AText) then
        RemoveLastEOL(Result);

      Result := Trim(Result);
    finally
      FreeAndNil(Lines);
    end;
  end;

  Result := AddIndent(Result, AIndentText, False);
end;

function TMacroReplacer.GetReplaceString(StVar: string; const AIndentText: string): string;
var
  ProcName, ProcArgs, ProcResult: string;
  VarName: string;
  OldStVar: string;
  OptionValue : Variant;
begin
  // Do not localize any of the below items.
  Result := '';
  ProcName := SUnknownNameResult;
  ProcArgs := SUnknownNameResult;
  ProcResult := SUnknownNameResult;
  OldStVar := StVar;
  StVar := AnsiUpperCase(StVar);
  // Project macros
  if StVar = '%PROJECTGROUPNAME%' then
  begin
    Result := ExtractFileName(GxOtaGetProjectGroupFileName);
     if Result = '' then
       Result := SUnknownNameResult
     else
       Result := ChangeFileExt(Result, '');
  end;
  if StVar = '%PROJECTGROUPDIR%' then
  begin
    Result := ExtractFilePath(GxOtaGetProjectGroupFileName);
    if Result = '' then
      Result := SUnknownNameResult;
  end;
  if StVar = '%PROJECTDIR%' then
    Result := ExtractFilePath(GxOtaGetCurrentProjectFileName)
  else if StVar = '%PROJECTNAME%' then
    Result := GxOtaGetCurrentProjectName
  // Date & time macros
  else if StVar = '%DATETIME%' then
    Result := DateTimetoStr(Date + Time)
  else if StVar = '%HOUR%' then
    Result := FormatDateTime('hh', Time)
  else if StVar = '%MINUTE%' then
    Result := FormatDateTime('nn', Time)
  else if StVar = '%SECOND%' then
    Result := FormatDateTime('ss', Time)
  else if StVar = '%DATE%' then
    Result := DateToStr(Date)
  else if StVar = '%YEAR%' then
    Result := FormatDateTime('yyyy', Date)
  else if StVar = '%MONTH%' then
    Result := FormatDateTime('mm', Date)
  else if StVar = '%MONTHSHORTNAME%' then
    Result := FormatDateTime('mmm', Date)
  else if StVar = '%MONTHLONGNAME%' then
    Result := FormatDateTime('mmmm', Date)
  else if StVar = '%DAY%' then
    Result := FormatDateTime('dd', Date)
  else if StVar = '%DAYSHORTNAME%' then
    Result := FormatDateTime('ddd', Date)
  else if StVar = '%DAYLONGNAME%' then
    Result := FormatDateTime('dddd', Date)
  // Basic token macros
  else if StVar = '%UNIT%' then
    Result := GetUnitName
  else if StVar = '%UNITDIR%' then
    Result := GetUnitDir
  else if StVar = '%USER%' then
    Result := GetUserNameTag
  else if StVar = '%BEFORE%' then
  begin
    BeforeMode := not BeforeMode;
    Result := '';
  end
  else if (StVar = '%CLIPBOARD%') or (StVar = '%CLIPBOARD1%') then
  begin
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      Result := Clipboard.AsText;
    end
    else
      Result := '';

    if StVar = '%CLIPBOARD%' then
      Result := ReformatMultiLineBlock(Result, AIndentText);
  end
  else if (StVar = '%SELECTION%') or (StVar = '%COPYSELECTION%') then
  begin
    Result := GxOtaGetCurrentSelection(False);

    if StVar = '%SELECTION%' then
      Result := ReformatMultiLineBlock(Result, AIndentText);
  end
  else if StVar = '%PROCNAME%' then
  begin
    if FParseSource then
      Result := GetProcName
    else
      Result := SUnknownNameResult;
  end
  else if StVar = '%RESULT%' then
  begin
    if FParseSource then
    begin
      GetNameArgsResult(ProcName, ProcArgs, ProcResult);
      Result := ProcResult;
    end
    else
      Result := SUnknownNameResult;
  end
  else if StVar = '%ARGUMENTS%' then
  begin
    if FParseSource then
    begin
      GetNameArgsResult(ProcName, ProcArgs, ProcResult);
      Result := ProcArgs;
    end
    else
      Result := SUnknownNameResult;
  end
  // Version macros
  else if StVar = '%VERPRODUCTVERSION%' then
  begin
    if GxOtaGetVersionInfoKeysOptions(ProductVersionOptionNames, OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERFILEVERSION%' then
  begin
    if GxOtaGetVersionInfoKeysOptions(FileVersionOptionNames, OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERMAJOR%' then
  begin
    if GxOtaGetActiveProjectOption('MajorVersion', OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERMINOR%' then
  begin
    if GxOtaGetActiveProjectOption('MinorVersion', OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERRELEASE%' then
  begin
    if GxOtaGetActiveProjectOption('Release', OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERBUILD%' then
  begin
    if GxOtaGetActiveProjectOption('Build', OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERPRODUCTNAME%' then
  begin
    if GxOtaGetVersionInfoKeysOptions(ProductNameOptionNames, OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERINTERNALNAME%' then
  begin
    if GxOtaGetVersionInfoKeysOptions(InternalNameOptionNames, OptionValue) then
      Result := OptionValue;
  end
  else if StVar = '%VERFILEDESCRIPTION%' then
  begin
    if GxOtaGetVersionInfoKeysOptions(FileDescriptionOptionNames, OptionValue) then
      Result := OptionValue;
  end
  // More token macros
  else if StVar = '%METHODCLASS%' then
  begin
    if FParseSource then
      Result := GetMethodClassName
    else
      Result := SUnknownNameResult;
  end
  else if StVar = '%CLASS%' then
  begin
    if FParseSource then
      Result := GetClassName
    else
      Result := SUnknownNameResult;
  end
  else if StVar = '%INTERFACE%' then
  begin
    if FParseSource then
      Result := GetInterfaceName
    else
      Result := SUnknownNameResult;
  end
  else if StVar = '%IDENT%' then
  begin
    if FParseSource then
      Result := GetIdentName
    else
      Result := SUnknownNameResult;
  end
  else if ReplaceByLibraries(OldStVar, Result) then
    Result := Result // Just to complete the if-else statement
  else
    // If there is input string macro, format: %INPUTVAR,VarName,<caption>%
    // it can can be used later using %VarName%
    if IsInputVarMacro(StVar) then
    begin
      if not ProcessNewVar(OldStVar, VarName, Result) then
        Result := SUnknownNameResult
      else
        FDictionary.Add('%' + VarName + '%' + '=' + Result);
    end
    else if FDictionary.IndexOfName(StVar) >= 0 then
    begin
      Result := FDictionary.Values[StVar];
    end;
end;

// Returns True if the macro has been replaced
function TMacroReplacer.ReplaceByLibraries(const AMacroText: string; var VResult: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  if InternalMacroLibList <> nil then
  for i := 0 to InternalMacroLibList.Count-1 do
    if TBasicMacroLibrary(InternalMacroLibList[i]).ParseMacro(AMacroText, VResult) then
    begin
      Result := True;
      Break;
    end;
end;

function TMacroReplacer.ReplaceStrings(const RawString: string; DoParseSource: Boolean): string;
resourcestring
  SReplaceError = 'Error!';
var
  i: Integer;
  j: Integer;
  IndentSize: Integer;
  IndentText: string;
  NewToken: string;
  ParamListStart: Integer;
  MacroName: string;
begin
  FParseSource := DoParseSource;
  ParamListStart := 0;
  Result := '';
  i := 1;
  while i <= Length(RawString) do
  begin
    if RawString[i] <> '%' then
      Result := Result + RawString[i]
    else
    begin
      if i < Length(RawString) then
        if RawString[i + 1] = '%' then
        begin
          Result := Result + '%';
          Inc(i, 2);
          Continue;
        end;
      j := i + 1;
      while (j <= Length(RawString)) and (RawString[j] <> '%') do
        Inc(j);

      IndentSize := CalcLineIndent(Result);
      IndentText := Copy(Result, Length(Result)-IndentSize+1, IndentSize);

      if j <= Length(RawString) then
      begin
        MacroName := Copy(RawString, i, j - i + 1);
        NewToken := '';

        if AnsiUpperCase(MacroName) = '%BEGINPARAMLIST%' then
          ParamListStart := Length(Result) + 1
        else if (AnsiUpperCase(MacroName) = '%PARAMNAME%') or
                (AnsiUpperCase(MacroName) = '%PARAMTYPE%') or
                (AnsiUpperCase(MacroName) = '%PARAMDEF%')
        then
          NewToken := MacroName
        else if AnsiUpperCase(MacroName) = '%ENDPARAMLIST%' then
        begin
          if ParamListStart < 1 then
            NewToken := SReplaceError
          else
          begin
            NewToken :=
              Copy(Result, ParamListStart, Length(Result) - ParamListStart + 1);
            NewToken :=
              FormatParamList(NewToken);

            SetLength(Result, ParamListStart - 1);
          end;
        end
        else
          NewToken := GetReplaceString(MacroName, IndentText);
      end
      else
        NewToken := SReplaceError;

      Result := Result + NewToken;
      i := j;
    end;
    Inc(i);
  end;
end;

// Calculate length of last line in the string
function TMacroReplacer.CalcLineIndent(const AText: string): Integer;
var
  Idx: Integer;
begin
  Idx := Length(AText);
  Result := 0;

  while Idx >= 1 do
  begin
    if IsCharLineEnding(AText[Idx]) then
      Break;
    Inc(Result);
    Dec(Idx);
  end;
end;

function TMacroReplacer.FormatParamList(const ASingleTemplate: string): string;
var
  ArgList: TLexArgList;
  i: Integer;
begin
  ArgList := TLexArgList.Create;
  try
    GetArgList(ArgList);

    Result := '';

    for i:=0 to ArgList.Count-1 do
      Result := Result + FormatParamItem(ASingleTemplate, ArgList[i]);
  finally
    FreeAndNil(ArgList);
  end;
end;

function TMacroReplacer.FormatParamItem(const ATemplate: string;
  AItem: TLexArgItem): string;
begin
  Result := ATemplate;
  Result := StringReplace(Result, '%PARAMNAME%', AItem.CoreName,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%PARAMTYPE%', AItem.ArgType,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%PARAMDEF%', AItem.DefaultValue,
    [rfReplaceAll, rfIgnoreCase]);
end;

function TBasicMacroLibrary.GetUnknownNameResult: string;
begin
  Result := SUnknownNameResult;
end;

procedure TMacroSourceParser.GetClassMethodDetailsFromCurrentPos(
  var NestedClasses, ClassName, MethodName, Args, ResultType: string);
begin
  FLexer.GetClassMethodDetailsFromCurrentPos(NestedClasses, ClassName, MethodName, Args, ResultType);
  Args := CompressWhiteSpace(Args);
end;

procedure TMacroSourceParser.GetClassMethodDetailsFromCurrentPosEx(
  var NestedClasses, ClassName, MethodName, Args, ResultType: string;
  ArgList: TLexArgList);
begin
  FLexer.GetClassMethodDetailsFromCurrentPosEx(NestedClasses, ClassName,
    MethodName, Args, ResultType, ArgList);

  Args := CompressWhiteSpace(Args);
end;

function TMacroSourceParser.GoToNextMethodDefinition: Boolean;
begin
  Result := FLexer.GoToNextMethodDefinition;
end;

function TMacroSourceParser.GetDottedIdentifierAtPos: string;
begin
  Result := FLexer.GetDottedIdentifierAtPos;
end;

procedure TMacroSourceParser.GetRelevantClassMethodDetails(
  var NestedClasses, ClassName, MethodName, Args, ResultType: string);
begin
  repeat
    if GoToNextMethodDefinition then
    begin
      NextNoJunk; // Skip to first class/method name
      GetClassMethodDetailsFromCurrentPos(NestedClasses, ClassName, MethodName, Args, ResultType);
    end;
  until (not BeforeMode) or (TokenID = tkNull);
end;

initialization
  PrepareMacroLibList;
finalization
  UnprepareMacroLibList;
end.
