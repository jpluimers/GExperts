unit GX_UnitExportsParser;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GX_dzNamedThread,
  GX_dzCompilerAndRtlVersions,
  mwPasParserTypes,
  mPasLex;

{$IFOPT D+}
{$IF RTLVersion > RtlVersionDelphiXE}
// System.Diagnostics, which exports TStopWatch, was added to the RTL in DelphiXE2
{$DEFINE DO_TIMING}
{$IFEND}
{$ENDIF}

type
  // itUnknown is only there so Delphi 6 does not bomb out because of NIL objects in FIdentifiers
  TIdentifierTypes = (itUnknown, itConst, itType, itVar, itProcedure, itFunction);
  TIdentifier = record
    IdName: string;
    IdType: TIdentifierTypes;
  end;

type
  TSkipToElseOrEndifResult = (seeElse, seeEndif, seeElseIf, seeNull);

type
  TPasLexEx = class(TmwPasLex)
  private
    FSymbols: TStringList;
    FIfdefStack: TStringList;
    procedure Push(const _Condition: string);
    procedure Pop(out _Condition: string); overload;
    procedure Pop; overload;
    function TopOfStack: string;
    function GetSymbols: TStrings;
    procedure SetSymbols(const _Value: TStrings);
    function SkipToElseOrEndif(out _Expression: string): TSkipToElseOrEndifResult;
    ///<summary>
    /// @returns true, if the current token is {$ELSE}
    /// (allows additional text in the comment, so {$ENDIF bla} is also detected) </summary>
    function TokenIsElse: Boolean;
    ///<summary>
    /// @returns true, if the current token is either {$ENDIF} or {$IFEND}
    /// (allows additional text in the comment, so {$ENDIF bla} is also detected) </summary>
    function TokenIsEndif: Boolean;
    ///<summary>
    /// Checks for {$IFDEF <symbol>} or {$IF <expression>}
    /// @param Expression will return either be the defined(<symbol>) (for {$IFDEF <symbol>}
    ///        or the whole expression if the expression is more conplex
    /// @returns true, if the current tooken is {$IFDEF <symbol>} or {$IF <expression>}
    /// </summary>
    function TokenIsIfdef(out _Expression: string): Boolean;
    function TokenIsIfndef(out _Expression: string): Boolean;
    function TokenIsElseIf(out _Expression: string): Boolean;
    ///<summary>
    /// @returns true, if last token is <> tkNull </summary>
    function SkipToEndif: Boolean;
    function CheckExpression(const _Expression: string): Boolean;
    function Content: string;
  public
    constructor Create;
    destructor Destroy; override;
    function NextNoJunkEx: Boolean;
    property Symbols: TStrings read GetSymbols write SetSymbols;
  end;

type
  ///<summary>
  /// Simple parser that collects all identifiers declared in the interface of a unit.
  TUnitExportsParser = class
  private
    FFilename: string;
    FParser: TPasLexEx;
    FProcedures: TStrings;
    FFunctions: TStrings;
    FConstants: TStrings;
    FTypes: TStrings;
    FVariables: TStrings;
    FIdentifiers: TStrings;
    FSymbols: TStrings;
    procedure AddToIdentifiers(const _Identifier: string; _IdType: TIdentifierTypes);
    procedure AddToConsts(const _Token: string);
    procedure AddToFunctions(const _Token: string);
    procedure AddToProcedures(const _Token: string);
    procedure AddToTypes(const _Token: string);
    procedure AddToVars(const _Token: string);
    procedure SkipClassOrRecord;
    procedure SkipConstDeclaration;
    procedure SkipFunctionDeclaration;
    procedure SkipProcedureDeclaration;
    procedure SkipToClosingDelimiter(_OpeningDel, _ClosingDel: TTokenKind);
    procedure HandleTypeDeclaration;
    procedure SkipVarDeclaration;
    function GetIdentifier(_Idx: Integer): TIdentifier;
  public
    ///<summary>
    /// @returns the name of the given identifier type </summary>
    class function IdentfierTypeNames(_IdType: TIdentifierTypes): string;
    ///<summary>
    /// constructs a TUnitExportsParser instance for the given file </summary>
    constructor Create(const _Filename: string);
    destructor Destroy; override;
    ///<summary>
    /// Parses the file and fills the various properties
    /// may raise an exception if the file cannot be opened </summary>
    procedure Execute;
    ///<summary>
    /// Sorted list of all exported procedures </summary>
    property Procedures: TStrings read FProcedures;
    ///<summary>
    /// Sorted list of all exported functions </summary>
    property Functions: TStrings read FFunctions;
    ///<summary>
    /// Sorted list of all exported constants </summary>
    property Constants: TStrings read FConstants;
    ///<summary>
    /// Sorted list of all exported variables </summary>
    property Variables: TStrings read FVariables;
    ///<summary>
    /// Sorted list of all exported types </summary>
    property Types: TStrings read FTypes;
    ///<summary>
    /// Sorted list of all exported identifiers </summary>
    property Identifiers: TStrings read FIdentifiers;
    ///<summary>
    /// Sorted list of all exported identifiers as a TIdentifier record which
    /// in addition to the name contains the identifiert type </summary>
    property Identifier[_Idx: Integer]: TIdentifier read GetIdentifier;
    ///<summary>
    /// Count of exported identifiers </summary>
    function IdentifierCount: Integer;
    ///<summary>
    /// Add any conditional symbols here </summary>
    property Symbols: TStrings read FSymbols;
  end;

type
  TUnitExportParserThread = class(TNamedThread)
  private
    FUnits: TStringList;
    FFiles: TStringList;
    FIdentifiers: TStrings;
    FPaths: TStringList;
    FCacheDirBS: string;
{$IFDEF DO_TIMING}
    FLoadingTimeMS: Int64;
    FInsertingTimeMS: Int64;
    FSortingTimeMS: Int64;
    FParsedUnitsCount: Integer;
    FLoadedUnitsCount: Integer;
    FProcessingTimeMS: Int64;
    FTotalTimeMS: Int64;
    FSearchingTimeMS: Int64;
    FParsingTimeMS: Int64;
{$ENDIF}
    procedure AddSymbols(_Parser: TUnitExportsParser);
    procedure GetAllFilesInPath(_sl: TStringList);
    procedure GetAllFilesInDir(_dir: string; _sl: TStringList);
  protected
    procedure Execute; override;
  public
    ///<summary>
    /// @param Files is a list of unit names, without path and extension, which are to be parsed.
    ///              Can be NIL, in which case all files in the search path will be parsed
    /// @param Paths is a list of possible search paths
    /// @param CacheDir is a directory to cache the identifier lists </summary>
    constructor Create(const _Files: TStrings; _Paths: TStrings; const _CacheDir: string; _OnTerminate: TNotifyEvent);
    destructor Destroy; override;
    ///<summary>
    /// After execution Identifiers contains a sorted list of all identfiers. The
    /// Objects[] pointers are PChars pointing to the unit in which they were found.
    /// @NOTE: Make a copy of these PChars (e.g. assign them to a string because they point
    ///        to entries in FUnits that are freed in the destructor! </summary>
    property Identifiers: TStrings read FIdentifiers;
{$IFDEF DO_TIMING}
    property ParsedUnitsCount: Integer read FParsedUnitsCount;
    property LoadedUnitsCount: Integer read FLoadedUnitsCount;
    property LoadingTimeMS: Int64 read FLoadingTimeMS;
    property InsertingTimeMS: Int64 read FInsertingTimeMS;
    property ParsingTimeMS: Int64 read FParsingTimeMS;
    property ProcessingTimeMS: Int64 read FProcessingTimeMS;
    property SortingTimeMS: Int64 read FSortingTimeMS;
    property TotalTimeMS: Int64 read FTotalTimeMS;
    property SearchingTimeMS: Int64 read FSearchingTimeMS;
{$ENDIF}
  end;

implementation

uses
{$IFDEF DO_TIMING}
  System.Diagnostics,
{$ENDIF}
  StrUtils,
  GX_GenericUtils;

{ TPasLexEx }

constructor TPasLexEx.Create;
begin
  inherited Create;
  FSymbols := TStringList.Create;
  FSymbols.Sorted := True;
  FSymbols.Duplicates := dupIgnore;
  FSymbols.CaseSensitive := False;
  FIfdefStack := TStringList.Create;

  // call the following functions so they don't get elimiated by the linker
  TopOfStack;
  Content;
end;

destructor TPasLexEx.Destroy;
begin
  FreeAndNil(FIfdefStack);
  FreeAndNil(FSymbols);
  inherited;
end;

function TPasLexEx.GetSymbols: TStrings;
begin
  Result := FSymbols;
end;

procedure TPasLexEx.SetSymbols(const _Value: TStrings);
begin
  FSymbols.Assign(_Value);
end;

function TPasLexEx.CheckExpression(const _Expression: string): Boolean;
var
  s: string;
  Idx: Integer;
begin
  if StartsText('defined(', _Expression) then begin
    s := Trim(Copy(_Expression, 9, Length(_Expression) - 9));
    if (Pos('(', s) > 0) or (Pos(' ', s) > 0) then begin
      // we don't handle more complex expressions (yet)
      Result := False;
    end else begin
      // it's a simple defined(<symbol>), we can handle that
      Result := FSymbols.Find(s, Idx);
    end;
  end else begin
    // we don't handle more complex expressions (yet)
    Result := False;
  end;
end;

function TPasLexEx.TokenIsElse: Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := SameText('{$else}', TheToken) or StartsText('{$else ', TheToken);
end;

function TPasLexEx.TokenIsElseIf(out _Expression: string): Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := StartsText('{$elseif ', TheToken);
  if not Result then
    Exit; //==>

  _Expression := Trim(Copy(TheToken, 9, Length(TheToken) - 9));
end;

function TPasLexEx.TokenIsEndif: Boolean;
var
  TheToken: string;
begin
  TheToken := Token;
  Result := SameText('{$endif}', TheToken) or StartsText('{$endif ', TheToken)
    or SameText('{$ifend}', TheToken) or StartsText('{$ifend ', TheToken);
end;

function TPasLexEx.TokenIsIfdef(out _Expression: string): Boolean;
var
  TheToken: string;
  p: Integer;
begin
  TheToken := Token;
  Result := StartsText('{$ifdef ', TheToken);
  if Result then begin
    _Expression := Trim(Copy(TheToken, 9, Length(TheToken) - 9));
    p := Pos(' ', _Expression);
    if p > 0 then
      _Expression := Copy(_Expression, 1, p - 1);
    _Expression := 'defined(' + _Expression + ')';
    Exit; //==>
  end;

  if StartsText('{$if ', TheToken) then begin
    _Expression := Trim(Copy(TheToken, 5, Length(TheToken) - 5));
    Result := True;
  end;
end;

function TPasLexEx.TokenIsIfndef(out _Expression: string): Boolean;
var
  TheToken: string;
  p: Integer;
begin
  TheToken := Token;
  Result := StartsText('{$ifndef ', TheToken);
  if Result then begin
    _Expression := Trim(Copy(TheToken, 10, Length(TheToken) - 10));
    p := Pos(' ', _Expression);
    if p > 0 then
      _Expression := Copy(_Expression, 1, p - 1);
    _Expression := 'defined(' + _Expression + ')';
  end;
end;

function TPasLexEx.TopOfStack: string;
var
  cnt: Integer;
begin
  cnt := FIfdefStack.Count;
  if cnt > 0 then
    Result := FIfdefStack[cnt - 1]
  else
    Result := '<empty>';
end;

function TPasLexEx.Content: string;
begin
  if Assigned(Origin) then
    Result := Origin + RunPos
  else
    Result := '';
end;

function TPasLexEx.NextNoJunkEx: Boolean;
var
  Expression: string;
  ElseifExp: string;
  Res: TSkipToElseOrEndifResult;
begin
  Result := NextNoJunk;
  if not Result or (Tokenid <> tkCompDirect) then
    Exit; //==>

  if TokenIsIfdef(Expression) then begin
    if CheckExpression(Expression) then begin
      Push(Expression);
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end else begin
      Res := SkipToElseOrEndif(ElseifExp);
      while Res = seeElseIf do begin
        Pop;
        Push(ElseifExp);
        if CheckExpression(ElseifExp) then begin
          // it's possible that the next token again is a compiler directive, so recurse
          Result := NextNoJunkEx;
          Exit; //==>
        end;
        Res := SkipToElseOrEndif(ElseifExp);
      end;
      case Res of
        seeElse: begin
            Push('not ' + Expression);
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
        seeEndif: begin
            Pop;
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
      else // seeNull
        Result := False;
      end;
    end;
    Exit; //==>
  end;

  if TokenIsIfndef(Expression) then begin
    if not CheckExpression(Expression) then begin
      Push('not ' + Expression);
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end else begin
      Res := SkipToElseOrEndif(ElseifExp);
      while Res = seeElseIf do begin
        Pop;
        Push(ElseifExp);
        if CheckExpression(ElseifExp) then begin
          // it's possible that the next token again is a compiler directive, so recurse
          Result := NextNoJunkEx;
          Exit; //==>
        end;
        Res := SkipToElseOrEndif(ElseifExp);
      end;
      case Res of
        seeElse: begin
            Push(Expression);
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
        seeEndif: begin
            // it's possible that the next token again is a compiler directive, so recurse
            Result := NextNoJunkEx;
          end;
      else // seeNull
        Result := False;
      end;
    end;
    Exit; //==>
  end;

  if TokenIsElse then begin
    // we have already parsed the if branch, so skip  to endif
    Result := SkipToEndif;
    if Result then begin
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end;
    Exit; //==>
  end;

  if TokenIsElseIf(Expression) then begin
    // if we get here, we have already parsed the if branch, so skip to endif
    Pop;
    Push(Expression);
    // we have already parsed the if branch, so skip  to endif
    Result := SkipToEndif;
    if Result then begin
      // it's possible that the next token again is a compiler directive, so recurse
      Result := NextNoJunkEx;
    end;
    Exit; //==>
  end;

  if TokenIsEndif then begin
    // we have parsed the else branch, so we remove the symbol from the stack and continue
    Pop;
    // it's possible that the next token again is a compiler directive, so recurse
    Result := NextNoJunkEx;
    Exit; //==>
  end;
end;

procedure TPasLexEx.Pop(out _Condition: string);
var
  cnt: Integer;
begin
  cnt := FIfdefStack.Count;
  if cnt > 0 then begin
    _Condition := FIfdefStack[cnt - 1];
    FIfdefStack.Delete(cnt - 1);
  end;
end;

procedure TPasLexEx.Pop;
var
  Condition: string;
begin
  Pop(Condition);
end;

procedure TPasLexEx.Push(const _Condition: string);
begin
  FIfdefStack.Add(_Condition);
end;

function TPasLexEx.SkipToEndif: Boolean;
var
  Expression: string;
begin
  Result := True;
  while NextNoJunk do begin
    if Tokenid = tkCompDirect then begin
      if TokenIsEndif then
        Exit; //==>
      if TokenIsIfdef(Expression) then begin
        // nested $if
        SkipToEndif;
      end;
    end;
  end;
  Result := False;
end;

function TPasLexEx.SkipToElseOrEndif(out _Expression: string): TSkipToElseOrEndifResult;
var
  Expression: string;
begin
  while NextNoJunk do begin
    if Tokenid = tkCompDirect then begin
      if TokenIsElse then begin
        Result := seeElse;
        Exit; //==>
      end;
      if TokenIsEndif then begin
        Result := seeEndif;
        Exit; //==>
      end;
      if TokenIsElseIf(_Expression) then begin
        Result := seeElseIf;
        Exit; //==>
      end;

      if TokenIsIfdef(Expression) then begin
        // nested $if
        SkipToEndif;
      end;
    end;
  end;
  Result := seeNull;
end;

{ TUnitExportsParser }

class function TUnitExportsParser.IdentfierTypeNames(_IdType: TIdentifierTypes): string;
begin
  case _IdType of
    itConst: Result := 'const';
    itType: Result := 'type';
    itVar: Result := 'var';
    itProcedure: Result := 'procedure';
    itFunction: Result := 'function';
  else // itUnknown:
    Result := 'unknown';
  end;
end;

constructor TUnitExportsParser.Create(const _Filename: string);

  procedure CreateStrings(var _st: TStrings);
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;
    _st := sl;
  end;

begin
  inherited Create;
  FFilename := _Filename;
  CreateStrings(FProcedures);
  CreateStrings(FFunctions);
  CreateStrings(FConstants);
  CreateStrings(FVariables);
  CreateStrings(FTypes);
  CreateStrings(FIdentifiers);
  CreateStrings(FSymbols);
end;

destructor TUnitExportsParser.Destroy;
begin
  FreeAndNil(FSymbols);
  FreeAndNil(FProcedures);
  FreeAndNil(FFunctions);
  FreeAndNil(FConstants);
  FreeAndNil(FVariables);
  FreeAndNil(FTypes);
  FreeAndNil(FIdentifiers);
  inherited;
end;

type
  TDeclarationType = (dtNone, dtConst, dtType, dtVar, dtFunction, dtProcedure);

procedure TUnitExportsParser.Execute;
var
  sl: TStringList;
  s: string;
  DeclarationType: TDeclarationType;
begin
  sl := nil;
  FParser := TPasLexEx.Create;
  try
    FParser.Symbols := FSymbols;
    sl := TStringList.Create;
    sl.LoadFromFile(FFilename);
    s := sl.Text;
    FParser.Origin := @s[1];
    DeclarationType := dtNone;
    while FParser.Tokenid <> tkNull do begin
      if FParser.Tokenid = tkImplementation then
        Exit;
      case FParser.Tokenid of
        tkConst: begin
            DeclarationType := dtConst;
          end;
        tkVar: begin
            DeclarationType := dtVar;
          end;
        tkType: begin
            DeclarationType := dtType;
          end;
        tkIdentifier: begin
            case DeclarationType of
              dtNone: ;
              dtConst: begin
                  AddToConsts(FParser.Token);
                  SkipConstDeclaration;
                end;
              dtType: begin
                  AddToTypes(FParser.Token);
                  HandleTypeDeclaration;
                end;
              dtVar: begin
                  AddToVars(FParser.Token);
                  SkipVarDeclaration;
                end;
              dtFunction: begin
                  AddToFunctions(FParser.Token);
                  SkipFunctionDeclaration;
                end;
              dtProcedure: begin
                  AddToProcedures(FParser.Token);
                  SkipProcedureDeclaration
                end;
            end;
          end;
        tkProcedure: begin
            DeclarationType := dtProcedure;
          end;
        tkFunction: begin
            DeclarationType := dtFunction;
          end;
      end;
      FParser.NextNoJunkEx;
    end;
  finally
    FreeAndNil(sl);
    FreeAndNil(FParser);
  end;
end;

function TUnitExportsParser.IdentifierCount: Integer;
begin
  Result := FIdentifiers.Count;
end;

function TUnitExportsParser.GetIdentifier(_Idx: Integer): TIdentifier;
begin
  Result.IdName := FIdentifiers[_Idx];
  Result.IdType := TIdentifierTypes(FIdentifiers.Objects[_Idx]);
end;

procedure TUnitExportsParser.AddToIdentifiers(const _Identifier: string; _IdType: TIdentifierTypes);
begin
  FIdentifiers.AddObject(_Identifier, Pointer(Ord(_IdType)));
end;

procedure TUnitExportsParser.AddToConsts(const _Token: string);
begin
  FConstants.Add(_Token);
  AddToIdentifiers(_Token, itConst);
end;

procedure TUnitExportsParser.AddToVars(const _Token: string);
begin
  FVariables.Add(_Token);
  AddToIdentifiers(_Token, itVar);
end;

procedure TUnitExportsParser.AddToTypes(const _Token: string);
begin
  FTypes.Add(_Token);
  AddToIdentifiers(_Token, itType);
end;

procedure TUnitExportsParser.AddToFunctions(const _Token: string);
begin
  FFunctions.Add(_Token);
  AddToIdentifiers(_Token, itFunction);
end;

procedure TUnitExportsParser.AddToProcedures(const _Token: string);
begin
  FProcedures.Add(_Token);
  AddToIdentifiers(_Token, itProcedure);
end;

procedure TUnitExportsParser.SkipToClosingDelimiter(_OpeningDel, _ClosingDel: TTokenKind);
begin
  while FParser.NextNoJunkEx do begin
    if FParser.Tokenid = _ClosingDel then begin
      // we have found the closing delimiter
      Exit; //==>
    end;
    if FParser.Tokenid = _OpeningDel then
      // we found another opening delimiter
      SkipToClosingDelimiter(_OpeningDel, _ClosingDel);
  end;
end;

procedure TUnitExportsParser.SkipProcedureDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunkEx;
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipClassOrRecord;
begin
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkEnd:
        Exit; //==>
      tkClass: begin
          FParser.NextNoJunkEx;
          if not (FParser.Tokenid in [tkFunction, tkProcedure, tkConstructor, tkDestructor, tkOperator,
            tkVar, tkThreadvar, tkProperty]) then begin
            // nested class declaration
            if FParser.Tokenid = tkRoundOpen then begin
              // class declaration with ancestor
              SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
              FParser.NextNoJunkEx;
              if FParser.Tokenid = tkSemiColon then
                // type class bla(blub);
              else
                SkipClassOrRecord;
            end else
              SkipClassOrRecord;
          end;
        end;
      tkRecord,
        tkInterface: begin
          FParser.NextNoJunkEx;
          // nested record/interface declaration
          SkipClassOrRecord;
        end;
    end;
    // todo: handle more complex declarations
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.HandleTypeDeclaration;
begin
  // this mostly just skips the type declaration but adds enum identifiers to the identifier list
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkLower then begin
    // type bla<tresult>
    SkipToClosingDelimiter(tkLower, tkGreater);
    FParser.NextNoJunkEx;
  end;
  if FParser.Tokenid <> tkEqual then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    // enum declaration -> add the enum identifiers
    FParser.NextNoJunkEx;
    while FParser.Tokenid <> tkRoundClose do begin
      if FParser.Tokenid = tkNull then
        Exit; //==>
      if FParser.Tokenid = tkIdentifier then begin
        AddToIdentifiers(FParser.Token, itConst);
      end;
      FParser.NextNoJunkEx;
    end;
  end;
  if FParser.Tokenid = tkPacked then
    FParser.NextNoJunkEx;
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkSemiColon: begin
          // we have reached the end of the type declaration
          Exit; //==>
        end;
      tkInterface, tkDispinterface, tkClass: begin
          FParser.NextNoJunkEx;
          if FParser.Tokenid = tkSemiColon then begin
            // forward declaration: type Tbla = class;
            //                  or: type Tbla = interface;
            Exit; //==>
          end else if FParser.Tokenid = tkRoundOpen then begin
            SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
            FParser.NextNoJunkEx;
            if FParser.Tokenid = tkSquareOpen then begin
              // interface(bla)[guid]
              SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
              FParser.NextNoJunkEx;
            end;
            if FParser.Tokenid = tkSemiColon then begin
              // simple declaration: type Tbla = class(Tblub); as used for e.g. exceptions or forward declarations
              Exit; //==>
            end;
          end else if FParser.Tokenid = tkSquareOpen then begin
            // interface[guid]
            SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
            FParser.NextNoJunkEx;
            if FParser.Tokenid = tkSemiColon then begin
              // simple declaration: type Tbla = class(Tblub); as used for e.g. exceptions or forward declarations
              Exit; //==>
            end;
          end else if FParser.Tokenid = tkOf then begin
            // TBla = class of Tblub;
            FParser.NextNoJunkEx;
            while FParser.Tokenid <> tkNull do begin
              if FParser.Tokenid = tkSemiColon then
                Exit; //==>
              FParser.NextNoJunkEx;
            end;
            // should never happen
            Exit; //==>
          end;
          SkipClassOrRecord;
          Exit; //==>
        end;
      tkRecord: begin
          FParser.NextNoJunkEx;
          if FParser.Tokenid = tkSemiColon then begin
            // forward declaration: type Tbla = record;
            Exit; //==>
          end;
          SkipClassOrRecord;
          Exit; //==>
        end;
      tkProcedure: begin
          // type bla = procedure(...);
          // type bla = procedure(...) of object;
          SkipProcedureDeclaration;
          Exit; //==>
        end;
      tkFunction: begin
          // type bla = function(...): Sometype;
          // type bla = function(...): Sometype of object;
          SkipFunctionDeclaration;
          Exit; //==>
        end;
    end;

    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipConstDeclaration;
begin
  FParser.NextNoJunkEx;
  while FParser.Tokenid <> tkNull do begin
    case FParser.Tokenid of
      tkSquareOpen: begin
          SkipToClosingDelimiter(tkSquareOpen, tkSquareClose);
        end;
      tkRoundOpen: begin
          SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
        end;
      tkSemiColon: begin
          // found the end
          Exit; //==>
        end;
    end;
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipVarDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkComma then begin
    // multiple variables in one declaration
    Exit; //==>
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkRoundOpen then begin
      SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    end else if FParser.Tokenid = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunkEx;
  end;
end;

procedure TUnitExportsParser.SkipFunctionDeclaration;
begin
  FParser.NextNoJunkEx;
  if FParser.Tokenid = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunkEx;
  end;
  if FParser.Tokenid <> tkColon then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  while FParser.Tokenid <> tkNull do begin
    if FParser.Tokenid = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunkEx;
  end;
end;

{ TUnitExportParserThread }

constructor TUnitExportParserThread.Create(const _Files: TStrings; _Paths: TStrings;
  const _CacheDir: string; _OnTerminate: TNotifyEvent);
var
  i: Integer;
  s: string;
begin
  OnTerminate := _OnTerminate;

  FCacheDirBS := IncludeTrailingPathDelimiter(_CacheDir);
  UniqueString(FCacheDirBS);

  FIdentifiers := TStringList.Create;
  FUnits := TStringList.Create;

  if Assigned(_Files) then begin
    FFiles := TStringList.Create;
    for i := 0 to _Files.Count - 1 do begin
      s := _Files[i];
      UniqueString(s);
      FFiles.Add(s);
    end;
  end else
    FFiles := nil;

  FPaths := TStringList.Create;
  for i := 0 to _Paths.Count - 1 do begin
    s := _Paths[i];
    UniqueString(s);
    FPaths.Add(s);
  end;
  inherited Create(False);
end;

destructor TUnitExportParserThread.Destroy;
begin
  OnTerminate := nil;
  inherited;
  FreeAndNil(FPaths);
  FreeAndNil(FFiles);
  FreeAndNil(FIdentifiers);
  FreeAndNil(FUnits);
end;

procedure TUnitExportParserThread.AddSymbols(_Parser: TUnitExportsParser);
begin
  _Parser.Symbols.Add(Format('VER%.0f', [CompilerVersion * 10]));
  // todo: This might not be correct: Are all targets of Unicode aware Delphi versions also Unicode aware?
{$IFDEF UNICODE}
  _Parser.Symbols.Add('UNICODE');
{$ENDIF}
  // todo: Handle the symbols defined by the target somehow
  // {$ifdef WINDOWS}
  //  _Parser.Symbols.Add('WINDOWS');
  // {$ENDIF}
end;

function FileTimeToDosTime(_ft: _FILETIME): GXNativeUInt;
var
  DosDate: Word;
  DosTime: Word;
begin
  if FileTimeToDosDateTime(_ft, DosDate, DosTime) then begin
    Result := DosDate shl 16 + DosTime;
  end else begin
    // If converting the file date/time to DOS date/time fails, we don't really care, since the
    // worst thing that can happen is that we continue using an outdated cache file
    // (if it fails for a unit in the sarch path) or reparse a unit (if it fails for a cache file).
    Result := 0;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

{$IF not declared(faTemporary)}
const
  faTemporary = $00000100;
  faSymLink = $00000400;
{$IFEND}

procedure TUnitExportParserThread.GetAllFilesInDir(_dir: string; _sl: TStringList);
var
  sr: TSearchRec;
begin
  _dir := IncludeTrailingPathDelimiter(_dir);
  if SysUtils.FindFirst(_dir + '*.pas', faAnyFile, sr) = 0 then begin
    try
      repeat
        if (sr.Attr and (faDirectory or faHidden or faSysFile or faTemporary or faSymLink)) = 0 then begin
          _sl.AddObject(_dir + sr.Name, Pointer(FileTimeToDosTime(sr.FindData.ftLastWriteTime)));
        end;
      until SysUtils.FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr);
    end;
  end;
end;

procedure TUnitExportParserThread.GetAllFilesInPath(_sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to FPaths.Count - 1 do begin
    GetAllFilesInDir(FPaths[i], _sl);
  end;
end;

procedure TUnitExportParserThread.Execute;
var
  FilesInPath: TStringList;

  function MakeFilename(const Path, FileName: string): string;
  begin
    if Path = '' then
      Result := FileName
    else if Path[Length(Path)] = PathDelim then
      Result := Path + FileName
    else
      Result := Path + PathDelim + FileName;
  end;

  function GxTryGetFileAge(const _fn: string; out _DosTime: GXNativeUInt): Boolean;
  var
    FileInformation: TWin32FileAttributeData;
  begin
    Result := GetFileAttributesEx(PChar(_fn), GetFileExInfoStandard, @FileInformation);
    if Result then begin
      _DosTime := FileTimeToDosTime(FileInformation.ftLastWriteTime);
    end;
  end;

  function TryFindPathToFile(const _fn: string; out _FoundFn: string; out _FileAge: GXNativeUInt): Boolean;
  var
    i: Integer;
    fno: string;
    fn: string;
  begin
    for i := 0 to FilesInPath.Count - 1 do begin
      fn := FilesInPath[i];
      fno := ExtractFileName(fn);
      if SameText(fno, _fn) then begin
        Result := True;
        _FoundFn := fn;
        _FileAge := GXNativeUInt(FilesInPath.Objects[i]);
        Exit;
      end;
    end;
    Result := False;
  end;

var
  FileIdx: Integer;
  fn: string;
  Parser: TUnitExportsParser;
  IdentIdx: Integer;
  sl: TStrings;
  UnitName: string;
  CacheFn: string;
  UnitTime: UInt32;
  CacheTime: UInt32;
{$IFDEF  DO_TIMING}
  Loading: TStopwatch;
  Inserting: TStopwatch;
  Sorting: TStopwatch;
  Processing: TStopwatch;
  Total: TStopwatch;
  Searching: TStopwatch;
  Parsing: TStopwatch;
{$ENDIF}
begin
  inherited;

{$IFDEF  DO_TIMING}
  Loading := TStopwatch.Create;
  Inserting := TStopwatch.Create;
  Sorting := TStopwatch.Create;
  Processing := TStopwatch.Create;
  Total := TStopwatch.Create;
  Searching := TStopwatch.Create;
  Parsing := TStopwatch.Create;

  Total.Start;
  Searching.Start;
{$ENDIF}

  sl := nil;
  FilesInPath := TStringList.Create;
  try
    sl := TStringList.Create;
    GetAllFilesInPath(FilesInPath);

    if Assigned(FFiles) then begin
      for FileIdx := 0 to FFiles.Count - 1 do begin
        if TryFindPathToFile(FFiles[FileIdx] + '.pas', fn, UnitTime) then
          sl.AddObject(fn, Pointer(UnitTime));
      end;
      if sl.Count = 0 then
        Exit; //==>
      FFiles.Assign(sl);
    end else begin
      FFiles := FilesInPath;
      FilesInPath := nil;
    end;
  finally
    FreeAndNil(sl);
    FreeAndNil(FilesInPath);
  end;
{$IFDEF  DO_TIMING}
  Searching.Stop;
{$ENDIF}

  ForceDirectories(FCacheDirBS);

{$IFDEF  DO_TIMING}
  Processing.Start;
{$ENDIF}
  for FileIdx := 0 to FFiles.Count - 1 do begin
    if Terminated then
      Exit; //==>
    fn := FFiles[FileIdx];
    UnitTime := UInt32(FFiles.Objects[FileIdx]);
    UnitName := ExtractFileName(fn);
    UnitName := ChangeFileExt(UnitName, '');
    CacheFn := MangleFilename(fn);
    CacheFn := FCacheDirBS + CacheFn;
    if GxTryGetFileAge(CacheFn, CacheTime) and (UnitTime < CacheTime) then begin
{$IFDEF  DO_TIMING}
      Inc(FLoadedUnitsCount);
{$ENDIF}
      sl := TStringList.Create;
      try
{$IFDEF  DO_TIMING}
        Loading.Start;
{$ENDIF}
        sl.LoadFromFile(CacheFn);
{$IFDEF  DO_TIMING}
        Loading.Stop;
{$ENDIF}

        FUnits.Add(UnitName);
        FIdentifiers.AddObject(UnitName, Pointer(PChar(UnitName)));

{$IFDEF  DO_TIMING}
        Inserting.Start;
{$ENDIF}
        for IdentIdx := 0 to sl.Count - 1 do
          FIdentifiers.AddObject(sl[IdentIdx], Pointer(PChar(UnitName)));
{$IFDEF  DO_TIMING}
        Inserting.Stop;
{$ENDIF}
      finally
        FreeAndNil(sl);
      end;
    end else begin
{$IFDEF  DO_TIMING}
      Inc(FParsedUnitsCount);
      Parsing.Start;
{$ENDIF}
      Parser := TUnitExportsParser.Create(fn);
      try
        AddSymbols(Parser);
        Parser.Execute;
        if Terminated then
          Exit; //==>
        FUnits.Add(UnitName);
        sl := Parser.Identifiers;
        sl.SaveToFile(CacheFn);
        FIdentifiers.AddObject(UnitName, Pointer(PChar(UnitName)));
        for IdentIdx := 0 to sl.Count - 1 do begin
          FIdentifiers.AddObject(sl[IdentIdx], Pointer(PChar(UnitName)));
        end;
      finally
        FreeAndNil(Parser);
      end;
{$IFDEF  DO_TIMING}
      Parsing.Stop;
{$ENDIF}
    end;
  end;
  if Terminated then
    Exit; //==>

{$IFDEF  DO_TIMING}
  Processing.Stop;

  Sorting.Start;
{$ENDIF}
  TStringList(FIdentifiers).Sort;
{$IFDEF  DO_TIMING}
  Sorting.Stop;

  Total.Stop;

  FSearchingTimeMS := Searching.ElapsedMilliseconds;
  FSortingTimeMS := Sorting.ElapsedMilliseconds;
  FLoadingTimeMS := Loading.ElapsedMilliseconds;
  FInsertingTimeMS := Inserting.ElapsedMilliseconds;
  FParsingTimeMS := Parsing.ElapsedMilliseconds;
  FProcessingTimeMS := Processing.ElapsedMilliseconds;
  FTotalTimeMS := Total.ElapsedMilliseconds;
{$ENDIF}
end;

end.

