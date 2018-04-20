unit GX_UnitExportsParser;

interface

uses
  SysUtils,
  Classes,
  mwPasParserTypes,
  mPasLex,
  GX_dzNamedThread;

type
  // itUnknown is only there so Delphi 6 does not bomb out because of NIL objects in FIdentifiers
  TIdentifierTypes = (itUnknown, itConst, itType, itVar, itProcedure, itFunction);
  TIdentifier = record
    IdName: string;
    IdType: TIdentifierTypes;
  end;

type
  ///<summary>
  /// Simple parser that collects all identifiers declared in the interface of a unit.
  TUnitExportsParser = class
  private
    FFilename: string;
    FParser: TmwPasLex;
    FProcedures: TStrings;
    FFunctions: TStrings;
    FConstants: TStrings;
    FTypes: TStrings;
    FVariables: TStrings;
    FIdentifiers: TStrings;
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
    procedure SkipTypeDeclaration;
    procedure SkipVarDeclaration;
    function GetIdentifier(_Idx: Integer): TIdentifier;
  public
    ///<summary>
    /// @returns the namve of the given identifier type </summary>
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
  end;

type
  TUnitExportParserThread = class(TNamedThread)
  private
    FUnits: TStringList;
    FFiles: TStringList;
    FIdentifiers: TStrings;
  protected
    procedure Execute; override;
  public
    ///<summary>
    /// @param Files must be a list of full file names </summary>
    constructor Create(const _Files: TStrings; _OnTerminate: TNotifyEvent);
    destructor Destroy; override;
    ///<summary>
    /// After execution Identifiers contains a sorted list of all identfiers. The
    /// Objects[] pointers are PChars pointing to the unit in which they were found.
    /// @NOTE: Make a copy of these PChars (e.g. assign them to a string because they point
    ///        to entries in FUnits that are freed in the destructor! </summary>
    property Identifiers: TStrings read FIdentifiers;
  end;

implementation

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
end;

destructor TUnitExportsParser.Destroy;
begin
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
  s: AnsiString;
  DeclarationType: TDeclarationType;
begin
  sl := nil;
  FParser := TmwPasLex.Create;
  try
    sl := TStringList.Create;
    sl.LoadFromFile(FFilename);
    s := sl.Text;
    FParser.Origin := @s[1];
    DeclarationType := dtNone;
    while FParser.TokenID <> tkNull do begin
      if FParser.TokenID = tkImplementation then
        Exit;
      case FParser.TokenID of
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
                  SkipTypeDeclaration;
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
      FParser.NextNoJunk;
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
  FParser.NextNoJunk;
  while FParser.TokenID <> tkNull do begin
    if FParser.TokenID = _ClosingDel then begin
      // we have found the closing delimiter
      Exit; //==>
    end;
    if FParser.TokenID = _OpeningDel then
      // we found another opening delimiter
      SkipToClosingDelimiter(_OpeningDel, _ClosingDel);
    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipProcedureDeclaration;
begin
  FParser.NextNoJunk;
  if FParser.TokenID = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunk;
  end;
  while FParser.TokenID <> tkNull do begin
    if FParser.TokenID = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipClassOrRecord;
begin
  while FParser.TokenID <> tkNull do begin
    if FParser.TokenID = tkEnd then
      Exit; //==>
    // todo: handle more complex declarations
    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipTypeDeclaration;
begin
  FParser.NextNoJunk;
  if FParser.TokenID <> tkEqual then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  FParser.NextNoJunk;
  if FParser.TokenID = tkPacked then
    FParser.NextNoJunk;
  while FParser.TokenID <> tkNull do begin
    case FParser.TokenID of
      tkInterface, tkClass: begin
          FParser.NextNoJunk;
          if FParser.TokenID = tkRoundOpen then begin
            SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
            FParser.NextNoJunk;
            if FParser.TokenID = tkSemiColon then begin
              // simple declaration: type Tbla = class(Tblub); as used for e.g. exceptions or forward declarations
              Exit; //==>
            end;
          end else if FParser.TokenId = tkOf then begin
            // TBla = class of Tblub;
            FParser.NextNoJunk;
            while FParser.TokenID <> tkNull do begin
              if FParser.TokenID = tkSemiColon then
                Exit; //==>
              FParser.NextNoJunk;
            end;
            // should never happen
            Exit; //==>
          end;
          SkipClassOrRecord;
          Exit; //==>
        end;
      tkRecord: begin
          SkipClassOrRecord;
        end;
      tkSemiColon: begin
          // we have reached the end of the type declaration
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

    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipConstDeclaration;
begin
  FParser.NextNoJunk;
  while FParser.TokenID <> tkNull do begin
    case FParser.TokenID of
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
    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipVarDeclaration;
begin
  FParser.NextNoJunk;
  if FParser.TokenID = tkComma then begin
    // multiple variables in one declaration
    Exit; //==>
  end;
  while FParser.TokenID <> tkNull do begin
    if FParser.TokenID = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunk;
  end;
end;

procedure TUnitExportsParser.SkipFunctionDeclaration;
begin
  FParser.NextNoJunk;
  if FParser.TokenID = tkRoundOpen then begin
    SkipToClosingDelimiter(tkRoundOpen, tkRoundClose);
    FParser.NextNoJunk;
  end;
  if FParser.TokenID <> tkColon then begin
    // todo: this is an error -> handle it gracefully somehow
    Exit; //==>
  end;
  while FParser.TokenID <> tkNull do begin
    if FParser.TokenID = tkSemiColon then
      Exit; //==>
    FParser.NextNoJunk;
  end;
end;

{ TUnitExportParserThread }

constructor TUnitExportParserThread.Create(const _Files: TStrings; _OnTerminate: TNotifyEvent);
var
  i: Integer;
  fn: string;
begin
  OnTerminate := _OnTerminate;
  FIdentifiers := TStringList.Create;
  FUnits := TStringList.Create;
  FFiles := TStringList.Create;
  for i := 0 to _Files.Count - 1 do begin
    fn := _Files[i];
    UniqueString(fn);
    FFiles.Add(fn);
  end;
  inherited Create(False);
end;

destructor TUnitExportParserThread.Destroy;
begin
  OnTerminate := nil;
  FreeAndNil(FFiles);
  FreeAndNil(FIdentifiers);
  FreeAndNil(FUnits);
  inherited;
end;

procedure TUnitExportParserThread.Execute;
var
  FileIdx: Integer;
  fn: string;
  Parser: TUnitExportsParser;
  IdentIdx: Integer;
  sl: TStrings;
  UnitName: string;
begin
  inherited;

  for FileIdx := 0 to FFiles.Count - 1 do begin
    if Terminated then
      Exit; //==>
    fn := FFiles[FileIdx];
    if FileExists(fn) then begin
      Parser := TUnitExportsParser.Create(fn);
      try
        Parser.Execute;
        if Terminated then
          Exit; //==>
        UnitName := ExtractFileName(fn);
        UnitName := ChangeFileExt(UnitName, '');
        FUnits.Add(UnitName);
        sl := Parser.Identifiers;
        for IdentIdx := 0 to sl.Count - 1 do begin
          FIdentifiers.AddObject(sl[IdentIdx], Pointer(PChar(UnitName)));
        end;
      finally
        FreeAndNil(Parser);
      end;
    end;
  end;
  if Terminated then
    Exit; //==>
  TStringList(FIdentifiers).Sort;
end;

end.
