unit GX_FileScanner;

(*
  Scans pas/cpp files and produces a list of implemented methods

  Known Scanning False Positives/Bugs:
  var CoInitializeExProc: function (pvReserved: Pointer; coInit: Longint): HResult; stdcall;
  procedure ReleaseToolMenuKeyHooks; forward;
*)

interface

uses
  Classes, mPasLex, mwBCBTokenList;

type
  TSourceLanguage = (ltPas, ltCpp);
  TProcScanningStatus = (stNone, stInProcedure, stScanned, stOutofProcedure);

  TProcedure = class(TCollectionItem)
  private
    FLineNo: Integer;
    FName: string;
    FDisplayName: string;
    FProcedureType: string;
    FProcArgs: string;
    FProcClass: string;
    FProcReturnType: string;
    FBeginIndex: Integer;
    FScanningStatus: TProcScanningStatus;
    FEndIndex: Integer;
    FProcName: string;
    FObjectSeparator: string;
    function GetProcName: string;
    function GetBody: string;
    function GetProcClass: string;
    procedure GetProcNameClassDetails;
  public
    ProcLine: string;
    property LineNo: Integer read FLineNo write FLineNo;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property ProcedureType: string read FProcedureType write FProcedureType;
    property ProcArgs: string read FProcArgs write FProcArgs;
    property ProcName: string read GetProcName write FProcName;
    property ProcClass: string read GetProcClass write FProcClass;
    property ProcReturnType: string read FProcReturnType write FProcReturnType;
    property Body: string read GetBody;
    property BeginIndex: Integer read FBeginIndex write FBeginIndex;
    property ScanningStatus: TProcScanningStatus read FScanningStatus write FScanningStatus;
    property EndIndex: Integer read FEndIndex write FEndIndex;
    property ObjectSeparator: string read FObjectSeparator write FObjectSeparator;
  end;

  TProcedures = class(TCollection)
  private
    FMemStream: TMemoryStream;
    function GetItem(Index: Integer): TProcedure;
  public
    property MemStream: TMemoryStream read FMemStream write FMemStream;
    property Items[Index: Integer]: TProcedure read GetItem; default;
  end;

  TLanguage = class(TObject)
  private
    FOrigin: PAnsiChar;
    FFileStream: TMemoryStream;
  protected
    FProcedures: TProcedures;
    FFileName: string;
  public
    constructor Create(const FileName: string);
    procedure Execute; virtual; abstract;
    property Procedures: TProcedures read FProcedures write FProcedures;
    property Origin: PAnsiChar read fOrigin write FOrigin;
    property FileStream: TMemoryStream read FFileStream write FFileStream;
    property FileName: string read FFileName write FFileName;
  end;

  TFileScanner = class(TComponent)
  private
    FMemStream: TMemoryStream;
    FLanguage: TSourceLanguage;
    FLanguageParser: TLanguage;
    FProcedures: TProcedures;
    FFileName: string;
  public
    procedure Execute;
    constructor CreateWithFileName(AOwner: TComponent; const FileName: string); overload;
    destructor Destroy; override;
    property Language: TSourceLanguage read FLanguage write FLanguage;
    property MemStream: TMemoryStream read FMemStream write FMemStream;
    property Procedures: TProcedures read FProcedures write FProcedures;
  end;

  TPascal = class(TLanguage)
  private
    FParser: TmwPasLex;
    function MoveToImplementation: Boolean;
    function GetProperProcName(ProcType: TTokenKind; IsClass: Boolean): string;
  public
    procedure Execute; override;
  end;

  TCpp = class(TLanguage)
  private
    FCParser: TBCBTokenList;
    FNameList: TStringList;
    FBraceCount: Integer;
    FBeginBracePosition: Longint;
    function InProcedureBlacklist(const Name: string): Boolean;
    procedure EraseName(Index: Integer);
    procedure FindBeginningBrace;
    procedure FindEndingBrace(const BraceCountDelta: Integer; const DecrementOpenBrace: Boolean);
    procedure FindBeginningProcedureBrace(var Name: string);
    function SearchForProcedureName: string;
    function SearchForTemplateArgs: string;
  public
    procedure Execute; override;
  end;

implementation

uses SysUtils, Contnrs, GX_GenericUtils, Math;

resourcestring
  SUnknown = 'Unknown';
  SImplementationNotFound = 'Implementation section not found.';

// These are some hardcoded macros to ignore as C++ procedure names
const
  ProcBlacklist: array[0..2] of string = ('CATCH_ALL', 'CATCH', 'AND_CATCH_ALL');

{ TProcedure }  

function TProcedure.GetProcName: string;
begin
  if FProcName = '' then
    GetProcNameClassDetails;
  Result := FProcName;
end;

function TProcedure.GetBody: string;
begin
  Result := Copy(PAnsiChar(TProcedures(Collection).MemStream.Memory), FBeginIndex +
    1, (FEndIndex - FBeginIndex));
end;

function TProcedure.GetProcClass: string;
begin
  if FProcClass = '' then
    GetProcNameClassDetails;

  Result := FProcClass;
end;

procedure TProcedure.GetProcNameClassDetails;
var
  ParamStart: Integer;
  SpacePos: Integer;
  Line: string;
  NestedClasses: string;
begin
  Line := Trim(ProcLine);
  if StrBeginsWith('class ', Line, False) then
    Line := Trim(Copy(Line, Length('class '), Length(Line)));

  ParamStart := Pos('(', Line);
  if ParamStart = 0 then
  begin
    ParamStart := Pos(':', Line); // Could be a function
    if ParamStart = 0 then
      ParamStart := Length(Line);
  end;
  SpacePos := Pos(' ', Line);
  SpacePos := Max(SpacePos, 1);
  Line := Trim(Copy(Line, SpacePos + 1, ParamStart - SpacePos - 1));
  DecomposeClassMethodString(Line, FProcName, FProcClass, NestedClasses);
end;

{ TProcedures }

function TProcedures.GetItem(Index: Integer): TProcedure;
begin
  Result := TProcedure(inherited GetItem(Index));
end;

{ TFileScanner }

procedure TFileScanner.Execute;
var
  IsSelfMemoryStream: Boolean;
begin
  case Language of
    ltPas: FLanguageParser := TPascal.Create(FFileName);
    ltCpp: FLanguageParser := TCpp.Create(FFileName);
  end;
  try
    IsSelfMemoryStream := not Assigned(FMemStream);
    if IsSelfMemoryStream then
      FMemStream := TMemoryStream.Create;
    FLanguageParser.FileStream := FMemStream;
    try
      FLanguageParser.Procedures := FProcedures;
      FLanguageParser.Execute;
    finally
      if IsSelfMemoryStream then
        FreeAndNil(FMemStream);
    end;
  finally
    FreeAndNil(FLanguageParser);
  end;
end;

constructor TFileScanner.CreateWithFileName(AOwner: TComponent; const FileName: string);
begin
  inherited Create(AOwner);
  FMemStream := nil;
  FFileName := FileName;
  FProcedures := TProcedures.Create(TProcedure);
  FProcedures.MemStream := FMemStream;

  if IsCppSourceModule(FileName) then
    Language := ltCpp
  else
    Language := ltPas;
end;

destructor TFileScanner.Destroy;
begin
  FreeAndNil(FProcedures);
  inherited;
end;

{ TPascal }

procedure TPascal.Execute;
var
  ClassLast: Boolean;
  InParenthesis: Boolean;
  ProcScanning: TProcScanningStatus;
  InTypeDeclaration: Boolean;
  DeclarationsPresent: Boolean;
  IdentifierNeeded: Boolean;
  BeginCount: Integer;
  ScanBeginIndex: Integer;
  ProcedureStack: TStack;
  ProcedureItem: TProcedure;
  ProcLineLength: Integer;
begin
  FParser := TmwPasLex.Create;
  FParser.Origin := FFileStream.Memory;
  FProcedures.MemStream := FFileStream;
  ProcedureStack := TStack.Create;
  FProcedures.BeginUpdate;
  ProcedureItem := nil;
  try
    if (not (IsDpr(FileName) or IsInc(FileName))) and (not MoveToImplementation) then
      raise Exception.Create(SImplementationNotFound);
    InParenthesis := False;
    InTypeDeclaration := False;
    DeclarationsPresent := False;
    ProcScanning := stNone;
    BeginCount := 0;

    while FParser.TokenID <> tkNull do
    begin
      // Filter out method declarations inside type declarations
      if ((FParser.TokenID in [tkClass]) and FParser.IsClass) or (FParser.TokenID = tkInterface) then
      begin
        InTypeDeclaration := True;
        DeclarationsPresent := False;
      end
      else if InTypeDeclaration and
        (FParser.TokenID in [tkProcedure, tkFunction, tkOperator, tkProperty,
        tkPrivate, tkProtected, tkPublic, tkPublished]) then
      begin
        DeclarationsPresent := True;
      end
      else if InTypeDeclaration and
        ((FParser.TokenID = tkEnd) or
        ((FParser.TokenID = tkSemiColon) and not DeclarationsPresent)) then
      begin
        InTypeDeclaration := False;
      end;

      // Start scanning dependant in if a class proc or not
      ClassLast := (FParser.TokenID = tkClass);
      if ClassLast then
      begin
        ScanBeginIndex := FParser.TokenPos;
        FParser.NextNoJunk;
      end
      else
      begin
        FParser.NextNoJunk;
        ScanBeginIndex := FParser.TokenPos;
      end;

      // Get procedure name and parameters
      if not InTypeDeclaration and (FParser.TokenID in MethodMarkers) then
      begin
        if (FParser.TokenID = tkOperator) and (not ClassLast) then
        begin
          FParser.NextNoJunk;
          Continue; // Operator overload methods must be class methods
        end;
        // A new procedure was found.
        // If already scanning a procedure,
        // then the new procedure must be a subprocedure.  Store
        // current procedure because it'll be further down in the file.
        if Assigned(ProcedureItem) then
        begin
          ProcedureItem.EndIndex := FParser.TokenPos;
          ProcedureItem.ScanningStatus := stOutofProcedure;
          ProcedureStack.Push(ProcedureItem);
          ProcScanning := stNone;
          BeginCount := 0;
        end;

        ProcedureItem := TProcedure(FProcedures.Add);
        ProcedureItem.BeginIndex := ScanBeginIndex;
        ProcedureItem.LineNo := FParser.LineNumber + 1;
        ProcedureItem.ObjectSeparator := '.';

        IdentifierNeeded := True;
        ProcedureItem.ProcedureType := GetProperProcName(FParser.TokenID, ClassLast);

        // Parse the procedure name and parameters
        while not (FParser.TokenID in [tkNull]) do
        begin
          case FParser.TokenID of
            tkIdentifier, tkRegister:
              IdentifierNeeded := False;

            tkRoundOpen:
              begin
                // Prevent AProcedure = procedure() of object;
                // from being recognised as a procedure
                if IdentifierNeeded then
                begin
                  FreeAndNil(ProcedureItem);
                  Break;
                end;
                InParenthesis := True;
              end;

            tkRoundClose:
              InParenthesis := False;
          end; // case

          // End of the parameter list?
          if (not InParenthesis) and (FParser.TokenID = tkSemiColon) then
            Break;

          FParser.NextNoJunk;
        end; // while
        if Assigned(ProcedureItem) then
        begin
          ProcLineLength := FParser.TokenPos - ProcedureItem.BeginIndex + 1;
          SetString(ProcedureItem.ProcLine, FParser.Origin + ProcedureItem.BeginIndex, ProcLineLength);
          ProcedureItem.ProcLine := CompressWhiteSpace(ProcedureItem.ProcLine);
        end;
      end
      else
      begin // Into the code for the procedure text
        if not ((FParser.TokenID = tkSemiColon)) then
        begin
          case FParser.TokenID of
            tkBegin, tkAsm:
              begin
                ProcScanning := stInProcedure;
                Inc(BeginCount);
              end;
            tkInitialization, tkFinalization:
              begin
                ProcScanning := stOutofProcedure;
                if Assigned(ProcedureItem) then
                  ProcedureItem.EndIndex := FParser.TokenPos;
              end;
          end;

          if (ProcScanning in [stInProcedure]) then
          begin
            // These have ends with no begins - account for them.
            if FParser.TokenID in [tkCase, tkRecord, tkTry] then
              Inc(BeginCount);
            if FParser.TokenID = tkEnd then
            begin
              if BeginCount = 1 then
                ProcScanning := stScanned; // Found the last 'end'
              Dec(BeginCount);

              if (ProcScanning = stScanned) then
              begin
                FParser.NextNoJunk; // Move to the semicolon

                // Reached the end of the procedure so store it.
                if Assigned(ProcedureItem) then
                begin
                  ProcedureItem.EndIndex := FParser.TokenPos + 3; // +3 is for ';CRLF'
                  ProcScanning := stOutofProcedure;
                  ProcedureItem := nil;
                end;

                if (ProcedureStack.Count > 0) then
                begin
                  // If Stack containes procedures, retrieve them, since it might be
                  // that one's turn.
                  ProcedureItem := ProcedureStack.Pop;
                  ProcScanning := ProcedureItem.ScanningStatus;
                  BeginCount := 0;
                end; // ProcedureStack.Count > 0
              end; // ProcScanning = stScanned
            end; // FParser.TokenID = tkEnd
          end; // ProcScanning in [stInProcedure]
        end; // FParser.TokenID = tkSemiColon
      end; // Procedure body collection
    end; // FParser.TokenID <> tkNull

    // Store the last procedure
    if Assigned(ProcedureItem) then
    begin
      if not (ProcScanning in [stOutofProcedure]) then
        ProcedureItem.EndIndex := FParser.TokenPos;
    end;
  finally
    FProcedures.EndUpdate;
    FreeAndNil(ProcedureStack);
    FreeAndNil(FParser);
  end;
end;

function TPascal.MoveToImplementation: Boolean;
begin
  Result := False;
  while FParser.TokenID <> tkNull do
  begin
    if FParser.TokenID = tkImplementation then
    begin
      Result := True;
      Break;
    end;
    FParser.NextNoJunk;
  end;
end;

function TPascal.GetProperProcName(ProcType: TTokenKind; IsClass: Boolean): string;
begin
  Result := SUnknown;
  if IsClass then
  begin
    // Do not localize
    if ProcType = tkFunction then
      Result := 'Class Func'
    else if ProcType = tkProcedure then
      Result := 'Class Proc'
    else if ProcType = tkOperator then
      Result := 'Operator'
    else if ProcType = tkConstructor then
      Result := 'Class Constr';
  end
  else
  begin
    case ProcType of // Do not localize.
      tkFunction: Result := 'Function';
      tkProcedure: Result := 'Procedure';
      tkConstructor: Result := 'Constructor';
      tkDestructor: Result := 'Destructor';
      tkOperator: Result := 'Operator';
    end;
  end;
end;

{ TCpp }

procedure TCpp.Execute;
var
  LineNo: Integer;
  PreviousBraceCount: Integer;
  i, j: Integer;
  NewName, TmpName, ProcClassAdd, ClassName: string;
  BraceCountDelta: Integer;
  BeginProcHeaderPosition: Longint;
  ProcLine: string;
  ProcName, ProcReturnType: string;
  ProcedureType, ProcClass, ProcArgs: string;
  ProcedureItem: TProcedure;
  BeginIndex: Integer;
begin
  FProcedures.MemStream := FFileStream;
  FCParser := TBCBTokenList.Create;
  FCParser.SetOrigin(FFileStream.Memory, FFileStream.Size);
  try
    FNameList := TStringList.Create;
    try
      FBraceCount := 0;
      FNameList.Add('0='); // Empty enclosure name
      j := FCParser.TokenPositionsList[FCParser.TokenPositionsList.Count - 1];
      PreviousBraceCount := FBraceCount;
      FindBeginningProcedureBrace(NewName);

      while (FCParser.RunPosition <= j - 1) or (FCParser.RunID <> ctknull) do
      begin
        // If NewName = '' then we are looking at a real procedure - otherwise
        // we've just found a new enclosure name to add to our list
        if NewName = '' then
        begin
          // If we found a brace pair then special handling is necessary
          // for the bracecounting stuff (it is off by one)
          if FCParser.RunID = ctkbracepair then
            BraceCountDelta := 0
          else
            BraceCountDelta := 1;

          if (BraceCountDelta > 0) and (PreviousBraceCount >= FBraceCount) then
            EraseName(PreviousBraceCount);
          // Back up a tiny bit so that we are "in front of" the
          // ctkbraceopen or ctkbracepair we just found
          FCParser.Previous;

          while not ((FCParser.RunID in [ctkSemiColon, ctkbraceclose,
            ctkbraceopen, ctkbracepair]) or
              (FCParser.RunID in IdentDirect) or
            (FCParser.RunIndex = 0)) do
          begin
            FCParser.PreviousNonJunk;
            // Handle the case where a colon is part of a valid procedure definition
            if FCParser.RunID = ctkcolon then
            begin
              // A colon is valid in a procedure definition only if it is immediately
              // following a close parenthesis (possibly separated by "junk")
              FCParser.PreviousNonJunk;
              if FCParser.RunID in [ctkroundclose, ctkroundpair] then
                FCParser.NextNonJunk
              else
              begin
                // Restore position and stop backtracking
                FCParser.NextNonJunk;
                Break;
              end;
            end;
          end;

          if FCParser.RunID in [ctkcolon, ctkSemiColon, ctkbraceclose,
            ctkbraceopen, ctkbracepair] then
            FCParser.NextNonComment
          else if FCParser.RunIndex = 0 then
          begin
            if FCParser.IsJunk then
              FCParser.NextNonJunk;
          end
          else // IdentDirect
          begin
            while FCParser.RunID <> ctkcrlf do
            begin
              if (FCParser.RunID = ctknull) then
                Exit;
              FCParser.Next;
            end;
            FCParser.NextNonJunk;
          end;
          // We are at the beginning of procedure header
          BeginProcHeaderPosition := FCParser.RunPosition;

          ProcLine := '';
          while (FCParser.RunPosition < FBeginBracePosition) and
            (FCParser.RunID <> ctkcolon) do
          begin
            if (FCParser.RunID = ctknull) then
              Exit
            else if (FCParser.RunID <> ctkcrlf) then
              if (FCParser.RunID = ctkspace) and (FCParser.RunToken = #9) then
                ProcLine := ProcLine + #32
              else
                ProcLine := ProcLine + FCParser.RunToken;
            FCParser.NextNonComment;
          end;
          // We are at the end of a procedure header
          // Go back and skip parenthesis to find the procedure name
          ProcName := '';
          ProcClass := '';
          ProcReturnType := '';
          ProcArgs := SearchForProcedureName;
          // We have to check for ctknull and exit since we moved the
          // code to a nested procedure (if we exit SearchForProcedureName
          // early due to RunID = ctknull we exit this procedure early as well)
          if FCParser.RunID = ctknull then
            Exit;
          if FCParser.RunID = ctkthrow then
          begin
            ProcArgs := FCParser.RunToken + ProcArgs;
            ProcArgs := SearchForProcedureName + ProcArgs;
          end;
          // Since we've enabled nested procedures it is now possible
          // that we think we've found a procedure but what we've really found
          // is a standard C or C++ construct (like if or for, etc...)
          // To guard against this we require that our procedures be of type
          // ctkidentifier.  If not, then skip this step.
          if (FCParser.RunID = ctkidentifier) and not
            InProcedureBlacklist(FCParser.RunToken) then
          begin
            BeginIndex := FCParser.RunPosition;
            ProcName := FCParser.RunToken;
            LineNo := FCParser.PositionAtLine(FCParser.RunPosition);
            FCParser.PreviousNonJunk;
            if FCParser.RunID = ctkcoloncolon then
            // The object/method delimiter
            begin
              // There may be multiple name::name::name:: sets here
              // so loop until no more are found
              ClassName := '';
              while FCParser.RunID = ctkcoloncolon do
              begin
                FCParser.PreviousNonJunk; // The object name?
                // It is possible that we are looking at a templatized class and
                // what we have in front of the :: is the end of a specialization:
                // ClassName<x, y, z>::Function
                if FCParser.RunID = ctkGreater then
                  SearchForTemplateArgs;
                ProcClass := FCParser.RunToken + ProcClass;
                if ClassName = '' then
                  ClassName := FCParser.RunToken;
                FCParser.PreviousNonJunk; // look for another ::
                if FCParser.RunID = ctkcoloncolon then
                  ProcClass := FCParser.RunToken + ProcClass;
              end;
              // We went back one step too far so go ahead one
              FCParser.NextNonJunk;
              ProcedureType := 'Procedure';
              if ProcName = ClassName then
                ProcedureType := 'Constructor'
              else if ProcName = '~' + ClassName then
                ProcedureType := 'Destructor';
            end
            else
            begin
              ProcedureType := 'Procedure';
              // If type is a procedure is 1 then we have backed up too far already
              // so restore our previous position in order to correctly
              // get the return type information for non-class methods
              FCParser.NextNonJunk;
            end;

            while FCParser.RunPosition > BeginProcHeaderPosition do
            begin // Find the return type of the procedure
              FCParser.PreviousNonComment;
              // Handle the possibility of template specifications and
              // do not include them in the return type
              if FCParser.RunID = ctkGreater then
                SearchForTemplateArgs;
              if FCParser.RunID = ctktemplate then
                Continue;
              if FCParser.RunID in [ctkcrlf, ctkspace] then
                ProcReturnType := ' ' + ProcReturnType
              else
              begin
                ProcReturnType := FCParser.RunToken + ProcReturnType;
                BeginIndex := FCParser.RunPosition;
              end;
            end;
            // If the return type is an empty string then it must be a constructor
            // or a destructor (depending on the presence of a ~ in the name
            if (Trim(ProcReturnType) = '') or (Trim(ProcReturnType) = 'virtual') then
            begin
              if StrBeginsWith('~', ProcName) then
                ProcedureType := 'Destructor'
              else
                ProcedureType := 'Constructor';
            end;

            ProcLine := Trim(ProcReturnType) + ' ';

            // This code sticks enclosure names in front of
            // methods (namespaces & classes with in-line definitions)
            ProcClassAdd := '';
            for i := 0 to FBraceCount - BraceCountDelta do
            begin
              if i < FNameList.Count then
              begin
                TmpName := FNameList.Values[IntToStr(i)];
                if TmpName <> '' then
                begin
                  if ProcClassAdd <> '' then
                    ProcClassAdd := ProcClassAdd + '::';
                  ProcClassAdd := ProcClassAdd + TmpName;
                end;
              end;
            end;

            if Length(ProcClassAdd) > 0 then
            begin
              if Length(ProcClass) > 0 then
                ProcClassAdd := ProcClassAdd + '::';
              ProcClass := ProcClassAdd + ProcClass;
            end;
            if Length(ProcClass) > 0 then
              ProcLine := ProcLine + ' ' + ProcClass + '::';
            ProcLine := ProcLine + ProcName + ' ' + ProcArgs;

            if ProcedureType = 'Procedure' then
            begin
              if StrBeginsWith('static ', Trim(ProcReturnType)) and
                (Length(ProcClass) > 0) then
              begin
                if StrContains('void', ProcReturnType) then
                  ProcedureType := 'Class Proc'
                else
                  ProcedureType := 'Class Func'
              end
              else if not StrContains('void', ProcReturnType) then
                ProcedureType := 'Function';
            end;

            ProcedureItem := TProcedure.Create(FProcedures);
            ProcedureItem.ProcLine := ProcLine;
            ProcedureItem.ProcName := ProcName;
            ProcedureItem.ProcedureType := ProcedureType;
            ProcedureItem.LineNo := LineNo;
            ProcedureItem.ProcClass := ProcClass;
            ProcedureItem.ProcArgs := ProcArgs;
            ProcedureItem.ProcReturnType := ProcReturnType;
            ProcedureItem.ObjectSeparator := '::';

            while (FCParser.RunPosition < FBeginBracePosition) do
              FCParser.Next;

            ProcedureItem.BeginIndex := BeginIndex;
            FindEndingBrace(BraceCountDelta, (FBraceCount > 1));
            ProcedureItem.EndIndex := FCParser.RunPosition + 1;
          end
          else
            while (FCParser.RunPosition < FBeginBracePosition) do
              FCParser.Next;
        end
        else
        begin
          // Insert enclosure name into our list (delete the old one if found)
          EraseName(FBraceCount);
          FNameList.Add(IntToStr(FBraceCount) + '=' + NewName);
        end;
        PreviousBraceCount := FBraceCount;
        FindBeginningProcedureBrace(NewName);
      end; // while (RunPosition <= j-1)
    finally
      FreeAndNil(FNameList);
    end;
  finally
    FreeAndNil(FCParser);
  end;
end;

function TCpp.InProcedureBlacklist(const Name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(ProcBlacklist) to High(ProcBlacklist) do
  begin
    if Name = ProcBlacklist[i] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TCpp.EraseName(Index: Integer);
var
  NameIndex: Integer;
begin
  NameIndex := FNameList.IndexOfName(IntToStr(Index));
  if NameIndex <> -1 then
    FNameList.Delete(NameIndex);
end;

procedure TCpp.FindBeginningBrace;
begin
  repeat
    FCParser.NextNonJunk;
    case FCParser.RunID of
      ctkbraceopen: Inc(FBraceCount);
      ctkbraceclose: Dec(FBraceCount);
      ctknull: Exit;
    end;
  until FCParser.RunID in [ctkbraceopen, ctkbracepair, ctknull];
end;

procedure TCpp.FindEndingBrace(const BraceCountDelta: Integer; const DecrementOpenBrace: Boolean);
var
  BraceCount: Integer;
begin
  if DecrementOpenBrace then
    BraceCount := BraceCountDelta
  else
    BraceCount := 0;

  repeat
    FCParser.NextNonComment;
    case FCParser.RunID of
      ctkbraceopen: Inc(FBraceCount);
      ctkbraceclose: Dec(FBraceCount);
      ctknull: Exit;
    end;
  until ((FBraceCount - BraceCount) = 0) or
    (FCParser.RunID = ctknull);
end;

// This procedure does two things.  It looks for procedures and it
// looks for named scopes (like class/struct definitions & namespaces)
// If it finds a named scope it returns the non-blank name.  If it finds
// a procedure it returns a blank name.
procedure TCpp.FindBeginningProcedureBrace(var Name: string);
var
  InitialPosition: Integer;
  RestorePosition: Integer;
  FoundClass: Boolean;
begin
  FBeginBracePosition := 0;
  InitialPosition := FCParser.RunPosition;
  // Skip these: enum {a, b, c};  or  int a[] = {0, 3, 5};  and find  foo () {
  FindBeginningBrace;
  if FCParser.RunID = ctknull then
    Exit;
  FCParser.PreviousNonJunk;
  // Check for a namespace or a class name
  if FCParser.RunID = ctkidentifier then
  begin
    Name := FCParser.RunToken; // The name
    // This might be a derived class so search backward
    // no further than InitialPosition to see
    RestorePosition := FCParser.RunPosition;
    FoundClass := False;
    while FCParser.RunPosition >= InitialPosition do
    begin
      if FCParser.RunID in [ctkclass, ctkstruct, ctknamespace] then
      begin
        FoundClass := True;
        Break;
      end;
      if FCParser.RunPosition = InitialPosition then
        Break;
      FCParser.PreviousNonJunk;
    end;
    // The class name is the last token before a : or {
    if FoundClass then
    begin
      while not (FCParser.RunID in [ctkcolon, ctkbraceopen, ctknull]) do
      begin
        Name := FCParser.RunToken;
        FCParser.NextNonJunk;
      end;
      // Back up a bit if we are on a brace open so empty enums don't get treated as namespaces
      if FCParser.RunID = ctkbraceopen then
        FCParser.PreviousNonJunk;
    end;
    // Now get back to where you belong
    while FCParser.RunPosition < RestorePosition do
      FCParser.NextNonJunk;
    FCParser.NextNonJunk;
    FBeginBracePosition := FCParser.RunPosition;
  end
  else
  begin
    if FCParser.RunID in [ctkroundclose, ctkroundpair, ctkconst, ctkvolatile,
      ctknull] then
    begin
      // Return an empty name to indicate that a procedure was found
      Name := '';
      FCParser.NextNonJunk;
      FBeginBracePosition := FCParser.RunPosition;
    end
    else
    begin
      while not (FCParser.RunID in [ctkroundclose, ctkroundpair, ctkconst,
        ctkvolatile, ctknull]) do
      begin
        FCParser.NextNonJunk;
        if FCParser.RunID = ctknull then
          Exit;
        // Recurse
        FindBeginningProcedureBrace(Name);
        FCParser.PreviousNonJunk;
        if Name <> '' then
          Break;
      end;
      FCParser.NextNonJunk;
    end;
  end;
end;

// This function searches backward from the current parser position
// trying to find the procedure name - it returns all of the text
// between the starting position and the position where it thinks a
// procedure name has been found.
function TCpp.SearchForProcedureName: string;
var
  ParenCount: Integer;
begin
  ParenCount := 0;
  Result := '';
  repeat
    FCParser.Previous;
    if FCParser.RunID <> ctkcrlf then
      if (FCParser.RunID = ctkspace) and (FCParser.RunToken = #9) then
        Result := #32 + Result
      else
        Result := FCParser.RunToken + Result;
    case FCParser.RunID of
      ctkroundclose: Inc(ParenCount);
      ctkroundopen: Dec(ParenCount);
      ctknull: Exit;
    end;
  until ((ParenCount = 0) and ((FCParser.RunID = ctkroundopen) or
    (FCParser.RunID = ctkroundpair)));
  FCParser.PreviousNonJunk; // This is the procedure name
end;

function TCpp.SearchForTemplateArgs: string;
var
  AngleCount: Integer;
begin
  Result := '';
  if FCParser.RunID <> ctkGreater then
    Exit; // Only use if we are on a '>'
  AngleCount := 1;
  Result := FCParser.RunToken;
  repeat
    FCParser.Previous;
    if FCParser.RunID <> ctkcrlf then
      if (FCParser.RunID = ctkspace) and (FCParser.RunToken = #9) then
        Result := #32 + Result
      else
        Result := FCParser.RunToken + Result;
    case FCParser.RunID of
      ctkGreater: Inc(AngleCount);
      ctklower: Dec(AngleCount);
      ctknull: Exit;
    end;
  until (((AngleCount = 0) and (FCParser.RunID = ctklower)) or
    (FCParser.RunIndex = 0));
  FCParser.PreviousNonJunk; // This is the token before the template args
end;

{ TLanguage }

constructor TLanguage.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

end.

