unit GX_GrepBackend;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  ToolsAPI,
  GX_GrepRegExSearch, GX_GenericUtils;

type
  TGrepAction = (gaProjGrep, gaCurrentOnlyGrep, gaOpenFilesGrep, gaDirGrep, gaProjGroupGrep);

  // Saved grep settings (used for refresh)
  TGrepSettings = record
    IncludeComments: Boolean;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
    RegEx: Boolean;
    IncludeSubdirs: Boolean;
    Directories: string;
    Mask: string;
    Pattern: string;
    Replace: string;
    GrepAction: TGrepAction;
    CanRefresh: Boolean;
    IncludeForms: Boolean;
    ANSICompatible: Boolean;
  end;

type
  // Individual grep match in a line
  TMatchResult = class(TCollectionItem)
  private
    FSPos: Integer;
    FEPos: Integer;
    FShowBold: Boolean;
  public
    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
    property ShowBold: Boolean read FShowBold write FShowBold;
    constructor Create(Collection: TCollection); override;
    function Length: Integer;
  end;

  // Collection of TMatchResult
  // Collection of all matches in a line
  TLineMatches = class(TCollection)
  private
    function GetItem(Index: Integer): TMatchResult;
    procedure SetItem(Index: Integer; Value: TMatchResult);
  public
    constructor Create;
    function Add: TMatchResult;
    property Items[Index: Integer]: TMatchResult read GetItem write SetItem; default;
  end;

  // A single line that has a match from a file
  // One collection item per line with any number of matches
  TLineResult = class(TCollectionItem)
  private
    FLine: string;
    FLineNo: Integer; // 1-based
    FMatches: TLineMatches;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Add: TMatchResult;
  public
    property Line: string read FLine write FLine;
    property LineNo: Integer read FLineNo write FLineNo; // 1-based
    // Collection of all matches in a line
    property Matches: TLineMatches read FMatches;
  end;

  TMatchArray = array of TMatchResult;

  // Contains collection of all lines in a single source file that match.
  TFileResult = class(TCollection)
  private
    FExpanded: Boolean;
    FFileName: string;
    FRelativeFileName: string;
    FLastLineResult: Integer; // Last LineNo added to result set
    FLastIndex: Integer;      // Index of last added result
    FTotalMatches: Integer;   // Total matches in file
    function GetItem(Index: Integer): TLineResult;
    procedure SetItem(Index: Integer; Value: TLineResult);
  public
    constructor Create;
    function Add: TLineResult;
    procedure GetMatchesOnLine(Line: Integer; var Matches: TMatchArray);
    property Expanded: Boolean read FExpanded write FExpanded;
    property FileName: string read FFileName write FFileName;
    property RelativeFileName: string read FRelativeFileName write FRelativeFileName;
    property LastIndex: Integer read FLastIndex write FLastIndex;
    property LastLineResult: Integer read FLastLineResult write FLastLineResult;
    property Items[Index: Integer]: TLineResult read GetItem write SetItem; default;
    property TotalMatches: Integer read FTotalMatches write FTotalMatches;
  end;

type
  TOnHitMatch = procedure(Sender: TObject; LineNo: Integer; const Line: string;
      SPos, EPos: Integer) of object;
  TOnSearchFile = procedure(Sender: TObject; const FileName: string) of object;

  TGrepSearchRunner = class(TObject)
  private
    FOnHitMatch: TOnHitMatch;
    FOnSearchFile: TOnSearchFile;
    FStorageTarget: TStrings;
    FDupeFileList: TStrings;
    FAbortSignalled: Boolean;
    FFileSearchCount: Integer;
    FMatchCount: Integer;
    FFileResult: TFileResult;
    FSearcher: TSearcher;
    FSearchRoot: string;
    procedure FoundIt(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString);
    procedure StartFileSearch(const FileName: string);
    procedure ExecuteSearchOnFile(const FileName: string);
    procedure SearchFormForFile(const FileName: string);
  private
    FGrepSettings: TGrepSettings;
    procedure GrepFile(const FileName: string);
  protected
    procedure DoHitMatch(LineNo: Integer; const Line: string;
      SPos, EPos: Integer); virtual;
    procedure GrepCurrentSourceEditor;
    procedure GrepProjectGroup;
    procedure GrepProject(Project: IOTAProject);
    procedure GrepDirectory(Dir, Mask: string);
    procedure GrepDirectories(const Dir, Mask: string);
  public
    constructor Create(const Settings: TGrepSettings; StorageTarget: TStrings);
    procedure Execute;
    property OnSearchFile: TOnSearchFile read FOnSearchFile write FOnSearchFile;
    property FileSearchCount: Integer read FFileSearchCount;
    property MatchCount: Integer read FMatchCount;
    property AbortSignalled: Boolean read FAbortSignalled write FAbortSignalled;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Forms,
  GX_OtaUtils, GX_EditReader, GX_IdeUtils;

{ TLineMatches }

constructor TLineMatches.Create;
begin
  inherited Create(TMatchResult);
end;

function TLineMatches.Add: TMatchResult;
begin
  Result := TMatchResult(inherited Add);
end;

function TLineMatches.GetItem(Index: Integer): TMatchResult;
begin
  Result := TMatchResult(inherited GetItem(Index));
end;

procedure TLineMatches.SetItem(Index: Integer; Value: TMatchResult);
begin
  inherited SetItem(Index, Value);
end;

{ TLineResult }

constructor TLineResult.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FMatches := TLineMatches.Create;
end;

destructor TLineResult.Destroy;
begin
  if Assigned(FMatches) then
  begin
    FMatches.Clear;
    FreeAndNil(FMatches);
  end;
  inherited Destroy;
end;

function TLineResult.Add: TMatchResult;
begin
  Result := Matches.Add;
end;

{ TFileResult }

constructor TFileResult.Create;
begin
  inherited Create(TLineResult);
  FLastLineResult := -1;
  FTotalMatches := 0;
end;

function TFileResult.Add: TLineResult;
begin
  Result := TLineResult(inherited Add);
end;

function TFileResult.GetItem(Index: Integer): TLineResult;
begin
  Result := TLineResult(inherited GetItem(Index));
end;

procedure TFileResult.SetItem(Index: Integer; Value: TLineResult);
begin
  inherited SetItem(Index, Value);
end;

procedure TFileResult.GetMatchesOnLine(Line: Integer; var Matches: TMatchArray);
var
  i, j: Integer;
  LineMatches: TLineResult;
  MR: TMatchResult;
begin
  SetLength(Matches, 0);
  for i := 0 to Count - 1 do
  begin
    LineMatches := GetItem(i);
    if LineMatches.FLineNo = Line then
    begin
      for j := 0 to LineMatches.Matches.Count - 1 do
      begin
        SetLength(Matches, Length(Matches) + 1);
        MR := LineMatches.Matches.GetItem(j);
        Matches[Length(Matches) - 1] := MR;
      end;
    end;
  end;
end;

{ TGrepSearchRunner }

procedure TGrepSearchRunner.GrepFile(const FileName: string);
begin
  try
    Application.ProcessMessages;

    if IsBdsSourceFile(FileName) then
    begin
      if FDupeFileList.IndexOf(FileName) > -1 then
        Exit;
      FDupeFileList.Add(FileName);

      Assert(FFileResult = nil, 'FFileResult leak');
      FFileResult := nil;

      if (FGrepSettings.GrepAction = gaOpenFilesGrep) and (not GxOtaIsFileOpen(FileName)) then
        Exit;
      ExecuteSearchOnFile(FileName);
      FFileResult := nil;
    end;
  except
    on E: Exception do
      {$IFOPT D+} SendDebugError('GrepFile: ' + E.Message); {$ENDIF}
  end;
end;

constructor TGrepSearchRunner.Create(const Settings: TGrepSettings; StorageTarget: TStrings);
begin
  inherited Create;

  Assert(Assigned(StorageTarget));
  FStorageTarget := StorageTarget;
  FGrepSettings := Settings;
end;

procedure TGrepSearchRunner.GrepProjectGroup;
var
  i: Integer;
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GxOtaGetProjectGroup;
  if ProjectGroup = nil then
    Exit;
  FSearchRoot := ExtractFilePath(ProjectGroup.FileName);
  GrepFile(ProjectGroup.FileName);
  for i := 0 to ProjectGroup.ProjectCount - 1 do
    GrepProject(ProjectGroup.Projects[i]);
end;

procedure TGrepSearchRunner.GrepProject(Project: IOTAProject);
var
  i: Integer;
begin
  if Project = nil then
    Exit;

  FSearchRoot := ExtractFilePath(Project.FileName);
  GrepFile(GxOtaGetProjectFileName(Project, True));

  for i := 0 to Project.GetModuleCount - 1 do
  begin
    GrepFile(Project.GetModule(i).GetFileName);
    if FAbortSignalled then
      Break;
  end;
end;

type
  IGxGrepSearchAdapter = interface
  ['{2A8DD529-D06F-4CA7-83E5-E4880477BE31}']
    function GetText(Position: Longint; Buffer: PAnsiChar; Count: Longint): Longint;
  end;

type
  TGrepStreamAdapter = class(TInterfacedObject, IGxGrepSearchAdapter)
  private
    function GetText(Position: Longint; Buffer: PAnsiChar; Count: Longint): Longint;
  private
    FOwned: Boolean;
    FStream: TStream;
  public
    constructor Create(Stream: TStream; Owned: Boolean = False);
    destructor Destroy; override;
  end;

type
  TGrepOtaAdapter = class(TInterfacedObject, IGxGrepSearchAdapter)
  private
    function GetText(Position: Longint; Buffer: PAnsiChar; Count: Longint): Longint;
  private
    FEditReader: IOTAEditReader;
  public
    constructor Create(EditReader: IOTAEditReader);
  end;


procedure TGrepSearchRunner.GrepCurrentSourceEditor;
resourcestring
  SNoFileOpen = 'No file is currently open';
var
  CurrentFile: string;
begin
  if IsStandAlone then Exit;

  CurrentFile := GxOtaGetBaseModuleFileName(GxOtaGetCurrentSourceFile);

  Assert(FFileResult = nil, 'FFileResult leak');
  FFileResult := nil;

  FSearchRoot := ExtractFilePath(CurrentFile);
  if NotEmpty(CurrentFile) and (not FileIsWelcomePage(CurrentFile)) then
    ExecuteSearchOnFile(CurrentFile)
  else
    raise Exception.Create(SNoFileOpen);
end;

procedure TGrepSearchRunner.GrepDirectories(const Dir, Mask: string);
var
  i: Integer;
  DirList: TStringList;
begin
  DirList := TStringList.Create;
  try
    AnsiStrTok(Dir, ';', DirList);
    for i := 0 to DirList.Count - 1 do
    begin
      if FAbortSignalled then
        Break;
      FSearchRoot := DirList[i];
      GrepDirectory(DirList[i], Mask);
    end;
  finally
    FreeAndNil(DirList);
  end;
end;

procedure TGrepSearchRunner.GrepDirectory(Dir, Mask: string);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist';
var
  Search: TSearchRec;
  Result: Integer;
  Masks: TStrings;
  i: Integer;
  SearchFile: string;
begin
  {$IFOPT D+} SendDebug('DirGrep on: ' +Dir+'; Mask: '+Mask); {$ENDIF}
  Dir := AddSlash(Dir);
  if not DirectoryExists(Dir) then
    raise Exception.CreateFmt(SSpecifiedDirectoryDoesNotExist, [Dir]);

  Masks := TStringList.Create;
  try
    for i := 1 to Length(Mask) do
      if CharInSet(Mask[i], [';', ',']) then
        Mask[i] := #13;

    Masks.Text := Mask;

    if FGrepSettings.IncludeSubdirs then
    begin
      Result := FindFirst(Dir + AllFilesWildCard, faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) <> 0 then
          begin
            if (Search.Name <> '.') and (Search.Name <> '..') then
              GrepDirectory(Dir + Search.Name, Mask);
          end;
          if FAbortSignalled then
            Exit;
          Result := FindNext(Search);
        end;
      finally
        FindClose(Search);
      end;
    end;

    for i := 0 to Masks.Count-1 do
    begin
      if FAbortSignalled then
        Break;

      Result := FindFirst(Dir + Trim(Masks.Strings[i]), faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if (Search.Attr and faDirectory) <> 0 then
            Result := FindNext(Search)
          else
          begin
            Assert(FFileResult = nil, 'FFileResult leak');
            FFileResult := nil;

            SearchFile := Dir + Search.Name;
            ExecuteSearchOnFile(SearchFile);
            FFileResult := nil;

            if FAbortSignalled then
              Break;

            Result := FindNext(Search);
          end;
        end;
      finally
        FindClose(Search);
      end;
    end;
  finally
    FreeAndNil(Masks);
  end;
end;

procedure TGrepSearchRunner.Execute;
begin
  FFileSearchCount := 0;
  FMatchCount := 0;

  FSearcher := TSearcher.Create;
  try
    FSearcher.OnFound := FoundIt;
    //FSearcher.NoComments := FGrepSettings.NoComments;
    //FSearcher.IncludeForms := FGrepSettings.IncludeForms;
    FSearcher.CaseSensitive := FGrepSettings.CaseSensitive;
    FSearcher.WholeWord := FGrepSettings.WholeWord;
    FSearcher.RegularExpression := FGrepSettings.RegEx;
    FSearcher.Pattern := FGrepSettings.Pattern;

    FDupeFileList := TStringList.Create;
    try
    case FGrepSettings.GrepAction of
      gaProjGrep:
        GrepProject(GxOtaGetCurrentProject);
      gaProjGroupGrep:
        GrepProjectGroup;
      gaCurrentOnlyGrep:
        GrepCurrentSourceEditor;
      gaOpenFilesGrep:
        GrepProject(GxOtaGetCurrentProject);
      gaDirGrep:
        begin
          if Length(Trim(FGrepSettings.Mask)) = 0 then
          begin
            if GxOtaCurrentProjectIsNativeCpp then
              GrepDirectories(FGrepSettings.Directories, '*.cpp;*.hpp;*.h;*.pas;*.inc')
            else if GxOtaCurrentProjectIsCSharp then
              GrepDirectories(FGrepSettings.Directories, '*.cs')
            else
              GrepDirectories(FGrepSettings.Directories, '*.pas;*.dpr;*.inc')
          end
          else
            GrepDirectories(FGrepSettings.Directories, AnsiUpperCase(FGrepSettings.Mask));
        end;
    end;	// end case
    finally
      FreeAndNil(FDupeFileList);
    end;

  finally
    FreeAndNil(FSearcher);
  end;
end;

procedure TGrepSearchRunner.SearchFormForFile(const FileName: string);
var
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  FormFile: string;
begin
  Module := GxOtaGetModule(FileName);
  FormEditor := GxOtaGetFormEditorFromModule(Module);
  if Assigned(FormEditor) then
    ExecuteSearchOnFile(FormEditor.FileName)
  else
  begin
    FormFile := ChangeFileExt(FileName, '.dfm');
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.nfm');
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.xfm');
    if FileExists(FormFile) then
      ExecuteSearchOnFile(FormFile);
  end;
end;

procedure TGrepSearchRunner.ExecuteSearchOnFile(const FileName: string);
begin
  Assert(Assigned(FDupeFileList));
  if FDupeFileList.IndexOf(FileName) = -1 then
  begin
    StartFileSearch(FileName);
    FSearcher.FileName := FileName;
    FDupeFileList.Add(FileName);
    FSearcher.Execute;

    if FGrepSettings.IncludeForms and (IsPas(FileName) or IsCpp(FileName)) then
      SearchFormForFile(FileName);
  end;
end;

procedure TGrepSearchRunner.FoundIt(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString);
var
  ALineResult: TLineResult;
  AMatchResult: TMatchResult;
begin
  Inc(FMatchCount);

  // If this is the first match or the match is on a
  // different file then add a new TFileResult.
  if (FFileResult = nil) or (FFileResult.FileName <> FSearcher.FileName) then
  begin
    FFileResult := TFileResult.Create;
    FFileResult.FileName := FSearcher.FileName;
    FFileResult.RelativeFileName := StringReplace(FSearcher.FileName, FSearchRoot, '', [rfIgnoreCase]);
    FStorageTarget.AddObject(FSearcher.FileName, FFileResult);
  end;

  // If the match is not on the same line number as the
  // last match then add another TLineResult to the file's
  // result set.
  if FFileResult.LastLineResult <> LineNo then
  begin
    ALineResult := FFileResult.Add;
    ALineResult.Line := Line;
    ALineResult.LineNo := LineNo;

    // Save Index number and line number for next match
    FFileResult.LastIndex := FFileResult.Count-1;
    FFileResult.LastLineResult := LineNo;
  end
  else
  begin
    // If the match is on the same line then add the
    // match to the previous match line
    ALineResult := FFileResult[FFileResult.LastIndex];
  end;

  AMatchResult := ALineResult.Add;
  AMatchResult.SPos := StartCol;
  AMatchResult.EPos := EndCol;
  FFileResult.TotalMatches := FFileResult.TotalMatches + 1;
end;

procedure TGrepSearchRunner.StartFileSearch(const FileName: string);
begin
  Inc(FFileSearchCount);
  if Assigned(FOnSearchFile) then
    FOnSearchFile(Self, FileName);
end;

procedure TGrepSearchRunner.DoHitMatch(LineNo: Integer; const Line: string;
  SPos, EPos: Integer);
begin
  if Assigned(FOnHitMatch) then
    FOnHitMatch(Self, LineNo, Line, SPos, EPos);
end;

{ TGrepStreamAdapter }

constructor TGrepStreamAdapter.Create(Stream: TStream; Owned: Boolean);
begin
  inherited Create;

  Assert(Assigned(Stream));
  FStream := Stream;
  FOwned := Owned;
end;

destructor TGrepStreamAdapter.Destroy;
begin
  if FOwned then
    FreeAndNil(FStream);

  inherited Destroy;
end;

function TGrepStreamAdapter.GetText(Position: Integer; Buffer: PAnsiChar;
  Count: Integer): Longint;
begin
  FStream.Position := Position;
  Result := FStream.Read(Buffer^, Count);
end;

{ TGrepOtaAdapter }

constructor TGrepOtaAdapter.Create(EditReader: IOTAEditReader);
begin
  inherited Create;

  Assert(Assigned(EditReader));
  FEditReader := EditReader;
end;

function TGrepOtaAdapter.GetText(Position: Integer; Buffer: PAnsiChar;
  Count: Integer): Longint;
begin
  Result := FEditReader.GetText(Position, Buffer, Count);
end;

{ TMatchResult }

constructor TMatchResult.Create(Collection: TCollection);
begin
  inherited;
  ShowBold := True;
end;

function TMatchResult.Length: Integer;
begin
  Result := EPos - SPos + 1;
end;

end.
