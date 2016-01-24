{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepBackend;

{Grep history settings format: iniversion=2

ifmMulti: multi INI files mode
for autosave and autoload
[HistoryList]
GrepHistoryXX=ZZ
  where
    XX=KeyIndex
    ZZ=Order
LastIndex= last keyindex
[HistoryXX]
  where
    XX=KeyIndex
KeyIndex= the item's unique index
Multi INI file filenames:
Main: GrepHistory.ini
Item: HistoryXX.ini

ifmSingle: one INI file mode
only for Save, Save & Pint, Open
[HistoryList]
GrepHistoryCount=the count of the items
GrepHistoryAA=BB
  where
    AA=index number
    BB=search text
[HistoryAA]
  where
    AA=index number
INI file name:
"any filename".ini
}

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  ToolsAPI,
  RegExpr, IniFiles,
  GX_GrepRegExSearch, GX_GenericUtils;

type
  TGrepAction = (gaProjGrep, gaCurrentOnlyGrep, gaOpenFilesGrep, gaDirGrep, gaProjGroupGrep, gaResults);

  TGrepSearchState = (gssNormal, gssRefresh, gssRefreshAll, gssSearchAgain, gssSearchModifyOptions);

  // Saved grep settings (used for refresh)
  TGrepSettings = record
    IncludeComments: Boolean;
    CaseSensitive: Boolean;
    WholeWord: Boolean;
    RegEx: Boolean;
    IncludeSubdirs: Boolean;
    Directories: string;
    ExcludedDirs: string;
    Mask: string;
    Pattern: string;
    Replace: string;
    GrepAction: TGrepAction;
    CanRefresh: Boolean;
    IncludeForms: Boolean;
    IncludeSQLs: Boolean;
  end;

type
  TIniFileMode = (ifmSingle, ifmMulti);

  TGrepIniFile = class(TMemIniFile)
  end;

  // Individual grep match in a line
  TMatchResult = class(TCollectionItem)
  private
    FSPos: Integer;
    FEPos: Integer;
    FShowBold: Boolean;
  public
    class function SubKeyName: string;

    constructor Create(Collection: TCollection); override;
    function Length: Integer;
    procedure LoadFromIni(AIni: TCustomIniFile; ASection, ASubKey: String);
    procedure WriteToIni(AIni: TCustomIniFile; ASection, ASubKey: String);

    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
    property ShowBold: Boolean read FShowBold write FShowBold;
  end;

  // Collection of TMatchResult
  // Collection of all matches in a line
  TLineMatches = class(TCollection)
  private
    function GetItem(Index: Integer): TMatchResult;
    procedure SetItem(Index: Integer; Value: TMatchResult);
  public
    class function SubKeyName: string;

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
    procedure LoadFromIni(AIni: TCustomIniFile; ASection: String);
    procedure WriteToIni(AIni: TCustomIniFile; ASection: String);

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
    FExpandState: Boolean;
    FFileName: string;
    FRelativeFileName: string;
    FLastLineResult: Integer; // Last LineNo added to result set
    FLastIndex: Integer;      // Index of last added result
    FTotalMatches: Integer;   // Total matches in file
    function  GetItem(Index: Integer): TLineResult;
    procedure SetItem(Index: Integer; Value: TLineResult);
  public
    class function SubKeyName: string;

    constructor Create;
    function  Add: TLineResult;
    procedure GetMatchesOnLine(Line: Integer; var Matches: TMatchArray);
    procedure LoadFromIni(AIni: TCustomIniFile; ASection: String);
    procedure WriteToIni(AIni: TCustomIniFile; ASection: String);

    property Expanded: Boolean read FExpanded write FExpanded;
    property ExpandState: Boolean read FExpandState write FExpandState;
    property FileName: string read FFileName write FFileName;
    property RelativeFileName: string read FRelativeFileName write FRelativeFileName;
    property LastIndex: Integer read FLastIndex write FLastIndex;
    property LastLineResult: Integer read FLastLineResult write FLastLineResult;
    property Items[Index: Integer]: TLineResult read GetItem write SetItem; default;
    property TotalMatches: Integer read FTotalMatches write FTotalMatches;
  end;

  TGrepHistoryListItem = class
  private
    FKeyIndex: Integer;
    FResultList: TStringList;
    FTotalMatchCount: Integer;
    FGrepSettings: TGrepSettings;
    FChecked: Boolean;
    FLastSearchTime: TDateTime;
    FSaved: Boolean;
    function  GetSearchText: String;
    procedure ClearResultList;
    function  GetItemIni(AIni: TCustomIniFile; AIniMode: TIniFileMode; DoClear: Boolean): TCustomIniFile;
    procedure FreeItemIni(var AIni: TCustomIniFile; AIniMode: TIniFileMode);

    function  SubKeyIndexedName: String; //Used only in multi mode
  public
    class function SubKeyNameHistory: string;

    constructor Create(AGrepSettings: TGrepSettings);
    destructor Destroy; override;
    procedure View(AResultList: TStrings);
    procedure Update(AGrepSettings: TGrepSettings; DoClearResults, DoClearMatchCount: Boolean);
    procedure LoadFromIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; ASection: String);
    procedure WriteToIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; ASection: String);
    procedure RemoveFromSettings(AIni: TCustomIniFile; const BaseKey: String);

    property SearchText: String read GetSearchText;
    property GrepSettings: TGrepSettings read FGrepSettings;
    property ResultList: TStringList read FResultList;
    property TotalMatchCount: Integer read FTotalMatchCount write FTotalMatchCount;

    property KeyIndex: Integer read FKeyIndex;
    property Checked: Boolean read FChecked write FChecked;
    property LastSearchTime: TDateTime read FLastSearchTime;
    property Saved: Boolean read FSaved write FSaved;
  end;

  TGrepHistoryList = class(TStringList)
  private
    FEnabled: Boolean;
    FLastIndex: Integer;
    function  GetItems(AIndex: Integer): TGrepHistoryListItem;
    procedure ClearItems;
    function  SearchItem(AGrepSettings: TGrepSettings; var AHistoryItem: TGrepHistoryListItem): Integer;
    function  GetNextIndex: Integer;

    function  SubKeyNameGrepHistory(AKeyIndex: Integer): String; overload; //Used only in multi mode
  protected
  public
    class function KeyName: string;
    class function SubKeyNameGrepHistory: string; overload;
    class function SettingsFileName: string;

    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function  AddItem(AGrepSettings: TGrepSettings; AResultList: TStrings): Integer;
    procedure UpdateGrepSettings(AGrepSettings: TGrepSettings);

    function  LoadFromSettings(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
      AMainIniVersion: Integer; AIniMode: TIniFileMode; const BaseKey: String; DoClear: Boolean): Boolean;
    function  LoadItemFromIni(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile; AMainIniVersion: Integer): Integer;
    procedure SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
      const BaseKey: String); overload;
    procedure SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; const BaseKey: String;
      AHistoryIndex: Integer); overload;

    procedure RemoveFromSettings(const AIni: TCustomIniFile; const BaseKey: String; AHistoryIndex: Integer);
    procedure DeleteINIFiles(AMainINIFileName: String; AIniVersion, AHistoryIndex: Integer);

    procedure SetAllChecked(AChecked: Boolean);

    property Items[AIndex: Integer]: TGrepHistoryListItem read GetItems;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

type
  TOnHitMatch = procedure(Sender: TObject; LineNo: Integer; const Line: string;
      SPos, EPos: Integer) of object;
  TOnSearchFile = procedure(Sender: TObject; const FileName: string) of object;

  TGrepSearchContext = class(TObject)
  public
    Project: string;
    function ToString: string; {$ifdef GX_VER200_up} override {$else} virtual {$endif GX_VER200_up};
  end;

  TGrepSearchRunner = class(TObject)
  private
    FOnHitMatch: TOnHitMatch;
    FOnSearchFile: TOnSearchFile;
    FStorageTarget: TStrings;
    FDupeFileList: TStringList;
    FExceptionList: TStrings;
    FAbortSignalled: Boolean;
    FFileSearchCount: Integer;
    FMatchCount: Integer;
    FExcludedDirsRegEx: TRegExpr;
    FFileResult: TFileResult;
    FSearcher: TSearcher;
    FSearchRoot: string;
    FFilesInResults: TStrings;
    procedure FoundIt(LineNo, StartCol, EndCol: Integer; const Line: TGXUnicodeString);
    procedure StartFileSearch(const FileName: string);
    procedure ExecuteSearchOnFile(const FileName: string; Context: TGrepSearchContext; FromProject: Boolean = False);
    procedure SearchFormForFile(const FileName: string; Context: TGrepSearchContext);
  private
    FGrepSettings: TGrepSettings;
    procedure GrepProjectFile(const FileName: string; Context: TGrepSearchContext);
  protected
    procedure DoHitMatch(LineNo: Integer; const Line: string;
      SPos, EPos: Integer); virtual;
    procedure GrepCurrentSourceEditor;
    procedure GrepProjectGroup;
    procedure GrepProject(Project: IOTAProject);
    procedure GrepDirectory(Dir, Mask: string);
    procedure GrepDirectories(const Dir, Mask: string);
    procedure GrepResults;
  public
    constructor Create(const Settings: TGrepSettings; StorageTarget, FilesInResults: TStrings);
    procedure Execute;
    property OnSearchFile: TOnSearchFile read FOnSearchFile write FOnSearchFile;
    property FileSearchCount: Integer read FFileSearchCount;
    property MatchCount: Integer read FMatchCount;
    property AbortSignalled: Boolean read FAbortSignalled write FAbortSignalled;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Forms, Dialogs, Controls,
  GX_OtaUtils, GX_EditReader, GX_IdeUtils, GX_dzClassUtils;

const
  cIniSubKeyCount = 'Count';

{ TGrepSearchRunner }

procedure TGrepSearchRunner.GrepProjectFile(const FileName: string; Context: TGrepSearchContext);
begin
  try
    Application.ProcessMessages;

    if IsBdsSourceFile(FileName) or (FGrepSettings.IncludeSQLs and IsSQL(FileName)) then
    begin
      Assert(FFileResult = nil, 'FFileResult leak');
      FFileResult := nil;

      if (FGrepSettings.GrepAction = gaOpenFilesGrep) and (not GxOtaIsFileOpen(FileName)) then
        Exit;
      ExecuteSearchOnFile(FileName, Context, True);
      if IsCpp(FileName) and (FGrepSettings.GrepAction in [gaProjGrep, gaOpenFilesGrep, gaProjGroupGrep]) then
        if GxOtaFileOrModuleExists(ChangeFileExt(FileName, '.h')) then
          ExecuteSearchOnFile(ChangeFileExt(FileName, '.h'), Context, True);
      FFileResult := nil;
    end;
  except
    on E: Exception do
      {$IFOPT D+} SendDebugError('GrepFile: ' + E.Message); {$ENDIF}
  end;
end;

constructor TGrepSearchRunner.Create(const Settings: TGrepSettings; StorageTarget, FilesInResults: TStrings);
begin
  inherited Create;

  Assert(Assigned(StorageTarget));
  Assert(Assigned(FilesInResults));
  FStorageTarget := StorageTarget;
  FFilesInResults := FilesInResults;
  FGrepSettings := Settings;
end;

procedure TGrepSearchRunner.GrepProjectGroup;
var
  i: Integer;
  ProjectGroup: IOTAProjectGroup;
  Context: TGrepSearchContext;
begin
  ProjectGroup := GxOtaGetProjectGroup;
  if ProjectGroup = nil then
    Exit;
  FSearchRoot := ExtractFilePath(ProjectGroup.FileName);
  Context := TGrepSearchContext.Create;
  try
    GrepProjectFile(ProjectGroup.FileName, Context);
  finally
    FreeAndNil(Context);
  end;
  for i := 0 to ProjectGroup.ProjectCount - 1 do
    GrepProject(ProjectGroup.Projects[i]);
end;

procedure TGrepSearchRunner.GrepProject(Project: IOTAProject);
var
  i: Integer;
  Context: TGrepSearchContext;
begin
  if Project = nil then
    Exit;

  FSearchRoot := ExtractFilePath(Project.FileName);
  Context := TGrepSearchContext.Create;
  try
    Context.Project := GxOtaGetProjectFileName(Project, True);
    GrepProjectFile(Context.Project, Context);

    for i := 0 to Project.GetModuleCount - 1 do
    begin
      GrepProjectFile(Project.GetModule(i).GetFileName, Context);
      if FAbortSignalled then
        Break;
    end;
  finally
    FreeAndNil(Context);
  end;
end;

procedure TGrepSearchRunner.GrepCurrentSourceEditor;
resourcestring
  SNoFileOpen = 'No file is currently open';
var
  CurrentFile: string;
  Context: TGrepSearchContext;
begin
  if IsStandAlone then Exit;

  CurrentFile := GxOtaGetBaseModuleFileName(GxOtaGetCurrentSourceFile);

  Assert(FFileResult = nil, 'FFileResult leak');
  FFileResult := nil;

  FSearchRoot := ExtractFilePath(CurrentFile);
  if NotEmpty(CurrentFile) and (not FileIsWelcomePage(CurrentFile)) then
  begin
    Context := TGrepSearchContext.Create;
    try
      ExecuteSearchOnFile(CurrentFile, Context)
    finally
      FreeAndNil(Context);
    end;
  end
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

{$WARN SYMBOL_PLATFORM OFF}

procedure TGrepSearchRunner.GrepDirectory(Dir, Mask: string);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist';
var
  Search: TSearchRec;
  Result: Integer;
  Masks: TStrings;
  i: Integer;
  SearchFile: string;
  Context: TGrepSearchContext;
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

    if FGrepSettings.IncludeSQLs and (Masks.IndexOf('*.sql') = -1) then
      Masks.Add('*.sql');

    if FGrepSettings.IncludeSubdirs then
    begin
      Result := FindFirst(Dir + AllFilesWildCard, faAnyFile, Search);
      try
        while Result = 0 do
        begin
          if ((Search.Attr and faDirectory) <> 0) {$IFDEF GX_VER150_up} and ((Search.Attr and faSymLink) = 0) {$ENDIF} then
          begin
            if (Search.Name <> '.') and (Search.Name <> '..') then
            begin
              if IsEmpty(FGrepSettings.ExcludedDirs) or (not FExcludedDirsRegEx.Exec(Dir + Search.Name)) then
                GrepDirectory(Dir + Search.Name, Mask);
            end;
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
        Context := TGrepSearchContext.Create;
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
              Result := FindNext(Search)
            else
            begin
              Assert(FFileResult = nil, 'FFileResult leak');
              FFileResult := nil;

              // FindFirst matches *.pas~ with a wildcard of *.pas, so we correct for that here
              if WildcardCompare(Masks.Strings[i], Search.Name, True) then
              begin
                SearchFile := Dir + Search.Name;
                if IsEmpty(FGrepSettings.ExcludedDirs) or (not FExcludedDirsRegEx.Exec(SearchFile)) then
                  ExecuteSearchOnFile(SearchFile, Context);
              end;
              FFileResult := nil;

              if FAbortSignalled then
                Break;

              Result := FindNext(Search);
            end;
          end; // while
        finally
          FreeAndNil(Context);
        end; // finally
      finally
        FindClose(Search);
      end; // finally
    end;
  finally
    FreeAndNil(Masks);
  end; // finally
end;

procedure TGrepSearchRunner.GrepResults;
var
  i: Integer;
  Context: TGrepSearchContext;
begin
  Context := TGrepSearchContext.Create;
  try
    for i := 0 to FFilesInResults.Count - 1 do
    begin
      if GxOtaFileOrModuleExists(FFilesInResults[i]) then
      begin
        {$IFOPT D+} SendDebug('ResultsGrep on ' + FFilesInResults[i]); {$ENDIF}
        ExecuteSearchOnFile(FFilesInResults[i], Context);
      end;
    end;
  finally
    FreeAndNil(Context);
  end; // finally
end;

procedure TGrepSearchRunner.Execute;
var
  i: Integer;
  lExcludedDirs: string;
begin
  FFileSearchCount := 0;
  FMatchCount := 0;

  FExcludedDirsRegEx := TRegExpr.Create;
  try
    if NotEmpty(FGrepSettings.ExcludedDirs) then
    begin
      lExcludedDirs := Trim(FGrepSettings.ExcludedDirs);
      i := Length(lExcludedDirs);
      while (i > 0) and (lExcludedDirs[i] = ';') do
        Dec(i);
      SetLength(lExcludedDirs, i);
      lExcludedDirs := QuoteRegExprMetaChars(lExcludedDirs);
      FExcludedDirsRegEx.Expression := StringReplace(lExcludedDirs, ';', '|', [rfReplaceAll]);
      FExcludedDirsRegEx.ModifierI := True;
      try
        FExcludedDirsRegEx.Compile;
      except
        on E: Exception do
        begin
          E.Message := 'Invalid or empty item in directory exclusion list: ' + E.Message;
          raise;
        end;
      end;
    end;

    FSearcher := TSearcher.Create;
    try
      FSearcher.OnFound := FoundIt;
      FSearcher.NoComments := not FGrepSettings.IncludeComments;
      FSearcher.CaseSensitive := FGrepSettings.CaseSensitive;
      FSearcher.WholeWord := FGrepSettings.WholeWord;
      FSearcher.RegularExpression := FGrepSettings.RegEx;
      FSearcher.Pattern := FGrepSettings.Pattern;

      FDupeFileList := TStringList.Create;
      try
        FDupeFileList.Sorted := True;
        FExceptionList := TStringList.Create;
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
            gaResults:
              GrepResults;
          end;	// end case
          if FExceptionList.Count > 0 then
          begin
            if MessageDlg(FExceptionList.Text, mtError, [mbOK, mbCancel], 0) = mrCancel then
              Abort;
          end;
        finally
          FreeAndNil(FExceptionList);
        end;
      finally
        FreeAndNil(FDupeFileList);
      end;

    finally
      FreeAndNil(FSearcher);
    end;
  finally
    FreeAndNil(FExcludedDirsRegEx);
  end;
end;

procedure TGrepSearchRunner.SearchFormForFile(const FileName: string; Context: TGrepSearchContext);
var
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  FormFile: string;
begin
  FormEditor := nil;
  if RunningInsideIDE then
  begin
    Module := GxOtaGetModule(FileName);
    FormEditor := GxOtaGetFormEditorFromModule(Module);
  end;
  if Assigned(FormEditor) then
    ExecuteSearchOnFile(FormEditor.FileName, Context)
  else
  begin
    {TODO -o##jwp -cFix : When project has multiple forms using IFDEF, search them all}
    FormFile := ChangeFileExt(FileName, '.dfm');
    {TODO : Support firemonkey forms, maybe simlpy like this: }
//    if not FileExists(FormFile) then
//      FormFile := ChangeFileExt(FormFile, '.fmx'); // firemonkey
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.nfm'); // some dotnet format??
    if not FileExists(FormFile) then
      FormFile := ChangeFileExt(FormFile, '.xfm'); // clx
    if FileExists(FormFile) then
      ExecuteSearchOnFile(FormFile, Context);
  end;
end;

procedure TGrepSearchRunner.ExecuteSearchOnFile(const FileName: string; Context: TGrepSearchContext; FromProject: Boolean);
var
  TmpNoComments: Boolean;
  ContextString: string;
begin
  Assert(Assigned(FDupeFileList));
  if FDupeFileList.IndexOf(FileName) = -1 then
  begin
    StartFileSearch(FileName);
    try
      FSearcher.FileName := FileName;
      FDupeFileList.Add(FileName);

      // Because we can search directories and multiple extensions, and because the
      // ignore comments option is ignored with anything non Delphi, we may need to
      // turn it off temporarily, depending on what is searched for.
      TmpNoComments := FSearcher.NoComments;
      FSearcher.NoComments := ((FSearcher.NoComments) and (IsPascalSourceFile(FileName)));

      FSearcher.Execute;

      FSearcher.NoComments := TmpNoComments;
    except
      on E: Exception do
      begin
        if FromProject and (E is EGXFileNotFound) then
          E.Message := E.Message + '  Please check your dpr/dproj files and correct the path/filename referenced there.';
        ContextString := Context.ToString;
        if ContextString = '' then
          FExceptionList.Add(Format('Exception %s while searching "%s"; Message %s', [E.ClassName, FileName, E.Message]))
        else
          FExceptionList.Add(Format('Exception %s while searching "%s" in context "%s"; Message %s', [E.ClassName, FileName, ContextString, E.Message]));
        if not (E is EGXFileNotFound) then
          if MessageDlg(E.Message, mtError, [mbOK, mbCancel], 0) = mrCancel then
            Abort;
      end;
    end;

    if FGrepSettings.IncludeForms and (IsPas(FileName) or IsCpp(FileName)) then
      SearchFormForFile(FileName, Context);
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

{ TGrepSearchContext }

function TGrepSearchContext.ToString: string;
begin
  Result := '';
  if Project <> '' then
    Result := Format('Project: %s', [Project]);
{
For now, handle the Project.
In the future, try to handle these:

- GrepProjectGroup
- GrepProject
- GrepDirectories
- GrepProjectFile
- GrepCurrentSourceEditor
- GrepDirectory
- GrepResults
- SearchFormForFile
}
end;

{ TMatchResult }

class function TMatchResult.SubKeyName: string;
begin
  Result := 'Match';
end;

constructor TMatchResult.Create(Collection: TCollection);
begin
  inherited;
  ShowBold := True;
end;

function TMatchResult.Length: Integer;
begin
  Result := EPos - SPos + 1;
end;

procedure TMatchResult.LoadFromIni(AIni: TCustomIniFile; ASection, ASubKey: String);
begin
  SPos := AIni.ReadInteger(ASection, ASubKey + 'SPos', SPos);
  EPos := AIni.ReadInteger(ASection, ASubKey + 'EPos', EPos);
  ShowBold := AIni.ReadBool(ASection, ASubKey + 'ShowBold', ShowBold);
end;

procedure TMatchResult.WriteToIni(AIni: TCustomIniFile; ASection, ASubKey: String);
begin
  AIni.WriteInteger(ASection, ASubKey + 'SPos', SPos);
  AIni.WriteInteger(ASection, ASubKey + 'EPos', EPos);
  AIni.WriteBool(ASection, ASubKey + 'ShowBold', ShowBold);
end;

{ TLineMatches }

class function TLineMatches.SubKeyName: string;
begin
  Result := 'Line';
end;

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

procedure TLineResult.LoadFromIni(AIni: TCustomIniFile; ASection: String);
var
  I, ACount: Integer;
  ASubKey: String;
begin
  // INI file trims when reading back in, so a magic marker is used but we need to strip it when reading in.
  Line := Copy(AIni.ReadString(ASection, 'Line', '#' + Line), 2, MaxInt);
  LineNo := AIni.ReadInteger(ASection, 'LineNo', LineNo);

//MatchList
  ASubKey := TMatchResult.SubKeyName;
  Matches.Clear;
  ACount := AIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
  for I := 0 to ACount - 1 do
    Add.LoadFromIni(AIni, ASection, Format('%s%d', [ASubKey, I]));
end;

procedure TLineResult.WriteToIni(AIni: TCustomIniFile; ASection: String);
var
  I: Integer;
  ASubKey: String;
begin
  // INI file trims when reading back in, so a magic marker is used to keep the read contents.
  AIni.WriteString(ASection, 'Line', '#' + Line);
  AIni.WriteInteger(ASection, 'LineNo', LineNo);

//MatchList
  ASubKey := TMatchResult.SubKeyName;
  AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
  for I := 0 to Matches.Count - 1 do
    Matches.Items[I].WriteToIni(AIni, ASection, Format('%s%d', [ASubKey, I]));
  AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, Matches.Count);
end;

{ TFileResult }

class function TFileResult.SubKeyName: string;
begin
  Result := 'File';
end;

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

procedure TFileResult.LoadFromIni(AIni: TCustomIniFile; ASection: String);
var
  I, ACount: Integer;
  ASubKey: String;
begin
  ExpandState := AIni.ReadBool(ASection, 'ExpandState', False);
  FileName := AIni.ReadString(ASection, 'FileName', FileName);
  RelativeFileName := AIni.ReadString(ASection, 'RelativeFileName', RelativeFileName);
  LastLineResult := AIni.ReadInteger(ASection, 'LastLineResult', LastLineResult);
  LastIndex := AIni.ReadInteger(ASection, 'LastIndex', LastIndex);
  TotalMatches := AIni.ReadInteger(ASection, 'TotalMatches', TotalMatches);

//LineList
  ASubKey := TLineMatches.SubKeyName;
  Clear;
  ACount := AIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
  for I := 0 to ACount - 1 do
    Add.LoadFromIni(AIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
end;

procedure TFileResult.WriteToIni(AIni: TCustomIniFile; ASection: String);
var
  I: Integer;
  ASubKey: String;
begin
  AIni.WriteBool(ASection, 'ExpandState', ExpandState);
  AIni.WriteString(ASection, 'FileName', FileName);
  AIni.WriteString(ASection, 'RelativeFileName', RelativeFileName);
  AIni.WriteInteger(ASection, 'LastLineResult', LastLineResult);
  AIni.WriteInteger(ASection, 'LastIndex', LastIndex);
  AIni.WriteInteger(ASection, 'TotalMatches', TotalMatches);

//LineList
  ASubKey := TLineMatches.SubKeyName;
  AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
  for I := 0 to Count - 1 do
    Items[I].WriteToIni(AIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
  AIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, Count);
end;

{ TGrepHistoryListItem }

class function TGrepHistoryListItem.SubKeyNameHistory: string;
begin
  Result := 'History';
end;

constructor TGrepHistoryListItem.Create(AGrepSettings: TGrepSettings);
begin
  inherited Create;
  FKeyIndex := -1;
  FChecked := False;
  FLastSearchTime := 0;
  FSaved := True;
  FResultList := TStringList.Create;
  Update(AGrepSettings, False, True);
end;

destructor TGrepHistoryListItem.Destroy;
begin
  ClearResultList;
  FResultList.Free;
  inherited Destroy;
end;

procedure TGrepHistoryListItem.ClearResultList;
var
  I: Integer;
begin
  for I := 0 to FResultList.Count - 1 do
    FResultList.Objects[I].Free;
end;

function TGrepHistoryListItem.GetSearchText: String;
begin
  Result := FGrepSettings.Pattern;
end;

function TGrepHistoryListItem.SubKeyIndexedName: String;
begin
  Result := Format('%s%d', [SubKeyNameHistory, FKeyIndex]);
end;

procedure TGrepHistoryListItem.View(AResultList: TStrings);
var
  I: Integer;
begin
  for I := 0 to FResultList.Count - 1 do
    AResultList.AddObject(FResultList[I], FResultList.Objects[I]);
end;

procedure TGrepHistoryListItem.Update(AGrepSettings: TGrepSettings; DoClearResults, DoClearMatchCount: Boolean);
begin
  FGrepSettings := AGrepSettings;
  if DoClearMatchCount then
    FTotalMatchCount := 0;
  if DoClearResults then
  begin
    ClearResultList;
    FResultList.Clear;
  end;
end;

function TGrepHistoryListItem.GetItemIni(AIni: TCustomIniFile; AIniMode: TIniFileMode; DoClear: Boolean): TCustomIniFile;
begin
  if (AIni is TGrepIniFile) and (AIniMode = ifmMulti) then
  begin
    Result := TGrepIniFile.Create(ExtractFilePath(AIni.FileName) + SubKeyIndexedName + ExtractFileExt(AIni.FileName));
    if DoClear then
      TGrepIniFile(Result).Clear;
  end
  else
    Result := AIni;
end;

procedure TGrepHistoryListItem.FreeItemIni(var AIni: TCustomIniFile; AIniMode: TIniFileMode);
begin
  if (AIni is TGrepIniFile) and (AIniMode = ifmMulti) then
    FreeAndNil(AIni);
end;

procedure TGrepHistoryListItem.LoadFromIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; ASection: String);
var
  I, ACount: Integer;
  ASubKey: String;
  AFileItem: TFileResult;
  AItemIni: TCustomIniFile;
begin
  AItemIni := GetItemIni(AIni, AIniMode, False);
  try
    if AIniMode = ifmMulti then
      FKeyIndex := AItemIni.ReadInteger(ASection, 'KeyIndex', FKeyIndex);

    FLastSearchTime := AItemIni.ReadDateTime(ASection, 'LastSearchTime', LastSearchTime);

    TotalMatchCount := AItemIni.ReadInteger(ASection, 'TotalMatchCount', TotalMatchCount);

  //GrepSettings
    FGrepSettings.IncludeComments := AItemIni.ReadBool(ASection, 'IncludeComments', GrepSettings.IncludeComments);
    FGrepSettings.CaseSensitive := AItemIni.ReadBool(ASection, 'CaseSensitive', GrepSettings.CaseSensitive);
    FGrepSettings.WholeWord := AItemIni.ReadBool(ASection, 'WholeWord', GrepSettings.WholeWord);
    FGrepSettings.RegEx := AItemIni.ReadBool(ASection, 'RegEx', GrepSettings.RegEx);
    FGrepSettings.IncludeSubdirs := AItemIni.ReadBool(ASection, 'IncludeSubdirs', GrepSettings.IncludeSubdirs);
    FGrepSettings.Directories := AItemIni.ReadString(ASection, 'Directories', GrepSettings.Directories);
    FGrepSettings.ExcludedDirs := AItemIni.ReadString(ASection, 'ExcludedDirs', GrepSettings.ExcludedDirs);
    FGrepSettings.Mask := AItemIni.ReadString(ASection, 'Mask', GrepSettings.Mask);
    FGrepSettings.Pattern := AItemIni.ReadString(ASection, 'Pattern', GrepSettings.Pattern);
    FGrepSettings.Replace := AItemIni.ReadString(ASection, 'Replace', GrepSettings.Replace);
    FGrepSettings.GrepAction := TGrepAction(AItemIni.ReadInteger(ASection, 'GrepAction', Integer(GrepSettings.GrepAction)));
    FGrepSettings.IncludeForms := AItemIni.ReadBool(ASection, 'IncludeForms', GrepSettings.IncludeForms);
    FGrepSettings.IncludeSQLs := AItemIni.ReadBool(ASection, 'IncludeSQLs', GrepSettings.IncludeSQLs);
    FGrepSettings.CanRefresh := True;

  //ResultList
    ASubKey := TFileResult.SubKeyName;
    ACount := AItemIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
    for I := 0 to ACount - 1 do
    begin
      AFileItem := TFileResult.Create;
      AFileItem.LoadFromIni(AItemIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
      ResultList.AddObject(AFileItem.FileName, AFileItem);
    end;
  finally
    FreeItemIni(AItemIni, AIniMode);;
  end;
end;

procedure TGrepHistoryListItem.WriteToIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
  ASection: String);
var
  I: Integer;
  ASubKey: String;
  AItemIni: TCustomIniFile;
begin
  AItemIni := GetItemIni(AIni, AIniMode, True);
  try
    if not (AItemIni is TGrepIniFile) then
      AItemIni.EraseSection(ASection);

    if AItemIni is TGrepIniFile then
      AItemIni.WriteInteger(ASection, 'IniVersion', AIniVersion);
    if AIniMode = ifmMulti then
      AItemIni.WriteInteger(ASection, 'KeyIndex', FKeyIndex);

    AItemIni.WriteDateTime(ASection, 'LastSearchTime', LastSearchTime);

    AItemIni.WriteInteger(ASection, 'TotalMatchCount', TotalMatchCount);

  //GrepSettings
    AItemIni.WriteBool(ASection, 'IncludeComments', GrepSettings.IncludeComments);
    AItemIni.WriteBool(ASection, 'CaseSensitive', GrepSettings.CaseSensitive);
    AItemIni.WriteBool(ASection, 'WholeWord', GrepSettings.WholeWord);
    AItemIni.WriteBool(ASection, 'RegEx', GrepSettings.RegEx);
    AItemIni.WriteBool(ASection, 'IncludeSubdirs', GrepSettings.IncludeSubdirs);
    AItemIni.WriteString(ASection, 'Directories', GrepSettings.Directories);
    AItemIni.WriteString(ASection, 'ExcludedDirs', GrepSettings.ExcludedDirs);
    AItemIni.WriteString(ASection, 'Mask', GrepSettings.Mask);
    AItemIni.WriteString(ASection, 'Pattern', GrepSettings.Pattern);
    AItemIni.WriteString(ASection, 'Replace', GrepSettings.Replace);
    AItemIni.WriteInteger(ASection, 'GrepAction', Integer(GrepSettings.GrepAction));
    AItemIni.WriteBool(ASection, 'IncludeForms', GrepSettings.IncludeForms);
    AItemIni.WriteBool(ASection, 'IncludeSQLs', GrepSettings.IncludeSQLs);

  //ResultList
    ASubKey := TFileResult.SubKeyName;
    AItemIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
    for I := 0 to ResultList.Count - 1 do
      TFileResult(ResultList.Objects[I]).WriteToIni(AItemIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
    AItemIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, ResultList.Count);
    AItemIni.UpdateFile;
  finally
    FreeItemIni(AItemIni, AIniMode);
  end;
end;

procedure TGrepHistoryListItem.RemoveFromSettings(AIni: TCustomIniFile; const BaseKey: String);
var
  ASubKey: String;
begin
  ASubKey := BaseKey + PathDelim + SubKeyIndexedName;
  AIni.EraseSection(ASubKey);
end;

{ TGrepHistoryList }

class function TGrepHistoryList.KeyName: string;
begin
  Result := 'HistoryList';    //Main
end;

class function TGrepHistoryList.SettingsFileName: string;
begin
  Result := 'GrepHistory.ini';
end;

class function TGrepHistoryList.SubKeyNameGrepHistory: string;
begin
  Result := 'GrepHistory';
end;

constructor TGrepHistoryList.Create;
begin
  inherited Create;
  FEnabled := True;
  FLastIndex := 0;
end;

destructor TGrepHistoryList.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

procedure TGrepHistoryList.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
end;

procedure TGrepHistoryList.Clear;
begin
  ClearItems;
  inherited Clear;
end;

function TGrepHistoryList.GetNextIndex: Integer;
begin
  Inc(FLastIndex);
  Result := FLastIndex;
end;

function TGrepHistoryList.SubKeyNameGrepHistory(AKeyIndex: Integer): String;
begin
  Result := Format('%s%d', [SubKeyNameGrepHistory, AKeyIndex])
end;

function TGrepHistoryList.GetItems(AIndex: Integer): TGrepHistoryListItem;
begin
  Result := TGrepHistoryListItem(Objects[AIndex]);
end;

function TGrepHistoryList.SearchItem(AGrepSettings: TGrepSettings; var AHistoryItem: TGrepHistoryListItem): Integer;
begin
  AHistoryItem := nil;
  for Result := 0 to Count - 1 do
  begin
    AHistoryItem := Items[Result];
    if not FEnabled and (Result = 0) then
      Break;
    if AnsiSameText(AHistoryItem.SearchText, AGrepSettings.Pattern) then
      Break;
    AHistoryItem := nil;
  end;
end;

function TGrepHistoryList.AddItem(AGrepSettings: TGrepSettings; AResultList: TStrings): Integer;
var
  AItem: TGrepHistoryListItem;
  I: Integer;
  AResultItem: TFileResult;
  IsUpdate: Boolean;
begin
  Result := SearchItem(AGrepSettings, AItem);
  IsUpdate := Assigned(AItem);

  if IsUpdate then
    AItem.Update(AGrepSettings, True, True)
  else
    AItem := TGrepHistoryListItem.Create(AGrepSettings);

  for I := 0 to AResultList.Count - 1 do
  begin
    AResultItem := TFileResult(AResultList.Objects[I]);
    AItem.ResultList.AddObject(AResultList[I], AResultItem);
    Inc(AItem.FTotalMatchCount, AResultItem.TotalMatches);
  end;

  if not IsUpdate then
    Result := AddObject(AItem.SearchText, AItem);
end;

procedure TGrepHistoryList.UpdateGrepSettings(AGrepSettings: TGrepSettings);
var
  AItem: TGrepHistoryListItem;
begin
  SearchItem(AGrepSettings, AItem);
  if Assigned(AItem) then
    AItem.Update(AGrepSettings, False, False)
end;

function TGrepHistoryList.LoadFromSettings(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
  AMainIniVersion: Integer; AIniMode: TIniFileMode; const BaseKey: String; DoClear: Boolean): Boolean;
var
  Key: String;
  AIniVersion: Integer;

  function LoadIniSingle: Boolean;
  var
    I, ACount, Added: Integer;
    AItemText, ASubKeyName, AItemSubKeyName: String;
    AItem: TGrepHistoryListItem;
  begin
    ASubKeyName := SubKeyNameGrepHistory;
    AItemSubKeyName := TGrepHistoryListItem.SubKeyNameHistory;
    if AIniVersion = 0 then
    begin
      Key := BaseKey + 'FoundList';
      ASubKeyName := 'GrepFound';
      AItemSubKeyName := 'Found';
    end;

    ACount := AIni.ReadInteger(Key, ASubKeyName + cIniSubKeyCount, 0);
    if DoClear then
      ClearItems;
    Added := 0;
    for I := 0 to ACount - 1 do
    begin
      AItemText := AIni.ReadString(Key, Format('%s%d', [ASubKeyName, I]), '');
      if Trim(AItemText) <> '' then
      begin
        AItem := TGrepHistoryListItem.Create(ADefGrepSettings);
        AItem.LoadFromIni(AIni, AIniVersion, ifmSingle, Key + PathDelim + Format('%s%d', [AItemSubKeyName, I]));
        if not DoClear and (IndexOf(AItem.GrepSettings.Pattern) <> -1) then
        begin
          AItem.Free;
          Continue;
        end;

        AddObject(AItemText, AItem);
        Inc(Added);
      end;
    end;
    Result := ACount <> Added;
  end;

  function LoadIniMulti: Boolean;
  var
    I, Added: Integer;
    AItems: TStringList;
    AKeyText: String;
    AItem: TGrepHistoryListItem;
  begin
    AItems := TStringList.Create;
    try
      AIni.ReadSectionValues(Key, AItems);
      for I := 0 to AItems.Count - 1 do
                     //order index to first                                 //keyindex
        AItems[I] := TStrings_ValueFromIndex(AItems, I) + '=' + Copy(AItems.Names[I], 12, MaxInt);
      AItems.Sort;
      if DoClear then
        ClearItems;
      Added := 0;
      for I := 0 to AItems.Count - 1 do
      begin
        AKeyText := TStrings_ValueFromIndex(AItems, I);
        if Trim(AKeyText) <> '' then
        begin
          AItem := TGrepHistoryListItem.Create(ADefGrepSettings);
          AItem.FKeyIndex := StrToIntDef(AKeyText, -1);
          AItem.LoadFromIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + TGrepHistoryListItem.SubKeyNameHistory + AKeyText);
          if not DoClear and (IndexOf(AItem.GrepSettings.Pattern) <> -1) then
          begin
            AItem.Free;
            Continue;
          end;

          AddObject(AItem.GrepSettings.Pattern, AItem);
          Inc(Added);
        end;
      end;
      Result := AItems.Count <> Added;
    finally
      AItems.Free;
    end;
  end;

begin
  Key := BaseKey + KeyName;

  if AIni is TGrepIniFile then
    AIniVersion := AIni.ReadInteger(Key, 'IniVersion', 0)
  else
    AIniVersion := AMainIniVersion;

  FLastIndex := AIni.ReadInteger(Key, 'LastIndex', 0);

  case AIniMode of
  ifmSingle:
    Result := LoadIniSingle;
  else // ifmMulti:
    Result := LoadIniMulti;
  end;
end;

function TGrepHistoryList.LoadItemFromIni(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
  AMainIniVersion: Integer): Integer;
var
  AItem: TGrepHistoryListItem;
  AItemSubKeyName: String;
  AIniVersion: Integer;
begin
  AItemSubKeyName := TGrepHistoryListItem.SubKeyNameHistory;

  if AIni is TGrepIniFile then
    AIniVersion := AIni.ReadInteger(KeyName, 'IniVersion', 0)
  else
    AIniVersion := AMainIniVersion;
  if AIniVersion = 0 then
    AItemSubKeyName := 'Found';

  AItem := TGrepHistoryListItem.Create(ADefGrepSettings);
  AItem.LoadFromIni(AIni, AIniVersion, ifmSingle, AItemSubKeyName);
  if IndexOf(AItem.GrepSettings.Pattern) <> -1 then
  begin
    AItem.Free;
    Result := -1;
    Exit;
  end;

  Result := AddObject(AItem.GrepSettings.Pattern, AItem)
end;

procedure TGrepHistoryList.SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
  const BaseKey: String);
var
  Key: String;

  procedure SaveIniSingle;
  var
    I: Integer;
  begin
    AIni.WriteInteger(Key, SubKeyNameGrepHistory + cIniSubKeyCount, 0);
    for I := 0 to Count - 1 do
    begin
      //This is old and single format
      AIni.WriteString(Key, Format('%s%d', [SubKeyNameGrepHistory, I]), Strings[I]);                                                    //for ordered read
      Items[I].WriteToIni(AIni, AIniVersion, ifmSingle, Key + PathDelim + Format('%s%d', [TGrepHistoryListItem.SubKeyNameHistory, I])); //data
    end;
    AIni.WriteInteger(Key, SubKeyNameGrepHistory + cIniSubKeyCount, Count);
  end;

  procedure SaveIniMulti;
  var
    I: Integer;
    AItem: TGrepHistoryListItem;
  begin
    for I := 0 to Count - 1 do
    begin
      AItem := Items[I];
      if AItem.KeyIndex = -1 then
        AItem.FKeyIndex := GetNextIndex;
      AIni.WriteString(Key, SubKeyNameGrepHistory(AItem.KeyIndex), Format('%5.5d', [I]));       //for ordered read
      AItem.WriteToIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + AItem.SubKeyIndexedName); //data
    end;
    AIni.WriteInteger(Key, 'LastIndex', FLastIndex);
  end;

begin
  Key := BaseKey + KeyName;

  AIni.EraseSection(Key);
  if AIni is TGrepIniFile then
    AIni.WriteInteger(Key, 'IniVersion', AIniVersion);

  case AIniMode of
    ifmSingle: SaveIniSingle;
    ifmMulti: SaveIniMulti;
  end;
  AIni.UpdateFile;
end;

procedure TGrepHistoryList.SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; const BaseKey: String;
  AHistoryIndex: Integer);
var
  Key: String;
  AItem: TGrepHistoryListItem;
  I: Integer;
begin
  if (AHistoryIndex = -1) or (Count = 1) then
  begin
    SaveToSettings(AIni, AIniVersion, ifmMulti, BaseKey);
    Exit;
  end;

  Key := BaseKey + KeyName;

  AItem := Items[AHistoryIndex];
  if AItem.KeyIndex = -1 then
    AItem.FKeyIndex := GetNextIndex;
  AItem.WriteToIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + AItem.SubKeyIndexedName);  //data

  AIni.WriteInteger(Key, 'LastIndex', FLastIndex);

  //write order list
  //csak akkor mindegyik ha uj ertek kerul a lista vegere, mert lehet, hogy torolt van a listaban
  for I := 0 to Count-1 do
    AIni.WriteString(Key, SubKeyNameGrepHistory(Items[I].KeyIndex), Format('%5.5d', [I])); //for ordered read
  AIni.UpdateFile;
end;

procedure TGrepHistoryList.RemoveFromSettings(const AIni: TCustomIniFile; const BaseKey: String; AHistoryIndex: Integer);
var
  Key: String;
  AItem: TGrepHistoryListItem;
begin
  Key := BaseKey + KeyName;
  if AHistoryIndex = -1 then
    AIni.EraseSection(Key)
  else
  begin
    AItem := Items[AHistoryIndex];
    AIni.DeleteKey(Key, SubKeyNameGrepHistory(AItem.KeyIndex));
    AItem.RemoveFromSettings(AIni, Key);
  end;
end;

procedure TGrepHistoryList.DeleteINIFiles(AMainINIFileName: String; AIniVersion, AHistoryIndex: Integer);
var
  I: Integer;
  AConfigPath, AExt: String;
  Settings: TCustomIniFile;
begin
  AConfigPath := ExtractFilePath(AMainINIFileName);
  AExt := ExtractFileExt(AMainINIFileName);
  if AHistoryIndex = -1 then
  begin
    DeleteFile(AMainINIFileName);
    if AIniVersion >= 2 then
      for I := 0 to Count-1 do
        DeleteFile(AConfigPath + Items[I].SubKeyIndexedName + AExt);
  end
  else
  begin
    Settings := TGrepIniFile.Create(AMainINIFileName);
    try
      Settings.DeleteKey(KeyName, SubKeyNameGrepHistory(Items[AHistoryIndex].FKeyIndex));
      Settings.UpdateFile;
    finally
      FreeAndNil(Settings);
    end;
    DeleteFile(AConfigPath + Items[AHistoryIndex].SubKeyIndexedName + AExt);
  end;
end;

procedure TGrepHistoryList.SetAllChecked(AChecked: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Items[I].Checked := AChecked;
end;

end.

