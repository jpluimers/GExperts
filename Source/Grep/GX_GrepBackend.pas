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

  TGrepSearchState = (gssNormal, gssRefresh, gssRefreshAll, gssRefreshSelected,
    gssSearchAgain, gssModifySearchSettings, gssSearchEmbedded);

  TGrepSaveOption = (gsoSaveSettingsAndResults, gsoOnlySaveSettings, gsoNoSave);

  TGrepOnlySaveSettingsAction = (gossaShowSearchWindow, gossaShowEmbeddedSearch,
    gossaAutoRefresh, gossaAutoRefreshDouble, gossaEmptyList);

  // Saved grep settings (used for refresh)
  TGrepSettings = record
    IncludeComments: Boolean;
    IncludeCode: Boolean;
    IncludeStrings: Boolean;
    SectionInterface: Boolean;
    SectionImplementation: Boolean;
    SectionInitialization: Boolean;
    SectionFinalization: Boolean;
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
    SaveOption: TGrepSaveOption;
  end;

type
  TIniFileMode = (ifmSingle, ifmMulti);

  TGrepIniFile = class(TMemIniFile)
  end;

  TGrepSelectResult = (gsrNoSelection, gsrSelectItems, gsrSelectAll);
  TGrepSelectType = (gstDelete, gstSave, gstPrint, gstSavePrint, gstRefresh, gstSaveOptions, gstOpen, gstSort, gstSearchInHistory);

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
    procedure LoadFromIni(AIni: TCustomIniFile; const ASection, ASubKey: String);
    procedure WriteToIni(AIni: TCustomIniFile; const ASection, ASubKey: String);

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
    procedure LoadFromIni(AIni: TCustomIniFile; const ASection: String);
    procedure WriteToIni(AIni: TCustomIniFile; const ASection: String);

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
    function  LoadFromIni(AIni: TCustomIniFile; const ASection: String): Boolean;
    procedure WriteToIni(AIni: TCustomIniFile; const ASection: String);

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
    FFileCount: Integer;
    FGrepSettings: TGrepSettings;
    FChecked: Boolean;
    FLastSearchTime: TDateTime;
    FIsOnlySaveSettings: Boolean;
    FSaveOptionLoaded: Boolean;
    FSortIndex: Integer;

    procedure SetGrepSettingsSaveOption(const Value: TGrepSaveOption);
    function  GetFileCount: Integer;

    function  GetSearchText: String;
    class function KeyIndex2Text(AKeyIndex: Integer): String;
    function  GetKeyIndexText: String;
    procedure SetKeyIndexText(const Value: String);
    procedure ClearResultList;
    function  GetItemIni(AIni: TCustomIniFile; AIniMode: TIniFileMode; DoClear: Boolean): TCustomIniFile;
    procedure FreeItemIni(var AIni: TCustomIniFile; AIniMode: TIniFileMode);

    function  SubKeyIndexedName: String; //Used only in multi mode
  protected
  public
    class function SubKeyNameHistory: string;

    constructor Create(AGrepSettings: TGrepSettings);
    destructor Destroy; override;

    procedure View(AResultList: TStrings);
    procedure Update(AGrepSettings: TGrepSettings; DoClearResults, DoClearMatchCount: Boolean);

    procedure LoadFromIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; const ASection: String);
    procedure WriteToIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; const ASection: String);
    procedure RemoveFromSettings(AIni: TCustomIniFile; const BaseKey: String);

    property SearchText: String read GetSearchText;
    property GrepSettings: TGrepSettings read FGrepSettings;
    property GrepSettingsSaveOption: TGrepSaveOption read FGrepSettings.SaveOption write SetGrepSettingsSaveOption;
    property ResultList: TStringList read FResultList;
    property TotalMatchCount: Integer read FTotalMatchCount write FTotalMatchCount;
    property FileCount: Integer read GetFileCount;

    property KeyIndex: Integer read FKeyIndex;
    property KeyIndexText: String read GetKeyIndexText write SetKeyIndexText;
    property Checked: Boolean read FChecked write FChecked;
    property LastSearchTime: TDateTime read FLastSearchTime;
    property IsOnlySaveSettings: Boolean read FIsOnlySaveSettings;
    property SortIndex: Integer read FSortIndex write FSortIndex;
  end;

  TGrepHistorySort = (ghsUnsorted, ghsKeyIndex, ghsSearchText, ghsSearchTime, ghsSetSort);

  //Strings: KeyIndex
  //Objects: TGrepHistoryListItem
  //Sorted: False, use own sort method
  TGrepHistorySortableList = class(TStringList)
  private
    FSortMode: TGrepHistorySort;
    FSortDesc: Boolean;
    function  GetItem(AIndex: Integer): TGrepHistoryListItem;
    procedure SetSortDesc(const Value: Boolean);
    procedure SetSortMode(const Value: TGrepHistorySort);
    function  FindItem(const AItem: TGrepHistoryListItem; var AIndex: Integer): Boolean;
    function  CompareItems(AItem1, AItem2: TGrepHistoryListItem; AIndex1, AIndex2: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  Find(const S: string; var Index: Integer): Boolean; override;
    function  AddObject(const S: string; AObject: TObject): Integer; override;
    function  IndexOf(const S: string): Integer; override;

    function  AddItem(AItem: TGrepHistoryListItem; DoFind: Boolean = True): Integer;
    function  DeleteItem(AItem: TGrepHistoryListItem): Boolean;

    function  IndexByKeyIndex(const AKeyIndex: Integer): Integer;
    function  IndexOfItem(const AItem: TGrepHistoryListItem): Integer;
    function  ItemByKeyIndex(AKeyIndex: Integer): TGrepHistoryListItem;

    procedure Sort; override;
    procedure SortWithOptions(ASortMode: TGrepHistorySort; ASortDesc: Boolean);

    property Items[AIndex: Integer]: TGrepHistoryListItem read GetItem;
    property SortMode: TGrepHistorySort read FSortMode write SetSortMode;
    property SortDesc: Boolean read FSortDesc write SetSortDesc;
  end;

  TGrepHistoryListMode = (hlmResults, hlmSettings, hlmAll, hlmSearch);
  TGrepDeleteMode = (delOneItem, delSelected, delAll);

  TGrepHistoryList = class
  private
    FEnabled: Boolean;
    FLastIndex: Integer;
    FListMode: TGrepHistoryListMode;
    FHistoryList: array[TGrepHistoryListMode] of TGrepHistorySortableList;
    procedure SetListMode(const Value: TGrepHistoryListMode); //Used only in multi mode
    function  GetHistoryList(AIndex: TGrepHistoryListMode): TGrepHistorySortableList;
    function  GetCount: Integer;
    function  GetHistoryItems(AIndex: Integer): TGrepHistoryListItem;
    function  GetItems(AIndex: Integer): TGrepHistoryListItem;
    function  GetStrings(AIndex: Integer): String;
    procedure ClearItems;
    procedure DeleteItem(AItem: TGrepHistoryListItem; DoFree: Boolean = True; ACheckListMode: Boolean = True);
    function  SearchHistoryItem(AGrepSettings: TGrepSettings; var AHistoryItem: TGrepHistoryListItem): Integer;
    function  GetNextIndex: Integer;
    procedure WriteOrders(const AIni: TCustomIniFile; const Key: String; AIndex: Integer; AItem: TGrepHistoryListItem; AOnlySearchList: Boolean);

    function  SubKeyNameGrepHistory(AKeyIndex: Integer): String; overload;
    function GetSortDesc: Boolean;
    function GetSortMode: TGrepHistorySort;

    property ResultsList: TGrepHistorySortableList index hlmResults read GetHistoryList;
    property SettingsList: TGrepHistorySortableList index hlmSettings read GetHistoryList;
    property SearchList: TGrepHistorySortableList index hlmSearch read GetHistoryList;
  protected
    property HistoryItems[AIndex: Integer]: TGrepHistoryListItem read GetHistoryItems;
  public
    class function KeyName: string;
    class function SubKeyNameGrepHistory: string; overload;
    class function SettingsFileName: string;
    class function SubKeyNameResults: String;
    class function SubKeyNameSettings: String;
    class function SubKeyNameSearchList: String;

    constructor Create;
    destructor Destroy; override;

    function  AddItem(AGrepSettings: TGrepSettings; AResultList: TStrings; ASearchTime: TDateTime): Integer;
    procedure UpdateGrepSettings(AGrepSettings: TGrepSettings);
    procedure Delete(AItemIndex: Integer);
    procedure Clear;
    procedure DeleteSelected;
    function  MoveItems(AFromHistoryList: TGrepHistoryList; DoClear, DoOverWrite, AOnlyIfNewer: Boolean): Integer;
    function  HistoryItemByKeyIndex(AKeyIndex: Integer): TGrepHistoryListItem;
    function  ItemIndexByKeyIndex(AKeyIndex: Integer): Integer; overload;
    function  ItemIndexByKeyIndex(AKeyIndex: Integer; AListMode: TGrepHistoryListMode): Integer; overload;

    procedure  LoadFromSettings(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
      AMainIniVersion: Integer; AIniMode: TIniFileMode; const BaseKey: String;
      ASaveOption: TGrepSaveOption; AMainHistoryList: TGrepHistoryList = nil);
    function  LoadItemFromIni(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
      AMainIniVersion: Integer; ASaveOption: TGrepSaveOption; AMainHistoryList: TGrepHistoryList): Integer;
    procedure SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
      const BaseKey: String; ASaveAll, AMoveIfEmpty, AEnabledDeleteAfter: Boolean; ADeleteAfterDays, ASplitCount: Integer); overload;
    procedure SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; const BaseKey: String;
      AItemIndex: Integer; AMoveIfEmpty, AEnabledDeleteAfter: Boolean; ADeleteAfterDays: Integer); overload;

    procedure RemoveFromSettings(const AIni: TCustomIniFile; const BaseKey: String; ADelMode: TGrepDeleteMode;
      AItemIndex: Integer);
    procedure DeleteINIFiles(const AMainINIFileName: String; ADelMode: TGrepDeleteMode; AIniVersion, AItemIndex: Integer);

    procedure ClearAllChecked;
    function  AnyChecked: Boolean;
    procedure ClearAllSortIndex;

    procedure SortWithOptions(ASortMode: TGrepHistorySort; ASortDesc: Boolean);

    procedure UpdateSearchList(DoClear: Boolean);
    procedure SaveSearchListToSettings(const AIni: TCustomIniFile; const BaseKey: String);

    property HistoryList: TGrepHistorySortableList index hlmAll read GetHistoryList;
    property ListMode: TGrepHistoryListMode read FListMode write SetListMode;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TGrepHistoryListItem read GetItems;
    property Strings[AIndex: Integer]: String read GetStrings; default;
    property Enabled: Boolean read FEnabled write FEnabled;
    property SortMode: TGrepHistorySort read GetSortMode;
    property SortDesc: Boolean read GetSortDesc;
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

function SaveOptionText(ASaveOption: TGrepSaveOption): String;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Forms, Dialogs, Controls, DateUtils, StrUtils, Math,
  GX_OtaUtils, GX_EditReader, GX_IdeUtils, GX_dzClassUtils, GX_GrepProgress;

const
  cIniSubKeyCount = 'Count';

function SaveOptionText(ASaveOption: TGrepSaveOption): String;
begin
  case ASaveOption of
    gsoSaveSettingsAndResults: Result := 'Save params and results';
    gsoOnlySaveSettings: Result := 'Only save parameters';
    gsoNoSave: Result := 'No save (temp)';
  end;
end;

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
      FSearcher.NoCode := not FGrepSettings.IncludeCode;
      FSearcher.NoStrings := not FGrepSettings.IncludeStrings;
      FSearcher.SectionInterface := FGrepSettings.SectionInterface;
      FSearcher.SectionImplementation := FGrepSettings.SectionImplementation;
      FSearcher.SectionInitialization := FGrepSettings.SectionInitialization;
      FSearcher.SectionFinalization := FGrepSettings.SectionFinalization;
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
          end; // end case
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
      FSearcher.IsPascalSourceFile := IsPascalSourceFile(FileName);

      FSearcher.Execute;
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

  function RemoveMatchingPart(AFileName: String): String;
  var
    I, ARemoveCount: Integer;
  begin
    ARemoveCount := 0;
    for I := 1 to Min(Length(AFileName), Length(FSearchRoot)) do
      if ( UpperCase(AFileName[I]) = UpperCase(FSearchRoot[I]) ) and (AFileName[I] = PathDelim) then
        ARemoveCount := I;
    Result := Copy(AFileName, ARemoveCount+1, MaxInt);
  end;

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
    FFileResult.RelativeFileName := RemoveMatchingPart(FSearcher.FileName);
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

procedure TMatchResult.LoadFromIni(AIni: TCustomIniFile; const ASection, ASubKey: String);
begin
  SPos := AIni.ReadInteger(ASection, ASubKey + 'SPos', SPos);
  EPos := AIni.ReadInteger(ASection, ASubKey + 'EPos', EPos);
  ShowBold := AIni.ReadBool(ASection, ASubKey + 'ShowBold', ShowBold);
end;

procedure TMatchResult.WriteToIni(AIni: TCustomIniFile; const ASection, ASubKey: String);
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

procedure TLineResult.LoadFromIni(AIni: TCustomIniFile; const ASection: String);
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

procedure TLineResult.WriteToIni(AIni: TCustomIniFile; const ASection: String);
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

function TFileResult.LoadFromIni(AIni: TCustomIniFile; const ASection: String): Boolean;
var
  I, ACount: Integer;
  ASubKey: String;
begin
  Result := False;
  FileName := AIni.ReadString(ASection, 'FileName', FileName);
  if Trim(FileName) = '' then
    Exit;

  ExpandState := AIni.ReadBool(ASection, 'ExpandState', False);
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
  Result := True;
end;

procedure TFileResult.WriteToIni(AIni: TCustomIniFile; const ASection: String);
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
  FIsOnlySaveSettings := False;
  FResultList := TStringList.Create;
  FFileCount := 0;
  FSaveOptionLoaded := False;
  FSortIndex := -1;
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

class function TGrepHistoryListItem.KeyIndex2Text(AKeyIndex: Integer): String;
begin
  Result := IntToStr(AKeyIndex);
end;

function TGrepHistoryListItem.GetKeyIndexText: String;
begin
  Result := KeyIndex2Text(FKeyIndex);
end;

procedure TGrepHistoryListItem.SetKeyIndexText(const Value: String);
begin
  FKeyIndex := StrToIntDef(Value, -1);
end;

function TGrepHistoryListItem.SubKeyIndexedName: String;
begin
  Result := Format('%s%d', [SubKeyNameHistory, FKeyIndex]);
end;

procedure TGrepHistoryListItem.View(AResultList: TStrings);
var
  I: Integer;
begin
  if FIsOnlySaveSettings then
  begin
    AResultList.Add(Format('Total match count = %d', [FTotalMatchCount]));
    AResultList.Add(Format('File count = %d', [FFileCount]));
    AResultList.Add(Format('Last search time = %s', [DateTimeToStr(FLastSearchTime)]));
  end
  else
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

procedure TGrepHistoryListItem.LoadFromIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode; const ASection: String);
var
  I, ACount: Integer;
  ASubKey, APattern: String;
  AFileItem: TFileResult;
  AItemIni: TCustomIniFile;

  function ReadAndCheckBool(AIndent: String; ADefault: Boolean): Boolean;
  begin
    Result := not AItemIni.ValueExists(ASection, AIndent) or AItemIni.ReadBool(ASection, AIndent, ADefault);
  end;

begin
  AItemIni := GetItemIni(AIni, AIniMode, False);
  try
    if AIniMode = ifmMulti then
      FKeyIndex := AItemIni.ReadInteger(ASection, 'KeyIndex', FKeyIndex);

    FLastSearchTime := AItemIni.ReadFloat(ASection, 'LastSearchTimeStamp', LastSearchTime);
    FLastSearchTime := AItemIni.ReadDateTime(ASection, 'LastSearchTime', LastSearchTime);

    FTotalMatchCount := AItemIni.ReadInteger(ASection, 'TotalMatchCount', TotalMatchCount);

  //GrepSettings
    FGrepSettings.CaseSensitive := AItemIni.ReadBool(ASection, 'CaseSensitive', GrepSettings.CaseSensitive);
    FGrepSettings.WholeWord := AItemIni.ReadBool(ASection, 'WholeWord', GrepSettings.WholeWord);
    FGrepSettings.RegEx := AItemIni.ReadBool(ASection, 'RegEx', GrepSettings.RegEx);
    FGrepSettings.IncludeSubdirs := AItemIni.ReadBool(ASection, 'IncludeSubdirs', GrepSettings.IncludeSubdirs);
    FGrepSettings.Directories := AItemIni.ReadString(ASection, 'Directories', GrepSettings.Directories);
    FGrepSettings.ExcludedDirs := AItemIni.ReadString(ASection, 'ExcludedDirs', GrepSettings.ExcludedDirs);
    FGrepSettings.Mask := AItemIni.ReadString(ASection, 'Mask', GrepSettings.Mask);

    if AItemIni.ValueExists(ASection, 'PatternEx') then
    begin
      APattern := AItemIni.ReadString(ASection, 'PatternEx', GrepSettings.Pattern);
      FGrepSettings.Pattern := Copy(APattern, 2, Length(APattern)-2);
    end
    else
    begin
      APattern := AItemIni.ReadString(ASection, 'Pattern', GrepSettings.Pattern);
      if (Length(APattern) > 2) and (LeftStr(APattern, 1)[1] = '#') and (RightStr(APattern, 1)[1] = '#') then
        FGrepSettings.Pattern := Copy(APattern, 2, Length(APattern)-2)
      else
        FGrepSettings.Pattern := APattern;
    end;

    FGrepSettings.Replace := AItemIni.ReadString(ASection, 'Replace', GrepSettings.Replace);
    FGrepSettings.GrepAction := TGrepAction(AItemIni.ReadInteger(ASection, 'GrepAction', Integer(GrepSettings.GrepAction)));
    FGrepSettings.IncludeForms := AItemIni.ReadBool(ASection, 'IncludeForms', GrepSettings.IncludeForms);
    FGrepSettings.IncludeSQLs := AItemIni.ReadBool(ASection, 'IncludeSQLs', GrepSettings.IncludeSQLs);

    FGrepSettings.IncludeCode := ReadAndCheckBool('IncludeCode', GrepSettings.IncludeCode);
    FGrepSettings.IncludeStrings := ReadAndCheckBool('IncludeStrings', GrepSettings.IncludeStrings);
    FGrepSettings.IncludeComments := ReadAndCheckBool('IncludeComments', GrepSettings.IncludeComments);

    FGrepSettings.SectionInterface := ReadAndCheckBool('Interface', GrepSettings.SectionInterface);
    FGrepSettings.SectionImplementation := ReadAndCheckBool('Implementation', GrepSettings.SectionImplementation);
    FGrepSettings.SectionInitialization := ReadAndCheckBool('Initialization', GrepSettings.SectionInitialization);
    FGrepSettings.SectionFinalization := ReadAndCheckBool('Finalization', GrepSettings.SectionFinalization);

    if AIniMode = ifmMulti then
    begin
      FSaveOptionLoaded := AItemIni.ValueExists(ASection, 'SaveOption');
      FGrepSettings.SaveOption := TGrepSaveOption(AItemIni.ReadInteger(ASection, 'SaveOption', Integer(gsoSaveSettingsAndResults)));
    end;
    FGrepSettings.CanRefresh := True;

    FIsOnlySaveSettings := FGrepSettings.SaveOption = gsoOnlySaveSettings;

  //ResultList
    ASubKey := TFileResult.SubKeyName;
    ACount := AItemIni.ReadInteger(ASection, ASubKey + cIniSubKeyCount, 0);
    case FGrepSettings.SaveOption of
      gsoSaveSettingsAndResults:
      begin
        for I := 0 to ACount - 1 do
        begin
          AFileItem := TFileResult.Create;
          if AFileItem.LoadFromIni(AItemIni, ASection + PathDelim + Format('%s%d', [ASubKey, I])) then
            ResultList.AddObject(AFileItem.FileName, AFileItem)
          else
            AFileItem.Free;
        end;
        if (ACount > 0) and (ResultList.Count = 0) then
        begin
          FFileCount := ACount;
          GrepSettingsSaveOption := gsoOnlySaveSettings;
        end;
      end;
      gsoOnlySaveSettings: FFileCount := ACount;
    end;
  finally
    FreeItemIni(AItemIni, AIniMode);;
  end;
end;

procedure TGrepHistoryListItem.WriteToIni(AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
  const ASection: String);
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
    AItemIni.WriteFloat(ASection, 'LastSearchTimeStamp', LastSearchTime);

    AItemIni.WriteInteger(ASection, 'TotalMatchCount', TotalMatchCount);

  //GrepSettings
    AItemIni.WriteBool(ASection, 'CaseSensitive', GrepSettings.CaseSensitive);
    AItemIni.WriteBool(ASection, 'WholeWord', GrepSettings.WholeWord);
    AItemIni.WriteBool(ASection, 'RegEx', GrepSettings.RegEx);
    AItemIni.WriteBool(ASection, 'IncludeSubdirs', GrepSettings.IncludeSubdirs);
    AItemIni.WriteString(ASection, 'Directories', GrepSettings.Directories);
    AItemIni.WriteString(ASection, 'ExcludedDirs', GrepSettings.ExcludedDirs);
    AItemIni.WriteString(ASection, 'Mask', GrepSettings.Mask);
    AItemIni.WriteString(ASection, 'PatternEx', '#' + GrepSettings.Pattern + '#');
    AItemIni.WriteString(ASection, 'Replace', GrepSettings.Replace);
    AItemIni.WriteInteger(ASection, 'GrepAction', Integer(GrepSettings.GrepAction));
    AItemIni.WriteBool(ASection, 'IncludeForms', GrepSettings.IncludeForms);
    AItemIni.WriteBool(ASection, 'IncludeSQLs', GrepSettings.IncludeSQLs);

    AItemIni.WriteBool(ASection, 'IncludeCode', GrepSettings.IncludeCode);
    AItemIni.WriteBool(ASection, 'IncludeStrings', GrepSettings.IncludeStrings);
    AItemIni.WriteBool(ASection, 'IncludeComments', GrepSettings.IncludeComments);

    AItemIni.WriteBool(ASection, 'Interface', GrepSettings.SectionInterface);
    AItemIni.WriteBool(ASection, 'Implementation', GrepSettings.SectionImplementation);
    AItemIni.WriteBool(ASection, 'Initialization', GrepSettings.SectionInitialization);
    AItemIni.WriteBool(ASection, 'Finalization', GrepSettings.SectionFinalization);

    if AIniMode = ifmMulti then
      AItemIni.WriteInteger(ASection, 'SaveOption', Integer(GrepSettings.SaveOption));

  //ResultList
    ASubKey := TFileResult.SubKeyName;
    if ( (AIniMode = ifmMulti) and (GrepSettingsSaveOption = gsoSaveSettingsAndResults) ) or
      ( AIniMode = ifmSingle ) // and not FIsOnlySaveSettings
    then
    begin
      AItemIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, 0);
      for I := 0 to ResultList.Count - 1 do
        TFileResult(ResultList.Objects[I]).WriteToIni(AItemIni, ASection + PathDelim + Format('%s%d', [ASubKey, I]));
      AItemIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, ResultList.Count);
    end
    else if (AIniMode = ifmMulti) and (GrepSettingsSaveOption = gsoOnlySaveSettings) then
      AItemIni.WriteInteger(ASection, ASubKey + cIniSubKeyCount, FileCount);

    if (AItemIni is TGrepIniFile) and (AIniMode = ifmMulti) then
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

procedure TGrepHistoryListItem.SetGrepSettingsSaveOption(const Value: TGrepSaveOption);
begin
  FGrepSettings.SaveOption := Value;
end;

function TGrepHistoryListItem.GetFileCount: Integer;
begin
  if (GrepSettingsSaveOption = gsoOnlySaveSettings) and FIsOnlySaveSettings then
    Result := FFileCount
  else
    Result := FResultList.Count;
end;

{ TGrepHistorySortableList }

constructor TGrepHistorySortableList.Create;
begin
  inherited Create;
  FSortMode := ghsUnsorted;
  FSortDesc := False;
end;

destructor TGrepHistorySortableList.Destroy;
begin
  inherited Destroy;
end;

function TGrepHistorySortableList.GetItem(AIndex: Integer): TGrepHistoryListItem;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := TGrepHistoryListItem(Objects[AIndex])
  else
    Result := nil;
end;

procedure TGrepHistorySortableList.SetSortDesc(const Value: Boolean);
begin
  if FSortDesc <> Value then
  begin
    if FSortMode = ghsUnsorted then
      FSortDesc := False
    else
      FSortDesc := Value;
    Sort;
  end;
end;

procedure TGrepHistorySortableList.SetSortMode(const Value: TGrepHistorySort);
begin
  if FSortMode <> Value then
  begin
    FSortMode := Value;
    if FSortMode in [ghsUnsorted, ghsSetSort] then
      FSortDesc := False;
    Sort;
  end;
end;

function StringListCompareItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  ASortableList: TGrepHistorySortableList;
  AItem1, AItem2: TGrepHistoryListItem;
begin
  if Index1 = -1 then
  begin
    Result := -1;
    Exit;
  end
  else if Index2 = -1 then
  begin
    Result := 1;
    Exit;
  end;

  ASortableList := TGrepHistorySortableList(List);

  AItem1 := ASortableList.Items[Index1];
  AItem2 := ASortableList.Items[Index2];

  Result := ASortableList.CompareItems(AItem1, AItem2, Index1, Index2);
end;

function TGrepHistorySortableList.CompareItems(AItem1, AItem2: TGrepHistoryListItem; AIndex1, AIndex2: Integer): Integer;
var
  AItem: TGrepHistoryListItem;
  AIndex: Integer;
begin
  if SortDesc then
  begin
    AItem  := AItem1; AIndex  := AIndex1;
    AItem1 := AItem2; AIndex1 := AIndex2;
    AItem2 := AItem;  AIndex2 := AIndex;
  end;

  case SortMode of
    ghsKeyIndex: Result := CompareValue(AItem1.KeyIndex, AItem2.KeyIndex);
    ghsSearchText: Result := CompareStrings(AItem1.SearchText, AItem2.SearchText);
    ghsSearchTime: Result := CompareDateTime(AItem1.LastSearchTime, AItem2.LastSearchTime);
    ghsSetSort: Result := CompareValue(AItem1.SortIndex, AItem2.SortIndex);
  else  //ghsUnsorted
    Result := CompareValue(AIndex1, AIndex2)
  end;
end;

procedure TGrepHistorySortableList.Sort;
begin
  if SortMode <> ghsUnsorted then
    CustomSort(StringListCompareItems);

  if SortMode = ghsSetSort then
    FSortMode := ghsUnsorted;
end;

procedure TGrepHistorySortableList.SortWithOptions(ASortMode: TGrepHistorySort; ASortDesc: Boolean);
begin
  if (FSortDesc <> ASortDesc) or (FSortMode <> ASortMode) then
  begin
    FSortMode := ASortMode;
    if FSortMode in [ghsUnsorted, ghsSetSort] then
      FSortDesc := False
    else
      FSortDesc := ASortDesc;
    Sort;
  end;
end;

function TGrepHistorySortableList.Find(const S: string; var Index: Integer): Boolean;
begin
//  Result := inherited Find(S, Index);
  raise Exception.Create('Please, do not used Find!');
end;

function TGrepHistorySortableList.FindItem(const AItem: TGrepHistoryListItem; var AIndex: Integer): Boolean;
var
  L, H, I, C, ACount: Integer;
begin
  Result := False;
  ACount := Count;
  L := 0;
  H := ACount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareItems(Items[I], AItem, I, ACount);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        L := I;
        Result := True;
      end;
    end;
  end;
  AIndex := L;
end;

function TGrepHistorySortableList.AddObject(const S: string; AObject: TObject): Integer;
begin
//  Result := inherited AddObject(S, AObject);
  raise Exception.Create('Please, do not used AddObject!');
end;

function TGrepHistorySortableList.AddItem(AItem: TGrepHistoryListItem; DoFind: Boolean): Integer;
begin
//  Result := AddObject(AItem.SearchText, AItem);
  if DoFind then
  begin
    if FindItem(AItem, Result) then
      raise Exception.Create('Sortable list AddItem duplicate error.');
  end
  else
    Result := Count;

  InsertItem(Result, AItem.KeyIndexText, AItem);
end;

function TGrepHistorySortableList.IndexOf(const S: string): Integer;
begin
//  Result := inherited IndexOf(S);
  raise Exception.Create('Please, do not used IndexOf!');
end;

function TGrepHistorySortableList.IndexByKeyIndex(const AKeyIndex: Integer): Integer;
begin
  Result := inherited IndexOf(TGrepHistoryListItem.KeyIndex2Text(AKeyIndex));
end;

function TGrepHistorySortableList.IndexOfItem(const AItem: TGrepHistoryListItem): Integer;
begin
  if Assigned(AItem) then
    Result := inherited IndexOf(AItem.KeyIndexText)
  else
    Result := -1;
end;

function TGrepHistorySortableList.ItemByKeyIndex(AKeyIndex: Integer): TGrepHistoryListItem;
begin
  Result := Items[ IndexByKeyIndex(AKeyIndex) ]
end;

function TGrepHistorySortableList.DeleteItem(AItem: TGrepHistoryListItem): Boolean;
var
  I: Integer;
begin
  I := IndexOfItem(AItem);
  Result := I <> -1;
  if Result then
    Delete(I);
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

class function TGrepHistoryList.SubKeyNameResults: String;
begin
  Result := 'ResultsPageOrder';
end;

class function TGrepHistoryList.SubKeyNameSettings: String;
begin
  Result := 'ParamsPageOrder';
end;

class function TGrepHistoryList.SubKeyNameSearchList: String;
begin
  Result := 'SearchPageOrder';
end;

constructor TGrepHistoryList.Create;
var
  IM: TGrepHistoryListMode;
begin
  inherited Create;
  FListMode := hlmResults;
  FEnabled := True;
  FLastIndex := 0;
  for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
    FHistoryList[IM] := TGrepHistorySortableList.Create;
end;

destructor TGrepHistoryList.Destroy;
var
  IM: TGrepHistoryListMode;
begin
  ClearItems;
  for IM := High(TGrepHistoryListMode) downto Low(TGrepHistoryListMode) do
    FHistoryList[IM].Free;
  inherited Destroy;
end;

procedure TGrepHistoryList.ClearItems;
var
  I: Integer;
begin
  for I := 0 to HistoryList.Count - 1 do
    HistoryList.Objects[I].Free;
end;

procedure TGrepHistoryList.Clear;
var
  IM: TGrepHistoryListMode;
begin
  ClearItems;
  for IM := High(TGrepHistoryListMode) downto Low(TGrepHistoryListMode) do
    FHistoryList[IM].Clear;
end;

procedure TGrepHistoryList.DeleteItem(AItem: TGrepHistoryListItem; DoFree, ACheckListMode: Boolean);
var
  IM: TGrepHistoryListMode;
begin
  if ACheckListMode and (FListMode = hlmSearch) then
  begin
    SearchList.DeleteItem(AItem);
    Exit;
  end;

  for IM := High(TGrepHistoryListMode) downto Low(TGrepHistoryListMode) do
    FHistoryList[IM].DeleteItem(AItem);

  if DoFree then
    AItem.Free;
end;

procedure TGrepHistoryList.Delete(AItemIndex: Integer);
begin
  DeleteItem(Items[AItemIndex]);
end;

procedure TGrepHistoryList.DeleteSelected;
var
  I: Integer;
  AHistoryItem: TGrepHistoryListItem;
begin
  for I := HistoryList.Count-1 downto 0 do
  begin
    AHistoryItem := HistoryItems[I];
    if AHistoryItem.Checked then
      DeleteItem(AHistoryItem);
  end;
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

procedure TGrepHistoryList.SetListMode(const Value: TGrepHistoryListMode);
begin
  if FListMode <> Value then
  begin
    FListMode := Value;

  end;
end;

function TGrepHistoryList.GetHistoryList(AIndex: TGrepHistoryListMode): TGrepHistorySortableList;
begin
  Result := FHistoryList[AIndex];
end;

function TGrepHistoryList.GetCount: Integer;
begin
  Result := FHistoryList[FListMode].Count;
end;

function TGrepHistoryList.GetHistoryItems(AIndex: Integer): TGrepHistoryListItem;
begin
  Result := HistoryList.Items[AIndex];
end;

function TGrepHistoryList.GetItems(AIndex: Integer): TGrepHistoryListItem;
begin
  Result := FHistoryList[FListMode].Items[AIndex]; //FI:W517 - false positive
end;

function TGrepHistoryList.HistoryItemByKeyIndex(AKeyIndex: Integer): TGrepHistoryListItem;
begin
  Result :=  HistoryList.ItemByKeyIndex(AKeyIndex);
end;

function TGrepHistoryList.ItemIndexByKeyIndex(AKeyIndex: Integer): Integer;
begin
  Result := FHistoryList[FListMode].IndexByKeyIndex(AKeyIndex);
end;

function TGrepHistoryList.ItemIndexByKeyIndex(AKeyIndex: Integer; AListMode: TGrepHistoryListMode): Integer;
begin
  Result := FHistoryList[AListMode].IndexByKeyIndex(AKeyIndex);
end;

function TGrepHistoryList.GetStrings(AIndex: Integer): String;
var
  AItem: TGrepHistoryListItem;
begin
  AItem := Items[AIndex];
  if Assigned(AItem) then
    Result := AItem.SearchText
  else
    Result := '';
end;

function TGrepHistoryList.SearchHistoryItem(AGrepSettings: TGrepSettings; var AHistoryItem: TGrepHistoryListItem): Integer;
begin
  AHistoryItem := nil;
  for Result := 0 to HistoryList.Count - 1 do
  begin
    AHistoryItem := HistoryItems[Result];
    if not FEnabled and (Result = 0) then
      Break;
    if AnsiSameText(AHistoryItem.SearchText, AGrepSettings.Pattern) then
      Break;
    AHistoryItem := nil;
  end;
end;

function TGrepHistoryList.AddItem(AGrepSettings: TGrepSettings; AResultList: TStrings; ASearchTime: TDateTime): Integer;
var
  AItem: TGrepHistoryListItem;
  I, RI: Integer;
  AResultItem: TFileResult;
  IsUpdate, AddResults: Boolean;
begin
  Result := SearchHistoryItem(AGrepSettings, AItem);
  IsUpdate := Assigned(AItem);

  if IsUpdate then
    AItem.Update(AGrepSettings, True, True)
  else
  begin
    AItem := TGrepHistoryListItem.Create(AGrepSettings);
    AItem.FKeyIndex := GetNextIndex;
  end;

  for I := 0 to AResultList.Count - 1 do
  begin
    AResultItem := TFileResult(AResultList.Objects[I]);
    AItem.ResultList.AddObject(AResultList[I], AResultItem);
    Inc(AItem.FTotalMatchCount, AResultItem.TotalMatches);
  end;

  AItem.FLastSearchTime := ASearchTime;

  AddResults := False;
  if not IsUpdate then
  begin
    Result := HistoryList.AddItem(AItem);
    AddResults := True;
  end
  else
  begin
    if SettingsList.DeleteItem(AItem) then
    begin
      AddResults := True;
      AItem.FIsOnlySaveSettings := False;
      AItem.FFileCount := 0;
    end;
  end;

  RI := -1;
  if AddResults then
    RI := ResultsList.AddItem(AItem);

  if FListMode = hlmResults then
    if AddResults then
      Result := RI
    else
      Result := ResultsList.IndexOfItem(AItem)
  else if FListMode = hlmSearch then
    Result := SearchList.IndexOfItem(AItem)
end;

function TGrepHistoryList.MoveItems(AFromHistoryList: TGrepHistoryList; DoClear, DoOverWrite, AOnlyIfNewer: Boolean): Integer;
var
  FromI, RI, FI: Integer;
  AFromItem, AHistoryItem: TGrepHistoryListItem;
begin
  Result := -1;

  if DoClear then
    Clear;

  FromI := 0;
  while FromI < AFromHistoryList.Count do
  begin
    AFromItem := AFromHistoryList.Items[FromI];

    if AFromItem.Checked then
    begin
      SearchHistoryItem(AFromItem.GrepSettings, AHistoryItem);

      if not Assigned(AHistoryItem) or
        (DoOverWrite and (not AOnlyIfNewer or (AFromItem.LastSearchTime > AHistoryItem.LastSearchTime) ) )
      then
      begin
        FI := SearchList.IndexOfItem(AHistoryItem);
        if DoOverWrite then
          DeleteItem(AHistoryItem, True, False);

//        AFromItem.LastSearchTime := 0; ??? it may be needed

        AFromHistoryList.DeleteItem(AFromItem, False);
        Result := HistoryList.AddItem(AFromItem);
        RI := ResultsList.AddItem(AFromItem);
        if DoOverWrite and (FI <> -1) then
          SearchList.AddItem(AFromItem);

        if FListMode = hlmResults then
          Result := RI;
      end;
    end
    else
      Inc(FromI);
  end;
end;

procedure TGrepHistoryList.UpdateGrepSettings(AGrepSettings: TGrepSettings);
var
  AItem: TGrepHistoryListItem;
begin
  SearchHistoryItem(AGrepSettings, AItem);
  if Assigned(AItem) then
    AItem.Update(AGrepSettings, False, False)
end;

procedure TGrepHistoryList.LoadFromSettings(ADefGrepSettings: TGrepSettings;
  const AIni: TCustomIniFile; AMainIniVersion: Integer; AIniMode: TIniFileMode;
  const BaseKey: String; ASaveOption: TGrepSaveOption; AMainHistoryList: TGrepHistoryList);
var
  Key: String;
  AIniVersion: Integer;

  function LoadIniSingle: Boolean;
  var
    I, ACount, Added: Integer;
    AItemText, ASubKeyName, AItemSubKeyName: String;
    AItem, AHistoryItem: TGrepHistoryListItem;
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
    Added := 0;
    for I := 0 to ACount - 1 do
    begin
      AItemText := AIni.ReadString(Key, Format('%s%d', [ASubKeyName, I]), '');
      if Trim(AItemText) <> '' then
      begin
        AItem := TGrepHistoryListItem.Create(ADefGrepSettings);
        AItem.GrepSettingsSaveOption := gsoSaveSettingsAndResults;
        AItem.LoadFromIni(AIni, AIniVersion, ifmSingle, Key + PathDelim + Format('%s%d', [AItemSubKeyName, I]));
        AItem.GrepSettingsSaveOption := ASaveOption;

        SearchHistoryItem(AItem.GrepSettings, AHistoryItem);
        if Assigned(AHistoryItem) then
        begin
          AItem.Free;
          Continue;
        end;

        if Assigned(AMainHistoryList) then
        begin
          AMainHistoryList.SearchHistoryItem(AItem.GrepSettings, AHistoryItem);
          AItem.Checked := not Assigned(AHistoryItem);
        end
        else
          AItem.Checked := True;

        HistoryList.AddItem(AItem);
        ResultsList.AddItem(AItem);
        Inc(Added);
      end;
    end;
    Result := ACount <> Added;
  end;

  procedure SectionMoveToOrderList(ASection: TStringList);
  var
    I: Integer;
  begin
    for I := 0 to ASection.Count - 1 do
                   //order index to first                                 //keyindex
      ASection[I] := TStrings_ValueFromIndex(ASection, I) + '=' + Copy(ASection.Names[I], 12, MaxInt);
    ASection.Sort;
  end;

  function AddToList(AList: TGrepHistorySortableList; AItem: TGrepHistoryListItem; ALoadAllSO: Boolean; ASaveOption: TGrepSaveOption): Boolean;
  begin
    Result := ALoadAllSO or ( AItem.GrepSettingsSaveOption = ASaveOption );
    if Result and ( AList.IndexOfItem(AItem) = -1 ) then
      AList.AddItem(AItem, False);
  end;

  procedure CreateOrderedList(AKey: String; ASubList, ASubList2: TGrepHistorySortableList; ALoadAllSO: Boolean; ASaveOption, ASaveOption2: TGrepSaveOption);
  var
    AOrdersList: TStringList;
    I, AKeyIndex: Integer;
    AHistoryItem: TGrepHistoryListItem;
  begin
    AOrdersList := TStringList.Create;
    try
      ASubList.FSortMode := TGrepHistorySort(AIni.ReadInteger(AKey, 'SortMode', Integer(HistoryList.SortMode)));
      ASubList.FSortDesc := AIni.ReadBool(AKey, 'SortDesc', HistoryList.SortDesc);

      AIni.ReadSectionValues(AKey, AOrdersList);
      if AOrdersList.Count > 0 then
      begin
        SectionMoveToOrderList(AOrdersList);
        for I := 0 to AOrdersList.Count-1 do
        begin
          AKeyIndex := StrToIntDef(TStrings_ValueFromIndex(AOrdersList, I), -1);
          AHistoryItem := HistoryItemByKeyIndex(AKeyIndex);
          if Assigned(AHistoryItem) then
          begin
            if not AddToList(ASubList, AHistoryItem, ALoadAllSO, ASaveOption) and Assigned(ASubList2) then
              AddToList(ASubList2, AHistoryItem, False, ASaveOption2);
          end;
        end;
      end;

      if not ALoadAllSO then
        for I := 0 to HistoryList.Count-1 do
          AddToList(ASubList, HistoryItems[I], False, ASaveOption);
    finally
      AOrdersList.Free;
    end;
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
      HistoryList.FSortMode := TGrepHistorySort(AIni.ReadInteger(Key, 'SortMode', Integer(HistoryList.SortMode)));
      HistoryList.FSortDesc := AIni.ReadBool(Key, 'SortDesc', HistoryList.SortDesc);

      AIni.ReadSectionValues(Key, AItems);
      SectionMoveToOrderList(AItems);
      Added := 0;
      for I := 0 to AItems.Count - 1 do
      begin
        AKeyText := TStrings_ValueFromIndex(AItems, I);
        if Trim(AKeyText) <> '' then
        begin
          AItem := TGrepHistoryListItem.Create(ADefGrepSettings);

          AItem.KeyIndexText := AKeyText;
          if AItem.KeyIndex > FLastIndex then  //correction if needed
            FLastIndex := AItem.KeyIndex;

          AItem.LoadFromIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + TGrepHistoryListItem.SubKeyNameHistory + AKeyText);
          if HistoryList.IndexOfItem(AItem) <> -1 then
          begin
            AItem.Free;
            Continue;
          end;

          HistoryList.AddItem(AItem, False);
          Inc(Added);
        end;
      end;

      CreateOrderedList(Key + PathDelim + SubKeyNameSettings, SettingsList, nil, False, gsoOnlySaveSettings, gsoNoSave);
      CreateOrderedList(Key + PathDelim + SubKeyNameResults, ResultsList, SettingsList, False, gsoSaveSettingsAndResults, gsoOnlySaveSettings);
      CreateOrderedList(Key + PathDelim + SubKeyNameSearchList, SearchList, nil, True, gsoNoSave, gsoNoSave);

      for I := 0 to HistoryList.Count-1 do
      begin
        AItem := HistoryItems[I];
        if not AItem.FSaveOptionLoaded then
        begin
          AItem.GrepSettingsSaveOption := gsoOnlySaveSettings;
          AItem.FSaveOptionLoaded := True;
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
    ifmSingle: LoadIniSingle;
  else
    LoadIniMulti;
  end;
end;

function TGrepHistoryList.LoadItemFromIni(ADefGrepSettings: TGrepSettings; const AIni: TCustomIniFile;
  AMainIniVersion: Integer; ASaveOption: TGrepSaveOption; AMainHistoryList: TGrepHistoryList): Integer;
var
  AItem, AHistoryItem: TGrepHistoryListItem;
  AItemSubKeyName: String;
  AIniVersion, RI: Integer;
begin
  AItemSubKeyName := TGrepHistoryListItem.SubKeyNameHistory;

  if AIni is TGrepIniFile then
    AIniVersion := AIni.ReadInteger(KeyName, 'IniVersion', 0)
  else
    AIniVersion := AMainIniVersion;
  if AIniVersion = 0 then
    AItemSubKeyName := 'Found';

  AItem := TGrepHistoryListItem.Create(ADefGrepSettings);
  AItem.GrepSettingsSaveOption := gsoSaveSettingsAndResults;
  AItem.LoadFromIni(AIni, AIniVersion, ifmSingle, AItemSubKeyName);
  AItem.GrepSettingsSaveOption := ASaveOption;

  AMainHistoryList.SearchHistoryItem(AItem.GrepSettings, AHistoryItem);
  AItem.Checked := not Assigned(AHistoryItem);

  Result := HistoryList.AddItem(AItem);
  RI := ResultsList.AddItem(AItem);
  if FListMode = hlmResults then
    Result := RI;
end;

procedure TGrepHistoryList.WriteOrders(const AIni: TCustomIniFile; const Key: String; AIndex: Integer; AItem: TGrepHistoryListItem; AOnlySearchList: Boolean);
const
  cOrderFormat = '%5.5d';
var
  AIndentName: string;
  RI, SI, FI: Integer;
begin
  AIndentName := SubKeyNameGrepHistory(AItem.KeyIndex);
  if not AOnlySearchList then
  begin
    //for ordered read
    AIni.WriteString(Key, AIndentName, Format(cOrderFormat, [AIndex]));
    //for results, ordered read
    RI := ResultsList.IndexOfItem(AItem);
    if RI <> -1 then
      AIni.WriteString(Key + PathDelim + SubKeyNameResults, AIndentName, Format(cOrderFormat, [RI]));
    //for params, ordered read
    SI := SettingsList.IndexOfItem(AItem);
    if SI <> -1 then
      AIni.WriteString(Key + PathDelim + SubKeyNameSettings, AIndentName, Format(cOrderFormat, [SI]));
  end;
  //for SearchList, ordered read
  FI := SearchList.IndexOfItem(AItem);
  if FI <> -1 then
    AIni.WriteString(Key + PathDelim + SubKeyNameSearchList, AIndentName, Format(cOrderFormat, [FI]));
end;

procedure TGrepHistoryList.SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; AIniMode: TIniFileMode;
  const BaseKey: String; ASaveAll, AMoveIfEmpty, AEnabledDeleteAfter: Boolean; ADeleteAfterDays, ASplitCount: Integer);
var
  Key: String;

  procedure SaveIniSingle;
  var
    I, ASavedIndex: Integer;
    AItem: TGrepHistoryListItem;
  begin
    AIni.WriteInteger(Key, SubKeyNameGrepHistory + cIniSubKeyCount, 0);
    ASavedIndex := 0;
    for I := 0 to HistoryList.Count - 1 do
    begin
      AItem := HistoryItems[I];
      if ( ASaveAll and (ASplitCount = -1) ) or AItem.Checked then
      begin
        TfmGrepProgress.ProgressingDetail(AItem.SearchText);
        //This is old and single format
        AIni.WriteString(Key, Format('%s%d', [SubKeyNameGrepHistory, ASavedIndex]), AItem.SearchText);                                     //for ordered read
        AItem.WriteToIni(AIni, AIniVersion, ifmSingle, Key + PathDelim + Format('%s%d', [TGrepHistoryListItem.SubKeyNameHistory, ASavedIndex])); //data
        Inc(ASavedIndex);
        AItem.Checked := False;

        TfmGrepProgress.ProgressedDetail;
        if ASplitCount > 0 then
        begin
          Dec(ASplitCount);
          if ASplitCount = 0 then
            Break;
        end;
      end;
    end;
    AIni.WriteInteger(Key, SubKeyNameGrepHistory + cIniSubKeyCount, ASavedIndex);
  end;

  procedure SaveIniMulti;
  var
    I: Integer;
    AItem: TGrepHistoryListItem;
  begin
    for I := 0 to HistoryList.Count - 1 do
    begin
      AItem := HistoryItems[I];
      if AItem.GrepSettings.SaveOption <> gsoNoSave then
      begin
        //delete after deys
        if ( AEnabledDeleteAfter and (DaysBetween(Now, AItem.FLastSearchTime) >= ADeleteAfterDays) ) or
        //if results is empty, move to params page
          ( AMoveIfEmpty and (AItem.ResultList.Count = 0) )  // and (AItem.TotalMatchCount = 0)
        then
          AItem.GrepSettingsSaveOption := gsoOnlySaveSettings;

        if AItem.KeyIndex = -1 then
          AItem.FKeyIndex := GetNextIndex;
        WriteOrders(AIni, Key, I, AItem, False);

        AItem.WriteToIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + AItem.SubKeyIndexedName); //data
      end;
    end;
    AIni.WriteInteger(Key, 'LastIndex', FLastIndex);

    //sort by
    AIni.WriteInteger(Key, 'SortMode', Integer(HistoryList.SortMode));
    AIni.WriteInteger(Key + PathDelim + SubKeyNameResults, 'SortMode', Integer(ResultsList.SortMode));
    AIni.WriteInteger(Key + PathDelim + SubKeyNameSettings, 'SortMode', Integer(SettingsList.SortMode));
    AIni.WriteInteger(Key + PathDelim + SubKeyNameSearchList, 'SortMode', Integer(SearchList.SortMode));
    AIni.WriteBool(Key, 'SortDesc', HistoryList.SortDesc);
    AIni.WriteBool(Key + PathDelim + SubKeyNameResults, 'SortDesc', ResultsList.SortDesc);
    AIni.WriteBool(Key + PathDelim + SubKeyNameSettings, 'SortDesc', SettingsList.SortDesc);
    AIni.WriteBool(Key + PathDelim + SubKeyNameSearchList, 'SortDesc', SearchList.SortDesc);
  end;

begin
  Key := BaseKey + KeyName;

  if AIni is TGrepIniFile then
  begin
    TGrepIniFile(AIni).Clear;
    AIni.WriteInteger(Key, 'IniVersion', AIniVersion);
  end
  else
    AIni.EraseSection(Key);

  case AIniMode of
    ifmSingle: SaveIniSingle;
  else
    SaveIniMulti;
  end;
  AIni.UpdateFile;
end;

procedure TGrepHistoryList.SaveToSettings(const AIni: TCustomIniFile; AIniVersion: Integer; const BaseKey: String;
  AItemIndex: Integer; AMoveIfEmpty, AEnabledDeleteAfter: Boolean; ADeleteAfterDays: Integer);
var
  Key: String;
  AItem: TGrepHistoryListItem;
  I: Integer;
begin
  if (AItemIndex = -1) or (HistoryList.Count = 1) then
  begin
    SaveToSettings(AIni, AIniVersion, ifmMulti, BaseKey, False, AMoveIfEmpty, AEnabledDeleteAfter, ADeleteAfterDays, -1);
    Exit;
  end;

  AItem := Items[AItemIndex];
  if AItem.GrepSettings.SaveOption = gsoNoSave then
    Exit;

  Key := BaseKey + KeyName;

  if AItem.KeyIndex = -1 then
    AItem.FKeyIndex := GetNextIndex;
  AItem.WriteToIni(AIni, AIniVersion, ifmMulti, Key + PathDelim + AItem.SubKeyIndexedName);  //data

  AIni.WriteInteger(Key, 'LastIndex', FLastIndex);

  //sort by
  AIni.WriteInteger(Key, 'SortMode', Integer(HistoryList.SortMode));
  AIni.WriteInteger(Key + PathDelim + SubKeyNameResults, 'SortMode', Integer(ResultsList.SortMode));
  AIni.WriteInteger(Key + PathDelim + SubKeyNameSettings, 'SortMode', Integer(SettingsList.SortMode));
  AIni.WriteInteger(Key + PathDelim + SubKeyNameSearchList, 'SortMode', Integer(SearchList.SortMode));
  AIni.WriteBool(Key, 'SortDesc', HistoryList.SortDesc);
  AIni.WriteBool(Key + PathDelim + SubKeyNameResults, 'SortDesc', ResultsList.SortDesc);
  AIni.WriteBool(Key + PathDelim + SubKeyNameSettings, 'SortDesc', SettingsList.SortDesc);
  AIni.WriteBool(Key + PathDelim + SubKeyNameSearchList, 'SortDesc', SearchList.SortDesc);

  //write order list
  for I := 0 to HistoryList.Count-1 do
  begin
    AItem := HistoryItems[I];
    if AItem.GrepSettings.SaveOption <> gsoNoSave then
      WriteOrders(AIni, Key, I, AItem, False);
  end;
  AIni.UpdateFile;
end;

procedure TGrepHistoryList.RemoveFromSettings(const AIni: TCustomIniFile; const BaseKey: String;
  ADelMode: TGrepDeleteMode; AItemIndex: Integer);

  procedure ItemRemoveFromSettings(Key: String; AHistoryItem: TGrepHistoryListItem);
  begin
    AIni.DeleteKey(Key + PathDelim + SubKeyNameSearchList, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
    if FListMode <> hlmSearch then
    begin
      AIni.DeleteKey(Key, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
      AIni.DeleteKey(Key + PathDelim + SubKeyNameResults, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
      AIni.DeleteKey(Key + PathDelim + SubKeyNameSettings, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
      AHistoryItem.RemoveFromSettings(AIni, Key);
    end;
  end;

var
  Key: String;
  AHistoryItem: TGrepHistoryListItem;
  I: Integer;
begin
  Key := BaseKey + KeyName;
  case ADelMode of
    delAll: AIni.EraseSection(Key);
    delOneItem: ItemRemoveFromSettings(Key, Items[AItemIndex]);
    delSelected:
    begin
      for I := 0 to HistoryList.Count-1 do
      begin
        AHistoryItem := HistoryItems[I];
        if AHistoryItem.Checked then
          ItemRemoveFromSettings(Key, AHistoryItem);
      end;
    end;
  end;
  AIni.UpdateFile;
end;

procedure TGrepHistoryList.DeleteINIFiles(const AMainINIFileName: String; ADelMode: TGrepDeleteMode; AIniVersion, AItemIndex: Integer);

  procedure ItemDeleteINIFiles(Settings: TCustomIniFile; AHistoryItem: TGrepHistoryListItem; AConfigPath, AExt: String);
  begin
    Settings.DeleteKey(KeyName, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
    Settings.DeleteKey(KeyName + PathDelim + SubKeyNameResults, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
    Settings.DeleteKey(KeyName + PathDelim + SubKeyNameSettings, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));
    Settings.DeleteKey(KeyName + PathDelim + SubKeyNameSearchList, SubKeyNameGrepHistory(AHistoryItem.KeyIndex));

    DeleteFile(AConfigPath + AHistoryItem.SubKeyIndexedName + AExt);
  end;

var
  I: Integer;
  AConfigPath, AExt: String;
  Settings: TCustomIniFile;
  AHistoryItem: TGrepHistoryListItem;
begin
  AConfigPath := ExtractFilePath(AMainINIFileName);
  AExt := ExtractFileExt(AMainINIFileName);

  case ADelMode of
    delAll:
    begin
      DeleteFile(AMainINIFileName);
      if AIniVersion >= 2 then
        for I := 0 to HistoryList.Count-1 do
        begin
          AHistoryItem := HistoryItems[I];
          DeleteFile(AConfigPath + AHistoryItem.SubKeyIndexedName + AExt);
        end;
    end;
    delOneItem:
    begin
      Settings := TGrepIniFile.Create(AMainINIFileName);
      try
        ItemDeleteINIFiles(Settings, Items[AItemIndex], AConfigPath, AExt);
        Settings.UpdateFile;
      finally
        FreeAndNil(Settings);
      end;
    end ;
    delSelected:
    begin
      Settings := TGrepIniFile.Create(AMainINIFileName);
      try
        for I := 0 to HistoryList.Count-1 do
        begin
          AHistoryItem := HistoryItems[I];
          if AHistoryItem.Checked then
            ItemDeleteINIFiles(Settings, AHistoryItem, AConfigPath, AExt);
        end;
        Settings.UpdateFile;
      finally
        FreeAndNil(Settings);
      end;
    end;
  end;
end;

procedure TGrepHistoryList.ClearAllChecked;
var
  I: Integer;
begin
  for I := 0 to HistoryList.Count-1 do
    HistoryItems[I].Checked := False;
end;

function TGrepHistoryList.AnyChecked: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to HistoryList.Count-1 do
  begin
    Result := HistoryItems[I].Checked;
    if Result then
      Exit;
  end;
end;

procedure TGrepHistoryList.ClearAllSortIndex;
var
  I: Integer;
begin
  for I := 0 to HistoryList.Count-1 do
    HistoryItems[I].SortIndex := -1;
end;

procedure TGrepHistoryList.SortWithOptions(ASortMode: TGrepHistorySort; ASortDesc: Boolean);
begin
  FHistoryList[FListMode].SortWithOptions(ASortMode, ASortDesc);
end;

function TGrepHistoryList.GetSortDesc: Boolean;
begin
  Result := FHistoryList[FListMode].SortDesc;
end;

function TGrepHistoryList.GetSortMode: TGrepHistorySort;
begin
  Result := FHistoryList[FListMode].SortMode;
end;

procedure TGrepHistoryList.UpdateSearchList(DoClear: Boolean);
var
  I, ACount: Integer;
  AItem: TGrepHistoryListItem;
begin
  if DoClear then
    SearchList.Clear;

  ACount := SearchList.Count;
  for I := 0 to ACount-1 do
  begin
    AItem := SearchList.Items[I];
    AItem.SortIndex := I - ACount;
  end;

  for I := 0 to HistoryList.Count-1 do
  begin
    AItem := HistoryItems[I];
    if AItem.Checked and (DoClear or (SearchList.IndexOfItem(AItem) = -1)) then
      SearchList.AddItem(AItem, True);
  end;

  SearchList.SortWithOptions(ghsSetSort, False);
end;

procedure TGrepHistoryList.SaveSearchListToSettings(const AIni: TCustomIniFile; const BaseKey: String);
var
  I: Integer;
  AItem: TGrepHistoryListItem;
  Key: String;
begin
  Key := BaseKey + KeyName;

  //sort by
  AIni.WriteInteger(Key + PathDelim + SubKeyNameSearchList, 'SortMode', Integer(SearchList.SortMode));
  AIni.WriteBool(Key + PathDelim + SubKeyNameSearchList, 'SortDesc', SearchList.SortDesc);

  //write order list
  for I := 0 to HistoryList.Count-1 do
  begin
    AItem := HistoryItems[I];
    if AItem.GrepSettings.SaveOption <> gsoNoSave then
      WriteOrders(AIni, Key, I, AItem, True);
  end;
  AIni.UpdateFile;
end;

end.

