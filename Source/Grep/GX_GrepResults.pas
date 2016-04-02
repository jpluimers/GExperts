{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepResults;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, ActnList, Dialogs, StdCtrls, ExtCtrls, ToolWin,
  ComCtrls, Menus, Actions,
  DropSource, GX_GrepBackend, GX_GrepExpert, GX_ConfigurationInfo, GX_IdeDock, GX_GrepSearch;

type
  TPageIndexType = (pitClickedEntryKeyIndex, pitTopKeyIndex, pitClickedEntryItemIndex, pitTopItemIndex);
  TPageSavedIndexType = Low(TPageIndexType)..pitTopKeyIndex;
  TPageIndexes = array[TPageIndexType] of Integer;

  TfmGrepResults = class(TfmIdeDockForm)
    StatusBar: TStatusBar;
    lbResults: TListBox;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileSearch: TMenuItem;
    mitFileExit: TMenuItem;
    mitFilePrint: TMenuItem;
    mitList: TMenuItem;
    mitListContract: TMenuItem;
    mitListExpand: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFileAbort: TMenuItem;
    mitListGotoSelected: TMenuItem;
    mitFileRefresh: TMenuItem;
    mitViewStayOnTop: TMenuItem;
    mitHelpSep1: TMenuItem;
    Actions: TActionList;
    actFileSearch: TAction;
    actFileRefresh: TAction;
    actFileAbort: TAction;
    actFilePrint: TAction;
    actViewStayOnTop: TAction;
    actFileExit: TAction;
    actListGotoSelected: TAction;
    actListContract: TAction;
    actListExpand: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    mitHelpContents: TMenuItem;
    ToolBar: TToolBar;
    tbnSearch: TToolButton;
    tbnRefresh: TToolButton;
    tbnAbort: TToolButton;
    tbnSep2: TToolButton;
    tbnGoto: TToolButton;
    tbnSep3: TToolButton;
    tbnPrint: TToolButton;
    tbnSep4: TToolButton;
    tbnContract: TToolButton;
    tbnExpand: TToolButton;
    tbnSep1: TToolButton;
    tbnSep5: TToolButton;
    tbnStayOnTop: TToolButton;
    tbnSep6: TToolButton;
    tbnHelp: TToolButton;
    mitListSep1: TMenuItem;
    mitFileSep1: TMenuItem;
    reContext: TRichEdit;
    SplitterContext: TSplitter;
    mitViewSep1: TMenuItem;
    actViewShowContext: TAction;
    miViewShowMatchContext: TMenuItem;
    actFileSave: TAction;
    actFileCopy: TAction;
    mitFileSave: TMenuItem;
    mitFileCopy: TMenuItem;
    mitView: TMenuItem;
    mitViewSep3: TMenuItem;
    actViewToolBar: TAction;
    mitViewToolBar: TMenuItem;
    mitViewOptions: TMenuItem;
    actViewOptions: TAction;
    actReplaceAll: TAction;
    actReplaceSelected: TAction;
    tbnReplaceAll: TToolButton;
    tbnReplaceSelected: TToolButton;
    tbnSep7: TToolButton;
    mitReplace: TMenuItem;
    mitReplaceReplaceAll: TMenuItem;
    mitReplaceSelected: TMenuItem;
    mitListSep2: TMenuItem;
    pnlMain: TPanel;
    actListGotoSelectedAndClose: TAction;
    GotoSelectedandClose1: TMenuItem;
    lbHistoryList: TListBox;
    SplitterHistoryList: TSplitter;
    pmHistoryMenu: TPopupMenu;
    actHistoryView: TAction;
    actHistoryDelete: TAction;
    actHistoryRefresh: TAction;
    miHistoryView: TMenuItem;
    miHistoryRefresh: TMenuItem;
    mitHistorySep2: TMenuItem;
    miHistoryDelete: TMenuItem;
    mitHistorySep1: TMenuItem;
    miHistoryItemName: TMenuItem;
    actViewShowHistoryList: TAction;
    actViewShowFullFilename: TAction;
    miViewShowHistoryList: TMenuItem;
    mitViewSep2: TMenuItem;
    miViewShowFullFilename: TMenuItem;
    actHistoryDeleteSelected: TAction;
    pmContextMenu: TPopupMenu;
    actContextSelSearch: TAction;
    miContextSearchSelectedText: TMenuItem;
    actHistorySearch: TAction;
    miHistorySearch: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileSep4: TMenuItem;
    actFilePrintToFile: TAction;
    actFileSavePrint: TAction;
    actFileOpen: TAction;
    miFilePrintToFile: TMenuItem;
    miFileSavePrint: TMenuItem;
    mitFileOpen: TMenuItem;
    mitFileSep3: TMenuItem;
    OpenDialog: TOpenDialog;
    actHistoryRefreshSelected: TAction;
    mitFileSep5: TMenuItem;
    miFileRefreshSelected: TMenuItem;
    actHistoryModifySearchSettings: TAction;
    miHistoryModifySearchSettings: TMenuItem;
    tbnSep8: TToolButton;
    tbnShowFullFilename: TToolButton;
    mitFileDeleteSelected: TMenuItem;
    mitFileSep7: TMenuItem;
    miHistorySettings: TMenuItem;
    miSettingsCurrentFile: TMenuItem;
    miSettingsAllFilesInProjectGroup: TMenuItem;
    miSettingsAllFilesInProject: TMenuItem;
    miSettingsOpenProjectFiles: TMenuItem;
    miSettingsDirectories: TMenuItem;
    miSettingsPreviousSearchResultFiles: TMenuItem;
    miSettingsCaseSensitive: TMenuItem;
    miSettingsWholeWord: TMenuItem;
    miSettingsSearchFormFiles: TMenuItem;
    miSettingsSearchSQLFiles: TMenuItem;
    miSettingsRegularExpression: TMenuItem;
    miSettingsDirectoriesData: TMenuItem;
    miSettingsExcludeDirs: TMenuItem;
    miSettingsFileMasks: TMenuItem;
    miSettingsSearchSubDirectories: TMenuItem;
    tcHistoryListPage: TTabControl;
    actHistorySort: TAction;
    miSettingsSaveOption: TMenuItem;
    miSettingsSep1: TMenuItem;
    miSettingsSep2: TMenuItem;
    miSettingsSep3: TMenuItem;
    actHistoryModifySaveOptions: TAction;
    mitFileSep6: TMenuItem;
    mitFileModifySaveOptions: TMenuItem;
    miHistoryLastSearchTime: TMenuItem;
    actViewShowIndent: TAction;
    miViewShowIndent: TMenuItem;
    tbnShowLineIndent: TToolButton;
    mitHistorySep3: TMenuItem;
    miHistorySort: TMenuItem;
    actHistorySearchInHistory: TAction;
    mitFileSearhInHistory: TMenuItem;
    miSettingsSep4: TMenuItem;
    miSettingsGrepCode: TMenuItem;
    miSettingsGrepStrings: TMenuItem;
    miSettingsGrepComments: TMenuItem;
    miSettingsSectionInterface: TMenuItem;
    miSettingsSectionImplementation: TMenuItem;
    miSettingsSectionInitialization: TMenuItem;
    miSettingsSectionFinalization: TMenuItem;
    miSettingsSepDir: TMenuItem;
    tbnSearchInHistory: TToolButton;
    tbnSep9: TToolButton;
    procedure FormResize(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbResultsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actFileSearchExecute(Sender: TObject);
    procedure actFileRefreshExecute(Sender: TObject);
    procedure actFileAbortExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileCopyExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actViewStayOnTopExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actListGotoSelectedExecute(Sender: TObject);
    procedure actListContractExecute(Sender: TObject);
    procedure actListExpandExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure lbResultsClick(Sender: TObject);
    procedure actShowMatchContextExecute(Sender: TObject);
    procedure actViewToolBarExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure actReplaceSelectedExecute(Sender: TObject);
    procedure lbResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actListGotoSelectedAndCloseExecute(Sender: TObject);
    procedure lbHistoryListData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lbHistoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actHistoryViewExecute(Sender: TObject);
    procedure actHistoryRefreshExecute(Sender: TObject);
    procedure lbHistoryListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure lbHistoryListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure actHistoryDeleteExecute(Sender: TObject);
    procedure actViewShowHistoryListExecute(Sender: TObject);
    procedure actViewShowFullFilenameExecute(Sender: TObject);
    procedure actContextSelSearchExecute(Sender: TObject);
    procedure actHistoryDeleteSelectedExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actHistoryRefreshSelectedExecute(Sender: TObject);
    procedure lbHistoryListDblClick(Sender: TObject);
    procedure actHistoryUpdate(Sender: TObject);
    procedure tcHistoryListPageChange(Sender: TObject);
    procedure actHistorySortExecute(Sender: TObject);
    procedure lbHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure actHistoryModifySaveOptionsExecute(Sender: TObject);
    procedure actViewShowIndentExecute(Sender: TObject);
    procedure miHistoryItemNameClick(Sender: TObject);
    procedure lbHistoryListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actHistorySearchInHistoryExecute(Sender: TObject);
    procedure SplitterHistoryListMoved(Sender: TObject);
    procedure reContextContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FLastRepaintTick: DWORD;
    FSearchInProgress: Boolean;
    FReplaceInProgress: Boolean;
    FDragSource: TDropFileSource;
    FDragPoint: TPoint;
    FGrepSettings: TGrepSettings;
    FSearcher: TGrepSearchRunner;
    FShowContext: Boolean;
    FDoSearchReplace: Boolean;
    FShowFullFilename: Boolean;
    FShowLineIndent: Boolean;
    FShowHistoryList: Boolean;
    FLoadContextHeight: Integer;
    FLoadContextHeightPercent: Integer;
    FLoadHistoryListWidth: Integer;
    FLoadHistoryListPage: Integer;
    FContextSearchText: string;
    FPageIndexes: array[TGrepHistoryListMode] of TPageIndexes;
    FSavedLastSearchTimeCaption: String;
    FSavedSaveOptionCaption: String;
    FSavedDirectoriesDataCaption: String;
    FSavedExcludeDirsCaption: String;
    FSavedFileMasksCaption: String;
    FHistoryMousePos: TPoint;
    FSelectedCount: Integer;
    FSaveSplitCount: Integer;
    FEmbeddedGrepSearch: TfmGrepSearch;
    FNewSortMode: TGrepHistorySort;
    FNewSortDesc: Boolean;
    FSearchInClearSearchList: Boolean;
    FSaveItemEmptyCaption: String;
    FSaveSortCaption: String;
    FSavedFormCaption: String;
    procedure SetStayOnTop(Value: Boolean);
    procedure RefreshContextLines;
    procedure SetShowContext(Value: Boolean);
    procedure HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
    procedure StartFileSearch(Sender: TObject; const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ToggleFileResultExpanded(ListBoxIndex: Integer);
    procedure ExpandList(AUsedState, ASetState: Boolean; AExpandFewLines: Integer);
    procedure ContractList(ASetState: Boolean);
    procedure ResizeListBox;
    procedure GotoHighlightedListEntry;
    procedure ClearResultsListbox;
    function ShowModalForm(Dlg: TCustomForm): TModalResult;
    function QueryUserForGrepOptions(AState: TGrepSearchState): Boolean;
    function QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
    procedure Abort;
    procedure SetStatusString(const StatusStr: string);
    procedure SetMatchString(const MatchStr: string);
    function DoingSearchOrReplace: Boolean;
    procedure ExpandOrContractList(Expand, UsedState, SetState: Boolean; AExpandFewLines: Integer);
    function GetStayOnTop: Boolean;
    procedure ResizeStatusBar;
    procedure RefreshInformation(AMatchesFound: Integer; ADoExpand, AUSedExpandState, ASetExpandState: Boolean);
    procedure ViewHistoryListItems(AIndex: Integer; AUsedExpandState: Boolean);
    procedure SetShowHistoryList(const Value: Boolean);
    procedure SetShowFullFilename(const Value: Boolean);
    procedure SetShowLineIndent(const Value: Boolean);
    procedure RefreshHistoryView(DoUpdateIndex: Boolean);
    procedure SetHistoryListMode(ANewMode: TGrepHistoryListMode; DoRefresh, DoSaveIndex, DoUpdateIndex: Boolean;
      AddIf: Boolean = False; AConditionMode: TGrepHistoryListMode = hlmSettings);
    function  GetSavedHistoryIndex: Integer;
    procedure SetSavedHistoryIndexes(AIndex: Integer);
    function  HistoryListSelect(ASelectType: TGrepSelectType; ACaption: String): TGrepSelectResult;
    procedure ClearResultsData;
    procedure DoEmbeddedSearch(Sender: TObject);
    procedure UpdateHistoryPagesOptions;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AssignSettingsToForm;
    function ConfigurationKey: string;
    function ConfigWindowKey: String;
  public
    GrepExpert: TGrepExpert;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitGrepSettings(AGrepSettings: TGrepSettings);
    function  Execute(AState: TGrepSearchState): Boolean;
    procedure UpdateFromSettings;
    procedure InternalSaveSettings(Settings: TGExpertsSettings);
    property StayOnTop: Boolean read GetStayOnTop write SetStayOnTop;
    property ShowContext: Boolean read FShowContext write SetShowContext;
    property ShowFullFilename: Boolean read FShowFullFilename write SetShowFullFilename;
    property ShowLineIndent: Boolean read FShowLineIndent write SetShowLineIndent;
    property ShowHistoryList: Boolean read FShowHistoryList write SetShowHistoryList;
    property DoSearchReplace: Boolean read FDoSearchReplace write FDoSearchReplace;
    property ContextSearchText: string read FContextSearchText;
    property GrepSettings: TGrepSettings read FGrepSettings;
  end;

var
  fmGrepResults: TfmGrepResults = nil;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  SysUtils, Messages, ToolsAPI, Math, StrUtils, IniFiles, TypInfo, Contnrs, Clipbrd, DateUtils,
  GX_GExperts, GX_SharedImages, GX_GenericUtils, GX_OtaUtils, GX_GxUtils, GX_IdeUtils, GX_MessageBox,
  GX_GrepPrinting, GX_Replace, GX_GrepReplace, GX_GrepSelect,
  GX_GrepProgress;

resourcestring
  SGrepReplaceStats = 'Replaced %d occurrence(s) in %.2f seconds';

const  //do not localize
  cKeyPageIndexType: array[TPageSavedIndexType] of String = ('Last', 'Top');
  cKeyPageIndexMode: array[TGrepHistoryListMode] of String = (
    'ViewedResultsItem',
    'ViewedSettingsItem',
    'ViewedItem',
    'ViewedSearchItems');

type
  TShowUnicodeReplaceMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

  TGxResultRefreshSelectedQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

  TGxResultDeleteSelectedQuestion = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

procedure GoToMatchLine(MatchLine: TLineResult; SourceEditorInMiddle: Boolean);
var
  MatchFileName: string;
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
begin
  MatchFileName := TFileResult(MatchLine.Collection).FileName;

  if IsStandAlone then
    GXShellExecute(MatchFileName, '', True)
  else
    GxOtaGoToFileLineColumn(MatchFileName, MatchLine.LineNo, MatchLine.Matches[0].SPos, MatchLine.Matches[0].EPos, SourceEditorInMiddle);
end;

{ TShowUnicodeReplaceMessage }

function TShowUnicodeReplaceMessage.GetMessage: string;
begin
  Result := 'Using GExperts grep replace can corrupt your source code if the file ' +
    'being replaced is in a UNICODE format on disk or if the file is loaded into the IDE ' +
    'editor and contains characters other than low ASCII.  Canceling the replace ' +
    'is recommended if any of your files fall into one of these categories.';
end;

function TShowUnicodeReplaceMessage.ShouldShow: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

{ TGxResultRefreshSelectedQuestion }

function TGxResultRefreshSelectedQuestion.GetMessage: string;
begin
  if FData = 'A' then
    Result := 'Are you sure you want to refresh all search history item lists?'
  else
    Result := 'Are you sure you want to refresh selected search history item lists?';
end;

{ TGxResultDeleteSelectedQuestion }

function TGxResultDeleteSelectedQuestion.GetMessage: string;
begin
  if FData = 'A' then
    Result := 'Are you sure you want to delete all search history items?'
  else
    Result := 'Are you sure you want to delete selected search history items?';
end;

{ TfmGrepResults }

procedure TfmGrepResults.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    if FSearchInProgress then
      Self.Abort
    else
    begin
      if IsStandAlone then
        ModalResult := mrCancel
      else
        Hide;
    end;
  end;
end;

procedure TfmGrepResults.StartFileSearch(Sender: TObject; const FileName: string);
resourcestring
  SProcessing = 'Processing %s';
var
  Dummy: Boolean;
  CurrentGetTickCount: DWORD;
begin
  SetStatusString(Format(SProcessing, [FileName]));
  ActionsUpdate(nil, Dummy);
  CurrentGetTickCount := GetTickCount;
  if CurrentGetTickCount <> FLastRepaintTick then
  begin
    Application.ProcessMessages;
    FLastRepaintTick := CurrentGetTickCount;
  end;
end;

function TfmGrepResults.Execute(AState: TGrepSearchState): Boolean;
resourcestring
  SGrepActive = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepSearchStats = 'Searched %d files in %.2f seconds for "%s"';
var
  TimeStart: TDateTime;
  FilesSearched: Cardinal;
  MatchesFound: Cardinal;
  Cursor: IInterface;
  ResultFiles: TStringList;
  I : Integer;
  AItemIndex, ATopIndex : Integer;
begin
  Result := False;
  if FSearchInProgress then
    raise Exception.Create(SGrepActive);

  if (AState in [gssNormal, gssSearchAgain, gssModifySearchSettings]) or not FGrepSettings.CanRefresh then
    if not QueryUserForGrepOptions(AState) then
      Exit;

  Result := True;

  if AState = gssModifySearchSettings then
  begin
    GrepExpert.HistoryList.UpdateGrepSettings(FGrepSettings);
    lbHistoryList.Refresh;
    Exit;
  end;

  FLastRepaintTick := GetTickCount;
  reContext.Clear;

  ResultFiles := TStringList.Create;
  try
    ContractList(False);
    for i := 0 to lbResults.Items.Count - 1 do
      ResultFiles.Add(lbResults.Items[i]);
    SetStatusString('');
    SetMatchString('');
    ClearResultsListbox;
    BringToFront;
    IdeDockManager.ShowForm(Self);
    EnsureFormVisible(Self);

    TimeStart := Now;
    Cursor := TempHourGlassCursor;

    FSearcher := TGrepSearchRunner.Create(FGrepSettings, lbResults.Items, ResultFiles);

    try
      FSearcher.OnSearchFile := StartFileSearch;
      FSearchInProgress := True;
      FSearcher.Execute;
      FilesSearched := FSearcher.FileSearchCount;
      MatchesFound := FSearcher.MatchCount;
    finally
      FreeAndNil(FSearcher);
      FSearchInProgress := False;
    end;
  finally
    FreeAndNil(ResultFiles);
  end;

  SetStatusString(Format(SGrepSearchStats, [FilesSearched, (Now - TimeStart) * 24*60*60, FGrepSettings.Pattern]));

  lbResults.Sorted := True;  // There is no Sort method
  lbResults.Sorted := False;

  if GrepExpert.HistoryList.ListMode = hlmSettings then
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex + 1);

  SetHistoryListMode(hlmResults, False, False, False, True);

  AItemIndex := GrepExpert.HistoryList.AddItem(FGrepSettings, lbResults.Items, TimeStart);
  GrepExpert.HistoryListSaveSettings(AItemIndex);

  ATopIndex := lbHistoryList.TopIndex;
  lbHistoryList.Count := GrepExpert.HistoryList.Count;
  lbHistoryList.ItemIndex := AItemIndex;
  if (ATopIndex < lbHistoryList.Count) and (AItemIndex < lbHistoryList.Count-1) then
    lbHistoryList.TopIndex := ATopIndex;
  SetSavedHistoryIndexes(AItemIndex);

  lbHistoryList.Refresh;

  RefreshInformation(MatchesFound, GrepExpert.GrepExpandAll, False, True);

  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.Hide;
end;

procedure TfmGrepResults.RefreshHistoryView(DoUpdateIndex: Boolean);
begin
  lbHistoryList.Items.BeginUpdate;
  try
    lbHistoryList.Count := GrepExpert.HistoryList.Count;
//    lbHistoryList.Refresh;
    if DoUpdateIndex then
      GetSavedHistoryIndex;
  finally
    lbHistoryList.Items.EndUpdate;
  end;
end;

procedure TfmGrepResults.SetHistoryListMode(ANewMode: TGrepHistoryListMode;
  DoRefresh, DoSaveIndex, DoUpdateIndex, AddIf: Boolean; AConditionMode: TGrepHistoryListMode);
begin
  if not AddIf or (GrepExpert.HistoryList.ListMode = AConditionMode) then
  begin
    if DoSaveIndex then
      SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    GrepExpert.HistoryList.ListMode := ANewMode;
    tcHistoryListPage.TabIndex := Integer(ANewMode);
    Caption := Format(FSavedFormCaption, [tcHistoryListPage.Tabs[Integer(ANewMode)]]);
    if DoRefresh then
      RefreshHistoryView(DoUpdateIndex);
  end;
end;

function TfmGrepResults.GetSavedHistoryIndex: Integer;
var
  APageIndexes: TPageIndexes;
  ATopIndex: Integer;
begin
  APageIndexes := FPageIndexes[GrepExpert.HistoryList.ListMode];
  Result := GrepExpert.HistoryList.ItemIndexByKeyIndex(APageIndexes[pitClickedEntryKeyIndex]);
  if Result = -1 then
    Result := APageIndexes[pitClickedEntryItemIndex];
  ATopIndex := GrepExpert.HistoryList.ItemIndexByKeyIndex(APageIndexes[pitTopKeyIndex]);
  if ATopIndex = -1 then
    ATopIndex := APageIndexes[pitTopItemIndex];

  if Result <> -1 then
    lbHistoryList.ItemIndex := Result
  else
  begin
    lbHistoryList.ItemIndex := lbHistoryList.Count-1;
    Result := lbHistoryList.ItemIndex;
  end;
  if (ATopIndex <> -1) and (ATopIndex <= Result) then
    lbHistoryList.TopIndex := ATopIndex;

  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
end;

procedure TfmGrepResults.SetSavedHistoryIndexes(AIndex: Integer);
var
  AItem: TGrepHistoryListItem;
  APageIndexes: TPageIndexes;
begin
  APageIndexes[pitClickedEntryItemIndex] := AIndex;
  AItem := GrepExpert.HistoryList.Items[AIndex];
  if Assigned(AItem) then
    APageIndexes[pitClickedEntryKeyIndex] := AItem.KeyIndex
  else
    APageIndexes[pitClickedEntryKeyIndex] := -1;

  APageIndexes[pitClickedEntryItemIndex] := lbHistoryList.TopIndex;
  AItem := GrepExpert.HistoryList.Items[lbHistoryList.TopIndex];
  if Assigned(AItem) then
    APageIndexes[pitTopKeyIndex] := AItem.KeyIndex
  else
    APageIndexes[pitTopKeyIndex] := -1;

  FPageIndexes[GrepExpert.HistoryList.ListMode] := APageIndexes;
end;

procedure TfmGrepResults.RefreshInformation(AMatchesFound: Integer; ADoExpand, AUSedExpandState, ASetExpandState: Boolean);
resourcestring
  SMatches = '%d matches';
  SMatches1 = '%d match';
  SFiles = 'in %d files';
  SFiles1 = 'in %d file';
var
  MatchString: string;
  FilesHit: Integer;
begin
  FilesHit := lbResults.Items.Count;

  if (lbResults.Items.Count = 1) or ADoExpand or GrepExpert.GrepExpandFew or
    (GrepExpert.GrepExpandIf and
      (AMatchesFound <= GrepExpert.GrepExpandIfMatches) and (FilesHit <= GrepExpert.GrepExpandIfFiles)
    )
  then
  begin
    lbResults.ItemIndex := 0;
    ExpandList(AUSedExpandState, ASetExpandState, IfThen(GrepExpert.GrepExpandFew, GrepExpert.GrepExpandFewLines));
  end;

  lbResults.Refresh;

  if AMatchesFound = 1 then
    MatchString := Format(SMatches1, [AMatchesFound])
  else
    MatchString := Format(SMatches, [AMatchesFound]);

  if FilesHit = 1 then
    MatchString := MatchString + ' ' + Format(SFiles1, [FilesHit])
  else
    MatchString := MatchString + ' ' + Format(SFiles, [FilesHit]);

  SetMatchString(MatchString);
end;

procedure TfmGrepResults.tcHistoryListPageChange(Sender: TObject);
var
  ANewMode: TGrepHistoryListMode;
begin
  ANewMode := TGrepHistoryListMode(tcHistoryListPage.TabIndex);
  if GrepExpert.HistoryList.ListMode <> ANewMode then
    SetHistoryListMode(ANewMode, True, True, True);
end;

procedure TfmGrepResults.ClearResultsListbox;
begin
  lbResults.Clear;
end;

procedure TfmGrepResults.lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  DragTreshold: Integer;
  Temp: TObject;
begin
  DragTreshold := Mouse.DragThreshold;

  // Make sure mouse has moved at least DragTreshold pixels before starting drag ...
  if (FDragPoint.X = -1) or ((Shift <> [ssLeft]) and (Shift <> [ssRight])) or
    ((Abs(FDragPoint.X - X) < DragTreshold) and (Abs(FDragPoint.Y - Y) < DragTreshold)) then
  begin
    Exit;
  end;

  i := lbResults.ItemAtPos(Point(X, Y), True);

  if i >= 0 then
  begin
    FDragSource.Files.Clear;

    Temp := lbResults.Items.Objects[i];
    if Temp is TFileResult then
      FDragSource.Files.Add(TFileResult(Temp).FileName)
    else
    begin
      Temp := lbResults.Items.Objects[i];
      if Temp is TLineResult then
      begin
        Temp := TLineResult(Temp).Collection;
        FDragSource.Files.Add((Temp as TFileResult).FileName);
      end
      else
        Assert(False, 'Internal Error');
    end;

    if FDragSource.Files.Count > 0 then
      FDragSource.Execute;
  end;
end;

procedure TfmGrepResults.lbResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragPoint := Point(X, Y);
end;

procedure TfmGrepResults.FormResize(Sender: TObject);
begin
  ResizeStatusBar;
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.EmbeddedUpdatePos;
  lbResults.Refresh;
end;

procedure TfmGrepResults.InitGrepSettings(AGrepSettings: TGrepSettings);
begin
  FGrepSettings := AGrepSettings;
end;

procedure TfmGrepResults.InternalSaveSettings(Settings: TGExpertsSettings);
var
  WindowSettings: TExpertSettings;
  Percent: integer;
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  WindowSettings := Settings.CreateExpertSettings(ConfigWindowKey);
  try
    Settings.SaveForm(Self, ConfigWindowKey);
    WindowSettings.WriteBool('OnTop', StayOnTop);
    WindowSettings.WriteBool('ShowToolBar', ToolBar.Visible);
    WindowSettings.WriteBool('ShowContext', ShowContext);
    WindowSettings.WriteBool('ShowHistoryList', ShowHistoryList);

    if GrepExpert.ContextSaveFixedHeight then
      WindowSettings.WriteInteger('ContextHeight', reContext.Height)
    else
    begin
      Percent := (reContext.Height * 100) div ClientHeight;
      WindowSettings.WriteInteger('ContextHeightPercent', Percent);
    end;

    WindowSettings.WriteInteger('HistoryListWidth', lbHistoryList.Width);

    WindowSettings.WriteBool('ShowFullFilename', ShowFullFilename);
    WindowSettings.WriteBool('ShowLineIndent', ShowLineIndent);

    for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
      for IT := Low(TPageSavedIndexType) to High(TPageSavedIndexType) do
        WindowSettings.WriteInteger(cKeyPageIndexType[IT] + cKeyPageIndexMode[IM], FPageIndexes[IM, IT]);

    WindowSettings.WriteInteger('HistoryListPage', tcHistoryListPage.TabIndex);
  finally
    WindowSettings.Free;
  end;
end;

procedure TfmGrepResults.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize any of the below strings.
  Settings := TGExpertsSettings.Create;
  try
    InternalSaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmGrepResults.LoadSettings;
var
  WindowSettings: TExpertSettings;
  Settings: TGExpertsSettings;
  AHistoryIniVersion: Integer;
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  // Do not localize any of the below strings.
  WindowSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    AHistoryIniVersion := Settings.ReadInteger(ConfigurationKey, 'HistoryIniVersion', 0);

    WindowSettings := Settings.CreateExpertSettings(ConfigWindowKey);
    Settings.LoadForm(Self, ConfigWindowKey);
    EnsureFormVisible(Self);
    StayOnTop := WindowSettings.ReadBool('OnTop', True);
    ToolBar.Visible := WindowSettings.ReadBool('ShowToolBar', ToolBar.Visible);
    ShowContext := WindowSettings.ReadBool('ShowContext', True);

    for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
      for IT := Low(TPageSavedIndexType) to High(TPageSavedIndexType) do
        FPageIndexes[IM, IT] := WindowSettings.ReadInteger(cKeyPageIndexType[IT] + cKeyPageIndexMode[IM], FPageIndexes[IM, IT]);

    if AHistoryIniVersion = 0 then
    begin
      ShowHistoryList := WindowSettings.ReadBool('ShowFoundList', True);
      FLoadHistoryListWidth := WindowSettings.ReadInteger('FoundListWidth', lbHistoryList.Width)
    end
    else
    begin
      ShowHistoryList := WindowSettings.ReadBool('ShowHistoryList', True);
      FLoadHistoryListWidth := WindowSettings.ReadInteger('HistoryListWidth', lbHistoryList.Width);
    end;

    if WindowSettings.ValueExists('ContextHeightPercent') then
      FLoadContextHeightPercent := WindowSettings.ReadInteger('ContextHeightPercent', 20)
    else
      FLoadContextHeightPercent := -1;

    if WindowSettings.ValueExists('ContextHeight') then
      FLoadContextHeight := WindowSettings.ReadInteger('ContextHeight', reContext.Height)
    else
      FLoadContextHeight := pnlMain.Height - ToolBar.Height - SplitterContext.Height -
        WindowSettings.ReadInteger('ResultsHeight', lbResults.Height);

    ShowFullFilename := WindowSettings.ReadBool('ShowFullFilename', False);
    ShowLineIndent := WindowSettings.ReadBool('ShowLineIndent', False);

    FLoadHistoryListPage := WindowSettings.ReadInteger('HistoryListPage', tcHistoryListPage.TabIndex);
  finally
    FreeAndNil(WindowSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TfmGrepResults.UpdateFromSettings;
begin
  FEmbeddedGrepSearch := TfmGrepSearch.Create(pnlMain);
  FEmbeddedGrepSearch.EmbeddedInit(lbResults, DoEmbeddedSearch);

  GrepExpert.HistoryList.Enabled := ShowHistoryList;
  if not GrepExpert.ContextSaveFixedHeight and (FLoadContextHeightPercent > 0) then
    reContext.Height := (ClientHeight * FLoadContextHeightPercent) div 100
  else
    reContext.Height := FLoadContextHeight;

  if FShowHistoryList then
  begin
    UpdateHistoryPagesOptions;
    if GrepExpert.GrepSaveHistoryListItems then
    begin
      lbHistoryList.Width := FLoadHistoryListWidth;
      if GrepExpert.GrepHistoryListDefaultPage < tcHistoryListPage.Tabs.Count then
        FLoadHistoryListPage := GrepExpert.GrepHistoryListDefaultPage;
      SetHistoryListMode(TGrepHistoryListMode(FLoadHistoryListPage), True, False, True);
      SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    end;
  end;
end;

procedure TfmGrepResults.UpdateHistoryPagesOptions;
begin
  tcHistoryListPage.MultiLine := GrepExpert.GrepHistoryPagesTabMultiline;
  if not GrepExpert.GrepHistoryPagesTabMultiline then
    tcHistoryListPage.TabWidth := GrepExpert.GrepHistoryPagesTabWidth;
end;

procedure TfmGrepResults.lbResultsClick(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if Assigned(AItem) and AItem.IsOnlySaveSettings then
    Exit;

  RefreshContextLines;
end;

procedure TfmGrepResults.actShowMatchContextExecute(Sender: TObject);
begin
  ShowContext := not ShowContext;
end;

procedure TfmGrepResults.SetShowContext(Value: Boolean);
begin
  FShowContext := Value;
  reContext.Visible := ShowContext;
  SplitterContext.Visible := ShowContext;
  RefreshContextLines;
end;

procedure TfmGrepResults.RefreshContextLines;
resourcestring
  SMatchContextNotAvail = 'Unable to load match context lines';
var
  CurrentLine: TLineResult;
  CurrentFile: TFileResult;
  MatchLineNo, BeginLineNo, EndLineNo, REMatchLineNo: Integer;
  FileLines: TGXUnicodeStringList;
  FileName: string;
  i: Integer;
begin
  if not ShowContext then
    Exit;

  reContext.Lines.BeginUpdate;
  try
    reContext.Clear;
    if (lbResults.ItemIndex < 0) then
      Exit;
    if (ShowContext) and (GrepExpert.NumContextLines > 0) then
    begin
      if (lbResults.Items.Objects[lbResults.ItemIndex] is TLineResult) then
      begin
        CurrentLine := TLineResult(lbResults.Items.Objects[lbResults.ItemIndex]);
        CurrentFile := TFileResult(CurrentLine.Collection);
        FileName := CurrentFile.FileName;
        SetStatusString(CurrentFile.RelativeFileName);

        MatchLineNo := CurrentLine.LineNo - 1;

        FileLines := TGXUnicodeStringList.Create;
        try
          try
            GxOtaLoadFileToUnicodeStrings(FileName, FileLines);
          except
            on E: EGXFileNotFound do
            begin
              reContext.Lines.Text := E.Message;
              Exit;
            end;
          end;
          if FileLines.Count < 1 then
          begin
            reContext.Lines.Text := SMatchContextNotAvail;
            Exit;
          end;

          BeginLineNo := MatchLineNo - GrepExpert.NumContextLines;
          BeginLineNo := Max(BeginLineNo, 0);
          EndLineNo := MatchLineNo + GrepExpert.NumContextLines;
          EndLineNo := Min(EndLineNo, FileLines.Count - 1);

          REMatchLineNo := 0;
          reContext.SelStart := reContext.GetTextLen;
          for i := BeginLineNo to EndLineNo do
          begin
            reContext.SelText := FileLines[i] + IfThen(i <> EndLineNo, sLineBreak);
            if i = MatchLineNo then
              REMatchLineNo := reContext.Lines.Count - 1;
          end;
        finally
          FreeAndNil(FileLines);
        end;
        HighlightMemo(TFileResult(CurrentLine.Collection), BeginLineNo, REMatchLineNo);
        CenterLineInEdit(reContext, REMatchLineNo)
      end;
    end;
  finally
    reContext.Lines.EndUpdate;
  end;
end;

// Make any matches found in the context lines bold
// Also highlight the current match line using clHighlightText
procedure TfmGrepResults.HighlightMemo(FileMatches: TFileResult; StartLine, MatchLineNo: Integer);
var
  Matches: TMatchArray;
  i, j: Integer;
begin
  reContext.SelStart := 0;
  reContext.SelLength := Length(reContext.Lines.Text);
  reContext.SelAttributes.Name := reContext.DefAttributes.Name;
  reContext.SelAttributes.Size := reContext.DefAttributes.Size;
  reContext.SelAttributes.Style := [];

  // Highlight the matched line
  reContext.SelStart := reContext.Perform(EM_LINEINDEX, MatchLineNo, 0);
  reContext.SelLength := Length(reContext.Lines[MatchLineNo]);
  reContext.SelAttributes.Color := GrepExpert.ContextMatchLineColor;

  for i := StartLine + 1 to StartLine + reContext.Lines.Count + 1 do
  begin
    FileMatches.GetMatchesOnLine(i, Matches);
    for j := 0 to Length(Matches) - 1 do
    begin
      if Matches[j].ShowBold then
      begin
        reContext.SelStart := reContext.Perform(EM_LINEINDEX, i - StartLine - 1, 0) + Matches[j].SPos - 1;
        reContext.SelLength := Matches[j].EPos - Matches[j].SPos + 1;
        reContext.SelAttributes.Color := GrepExpert.ContextMatchColor;
        reContext.SelAttributes.Style := [fsBold];
      end;
    end;
  end;
  reContext.SelStart := 0;
end;

procedure TfmGrepResults.lbResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedEntry: Integer;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbResults.ItemAtPos(Point(X, Y), True);
    if (ClickedEntry <> -1) and
       (lbResults.Items.Objects[ClickedEntry] is TFileResult) then
    begin
      ToggleFileResultExpanded(ClickedEntry);
    end;
  end;
end;

procedure TfmGrepResults.ToggleFileResultExpanded(ListBoxIndex: Integer);
var
  AFileResult: TFileResult;
  i: Integer;
begin
  if FSearchInProgress or
     (ListBoxIndex < 0) or (ListBoxIndex >= lbResults.Items.Count) then
  begin
    Exit;
  end;

  if lbResults.Items.Objects[ListBoxIndex] is TFileResult then
  begin
    AFileResult := TFileResult(lbResults.Items.Objects[ListBoxIndex]);

    lbResults.Items.BeginUpdate;
    try
      if AFileResult.Expanded then
      begin
        while (ListBoxIndex + 1 <= lbResults.Items.Count - 1) and
              (not (lbResults.Items.Objects[ListBoxIndex + 1] is TFileResult)) do
        begin
          lbResults.Items.Delete(ListBoxIndex + 1);
        end;
        AFileResult.Expanded := False;
        AFileResult.ExpandState := False;
      end
      else
      begin
        for i := AFileResult.Count - 1 downto 0 do
          lbResults.Items.InsertObject(ListBoxIndex + 1, AFileResult.Items[i].Line, AFileResult.Items[i]);
        AFileResult.Expanded := True;
        AFileResult.ExpandState := True;
      end
    finally
      lbResults.Items.EndUpdate;
    end;
  end;
end;

procedure TfmGrepResults.lbResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  lbResults.Items.BeginUpdate;
  try
    i := lbResults.ItemIndex;
    if (i < 0) then
      Exit;

    // Search for a TFileResult above the selected line
    while (i > 0) and not (lbResults.Items.Objects[i] is TFileResult) do
      Dec(i);

    if not (lbResults.Items.Objects[i] is TFileResult) then
      Exit;

    if  (Key in [VK_LEFT, VK_RIGHT])
      and (TFileResult(lbResults.Items.Objects[i]).Expanded = (Key = VK_LEFT)) then
    begin
      lbResults.ItemIndex := i;
      ToggleFileResultExpanded(i);
      Key := 0;
    end;

  finally
    lbResults.Items.EndUpdate;
  end;
end;

procedure TfmGrepResults.lbResultsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '+', '-':
      ToggleFileResultExpanded(lbResults.ItemIndex);
    #13:
      GotoHighlightedListEntry;
  end;
end;

procedure TfmGrepResults.ResizeListBox;
begin
  lbResults.Canvas.Font.Assign(lbResults.Font);
  lbResults.ItemHeight := lbResults.Canvas.TextHeight(SAllAlphaNumericChars) + 3;
  lbResults.Refresh;
end;

procedure TfmGrepResults.lbResultsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ResultsCanvas: TCanvas;
  LineText: string;
resourcestring
  SItemMatch = '%5d matches';

  procedure PaintFileHeader;
  var
    TopColor: TColor;
    BottomColor: TColor;
    i: Integer;
    FileNameWidth: Integer;
    FileString: string;
    FileResult: TFileResult;
  begin
    TopColor := clBtnHighlight;
    BottomColor := clBtnShadow;

    FileResult := TFileResult(lbResults.Items.Objects[Index]);
    // Paint an expandable search file header (gray)
    ResultsCanvas.Brush.Color := clBtnFace;
    ResultsCanvas.Font.Color := clBtnText;
    ResultsCanvas.FillRect(Rect);

    Rect.Right := Rect.Right + 2;
    if odSelected in State then
      Frame3D(ResultsCanvas, Rect, BottomColor, TopColor, 1)
    else
      Frame3D(ResultsCanvas, Rect, TopColor, BottomColor, 1);

    i := ResultsCanvas.TextWidth('+');
    if ShowFullFilename then
      FileString := FileResult.FileName
    else
      FileString := FileResult.RelativeFileName;
    ResultsCanvas.TextOut(Rect.Left + i + 8, Rect.Top, FileString);

    if FileResult.Expanded then
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '-')
    else
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '+');

    LineText := Format(SItemMatch, [FileResult.TotalMatches]);

    FileNameWidth := ResultsCanvas.TextWidth(LineText) + 10;
    if (ResultsCanvas.TextWidth(FileString) + i + 10) <= Rect.Right - FileNameWidth then
      ResultsCanvas.TextOut(lbResults.ClientWidth - FileNameWidth, Rect.Top, LineText);
  end;

  procedure PaintLineMatch;
  var
    i: Integer;
    ALineResult: TLineResult;
    LineNoWidth: Integer;
    PaintText: string;
    Trimmed: Integer;
    TextTop: Integer;
    sb: TColor;
    sf: TColor;
    nb: TColor;
    nf: TColor;
    Match: TMatchResult;
    NextMatchStart: Integer;
    PrevMatchEnd: Integer;
  begin
    // Paint a search match line number and highlighted match
    ALineResult := lbResults.Items.Objects[Index] as TLineResult;

    if odSelected in State then
    begin
      if GrepExpert.ListUseDefaultColors then
      begin
        nb := clHighLight;
        nf := clHighLightText;
        sb := clWindow;
        sf := clWindowText;
      end
      else
      begin
        nb := GrepExpert.ListMatchBrushColor;
        nf := GrepExpert.ListMatchTextColor;
        sb := clWindow;
        sf := GrepExpert.ListFont.Color;
      end;
    end
    else
    begin
      if GrepExpert.ListUseDefaultColors then
      begin
        sb := clHighLight;
        sf := clHighLightText;
        nb := clWindow;
        nf := clWindowText;
      end
      else
      begin
        sb := GrepExpert.ListMatchBrushColor;
        sf := GrepExpert.ListMatchTextColor;
        nb := clWindow;
        nf := GrepExpert.ListFont.Color;
      end;
    end;

    // Paint line number of match line
    TextTop := Rect.Top + 1;
    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(Rect);
    ResultsCanvas.TextOut(Rect.Left + 10, TextTop, IntToStr(ALineResult.LineNo));

    LineNoWidth := 60;
    LineText := lbResults.Items[Index];

    if not FShowLineIndent then
    begin
      if ALineResult.Matches.Count > 0 then
      begin
        // Avoid trimming inside the first match :
        Trimmed := 0;
        while (Length(LineText) > Trimmed)
          and CharInSet(LineText[Trimmed + 1], [#9, #32])
          and (Trimmed < ALineResult.Matches[0].SPos - 1) do
            Inc(Trimmed);

        if Trimmed > 0 then
          Delete(LineText, 1, Trimmed);
      end
      else
        Trimmed := LeftTrimChars(LineText);
    end
    else
      Trimmed := 0;

    PrevMatchEnd := 0;

    // Paint first match line up to first match character
    PaintText := Copy(LineText, 0, ALineResult.Matches[0].SPos - 1 - Trimmed);
    ResultsCanvas.TextOut(Rect.Left + LineNoWidth, TextTop, PaintText);

    // For each match, paint out the match and then the text until the next match
    for i := 0 to ALineResult.Matches.Count - 1 do
    begin
      // Paint the highlighted match
      Match := ALineResult.Matches[i];
      PaintText := Copy(LineText, Max(Match.SPos, PrevMatchEnd + 1) - Trimmed, Match.Length);
      ResultsCanvas.Font.Color := sf;
      ResultsCanvas.Brush.Color := sb;
      ResultsCanvas.TextOut(ResultsCanvas.PenPos.X + 1, TextTop, PaintText);
      PrevMatchEnd := Match.EPos;

      // Paint any non-matching text after the match
      if i = ALineResult.Matches.Count - 1 then
        NextMatchStart := Length(LineText) + Trimmed
      else
        NextMatchStart := ALineResult.Matches[i + 1].SPos - Trimmed - 1;
      PaintText := Copy(LineText, Match.EPos - Trimmed + 1, NextMatchStart - Match.EPos + Trimmed);
      ResultsCanvas.Font.Color := nf;
      ResultsCanvas.Brush.Color := nb;
      ResultsCanvas.TextOut(ResultsCanvas.PenPos.X + 1, TextTop, PaintText);
    end;
  end;

  procedure PaintOnlySaveSettings;
  begin
    ResultsCanvas.Brush.Color := clWindow;
    ResultsCanvas.Font.Color := clWindowText;
    ResultsCanvas.FillRect(Rect);

    ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top + 1, lbResults.Items[Index]);
  end;

begin
  ResultsCanvas := lbResults.Canvas;
  if not Assigned(lbResults.Items.Objects[Index]) then
    PaintOnlySaveSettings
  else if lbResults.Items.Objects[Index] is TFileResult then
    PaintFileHeader
  else
    PaintLineMatch;
end;

procedure TfmGrepResults.SetStayOnTop(Value: Boolean);
begin
  // Stay on top hides some modal dialogs
  if (StayOnTop <> Value) and IsStandAlone then
  begin
    if Value then
      Self.FormStyle := fsStayOnTop
    else
      Self.FormStyle := fsNormal;
  end;
end;

procedure TfmGrepResults.actFileSearchExecute(Sender: TObject);
begin
  Execute(gssNormal);
end;

procedure TfmGrepResults.actFileRefreshExecute(Sender: TObject);
begin
  Execute(gssRefresh);
  RefreshContextLines;
end;

procedure TfmGrepResults.actFileAbortExecute(Sender: TObject);
begin
  {$IFOPT D+} SendDebug('Grep abort requested by user'); {$ENDIF}
  Self.Abort;
end;

procedure TfmGrepResults.actFilePrintExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  if (lbHistoryList.ItemIndex = -1) or (lbHistoryList.Count = 0) then
    Exit;

  AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not AItem.IsOnlySaveSettings then
  begin
    GrepExpert.HistoryList.ClearAllChecked;
    AItem.Checked := True;
    SaveGrepResultsToFile(Self, GrepExpert.HistoryList, GrepExpert.HistoryIniVersion, False,
      sfPrintToFile, grPrint);
  end;
end;

procedure TfmGrepResults.actFileCopyExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  if reContext.Focused and (reContext.SelLength > 0) then
  begin
    reContext.CopyToClipboard;
    Exit;
  end;

  if (lbHistoryList.ItemIndex = -1) or (lbHistoryList.Count = 0) then
    Exit;

  AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not AItem.IsOnlySaveSettings then
  begin
    GrepExpert.HistoryList.ClearAllChecked;
    AItem.Checked := True;
    SaveGrepResultsToFile(Self, GrepExpert.HistoryList, GrepExpert.HistoryIniVersion, False,
      sfPrintToFile, grCopy);
  end;
end;

procedure TfmGrepResults.actFileOpenExecute(Sender: TObject);
resourcestring
  rsOpenCaption = 'Select the items for open';   //Open saved Search History
var
  ASelResult: TGrepSelectResult;
  AIni: TGrepIniFile;
  frmSelect: TfmGrepSelect;
  AOpenHistoryList: TGrepHistoryList;
  AItemIndex: Integer;
  AClearList, AOverwriteItem, AOnlyIfNewer: Boolean;
begin
  if not OpenDialog.Execute then
    Exit;

  AOpenHistoryList := TGrepHistoryList.Create;
  try
    AOpenHistoryList.ListMode := hlmAll;

    AIni := TGrepIniFile.Create(OpenDialog.FileName);
    try
      if AIni.SectionExists(TGrepHistoryListItem.SubKeyNameHistory) then //without numbers
        AOpenHistoryList.LoadItemFromIni(FGrepSettings, AIni, GrepExpert.HistoryIniVersion, GrepExpert.OpenSaveOption, GrepExpert.HistoryList)
      else
        AOpenHistoryList.LoadFromSettings(FGrepSettings, AIni, GrepExpert.HistoryIniVersion, ifmSingle, '', GrepExpert.OpenSaveOption, GrepExpert.HistoryList);
    finally
      AIni.Free;
    end;

    if AOpenHistoryList.Count = 0 then
    begin
      MessageDlg('Could not read history list from file!', mtWarning, [mbOK], 0);
      Exit;
    end;

    frmSelect := TfmGrepSelect.Create(Self);
    try
      frmSelect.Init(AOpenHistoryList, gstOpen, rsOpenCaption);
      ASelResult := frmSelect.Execute;
      AClearList := frmSelect.cbOpenClear.Checked;
      AOverwriteItem := frmSelect.cbOpenOverwrite.Checked;
      AOnlyIfNewer := frmSelect.cbOpenOnlyIfNewer.Checked;
    finally
      frmSelect.Free;
    end;

    if ASelResult = gsrNoSelection then
      Exit;

    SetHistoryListMode(hlmResults, False, True, False, True);

    //"copy to"
    AItemIndex := GrepExpert.HistoryList.MoveItems(AOpenHistoryList, AClearList, AOverwriteItem, AOnlyIfNewer);

    RefreshHistoryView(False);

    lbHistoryList.ItemIndex := AItemIndex;

    lbHistoryList.Refresh;

    ViewHistoryListItems(AItemIndex, False);
  finally
    AOpenHistoryList.Free;
  end;
end;

procedure TfmGrepResults.actViewStayOnTopExecute(Sender: TObject);
begin
  StayOnTop := not StayOnTop;
end;

procedure TfmGrepResults.actFileExitExecute(Sender: TObject);
begin
  if IsStandAlone then
    ModalResult := mrCancel
  else
    Hide;
end;

procedure TfmGrepResults.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 3);
end;

procedure TfmGrepResults.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmGrepResults.actListGotoSelectedExecute(Sender: TObject);
begin
  GotoHighlightedListEntry;
end;

procedure TfmGrepResults.actListContractExecute(Sender: TObject);
begin
  ContractList(True);
end;

procedure TfmGrepResults.actListExpandExecute(Sender: TObject);
begin
  ExpandList(False, True, 0);
end;

procedure TfmGrepResults.ExpandOrContractList(Expand, UsedState, SetState: Boolean; AExpandFewLines: Integer);

  function ExpandFileResult(ListBoxIndex: Integer): Integer;
  var
    FileResult: TFileResult;
    t: Integer;
  begin
    FileResult := lbResults.Items.Objects[ListBoxIndex] as TFileResult;

    for t := FileResult.Count - 1 downto 0 do
      lbResults.Items.InsertObject(ListBoxIndex + 1, FileResult.Items[t].Line, FileResult.Items[t]);

    FileResult.Expanded := True;
    if SetState then
      FileResult.ExpandState := True;
    Result := ListBoxIndex + FileResult.Count - 1;
  end;

var
  i: Integer;
  LFileResult: TFileResult;
begin
  if (lbResults.Items.Count > 0) and not Assigned(lbResults.Items.Objects[0]) then
    Exit;

  lbResults.Items.BeginUpdate;
  try
    RefreshContextLines;

    i := 0;
    while i <= lbResults.Items.Count - 1 do
    begin
      if Expand then
      begin
        if lbResults.Items.Objects[i] is TFileResult then
        begin
          LFileResult := TFileResult(lbResults.Items.Objects[i]);

          if not LFileResult.Expanded and (not UsedState or LFileResult.ExpandState) and
            ( (AExpandFewLines = 0) or (LFileResult.Count <= AExpandFewLines) )
          then
            i := ExpandFileResult(i);
        end;

        Inc(i);
      end
      else // Contract
      begin
       if lbResults.Items.Objects[i] is TLineResult then
          lbResults.Items.Delete(i)
        else
        begin
          LFileResult := TFileResult(lbResults.Items.Objects[i]);
          LFileResult.Expanded := False;
          if SetState then
            LFileResult.ExpandState := False;

          Inc(i);
        end;
      end;
    end;
  finally
    lbResults.Items.EndUpdate;
  end;
end;

procedure TfmGrepResults.ExpandList(AUsedState, ASetState: Boolean; AExpandFewLines: Integer);
begin
  ExpandOrContractList(True, AUsedState, ASetState, AExpandFewLines);
end;

procedure TfmGrepResults.ContractList(ASetState: Boolean);
begin
  ExpandOrContractList(False, False, ASetState, 0);
end;

function TfmGrepResults.GetStayOnTop: Boolean;
begin
  Result := (FormStyle = fsStayOnTop);
end;

procedure TfmGrepResults.GotoHighlightedListEntry;
var
  CurrentLine: TLineResult;
  ResultIndex: Integer;
begin
  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  if lbResults.Items.Objects[ResultIndex] is TFileResult then
  begin
    ToggleFileResultExpanded(ResultIndex);
    Exit;
  end;

  CurrentLine := lbResults.Items.Objects[ResultIndex] as TLineResult;
  if CurrentLine = nil then
    Exit;

  GoToMatchLine(CurrentLine, GrepExpert.GrepMiddle);

  // Hide the results window if the window is not configured to stay on top in D8+ and we are floating
  if GrepExpert.AutoHide and RunningDelphi8OrGreater then begin
    if (not StayOnTop) and (not Assigned(Self.Parent)) then
    begin
      if IsStandAlone then
        ModalResult := mrCancel
      else
        Hide;
    end;
  end;
end;

constructor TfmGrepResults.Create(AOwner: TComponent);
var
  IM: TGrepHistoryListMode;
  IT: TPageIndexType;
begin
  inherited;
  for IM := Low(TGrepHistoryListMode) to High(TGrepHistoryListMode) do
    for IT := Low(TPageIndexType) to High(TPageIndexType) do
      FPageIndexes[IM, IT] := -1;

  SetToolbarGradient(ToolBar);

  FSearchInProgress := False;
  lbResults.DoubleBuffered := True;
  CenterForm(Self);
  LoadSettings;
  ResizeListBox;
  SetMatchString('');

  FContextSearchText := '';

  FDragSource := TDropFileSource.Create(nil);

  if IsStandAlone then
  begin
    FormStyle := fsNormal;
    BorderStyle := bsSizeable;
  end;

  FEmbeddedGrepSearch := nil;

  FSavedFormCaption := Caption;
  FSavedLastSearchTimeCaption := miHistoryLastSearchTime.Caption;
  FSavedSaveOptionCaption := miSettingsSaveOption.Caption;
  FSavedDirectoriesDataCaption := miSettingsDirectoriesData.Caption;
  FSavedExcludeDirsCaption := miSettingsExcludeDirs.Caption;
  FSavedFileMasksCaption := miSettingsFileMasks.Caption;
  FSaveItemEmptyCaption := miHistoryItemName.Caption;
  FSaveSortCaption := miHistorySort.Caption;
end;

destructor TfmGrepResults.Destroy;
begin
  // XE used to crash here with a "Component already destroyed" error due to the listbox handle being 0 and then recreated in a destructor
  if lbResults.HandleAllocated then
    ClearResultsListbox;

  Self.Abort;
  SaveSettings;

  FreeAndNil(FDragSource);

  inherited Destroy;

  fmGrepResults := nil;
end;

procedure TfmGrepResults.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  HaveItems, Processing, AIsOnlySaveSettings: Boolean;
  AItem: TGrepHistoryListItem;
begin
  if lbHistoryList.ItemIndex <> -1 then
    AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex]
  else
    AItem := nil;
  AIsOnlySaveSettings := Assigned(AItem) and AItem.IsOnlySaveSettings;

  HaveItems := (lbResults.Items.Count > 0) and not AIsOnlySaveSettings;
  Processing := DoingSearchOrReplace;
  actFileSearch.Enabled := not Processing;
  actViewOptions.Enabled := not Processing;
  actViewStayOnTop.Enabled := not Processing;
  actFilePrint.Enabled := not Processing and HaveItems;
  actFileCopy.Enabled := not Processing and HaveItems;
  actListGotoSelected.Enabled := not Processing and HaveItems;
  actListGotoSelectedAndClose.Enabled := not Processing and HaveItems;
  actListContract.Enabled := not Processing and HaveItems;
  actListExpand.Enabled := not Processing and HaveItems;
  actFileAbort.Enabled := Processing;
  actReplaceAll.Enabled := not Processing and HaveItems;
  actReplaceSelected.Enabled := not Processing and HaveItems;
  actViewShowFullFilename.Enabled := not Processing and not AIsOnlySaveSettings;
  actViewShowIndent.Enabled := not Processing and not AIsOnlySaveSettings;

  actViewStayOnTop.Visible := IsStandAlone;
  tbnSep6.Visible := actViewStayOnTop.Visible;

  actViewStayOnTop.Checked := StayOnTop;
  actViewShowContext.Checked := ShowContext;
  actViewToolBar.Checked := ToolBar.Visible;
  actViewShowHistoryList.Checked := ShowHistoryList;
  actViewShowFullFilename.Checked := ShowFullFilename;
  actViewShowIndent.Checked := ShowLineIndent;
end;

function TfmGrepResults.ShowModalForm(Dlg: TCustomForm): TModalResult;
var
  SavedOnTop: Boolean;
begin
  Result := mrCancel;
  SavedOnTop := StayOnTop;
  try
    // The search dialog can get hidden behind the results if we don't do this
    StayOnTop := False;

    if Dlg.ShowModal <> mrOk then
      Exit;

    Result := mrOk;
  finally
    StayOnTop := SavedOnTop;
  end;
end;

procedure TfmGrepResults.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmGrepResults.Abort;
begin
  if FSearcher <> nil then
    FSearcher.AbortSignalled := True;
end;

procedure TfmGrepResults.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if IsStandAlone then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
    Params.WndParent := GetDesktopwindow;
  end;
end;

procedure TfmGrepResults.actViewToolBarExecute(Sender: TObject);
begin
  ToolBar.Visible := not ToolBar.Visible;
end;

procedure TfmGrepResults.actViewOptionsExecute(Sender: TObject);
begin
  GrepExpert.Configure;
  AssignSettingsToForm;
  ResizeListBox;
  RefreshContextLines;
  UpdateHistoryPagesOptions;
  lbHistoryList.Refresh;
end;

procedure TfmGrepResults.FormShow(Sender: TObject);
begin
  AssignSettingsToForm;
  ResizeListBox;
end;

procedure TfmGrepResults.AssignSettingsToForm;
begin
  Assert(Assigned(GrepExpert));
  reContext.Font.Assign(GrepExpert.ContextFont);
  lbResults.Font.Assign(GrepExpert.ListFont);
end;

procedure TfmGrepResults.actReplaceAllExecute(Sender: TObject);
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  if not QueryUserForReplaceOptions('All matched files') then
    Exit;

  FReplaceInProgress := True;
  try
    Cursor := TempHourGlassCursor;
    TimeStart := Now;
    SetStatusString('');
    MatchesFound := ReplaceAll(lbResults.Items, FGrepSettings);
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

procedure TfmGrepResults.actReplaceSelectedExecute(Sender: TObject);
resourcestring
  SReplaceLine = sLineBreak + 'On line: ';
var
  TimeStart: TDateTime;
  MatchesFound: Integer;
  CurrentLine: TLineResult;
  ResultIndex: Integer;
  FileResult: TFileResult;
  MatchFile: string;
  ResultObject: TObject;
  Cursor: IInterface;
begin
  Assert(not DoingSearchOrReplace);

  ResultIndex := lbResults.ItemIndex;
  if ResultIndex < 0 then
    Exit;

  ResultObject := lbResults.Items.Objects[ResultIndex];
  FReplaceInProgress := True;
  try
    SetStatusString('');
    if ResultObject is TFileResult then
    begin
      FileResult := TFileResult(ResultObject);
      if not QueryUserForReplaceOptions(FileResult.FileName) then
        Exit;
      Cursor := TempHourGlassCursor;
      TimeStart := Now;
      MatchesFound := ReplaceAllInFiles(FileResult, FGrepSettings);
    end
    else if ResultObject is TLineResult then
    begin
      CurrentLine := ResultObject as TLineResult;
      MatchFile := TFileResult(CurrentLine.Collection).FileName;
      if not QueryUserForReplaceOptions(MatchFile + SReplaceLine + IntToStr(CurrentLine.LineNo)) then
        Exit;
      Cursor := TempHourGlassCursor;
      TimeStart := Now;
      MatchesFound := ReplaceLine(CurrentLine, FGrepSettings);
    end
    else
      raise Exception.Create('Internal Error: Unknown result type');
    SetStatusString(Format(SGrepReplaceStats, [MatchesFound, (Now - TimeStart) * 24*60*60]));
  finally
    FReplaceInProgress := False;
  end;
  RefreshContextLines;
end;

function TfmGrepResults.DoingSearchOrReplace: Boolean;
begin
  Result := FSearchInProgress or FReplaceInProgress;
end;

procedure TfmGrepResults.SetStatusString(const StatusStr: string);
begin
  StatusBar.Panels.Items[0].Text := StatusStr;
end;

procedure TfmGrepResults.SetMatchString(const MatchStr: string);
begin
  StatusBar.Panels.Items[1].Text := MatchStr;
  if IsEmpty(MatchStr) then
    StatusBar.Panels.Items[1].Width := 0
  else
    StatusBar.Panels.Items[1].Width := StatusBar.Canvas.TextWidth(MatchStr) + 50;
  ResizeStatusBar;
end;

function TfmGrepResults.QueryUserForGrepOptions(AState: TGrepSearchState): Boolean;
resourcestring
  rsSearchAgainCaption = 'Grep Search Again';
  rsModifySearchSettingsCaption = 'Grep Modify Search Parameters';
var
  Dlg: TfmGrepSearch;
begin
  Result := False;
  Dlg := TfmGrepSearch.Create(nil);
  try
    case AState of
      gssSearchAgain: Dlg.Caption := rsSearchAgainCaption;
      gssModifySearchSettings: Dlg.Caption := rsModifySearchSettingsCaption;
    end;
    if AState in [gssSearchAgain, gssModifySearchSettings] then
      Dlg.AdjustSettings(FGrepSettings);
    if ShowModalForm(Dlg) <> mrOk then
      Exit;
    FGrepSettings.CanRefresh := True;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    if AState <> gssModifySearchSettings then
      Dlg.GrepExpert.SaveSettings;
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TfmGrepResults.QueryUserForReplaceOptions(const ReplaceInString: string): Boolean;
var
  Dlg: TfmGrepReplace;
begin
  ShowGxMessageBox(TShowUnicodeReplaceMessage);

  Result := False;
  Dlg := TfmGrepReplace.Create(nil);
  try
    Dlg.ReplaceInString := ReplaceInString;
    Dlg.SearchString := FGrepSettings.Pattern;
    if ShowModalForm(Dlg) <> mrOk then
      Exit;
    SetMatchString('');
    Dlg.RetrieveSettings(FGrepSettings);
    Result := True;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TfmGrepResults.ConfigurationKey: string;
begin
  Result := TGrepExpert.ConfigurationKey;
end;

function TfmGrepResults.ConfigWindowKey: String;
begin
  Result := ConfigurationKey + '\Window';
end;

procedure TfmGrepResults.ResizeStatusBar;
begin
  StatusBar.Panels.Items[0].Width := StatusBar.ClientWidth - StatusBar.Panels.Items[1].Width;
end;

procedure TfmGrepResults.SetShowHistoryList(const Value: Boolean);
begin
  if FShowHistoryList <> Value then
  begin
    FShowHistoryList := Value;
    if Assigned(GrepExpert) then
      GrepExpert.HistoryList.Enabled := ShowHistoryList;
    lbHistoryList.Visible := ShowHistoryList;
    if Value then
      SplitterHistoryList.Left := lbHistoryList.Left + lbHistoryList.Width;
    SplitterHistoryList.Visible := ShowHistoryList;
    if lbHistoryList.Items.Count > 0 then
    begin
      lbHistoryList.ItemIndex := 0;
      ViewHistoryListItems(lbHistoryList.ItemIndex, True);
    end;
  end;
end;

procedure TfmGrepResults.SetShowFullFilename(const Value: Boolean);
begin
  if FShowFullFilename <> Value then
  begin
    FShowFullFilename := Value;
    lbResults.Refresh;
  end;
end;

procedure TfmGrepResults.SetShowLineIndent(const Value: Boolean);
begin
  if FShowLineIndent <> Value then
  begin
    FShowLineIndent := Value;
    lbResults.Refresh;
  end;
end;

procedure TfmGrepResults.actListGotoSelectedAndCloseExecute(Sender: TObject);
begin
  GotoHighlightedListEntry;
  Close;
end;

procedure TfmGrepResults.actViewShowHistoryListExecute(Sender: TObject);
begin
  ShowHistoryList := not ShowHistoryList
end;

procedure TfmGrepResults.actViewShowFullFilenameExecute(Sender: TObject);
begin
  ShowFullFilename := not ShowFullFilename;
end;

procedure TfmGrepResults.actViewShowIndentExecute(Sender: TObject);
begin
  ShowLineIndent := not ShowLineIndent;
end;

procedure TfmGrepResults.ViewHistoryListItems(AIndex: Integer; AUsedExpandState: Boolean);
var
  AHistoryItem: TGrepHistoryListItem;
begin
  ClearResultsData;
  SetSavedHistoryIndexes(AIndex);
  if AIndex <> -1 then
  begin
    AHistoryItem := GrepExpert.HistoryList.Items[AIndex];
    if Assigned(AHistoryItem) then
    begin
      AHistoryItem.View(lbResults.Items);
      RefreshInformation(AHistoryItem.TotalMatchCount, True, AUsedExpandState, False);
      FGrepSettings := AHistoryItem.GrepSettings;
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListData(Control: TWinControl; Index: Integer; var Data: string);
begin
  Data := GrepExpert.HistoryList[Index];
end;

procedure TfmGrepResults.lbHistoryListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  c2ndRowTop: Integer = 13;
  cMatchesLeft: Integer = 10;
var
  AItem: TGrepHistoryListItem;
begin
  AItem := GrepExpert.HistoryList.Items[Index];
  if not Assigned(AItem) or not (AItem is TGrepHistoryListItem) then
    Exit;

  if not GrepExpert.ListUseDefaultColors then
  begin
    if odSelected in State then
    begin
      lbHistoryList.Canvas.Font.Color := GrepExpert.ListMatchTextColor;
      lbHistoryList.Canvas.Brush.Color := GrepExpert.ListMatchBrushColor;
    end
    else
    begin
      lbHistoryList.Canvas.Font.Color := GrepExpert.ListFont.Color;
      lbHistoryList.Canvas.Brush.Color := clWindow;
    end;
  end;

  lbHistoryList.Canvas.FillRect(Rect);

  lbHistoryList.Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, lbHistoryList.Items[Index]);

  if AItem.GrepSettings.SaveOption = gsoNoSave then
    lbHistoryList.Canvas.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '!!')
  else if AItem.GrepSettings.SaveOption = gsoOnlySaveSettings then
    lbHistoryList.Canvas.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '*');

  if AItem.IsOnlySaveSettings then
    lbHistoryList.Canvas.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop,
      Format('(%d in %d)', [AItem.TotalMatchCount, AItem.FileCount]))
  else
    lbHistoryList.Canvas.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop,
      Format('%d in %d', [AItem.TotalMatchCount, AItem.ResultList.Count]));
end;

procedure TfmGrepResults.lbHistoryListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AIndex, APage: Integer;
  AItem: TGrepHistoryListItem;
  ADoSearch: Boolean;
begin
  AIndex := lbHistoryList.ItemIndex;
  APage := lbHistoryList.Height div lbHistoryList.ItemHeight;
  ADoSearch := False;
  if Key = VK_UP then
    Dec(AIndex)
  else if Key = VK_DOWN then
    Inc(AIndex)
  else if Key = VK_HOME then
    AIndex := 0
  else if Key = VK_END then
    AIndex := lbHistoryList.Count-1
  else if Key = VK_PRIOR then
    Dec(AIndex, APage)
  else if Key = VK_NEXT then
    Inc(AIndex, APage)
  else if Key = VK_RETURN then
    ADoSearch := True
  else if Key <> VK_SPACE then
    Exit;

  Key := 0;

  if AIndex < 0 then
    AIndex := 0
  else if AIndex >= lbHistoryList.Count then
    AIndex := lbHistoryList.Count-1;

  lbHistoryList.ItemIndex := AIndex;
  ViewHistoryListItems(AIndex, True);

  AItem := GrepExpert.HistoryList.Items[AIndex];
  if AItem.IsOnlySaveSettings and
    ( TGrepOnlySaveSettingsAction(GrepExpert.GrepOnlySaveParamsAction) = gossaShowEmbeddedSearch )
  then
  begin
    if ADoSearch then
      DoEmbeddedSearch(nil)
    else
    begin
      FEmbeddedGrepSearch.AdjustSettings(AItem.GrepSettings);
      FEmbeddedGrepSearch.EmbeddedShow;
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FHistoryMousePos.X := X;
  FHistoryMousePos.Y := Y;

  if (GrepExpert.HistoryList.ListMode = hlmSearch) and (lbHistoryList.Count = 0) then
    actHistorySearchInHistory.Execute;
end;

procedure TfmGrepResults.lbHistoryListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ClickedEntry: Integer;
  AItem: TGrepHistoryListItem;
begin
  if Button = mbLeft then
  begin
    ClickedEntry := lbHistoryList.ItemAtPos(Point(X, Y), True);
    if ClickedEntry <> -1 then
    begin
      AItem := GrepExpert.HistoryList.Items[ClickedEntry];
      if AItem.IsOnlySaveSettings then
        case TGrepOnlySaveSettingsAction(GrepExpert.GrepOnlySaveParamsAction) of
          gossaShowSearchWindow:
          begin
            ViewHistoryListItems(ClickedEntry, False);
            pmHistoryMenu.Tag := ClickedEntry;
            actHistorySearch.Execute;
          end ;
          gossaShowEmbeddedSearch:
          begin
            ViewHistoryListItems(ClickedEntry, False);
            FEmbeddedGrepSearch.AdjustSettings(AItem.GrepSettings);
            FEmbeddedGrepSearch.EmbeddedShow;
          end;
          gossaAutoRefresh:
          begin
            pmHistoryMenu.Tag := ClickedEntry;
            actHistoryRefresh.Execute;
          end;
          //Auto refresh when double click
          gossaAutoRefreshDouble: Exit;
        else //Empty list
          ViewHistoryListItems(ClickedEntry, True);
        end
      else
        ViewHistoryListItems(ClickedEntry, True);
    end;
  end;
end;

procedure TfmGrepResults.lbHistoryListDblClick(Sender: TObject);
var
  ClickedEntry: Integer;
  AItem: TGrepHistoryListItem;
begin
  ClickedEntry := lbHistoryList.ItemAtPos(FHistoryMousePos, True);
  if ClickedEntry <> -1 then
  begin
    AItem := GrepExpert.HistoryList.Items[ClickedEntry];
    if AItem.IsOnlySaveSettings then
      case TGrepOnlySaveSettingsAction(GrepExpert.GrepOnlySaveParamsAction) of
        //Auto refresh when double click
        gossaAutoRefreshDouble:
        begin
          pmHistoryMenu.Tag := ClickedEntry;
          actHistoryRefresh.Execute;
        end;
      end;
  end;
end;

procedure TfmGrepResults.lbHistoryListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
resourcestring
  rsItemCaptionFormat = 'Search text = %s';
  rsItemCaptionHint = 'Click for copy search text';
  rsSortUnsorted = 'Unsorted';
  rsSortKeyIndex = 'by keyindex';
  rsSortSearchText = 'by search text';
  rsSortSearchTime = 'by last search time';
  rsSortDesc = 'descending';
const
  cSortTexts: array[TGrepHistorySort] of String = (rsSortUnsorted, rsSortKeyIndex, rsSortSearchText, rsSortSearchTime, '');
var
  AIndex: Integer;
  AItem: TGrepHistoryListItem;
  IsDirectorySettings: Boolean;
begin
  AIndex := lbHistoryList.ItemAtPos(MousePos, True);
  if AIndex = -1 then
    AIndex := lbHistoryList.ItemIndex;

  pmHistoryMenu.Tag := AIndex;

  if AIndex = -1 then
  begin
    miHistoryItemName.Caption := FSaveItemEmptyCaption;
    miHistoryItemName.Enabled := False;
    miHistoryItemName.Hint := '';
    miHistorySettings.Visible := False;
    miHistoryLastSearchTime.Visible := False;
    Exit;
  end;

  AItem := GrepExpert.HistoryList.Items[AIndex];
  miHistoryItemName.Caption := Format(rsItemCaptionFormat, [lbHistoryList.Items[AIndex]]);
  miHistoryItemName.Hint := rsItemCaptionHint;

  miHistoryLastSearchTime.Caption := Format('%s = %s',
    [FSavedLastSearchTimeCaption, DateTimeToStr(AItem.LastSearchTime)]);
  actHistorySort.Caption := FSaveSortCaption + Format(' (%s%s)',
    [cSortTexts[GrepExpert.HistoryList.SortMode], IfThen(GrepExpert.HistoryList.SortDesc, ' ' + rsSortDesc)]);

  miHistorySettings.Visible := True;

  miSettingsSaveOption.Caption := Format('%s = %s',
    [FSavedSaveOptionCaption, SaveOptionText(AItem.GrepSettingsSaveOption)]);

  miSettingsCurrentFile.Checked := AItem.GrepSettings.GrepAction = gaCurrentOnlyGrep;
  miSettingsAllFilesInProjectGroup.Checked := AItem.GrepSettings.GrepAction = gaProjGroupGrep;
  miSettingsAllFilesInProject.Checked := AItem.GrepSettings.GrepAction = gaProjGrep;
  miSettingsOpenProjectFiles.Checked := AItem.GrepSettings.GrepAction = gaOpenFilesGrep;
  miSettingsPreviousSearchResultFiles.Checked := AItem.GrepSettings.GrepAction = gaResults;

  IsDirectorySettings := AItem.GrepSettings.GrepAction = gaDirGrep;
  miSettingsDirectories.Checked := IsDirectorySettings;
  miSettingsSepDir.Visible := IsDirectorySettings;
  miSettingsDirectoriesData.Visible := IsDirectorySettings;
  miSettingsDirectoriesData.Checked := AItem.GrepSettings.Directories <> '';
  miSettingsDirectoriesData.Caption := Format('%s = %s', [FSavedDirectoriesDataCaption, AItem.GrepSettings.Directories]);
  miSettingsExcludeDirs.Visible := IsDirectorySettings;
  miSettingsExcludeDirs.Checked := AItem.GrepSettings.ExcludedDirs <> '';
  miSettingsExcludeDirs.Caption := Format('%s = %s', [FSavedExcludeDirsCaption, AItem.GrepSettings.ExcludedDirs]);
  miSettingsFileMasks.Visible := IsDirectorySettings;
  miSettingsFileMasks.Checked := AItem.GrepSettings.Mask <> '';
  miSettingsFileMasks.Caption := Format('%s = %s', [FSavedFileMasksCaption, AItem.GrepSettings.Mask]);
  miSettingsSearchSubDirectories.Visible := IsDirectorySettings;
  miSettingsSearchSubDirectories.Checked := AItem.GrepSettings.IncludeSubdirs;

  miSettingsCaseSensitive.Checked := AItem.GrepSettings.CaseSensitive;
  miSettingsWholeWord.Checked := AItem.GrepSettings.WholeWord;
  miSettingsSearchFormFiles.Checked := AItem.GrepSettings.IncludeForms;
  miSettingsSearchSQLFiles.Checked := AItem.GrepSettings.IncludeSQLs;
  miSettingsRegularExpression.Checked := AItem.GrepSettings.RegEx;

  miSettingsGrepCode.Checked := AItem.GrepSettings.IncludeCode;
  miSettingsGrepStrings.Checked := AItem.GrepSettings.IncludeStrings;
  miSettingsGrepComments.Checked := AItem.GrepSettings.IncludeComments;

  miSettingsSectionInterface.Checked := AItem.GrepSettings.SectionInterface;
  miSettingsSectionImplementation.Checked := AItem.GrepSettings.SectionImplementation;
  miSettingsSectionInitialization.Checked := AItem.GrepSettings.SectionInitialization;
  miSettingsSectionFinalization.Checked := AItem.GrepSettings.SectionFinalization;
end;

procedure TfmGrepResults.actHistoryUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;
  AAction.Enabled := not DoingSearchOrReplace and (lbHistoryList.Count > 0) and
    ( (AAction.Tag = 1) or (GrepExpert.HistoryList.ListMode <> hlmSettings) );
end;

procedure TfmGrepResults.miHistoryItemNameClick(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := GrepExpert.HistoryList.Items[pmHistoryMenu.Tag];
  if Assigned(AItem) then
    Clipboard.AsText := AItem.SearchText;
end;

procedure TfmGrepResults.actHistoryViewExecute(Sender: TObject);
begin
  lbHistoryList.ItemIndex := pmHistoryMenu.Tag;
  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
end;

procedure TfmGrepResults.DoEmbeddedSearch(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
begin
  AItem := GrepExpert.HistoryList.Items[lbHistoryList.ItemIndex];
  if not Assigned(AItem) then
    Exit;

  FGrepSettings.CanRefresh := True;
  SetMatchString('');
  FEmbeddedGrepSearch.RetrieveSettings(FGrepSettings);
  FEmbeddedGrepSearch.GrepExpert.SaveSettings;
  FEmbeddedGrepSearch.Hide;
  Execute(gssSearchEmbedded);
end;

procedure TfmGrepResults.actHistoryRefreshExecute(Sender: TObject);
var
  AItem: TGrepHistoryListItem;
  ASaveSettings: TGrepSettings;
begin
  AItem := GrepExpert.HistoryList.Items[pmHistoryMenu.Tag];
  if not Assigned(AItem) then
    Exit;

  ASaveSettings := FGrepSettings;
  FGrepSettings := AItem.GrepSettings;
  if Sender = actHistoryRefresh then
    actFileRefresh.Execute
  else if Sender = actHistorySearch then
  begin
    if Execute(gssSearchAgain) then
      RefreshContextLines;
  end
  else if Sender = actHistoryModifySearchSettings then
  begin
    try
      Execute(gssModifySearchSettings);
    finally
      FGrepSettings := ASaveSettings;
    end;
  end;
end;

procedure TfmGrepResults.actFileSaveExecute(Sender: TObject);
resourcestring
  rsSaveAllCaption = 'Select the items for save';
  rsSaveProgress = 'Grep save progress';
  rsPrintAllCaption = 'Select the items for print to file';
  rsPrintProgress = 'Grep print to file progress';
  rsSavePrintAllCaption = 'Select the items for save & print';
  rsSavePrintProgress = 'Grep save & print progress';
const
  cSelectTypeByMode: array[TSaveToFileMode] of TGrepSelectType = (gstPrint, gstSave, gstSavePrint);
  cModeCaption: array[TSaveToFileMode] of string = (rsPrintAllCaption, rsSaveAllCaption, rsSavePrintAllCaption);
  cModeProgress: array[TSaveToFileMode] of string = (rsPrintProgress, rsSaveProgress, rsSavePrintProgress);
var
  AMode: TSaveToFileMode;
  ASelResult: TGrepSelectResult;
  AFileCount, ASplitCount: Integer;
begin
  if lbHistoryList.Count = 0 then
    Exit;

 if Sender = actFilePrintToFile then
    AMode := sfPrintToFile
  else if Sender = actFileSavePrint then
    AMode := sfBoth
  else
    AMode := sfSaveToLoadable;

  ASelResult := HistoryListSelect(cSelectTypeByMode[AMode], cModeCaption[AMode]);

  if ASelResult = gsrNoSelection then
    Exit;

  if FSaveSplitCount > 0 then
  begin
    AFileCount := (FSelectedCount div FSaveSplitCount) + 1;
    ASplitCount := FSaveSplitCount;
  end
  else
  begin
    AFileCount := 1;
    ASplitCount := FSelectedCount;
  end;
  if AMode = sfBoth then
    AFileCount := 2 * AFileCount;

  fmGrepProgress := TfmGrepProgress.Create(Self);
  try
    fmGrepProgress.Init(cModeProgress[AMode], AFileCount, ASplitCount);
    SaveGrepResultsToFile(Self, GrepExpert.HistoryList, GrepExpert.HistoryIniVersion,
      ASelResult = gsrSelectAll, AMode, grFile, 'GxGrep.' + TGrepHistoryList.KeyName, FSaveSplitCount);
  finally
    FreeAndNil(fmGrepProgress);
  end;
end;

procedure TfmGrepResults.ClearResultsData;
begin
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.Hide;
  reContext.Clear;
  ContractList(False);
  SetStatusString('');
  SetMatchString('');
  ClearResultsListbox;
end;

procedure TfmGrepResults.actHistoryDeleteExecute(Sender: TObject);
var
  AIndex, ATopIndex: Integer;
  IsCurrent: Boolean;
begin
  AIndex := pmHistoryMenu.Tag;
  if AIndex = -1 then
   Exit;

  IsCurrent := AIndex = lbHistoryList.ItemIndex;
  if IsCurrent then
    ClearResultsData;

  GrepExpert.HistoryListDeleteFromSettings(delOneItem, AIndex);

  GrepExpert.HistoryList.Delete(AIndex);

  ATopIndex := lbHistoryList.TopIndex;
  lbHistoryList.Count := GrepExpert.HistoryList.Count;
  if AIndex >= lbHistoryList.Count then
    Dec(AIndex);
  if (ATopIndex < lbHistoryList.Count) and (AIndex < lbHistoryList.Count-1) then
    lbHistoryList.TopIndex := ATopIndex;

  if IsCurrent and (AIndex > -1) then
  begin
    lbHistoryList.ItemIndex := AIndex;
    ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  end ;
end;

procedure TfmGrepResults.actHistoryDeleteSelectedExecute(Sender: TObject);
resourcestring
  rsDeleteSelectedCaption = 'Select the items for delete';
const
  cSelResult2DelMode: array[gsrSelectItems..gsrSelectAll] of TGrepDeleteMode = (delSelected, delAll);
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstDelete, rsDeleteSelectedCaption);

  if (ASelResult = gsrSelectAll) and (GrepExpert.HistoryList.ListMode <> hlmAll) then
    ASelResult := gsrSelectItems;

  if (ASelResult = gsrNoSelection) or
    (ShowGxMessageBox(TGxResultDeleteSelectedQuestion, IfThen(ASelResult = gsrSelectAll, 'A')) <> mrYes)
  then
    Exit;

  GrepExpert.HistoryListDeleteFromSettings(cSelResult2DelMode[ASelResult]);

  if ASelResult = gsrSelectAll then
  begin
    ClearResultsData;
    lbHistoryList.Count := 0;
    GrepExpert.HistoryList.Clear;
  end
  else
  begin
    GrepExpert.HistoryList.DeleteSelected;
    lbHistoryList.Count := GrepExpert.HistoryList.Count;
    lbHistoryList.ItemIndex := lbHistoryList.Count - 1;
    ViewHistoryListItems(lbHistoryList.ItemIndex, True);
  end;
end;

procedure TfmGrepResults.actHistoryModifySaveOptionsExecute(Sender: TObject);
resourcestring
  rsSaveOptionAllCaption = 'Select the items for modify save option';
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstSaveOptions, rsSaveOptionAllCaption);
  if ASelResult <> gsrNoSelection then
    lbHistoryList.Refresh;
end;

//Commented, because its not yet work
procedure TfmGrepResults.actHistoryRefreshSelectedExecute(Sender: TObject);
//resourcestring
//  rsRefreshSelectedCaption = 'Choose refreshed items';
//var
//  ASelResult: TGrepSelectResult;
begin
//  if lbHistoryList.Count = 0 then
    Exit;

//  ASelResult := HistoryListSelect(gstRefresh, rsRefreshSelectedCaption);
//
//  if (ASelResult = gsrNoSelection) or
//    ( ShowGxMessageBox(TGxResultRefreshSelectedQuestion//, IfThen(ASelResult = gsrSelectAll, 'A')) <> mrYes )
//  then
//    Exit;
//
//  lbHistoryList.ItemIndex := 0;
//  if ASelResult = gsrSelectAll then
//    Execute(gssRefreshAll)
//  else
//    Execute(gssRefreshSelected);
//
//  ViewHistoryListItems(lbHistoryList.ItemIndex, True);
//  RefreshContextLines;
end;

procedure TfmGrepResults.actHistorySortExecute(Sender: TObject);
resourcestring
  rsSortCaption = 'Sort history items';
var
  ASelResult: TGrepSelectResult;
begin
  if lbHistoryList.Count = 0 then
    Exit;

  ASelResult := HistoryListSelect(gstSort, rsSortCaption);
  if ASelResult <> gsrNoSelection then
  begin
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    GrepExpert.HistoryList.SortWithOptions(FNewSortMode, FNewSortDesc);
    GetSavedHistoryIndex;
    lbHistoryList.Refresh;
  end ;
end;

procedure TfmGrepResults.actHistorySearchInHistoryExecute(Sender: TObject);
resourcestring
  rsSearchInCaption = 'Search in the history list';
var
  ASelResult: TGrepSelectResult;
begin
  ASelResult := HistoryListSelect(gstSearchInHistory, rsSearchInCaption);
  if ASelResult <> gsrNoSelection then
  begin
    SetSavedHistoryIndexes(lbHistoryList.ItemIndex);
    GrepExpert.HistoryList.UpdateSearchList(FSearchInClearSearchList);
    SetHistoryListMode(hlmSearch, True, False, True);
    GrepExpert.HistoryListSaveSearchListSettings;
  end ;
end;

procedure TfmGrepResults.reContextContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  actContextSelSearch.Enabled := reContext.SelText <> '';
end;

procedure TfmGrepResults.actContextSelSearchExecute(Sender: TObject);
begin
  FContextSearchText := reContext.SelText;
  Execute(gssNormal);
  FContextSearchText := '';
end;

function TfmGrepResults.HistoryListSelect(ASelectType: TGrepSelectType; ACaption: String): TGrepSelectResult;
var
  frmSelect: TfmGrepSelect;
begin
  frmSelect := TfmGrepSelect.Create(Self);
  try
    FSaveSplitCount := -1;
    FNewSortMode := ghsUnsorted;
    FNewSortDesc := False;
    FSearchInClearSearchList := True;

    frmSelect.Init(GrepExpert.HistoryList, ASelectType, ACaption);
    Result := frmSelect.Execute;
    FSelectedCount := frmSelect.SelectedCount;

    case ASelectType of
      gstSave:
        if frmSelect.cbSaveSplitToMoreFiles.Checked then
          FSaveSplitCount := StrToIntDef(frmSelect.eSaveSplitCount.Text, -1);
      gstSort:
      begin
        FNewSortMode := frmSelect.SortMode;
        FNewSortDesc := frmSelect.SortDesc;
      end;
      gstSearchInHistory:
        FSearchInClearSearchList := frmSelect.cbSearchInClearSearchList.Checked;
//      gstDelete:
//        FDeleteSelectedMoveToParamsPage := frmSelect.cbMoveToParamsPage.Checked;
    end;
  finally
    frmSelect.Free;
  end;
end;

procedure TfmGrepResults.SplitterHistoryListMoved(Sender: TObject);
begin
  if Assigned(FEmbeddedGrepSearch) then
    FEmbeddedGrepSearch.EmbeddedUpdatePos;
end;

end.

