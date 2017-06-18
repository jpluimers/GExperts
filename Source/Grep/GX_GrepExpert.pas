{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepExpert;

interface

uses
  Classes, Graphics,
  GX_Experts, GX_ConfigurationInfo, GX_GrepBackend, IniFiles;

type
  TGrepExpert = class(TGX_Expert)
  private
    FHistoryIniVersion: Integer; //0: old, 1: renamed new, 2: multiINI/indexed new
    FGrepMiddle: Boolean;
    FGrepExpandAll: Boolean;
    FGrepExpandIf: Boolean;
    FGrepExpandIfFiles: Integer;
    FGrepExpandIfMatches: Integer;
    FGrepExpandFew: Boolean;
    FGrepExpandFewLines: Integer;
    FSearchList: TStrings;
    FReplaceList: TStrings;
    FMaskList: TStrings;
    FDirList: TStrings;
    FExcludedDirsList: TStrings;
    FGrepCaseSensitive: Boolean;
    FGrepCode: Boolean;
    FGrepStrings: Boolean;
    FGrepComments: Boolean;
    FGrepInterface: Boolean;
    FGrepImplementation: Boolean;
    FGrepInitialization: Boolean;
    FGrepFinalization: Boolean;
    FGrepForms: Boolean;
    FGrepSQLFiles: Boolean;
    FGrepSearch: Integer;
    FGrepSub: Boolean;
    FGrepWholeWord: Boolean;
    FGrepRegEx: Boolean;
    FGrepSaveOption: TGrepSaveOption;
    FGrepUseCurrentIdent: Boolean;
    FNumContextLines: Integer;
    FListFont: TFont;
    FListUseDefaultColors: Boolean;
    FListMatchTextColor: TColor;
    FListMatchBrushColor: TColor;
    FContextFont: TFont;
    FContextMatchColor: TColor;
    FAutoHide: Boolean;
    FContextMatchLineColor: TColor;
    FGrepSaveHistoryListItems: Integer;
    FHistoryList: TGrepHistoryList;
    FContextSaveFixedHeight: Boolean;
    FGrepOnlySaveParamsAction: Integer;
    FGrepFileListDeleteAfterDays: Boolean;
    FGrepHistoryListDefaultPage: Integer;
    FGrepDeleteAfterDays: Integer;
    FGrepSaveOptionDefaultValue: Integer;
    FGrepOpenSaveOptionDefaultValue: Integer;
    FGrepEmptyMoveToOnlySaveParams: Boolean;
    FGrepAdvancedOptions: Boolean;
    FGrepQuickRefresh: Boolean;
    FGrepHistoryPagesTabMultiline: Boolean;
    FGrepHistoryPagesTabWidth: Integer;
    FGrepMouseWheelPrevNextMatch: Boolean;
    function  GetGrepSaveHistoryListItems(AIndex: Integer): Boolean;
    procedure SetSearchList(New: TStrings);
    procedure SetReplaceList(New: TStrings);
    procedure SetMaskList(New: TStrings);
    procedure SetDirList(New: TStrings);
    procedure SetExcludedDirsList(const Value: TStrings);
    procedure LoadHistoryList(AGrepSettings : TGrepSettings);
    function  FillGrepSettings: TGrepSettings;
    function  GetSaveOption: TGrepSaveOption;
    function  GetOpenSaveOption: TGrepSaveOption;
  protected
    function  CreateSettings: TCustomIniFile;
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ShowModal;
    procedure ShowStandAlone(const _Directory: string);
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;

    function  GrepConfigPath: String;
    function  GrepHistorySettingsFileName: String;

    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    procedure HistoryListSaveSettings(AItemIndex: Integer = -1); //if -1 then all
    procedure HistoryListSaveSearchListSettings;
    procedure HistoryListDeleteFromSettings(ADelMode: TGrepDeleteMode; AItemIndex: Integer = -1); //if -1 then all

    property GrepMiddle: Boolean read FGrepMiddle write FGrepMiddle;
    property GrepExpandAll: Boolean read FGrepExpandAll write FGrepExpandAll;
    property GrepExpandIf: Boolean read FGrepExpandIf write FGrepExpandIf;
    property GrepExpandIfFiles: Integer read FGrepExpandIfFiles write FGrepExpandIfFiles;
    property GrepExpandIfMatches: Integer read FGrepExpandIfMatches write FGrepExpandIfMatches;
    property GrepExpandFew: Boolean read FGrepExpandFew write FGrepExpandFew;
    property GrepExpandFewLines: Integer read FGrepExpandFewLines write FGrepExpandFewLines;
    property GrepCaseSensitive: Boolean read FGrepCaseSensitive write FGrepCaseSensitive;
    property GrepCode: Boolean read FGrepCode write FGrepCode;
    property GrepStrings: Boolean read FGrepStrings write FGrepStrings;
    property GrepComments: Boolean read FGrepComments write FGrepComments;
    property GrepInterface: Boolean read FGrepInterface write FGrepInterface;
    property GrepImplementation: Boolean read FGrepImplementation write FGrepImplementation;
    property GrepInitialization: Boolean read FGrepInitialization write FGrepInitialization;
    property GrepFinalization: Boolean read FGrepFinalization write FGrepFinalization;
    property GrepForms: Boolean read FGrepForms write FGrepForms;
    property GrepSQLFiles: Boolean read FGrepSQLFiles write FGrepSQLFiles;
    property GrepSearch: Integer read FGrepSearch write FGrepSearch;
    property GrepSub: Boolean read FGrepSub write FGrepSub;
    property GrepWholeWord: Boolean read FGrepWholeWord write FGrepWholeWord;
    property GrepRegEx: Boolean read FGrepRegEx write FGrepRegEx;
    property GrepSaveOption: TGrepSaveOption read FGrepSaveOption write FGrepSaveOption;
    property GrepUseCurrentIdent: Boolean read FGrepUseCurrentIdent write FGrepUseCurrentIdent;
    property NumContextLines: Integer read FNumContextLines write FNumContextLines;

    property GrepAdvancedOptions: Boolean read FGrepAdvancedOptions write FGrepAdvancedOptions;
    property GrepSaveOptionDefaultValue: Integer read FGrepSaveOptionDefaultValue write FGrepSaveOptionDefaultValue;
    property GrepOpenSaveOptionDefaultValue: Integer read FGrepOpenSaveOptionDefaultValue write FGrepOpenSaveOptionDefaultValue;
    property GrepFileListDeleteAfterDays: Boolean read FGrepFileListDeleteAfterDays write FGrepFileListDeleteAfterDays;
    property GrepDeleteAfterDays: Integer read FGrepDeleteAfterDays write FGrepDeleteAfterDays;
    property GrepEmptyMoveToOnlySaveParams: Boolean read FGrepEmptyMoveToOnlySaveParams write FGrepEmptyMoveToOnlySaveParams;
    property GrepOnlySaveParamsAction: Integer read FGrepOnlySaveParamsAction write FGrepOnlySaveParamsAction;
    property GrepHistoryListDefaultPage: Integer read FGrepHistoryListDefaultPage write FGrepHistoryListDefaultPage;
    property GrepQuickRefresh: Boolean read FGrepQuickRefresh write FGrepQuickRefresh;
    property GrepHistoryPagesTabMultiline: Boolean read FGrepHistoryPagesTabMultiline write FGrepHistoryPagesTabMultiline;
    property GrepHistoryPagesTabWidth: Integer read FGrepHistoryPagesTabWidth write FGrepHistoryPagesTabWidth;
    property GrepMouseWheelPrevNextMatch: Boolean read FGrepMouseWheelPrevNextMatch write FGrepMouseWheelPrevNextMatch;

    property SaveOption: TGrepSaveOption read GetSaveOption;
    property OpenSaveOption: TGrepSaveOption read GetOpenSaveOption;

    property ListFont: TFont read FListFont write FListFont;
    property ListUseDefaultColors: Boolean read FListUseDefaultColors write FListUseDefaultColors;
    property ListMatchTextColor: TColor read FListMatchTextColor write FListMatchTextColor;
    property ListMatchBrushColor: TColor read FListMatchBrushColor write FListMatchBrushColor;
    property ContextFont: TFont read FContextFont write FContextFont;
    property ContextMatchColor: TColor read FContextMatchColor write FContextMatchColor;
    property ContextMatchLineColor: TColor read FContextMatchLineColor write FContextMatchLineColor;
    property AutoHide: Boolean read FAutoHide write FAutoHide;

    property GrepSaveHistoryListItems: Boolean index 3 read GetGrepSaveHistoryListItems;
    property GrepSaveHistoryListItemsToIni: Boolean index 1 read GetGrepSaveHistoryListItems;
    property GrepSaveHistoryListItemsToReg: Boolean index 2 read GetGrepSaveHistoryListItems;

    property ContextSaveFixedHeight: Boolean read FContextSaveFixedHeight write FContextSaveFixedHeight;

    property SearchList: TStrings read FSearchList write SetSearchList;
    property ReplaceList: TStrings read FReplaceList write SetReplaceList;
    property MaskList: TStrings read FMaskList write SetMaskList;
    property DirList: TStrings read FDirList write SetDirList;
    property ExcludedDirsList: TStrings read FExcludedDirsList write SetExcludedDirsList;

    property HistoryIniVersion: Integer read FHistoryIniVersion;
    property HistoryList: TGrepHistoryList read FHistoryList;
  end;

var
  GrepStandAlone: TGrepExpert = nil;

procedure ShowGrep; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure ShowGrepExW(const _Directory: PWideChar); {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure ShowGrepExA(const _Directory: PAnsiChar); {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

uses
  SysUtils, Menus, Controls, ComCtrls,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  GX_OtaUtils, GX_GenericUtils,
  GX_GrepResults, GX_GrepResultsOptions,
  GX_IdeDock, GX_GExperts;

{ TGrepExpert }

constructor TGrepExpert.Create;
begin
  inherited Create;
  FSearchList := TStringList.Create;
  FReplaceList := TStringList.Create;
  FMaskList := TStringList.Create;
  FDirList := TStringList.Create;
  FExcludedDirsList := TStringList.Create;

  FListFont := TFont.Create;
  FListUseDefaultColors := True;
  FListMatchTextColor := clHighlightText;
  FListMatchBrushColor := clHighlight;
  FContextFont := TFont.Create;
  FContextMatchColor := clHighlight;
  FContextMatchLineColor := clHighlight;

  FNumContextLines := 2;
  FAutoHide := False;

  FHistoryList := TGrepHistoryList.Create;

  FGrepAdvancedOptions := False;

  FGrepExpandAll := False;
  FGrepExpandIf := False;
  FGrepExpandIfFiles := 25;
  FGrepExpandIfMatches := 150;
  FGrepExpandFew := False;
  FGrepExpandFewLines := 20;
  FGrepUseCurrentIdent := False;
  FGrepSaveHistoryListItems := 0;
  FGrepHistoryPagesTabMultiline := True;

  FGrepSaveOption := gsoOnlySaveSettings;
  FGrepSaveOptionDefaultValue := Integer(gsoOnlySaveSettings);
  FGrepOpenSaveOptionDefaultValue := Integer(gsoNoSave);
  FGrepFileListDeleteAfterDays := True;
  FGrepDeleteAfterDays := 30;
  FGrepEmptyMoveToOnlySaveParams := False;
  FGrepOnlySaveParamsAction := 0;
  FGrepHistoryListDefaultPage := 0;
  FGrepQuickRefresh := False;

  FContextSaveFixedHeight := False;

  FHistoryIniVersion := 0;

  fmGrepResults := TfmGrepResults.Create(nil);
  SetFormIcon(fmGrepResults);
  if not IsStandAlone then
    IdeDockManager.RegisterDockableForm(TfmGrepResults, fmGrepResults, 'fmGrepResults');
  fmGrepResults.GrepExpert := Self;
end;

destructor TGrepExpert.Destroy;
begin
  IdeDockManager.UnRegisterDockableForm(fmGrepResults, 'fmGrepResults');

  HistoryListSaveSettings;
  SaveSettings;

  FreeAndNil(FHistoryList);

  FreeAndNil(fmGrepResults);
  FreeAndNil(FSearchList);
  FreeAndNil(FReplaceList);
  FreeAndNil(FMaskList);
  FreeAndNil(FDirList);
  FreeAndNil(FExcludedDirsList);
  FreeAndNil(FListFont);
  FreeAndNil(FContextFont);

  inherited Destroy;
end;

function TGrepExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Grep &Results';
begin
  Result := SMenuCaption;
end;

function TGrepExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Word('R'), [ssCtrl, ssAlt]);
end;

class function TGrepExpert.GetName: string;
begin
  Result := 'GrepResults';
end;

procedure TGrepExpert.Execute(Sender: TObject);
begin
  SetFormIcon(fmGrepResults);
  IdeDockManager.ShowForm(fmGrepResults);
  EnsureFormVisible(fmGrepResults);
end;

procedure TGrepExpert.ShowModal;
begin
  fmGrepResults.ShowModal;
end;

procedure TGrepExpert.ShowStandAlone(const _Directory: string);
begin
  AddMRUString(_Directory, FDirList, True);
  fmGrepResults.Execute(gssNormal);
end;

procedure TGrepExpert.Configure;
var
  Dialog: TfmGrepResultsOptions;
begin
  Dialog := TfmGrepResultsOptions.Create(nil);
  try
    Dialog.chkAdvanced.Checked := GrepAdvancedOptions;
    Dialog.chkAdvancedClick(nil);

    Dialog.chkGrepMiddle.Checked := GrepMiddle;
    Dialog.chkGrepExpandAll.Checked := GrepExpandAll;
    Dialog.chkGrepExpandIf.Checked := GrepExpandIf;
    Dialog.eExpandIfFiles.Text := IntToStr(GrepExpandIfFiles);
    Dialog.eExpandIfMatches.Text := IntToStr(GrepExpandIfMatches);
    Dialog.chkGrepExpandFew.Checked := GrepExpandFew;
    Dialog.eExpandFewLines.Text := IntToStr(GrepExpandFewLines);

    Dialog.chkDefaultListColors.Checked := ListUseDefaultColors;
    Dialog.pnlListFont.Font.Assign(ListFont);
    Dialog.pnlListMatchTextColor.Font.Assign(ListFont);
    Dialog.pnlListMatchTextColor.Font.Color := ListMatchTextColor;
    Dialog.pnlListMatchTextColor.Color := ListMatchBrushColor;
    Dialog.pnlListMatchBackgroundColor.Font.Assign(ListFont);
    Dialog.pnlListMatchBackgroundColor.Font.Color := ListMatchTextColor;
    Dialog.pnlListMatchBackgroundColor.Color := ListMatchBrushColor;

    Dialog.pnlContextFont.Font.Assign(ContextFont);
    Dialog.pnlContextMacthLineFontColor.Font.Assign(ContextFont);
    Dialog.pnlContextMacthLineFontColor.Font.Color := ContextMatchLineColor;
    Dialog.pnlContextMatchFontColor.Font.Assign(ContextFont);
    Dialog.pnlContextMatchFontColor.Font.Color := ContextMatchColor;

    Dialog.udContextLines.Position := NumContextLines;
    Dialog.chkGrepSaveHistoryListItems.Checked := GrepSaveHistoryListItems;
    Dialog.rbSaveToIniFile.Checked := GrepSaveHistoryListItemsToIni;
    Dialog.rbSaveToRegistry.Checked := GrepSaveHistoryListItemsToReg;

    Dialog.chkGrepAutoHide.Checked := AutoHide;

    Dialog.chkFileListDeleteAfterDays.Checked := GrepFileListDeleteAfterDays;
    Dialog.eDeleteAfterDays.Text := IntToStr(GrepDeleteAfterDays);
    Dialog.chkEmptyMoveToParams.Checked := GrepEmptyMoveToOnlySaveParams;
    Dialog.cbxSearchSaveOptionDefaultValue.ItemIndex := GrepSaveOptionDefaultValue;
    Dialog.cbxOpenSaveOptionDefaultValue.ItemIndex := GrepOpenSaveOptionDefaultValue;
    Dialog.cbxOnlySaveParamsAction.ItemIndex := GrepOnlySaveParamsAction;
    Dialog.cbxHistoryListDefaultPage.ItemIndex := GrepHistoryListDefaultPage;
    Dialog.chkQuickRefreshMode.Checked := GrepQuickRefresh;

    Dialog.chkHistoryPagesTabMultiLine.Checked := GrepHistoryPagesTabMultiline;
    Dialog.eHistoryPagesTabWidth.Text := IntToStr(GrepHistoryPagesTabWidth);
    Dialog.chkMouseWheelMoveItemIndex.Checked := GrepMouseWheelPrevNextMatch;

    Dialog.chkSaveContextFixedHeight.Checked := ContextSaveFixedHeight;

    if Dialog.ShowModal = mrOk then
    begin
      GrepAdvancedOptions := Dialog.chkAdvanced.Checked;

      GrepMiddle := Dialog.chkGrepMiddle.Checked;
      GrepExpandAll := Dialog.chkGrepExpandAll.Checked;
      GrepExpandIf := GrepAdvancedOptions and Dialog.chkGrepExpandIf.Checked;
      GrepExpandIfFiles := StrToIntDef(Dialog.eExpandIfFiles.Text, 25);
      GrepExpandIfMatches := StrToIntDef(Dialog.eExpandIfMatches.Text, 150);
      GrepExpandFew := GrepAdvancedOptions and Dialog.chkGrepExpandFew.Checked;
      GrepExpandFewLines := StrToIntDef(Dialog.eExpandFewLines.Text, 20);

      ListUseDefaultColors := Dialog.chkDefaultListColors.Checked;
      FListFont.Assign(Dialog.pnlListFont.Font);
      FContextFont.Assign(Dialog.pnlContextFont.Font);
      ListMatchTextColor := Dialog.pnlListMatchTextColor.Font.Color;
      ListMatchBrushColor := Dialog.pnlListMatchBackgroundColor.Color;
      ContextMatchLineColor := Dialog.pnlContextMacthLineFontColor.Font.Color;
      ContextMatchColor := Dialog.pnlContextMatchFontColor.Font.Color;

      NumContextLines := Dialog.udContextLines.Position;
      if GrepAdvancedOptions then
      begin
        if not Dialog.chkGrepSaveHistoryListItems.Checked then
          FGrepSaveHistoryListItems := 0
        else if Dialog.rbSaveToIniFile.Checked then
          FGrepSaveHistoryListItems := 1
        else if Dialog.rbSaveToRegistry.Checked then
          FGrepSaveHistoryListItems := 2;
      end
      else
      begin
        if not Dialog.chkGrepSaveHistoryListItems.Checked then
          FGrepSaveHistoryListItems := 0
        else
          FGrepSaveHistoryListItems := 1;
      end;

      GrepFileListDeleteAfterDays := Dialog.chkFileListDeleteAfterDays.Checked;
      GrepDeleteAfterDays := StrToIntDef(Dialog.eDeleteAfterDays.Text, 30);
      GrepSaveOptionDefaultValue := Dialog.cbxSearchSaveOptionDefaultValue.ItemIndex;
      GrepOpenSaveOptionDefaultValue := Dialog.cbxOpenSaveOptionDefaultValue.ItemIndex;
      GrepEmptyMoveToOnlySaveParams := GrepAdvancedOptions and Dialog.chkEmptyMoveToParams.Checked;
      GrepOnlySaveParamsAction := Dialog.cbxOnlySaveParamsAction.ItemIndex;
      GrepHistoryListDefaultPage := Dialog.cbxHistoryListDefaultPage.ItemIndex;
      GrepQuickRefresh := Dialog.chkQuickRefreshMode.Checked;

      if GrepAdvancedOptions then
      begin
        GrepHistoryPagesTabMultiline := Dialog.chkHistoryPagesTabMultiLine.Checked;
        GrepHistoryPagesTabWidth := StrToIntDef(Dialog.eHistoryPagesTabWidth.Text, GrepHistoryPagesTabWidth);
      end;

      GrepMouseWheelPrevNextMatch := GrepAdvancedOptions and Dialog.chkMouseWheelMoveItemIndex.Checked;

      ContextSaveFixedHeight := GrepAdvancedOptions and Dialog.chkSaveContextFixedHeight.Checked;

      AutoHide := DIalog.chkGrepAutoHide.Checked;
      SaveSettings;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TGrepExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);
  // do not localize any of the following lines
  Settings.WriteInteger( 'HistoryIniVersion', FHistoryIniVersion);

  Settings.WriteBool('CaseSensitive', GrepCaseSensitive);
  Settings.WriteBool('Code', GrepCode);
  Settings.WriteBool('Strings', GrepStrings);
  Settings.WriteBool('NoComments', not GrepComments);
  Settings.WriteBool('Interface', GrepInterface);
  Settings.WriteBool('Implementation', GrepImplementation);
  Settings.WriteBool('Initialization', GrepInitialization);
  Settings.WriteBool('Finalization', GrepFinalization);
  Settings.WriteBool('Forms', GrepForms);
  Settings.WriteBool('SQLFiles', GrepSQLFiles);
  Settings.WriteInteger('Search', GrepSearch);
  Settings.WriteBool('SubDirectories', GrepSub);
  Settings.WriteBool('ExpandAll', GrepExpandAll);
  Settings.WriteBool('ExpandIf', GrepExpandIf);
  Settings.WriteInteger('ExpandIfFiles', GrepExpandIfFiles);
  Settings.WriteInteger('ExpandIfMatches', GrepExpandIfMatches);
  Settings.WriteBool('ExpandFew', GrepExpandFew);
  Settings.WriteInteger('ExpandFewLines', GrepExpandFewLines);
  Settings.WriteBool('Whole Word', GrepWholeWord);
  Settings.WriteBool('Middle', GrepMiddle);
  Settings.WriteBool('AutoHide', AutoHide);
  Settings.WriteBool('RegEx', GrepRegEx);
  Settings.WriteInteger('SaveOption', Integer(GrepSaveOption));
  Settings.WriteBool('UseCurrentIdent', GrepUseCurrentIdent);

  Settings.WriteBool('AdvancedOptions', GrepAdvancedOptions);
  Settings.WriteInteger('SaveOptionDeafult', GrepSaveOptionDefaultValue);
  Settings.WriteInteger('SaveOptionDeafult4Open', GrepOpenSaveOptionDefaultValue);
  Settings.WriteBool('FileListDeleteAfterDays', GrepFileListDeleteAfterDays);
  Settings.WriteInteger('DeleteAfterDays', GrepDeleteAfterDays);
  Settings.WriteBool('EmptyResultsMoveToOnlySaveParams', GrepEmptyMoveToOnlySaveParams);
  Settings.WriteInteger('OnlySaveParamsAction', GrepOnlySaveParamsAction);
  Settings.WriteInteger('HistoryListDefaultPage', GrepHistoryListDefaultPage);
  Settings.WriteBool('QuickRefresh', GrepQuickRefresh);

  Settings.WriteBool('ListUseDefaultColors', ListUseDefaultColors);
  Settings.SaveFont('ListFont', ListFont, [ffColor]);
  Settings.WriteInteger('ListMatchTextColor', ListMatchTextColor);
  Settings.WriteInteger('ListMatchBrushColor', ListMatchBrushColor);
  Settings.SaveFont('ContextFont', ContextFont, [ffColor]);
  Settings.WriteInteger('ContextMatchColor', ContextMatchColor);
  Settings.WriteInteger('ContextMatchLineColor', ContextMatchLineColor);

  Settings.WriteInteger('NumContextLines', NumContextLines);
  Settings.WriteInteger('SaveHistoryListItems', FGrepSaveHistoryListItems);
  Settings.WriteBool('ContextSaveFixedHeight', ContextSaveFixedHeight);

  Settings.WriteBool('HistoryPagesTabMultilin', GrepHistoryPagesTabMultiline);
  Settings.WriteInteger('HistoryPagesTabWidth', GrepHistoryPagesTabWidth);
  Settings.WriteBool('MouseWheelPrevNextMatch', GrepMouseWheelPrevNextMatch);

  Settings.WriteStrings('DirectoryList', DirList, 'GrepDir');
  Settings.WriteStrings('SearchList', SearchList, 'GrepSearch');
  Settings.WriteStrings('ReplaceList', ReplaceList, 'GrepReplace');
  Settings.WriteStrings('MaskList', MaskList, 'GrepMask');
  Settings.WriteStrings('ExcludedDirsList', ExcludedDirsList, 'GrepExcludedDirs');
end;

function TGrepExpert.FillGrepSettings: TGrepSettings;
begin
  Result.CaseSensitive := GrepCaseSensitive;
  Result.WholeWord := GrepWholeWord;
  Result.RegEx := GrepRegEx;
  Result.Pattern := '';
  Result.IncludeForms := GrepForms;
  Result.IncludeSQLs := GrepSQLFiles;
  Result.SaveOption := GrepSaveOption;
  Result.Mask := '';
  Result.Directories := '';
  Result.ExcludedDirs := '';
  Result.IncludeSubdirs := GrepSub;

  Result.IncludeCode := GrepCode;
  Result.IncludeStrings := GrepStrings;
  Result.IncludeComments := GrepComments;

  Result.SectionInterface := GrepInterface;
  Result.SectionImplementation := GrepImplementation;
  Result.SectionInitialization := GrepInitialization;
  Result.SectionFinalization := GrepFinalization;

  case GrepSearch of
    0: Result.GrepAction := gaCurrentOnlyGrep;
    1: Result.GrepAction := gaProjGrep;
    2: Result.GrepAction := gaOpenFilesGrep;
    3: begin
      Result.GrepAction := gaDirGrep;
      if MaskList.Count > 0 then
        Result.Mask := MaskList[0];
      if DirList.Count > 0 then
        Result.Directories := DirList[0];
      if ExcludedDirsList.Count > 0 then
        Result.ExcludedDirs := ExcludedDirsList[0];
    end;
    4: Result.GrepAction := gaProjGroupGrep;
    5: Result.GrepAction := gaResults;
  else
    Result.GrepAction := gaProjGrep;
  end;
end;

function TGrepExpert.GrepConfigPath: String;
begin
  Result := AddSlash(ConfigInfo.ConfigPath);
  if FHistoryIniVersion >= 2 then
    Result := AddSlash(Result + ConfigurationKey + '.' + TGrepHistoryList.KeyName);
end;

function TGrepExpert.GrepHistorySettingsFileName: String;
begin
  if FHistoryIniVersion = 0 then
    Result := 'GrepFound.ini'
  else
    Result := TGrepHistoryList.SettingsFileName;
end;

function TGrepExpert.CreateSettings: TCustomIniFile;
begin
  Result := nil;
  case FGrepSaveHistoryListItems of
    1: begin
      ForceDirectories(GrepConfigPath);
      Result := TGrepIniFile.Create(GrepConfigPath + GrepHistorySettingsFileName);
    end;
    2: Result := TGExpertsSettings.Create;
  end;
end;

procedure TGrepExpert.LoadHistoryList(AGrepSettings : TGrepSettings);
var
  Settings: TCustomIniFile;
  BaseKey: String;
  AIniMode: TIniFileMode;
begin
  if not GrepSaveHistoryListItems then
    Exit;

  BaseKey := '';
  if GrepSaveHistoryListItemsToReg then
    BaseKey := ConfigurationKey + PathDelim;

  AIniMode := ifmMulti;
  if FHistoryIniVersion < 2 then
    AIniMode := ifmSingle;

  Settings := CreateSettings;
  try
    HistoryList.LoadFromSettings(AGrepSettings, Settings, HistoryIniVersion, AIniMode, BaseKey, SaveOption);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGrepExpert.HistoryListSaveSettings(AItemIndex: Integer);
var
  Settings: TCustomIniFile;
  BaseKey: String;
begin
  if not GrepSaveHistoryListItems then
    Exit;

  BaseKey := '';
  if GrepSaveHistoryListItemsToReg then
    BaseKey := ConfigurationKey + PathDelim;

  Settings := CreateSettings;
  try
    HistoryList.SaveToSettings(Settings, HistoryIniVersion, BaseKey, AItemIndex,
      GrepEmptyMoveToOnlySaveParams, GrepFileListDeleteAfterDays, GrepDeleteAfterDays);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGrepExpert.HistoryListSaveSearchListSettings;
var
  Settings: TCustomIniFile;
  BaseKey: String;
begin
  if not GrepSaveHistoryListItems then
    Exit;

  BaseKey := '';
  if GrepSaveHistoryListItemsToReg then
    BaseKey := ConfigurationKey + PathDelim;

  Settings := CreateSettings;
  try
    HistoryList.SaveSearchListToSettings(Settings, BaseKey);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGrepExpert.HistoryListDeleteFromSettings(ADelMode: TGrepDeleteMode; AItemIndex: Integer);
var
  Settings: TCustomIniFile;
  BaseKey: String;
begin
  if not GrepSaveHistoryListItems then
    Exit;

  BaseKey := '';
  if GrepSaveHistoryListItemsToReg then
    BaseKey := ConfigurationKey + PathDelim;

  //if you delete one file must be at least
  if GrepSaveHistoryListItemsToIni and ((ADelMode <> delOneItem) or (HistoryIniVersion >= 2)) and
    (HistoryList.ListMode <> hlmSearch)
  then
    HistoryList.DeleteINIFiles(GrepConfigPath + GrepHistorySettingsFileName, ADelMode, HistoryIniVersion, AItemIndex)
  else //only deleting keys
  begin
    Settings := CreateSettings;
    try
      HistoryList.RemoveFromSettings(Settings, BaseKey, ADelMode, AItemIndex);
    finally
      FreeAndNil(Settings);
    end;
  end;
end;

procedure TGrepExpert.InternalLoadSettings(Settings: TExpertSettings);

  // Build a guess for the RTL path from a passed in VCL path.
  function RtlPath(const VisualPath: string): string;
  const
    cCLX = 'clx';
    cVCL = 'vcl';
    cRTL = 'rtl';
  var
    SubPos: Integer;
  begin
    Result := '';

    SubPos := AnsiCaseInsensitivePos(cVCL, VisualPath);
    if SubPos > 0 then
    begin
      Result := VisualPath;
      Delete(Result, SubPos, Length(cVCL));
      Insert(cRTL, Result, SubPos);
    end;

    if Result <> '' then
      Exit;

    SubPos := AnsiCaseInsensitivePos(cCLX, VisualPath);
    if SubPos > 0 then
    begin
      Result := VisualPath;
      Delete(Result, SubPos, Length(cCLX));
      Insert(cRTL, Result, SubPos);
    end;
  end;

var
  TempPath: string;
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the following lines
  FHistoryIniVersion := Settings.ReadInteger('HistoryIniVersion', 0);

  FGrepCaseSensitive := Settings.ReadBool('CaseSensitive', False);
  FGrepCode := Settings.ReadBool('Code', True);
  FGrepStrings := Settings.ReadBool('Strings', True);
  FGrepComments := not Settings.ReadBool('NoComments', False);
  FGrepInterface := Settings.ReadBool('Interface', True);
  FGrepImplementation := Settings.ReadBool('Implementation', True);
  FGrepInitialization := Settings.ReadBool('Initialization', True);
  FGrepFinalization := Settings.ReadBool('Finalization', True);
  FGrepForms := Settings.ReadBool('Forms', False);
  FGrepSQLFiles := Settings.ReadBool('SQLFiles', False);
  FGrepSearch := Settings.ReadInteger('Search', 1);
  FGrepSub := Settings.ReadBool('SubDirectories', True);
  FGrepExpandAll := Settings.ReadBool('ExpandAll', False);
  FGrepExpandIf := Settings.ReadBool('ExpandIf', False);
  FGrepExpandIfFiles := Settings.ReadInteger('ExpandIfFiles', FGrepExpandIfFiles);
  FGrepExpandIfMatches := Settings.ReadInteger('ExpandIfMatches', FGrepExpandIfMatches);
  FGrepExpandFew := Settings.ReadBool('ExpandFew', False);
  FGrepExpandFewLines := Settings.ReadInteger('ExpandFewLines', FGrepExpandFewLines);
  FGrepWholeWord := Settings.ReadBool('Whole Word', True);
  FGrepMiddle := Settings.ReadBool('Middle', True);
  FAutoHide := Settings.ReadBool('AutoHide', False);
  FGrepRegEx := Settings.ReadBool('RegEx', False);
  FGrepSaveOption := TGrepSaveOption(Settings.ReadInteger('SaveOption', Integer(GrepSaveOption)));
  FGrepUseCurrentIdent := Settings.ReadBool('UseCurrentIdent', False);

  FGrepAdvancedOptions := Settings.ReadBool('AdvancedOptions', GrepAdvancedOptions);
  FGrepSaveOptionDefaultValue := Settings.ReadInteger('SaveOptionDeafult', GrepSaveOptionDefaultValue);
  FGrepOpenSaveOptionDefaultValue := Settings.ReadInteger('SaveOptionDeafult4Open', GrepOpenSaveOptionDefaultValue);
  FGrepFileListDeleteAfterDays := Settings.ReadBool('FileListDeleteAfterDays', GrepFileListDeleteAfterDays);
  FGrepDeleteAfterDays := Settings.ReadInteger('DeleteAfterDays', GrepDeleteAfterDays);
  FGrepEmptyMoveToOnlySaveParams := Settings.ReadBool('EmptyResultsMoveToOnlySaveParams', GrepEmptyMoveToOnlySaveParams);
  FGrepOnlySaveParamsAction := Settings.ReadInteger('OnlySaveParamsAction', GrepOnlySaveParamsAction);
  FGrepHistoryListDefaultPage := Settings.ReadInteger('HistoryListDefaultPage', GrepHistoryListDefaultPage);
  FGrepQuickRefresh := Settings.ReadBool('QuickRefresh', GrepQuickRefresh);

  FListUseDefaultColors := Settings.ReadBool('ListUseDefaultColors', False);
  Settings.LoadFont('ListFont', ListFont, [ffColor]);
  FListMatchTextColor :=  Settings.ReadInteger('ListMatchTextColor', FListMatchTextColor);
  FListMatchBrushColor :=  Settings.ReadInteger('ListMatchBrushColor', FListMatchBrushColor);
  Settings.LoadFont('ContextFont', ContextFont, [ffColor]);
  FContextMatchColor :=  Settings.ReadInteger('ContextMatchColor', FContextMatchColor);
  if Settings.ValueExists('ContextMatchLineColor') then
    FContextMatchLineColor := Settings.ReadInteger('ContextMatchLineColor', FContextMatchLineColor)
  else
    FContextMatchLineColor := FContextMatchColor;

  FNumContextLines :=  Settings.ReadInteger('NumContextLines', FNumContextLines);
  FContextSaveFixedHeight := Settings.ReadBool('ContextSaveFixedHeight', FContextSaveFixedHeight);

  FGrepHistoryPagesTabMultiline := Settings.ReadBool('HistoryPagesTabMultilin', GrepHistoryPagesTabMultiline);
  FGrepHistoryPagesTabWidth := Settings.ReadInteger('HistoryPagesTabWidth', GrepHistoryPagesTabWidth);
  FGrepMouseWheelPrevNextMatch := Settings.ReadBool('MouseWheelPrevNextMatch', GrepMouseWheelPrevNextMatch);

  Settings.ReadStrings('DirectoryList', DirList, 'GrepDir');
  Settings.ReadStrings('SearchList', SearchList, 'GrepSearch');
  Settings.ReadStrings('ReplaceList', ReplaceList, 'GrepReplace');
  Settings.ReadStrings('MaskList', MaskList, 'GrepMask');
  Settings.ReadStrings('ExcludedDirsList', ExcludedDirsList, 'GrepExcludedDirs');

  if FHistoryIniVersion = 0 then
    FGrepSaveHistoryListItems := Settings.ReadInteger('SaveResultListItems', 0)
  else
    FGrepSaveHistoryListItems := Settings.ReadInteger('SaveHistoryListItems', 0);

  if MaskList.Count = 0 then
  begin
    MaskList.Add('*.pas;*.dpr;*.inc');
    MaskList.Add('*.txt;*.html;*.htm;.rc;*.xml;*.todo;*.me');
    if IsStandAlone or GxOtaHaveCPPSupport then
      MaskList.Add('*.cpp;*.hpp;*.h;*.pas;*.dpr');
    if IsStandAlone or GxOtaHaveCSharpSupport then
      MaskList.Add('*.cs');
  end;
  if DirList.Count = 0 then
  begin
    TempPath := RemoveSlash(ConfigInfo.VCLPath);
    if NotEmpty(TempPath) and DirectoryExists(TempPath) then
      DirList.Add(TempPath);
    TempPath := RtlPath(ConfigInfo.VCLPath);
    if NotEmpty(TempPath) and DirectoryExists(TempPath) then
      DirList.Add(RemoveSlash(TempPath));
  end;

  fmGrepResults.InitGrepSettings(FillGrepSettings);

  LoadHistoryList(fmGrepResults.GrepSettings);

  fmGrepResults.UpdateFromSettings;

  if FHistoryIniVersion = 0 then
  begin
    HistoryListDeleteFromSettings(delAll);

    FHistoryIniVersion := 2;

    Settings.EraseSection(ConfigurationKey);

    InternalSaveSettings(Settings);
    fmGrepResults.InternalSaveSettings(Settings);

    HistoryListSaveSettings;
  end
  else if FHistoryIniVersion = 1 then
  begin
    HistoryListDeleteFromSettings(delAll);
    FHistoryIniVersion := 2;
    InternalSaveSettings(Settings);
    HistoryListSaveSettings;
  end;
end;

function TGrepExpert.GetGrepSaveHistoryListItems(AIndex: Integer): Boolean;
begin
  if AIndex = 3 then
    Result := FGrepSaveHistoryListItems in [1..2]
  else
    Result := FGrepSaveHistoryListItems = AIndex;
end;

function TGrepExpert.GetHelpString: string;
resourcestring
  SHelpString =
  '  The Grep Results window is where the results of a Grep Search are shown.'#13#10 +
  '  It also provides an interface for multi-file search and replace on matches.';
begin
  Result := SHelpString;
end;

procedure TGrepExpert.SetSearchList(New: TStrings);
begin
  FSearchList.Assign(New);
end;

procedure TGrepExpert.SetReplaceList(New: TStrings);
begin
  FReplaceList.Assign(New);
end;

procedure TGrepExpert.SetMaskList(New: TStrings);
begin
  FMaskList.Assign(New);
end;

procedure TGrepExpert.SetDirList(New: TStrings);
begin
  FDirList.Assign(New);
end;

procedure TGrepExpert.SetExcludedDirsList(const Value: TStrings);
begin
  FExcludedDirsList.Assign(Value);
end;

procedure TGrepExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    begin
      if fmGrepResults = nil then
        fmGrepResults := TfmGrepResults.Create(nil);
      fmGrepResults.GrepExpert := Self;
    end
    else
      FreeAndNil(fmGrepResults);
  end;
end;

class function TGrepExpert.ConfigurationKey: string;
begin
  Result := 'Grep';
end;

procedure ShowGrep;
begin
  {$IFOPT D+} SendDebug('Showing grep expert'); {$ENDIF}
  InitSharedResources;
  try
    GrepStandAlone := TGrepExpert.Create;
    try
      {$IFOPT D+} SendDebug('Created grep window'); {$ENDIF}
      GrepStandAlone.LoadSettings;
      GrepStandAlone.ShowModal;
      GrepStandAlone.HistoryListSaveSettings;
      GrepStandAlone.SaveSettings;
    finally
      FreeAndNil(GrepStandAlone);
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure doShowGrepEx(const _Directory: string);
begin
{$IFOPT D+}SendDebug('Showing grep expert for directory ' + _Directory);{$ENDIF}
  InitSharedResources;
  try
    GrepStandAlone := TGrepExpert.Create;
    try
      {$IFOPT D+} SendDebug('Created grep window'); {$ENDIF}
      GrepStandAlone.LoadSettings;
      GrepStandAlone.ShowStandAlone(_Directory);
      GrepStandAlone.HistoryListSaveSettings;
      GrepStandAlone.SaveSettings;
    finally
      FreeAndNil(GrepStandAlone);
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure ShowGrepExW(const _Directory: PWideChar);
var
  Dir: string;
begin
  Dir := _Directory;
  doShowGrepEx(Dir);
end;

procedure ShowGrepExA(const _Directory: PAnsiChar);
var
  Dir: string;
begin
  Dir := String(_Directory);
  doShowGrepEx(Dir);
end;


function TGrepExpert.GetSaveOption: TGrepSaveOption;
begin
  if FGrepSaveOptionDefaultValue <= Integer(High(TGrepSaveOption)) then
    Result := TGrepSaveOption(FGrepSaveOptionDefaultValue)
  else
    Result := FGrepSaveOption;
end;

function TGrepExpert.GetOpenSaveOption: TGrepSaveOption;
begin
  if FGrepOpenSaveOptionDefaultValue <= Integer(High(TGrepSaveOption)) then
    Result := TGrepSaveOption(FGrepOpenSaveOptionDefaultValue)
  else
    Result := SaveOption;
end;

initialization
  RegisterGX_Expert(TGrepExpert);
end.
