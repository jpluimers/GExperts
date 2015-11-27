unit GX_GrepExpert;

interface

uses
  Classes, Graphics,
  GX_Experts, GX_ConfigurationInfo, GX_GrepBackend;

type
  TGrepExpert = class(TGX_Expert)
  private
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
    FGrepSaveResultListItems: Boolean;
    FFoundList: TGrepFoundList;
    FContextSaveSize: Boolean;
    FFoundListSaveSize: Boolean;
    procedure SetSearchList(New: TStrings);
    procedure SetReplaceList(New: TStrings);
    procedure SetMaskList(New: TStrings);
    procedure SetDirList(New: TStrings);
    procedure SetExcludedDirsList(const Value: TStrings);
    procedure LoadFoundList(AGrepSettings : TGrepSettings);
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ShowModal;
    function GetActionCaption: string; override;
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    procedure FoundListSaveSettings(AFoundIndex: Integer = -1); //if -1 then all
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
    property GrepUseCurrentIdent: Boolean read FGrepUseCurrentIdent write FGrepUseCurrentIdent;
    property NumContextLines: Integer read FNumContextLines write FNumContextLines;

    property ListFont: TFont read FListFont write FListFont;
    property ListUseDefaultColors: Boolean read FListUseDefaultColors write FListUseDefaultColors;
    property ListMatchTextColor: TColor read FListMatchTextColor write FListMatchTextColor;
    property ListMatchBrushColor: TColor read FListMatchBrushColor write FListMatchBrushColor;
    property ContextFont: TFont read FContextFont write FContextFont;
    property ContextMatchColor: TColor read FContextMatchColor write FContextMatchColor;
    property ContextMatchLineColor: TColor read FContextMatchLineColor write FContextMatchLineColor;
    property AutoHide: Boolean read FAutoHide write FAutoHide;

    property GrepSaveResultListItems: Boolean read FGrepSaveResultListItems write FGrepSaveResultListItems;
    property ContextSaveSize: Boolean read FContextSaveSize write FContextSaveSize;
    property FoundListSaveSize: Boolean read FFoundListSaveSize write FFoundListSaveSize;

    property SearchList: TStrings read FSearchList write SetSearchList;
    property ReplaceList: TStrings read FReplaceList write SetReplaceList;
    property MaskList: TStrings read FMaskList write SetMaskList;
    property DirList: TStrings read FDirList write SetDirList;
    property ExcludedDirsList: TStrings read FExcludedDirsList write SetExcludedDirsList;

    property FoundList: TGrepFoundList read FFoundList;
  end;

var
  GrepStandAlone: TGrepExpert = nil;

procedure ShowGrep; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

uses
  SysUtils, Menus, Controls, ComCtrls, IniFiles,
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
  

  FFoundList := TGrepFoundList.Create;

  FGrepExpandAll := False;
  FGrepExpandIf := False;
  FGrepExpandIfFiles := 25;
  FGrepExpandIfMatches := 150;
  FGrepExpandFew := False;
  FGrepExpandFewLines := 20;
  FGrepUseCurrentIdent := False;
  FGrepSaveResultListItems := False;
  FContextSaveSize := False;
  FFoundListSaveSize := False;

  ShortCut := Menus.ShortCut(Word('R'), [ssCtrl, ssAlt]);
  fmGrepResults := TfmGrepResults.Create(nil);
  SetFormIcon(fmGrepResults);
  if not IsStandAlone then
    IdeDockManager.RegisterDockableForm(TfmGrepResults, fmGrepResults, 'fmGrepResults');
  fmGrepResults.GrepExpert := Self;
end;

destructor TGrepExpert.Destroy;
begin
  IdeDockManager.UnRegisterDockableForm(fmGrepResults, 'fmGrepResults');

  FoundListSaveSettings;
  SaveSettings;

  FreeAndNil(FFoundList);

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

class function TGrepExpert.GetName: string;
begin
  Result := 'GrepResults';
end;

procedure TGrepExpert.Click(Sender: TObject);
begin
  SetFormIcon(fmGrepResults);
  IdeDockManager.ShowForm(fmGrepResults);
  EnsureFormVisible(fmGrepResults);
end;

procedure TGrepExpert.ShowModal;
begin
  fmGrepResults.ShowModal;
end;

procedure TGrepExpert.Configure;
var
  Dialog: TfmGrepResultsOptions;
begin
  Dialog := TfmGrepResultsOptions.Create(nil);
  try
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
    Dialog.chkSaveContextSize.Checked := ContextSaveSize;
    Dialog.chkSaveFoundListSize.Checked := FoundListSaveSize;
    Dialog.chkGrepSaveResultListItems.Checked := GrepSaveResultListItems;

    Dialog.chkGrepAutoHide.Checked := AutoHide;
        
    if Dialog.ShowModal = mrOk then
    begin
      GrepMiddle := Dialog.chkGrepMiddle.Checked;
      GrepExpandAll := Dialog.chkGrepExpandAll.Checked;
      GrepExpandIf := Dialog.chkGrepExpandIf.Checked;
      GrepExpandIfFiles := StrToIntDef(Dialog.eExpandIfFiles.Text, 25);
      GrepExpandIfMatches := StrToIntDef(Dialog.eExpandIfMatches.Text, 150);
      GrepExpandFew := Dialog.chkGrepExpandFew.Checked;
      GrepExpandFewLines := StrToIntDef(Dialog.eExpandFewLines.Text, 20);

      ListUseDefaultColors := Dialog.chkDefaultListColors.Checked;
      FListFont.Assign(Dialog.pnlListFont.Font);
      FContextFont.Assign(Dialog.pnlContextFont.Font);
      ListMatchTextColor := Dialog.pnlListMatchTextColor.Font.Color;
      ListMatchBrushColor := Dialog.pnlListMatchBackgroundColor.Color;
      ContextMatchLineColor := Dialog.pnlContextMacthLineFontColor.Font.Color;
      ContextMatchColor := Dialog.pnlContextMatchFontColor.Font.Color;

      NumContextLines := Dialog.udContextLines.Position;
      ContextSaveSize := Dialog.chkSaveContextSize.Checked;
      FoundListSaveSize := Dialog.chkSaveFoundListSize.Checked;
      GrepSaveResultListItems := Dialog.chkGrepSaveResultListItems.Checked;

      AutoHide := DIalog.chkGrepAutoHide.Checked;
      SaveSettings;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TGrepExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // do not localize any of the following lines
  Settings.WriteBool(ConfigurationKey, 'CaseSensitive', GrepCaseSensitive);
  Settings.WriteBool(ConfigurationKey, 'Code', GrepCode);
  Settings.WriteBool(ConfigurationKey, 'Strings', GrepStrings);
  Settings.WriteBool(ConfigurationKey, 'NoComments', not GrepComments);
  Settings.WriteBool(ConfigurationKey, 'Interface', GrepInterface);
  Settings.WriteBool(ConfigurationKey, 'Implementation', GrepImplementation);
  Settings.WriteBool(ConfigurationKey, 'Initialization', GrepInitialization);
  Settings.WriteBool(ConfigurationKey, 'Finalization', GrepFinalization);
  Settings.WriteBool(ConfigurationKey, 'Forms', GrepForms);
  Settings.WriteBool(ConfigurationKey, 'SQLFiles', GrepSQLFiles);
  Settings.WriteInteger(ConfigurationKey, 'Search', GrepSearch);
  Settings.WriteBool(ConfigurationKey, 'SubDirectories', GrepSub);
  Settings.WriteBool(ConfigurationKey, 'ExpandAll', GrepExpandAll);
  Settings.WriteBool(ConfigurationKey, 'ExpandIf', GrepExpandIf);
  Settings.WriteInteger(ConfigurationKey, 'ExpandIfFiles', GrepExpandIfFiles);
  Settings.WriteInteger(ConfigurationKey, 'ExpandIfMatches', GrepExpandIfMatches);
  Settings.WriteBool(ConfigurationKey, 'ExpandFew', GrepExpandFew);
  Settings.WriteInteger(ConfigurationKey, 'ExpandFewLines', GrepExpandFewLines);
  Settings.WriteBool(ConfigurationKey, 'Whole Word', GrepWholeWord);
  Settings.WriteBool(ConfigurationKey, 'Middle', GrepMiddle);
  Settings.WriteBool(ConfigurationKey, 'AutoHide', AutoHide);
  Settings.WriteBool(ConfigurationKey, 'RegEx', GrepRegEx);
  Settings.WriteBool(ConfigurationKey, 'UseCurrentIdent', GrepUseCurrentIdent);

  Settings.WriteBool(ConfigurationKey, 'ListUseDefaultColors', ListUseDefaultColors);
  Settings.SaveFont(AddSlash(ConfigurationKey) + 'ListFont', ListFont, [ffColor]);
  Settings.WriteInteger(ConfigurationKey, 'ListMatchTextColor', ListMatchTextColor);
  Settings.WriteInteger(ConfigurationKey, 'ListMatchBrushColor', ListMatchBrushColor);
  Settings.SaveFont(AddSlash(ConfigurationKey) + 'ContextFont', ContextFont, [ffColor]);
  Settings.WriteInteger(ConfigurationKey, 'ContextMatchColor', ContextMatchColor);
  Settings.WriteInteger(ConfigurationKey, 'ContextMatchLineColor', ContextMatchLineColor);

  Settings.WriteInteger(ConfigurationKey, 'NumContextLines', NumContextLines);
  Settings.WriteBool(ConfigurationKey, 'SaveResultListItems', GrepSaveResultListItems);
  Settings.WriteBool(ConfigurationKey, 'ContextSaveSize', ContextSaveSize);
  Settings.WriteBool(ConfigurationKey, 'FoundListSaveSize', FoundListSaveSize);

  RegWriteStrings(Settings, DirList, ConfigurationKey + PathDelim + 'DirectoryList', 'GrepDir');
  RegWriteStrings(Settings, SearchList, ConfigurationKey + PathDelim + 'SearchList', 'GrepSearch');
  RegWriteStrings(Settings, ReplaceList, ConfigurationKey + PathDelim + 'ReplaceList', 'GrepReplace');
  RegWriteStrings(Settings, MaskList, ConfigurationKey + PathDelim + 'MaskList', 'GrepMask');
  RegWriteStrings(Settings, ExcludedDirsList, ConfigurationKey + PathDelim + 'ExcludedDirsList', 'GrepExcludedDirs');
end;

procedure TGrepExpert.FoundListSaveSettings(AFoundIndex: Integer);
var
  Settings: TIniFile;
begin
  if not GrepSaveResultListItems then
    Exit;

  Settings := TIniFile.Create(AddSlash(ConfigInfo().ConfigPath) + TGrepFoundList.SettingsFileName);
  try
    FoundList.SaveToSettings(Settings, TGrepFoundList.KeyName, TGrepFoundList.SubKeyName, AFoundIndex);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGrepExpert.LoadFoundList(AGrepSettings : TGrepSettings);
var
  Settings: TIniFile;
begin
  if not GrepSaveResultListItems then
    Exit;

  Settings := TIniFile.Create(AddSlash(ConfigInfo().ConfigPath) + TGrepFoundList.SettingsFileName);
  try
    FoundList.LoadFromSettings(AGrepSettings, Settings, TGrepFoundList.KeyName, TGrepFoundList.SubKeyName, True);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGrepExpert.InternalLoadSettings(Settings: TGExpertsSettings);

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
  FGrepCaseSensitive := Settings.ReadBool(ConfigurationKey, 'CaseSensitive', False);
  FGrepCode := Settings.ReadBool(ConfigurationKey, 'Code', True);
  FGrepStrings := Settings.ReadBool(ConfigurationKey, 'Strings', True);
  FGrepComments := not Settings.ReadBool(ConfigurationKey, 'NoComments', False);
  FGrepInterface := Settings.ReadBool(ConfigurationKey, 'Interface', True);
  FGrepImplementation := Settings.ReadBool(ConfigurationKey, 'Implementation', True);
  FGrepInitialization := Settings.ReadBool(ConfigurationKey, 'Initialization', True);
  FGrepFinalization := Settings.ReadBool(ConfigurationKey, 'Finalization', True);
  FGrepForms := Settings.ReadBool(ConfigurationKey, 'Forms', False);
  FGrepSQLFiles := Settings.ReadBool(ConfigurationKey, 'SQLFiles', False);
  FGrepSearch := Settings.ReadInteger(ConfigurationKey, 'Search', 0);
  FGrepSub := Settings.ReadBool(ConfigurationKey, 'SubDirectories', True);
  FGrepExpandAll := Settings.ReadBool(ConfigurationKey, 'ExpandAll', False);
  FGrepExpandIf := Settings.ReadBool(ConfigurationKey, 'ExpandIf', False);
  FGrepExpandIfFiles := Settings.ReadInteger(ConfigurationKey, 'ExpandIfFiles', 25);
  FGrepExpandIfMatches := Settings.ReadInteger(ConfigurationKey, 'ExpandIfMatches', 150);
  FGrepExpandFew := Settings.ReadBool(ConfigurationKey, 'ExpandFew', False);
  FGrepExpandFewLines := Settings.ReadInteger(ConfigurationKey, 'ExpandFewLines', 20);
  FGrepWholeWord := Settings.ReadBool(ConfigurationKey, 'Whole Word', False);
  FGrepMiddle := Settings.ReadBool(ConfigurationKey, 'Middle', True);
  FAutoHide := Settings.ReadBool(ConfigurationKey, 'AutoHide', False);
  FGrepRegEx := Settings.ReadBool(ConfigurationKey, 'RegEx', False);
  FGrepUseCurrentIdent := Settings.ReadBool(ConfigurationKey, 'UseCurrentIdent', False);
  FGrepSaveResultListItems := Settings.ReadBool(ConfigurationKey, 'SaveResultListItems', False);

  FListUseDefaultColors := Settings.ReadBool(ConfigurationKey, 'ListUseDefaultColors', False);
  Settings.LoadFont(AddSlash(ConfigurationKey) + 'ListFont', ListFont, [ffColor]);
  FListMatchTextColor :=  Settings.ReadInteger(ConfigurationKey, 'ListMatchTextColor', FListMatchTextColor);
  FListMatchBrushColor :=  Settings.ReadInteger(ConfigurationKey, 'ListMatchBrushColor', FListMatchBrushColor);
  Settings.LoadFont(AddSlash(ConfigurationKey) + 'ContextFont', ContextFont, [ffColor]);
  FContextMatchColor :=  Settings.ReadInteger(ConfigurationKey, 'ContextMatchColor', FContextMatchColor);
  if Settings.ValueExists(ConfigurationKey, 'ContextMatchLineColor') then
    FContextMatchLineColor := Settings.ReadInteger(ConfigurationKey, 'ContextMatchLineColor', FContextMatchLineColor)
  else
    FContextMatchLineColor := FContextMatchColor;

  FNumContextLines :=  Settings.ReadInteger(ConfigurationKey, 'NumContextLines', FNumContextLines);
  FContextSaveSize := Settings.ReadBool(ConfigurationKey, 'ContextSaveSize', False);
  FFoundListSaveSize := Settings.ReadBool(ConfigurationKey, 'FoundListSaveSize', False);

  RegReadStrings(Settings, DirList, ConfigurationKey + PathDelim + 'DirectoryList', 'GrepDir');
  RegReadStrings(Settings, SearchList, ConfigurationKey + PathDelim + 'SearchList', 'GrepSearch');
  RegReadStrings(Settings, ReplaceList, ConfigurationKey + PathDelim + 'ReplaceList', 'GrepReplace');
  RegReadStrings(Settings, MaskList, ConfigurationKey + PathDelim + 'MaskList', 'GrepMask');
  RegReadStrings(Settings, ExcludedDirsList, ConfigurationKey + PathDelim + 'ExcludedDirsList', 'GrepExcludedDirs');

  if GrepSaveResultListItems then
    LoadFoundList(fmGrepResults.GrepSettings);

  if MaskList.Count = 0 then
  begin
    MaskList.Add('*.pas;*.dpr;*.inc');
    MaskList.Add('*.pas;*.dpr;*.inc;*.sql');
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

  fmGrepResults.UpdateFromSettings;
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
      GrepStandAlone.FoundListSaveSettings;
      GrepStandAlone.SaveSettings;
    finally
      FreeAndNil(GrepStandAlone);
    end;
  finally
    FreeSharedResources;
  end;
end;

initialization
  RegisterGX_Expert(TGrepExpert);
end.
