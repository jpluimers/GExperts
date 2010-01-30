unit GX_GrepExpert;

interface

uses
  Classes, Graphics,
  GX_Experts, GX_ConfigurationInfo;

type
  TGrepExpert = class(TGX_Expert)
  private
    FGrepMiddle: Boolean;
    FGrepExpandAll: Boolean;
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
    FGrepSearch: Integer;
    FGrepSub: Boolean;
    FGrepWholeWord: Boolean;
    FGrepRegEx: Boolean;
    FGrepUseCurrentIdent: Boolean;
    FNumContextLines: Integer;
    FListFont: TFont;
    FContextFont: TFont;
    FContextMatchColor: TColor;
    procedure SetSearchList(New: TStrings);
    procedure SetReplaceList(New: TStrings);
    procedure SetMaskList(New: TStrings);
    procedure SetDirList(New: TStrings);
    procedure SetExcludedDirsList(const Value: TStrings);
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
    property GrepMiddle: Boolean read FGrepMiddle write FGrepMiddle;
    property GrepExpandAll: Boolean read FGrepExpandAll write FGrepExpandAll;
    property GrepCaseSensitive: Boolean read FGrepCaseSensitive write FGrepCaseSensitive;
    property GrepCode: Boolean read FGrepCode write FGrepCode;
    property GrepStrings: Boolean read FGrepStrings write FGrepStrings;
    property GrepComments: Boolean read FGrepComments write FGrepComments;
    property GrepInterface: Boolean read FGrepInterface write FGrepInterface;
    property GrepImplementation: Boolean read FGrepImplementation write FGrepImplementation;
    property GrepInitialization: Boolean read FGrepInitialization write FGrepInitialization;
    property GrepFinalization: Boolean read FGrepFinalization write FGrepFinalization;
    property GrepForms: Boolean read FGrepForms write FGrepForms;
    property GrepSearch: Integer read FGrepSearch write FGrepSearch;
    property GrepSub: Boolean read FGrepSub write FGrepSub;
    property GrepWholeWord: Boolean read FGrepWholeWord write FGrepWholeWord;
    property GrepRegEx: Boolean read FGrepRegEx write FGrepRegEx;
    property GrepUseCurrentIdent: Boolean read FGrepUseCurrentIdent write FGrepUseCurrentIdent;
    property NumContextLines: Integer read FNumContextLines write FNumContextLines;
    property ListFont: TFont read FListFont write FListFont;
    property ContextFont: TFont read FContextFont write FContextFont;
    property ContextMatchColor: TColor read FContextMatchColor write FContextMatchColor;

    property SearchList: TStrings read FSearchList write SetSearchList;
    property ReplaceList: TStrings read FReplaceList write SetReplaceList;
    property MaskList: TStrings read FMaskList write SetMaskList;
    property DirList: TStrings read FDirList write SetDirList;
    property ExcludedDirsList: TStrings read FExcludedDirsList write SetExcludedDirsList;
  end;

var
  GrepStandAlone: TGrepExpert = nil;

procedure ShowGrep; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

uses
  SysUtils, Menus, Controls,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  GX_OtaUtils, GX_GenericUtils,
  GX_GrepResults, GX_GrepResultsOptions,
  GX_IdeDock, GX_GExperts, ComCtrls;

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
  FContextFont := TFont.Create;
  FContextMatchColor := clHighlight;
  FNumContextLines := 2;
  
  FGrepExpandAll := False;
  FGrepUseCurrentIdent := False;
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

  SaveSettings;

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
    Dialog.pnlListFont.Font.Assign(ListFont);
    Dialog.pnlContextFont.Font.Assign(ContextFont);
    Dialog.pnlMatchLineColor.Font.Assign(ContextFont);
    Dialog.pnlMatchLineColor.Font.Color := ContextMatchColor;
    Dialog.udContextLines.Position := NumContextLines;
        
    if Dialog.ShowModal = mrOk then
    begin
      GrepMiddle := Dialog.chkGrepMiddle.Checked;
      GrepExpandAll := Dialog.chkGrepExpandAll.Checked;
      FListFont.Assign(Dialog.pnlListFont.Font);
      FContextFont.Assign(Dialog.pnlContextFont.Font);
      ContextMatchColor := Dialog.pnlMatchLineColor.Font.Color;
      NumContextLines := Dialog.udContextLines.Position;
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
  Settings.WriteInteger(ConfigurationKey, 'Search', GrepSearch);
  Settings.WriteBool(ConfigurationKey, 'SubDirectories', GrepSub);
  Settings.WriteBool(ConfigurationKey, 'ExpandAll', GrepExpandAll);
  Settings.WriteBool(ConfigurationKey, 'Whole Word', GrepWholeWord);
  Settings.WriteBool(ConfigurationKey, 'Middle', GrepMiddle);
  Settings.WriteBool(ConfigurationKey, 'RegEx', GrepRegEx);
  Settings.WriteBool(ConfigurationKey, 'UseCurrentIdent', GrepUseCurrentIdent);
  Settings.SaveFont(AddSlash(ConfigurationKey) + 'ListFont', ListFont);
  Settings.SaveFont(AddSlash(ConfigurationKey) + 'ContextFont', ContextFont);
  Settings.WriteInteger(ConfigurationKey, 'NumContextLines', NumContextLines);
  Settings.WriteInteger(ConfigurationKey, 'ContextMatchColor', ContextMatchColor);

  RegWriteStrings(Settings, DirList, ConfigurationKey + PathDelim + 'DirectoryList', 'GrepDir');
  RegWriteStrings(Settings, SearchList, ConfigurationKey + PathDelim + 'SearchList', 'GrepSearch');
  RegWriteStrings(Settings, ReplaceList, ConfigurationKey + PathDelim + 'ReplaceList', 'GrepReplace');
  RegWriteStrings(Settings, MaskList, ConfigurationKey + PathDelim + 'MaskList', 'GrepMask');
  RegWriteStrings(Settings, ExcludedDirsList, ConfigurationKey + PathDelim + 'ExcludedDirsList', 'GrepExcludedDirs');
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
  FGrepSearch := Settings.ReadInteger(ConfigurationKey, 'Search', 0);
  FGrepSub := Settings.ReadBool(ConfigurationKey, 'SubDirectories', True);
  FGrepExpandAll := Settings.ReadBool(ConfigurationKey, 'ExpandAll', False);
  FGrepWholeWord := Settings.ReadBool(ConfigurationKey, 'Whole Word', False);
  FGrepMiddle := Settings.ReadBool(ConfigurationKey, 'Middle', True);
  FGrepRegEx := Settings.ReadBool(ConfigurationKey, 'RegEx', False);
  FGrepUseCurrentIdent := Settings.ReadBool(ConfigurationKey, 'UseCurrentIdent', False);

  Settings.LoadFont(AddSlash(ConfigurationKey) + 'ListFont', ListFont);
  Settings.LoadFont(AddSlash(ConfigurationKey) + 'ContextFont', ContextFont);
  FNumContextLines :=  Settings.ReadInteger(ConfigurationKey, 'NumContextLines', FNumContextLines);
  FContextMatchColor :=  Settings.ReadInteger(ConfigurationKey, 'ContextMatchColor', FContextMatchColor);

  RegReadStrings(Settings, DirList, ConfigurationKey + PathDelim + 'DirectoryList', 'GrepDir');
  RegReadStrings(Settings, SearchList, ConfigurationKey + PathDelim + 'SearchList', 'GrepSearch');
  RegReadStrings(Settings, ReplaceList, ConfigurationKey + PathDelim + 'ReplaceList', 'GrepReplace');
  RegReadStrings(Settings, MaskList, ConfigurationKey + PathDelim + 'MaskList', 'GrepMask');
  RegReadStrings(Settings, ExcludedDirsList, ConfigurationKey + PathDelim + 'ExcludedDirsList', 'GrepExcludedDirs');

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
