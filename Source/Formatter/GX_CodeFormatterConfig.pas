// the code formatter configuration dialog
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterConfig;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ComCtrls,
  Dialogs,
  Grids,
  Menus,
  SynEdit,
  GX_CodeFormatterTypes,
  GX_CodeFormatterEngine,
  GX_CodeFormatterSettings,
  GX_EnhancedEditor,
  GX_GenericUtils;

type
  TStringGrid = class(Grids.TStringGrid)
  private
    FSpacingOptions: TStringList;
  protected
    function GetEditStyle(_Col: Integer; _Row: Integer): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    procedure OnGetSpacingOptions(_Col, _Row: Integer; _Items: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

type
  TfmCodeFormatterConfig = class(TForm)
    pc_Main: TPageControl;
    ts_Indent: TTabSheet;
    ts_Spacing: TTabSheet;
    ts_LineBreaks: TTabSheet;
    ed_SpacePerIndent: TEdit;
    ud_SpacePerIndent: TUpDown;
    l_SpacesPerIndent: TLabel;
    ts_Capitalization: TTabSheet;
    chk_IndentBegin: TCheckBox;
    chk_UpperCompDirectives: TCheckBox;
    chk_BlankProc: TCheckBox;
    chk_BlankSubProc: TCheckBox;
    chk_IndentTry: TCheckBox;
    chk_FeedAfterSemiColon: TCheckBox;
    cmb_FeedRoundBegin: TComboBox;
    l_BeginStyle: TLabel;
    chk_FeedAfterThen: TCheckBox;
    chk_FeedBeforeEnd: TCheckBox;
    chk_UpperNumbers: TCheckBox;
    chk_IndentTryElse: TCheckBox;
    l_Capitalize: TLabel;
    ts_Align: TTabSheet;
    OpenDialog: TOpenDialog;
    chk_WrapLines: TCheckBox;
    l_WrapAtPosition: TLabel;
    ed_WrapPosition: TEdit;
    ud_WrapPosition: TUpDown;
    chk_FeedAfterVar: TCheckBox;
    l_ReservedWords: TLabel;
    cmb_ReservedCase: TComboBox;
    l_StandardDirectives: TLabel;
    cmb_StandDirectives: TComboBox;
    l_Identifiers: TLabel;
    cmb_IdentifiersCase: TComboBox;
    chk_IndentComments: TCheckBox;
    chk_IndentCompDirectives: TCheckBox;
    ts_Misc: TTabSheet;
    chk_AlignComments: TCheckBox;
    l_AlignComentsAtPosition: TLabel;
    ed_AlignCommentPos: TEdit;
    ud_AlignCommentPos: TUpDown;
    chk_AlignVar: TCheckBox;
    l_AlignVarAtPosition: TLabel;
    ed_AlignVarPos: TEdit;
    ud_AlignVarPos: TUpDown;
    chk_FeedElseIf: TCheckBox;
    chk_NoFeedBeforeThen: TCheckBox;
    chk_NoIndentElseIf: TCheckBox;
    chk_IndentCaseElse: TCheckBox;
    chk_RemoveDoubleBlank: TCheckBox;
    b_EditCapitalization: TButton;
    ts_Preview: TTabSheet;
    l_Before: TLabel;
    l_After: TLabel;
    grid_Spacing: TStringGrid;
    chk_FeedEachUnit: TCheckBox;
    chk_ExceptSingle: TCheckBox;
    p_Botton: TPanel;
    b_Help: TButton;
    b_Ok: TButton;
    b_Cancel: TButton;
    pm_Extra: TPopupMenu;
    mi_ResetTo: TMenuItem;
    mi_ResetToDefault: TMenuItem;
    mi_Import: TMenuItem;
    mi_Export: TMenuItem;
    b_Tools: TButton;
    od_Import: TOpenDialog;
    sd_Export: TSaveDialog;
    grp_ExtraIndentBefore: TGroupBox;
    grp_AlwaysBreakLine: TGroupBox;
    grp_ForceBlankLineBetween: TGroupBox;
    l_TryStyle: TLabel;
    cmb_FeedRoundTry: TComboBox;
    rb_CapitalizationInRegistry: TRadioButton;
    rb_CapitalizationInFile: TRadioButton;
    ed_CapitalizationFile: TEdit;
    b_CapitalizationSelect: TButton;
    rg_Capitalization: TRadioGroup;
    od_CapitalizationFile: TOpenDialog;
    grp_ConfigPrecedence: TGroupBox;
    grp_DirectivesPreventFormatting: TGroupBox;
    l_MiscStart: TLabel;
    l_MiscEnd: TLabel;
    ed_StartComment: TEdit;
    ed_EndCommentOut: TEdit;
    lb_Precedence: TListBox;
    b_PrecedenceUp: TButton;
    b_PrecedenceDown: TButton;
    chk_NoIndentUsesComma: TCheckBox;
    chk_FeedBeforeElse: TCheckBox;
    chk_NoIndentVarDecl: TCheckBox;
    p_Main: TPanel;
    procedure b_HelpClick(Sender: TObject);
    procedure b_EditCapitalizationClick(Sender: TObject);
    procedure ts_PreviewShow(Sender: TObject);
    procedure HandleOnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormShow(Sender: TObject);
    procedure chk_FeedAfterThenClick(Sender: TObject);
    procedure ts_PreviewResize(Sender: TObject);
    procedure b_ToolsClick(Sender: TObject);
    procedure mi_ResetToDefaultClick(Sender: TObject);
    procedure mi_ImportClick(Sender: TObject);
    procedure mi_ExportClick(Sender: TObject);
    procedure rb_CapitalizationInRegistryClick(Sender: TObject);
    procedure rb_CapitalizationInFileClick(Sender: TObject);
    procedure b_CapitalizationSelectClick(Sender: TObject);
    procedure lb_PrecedenceClick(Sender: TObject);
    procedure b_PrecedenceUpClick(Sender: TObject);
    procedure b_PrecedenceDownClick(Sender: TObject);
  private
    FCapitalization: TGXUnicodeStringList;
    m_PreviewBefore: TGxEnhancedEditor;
    m_PreviewAfter: TGxEnhancedEditor;
    procedure EngineSettingsToForm(const _EngineSettings: TCodeFormatterEngineSettings);
    procedure SettingsToForm(const _Settings: TCodeFormatterSettings);
    procedure FormToEngineSettings(var _Settings: TCodeFormatterEngineSettings);
    procedure FormToSettings(_Settings: TCodeFormatterSettings);
    procedure FillPreview;
    procedure AddSpaceRow(_RowNo: Integer; const _StrCol1, _StrCol2: string;
      _Space: TSpaceSet);
    function GetSpaceItem(_Idx: Integer): TSpaceSet;
    procedure SetDefault(_Which: string);
    procedure HandleCaptitalizationFileDropped(_Sender: TObject; _Files: TStrings);
    procedure m_PreviewFileDropped(_Sender: TObject; _Files: TStrings);
    procedure UpdatePreview;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class function Execute(_Settings: TCodeFormatterSettings): TModalResult;
  end;

implementation

{$R *.DFM}

uses
  Messages,
  GX_dzVclUtils,
  GX_GxUtils,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterEditCapitalization,
  GX_CodeFormatterDefaultSettings;

resourcestring
  str_None = 'None';
  str_Before = 'Before only';
  str_after = 'After only';
  str_BeforeAfter = 'Before and after';
  str_DefaultSettings = '<default>';
  str_PrecedenceDirective = 'GXFormatter.config directive';
  str_PrecedenceIniFile = 'GXFormatter.ini file';
  str_PrecedenceMySettings = 'my settings as configured here';

constructor TfmCodeFormatterConfig.Create(_Owner: TComponent);
var
  st: TStringList;
  i: Integer;
  mi: TMenuItem;
begin
  inherited;

  TControl_SetMinConstraints(Self);

  FCapitalization := TGXUnicodeStringList.Create;
  st := TStringList.Create;
  try
    TCodeFormatterConfigHandler.GetDefaultsList(st);
    for i := 0 to st.Count - 1 do begin
      mi := TMenuItem.Create(Self);
      mi.Caption := st[i];
      mi.OnClick := mi_ResetToDefaultClick;
      mi_ResetTo.Add(mi);
    end;
  finally
    st.Free;
  end;

  TWinControl_ActivateDropFiles(ed_CapitalizationFile, HandleCaptitalizationFileDropped);
  TEdit_ActivateAutoComplete(ed_CapitalizationFile, [acsFileSystem], [actSuggest]);

  m_PreviewBefore := TGxEnhancedEditor.Create(Self);
  m_PreviewBefore.Name := 'm_PreviewBefore';
  m_PreviewBefore.Parent := ts_Preview;
  m_PreviewBefore.Left := 0;
  m_PreviewBefore.Top := 16;
  m_PreviewBefore.Width := 241;
  m_PreviewBefore.Height := 337;
  m_PreviewBefore.TabOrder := 0;
  m_PreviewBefore.OnChange := ts_PreviewShow;
  m_PreviewBefore.OnStatusChange := HandleOnStatusChange;
  m_PreviewBefore.Highlighter := gxpPas;

  m_PreviewAfter := TGxEnhancedEditor.Create(Self);
  m_PreviewAfter.Name := 'm_PreviewAfter';
  m_PreviewAfter.Parent := ts_Preview;
  m_PreviewAfter.Left := 248;
  m_PreviewAfter.Top := 16;
  m_PreviewAfter.Width := 214;
  m_PreviewAfter.Height := 337;
  m_PreviewAfter.Anchors := [akLeft, akTop, akRight];
  m_PreviewAfter.TabOrder := 1;
  m_PreviewAfter.Highlighter := gxpPas;

  TWinControl_ActivateDropFiles(m_PreviewBefore, m_PreviewFileDropped);
  TWinControl_ActivateDropFiles(m_PreviewAfter, m_PreviewFileDropped);

  grid_Spacing.DefaultRowHeight := grid_Spacing.Canvas.TextHeight('Mg') + 4;

  lb_Precedence.Items.AddObject(str_PrecedenceDirective, Pointer(cpDirective));
  lb_Precedence.Items.AddObject(str_PrecedenceIniFile, Pointer(cpIniFile));
  lb_Precedence.Items.AddObject(str_PrecedenceMySettings, Pointer(cpMyConfig));

  SetDefaultFont(Self);
end;

destructor TfmCodeFormatterConfig.Destroy;
begin
  FCapitalization.Free;
  inherited;
end;

procedure TfmCodeFormatterConfig.HandleCaptitalizationFileDropped(_Sender: TObject; _Files: TStrings);
begin
  ed_CapitalizationFile.Text := _Files[0];
end;

procedure TfmCodeFormatterConfig.m_PreviewFileDropped(_Sender: TObject; _Files: TStrings);
begin
  m_PreviewBefore.LoadFromFile(_Files[0]);
  UpdatePreview;
end;

function TfmCodeFormatterConfig.GetSpaceItem(_Idx: Integer): TSpaceSet;
var
  s: string;
begin
  s := grid_Spacing.Cells[2, _Idx];
  Result := spNone;
  if s = str_Before then
    Result := [spBefore]
  else if s = str_after then
    Result := [spAfter]
  else if s = str_BeforeAfter then
    Result := spBoth;
end;

procedure TfmCodeFormatterConfig.lb_PrecedenceClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Precedence.ItemIndex;
  b_PrecedenceUp.Enabled := (Idx <> 0);
  b_PrecedenceDown.Enabled := (Idx <> 2);
end;

procedure TfmCodeFormatterConfig.b_PrecedenceUpClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Precedence.ItemIndex;
  if Idx = 0 then
    Exit;
  lb_Precedence.Items.Exchange(Idx, Idx - 1);
  lb_PrecedenceClick(lb_Precedence);
end;

procedure TfmCodeFormatterConfig.b_PrecedenceDownClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Precedence.ItemIndex;
  if Idx = 2 then
    Exit;
  lb_Precedence.Items.Exchange(Idx, Idx + 1);
  lb_PrecedenceClick(lb_Precedence);
end;

procedure TfmCodeFormatterConfig.AddSpaceRow(_RowNo: Integer; const _StrCol1, _StrCol2: string;
  _Space: TSpaceSet);

  procedure SetColText(_Col: Integer; const _s: string; _Offset: Integer = 4);
  var
    w: Integer;
  begin
    grid_Spacing.Cells[_Col, _RowNo] := _s;
    w := grid_Spacing.Canvas.TextWidth(_s) + _Offset;
    if grid_Spacing.ColWidths[_Col] < w then
      grid_Spacing.ColWidths[_Col] := w;
  end;

begin
  SetColText(0, _StrCol1);
  SetColText(1, _StrCol2);

  if _Space = spNone then
    SetColText(2, str_None, 40)
  else if _Space = spBoth then
    SetColText(2, str_BeforeAfter, 40)
  else if spBefore in _Space then
    SetColText(2, str_Before, 40)
  else
    SetColText(2, str_after, 40);
end;

function GetModuleDir: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

procedure TfmCodeFormatterConfig.FillPreview;
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(GetModuleDir) + 'preview.pas';
  if FileExists(s) then begin
    m_PreviewBefore.Clear;
    m_PreviewBefore.LoadFromFile(s);
  end
end;

procedure TfmCodeFormatterConfig.FormToEngineSettings(var _Settings: TCodeFormatterEngineSettings);
begin
  _Settings := BorlandDefaults;

  _Settings.SpacePerIndent := ud_SpacePerIndent.Position;
  _Settings.IndentBegin := chk_IndentBegin.Checked;
  _Settings.IndentComments := chk_IndentComments.Checked;
  _Settings.IndentCompDirectives := chk_IndentCompDirectives.Checked;
  _Settings.IndentTry := chk_IndentTry.Checked;
  _Settings.IndentTryElse := chk_IndentTryElse.Checked;
  _Settings.IndentCaseElse := chk_IndentCaseElse.Checked;
  _Settings.UpperCompDirectives := chk_UpperCompDirectives.Checked;
  _Settings.UpperNumbers := chk_UpperNumbers.Checked;
  _Settings.ReservedCase := TCase(cmb_ReservedCase.ItemIndex);
  _Settings.StandDirectivesCase := TCase(cmb_StandDirectives.ItemIndex);
  _Settings.IdentifiersCase := TCase(cmb_IdentifiersCase.ItemIndex);
  _Settings.BlankProc := chk_BlankProc.Checked;
  _Settings.BlankSubProc := chk_BlankSubProc.Checked;
  _Settings.RemoveDoubleBlank := chk_RemoveDoubleBlank.Checked;
  _Settings.WrapLines := chk_WrapLines.Checked;
  _Settings.WrapPosition := ud_WrapPosition.Position;
  _Settings.AlignComments := chk_AlignComments.Checked;
  _Settings.AlignCommentPos := ud_AlignCommentPos.Position;
  _Settings.AlignVar := chk_AlignVar.Checked;
  _Settings.AlignVarPos := ud_AlignVarPos.Position;
  _Settings.SpaceEqualOper := GetSpaceItem(1);
  _Settings.SpaceOperators := GetSpaceItem(2);
  _Settings.SpaceColon := GetSpaceItem(3);
  _Settings.SpaceSemiColon := GetSpaceItem(4);
  _Settings.SpaceComma := GetSpaceItem(5);
  _Settings.SpaceLeftBr := GetSpaceItem(6);
  _Settings.SpaceRightBr := GetSpaceItem(7);
  _Settings.SpaceLeftHook := GetSpaceItem(8);
  _Settings.SpaceRightHook := GetSpaceItem(9);
  _Settings.FeedAfterThen := chk_FeedAfterThen.Checked;
  _Settings.ExceptSingle := chk_ExceptSingle.Checked;
  _Settings.FeedEachUnit := chk_FeedEachUnit.Checked;
  _Settings.NoFeedBeforeThen := chk_NoFeedBeforeThen.Checked;
  _Settings.FeedAfterVar := chk_FeedAfterVar.Checked;
  _Settings.FeedElseIf := chk_FeedElseIf.Checked;
  _Settings.NoIndentElseIf := chk_NoIndentElseIf.Checked;
  _Settings.NoIndentVarDecl := chk_NoIndentVarDecl.Checked;
  _Settings.NoIndentUsesComma := chk_NoIndentUsesComma.Checked;
  _Settings.FeedBeforeElse := chk_FeedBeforeElse.Checked;
  _Settings.FeedBeforeEnd := chk_FeedBeforeEnd.Checked;
  _Settings.FeedAfterSemiColon := chk_FeedAfterSemiColon.Checked;
  _Settings.FillNewWords := IntToCapfileMode(rg_Capitalization.ItemIndex);
  _Settings.StartCommentOut := Trim(ed_StartComment.Text);
  _Settings.EndCommentOut := Trim(ed_EndCommentOut.Text);
  _Settings.FeedRoundBegin := TFeedBegin(cmb_FeedRoundBegin.ItemIndex);
  _Settings.FeedRoundTry := TFeedBegin(cmb_FeedRoundTry.ItemIndex);
end;

procedure TfmCodeFormatterConfig.FormToSettings(_Settings: TCodeFormatterSettings);
var
  Settings: TCodeFormatterEngineSettings;
  i: Integer;
  Idx: Integer;
begin
  _Settings.UseCapitalizationFile := rb_CapitalizationInFile.Checked;
  _Settings.CapitalizationFile := ed_CapitalizationFile.Text;
  _Settings.CapNames.Assign(FCapitalization);

  for i := Low(TOneToThree) to High(TOneToThree) do begin
    Idx := i - Low(TOneToThree);
    _Settings.ConfigPrecedence[i] := TConfigPrecedenceEnum(lb_Precedence.Items.Objects[Idx])
  end;

  FormToEngineSettings(Settings);
  _Settings.Settings := Settings;
end;

procedure TfmCodeFormatterConfig.EngineSettingsToForm(const _EngineSettings: TCodeFormatterEngineSettings);
resourcestring
  str_Description = 'Description';
  str_Operators = 'Operators';
  str_Spacing = 'Spacing';
  str_Equals = 'Equals';
  str_MathOperators = 'Math. operators';
  str_MathOperatorsExample = '< > = + - / * etc.';
  str_Colon = 'Colon';
  str_SemiColon = 'Semicolon';
  str_Comma = 'Comma';
  str_LeftParenthesis = 'Left parenthesis';
  str_RightParenthesis = 'Right parenthesis';
  str_LeftBracket = 'Left bracket';
  str_RightBracket = 'Right bracket';
begin
  ud_SpacePerIndent.Position := _EngineSettings.SpacePerIndent;
  chk_IndentBegin.Checked := _EngineSettings.IndentBegin;
  chk_IndentComments.Checked := _EngineSettings.IndentComments;
  chk_IndentCompDirectives.Checked := _EngineSettings.IndentCompDirectives;
  chk_IndentTry.Checked := _EngineSettings.IndentTry;
  chk_IndentTryElse.Checked := _EngineSettings.IndentTryElse;
  chk_IndentCaseElse.Checked := _EngineSettings.IndentCaseElse;
  chk_UpperCompDirectives.Checked := _EngineSettings.UpperCompDirectives;
  chk_UpperNumbers.Checked := _EngineSettings.UpperNumbers;
  cmb_ReservedCase.ItemIndex := Ord(_EngineSettings.ReservedCase);
  cmb_StandDirectives.ItemIndex := Ord(_EngineSettings.StandDirectivesCase);
  cmb_IdentifiersCase.ItemIndex := Ord(_EngineSettings.IdentifiersCase);
  chk_BlankProc.Checked := _EngineSettings.BlankProc;
  chk_BlankSubProc.Checked := _EngineSettings.BlankSubProc;
  chk_RemoveDoubleBlank.Checked := _EngineSettings.RemoveDoubleBlank;
  with grid_Spacing do begin
    RowCount := 10;
    Cells[0, 0] := str_Description;
    Cells[1, 0] := str_Operators;
    Cells[2, 0] := str_Spacing;
    AddSpaceRow(1, str_Equals, ':=', _EngineSettings.SpaceEqualOper);
    AddSpaceRow(2, str_MathOperators, str_MathOperatorsExample,
      _EngineSettings.SpaceOperators);
    AddSpaceRow(3, str_Colon, ':', _EngineSettings.SpaceColon);
    AddSpaceRow(4, str_SemiColon, ';', _EngineSettings.SpaceSemiColon);
    AddSpaceRow(5, str_Comma, ',', _EngineSettings.SpaceComma);
    AddSpaceRow(6, str_LeftParenthesis, '(', _EngineSettings.SpaceLeftBr);
    AddSpaceRow(7, str_RightParenthesis, ')', _EngineSettings.SpaceRightBr);
    AddSpaceRow(8, str_LeftBracket, '[', _EngineSettings.SpaceLeftHook);
    AddSpaceRow(9, str_RightBracket, ']', _EngineSettings.SpaceRightHook);
  end;
  chk_FeedAfterSemiColon.Checked := _EngineSettings.FeedAfterSemiColon;
  chk_FeedEachUnit.Checked := _EngineSettings.FeedEachUnit;
  chk_FeedAfterThen.Checked := _EngineSettings.FeedAfterThen;
  chk_ExceptSingle.Checked := _EngineSettings.ExceptSingle;
  chk_NoFeedBeforeThen.Checked := _EngineSettings.NoFeedBeforeThen;
  chk_FeedAfterVar.Checked := _EngineSettings.FeedAfterVar;
  chk_FeedElseIf.Checked := _EngineSettings.FeedElseIf;
  chk_NoIndentElseIf.Checked := _EngineSettings.NoIndentElseIf;
  chk_NoIndentVarDecl.Checked := _EngineSettings.NoIndentVarDecl;
  chk_NoIndentUsesComma.Checked := _EngineSettings.NoIndentUsesComma;
  chk_FeedBeforeElse.Checked := _EngineSettings.FeedBeforeElse;
  chk_FeedBeforeEnd.Checked := _EngineSettings.FeedBeforeEnd;
  chk_WrapLines.Checked := _EngineSettings.WrapLines;
  ud_WrapPosition.Position := _EngineSettings.WrapPosition;
  chk_AlignComments.Checked := _EngineSettings.AlignComments;
  ud_AlignCommentPos.Position := _EngineSettings.AlignCommentPos;
  chk_AlignVar.Checked := _EngineSettings.AlignVar;
  ud_AlignVarPos.Position := _EngineSettings.AlignVarPos;
  rg_Capitalization.ItemIndex := CapfileModeToInt(_EngineSettings.FillNewWords);
  ed_StartComment.Text := string(_EngineSettings.StartCommentOut);
  ed_EndCommentOut.Text := string(_EngineSettings.EndCommentOut);
  cmb_FeedRoundBegin.ItemIndex := Integer(_EngineSettings.FeedRoundBegin);
  cmb_FeedRoundTry.ItemIndex := Integer(_EngineSettings.FeedRoundTry);
  ud_SpacePerIndent.Associate := ed_SpacePerIndent;
  ud_WrapPosition.Associate := ed_WrapPosition;
  ud_AlignCommentPos.Associate := ed_AlignCommentPos;
  ud_AlignVarPos.Associate := ed_AlignVarPos;
  chk_FeedAfterThenClick(nil)
end;

procedure TfmCodeFormatterConfig.SettingsToForm(const _Settings: TCodeFormatterSettings);

  procedure AddPrecedenceSetting(_cp: TConfigPrecedenceEnum);
  begin
    case _cp of
      cpDirective: lb_Precedence.Items.AddObject(str_PrecedenceDirective, Pointer(cpDirective));
      cpIniFile: lb_Precedence.Items.AddObject(str_PrecedenceIniFile, Pointer(cpIniFile));
      cpMyConfig: lb_Precedence.Items.AddObject(str_PrecedenceMySettings, Pointer(cpMyConfig));
    end;
  end;

var
  i: Integer;
  cp: TConfigPrecedenceEnum;
  PrecedenceSet: set of TConfigPrecedenceEnum;
begin
  rb_CapitalizationInFile.Checked := _Settings.UseCapitalizationFile;
  ed_CapitalizationFile.Text := string(_Settings.CapitalizationFile);
  FCapitalization.Assign(_Settings.CapNames);

  lb_Precedence.Items.Clear;
  // the set is used to prevent manipulated settings from crashing the program
  PrecedenceSet := [cpDirective, cpIniFile, cpMyConfig];
  for i := Low(TOneToThree) to High(TOneToThree) do begin
    if _Settings.ConfigPrecedence[i] in PrecedenceSet then begin
      AddPrecedenceSetting(_Settings.ConfigPrecedence[i]);
      Exclude(PrecedenceSet, _Settings.ConfigPrecedence[i]);
    end;
  end;
  for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
    if cp in PrecedenceSet then
      AddPrecedenceSetting(cp);
  end;

  EngineSettingsToForm(_Settings.Settings);
end;

procedure TfmCodeFormatterConfig.SetDefault(_Which: string);
resourcestring
  str_CouldNotReadS = 'Could not read default configuration %s.';
var
  Defaults: TCodeFormatterSettings;
begin
  _Which := StringReplace(_Which, '&', '', [rfReplaceAll]);
  grid_Spacing.EditorMode := False;
  Defaults := TCodeFormatterSettings.Create;
  try
    if _Which <> str_DefaultSettings then begin
      if not TCodeFormatterConfigHandler.GetDefaultConfig(_Which, Defaults) then begin
        MessageDlg(Format(str_CouldNotReadS, [_Which]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
    SettingsToForm(Defaults);
  finally
    Defaults.Free;
  end;
  if pc_Main.ActivePage = ts_Preview then
    ts_PreviewShow(nil);
end;

procedure TfmCodeFormatterConfig.b_CapitalizationSelectClick(Sender: TObject);
var
  s: string;
begin
  od_CapitalizationFile.FileName := ed_CapitalizationFile.Text;
  if not od_CapitalizationFile.Execute then
    Exit;

  s := od_CapitalizationFile.FileName;
  if FileExists(s) then begin
    if FCapitalization.Count > 0 then begin
      if mrYes <> MessageDlg(
        'Your current capitalization list is not empty and the file already exists.'#13#10
        + 'Do you want to discard your list and load the selected file instead?',
        mtWarning, [mbYes, mbCancel], 0) then
        Exit;
    end;
    FCapitalization.LoadFromFile(s);
  end else begin
    if FCapitalization.Count > 0 then begin
      FCapitalization.SaveToFile(s);
    end;
  end;

  ed_CapitalizationFile.Text := s;
end;

procedure TfmCodeFormatterConfig.b_HelpClick(Sender: TObject);
var
  HlpFile: string;
begin
  { TODO : replace with GExperts help (and add contents of DelFor help to GExperts help) }
  HlpFile := GetModuleDir + 'delfor.hlp';
  WinHelp(0, PChar(HlpFile), HELP_KEY, Integer(pc_Main.ActivePage.Caption));
end;

procedure TfmCodeFormatterConfig.b_EditCapitalizationClick(Sender: TObject);
var
  FileEditDlg: TfmCodeFormatterEditCapitalization;
  Cur: TCursor;
begin
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FileEditDlg := TfmCodeFormatterEditCapitalization.Create(Self);
  try
    FileEditDlg.ListToForm(FCapitalization);
    Screen.Cursor := Cur;
    if mrOk = FileEditDlg.ShowModal then begin
      if FileEditDlg.IsChanged then
        FileEditDlg.FormToList(FCapitalization);
    end;
  finally
    Screen.Cursor := Cur;
    FileEditDlg.Free;
  end;
end;

procedure TfmCodeFormatterConfig.ts_PreviewShow(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfmCodeFormatterConfig.HandleOnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if m_PreviewAfter.TopLine <> m_PreviewBefore.TopLine then
    m_PreviewAfter.TopLine := m_PreviewBefore.TopLine;
end;

procedure TfmCodeFormatterConfig.rb_CapitalizationInFileClick(Sender: TObject);
begin
  ed_CapitalizationFile.Enabled := True;
  b_CapitalizationSelect.Enabled := True;
end;

procedure TfmCodeFormatterConfig.rb_CapitalizationInRegistryClick(Sender: TObject);
begin
  ed_CapitalizationFile.Enabled := False;
  b_CapitalizationSelect.Enabled := False;
end;

procedure TfmCodeFormatterConfig.FormShow(Sender: TObject);
begin
  FillPreview;
  pc_Main.ActivePage := ts_Indent;
end;

procedure TfmCodeFormatterConfig.chk_FeedAfterThenClick(Sender: TObject);
begin
  chk_ExceptSingle.Enabled := chk_FeedAfterThen.Checked;
end;

procedure TfmCodeFormatterConfig.ts_PreviewResize(Sender: TObject);
var
  w: Integer;
begin
  w := (ts_Preview.ClientWidth - 16) div 2;
  m_PreviewBefore.Left := 8;
  l_Before.Left := 8;
  m_PreviewBefore.Width := w;
  m_PreviewBefore.Height := ts_Preview.ClientHeight - m_PreviewBefore.Top - 8;
  l_After.Left := w + 9;
  m_PreviewAfter.Left := w + 9;
  m_PreviewAfter.Width := w;
  m_PreviewAfter.Height := ts_Preview.ClientHeight - m_PreviewAfter.Top - 8;
end;

procedure TfmCodeFormatterConfig.b_ToolsClick(Sender: TObject);
var
  Point: TPoint;
begin
  Point.X := b_Tools.Width;
  Point.Y := 0;
  Point := b_Tools.ClientToScreen(Point);
  pm_Extra.Popup(Point.X, Point.Y);
end;

procedure TfmCodeFormatterConfig.mi_ResetToDefaultClick(Sender: TObject);
begin
  SetDefault(TMenuItem(Sender).Caption);
end;

procedure TfmCodeFormatterConfig.mi_ImportClick(Sender: TObject);
var
  Settings: TCodeFormatterSettings;
begin
  grid_Spacing.EditorMode := False;
  od_Import.FileName := 'DelForExOptions.ini';
  if not od_Import.Execute then
    Exit;

  Settings := TCodeFormatterSettings.Create;
  try
    TCodeFormatterConfigHandler.ImportFromFile(od_Import.FileName, Settings);
    SettingsToForm(Settings);
  finally
    Settings.Free;
  end;

  if pc_Main.ActivePage = ts_Preview then
    ts_PreviewShow(nil);
end;

procedure TfmCodeFormatterConfig.mi_ExportClick(Sender: TObject);
var
  Settings: TCodeFormatterSettings;
begin
  grid_Spacing.EditorMode := False;
  sd_Export.FileName := 'DelForExOptions.ini';
  if not sd_Export.Execute then
    Exit;

  Settings := TCodeFormatterSettings.Create;
  try
    FormToSettings(Settings);
    TCodeFormatterConfigHandler.ExportToFile(sd_Export.FileName, Settings);
  finally
    Settings.Free;
  end;
end;

class function TfmCodeFormatterConfig.Execute(_Settings: TCodeFormatterSettings): TModalResult;
var
  frm: TfmCodeFormatterConfig;
begin
  frm := TfmCodeFormatterConfig.Create(nil);
  try
    frm.HelpFile := 'delfor.hlp';
    frm.SettingsToForm(_Settings);
    Result := frm.ShowModal;
    if Result = mrOk then
      frm.FormToSettings(_Settings);
  finally
    frm.Free;
  end;
end;

procedure TfmCodeFormatterConfig.UpdatePreview;
var
  sl: TGXUnicodeStringList;
  Formatter: TCodeFormatterEngine;
begin
  sl := nil;
  Formatter := TCodeFormatterEngine.Create;
  try
    // this temporary string list is necessary to prevent an infinite loop (whose reason I don't really understand :-( )
    sl := TGXUnicodeStringList.Create;
    m_PreviewBefore.GetLines(sl);
    FormToSettings(Formatter.Settings);
    Formatter.Execute(sl);
    m_PreviewAfter.SetLines(sl);
  finally
    Formatter.Free;
    sl.Free;
  end;
end;

{ TStringGrid }

constructor TStringGrid.Create(_Owner: TComponent);
begin
  inherited;
  FSpacingOptions := TStringList.Create;
  FSpacingOptions.Add(str_None);
  FSpacingOptions.Add(str_Before);
  FSpacingOptions.Add(str_after);
  FSpacingOptions.Add(str_BeforeAfter);
end;

function TStringGrid.GetEditStyle(_Col, _Row: Integer): TEditStyle;
begin
  if Col = 2 then
    Result := esPickList
  else
    Result := esSimple;
end;

function TStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(Self);
  (Result as TInplaceEditList).OnGetPickListitems := OnGetSpacingOptions;
  (Result as TInplaceEditList).DropDownRows := 15;
end;

procedure TStringGrid.OnGetSpacingOptions(_Col, _Row: Integer; _Items: TStrings);
begin
  _Items.Assign(FSpacingOptions);
end;

destructor TStringGrid.Destroy;
begin
  FSpacingOptions.Free;
  inherited;
end;

function TStringGrid.CanEditModify: Boolean;
begin
  Result := False;
end;

function TStringGrid.CanEditShow: Boolean;
begin
  Result := (Col = 2);
end;

end.
