unit GX_GrepSearch;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls,
  GX_Experts, GX_GrepExpert, GX_GrepBackend, GX_BaseForm, GX_KbdShortCutBroker;

type
  TfmGrepSearch = class(TfmBaseForm)
    lblFind: TLabel;
    cbText: TComboBox;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbForms: TCheckBox;
    gbxWhere: TGroupBox;
    rbAllProjFiles: TRadioButton;
    rbOpenFiles: TRadioButton;
    rbDirectories: TRadioButton;
    gbxDirectories: TGroupBox;
    lblMasks: TLabel;
    cbMasks: TComboBox;
    cbInclude: TCheckBox;
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    cbWholeWord: TCheckBox;
    rbCurrentOnly: TRadioButton;
    btnHelp: TButton;
    cbRegEx: TCheckBox;
    cbDirectory: TComboBox;
    btnBrowse: TButton;
    lblDirectory: TLabel;
    rbAllProjGroupFiles: TRadioButton;
    rbResults: TRadioButton;
    cbExcludedDirs: TComboBox;
    lblExcludeDirs: TLabel;
    cbSQLFiles: TCheckBox;
    gbxContentTypes: TGroupBox;
    cbGrepCode: TCheckBox;
    cbGrepStrings: TCheckBox;
    cbGrepComments: TCheckBox;
    gbxUnitSections: TGroupBox;
    cbSectionInterface: TCheckBox;
    cbSectionImplementation: TCheckBox;
    cbSectionInitialization: TCheckBox;
    cbSectionFinalization: TCheckBox;
    btnOptions: TButton;
    rgSaveOption: TRadioGroup;
    btnSearch: TButton;
    timHintTimer: TTimer;
    procedure btnBrowseClick(Sender: TObject);
    procedure rbDirectoriesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbDirectoryDropDown(Sender: TObject);
    procedure cbExcludedDirsDropDown(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure cbGrepCodeClick(Sender: TObject);
    procedure cbGrepStringsClick(Sender: TObject);
    procedure cbGrepCommentsClick(Sender: TObject);
    procedure cbSectionInterfaceClick(Sender: TObject);
    procedure cbSectionImplementationClick(Sender: TObject);
    procedure cbSectionInitializationClick(Sender: TObject);
    procedure cbSectionFinalizationClick(Sender: TObject);
    procedure timHintTimerTimer(Sender: TObject);
  private
    FGrepExpert: TGrepExpert;
    FEmbedded: Boolean;
    FCheckedWhere: Boolean;
    FEmbeddedHolder: TWinControl;
    FSaveWidth: Integer;
    FSaveOptionsGroupWidth: Integer;
    FSaveWhereGroupWidth: Integer;
    FSaveWhereGroupLeft: Integer;
    FLoadingSettings: Boolean;
    FTheHintWindow: THintWindow;
    procedure EnableDirectoryControls(New: Boolean);
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure UpdateMRUs;
    procedure cbDirectoryOnDropFiles(_Sender: TObject; _Files: TStrings);
    procedure CheckEnabledWhereControls;
    procedure CheckContentTypeSelection(ClickedOption: TCheckBox);
    procedure CheckSectionSelection(ClickedOption: TCheckBox);
    procedure ShowControlHint(ctrl: TWinControl; const HintText: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure EmbeddedInit(AHolderControl: TWinControl; ASearchEvent: TNotifyEvent);
    procedure EmbeddedUpdatePos;
    procedure EmbeddedShow;
    procedure EmbeddedSetHeights;
    procedure RetrieveSettings(var Value: TGrepSettings);
    procedure AdjustSettings(Value: TGrepSettings);
    property GrepExpert: TGrepExpert read FGrepExpert;
  end;

  TGrepDlgExpert = class(TGX_Expert)
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, Graphics, Menus, RegExpr, Math,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_GrepResults, GX_GrepOptions,
  GX_GrepRegExSearch, GX_dzVclUtils;

resourcestring
  SGrepResultsNotActive = 'The Grep Results window is not active';

const
  cEmbeddedLeft = 2;
  cEmbeddedTop = 55;

{ TfmGrepSearch }

procedure TfmGrepSearch.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cbDirectory.Text;
  if GetDirectory(Temp) then
    cbDirectory.Text := Temp;
end;

procedure TfmGrepSearch.btnOptionsClick(Sender: TObject);
var
  UseCurrentIdent: Boolean;
begin
  UseCurrentIdent := GrepExpert.GrepUseCurrentIdent;
  if TfmGrepOptions.Execute(UseCurrentIdent) then
    GrepExpert.GrepUseCurrentIdent := UseCurrentIdent;
end;

procedure TfmGrepSearch.EnableDirectoryControls(New: Boolean);
begin
  cbDirectory.Enabled := New;
  cbMasks.Enabled := New;
  cbExcludedDirs.Enabled := New;
  cbInclude.Enabled := New;
  btnBrowse.Enabled := New;
  if not New then
  begin
    cbDirectory.Color := clBtnface;
    cbExcludedDirs.Color := clBtnface;
    cbMasks.Color := clBtnface;
  end
  else
  begin
    cbDirectory.Color := clWindow;
    cbExcludedDirs.Color := clWindow;
    cbMasks.Color := clWindow;
  end
end;

procedure TfmGrepSearch.rbDirectoriesClick(Sender: TObject);
begin
  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 1);
end;

procedure TfmGrepSearch.cbDirectoryDropDown(Sender: TObject);
begin
  SizeComboDropdownToItems(cbDirectory);
end;

procedure TfmGrepSearch.cbExcludedDirsDropDown(Sender: TObject);
begin
  SizeComboDropdownToItems(cbExcludedDirs);
end;

procedure TfmGrepSearch.ShowControlHint(ctrl: TWinControl; const HintText: string);
var
  r: TRect;
  Size: TSize;
begin
  timHintTimer.Enabled := False;
  if Assigned(FTheHintWindow) then begin
    FTheHintWindow.ReleaseHandle;
    FTheHintWindow.Free;
  end;
  FTheHintWindow := THintWindow.Create(Self);
  FTheHintWindow.Color := clInfoBk;
  r.TopLeft := ctrl.ClientToScreen(Point(0, ctrl.Height));
  Size := FTheHintWindow.Canvas.TextExtent(HintText);
  r.Right := r.Left + Size.cx + 8;
  r.Bottom := r.Top + Size.cy;
  FTheHintWindow.ActivateHint(r, HintText);
  timHintTimer.Enabled := True;
end;

procedure TfmGrepSearch.CheckContentTypeSelection(ClickedOption: TCheckBox);
resourcestring
  SCannotDisableAllContentTypes = 'You cannot disable all content types.';
begin
  if FLoadingSettings then
    Exit;

  if cbGrepCode.Checked or cbGrepStrings.Checked or cbGrepComments.Checked then begin
    // at least one option is selected -> OK
  end else begin
    if Assigned(ClickedOption) then begin
      // unchecked interactively -> check the last unchecked option again
      ClickedOption.Checked := True;
      ShowControlHint(ClickedOption, SCannotDisableAllContentTypes);
    end else begin
      // not interactively -> check them all
      cbGrepCode.Checked := True;
      cbGrepStrings.Checked := True;
      cbGrepComments.Checked := True;
    end;
  end;
end;

procedure TfmGrepSearch.cbGrepCodeClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepCode);
end;

procedure TfmGrepSearch.cbGrepCommentsClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepComments);
end;

procedure TfmGrepSearch.cbGrepStringsClick(Sender: TObject);
begin
  CheckContentTypeSelection(cbGrepStrings);
end;

procedure TfmGrepSearch.CheckSectionSelection(ClickedOption: TCheckBox);
resourcestring
  SCannotDisableAllContentTypes = 'You cannot disable all unit sections.';
begin
  if FLoadingSettings then
    Exit;

  if cbSectionInterface.Checked or cbSectionImplementation.Checked
    or cbSectionInitialization.Checked or cbSectionFinalization.Checked then begin
    // at least one option is selected -> OK
  end else begin
    if Assigned(ClickedOption) then begin
      // unchecked interactively -> check the last unchecked option again
      ClickedOption.Checked := True;
      ShowControlHint(ClickedOption, SCannotDisableAllContentTypes);
    end else begin
      // not interactively -> check them all
      cbSectionInterface.Checked := True;
      cbSectionImplementation.Checked := True;
      cbSectionInitialization.Checked := True;
      cbSectionFinalization.Checked := True;
    end;
  end;
end;

procedure TfmGrepSearch.cbSectionFinalizationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionFinalization);
end;

procedure TfmGrepSearch.cbSectionImplementationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionImplementation);
end;

procedure TfmGrepSearch.cbSectionInitializationClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionInitialization);
end;

procedure TfmGrepSearch.cbSectionInterfaceClick(Sender: TObject);
begin
  CheckSectionSelection(cbSectionInterface);
end;

{ TGrepDlgExpert }

constructor TGrepDlgExpert.Create;
begin
  inherited Create;
end;

function TGrepDlgExpert.GetActionCaption: string;
resourcestring
  SActionCaption = '&Grep Search...';
begin
  Result := SActionCaption;
end;

function TGrepDlgExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Word('S'), [ssAlt, ssShift]);
end;

function TGrepDlgExpert.GetHelpString: string;
resourcestring
  SHelpString =
  '  Grep regular expressions allow you to formulate complex searches'#13#10
  + '  that are not possible using a basic text search.'#13#10
  + '  GExperts implements a subset of the Perl regular expression syntax.';
begin
  Result := SHelpString;
end;

class function TGrepDlgExpert.GetName: string;
begin
  Result := 'GrepSearch'; // Do not localize.
end;

procedure TGrepDlgExpert.Execute(Sender: TObject);
begin
  if Assigned(fmGrepResults) then
    fmGrepResults.Execute(gssNormal)
  else
    raise Exception.Create(SGrepResultsNotActive);
end;

procedure TGrepDlgExpert.Configure;
var
  GrepExpert: TGrepExpert;
  UseCurrentIdent: Boolean;
begin
  if not Assigned(fmGrepResults) then
    raise Exception.Create(SGrepResultsNotActive);

  GrepExpert := fmGrepResults.GrepExpert;
  Assert(Assigned(GrepExpert));

  UseCurrentIdent := GrepExpert.GrepUseCurrentIdent;
  if TfmGrepOptions.Execute(UseCurrentIdent) then
    GrepExpert.GrepUseCurrentIdent := UseCurrentIdent;
end;

procedure TfmGrepSearch.btnOKClick(Sender: TObject);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist.';
  SSearchTextEmpty = 'The search text is empty.';
var
  i: Integer;
  Dirs: TStringList;
begin
  if IsEmpty(cbText.Text) then
    raise Exception.Create(SSearchTextEmpty);
  
  if rbDirectories.Checked then
  begin
    if Trim(cbDirectory.Text) = '' then
      cbDirectory.Text := GetCurrentDir;
    Dirs := TStringList.Create;
    try
      AnsiStrTok(cbDirectory.Text, ';', Dirs);
      for i := 0 to Dirs.Count - 1 do
      begin
        Dirs[i] := ExpandFileName(AddSlash(Dirs[i]));
        if not DirectoryExists(Dirs[i]) then
          raise Exception.CreateFmt(SSpecifiedDirectoryDoesNotExist, [Dirs[i]]);
        if i < Dirs.Count - 1 then
          Dirs[i] := Dirs[i] + ';'
      end;
      cbDirectory.Text := StringReplace(Dirs.Text, #13#10, '', [rfReplaceAll]);
    finally
      FreeAndNil(Dirs);
    end;
  end;
  while StrBeginsWith(';', cbExcludedDirs.Text) do
    cbExcludedDirs.Text := Copy(cbExcludedDirs.Text, 2, MaxInt);
  cbExcludedDirs.Text := StringReplace(cbExcludedDirs.Text, ';;', ';', [rfReplaceAll]);

  SaveFormSettings;

  if cbRegEx.Checked then
  try
    ExecRegExpr(cbText.Text, '');
  except
    on E: ERegExpr do begin
      ShowError(E.Message);
      TryFocusControl(cbText);
      cbText.SelStart := E.CompilerErrorPos;
      cbText.SelLength := 0;
      Abort;
    end;
  end;

  ModalResult := mrOk;
end;

constructor TfmGrepSearch.Create(AOwner: TComponent);
begin
  inherited;
  FEmbedded := False;
  FEmbeddedHolder := nil;

  TWinControl_ActivateDropFiles(cbDirectory, cbDirectoryOnDropFiles);

  pnlBottom.BevelOuter := bvNone;

  LoadFormSettings;
  FCheckedWhere := True;
end;

procedure TfmGrepSearch.FormCreate(Sender: TObject);
begin
  FSaveWidth := Width;
  FSaveOptionsGroupWidth := gbxOptions.Width;
  FSaveWhereGroupWidth := gbxWhere.Width;
  FSaveWhereGroupLeft := gbxWhere.Left;
end;

procedure TfmGrepSearch.cbDirectoryOnDropFiles(_Sender: TObject; _Files: TStrings);
var
  s: string;
begin
  if IsShiftDown then
    s := cbDirectory.Text + ';'
  else
    s := '';
  cbDirectory.Text := s + _Files.DelimitedText;
end;

procedure TfmGrepSearch.SaveFormSettings;
begin
  AddMRUString(cbText.Text, FGrepExpert.SearchList, False, 90, -1);
  AddMRUString(cbDirectory.Text, FGrepExpert.DirList, True);
  AddMRUString(cbMasks.Text, FGrepExpert.MaskList, False);
  AddMRUString(cbExcludedDirs.Text, FGrepExpert.ExcludedDirsList, False, True);

  FGrepExpert.GrepCaseSensitive := cbCaseSensitive.Checked;
  FGrepExpert.GrepCode := cbGrepCode.Checked;
  FGrepExpert.GrepComments := cbGrepComments.Checked;
  FGrepExpert.GrepStrings := cbGrepStrings.Checked;
  FGrepExpert.GrepFinalization := cbSectionFinalization.Checked;
  FGrepExpert.GrepImplementation := cbSectionImplementation.Checked;
  FGrepExpert.GrepInitialization := cbSectionInitialization.Checked;
  FGrepExpert.GrepInterface := cbSectionInterface.Checked;
  FGrepExpert.GrepForms := cbForms.Checked;
  FGrepExpert.GrepSQLFiles := cbSQLFiles.Checked;
  FGrepExpert.GrepSub := cbInclude.Checked;
  FGrepExpert.GrepWholeWord := cbWholeWord.Checked;
  FGrepExpert.GrepRegEx := cbRegEx.Checked;

  FGrepExpert.GrepSaveOption := TGrepSaveOption(rgSaveOption.ItemIndex);

  if rbCurrentOnly.Checked then
    FGrepExpert.GrepSearch := 0
  else if rbAllProjFiles.Checked then
    FGrepExpert.GrepSearch := 1
  else if rbOpenFiles.Checked then
    FGrepExpert.GrepSearch := 2
  else if rbDirectories.Checked then
    FGrepExpert.GrepSearch := 3
  else if rbAllProjGroupFiles.Checked then
    FGrepExpert.GrepSearch := 4
  else if rbResults.Checked then
    FGrepExpert.GrepSearch := 5;
end;

procedure TfmGrepSearch.timHintTimerTimer(Sender: TObject);
begin
  timHintTimer.Enabled := False;
  if Assigned(FTheHintWindow) then begin
    FTheHintWindow.ReleaseHandle;
    FreeAndNil(FTheHintWindow);
  end;
end;

procedure TfmGrepSearch.LoadFormSettings;

  function RetrieveEditorBlockSelection: string;
  var
    Temp: string;
    i: Integer;
  begin
    Temp := GxOtaGetCurrentSelection;
    // Only use the currently selected text if the length is between 1 and 80
    if (Length(Trim(Temp)) >= 1) and (Length(Trim(Temp)) <= 80) then
    begin
      i := Min(Pos(#13, Temp), Pos(#10, Temp));
      if i > 0 then
        Temp := Copy(Temp, 1, i - 1);
    end else
      Temp := '';
    Result := Temp;
  end;

  procedure SetSearchPattern(Str: string);
  begin
    cbText.Text := Str;
    cbText.SelectAll;
  end;

  procedure SetDefaultSearchPattern;
  var
    Selection: string;
  begin
    Selection := fmGrepResults.ContextSearchText;
    if Trim(Selection) = '' then
      Selection := RetrieveEditorBlockSelection;
    if (Trim(Selection) = '') and FGrepExpert.GrepUseCurrentIdent then
      try
        Selection := GxOtaGetCurrentIdent;  //if access violation created
      except
        on E: Exception do
          Selection := '';
      end;
    if (Selection = '') and (cbText.Items.Count > 0) then
      Selection := cbText.Items[0];
    SetSearchPattern(Selection);
  end;

begin
  if not Assigned(fmGrepResults) then
    raise Exception.Create(SGrepResultsNotActive);

  FGrepExpert := fmGrepResults.GrepExpert;

  FLoadingSettings := True;
  try
    cbText.Items.Assign(FGrepExpert.SearchList);
    cbDirectory.Items.Assign(FGrepExpert.DirList);
    cbMasks.Items.Assign(FGrepExpert.MaskList);
    cbExcludedDirs.Items.Assign(FGrepExpert.ExcludedDirsList);
    rbResults.Enabled := fmGrepResults.lbResults.Count > 0;

    cbCaseSensitive.Checked := FGrepExpert.GrepCaseSensitive;
    cbGrepCode.Checked := FGrepExpert.GrepCode;
    cbGrepComments.Checked := FGrepExpert.GrepComments;
    cbGrepStrings.Checked := FGrepExpert.GrepStrings;
    cbSectionFinalization.Checked := FGrepExpert.GrepFinalization;
    cbSectionImplementation.Checked := FGrepExpert.GrepImplementation;
    cbSectionInitialization.Checked := FGrepExpert.GrepInitialization;
    cbSectionInterface.Checked := FGrepExpert.GrepInterface;
    cbForms.Checked := FGrepExpert.GrepForms;
    cbSQLFiles.Checked := FGrepExpert.GrepSQLFiles;
    cbInclude.Checked := FGrepExpert.GrepSub;
    cbWholeWord.Checked := FGrepExpert.GrepWholeWord;
    cbRegEx.Checked := FGrepExpert.GrepRegEx;

    rgSaveOption.ItemIndex := Integer(FGrepExpert.SaveOption);

    case FGrepExpert.GrepSearch of
      0: rbCurrentOnly.Checked := True;
      1: rbAllProjFiles.Checked := True;
      2: rbOpenFiles.Checked := True;
      3: rbDirectories.Checked := True;
      4: rbAllProjGroupFiles.Checked := True;
      5: begin
          if rbResults.Enabled then
            rbResults.Checked := True
          else
            rbAllProjFiles.Checked := True;
        end;
    else
      rbAllProjFiles.Checked := True;
    end;

    if cbText.Items.Count > 0 then
      cbText.Text := cbText.Items[0];
    if cbDirectory.Items.Count > 0 then
      cbDirectory.Text := cbDirectory.Items[0];
    if cbMasks.Items.Count > 0 then
      cbMasks.Text := cbMasks.Items[0];
    if cbExcludedDirs.Items.Count > 0 then
      cbExcludedDirs.Text := cbExcludedDirs.Items[0];

    if not FGrepExpert.GrepSaveHistoryListItems then begin
      rgSaveOption.Visible := False;
      Height := Height - rgSaveOption.Height;
    end;

    if not IsStandAlone then
      SetDefaultSearchPattern;
  finally
    FLoadingSettings := False;
  end;

  CheckContentTypeSelection(nil);
  CheckSectionSelection(nil);
  CheckEnabledWhereControls;

  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.CheckEnabledWhereControls;
begin
  if not IsStandAlone then
  begin
    if Trim(GxOtaGetCurrentProjectName) = '' then
    begin
      rbAllProjFiles.Enabled := False;
      rbOpenFiles.Enabled := False;
    end
    else
    begin
      rbAllProjFiles.Enabled := True;
      rbOpenFiles.Enabled := True;
    end;

    if Trim(GxOtaGetProjectGroupFileName) = '' then
      rbAllProjGroupFiles.Enabled := False
    else
      rbAllProjGroupFiles.Enabled := True;

    rbCurrentOnly.Enabled := Trim(GxOtaGetFileNameOfCurrentModule) <> '';
  end
  else // IsStandAlone
  begin
    rbDirectories.Checked := True;
    rbAllProjFiles.Enabled := False;
    rbOpenFiles.Enabled := False;
    rbAllProjGroupFiles.Enabled := False;
    rbAllProjFiles.Enabled := False;
    rbCurrentOnly.Enabled := False;
  end;
end;

procedure TfmGrepSearch.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.IncludeComments := cbGrepComments.Checked;
  Value.IncludeCode := cbGrepCode.Checked;
  Value.IncludeStrings := cbGrepStrings.Checked;
  Value.SectionInterface := cbSectionInterface.Checked;
  Value.SectionImplementation := cbSectionImplementation.Checked;
  Value.SectionInitialization := cbSectionInitialization.Checked;
  Value.SectionFinalization := cbSectionFinalization.Checked;
  Value.CaseSensitive := cbCaseSensitive.Checked;
  Value.WholeWord := cbWholeWord.Checked;
  Value.RegEx := cbRegEx.Checked;
  Value.Pattern := cbText.Text;
  Value.IncludeForms := cbForms.Checked;
  Value.IncludeSQLs := cbSQLFiles.Checked;
  Value.SaveOption := TGrepSaveOption(rgSaveOption.ItemIndex);
  Value.Mask := '';
  Value.Directories := '';
  Value.ExcludedDirs := '';
  Value.IncludeSubdirs := True;

  if rbAllProjFiles.Checked then
    Value.GrepAction := gaProjGrep
  else if rbCurrentOnly.Checked then
    Value.GrepAction := gaCurrentOnlyGrep
  else if rbOpenFiles.Checked then
    Value.GrepAction := gaOpenFilesGrep
  else if rbAllProjGroupFiles.Checked then
    Value.GrepAction := gaProjGroupGrep
  else if rbResults.Checked then
    Value.GrepAction := gaResults
  else
  begin
    Value.GrepAction := gaDirGrep;
    Value.Mask := cbMasks.Text;
    Value.Directories := cbDirectory.Text;
    Value.IncludeSubdirs := cbInclude.Checked;
    Value.ExcludedDirs := cbExcludedDirs.Text;
  end;
end;

procedure TfmGrepSearch.AdjustSettings(Value: TGrepSettings);
begin
  cbGrepComments.Checked := Value.IncludeComments;
  cbGrepCode.Checked := Value.IncludeCode;
  cbGrepStrings.Checked := Value.IncludeStrings;
  cbSectionInterface.Checked := Value.SectionInterface;
  cbSectionImplementation.Checked := Value.SectionImplementation;
  cbSectionInitialization.Checked := Value.SectionInitialization;
  cbSectionFinalization.Checked := Value.SectionFinalization;
  cbCaseSensitive.Checked := Value.CaseSensitive;
  cbWholeWord.Checked := Value.WholeWord;
  cbRegEx.Checked := Value.RegEx;
  cbText.Text := Value.Pattern;
  cbText.SelectAll;
  cbForms.Checked := Value.IncludeForms;
  cbSQLFiles.Checked := Value.IncludeSQLs;

  rgSaveOption.ItemIndex := Integer(Value.SaveOption);

  cbMasks.Text := '';
  cbDirectory.Text := '';
  cbExcludedDirs.Text := '';
  cbInclude.Checked := True;
  case Value.GrepAction of
    gaProjGrep: rbAllProjFiles.Checked := True;
    gaCurrentOnlyGrep: rbCurrentOnly.Checked := True;
    gaOpenFilesGrep: rbOpenFiles.Checked := True;
    gaProjGroupGrep: rbAllProjGroupFiles.Checked := True;
    gaResults: rbResults.Checked := True;
    gaDirGrep:
    begin
      rbDirectories.Checked := True;
      cbMasks.Text := Value.Mask;
      cbDirectory.Text := Value.Directories;
      cbInclude.Checked := Value.IncludeSubdirs;
      cbExcludedDirs.Text := Value.ExcludedDirs;
    end ;
  end;
  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.FormShow(Sender: TObject);
begin
  TControl_SetMinConstraints(Self, True);
end;

procedure TfmGrepSearch.ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  SelectedIndex: Integer;
  Combo: TCustomComboBox;
begin
  if (Key = VK_DELETE) and (ssCtrl in Shift) then
  begin
    Combo := (Sender as TCustomComboBox);
    if Combo.DroppedDown then
    begin
      SelectedIndex := Combo.ItemIndex;
      if SelectedIndex > -1 then begin
        Combo.Items.Delete(SelectedIndex);
        UpdateMRUs;
      end;
    end;
  end;
end;

procedure TfmGrepSearch.UpdateMRUs;
begin
  FGrepExpert.SearchList.Assign(cbText.Items);
  FGrepExpert.DirList.Assign(cbDirectory.Items);
  FGrepExpert.MaskList.Assign(cbMasks.Items);
  FGrepExpert.ExcludedDirsList.Assign(cbExcludedDirs.Items);
end;

procedure TfmGrepSearch.EmbeddedInit(AHolderControl: TWinControl; ASearchEvent: TNotifyEvent);
begin
  FEmbedded := True;
  FEmbeddedHolder := AHolderControl;
  //.Left + 2, lbResults.Top + 55, lbResults.Width - 4

  Parent := TWinControl(Owner);
  Height := ClientHeight;
  BorderIcons := [];
  BorderStyle := bsNone; //ToolWindow;
  Position := poDesigned;
  FormStyle := fsStayOnTop;

  btnSearch.Left := btnOptions.Left;
  EmbeddedSetHeights;
  EmbeddedUpdatePos;

  btnOK.Visible := False;
  btnCancel.Visible := False;
  btnOptions.Visible := False;

  btnSearch.Visible := True;
  btnSearch.OnClick := ASearchEvent;

  FCheckedWhere := False;
end;

procedure TfmGrepSearch.EmbeddedSetHeights;

  function MoveTo(ATopDelta: Integer; AMainCtrl: TControl; AItemsDelta: Integer; AItems: array of TControl): Integer;
  var
    I, ADelta: Integer;
  begin
    AMainCtrl.Top := AMainCtrl.Top - ATopDelta;
    Result := 0;
    for I := 0 to High(AItems) do
    begin
      if AItemsDelta > 0 then
      begin
        ADelta := (I+1) * AItemsDelta;
        Inc(Result, ADelta div 2);
      end
      else
        ADelta := ATopDelta;
      AItems[I].Top := AItems[I].Top - ADelta;
    end;
    if High(AItems) = -1 then
      Inc(Result, AItemsDelta);
    if AItemsDelta > 0 then
      AMainCtrl.Height := AMainCtrl.Height - Result;
    Inc(Result, ATopDelta);
  end;

var
  LHS: Integer;  // LastHeightsSum
begin
  MoveTo(5, cbText, 0, [lblFind]);

         MoveTo(10, gbxOptions, 3, [cbCaseSensitive, cbWholeWord, cbForms, cbSQLFiles, cbRegEx]);
  LHS := MoveTo(10, gbxWhere, 5, [rbCurrentOnly, rbAllProjGroupFiles, rbAllProjFiles, rbOpenFiles, rbDirectories, rbResults]);
         gbxWhere.Height := gbxWhere.Height + 13;
         gbxOptions.Height := gbxWhere.Height;

         MoveTo(-5 + LHS, gbxContentTypes, 3, [cbGrepCode, cbGrepStrings, cbGrepComments]);
  LHS := MoveTo(-5 + LHS, gbxUnitSections, 3, [cbSectionInterface, cbSectionImplementation, cbSectionInitialization, cbSectionFinalization]);
         gbxContentTypes.Height := gbxUnitSections.Height;

  LHS := MoveTo(4 + LHS, gbxDirectories, 5, [cbDirectory, cbExcludedDirs, cbMasks, cbInclude]);
         lblDirectory.Top := cbDirectory.Top;
         btnBrowse.Top := cbDirectory.Top;
         lblExcludeDirs.Top := cbExcludedDirs.Top;
         lblMasks.Top := cbMasks.Top;

  LHS := MoveTo(5 + LHS, rgSaveOption, 15, []);

  Height := Height - LHS - 3;
end;

procedure TfmGrepSearch.EmbeddedUpdatePos;
const
  cMinWidth = 382;
  cWhereWidthCorrection = 5; //???
var
  ADelta, ADeltaLeft, ADeltaRight, AWidth: Integer;
begin
  Left := FEmbeddedHolder.Left + cEmbeddedLeft;
  Top := FEmbeddedHolder.Top + cEmbeddedTop;

  AWidth := FEmbeddedHolder.Width - 4;
  if AWidth >= FSaveWidth then
    Exit;

  AWidth := Max(AWidth, cMinWidth);
  gbxWhere.Anchors := [akTop, akLeft];
  gbxUnitSections.Anchors := [akTop, akLeft];

  ADelta := FSaveWidth - AWidth;
  ADeltaLeft := ADelta div 2;
  ADeltaRight := ADelta - ADeltaLeft;
  Width := AWidth;

  gbxOptions.Width := FSaveOptionsGroupWidth - ADeltaLeft;
  gbxContentTypes.Width := FSaveOptionsGroupWidth - ADeltaLeft;
  gbxWhere.Left := FSaveWhereGroupLeft - ADeltaLeft;
  gbxWhere.Width := FSaveWhereGroupWidth - ADeltaRight + cWhereWidthCorrection;
  gbxUnitSections.Left := FSaveWhereGroupLeft - ADeltaLeft;
  gbxUnitSections.Width := FSaveWhereGroupWidth - ADeltaRight + cWhereWidthCorrection;
end;

procedure TfmGrepSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if FEmbedded then
    Action := caHide;
end;

procedure TfmGrepSearch.EmbeddedShow;
begin
  if not FEmbedded then
    Exit;

  if not FCheckedWhere then
  begin
    FCheckedWhere := True;
    CheckEnabledWhereControls;
  end;

  if not Visible then
    EmbeddedUpdatePos;

  Show;
  BringToFront;
end;

initialization
  RegisterGX_Expert(TGrepDlgExpert);
end.

