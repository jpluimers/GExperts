unit GX_GrepSearch;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls,
  GX_Experts, GX_GrepExpert, GX_GrepBackend, GX_BaseForm;

type
  TfmGrepSearch = class(TfmBaseForm)
    lblFind: TLabel;
    cbText: TComboBox;
    gbxOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    cbNoComments: TCheckBox;
    cbForms: TCheckBox;
    gbxWhere: TGroupBox;
    rbAllProjFiles: TRadioButton;
    rbOpenFiles: TRadioButton;
    rbDirectories: TRadioButton;
    gbxDirectories: TGroupBox;
    lblMasks: TLabel;
    cbMasks: TComboBox;
    cbInclude: TCheckBox;
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
    procedure btnBrowseClick(Sender: TObject);
    procedure rbDirectoriesClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbDirectoryDropDown(Sender: TObject);
    procedure cbExcludedDirsDropDown(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FGrepExpert: TGrepExpert;
    procedure EnableDirectoryControls(New: Boolean);
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure UpdateMRUs;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RetrieveSettings(var Value: TGrepSettings);
    property GrepExpert: TGrepExpert read FGrepExpert;
  end;

  TGrepDlgExpert = class(TGX_Expert)
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, Graphics, Menus,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_GrepResults, GX_GrepOptions,
  GX_GrepRegExSearch, RegExpr, Math;

resourcestring
  SGrepResultsNotActive = 'The Grep Results window is not active';

procedure TfmGrepSearch.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cbDirectory.Text;
  if GetDirectory(Temp) then
    cbDirectory.Text := Temp;
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

{ TGrepDlgExpert }

constructor TGrepDlgExpert.Create;
begin
  inherited Create;

  ShortCut := Menus.ShortCut(Word('S'), [ssAlt, ssShift]);
end;

function TGrepDlgExpert.GetActionCaption: string;
resourcestring
  SActionCaption = '&Grep Search...';
begin
  Result := SActionCaption;
end;

class function TGrepDlgExpert.GetName: string;
begin
  Result := 'GrepSearch'; // Do not localize.
end;

procedure TGrepDlgExpert.Click(Sender: TObject);
begin
  if Assigned(fmGrepResults) then
    fmGrepResults.Execute(False)
  else
    raise Exception.Create(SGrepResultsNotActive);
end;

procedure TGrepDlgExpert.Configure;
var
  Dialog: TfmGrepOptions;
  GrepExpert: TGrepExpert;
begin
  if not Assigned(fmGrepResults) then
    raise Exception.Create(SGrepResultsNotActive);

  GrepExpert := fmGrepResults.GrepExpert;
  Assert(Assigned(GrepExpert));

  Dialog := TfmGrepOptions.Create(nil);
  try
    Dialog.chkGrepUseCurrentIdent.Checked := GrepExpert.GrepUseCurrentIdent;
    if Dialog.ShowModal = mrOk then
    begin
      GrepExpert.GrepUseCurrentIdent := Dialog.chkGrepUseCurrentIdent.Checked;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TfmGrepSearch.btnOKClick(Sender: TObject);
resourcestring
  SSpecifiedDirectoryDoesNotExist = 'The search directory %s does not exist';
var
  i: Integer;
  Dirs: TStringList;
begin
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
  LoadFormSettings;
end;

procedure TfmGrepSearch.SaveFormSettings;
begin
  AddMRUString(cbText.Text, FGrepExpert.SearchList, False);
  AddMRUString(cbDirectory.Text, FGrepExpert.DirList, True);
  AddMRUString(cbMasks.Text, FGrepExpert.MaskList, False);
  AddMRUString(cbExcludedDirs.Text, FGrepExpert.ExcludedDirsList, False, True);

  FGrepExpert.GrepCaseSensitive := cbCaseSensitive.Checked;
  //FGrepExpert.IncludeComments := not cbNoComments.Checked;
  FGrepExpert.GrepForms := cbForms.Checked;
  FGrepExpert.GrepSub := cbInclude.Checked;
  FGrepExpert.GrepWholeWord := cbWholeWord.Checked;
  FGrepExpert.GrepRegEx := cbRegEx.Checked;

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
    Selection := RetrieveEditorBlockSelection;
    if (Trim(Selection) = '') and FGrepExpert.GrepUseCurrentIdent then
      Selection := GxOtaGetCurrentIdent;
    if (Selection = '') and (cbText.Items.Count > 0) then
      Selection := cbText.Items[0];
    SetSearchPattern(Selection);
  end;

begin
  if not Assigned(fmGrepResults) then
    raise Exception.Create(SGrepResultsNotActive);

  FGrepExpert := fmGrepResults.GrepExpert;
  cbText.Items.Assign(FGrepExpert.SearchList);
  cbDirectory.Items.Assign(FGrepExpert.DirList);
  cbMasks.Items.Assign(FGrepExpert.MaskList);
  cbExcludedDirs.Items.Assign(FGrepExpert.ExcludedDirsList);
  rbResults.Enabled := fmGrepResults.lbResults.Count > 0;

  cbCaseSensitive.Checked := FGrepExpert.GrepCaseSensitive;
  //cbNoComments.Checked := not FGrepExpert.GrepIncludeComments;
  cbForms.Checked := FGrepExpert.GrepForms;
  cbInclude.Checked := FGrepExpert.GrepSub;
  cbWholeWord.Checked := FGrepExpert.GrepWholeWord;
  cbRegEx.Checked := FGrepExpert.GrepRegEx;
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

  if not IsStandAlone then
  begin
    SetDefaultSearchPattern;

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

  EnableDirectoryControls(rbDirectories.Checked);
end;

procedure TfmGrepSearch.RetrieveSettings(var Value: TGrepSettings);
begin
  Value.IncludeComments := not cbNoComments.Checked;
  Value.CaseSensitive := cbCaseSensitive.Checked;
  Value.WholeWord := cbWholeWord.Checked;
  Value.RegEx := cbRegEx.Checked;
  Value.Pattern := cbText.Text;
  Value.IncludeSubdirs := cbInclude.Checked;
  Value.Mask := '';
  Value.Directories := '';
  Value.ExcludedDirs := cbExcludedDirs.Text;
  Value.IncludeForms := cbForms.Checked;

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
  end;
end;

procedure TfmGrepSearch.FormShow(Sender: TObject);
begin
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
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

initialization
  RegisterGX_Expert(TGrepDlgExpert);
end.

