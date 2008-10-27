unit GX_ProofreaderConfig;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ActnList, ToolWin, ExtCtrls, Menus, Messages,
  GX_ProofreaderExpert, GX_ProofreaderData, GX_ProofreaderUtils, GX_SharedImages;

const
  UM_UPDATECOLS = WM_USER + 632;

type
  TfmProofreaderConfig = class(TForm)
    dlgGetWordlist: TOpenDialog;
    dlgPutWordlist: TSaveDialog;
    Actions: TActionList;
    actListDelete: TAction;
    actListInsert: TAction;
    actListEdit: TAction;
    pmList: TPopupMenu;
    pmiListInsert: TMenuItem;
    pmiListEdit: TMenuItem;
    pmiListDelete: TMenuItem;
    pmHistory: TPopupMenu;
    actDisableRule: TAction;
    pmiDisableRule: TMenuItem;
    pnlMain: TPanel;
    pnlButtons: TPanel;
    Pages: TPageControl;
    tabReplacement: TTabSheet;
    pnlReplacement: TPanel;
    lvReplacement: TListView;
    pnlACHeader: TPanel;
    tbrReplacement: TToolBar;
    tbnReplacementInsert: TToolButton;
    tbnReplacementEdit: TToolButton;
    tbnReplacementDelete: TToolButton;
    cbReplacerActive: TCheckBox;
    tabDictionary: TTabSheet;
    pnlDictionary: TPanel;
    gbxWords: TGroupBox;
    pnlWords: TPanel;
    tbrDictionary: TToolBar;
    tbnDictionaryInsert: TToolButton;
    tbnDictionaryEdit: TToolButton;
    tbnDictionaryDelete: TToolButton;
    tbnDictionarySep: TToolButton;
    tbnDictionaryExport: TToolButton;
    tbnDictionaryImport: TToolButton;
    lvDictionary: TListView;
    pnlDictOptions: TPanel;
    gbReplaceIf: TGroupBox;
    cbOneCharIncorrect: TCheckBox;
    cbAllowOneCharacterMissing: TCheckBox;
    cbAllowExtraChar: TCheckBox;
    cbCaseDiffer: TCheckBox;
    cbAllowSwitchedChars: TCheckBox;
    cbMustBeNearbyLetter: TCheckBox;
    cbFirstCharMustBeCorrect: TCheckBox;
    cbEnableDictionary: TCheckBox;
    cbEnableCompiler: TCheckBox;
    tabHistory: TTabSheet;
    pnlHistory: TPanel;
    lvHistory: TListView;
    pnlHistoryButtons: TPanel;
    btnDisableRule: TButton;
    pnlTop: TPanel;
    lblRules: TLabel;
    cbLanguage: TComboBox;
    cbBeep: TCheckBox;
    actImportWords: TAction;
    actExportWords: TAction;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure actDisableRuleExecute(Sender: TObject);
    procedure cbEnableDicitionaryClick(Sender: TObject);
    procedure cbEnableCompilerClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbOneCharIncorrectClick(Sender: TObject);
    procedure lvDictionaryDblClick(Sender: TObject);
    procedure lvReplacementDblClick(Sender: TObject);
    procedure actListInsertExecute(Sender: TObject);
    procedure actListEditExecute(Sender: TObject);
    procedure actListDeleteExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actImportWordsExecute(Sender: TObject);
    procedure actExportWordsExecute(Sender: TObject);
    procedure lvDictionaryData(Sender: TObject; Item: TListItem);
    procedure lvReplacementData(Sender: TObject; Item: TListItem);
    procedure PagesChange(Sender: TObject);
    procedure UMUpdateCols(var Msg: TMessage); message UM_UPDATECOLS;
  private
    FProofreaderExpert: TCodeProofreaderExpert;
    FProofreaderData: TProofreaderData;
    procedure UpdateHistoryGrid;
    procedure UpdateReplacementList;
    procedure UpdateDictionaryList;
    function GetReplacementSource: TReplacementSource;
    function ActiveTab: TTabSheet;
    procedure UpdateDisplayedData;
    procedure UpdateColumnWidths;
  public
    constructor Create(AOwner: TComponent; ProofreaderExpert: TCodeProofreaderExpert; ProofreaderData: TProofreaderData); reintroduce;
  end;

implementation

uses
  SysUtils, Windows,
  GX_ProofreaderAutoCorrectEntry, GX_KibitzComp,
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils;

{$R *.dfm}

constructor TfmProofreaderConfig.Create(AOwner: TComponent;
  ProofreaderExpert: TCodeProofreaderExpert; ProofreaderData: TProofreaderData);
var
  TabIndex: Integer;
begin
  inherited Create(AOwner);

  SetToolbarGradient(tbrReplacement);
  SetToolbarGradient(tbrDictionary);
  SetDefaultFont(Self);
  Assert(Assigned(ProofreaderExpert));
  FProofreaderExpert := ProofreaderExpert;
  Assert(Assigned(ProofreaderData));
  FProofreaderData := ProofreaderData;

  TabIndex := FProofreaderData.ActiveTab;
  if (TabIndex > -1) and (TabIndex < Pages.PageCount) then
    Pages.ActivePageIndex := TabIndex;
end;

function TfmProofreaderConfig.GetReplacementSource: TReplacementSource;
begin
  Assert(cbLanguage.ItemIndex > -1);
  Result := TReplacementSource(cbLanguage.ItemIndex);
end;

procedure TfmProofreaderConfig.UpdateHistoryGrid;
resourcestring
  SReplacementInfoHeader = 'Correction';
  SReplacementTime = 'Time';
var
  i: Integer;
  Item: TListItem;
  CorrectionHistory: TCorrectionHistory;
begin
  CorrectionHistory := FProofreaderData.History;
  lvHistory.Column[0].Caption := SReplacementInfoHeader;
  lvHistory.Column[1].Caption := SReplacementTime;
  lvHistory.Items.Clear;
  for i := 0 to CorrectionHistory.Count - 1 do
  begin
    Item := lvHistory.Items.Add;
    Item.Caption := CorrectionHistory[i].InfoString;
    Item.SubItems.Add(FormatDateTime('hh:nn:ss', CorrectionHistory[i].Time));
    Item.Data := Pointer(i);
  end;
end;

procedure TfmProofreaderConfig.UpdateReplacementList;
begin
  lvReplacement.Items.Count := FProofreaderData.GetReplacementCount(GetReplacementSource);
  UpdateColumnWidths;
  lvReplacement.Invalidate;
end;

procedure TfmProofreaderConfig.UpdateDictionaryList;
begin
  lvDictionary.Items.Count := FProofreaderData.GetDictionaryCount(GetReplacementSource);
  UpdateColumnWidths;
  lvDictionary.Invalidate;
end;

procedure TfmProofreaderConfig.FormShow(Sender: TObject);
var
  i: TReplacementSource;
begin
  // Fill combo box with "languages" to select from
  for i := Low(TReplacementSource) to High(TReplacementSource) do
    cbLanguage.Items.Add(ReplacementSourceText[i]);

  // Initialize option settings
  cbBeep.Checked := FProofreaderData.BeepOnReplace;
  cbReplacerActive.Checked := FProofreaderData.ReplacerActive;
  cbEnableDictionary.Checked := FProofreaderData.DictionaryActive;
  cbEnableCompiler.Checked := FProofreaderData.CompilerActive;
  cbCaseDiffer.Checked := FProofreaderData.DictionaryCaseMayDiffer;
  cbOneCharIncorrect.Checked := FProofreaderData.OneCharIncorrect;
  cbMustBeNearbyLetter.Checked := FProofreaderData.MustBeNearbyLetter;
  cbAllowOneCharacterMissing.Checked := FProofreaderData.AllowOneCharacterMissing;
  cbAllowExtraChar.Checked := FProofreaderData.AllowExtraChar;
  cbAllowSwitchedChars.Checked := FProofreaderData.AllowSwitchedChars;
  cbFirstCharMustBeCorrect.Checked := FProofreaderData.FirstCharMustBeCorrect;

  cbEnableCompiler.Enabled := KibitzEnabled;
  if not cbEnableCompiler.Enabled then
    cbEnableCompiler.Checked := False;

  case GxOtaGetCurrentSyntaxHighlighter of
    gxpNone, gxpPlaceHolder, gxpPAS:
      cbLanguage.ItemIndex := Ord(rtPasSrc);
    gxpCPP:
      cbLanguage.ItemIndex := Ord(rtCPPSrc);
    gxpSQL:
      cbLanguage.ItemIndex := Ord(rtSQLSrc);
    gxpCS:
      cbLanguage.ItemIndex := Ord(rtCSSrc);
  end;
  UpdateDisplayedData;
end;

procedure TfmProofreaderConfig.btnOKClick(Sender: TObject);
var
  Cursor: IInterface;
begin
  Cursor := TempHourGlassCursor;
  with FProofreaderData do
  begin
    // Store options
    BeepOnReplace := cbBeep.Checked;
    ReplacerActive := cbReplacerActive.Checked;
    DictionaryActive := cbEnableDictionary.Checked;
    CompilerActive := cbEnableCompiler.Checked;
    DictionaryCaseMayDiffer := cbCaseDiffer.Checked;
    OneCharIncorrect := cbOneCharIncorrect.Checked;
    MustBeNearbyLetter := cbMustBeNearbyLetter.Checked;
    AllowOneCharacterMissing := cbAllowOneCharacterMissing.Checked;
    AllowExtraChar := cbAllowExtraChar.Checked;
    AllowSwitchedChars := cbAllowSwitchedChars.Checked;
    FirstCharMustBeCorrect := cbFirstCharMustBeCorrect.Checked;
    ActiveTab := Pages.ActivePageIndex;

    // Save the modified dictionary and replacement tables to disk
    SaveData;
    // Reload data from the tables based on the new settings
    //ReloadData;
  end;
end;

procedure TfmProofreaderConfig.cbLanguageChange(Sender: TObject);
begin
  UpdateDisplayedData;
end;

procedure TfmProofreaderConfig.actDisableRuleExecute(Sender: TObject);
resourcestring
  SRuleRemoved = 'Rule has been removed from replacement table.';
  SWordAdded = 'The word "%s" has been added to the dictionary for %s.';
  SWordNotFound = 'The AutoCorrect word ''%s'' could not be found (%s)';
var
  Correction: TCorrectionItem;
  Index: Integer;
begin
  try
    Correction := FProofreaderData.History[Integer(lvHistory.Selected.Data)];
    if Correction <> nil then
    begin
      case Correction.CorrectionKind of
        ckAutoCorrection:
          begin
            Index := FProofreaderData.FindReplacementIndex(Correction.SourceLanguage,
              AnsiUpperCase(Correction.OriginalText));
            if Index >= 0 then
            begin
              FProofreaderData.DeleteReplacementEntry(Correction.SourceLanguage, Index, Correction.OriginalText);
              UpdateReplacementList;
              MessageDlg(SRuleRemoved, mtInformation, [mbOK], 0);
            end
            else
            begin
              MessageDlg(Format(SWordNotFound, [Correction.OriginalText, ReplacementSourceText[Correction.SourceLanguage]]),
                mtInformation, [mbOK], 0);
            end;
          end;

        ckWord:
          begin
            FProofreaderData.AddToDictionary(Correction.SourceLanguage, Correction.OriginalText);
            UpdateDictionaryList;
            MessageDlg(Format(SWordAdded, [Correction.OriginalText, ReplacementSourceText[Correction.SourceLanguage]]),
              mtInformation, [mbOK], 0);
          end;
      end; // case
    end;
  except
    on E: Exception do
    begin
      // Swallow exceptions
    end;
  end;
end;

procedure TfmProofreaderConfig.cbEnableDicitionaryClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to gbReplaceIf.ControlCount - 1 do
    gbReplaceIf.Controls[i].Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked;
  cbMustBeNearbyLetter.Enabled := cbMustBeNearbyLetter.Enabled and cbOneCharIncorrect.Checked;
  cbFirstCharMustBeCorrect.Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked
end;

procedure TfmProofreaderConfig.cbEnableCompilerClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to gbReplaceIf.ControlCount - 1 do
    gbReplaceIf.Controls[i].Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked;
  cbMustBeNearbyLetter.Enabled := cbMustBeNearbyLetter.Enabled and cbOneCharIncorrect.Checked;
  cbFirstCharMustBeCorrect.Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked
end;

procedure TfmProofreaderConfig.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 20);
end;

procedure TfmProofreaderConfig.cbOneCharIncorrectClick(Sender: TObject);
begin
  cbMustBeNearbyLetter.Enabled := cbOneCharIncorrect.Checked and cbOneCharIncorrect.Enabled;
end;

procedure TfmProofreaderConfig.lvDictionaryDblClick(Sender: TObject);
begin
  if Assigned(lvDictionary.Selected) then
    actListEdit.Execute
  else
    actListInsert.Execute;
end;

procedure TfmProofreaderConfig.lvReplacementDblClick(Sender: TObject);
begin
  if Assigned(lvReplacement.Selected) then
    actListEdit.Execute
  else
    actListInsert.Execute;
end;

function ExecuteDictionaryEntryForm(var Entry: string; Add: Boolean): Boolean;
resourcestring
  SDlgCaptionAdd = 'Add Dictionary Entry';
  SDlgCaptionEdit = 'Edit Dictionary Entry';
  SLabelEntry = 'Entry:';
var
  DictionaryWord: string;
begin
  if Add then
  begin
    DictionaryWord := '';
    Result := InputQuery(SDlgCaptionAdd, SLabelEntry, DictionaryWord);
  end
  else
  begin
    DictionaryWord := Entry;
    Result := InputQuery(SDlgCaptionEdit, SLabelEntry, DictionaryWord);
  end;

  if Result then
  begin
    DictionaryWord := Trim(DictionaryWord);
    if DictionaryWord = '' then
      Result := False
    else
      Entry := DictionaryWord;
  end;
end;

procedure TfmProofreaderConfig.actListInsertExecute(Sender: TObject);

  procedure InsertReplace;
  var
    TypedString, ReplaceWithString: string;
    ReplaceWhere: TGXWhereReplace;
  begin
    try
      if TfmProofreaderAutoCorrectEntry.Execute(TypedString, ReplaceWhere, ReplaceWithString, True) then
      begin
        FProofreaderData.AddToReplacements(GetReplacementSource, TypedString, ReplaceWhere, ReplaceWithString);
        UpdateReplacementList;
      end;
    except
      on E: Exception do
        GxLogAndShowException(E);
    end;
  end;

  procedure InsertDict;
  var
    Entry: string;
  begin
    try
      if ExecuteDictionaryEntryForm(Entry, True) then
      begin
        FProofreaderData.AddToDictionary(GetReplacementSource, Entry);
        UpdateDictionaryList;
      end;
    except
      on E: Exception do
        GxLogAndShowException(E);
    end;
  end;

begin
  if ActiveTab = tabReplacement then
    InsertReplace
  else if ActiveTab = tabDictionary then
    InsertDict;
end;

procedure TfmProofreaderConfig.actListEditExecute(Sender: TObject);

  procedure EditReplace;
  var
    Item: TListItem;
    ItemIndex: Integer;
    OrigTypedString: string;
    NewTypedString, NewReplaceWithString: string;
    NewReplaceWhere: TGXWhereReplace;
    AReplacement: TReplacementItem;
  begin
    Item := lvReplacement.Selected;
    Assert(Assigned(Item));
    try
      ItemIndex := Item.Index;
      AReplacement := FProofreaderData.GetReplacementEntry(GetReplacementSource, ItemIndex);
      OrigTypedString := AReplacement.Typed;
      NewTypedString := OrigTypedString;
      NewReplaceWhere := AReplacement.Where;
      NewReplaceWithString := AReplacement.Replace;
      if TfmProofreaderAutoCorrectEntry.Execute(NewTypedString, NewReplaceWhere, NewReplaceWithString, False) then
      begin
        FProofreaderData.SetReplacementEntry(GetReplacementSource, ItemIndex,
          OrigTypedString, NewTypedString, NewReplaceWhere, NewReplaceWithString);
        UpdateReplacementList;
      end;
    except
      on E: Exception do
        GxLogAndShowException(E);
    end;
  end;

  procedure EditDict;
  var
    Item: TListItem;
    ItemIndex: Integer;
    OrigValue: string;
    NewValue: string;
  begin
    Item := lvDictionary.Selected;
    Assert(Assigned(Item));
    try
      ItemIndex := Item.Index;
      OrigValue := Item.Caption;
      NewValue := OrigValue;
      if ExecuteDictionaryEntryForm(NewValue, False) then
      begin
        FProofreaderData.SetDictionaryEntry(GetReplacementSource, ItemIndex, OrigValue, NewValue);
        UpdateDictionaryList;
      end;
    except
      on E: Exception do
        GxLogAndShowException(E);
    end;
  end;

begin
  if ActiveTab = tabReplacement then
    EditReplace
  else if ActiveTab = tabDictionary then
    EditDict;
end;

procedure TfmProofreaderConfig.actListDeleteExecute(Sender: TObject);

  procedure DeleteReplace;
  var
    i: Integer;
  begin
    for i := lvReplacement.Items.Count - 1 downto 0 do
      if lvReplacement.Items[i].Selected then
      begin
        lvReplacement.Items[i].Selected := False;
        FProofreaderData.DeleteReplacementEntry(GetReplacementSource, i, lvReplacement.Items[i].Caption);
      end;
    UpdateReplacementList;
  end;

  procedure DeleteDict;
  var
    i: Integer;
  begin
    for i := lvDictionary.Items.Count - 1 downto 0 do
      if lvDictionary.Items[i].Selected then
      begin
        lvDictionary.Items[i].Selected := False;
        FProofreaderData.DeleteDictionaryEntry(GetReplacementSource, i, lvDictionary.Items[i].Caption);
      end;
    UpdateDictionaryList;
  end;

begin
  if ActiveTab = tabReplacement then
    DeleteReplace
  else if ActiveTab = tabDictionary then
    DeleteDict;
end;

procedure TfmProofreaderConfig.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  SelCount: Integer;
begin
  if ActiveTab = tabReplacement then
    SelCount := lvReplacement.SelCount
  else if ActiveTab = tabDictionary then
    SelCount := lvDictionary.SelCount
  else if ActiveTab = tabHistory then
    SelCount := lvHistory.SelCount
  else
    raise Exception.Create('Unknown tab activated');

  actListDelete.Enabled := SelCount > 0;
  actListEdit.Enabled := SelCount = 1;
  actDisableRule.Enabled := SelCount = 1;
  actExportWords.Enabled := lvDictionary.Items.Count > 0;
end;

procedure TfmProofreaderConfig.actImportWordsExecute(Sender: TObject);
resourcestring
  LargeListMsg = 'Importing word lists with over a thousand entries is not recommended for performance reasons.  Continue anyway?';
var
  Words: TStringList;
  i: Integer;
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if not GetOpenSaveDialogExecute(dlgGetWordlist) then
      Exit;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;

  if FileExists(dlgGetWordlist.FileName) then
  begin
    Words := TStringList.Create;
    try
      Words.LoadFromFile(dlgGetWordlist.FileName);
      if Words.Count > 1000 then
        if not (MessageDlg(LargeListMsg, mtWarning, [mbYes, mbNo], 0) = mrYes) then
          Exit;
      for i := 0 to Words.Count - 1 do
      try
        FProofreaderData.AddToDictionary(GetReplacementSource, Trim(Words[i]), False);
      except
        on E: Exception do
          GxLogException(E);
      end;
    finally
      FProofreaderData.ResortDictionary(GetReplacementSource);
      UpdateDictionaryList;
      FreeAndNil(Words);
    end;
  end;
end;

procedure TfmProofreaderConfig.actExportWordsExecute(Sender: TObject);
var
  AFile: TextFile;
  CurrentIdeFolder: string;
  i: Integer;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if not GetOpenSaveDialogExecute(dlgPutWordlist) then
      Exit;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;

  AssignFile(AFile, dlgPutWordlist.FileName);
  Rewrite(AFile);
  try
    for i := 0 to FProofreaderData.GetDictionaryCount(GetReplacementSource) - 1 do
      WriteLn(AFile, FProofreaderData.GetDictionaryEntry(GetReplacementSource, i));
  finally
    CloseFile(AFile);
  end;
end;

function TfmProofreaderConfig.ActiveTab: TTabSheet;
begin
  Result := Pages.ActivePage;
end;

procedure TfmProofreaderConfig.UpdateDisplayedData;
begin
  UpdateHistoryGrid;
  UpdateReplacementList;
  UpdateDictionaryList;
  UpdateColumnWidths;
end;

procedure TfmProofreaderConfig.lvDictionaryData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := FProofreaderData.GetDictionaryEntry(GetReplacementSource, Item.Index);
end;

procedure TfmProofreaderConfig.lvReplacementData(Sender: TObject; Item: TListItem);
var
  AReplacement: TReplacementItem;
begin
  AReplacement := FProofreaderData.GetReplacementEntry(GetReplacementSource, Item.Index);
  Item.Caption := AReplacement.Typed;
  Item.SubItems.Add(GXWhereReplaceStrings[AReplacement.Where]);
  Item.SubItems.Add(AReplacement.Replace);
end;

procedure TfmProofreaderConfig.UpdateColumnWidths;
begin
  PostMessage(Self.Handle, UM_UPDATECOLS, 0, 0)
end;

procedure TfmProofreaderConfig.PagesChange(Sender: TObject);
begin
  UpdateColumnWidths;
end;

procedure TfmProofreaderConfig.UMUpdateCols(var Msg: TMessage);
begin
  // Hacks to get the listview columns sizing right on startup, when the
  // language changes, and when items are added/deleted
  lvReplacement.Width := lvReplacement.Width + 1;
  lvDictionary.Width := lvDictionary.Width + 1;
end;

end.

