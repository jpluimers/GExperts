{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  Buttons, ExtCtrls, StdCtrls,
  GX_BaseForm, GX_GrepBackend;

type
  TfmGrepSelect = class(TfmBaseForm)
    lvHistoryList: TListView;
    pnlTools: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnCheckItems: TButton;
    btnUnCheckItems: TButton;
    lblSaveOption: TLabel;
    cbxSaveOptionValue: TComboBox;
    pnlSaveOptionModify: TPanel;
    pnlDelete: TPanel;
    cbMoveToParamsPage: TCheckBox;
    btnUnsorted: TButton;
    pnlOpen: TPanel;
    cbOpenClear: TCheckBox;
    cbOpenOverwrite: TCheckBox;
    pnlSave: TPanel;
    cbSaveSplitToMoreFiles: TCheckBox;
    eSaveSplitCount: TEdit;
    lblSaveSplitCount: TLabel;
    cbOpenOnlyIfNewer: TCheckBox;
    btnSortKeyIndex: TButton;
    pnlSortButtons: TPanel;
    btnSortSearchTextAsc: TButton;
    btnSortSearchTextDesc: TButton;
    btnSortTimeAsc: TButton;
    btnSortTimeDesc: TButton;
    btnSortKeyIndexAsc: TButton;
    btnSortKeyIndexDesc: TButton;
    pnlSearch: TPanel;
    cbSearchInClearSearchList: TCheckBox;
    lblSearch: TLabel;
    eSearch: TEdit;
    tmrSearchInFilter: TTimer;
    cbSearchSaveOption: TComboBox;
    lblSearchSaveOption: TLabel;
    pnlSearchIn: TPanel;
    cbCheckedMoveToTop: TCheckBox;
    cbSearchInChecked: TCheckBox;
    cbShowNotFound: TCheckBox;
    btnClearSearch: TBitBtn;
    btnRefreshSearch: TBitBtn;
    rgMoveWhat: TRadioGroup;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    pnlSortMoves: TPanel;
    lblQuickSortButtons: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure lvHistoryListColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvHistoryListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
      var Compare: Integer);
    procedure SortButtonClick(Sender: TObject);
    procedure lvHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure cbSaveSplitToMoreFilesClick(Sender: TObject);
    procedure SortButtonsClick(Sender: TObject);
    procedure cbOpenClearClick(Sender: TObject);
    procedure cbOpenOverwriteClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure tmrSearchInFilterTimer(Sender: TObject);
    procedure cbSearchSaveOptionChange(Sender: TObject);
    procedure cbSearchInCheckedClick(Sender: TObject);
    procedure cbShowNotFoundClick(Sender: TObject);
    procedure cbCheckedMoveToTopClick(Sender: TObject);
    procedure lvHistoryListAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnRefreshSearchClick(Sender: TObject);
    procedure MoveButtonsClick(Sender: TObject);
  private
    FSortOrderList: TList;
    FHistoryList: TGrepHistoryList;
    FSelectType: TGrepSelectType;
    FViewSortMode: Integer;
    FViewSortDesc: Boolean;
    FSelectedCount: Integer;
    FSortMode: TGrepHistorySort;
    FSortDesc: Boolean;
    procedure SetAllSelectState(ABIOnlyAll, AState: Boolean; AOnlyHasResults: Boolean = False);
    procedure AddItemTo(AItem: TGrepHistoryListItem; AUseItemChecked: Boolean; AChecked, ADoSelected, ASelected: Boolean);
    procedure SearchInFillHistoryList(AClearAll: Boolean);
    procedure ClearSearchInList(AClearAll, AClearSelected: Boolean);
    procedure FillSortOrderList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(AHistoryList: TGrepHistoryList; ASelectType: TGrepSelectType; const ACaption: String);
    function  Execute: TGrepSelectResult;
    property SelectedCount: Integer read FSelectedCount;
    property SortMode: TGrepHistorySort read FSortMode;
    property SortDesc: Boolean read FSortDesc;
  end;

implementation

uses
  Math, DateUtils, StrUtils;

{$R *.dfm}

{ TfmGrepSelect }

constructor TfmGrepSelect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortOrderList := TList.Create;
end;

destructor TfmGrepSelect.Destroy;
begin
  FSortOrderList.Free;
  inherited Destroy;
end;

procedure TfmGrepSelect.FormCreate(Sender: TObject);
begin
  btnCheckItems.Tag := Integer(True);
  btnUnCheckItems.Tag := Integer(False);

  FViewSortMode := Integer(ghsUnsorted);
  FViewSortDesc := False;

  btnSortKeyIndex.Tag := Integer(ghsKeyIndex);
  btnUnsorted.Tag := Integer(ghsUnsorted);

  btnSortKeyIndexAsc.Tag := Integer(ghsKeyIndex);
  btnSortKeyIndexDesc.Tag := Integer(ghsKeyIndex);
  btnSortSearchTextAsc.Tag := Integer(ghsSearchText);
  btnSortSearchTextDesc.Tag := Integer(ghsSearchText);
  btnSortTimeAsc.Tag := Integer(ghsSearchTime);
  btnSortTimeDesc.Tag := Integer(ghsSearchTime);

  btnMoveUp.Tag := -1;
  btnMoveDown.Tag := 1;
end;

procedure TfmGrepSelect.AddItemTo(AItem: TGrepHistoryListItem; AUseItemChecked, AChecked, ADoSelected, ASelected: Boolean);
resourcestring
  rsProjGrep = 'Project';
  rsCurrentOnlyGrep = 'Current';
  rsOpenFilesGrep = 'Open';
  rsDirGrep = 'Directories';
  rsProjGroupGrep = 'Group';
  rsResults = 'Results';
const
  cGrepActionText: array[TGrepAction] of String =
    (rsProjGrep, rsCurrentOnlyGrep, rsOpenFilesGrep, rsDirGrep, rsProjGroupGrep, rsResults);
begin
  with lvHistoryList.Items.Add do
  begin
    Caption := AItem.SearchText;
    SubItems.Add(DateTimeToStr(AItem.LastSearchTime));
    SubItems.Add(IntToStr(AItem.FileCount));
    SubItems.Add(IntToStr(AItem.TotalMatchCount));
    SubItems.Add(cGrepActionText[AItem.GrepSettings.GrepAction]);
    SubItems.Add(SaveOptionText(AItem.GrepSettingsSaveOption));
    if AUseItemChecked then
      Checked := AItem.Checked
    else
      Checked := AChecked;
    if ADoSelected then
      OverlayIndex := Integer(ASelected);
    Data := Pointer(AItem);
  end;
end;

procedure TfmGrepSelect.Init(AHistoryList: TGrepHistoryList; ASelectType: TGrepSelectType; const ACaption: String);
resourcestring
  rsDeleteText = 'Delete';
  rsTextSave = 'Save';
  rsTextPrint = 'Print to File';
  rsTextSavePrint = 'Save && Print';
  rsTextRefresh = 'Refresh';
  rsTextSaveOptions = 'Modify';
  rsTextOpen = 'Open';
  rsTextSort = 'Set Order';
  rsTextSearchIn = 'Search In';
const
  cButtonText: array[TGrepSelectType] of String =
    (rsDeleteText, rsTextSave, rsTextPrint, rsTextSavePrint, rsTextRefresh, rsTextSaveOptions, rsTextOpen, rsTextSort, rsTextSearchIn);
begin
  Caption := ACaption;

  FHistoryList := AHistoryList;
  FSelectType := ASelectType;

  pnlDelete.Visible := False; // FSelectType = gstDeleteAll;
  pnlSave.Visible := FSelectType in [gstSave];
  pnlSaveOptionModify.Visible := FSelectType in [gstRefresh, gstSaveOptions, gstOpen];
  pnlOpen.Visible := FSelectType = gstOpen;
  pnlSortMoves.Visible := FSelectType = gstSort;
  pnlSortButtons.Visible := FSelectType = gstSort;
  pnlSearchIn.Visible := FSelectType = gstSearchInHistory;

  if FSelectType = gstSort then
    btnOK.ModalResult := mrYesToAll;

  btnOK.Caption := cButtonText[ASelectType];

  FHistoryList.ClearAllSortIndex;
  if FSelectType <> gstOpen then
    FHistoryList.ClearAllChecked;

  cbCheckedMoveToTop.Checked := FSelectType <> gstSort;
  cbShowNotFound.Checked := FSelectType <> gstSearchInHistory;
  cbSearchInChecked.Checked := FSelectType <> gstSearchInHistory;

  SearchInFillHistoryList(True);
  FillSortOrderList;

  if FSelectType in [gstSave, gstPrint, gstSavePrint, gstRefresh] then
    SetAllSelectState(True, True, FSelectType in [gstSave, gstPrint, gstSavePrint]);
end;

function TfmGrepSelect.Execute: TGrepSelectResult;
var
  I, ACount, AShowResult: Integer;
  ASelCount: array[Boolean] of Integer;
  AListItem: TListItem;
  AItem: TGrepHistoryListItem;
  ADoSetSaveOption: Boolean;
begin
  Result := gsrNoSelection;
  FSelectedCount := 0;
  FSortMode := ghsUnsorted;
  FSortDesc := False;

  AShowResult := ShowModal;
  case AShowResult of
    mrOk:
    begin
      ADoSetSaveOption := (FSelectType in [gstRefresh, gstSaveOptions, gstOpen]) and
        (cbxSaveOptionValue.ItemIndex <= Integer(High(TGrepSaveOption)));

      ASelCount[False] := 0;
      ASelCount[True] := 0;
      ACount := lvHistoryList.Items.Count;
      for I := 0 to ACount-1 do
      begin
        AListItem := lvHistoryList.Items[I];
        Inc(ASelCount[AListItem.Checked]);

        AItem := TGrepHistoryListItem(AListItem.Data);
        AItem.Checked := AListItem.Checked;
        if FSelectType = gstSearchInHistory then
          AItem.SortIndex := I;

        if ADoSetSaveOption and AListItem.Checked then
          AItem.GrepSettingsSaveOption := TGrepSaveOption(cbxSaveOptionValue.ItemIndex);
      end;

      FSelectedCount := ASelCount[True];

      if ASelCount[True] = ACount then
        Result := gsrSelectAll
      else if ASelCount[False] <> ACount then
        Result := gsrSelectItems;
    end;
    mrYes, mrNo:
    begin
      FSortDesc := AShowResult = mrNo;
      Result := gsrSelectAll;
    end;
    mrYesToAll:
    begin
      for I := 0 to lvHistoryList.Items.Count-1 do
      begin
        AListItem := lvHistoryList.Items[I];
        TGrepHistoryListItem(AListItem.Data).SortIndex := I;
      end;

      FSortMode := ghsSetSort;
      Result := gsrSelectItems;
    end;
  end;
end;

procedure TfmGrepSelect.SetAllSelectState(ABIOnlyAll, AState, AOnlyHasResults: Boolean);
var
  I, ACount: Integer;
  BI: Boolean;
  AItem: TGrepHistoryListItem;
  AListItem: TListItem;
begin
  for BI := ABIOnlyAll to True do
  begin
    ACount := 0;
    for I := 0 to lvHistoryList.Items.Count-1 do
    begin
      AListItem := lvHistoryList.Items[I];
      AItem := TGrepHistoryListItem(AListItem.Data);
      if BI or (AListItem.OverlayIndex = Integer(True)) then
      begin
        AListItem.Checked := AState and ( not AOnlyHasResults or (AItem.ResultList.Count > 0) );
        Inc(ACount);
      end;
    end;
    if ACount > 0 then
      Break;
  end;
end;

procedure TfmGrepSelect.btnSelectClick(Sender: TObject);
var
  AChecked: Boolean;
begin
  AChecked := Boolean((Sender as TComponent).Tag);
  SetAllSelectState(False, AChecked);

  SearchInFillHistoryList(False);
end;

procedure TfmGrepSelect.lvHistoryListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, AStartIndex, AEndIndex: Integer;
  AListItem: TListItem;
begin
  if (Button <> mbLeft) or (Shift <> [ssShift, ssLeft]) then
    Exit;

  if not Assigned(lvHistoryList.Selected) then
    Exit;

  AListItem := lvHistoryList.GetItemAt(X, Y);
  if not Assigned(AListItem) then
    Exit;

  AStartIndex := lvHistoryList.Selected.Index;
  AEndIndex := AListItem.Index;
  for I := Min(AStartIndex, AEndIndex) to Max(AStartIndex, AEndIndex) do
  begin
    AListItem := lvHistoryList.Items[I];
    AListItem.Checked := True;
  end;
end;

procedure TfmGrepSelect.lvHistoryListColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Tag < 0 then
    Exit;

  if FViewSortMode = Column.Tag then
    FViewSortDesc := not FViewSortDesc;
  FViewSortMode := Column.Tag;
  lvHistoryList.AlphaSort;
  FillSortOrderList;
end;

procedure TfmGrepSelect.lvHistoryListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
  var Compare: Integer);
var
  AItem1, AItem2: TGrepHistoryListItem;
  ASortMode, AIndex1, AIndex2: Integer;
  AColumnModified: Boolean;
begin
  AItem1 := TGrepHistoryListItem(Item1.Data);
  AItem2 := TGrepHistoryListItem(Item2.Data);
  ASortMode := FViewSortMode;
  AColumnModified := False;

  if cbCheckedMoveToTop.Checked then
  begin
    Compare := 0;
    if Item1.Checked and not Item2.Checked then
      Compare := -1
    else if not Item1.Checked and Item2.Checked then
      Compare := 1
    else
    begin
      if (Item1.OverlayIndex = Integer(True)) and (Item2.OverlayIndex = Integer(False)) then
        Compare := -1
      else if (Item1.OverlayIndex = Integer(False)) and (Item2.OverlayIndex = Integer(True)) then
        Compare := 1;
      if Item1.Checked then
        Compare := 0 - Compare;
    end;

    if Compare <> 0 then
      Exit;
  end;

  if ASortMode = 11 then //sort by save options
  begin
    Compare := CompareValue(Integer(AItem1.GrepSettingsSaveOption), Integer(AItem2.GrepSettingsSaveOption));
    if Compare = 0 then
    begin
      ASortMode := 22; // sort by file direct desc
      AColumnModified := True;
    end;
  end;

  if ASortMode = 12 then //sort by file count
  begin
    Compare := CompareValue(Integer(AItem1.FileCount), Integer(AItem2.FileCount));
    if Compare = 0 then
    begin
      ASortMode := Integer(ghsSearchText);
      AColumnModified := True;
    end;
  end
  else if ASortMode = 22 then //sort by file count, null to end
  begin
    if AItem1.FileCount = 0 then
      Compare := 1
    else if AItem2.FileCount = 0 then
      Compare := -1
    else
      Compare := CompareValue(Integer(AItem1.FileCount), Integer(AItem2.FileCount));
    if Compare = 0 then
    begin
      ASortMode := Integer(ghsSearchText);
      AColumnModified := True;
    end;
  end
  else if ASortMode = 13 then //sort by matches
  begin
    Compare := CompareValue(Integer(AItem1.TotalMatchCount), Integer(AItem2.TotalMatchCount));
    if Compare = 0 then
    begin
      ASortMode := Integer(ghsSearchText);
      AColumnModified := True;
    end;
  end
  else if ASortMode = 14 then //sort by grepaction
  begin
    Compare := CompareValue(Integer(AItem1.GrepSettings.GrepAction), Integer(AItem2.GrepSettings.GrepAction));
    if Compare = 0 then
    begin
      ASortMode := Integer(ghsSearchText);
      AColumnModified := True;
    end;
  end
  else if ASortMode = Integer(ghsKeyIndex) then
    Compare := CompareValue(AItem1.KeyIndex, AItem2.KeyIndex);

  if ASortMode = -1 then  //sort by FSortOrderList / by Move buttons
  begin
    AIndex1 := FSortOrderList.IndexOf(Item1);
    AIndex2 := FSortOrderList.IndexOf(Item2);
    Compare := CompareValue(AIndex1, AIndex2);
  end
  else if ASortMode = Integer(ghsSearchText) then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else if ASortMode = Integer(ghsSearchTime) then
    Compare := CompareDateTime(AItem1.LastSearchTime, AItem2.LastSearchTime)
  else if ASortMode = Integer(ghsUnsorted) then
  begin
    AIndex1 := FHistoryList.ItemIndexByKeyIndex(AItem1.KeyIndex);
    AIndex2 := FHistoryList.ItemIndexByKeyIndex(AItem2.KeyIndex);
    Compare := CompareValue(AIndex1, AIndex2);
  end;

  if FViewSortDesc and not AColumnModified then
    Compare := 0 - Compare;
end;

procedure TfmGrepSelect.SortButtonClick(Sender: TObject);
var
  ButtonMode: Integer;
begin
  ButtonMode := (Sender as TComponent).Tag;
  if ButtonMode < 0 then
    Exit;

  FViewSortDesc := (FViewSortMode = ButtonMode) and not FViewSortDesc;
  FViewSortMode := ButtonMode;
  lvHistoryList.AlphaSort;
  FillSortOrderList;
end;

procedure TfmGrepSelect.cbSaveSplitToMoreFilesClick(Sender: TObject);
begin
  lblSaveSplitCount.Enabled := cbSaveSplitToMoreFiles.Checked;
  eSaveSplitCount.Enabled := cbSaveSplitToMoreFiles.Checked;
end;

procedure TfmGrepSelect.SortButtonsClick(Sender: TObject);
begin
  FSortMode := TGrepHistorySort((Sender as TComponent).Tag);
end;

procedure TfmGrepSelect.cbOpenClearClick(Sender: TObject);
begin
  if cbOpenClear.Checked then
  begin
    cbOpenOverwrite.Checked := False;
    cbOpenOnlyIfNewer.Enabled := False;
  end;
end;

procedure TfmGrepSelect.cbOpenOverwriteClick(Sender: TObject);
begin
  if cbOpenOverwrite.Checked then
    cbOpenClear.Checked := False;

  cbOpenOnlyIfNewer.Enabled := cbOpenOverwrite.Checked;
end;

procedure TfmGrepSelect.eSearchChange(Sender: TObject);
begin
  tmrSearchInFilter.Enabled := False;
  tmrSearchInFilter.Enabled := True; //FI:W508 - stop and restart the timer on key press
end;

procedure TfmGrepSelect.tmrSearchInFilterTimer(Sender: TObject);
begin
  SearchInFillHistoryList(False);
  tmrSearchInFilter.Enabled := False;
end;

procedure TfmGrepSelect.ClearSearchInList(AClearAll, AClearSelected: Boolean);
var
  I: Integer;
  AListItem: TListItem;
begin
  if AClearAll then
    lvHistoryList.Clear
  else
  begin
    lvHistoryList.Items.BeginUpdate;
    try
      for I := lvHistoryList.Items.Count-1 downto 0 do
      begin
        AListItem := lvHistoryList.Items[I];
        if not AListItem.Checked then
        begin
          if AClearSelected then
            AListItem.OverlayIndex := Integer(False)
          else
            AListItem.Delete;
        end
        else
          AListItem.OverlayIndex := Integer(False);
      end;
    finally
      lvHistoryList.Items.EndUpdate;
    end;
  end;
end;

procedure TfmGrepSelect.FillSortOrderList;
var
  I: Integer;
begin
  if FSelectType <> gstSort then
    Exit;

  FSortOrderList.Clear;
  for I := 0 to lvHistoryList.Items.Count-1 do
    FSortOrderList.Add(lvHistoryList.Items[I]);
  FViewSortMode := -1;
end;

procedure TfmGrepSelect.SearchInFillHistoryList(AClearAll: Boolean);

  function lvHistoryListIndexOf(AItem: TGrepHistoryListItem): Integer;
  begin
    for Result := 0 to lvHistoryList.Items.Count-1 do
      if lvHistoryList.Items[Result].Caption = AItem.SearchText then
        Exit;
    Result := -1;
  end;

var
  I, LI, ACount: Integer;
  AItem: TGrepHistoryListItem;
  AListItem: TListItem;
  AnySearch, AnySearchText, AnySaveOption, IsMatchText, IsMatchSaveOption, ShowNotFounded: Boolean;
begin
  if FSelectType = gstSearchInHistory then
    ACount := FHistoryList.HistoryList.Count
  else
    ACount := FHistoryList.Count;

  ShowNotFounded := cbShowNotFound.Checked;

  lvHistoryList.Items.BeginUpdate;
  try
    ClearSearchInList(AClearAll, ShowNotFounded);

    for I := 0 to ACount-1 do
    begin
      if FSelectType = gstSearchInHistory then
        AItem := FHistoryList.HistoryList.Items[I]
      else
        AItem := FHistoryList.Items[I];

      AnySearchText := Trim(eSearch.Text) <> '';
      AnySaveOption := cbSearchSaveOption.ItemIndex <= Integer(High(TGrepSaveOption));

      IsMatchText := not AnySearchText or ( AnsiContainsText(AItem.SearchText, eSearch.Text) );
      IsMatchSaveOption := not AnySaveOption or
        ( AItem.GrepSettingsSaveOption = TGrepSaveOption(cbSearchSaveOption.ItemIndex) );
      AnySearch :=
        (AnySearchText and not AnySaveOption and IsMatchText) or
        (AnySaveOption and not AnySearchText and IsMatchSaveOption) or
        (AnySearchText and IsMatchText and AnySaveOption and IsMatchSaveOption);

      if ShowNotFounded or (IsMatchText and IsMatchSaveOption) then
      begin
        LI := lvHistoryListIndexOf(AItem);
        if LI = -1 then
          AddItemTo(AItem, FSelectType <> gstSearchInHistory, False, AnySearch, True)
        else
        begin
          AListItem := lvHistoryList.Items[LI];
          if not AListItem.Checked or cbSearchInChecked.Checked then
            AListItem.OverlayIndex := Integer(AnySearch);
        end ;
      end ;
    end;
    lvHistoryList.AlphaSort;
  finally
    lvHistoryList.Items.EndUpdate;
  end;
end;

procedure TfmGrepSelect.cbSearchSaveOptionChange(Sender: TObject);
begin
  SearchInFillHistoryList(False);
end;

procedure TfmGrepSelect.cbSearchInCheckedClick(Sender: TObject);
begin
  SearchInFillHistoryList(False);
end;

procedure TfmGrepSelect.cbShowNotFoundClick(Sender: TObject);
begin
  lvHistoryList.Items.BeginUpdate;
  try
    SearchInFillHistoryList(False);
    SearchInFillHistoryList(False);
  finally
    lvHistoryList.Items.EndUpdate;
  end;
end;

procedure TfmGrepSelect.cbCheckedMoveToTopClick(Sender: TObject);
begin
  lvHistoryList.AlphaSort;
  FillSortOrderList;
end;

procedure TfmGrepSelect.lvHistoryListAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Item.OverlayIndex = Integer(True) then
    lvHistoryList.Canvas.Brush.Color := clSkyBlue;
end;

procedure TfmGrepSelect.btnClearSearchClick(Sender: TObject);
begin
  eSearch.Text := '';
  cbSearchSaveOption.ItemIndex := 3;
  SearchInFillHistoryList(False);
end;

procedure TfmGrepSelect.btnRefreshSearchClick(Sender: TObject);
begin
  SearchInFillHistoryList(False);
end;

procedure TfmGrepSelect.MoveButtonsClick(Sender: TObject);
var
  AList: TList;

  function FillList(AWhat: Integer; ADoReverse: Boolean): Boolean;
  var
    I: Integer;
    AListItem: TListItem;
  begin
    for I := 0 to lvHistoryList.Items.Count-1 do
    begin
      AListItem := lvHistoryList.Items[I];
      if ((AWhat = 0) and AListItem.Checked) or
        ((AWhat = 1) and not AListItem.Checked) or
        ((AWhat = 2) and (AListItem.OverlayIndex = Integer(True)))
      then
        if ADoReverse then
          AList.Insert(0, AListItem)
        else
          AList.Add(AListItem);
    end;
    Result := AList.Count > 0;
  end;

  procedure MoveItem(AListItem: TListItem; ADelta: Integer);
  var
    ACurrIndex, ANewIndex: Integer;
  begin
    ACurrIndex := FSortOrderList.IndexOf(AListItem);
    ANewIndex := ACurrIndex + ADelta;
    if (ANewIndex >= 0) and (ANewIndex < FSortOrderList.Count) and
      (ACurrIndex >= 0) and (ACurrIndex < FSortOrderList.Count)
    then
      FSortOrderList.Move(ACurrIndex, ANewIndex);
  end;

var
  AMoveDelta, I: Integer;
  AListItem, ASelItem: TListItem;
begin
  AMoveDelta := (Sender as TComponent).Tag;

  lvHistoryList.Items.BeginUpdate;
  try
    ASelItem := lvHistoryList.Selected;
    AList := TList.Create;
    try
      if FillList(rgMoveWhat.ItemIndex, AMoveDelta > 0) then
      begin
        for I := 0 to AList.Count-1 do
        begin
          AListItem := TListItem(AList[I]);
          MoveItem(AListItem, AMoveDelta);
        end ;
      end;
    finally
      AList.Free;
    end;
    FViewSortMode := -1;
    lvHistoryList.AlphaSort;

    if Assigned(ASelItem) then
      ASelItem.Selected;
  finally
    lvHistoryList.Items.EndUpdate;
  end;
end;

end.
