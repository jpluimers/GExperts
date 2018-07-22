unit GX_CompRenameConfig;

{$I GX_CondDefine.inc}

interface

uses
  Forms, StdCtrls, Classes, Controls, Messages, Grids, Menus, Dialogs, ActnList,
  ExtCtrls, GX_BaseForm;

const
  UM_SHOW_CONTROL = WM_USER + 133;

resourcestring
  SNotFound = 'not found: %s';

type
  TEditMode = (emRead, emEdit, emInsert);

  TOnRowHeaderClick = procedure(Sender: TObject; Col: Integer) of object;

  TRenameStringGrid = class(TStringGrid)
  private
    FComponents: TStringList;
    FOnRowHeaderClick: TOnRowHeaderClick;
  protected
    function GetEditStyle(ACol: Integer; ARow: Integer): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    procedure OnGetComponentList(ACol, ARow: Integer; Items: TStrings);
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure doRowHeaderClick(Col: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnRowHeaderClick: TOnRowHeaderClick read FOnRowHeaderClick write FOnRowHeaderClick;
  end;

  TfmCompRenameConfig = class(TfmBaseForm)
    chkShowDialog: TCheckBox;
    chkAutoAdd: TCheckBox;
    grpNames: TGroupBox;
    pnlRules: TPanel;
    pnlRight: TPanel;
    pnlIncSearch: TPanel;
    pnlNames: TPanel;
    edtFind: TEdit;
    btnClear: TButton;
    l_Find: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    ActionList: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    acFind: TAction;
    acCancel: TAction;
    pmGrid: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuDelete: TMenuItem;
    mnuSep1: TMenuItem;
    mnuFind: TMenuItem;
    acOK: TAction;
    mnuSep2: TMenuItem;
    acSortByClass: TAction;
    acSortByRule: TAction;
    mnuSort: TMenuItem;
    mnuSortByClass: TMenuItem;
    mnuSortByRule: TMenuItem;
    FindDialog: TFindDialog;
    btnDefaults: TButton;
    pnlFooter: TPanel;
    pnlTop: TPanel;
    btnOtherProperties: TButton;
    acOtherProperties: TAction;
    mnuOtherProperties: TMenuItem;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    procedure acOtherPropertiesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure acSortByClassExecute(Sender: TObject);
    procedure acSortByRuleExecute(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FindDialogFind(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtFindChange(Sender: TObject);
    procedure edtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FValueList: TStringList;
    Grid: TRenameStringGrid;
    procedure CopyValuesToGrid(Values: TStringList);
    procedure CopyGridToValues(var Values: TStringList);
    function IsEmptyRow(aGrid: TStringGrid; ARow: Integer): Boolean;
    function IsValidRule(ARow: Integer): Boolean;
    procedure GridDeleteRow(ARow: Integer);
    procedure GridAddRow;
    function RemoveEmptyBottomRow: Boolean;
    function RowHasComponent(aGrid: TStringGrid; ARow: Integer): Boolean;
    procedure HandleOnRowHeaderClick(Sender: TObject; Col: Integer);
    procedure SortByClass;
    procedure SortByRule;
    procedure GetData(_ValueList: TStringList; out _AutoShow, _AutoAdd: Boolean;
      out _FormWidth, _FormHeight: Integer);
    procedure SetData(_ValueList: TStringList; _AutoShow, _AutoAdd: Boolean;
      _FormWidth, _FormHeight: Integer; const _Selected: string);
  public
    class function Execute(_Owner: TComponent; _ValueList: TStringList; var _AutoShow: Boolean;
      var _AutoAdd: Boolean; var _FormWidth, _FormHeight: Integer; const _Selected: string): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Windows, SysUtils, Math, StrUtils,
  GX_GenericUtils, GX_OtaUtils, GX_SharedImages, GX_GxUtils, GX_CompRenameAdvanced,
  GX_MessageBox, GX_dzVclUtils, GX_dzClassUtils;

function CompareClassFunc(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  Assert(Assigned(List));
  S1 := List.Names[Index1];
  S2 := List.Names[Index2];
  Result := AnsiCompareStr(S1, S2);
end;

function CompareRuleFunc(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  Assert(Assigned(List));
  S1 := List.Values[List.Names[Index1]];
  S2 := List.Values[List.Names[Index2]];
  Result := AnsiCompareStr(S1, S2);
end;

{ TfmCompRenameConfig }

class function TfmCompRenameConfig.Execute(_Owner: TComponent; _ValueList: TStringList;
  var _AutoShow: Boolean; var _AutoAdd: Boolean; var _FormWidth, _FormHeight: Integer;
  const _Selected: string): Boolean;
var
  frm: TfmCompRenameConfig;
begin
  frm:= TfmCompRenameConfig.Create(_Owner);
  try
    frm.SetData(_ValueList, _Autoshow, _AutoAdd, _FormWidth, _FormHeight, _Selected);

    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ValueList, _Autoshow, _AutoAdd, _FormWidth, _FormHeight);
  finally
    FreeAndNil(frm);
  end;
end;

procedure CopyValueList(_Src: TStringList; _Dest: TStringList);
var
  i: Integer;
  Additional: TStringList;
  sl: TStringList;
begin
  Assert(Assigned(_Src));
  Assert(Assigned(_Dest));

  TStrings_FreeObjects(_Dest);
  _Dest.Clear;

  for i := 0 to _Src.Count - 1 do begin
    Additional := _Src.Objects[i] as TStringList;
    if Assigned(Additional) then begin
      sl := TStringList.Create;
      sl.Assign(Additional);
    end else
      sl := nil;
    _Dest.AddObject(_Src[i], sl);
  end;
end;

procedure TfmCompRenameConfig.SetData(_ValueList: TStringList; _AutoShow, _AutoAdd: Boolean;
      _FormWidth, _FormHeight: Integer; const _Selected: string);
begin
  Width := _FormWidth;
  Height := _FormHeight;
  chkShowDialog.Checked := _AutoShow;
  chkAutoAdd.Checked := _AutoAdd;
  CopyValueList(_ValueList, FValueList);
  CopyValuesToGrid(FValueList);
  Grid.Row := Grid.FixedRows; // Go to the top of the grid
  edtFind.Text := _Selected;
end;

procedure TfmCompRenameConfig.GetData(_ValueList: TStringList;
  out _AutoShow: Boolean; out _AutoAdd: Boolean; out _FormWidth, _FormHeight: Integer);
begin
  CopyGridToValues(FValueList);
  _FormWidth := Width;
  _FormHeight := Height;
  _AutoShow := chkShowDialog.Checked;
  _AutoAdd := chkAutoAdd.Checked;
  CopyValueList(FValueList, _ValueList);
end;

procedure TfmCompRenameConfig.FormCreate(Sender: TObject);
resourcestring
  ClassCaption = 'Class';
  RenameRuleCaption = 'Rename Rule';
const
  GridPad = 8;
begin
  TControl_SetMinConstraints(Self);

  pnlRules.BevelOuter := bvNone;
  pnlRight.BevelOuter := bvNone;
  pnlIncSearch.BevelOuter := bvNone;
  pnlNames.BevelOuter := bvNone;

  Grid := TRenameStringGrid.Create(Self);
  Grid.Name := 'Grid';
  Grid.Parent := pnlNames;
    // Delphi 6 does not support Margins, so we have to do it the hard way
  Grid.SetBounds(GridPad, GridPad, pnlNames.Width - (GridPad * 2), pnlNames.Height - (GridPad * 2));
  Grid.Anchors := [akLeft, akTop, akRight, akBottom];
  Grid.ColCount := 2;
  Grid.DefaultColWidth := 150;
  Grid.DefaultRowHeight := 17;
  Grid.FixedCols := 0;
  Grid.RowCount := 2;
  Grid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking];
  Grid.PopupMenu := pmGrid;
  Grid.ScrollBars := ssVertical;
  Grid.TabOrder := 0;
  Grid.OnRowHeaderClick := HandleOnRowHeaderClick;

  l_Find.Left := GridPad;
  btnClear.Left := pnlIncSearch.Width - btnClear.Width - GridPad;
  edtFind.Left := GridPad + l_Find.Width + 8;
  edtFind.Width := btnClear.Left - edtFind.Left;

  FValueList := TStringList.Create;
  Grid.Cells[0, 0] := ClassCaption;
  Grid.Cells[1, 0] := RenameRuleCaption;
end;

procedure TfmCompRenameConfig.FormDestroy(Sender: TObject);
begin
  TStrings_FreeWithObjects(FValueList);
end;

procedure TfmCompRenameConfig.HandleOnRowHeaderClick(Sender: TObject; Col: Integer);
begin
  if Col = 0 then
    SortByClass
  else
    SortByRule;
end;

procedure TfmCompRenameConfig.FormResize(Sender: TObject);
begin
  if Assigned(Grid) then // Or Delphi 2009 crashes here...
  begin
    Grid.ColWidths[0] := (Grid.ClientWidth div 2) - 1;
    Grid.ColWidths[1] := Grid.ColWidths[0];
  end;
end;

procedure TfmCompRenameConfig.CopyValuesToGrid(Values: TStringList);
var
  i: Integer;
  YOffset: Integer;
  SavedRow: Integer;
  s: string;
begin
  SavedRow := Grid.Row;
  YOffset := Grid.FixedRows;
  if Assigned(Values) then
  begin
    // Having only one row makes the fixed row paint wrong
    Grid.RowCount := Max(YOffset + Values.Count, 2);
    for i := 0 to Values.Count - 1 do
    begin
      s := Values.Names[i];
      Grid.Cells[0, YOffset + i] := s;
      Grid.Cells[1, YOffset + i] := Values.Values[s];
    end;
    if SavedRow < Grid.RowCount then
      Grid.Row := SavedRow;
  end;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.CopyGridToValues(var Values: TStringList);
var
  i: Integer;
  OrgValuelist: TStringList;
  Index: Integer;
begin
  if Assigned(Values) then
  begin
    // We need to keep the objects that are assigned to Values, so this is a bit more complicated
    OrgValueList := Values;
    OrgValuelist.Sorted := True;
    Values := TStringList.Create;
    for i := Grid.FixedRows to Grid.RowCount - 1 do
      if Trim(Grid.Cells[0, i]) <> '' then
        Values.Add(Grid.Cells[0, i] + '=' + Grid.Cells[1, i]);
    for i := 0 to Values.Count - 1 do
    begin
      Index := OrgValuelist.IndexOfName(Values.Names[i]);
      if Index <> -1 then
      begin
        Values.Objects[i] := OrgValuelist.Objects[Index];
        OrgValuelist.Objects[Index] := nil;
      end;
    end;
    for i := 0 to OrgValuelist.Count - 1 do
      OrgValuelist.Objects[i].Free;
    FreeAndNil(OrgValuelist);
  end;
end;

procedure TfmCompRenameConfig.edtFindChange(Sender: TObject);
var
  sl: TStringList;
  s: string;
  i: Integer;
begin
  s := edtFind.Text;
  if s = '' then
    Exit; //==>

  sl := TStringList.Create;
  try
    TStringGrid_GetCol(Grid, 0, sl);
    for i := 0 to sl.Count - 1 do begin
      if StartsText(s, sl[i]) then begin
        Grid.Row := Grid.FixedRows + i;
        Exit; //==>
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmCompRenameConfig.edtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN: begin
        Grid.SetFocus;
        Key := 0;
      end;
    VK_UP, VK_NEXT, VK_PRIOR: begin
        Grid.Perform(WM_KEYDOWN, Key, 0);
        Grid.SetFocus;
        Key := 0;
      end;
  end;
end;

procedure TfmCompRenameConfig.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  acAdd.Enabled := IsValidRule(Grid.Row) and not IsEmptyRow(Grid, Grid.RowCount - 1);
  acDelete.Enabled := ((Grid.Row >= Grid.FixedRows) and (Grid.Row < Grid.RowCount));
  acOtherProperties.Enabled := RowHasComponent(Grid, Grid.Row);
end;

procedure TfmCompRenameConfig.acAddExecute(Sender: TObject);
begin
  TryFocusControl(Grid);
  GridAddRow;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.acDeleteExecute(Sender: TObject);
begin
  TryFocusControl(Grid);
  GridDeleteRow(Grid.Row);
  if Grid.RowCount <= Grid.FixedRows then
    GridAddRow;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.acFindExecute(Sender: TObject);
var
  P: TPoint;
begin
  // Move the find dialog below the grid
  P := Point(pnlNames.Left, pnlNames.Top + pnlNames.Height);
  FindDialog.Position := pnlNames.ClientToScreen(P);
  FindDialog.Execute;
end;

procedure TfmCompRenameConfig.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmCompRenameConfig.acOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmCompRenameConfig.SortByClass;
begin
  CopyGridToValues(FValueList);
  FValueList.CustomSort(CompareClassFunc);
  CopyValuesToGrid(FValueList);
end;

procedure TfmCompRenameConfig.acSortByClassExecute(Sender: TObject);
begin
  SortByClass;
end;

procedure TfmCompRenameConfig.SortByRule;
begin
  CopyGridToValues(FValueList);
  FValueList.CustomSort(CompareRuleFunc);
  CopyValuesToGrid(FValueList);
end;

procedure TfmCompRenameConfig.acSortByRuleExecute(Sender: TObject);
begin
  SortByRule;
end;

procedure TfmCompRenameConfig.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DOWN:
        begin
          // If we are at bottom of grid, add a new empty row
          if (Grid.Row = Grid.RowCount - 1) and not IsEmptyRow(Grid, Grid.Row) then
            Grid.RowCount := Grid.RowCount + 1;
        end;

      VK_UP:
        begin
          // If we are at the bottom of grid, and this is an empty row, remove it
          if RemoveEmptyBottomRow then
            Key := 0;
        end;
    end; { case }
  end; { if Shift }
end; { GridKeyDown }

function TfmCompRenameConfig.IsEmptyRow(aGrid: TStringGrid; ARow: Integer): Boolean;
begin
  Result := Trim(aGrid.Rows[ARow].Text) = '';
end;

function TfmCompRenameConfig.RowHasComponent(aGrid: TStringGrid; ARow: Integer): Boolean;
begin
  Result := (Trim(Grid.Cells[0, ARow]) > '');
end;

function TfmCompRenameConfig.IsValidRule(ARow: Integer): Boolean;
begin
  Result := (Trim(Grid.Cells[0, ARow]) > '') and
            (Trim(Grid.Cells[1, ARow]) > '');
end;

function TfmCompRenameConfig.RemoveEmptyBottomRow: Boolean;
begin
  if IsEmptyRow(Grid, Grid.RowCount - 1) and (Grid.RowCount > Grid.FixedRows) then
  begin
    Grid.RowCount := Grid.RowCount - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TfmCompRenameConfig.GridAddRow;
begin
  Grid.RowCount := Grid.RowCount + 1;
  Application.ProcessMessages;
  Grid.Col := Grid.FixedCols;
  Grid.Row := Grid.RowCount - 1;
end;

procedure TfmCompRenameConfig.GridDeleteRow(ARow: Integer);
var
  i, j: Integer;
begin
  if ARow > Grid.FixedRows - 1 then
  begin
    if ARow < Grid.RowCount - 1 then
    begin
      // Move all cells one row up
      for i := ARow to Grid.RowCount - 2 do
        for j := 0 to Grid.ColCount - 1 do
          Grid.Cells[j, i] := Grid.Cells[j, i + 1];
    end;
    // Delete the last row
    Grid.Rows[Grid.RowCount - 1].Clear;
    if Grid.RowCount > Grid.FixedRows + 1 then
      Grid.RowCount := Grid.RowCount - 1;
  end;
end;

procedure TfmCompRenameConfig.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ARow < Grid.Row) and IsEmptyRow(Grid, Grid.Row) then
    RemoveEmptyBottomRow;
  if (ACol > 0) and (Grid.Cells[0, ARow]='') then
    CanSelect := False;
end;

procedure TfmCompRenameConfig.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CellCoord: TGridCoord;
begin
  CellCoord := Grid.MouseCoord(X, Y);
  if CellCoord.Y = 0 then
  begin
    case CellCoord.X of
      0: acSortByClass.Execute;
      1: acSortByRule.Execute;
    end;
  end;
end;

procedure TfmCompRenameConfig.FindDialogFind(Sender: TObject);
var
  Index: Integer;
  FoundRow: Integer;
  FoundCol: Integer;
  FindMsg: string;
begin
  Index := Grid.Row + 1;
  FoundRow := -1;
  FoundCol := 0;
  while (FoundRow < 0) and (Index < Grid.RowCount) do
  begin
    if CaseInsensitivePos(FindDialog.FindText, Grid.Cells[1, Index]) > 0 then
    begin
      FoundCol := 1;
      FoundRow := Index;
    end;

    if CaseInsensitivePos(FindDialog.FindText, Grid.Cells[0, Index]) > 0 then
    begin
      FoundCol := 0;
      FoundRow := Index;
    end;

    Inc(Index);
  end;
  if FoundRow >= 0 then
  begin
    Grid.Row := FoundRow;
    Grid.Col := FoundCol;
    Application.ProcessMessages;
    FindDialog.CloseDialog;
  end
  else begin
    FindMsg := Format(SNotFound, [FindDialog.FindText]);
    MessageDlg(FindMsg, mtInformation, [mbOK], 0);
  end;
end;

{ TDefaultRenameComponentsMessage }

type
  TDefaultRenameComponentsMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

function TDefaultRenameComponentsMessage.GetMessage: string;
resourcestring
  SClearIndividualShortcut =
    'This will reset this expert to the default settings. ' +
    'In particular it will replace existing rename rules with the defaults. ' +
    'Do you really want to do that?';
begin
  Result := SClearIndividualShortcut;
end;


procedure TfmCompRenameConfig.btnClearClick(Sender: TObject);
begin
  edtFind.Text := '';
  edtFind.SetFocus;
end;

procedure TfmCompRenameConfig.btnDefaultsClick(Sender: TObject);
begin
  if FValueList.Count > 0 then
    if ShowGxMessageBox(TDefaultRenameComponentsMessage) <> mrYes then
      Exit; //==>

  TStrings_FreeObjects(FValueList);
  FValueList.Clear;
  FValueList.Add('TAction=act');
  FValueList.Add('TBitBtn=btn');
  FValueList.Add('TButton=btn');
  FValueList.Add('TCheckBox=chk');
  FValueList.Add('TCheckListBox=lbx');
  FValueList.Add('TComboBox=cbx');
  FValueList.Add('TDrawGrid=grd');
  FValueList.Add('TEdit=edt');
  FValueList.Add('TGroupBox=gbx');
  FValueList.Add('TImage=img');
  FValueList.Add('TLabel=lbl');
  FValueList.Add('TListBox=lbx');
  FValueList.Add('TMaskEdit=edt');
  FValueList.Add('TMemo=mmo');
  FValueList.Add('TMenuItem=mnu');
  FValueList.Add('TPageControl=pag');
  FValueList.Add('TPanel=pnl');
  FValueList.Add('TRadioButton=rdo');
  FValueList.Add('TRadioGroup=rgp');
  FValueList.Add('TSpeedButton=btn');
  FValueList.Add('TStaticText=txt');
  FValueList.Add('TStringGrid=grd');
  FValueList.Add('TTabSheet=tab');

  CopyValuesToGrid(FValueList);
  chkShowDialog.Checked := False;
  chkAutoAdd.Checked := True;
end;

procedure TfmCompRenameConfig.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 42);
end;

procedure TfmCompRenameConfig.acOtherPropertiesExecute(Sender: TObject);
var
  CompType: WideString;
  Index: Integer;
  Additional: TStringList;
begin
  CompType := Grid.Cells[0, Grid.Row];

  Index := FValueList.IndexOfName(CompType);
  if Index = -1 then
    Index := FValueList.Add(Grid.Cells[0, Grid.Row] + '=' + Grid.Cells[1, Grid.Row]);
  if Index <> -1 then
    begin
      Additional := FValueList.Objects[Index] as TStringList;
      if TfmCompRenameAdvanced.Execute(Self, CompType, Additional) then
        FValueList.Objects[Index] := Additional;
    end;
end;

{ TRenameStringGrid }

constructor TRenameStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FComponents := TStringList.Create;
  GxOtaGetInstalledComponentList(FComponents, False);
end;

destructor TRenameStringGrid.Destroy;
begin
  FreeAndNil(FComponents);
  inherited;
end;

procedure TRenameStringGrid.doRowHeaderClick(Col: Integer);
begin
  if Assigned(FOnRowHeaderClick) then
    FOnRowHeaderClick(Self, Col);
end;

procedure TRenameStringGrid.WMLButtonUp(var msg: TWMLButtonUp);
var
  ACol, ARow: Integer;
begin
  inherited;
  MouseToCell(Msg.XPos, Msg.YPos, ACol, ARow);
  if ARow <= FixedRows-1 then
    doRowHeaderClick(ACol);
end;

function TRenameStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(Self);
  (Result as TInplaceEditList).OnGetPickListitems := OnGetComponentList;
  (Result as TInplaceEditList).DropDownRows := 15;
end;

function TRenameStringGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  if ACol = 0 then
    Result := esPickList
  else
    Result := esSimple;
end;

procedure TRenameStringGrid.OnGetComponentList(ACol, ARow: Integer; Items: TStrings);
begin
  Items.Assign(FComponents);
end;

end.

