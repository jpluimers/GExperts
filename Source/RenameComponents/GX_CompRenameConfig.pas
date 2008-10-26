unit GX_CompRenameConfig;

{$I GX_CondDefine.inc}

interface

uses
  Forms, StdCtrls, Classes, Controls, Messages, Grids, Menus, Dialogs, ActnList,
  ExtCtrls;

const
  UM_SHOW_CONTROL = WM_USER + 133;

resourcestring
  SNotFound = 'not found: %s';

type
  TEditMode = (emRead, emEdit, emInsert);

  TRenameStringGrid = class(TStringGrid)
  private
    FComponents: TStringList;
  protected
    function GetEditStyle(ACol: Integer; ARow: Integer): TEditStyle; override;
    function CreateEditor: TInplaceEdit; override;
    procedure OnGetComponentList(ACol, ARow: Integer; Items: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TfmCompRenameConfig = class(TForm)
    btnOK: TButton;
    btnClose: TButton;
    chkShowDialog: TCheckBox;
    chkAutoAdd: TCheckBox;
    pnlNames: TGroupBox;
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
    btnHelp: TButton;
    pnlFooter: TPanel;
    pnlRules: TPanel;
    btnOtherProperties: TButton;
    acOtherProperties: TAction;
    mnuOtherProperties: TMenuItem;
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
  private
    FValueList: TStringList;
    Grid: TRenameStringGrid;
    procedure CopyValuesToGrid;
    procedure CopyGridToValues;
    function IsEmptyRow(aGrid: TStringGrid; ARow: Integer): Boolean;
    function IsValidRule(ARow: Integer): Boolean;
    procedure GridDeleteRow(ARow: Integer);
    procedure GridAddRow;
    function RemoveEmptyBottomRow: Boolean;
    function RowHasComponent(aGrid: TStringGrid; ARow: Integer): Boolean;
  public
    property ValueList: TStringList read FValueList;
    function Execute: Boolean;
  end;

implementation

{$R *.dfm}

uses
  Windows, SysUtils,
  GX_GenericUtils, GX_OtaUtils, GX_SharedImages, GX_GxUtils, GX_CompRenameAdvanced;

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

function TfmCompRenameConfig.Execute: Boolean;
begin
  CopyValuesToGrid;
  Grid.Row := Grid.FixedRows; // Go to the bottom of grid
  Result := (ShowModal = mrOk);
  if Result then
    CopyGridToValues;
end;

procedure TfmCompRenameConfig.FormCreate(Sender: TObject);
resourcestring
  ClassCaption = 'Class';
  RenameRuleCaption = 'Rename Rule';
const
  GridPad = 8;
begin
  Grid := TRenameStringGrid.Create(Self);
  with Grid do
  begin
    Name := 'Grid';
    Parent := pnlNames;
    SetBounds(GridPad, GridPad * 2, pnlNames.Width - (GridPad*2) - (pnlNames.Width - btnAdd.Left), pnlNames.Height - (GridPad * 3));
    Anchors := [akLeft, akTop, akRight, akBottom];
    ColCount := 2;
    DefaultColWidth := 150;
    DefaultRowHeight := 17;
    FixedCols := 0;
    RowCount := 2;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking];
    PopupMenu := pmGrid;
    ScrollBars := ssVertical;
    TabOrder := 0;
  end;

  FValueList := TStringList.Create;
  Grid.Cells[0, 0] := ClassCaption;
  Grid.Cells[1, 0] := RenameRuleCaption;
end;

procedure TfmCompRenameConfig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FValueList);
end;

procedure TfmCompRenameConfig.FormResize(Sender: TObject);
begin
  if Assigned(Grid) then // Or Delphi 2009 crashes here...
  begin
    Grid.ColWidths[0] := (Grid.ClientWidth div 2) - 1;
    Grid.ColWidths[1] := Grid.ColWidths[0];
  end;
end;

procedure TfmCompRenameConfig.CopyValuesToGrid;
var
  i, YOffset, SavedRow: Integer;
begin
  SavedRow := Grid.Row;
  YOffset := Grid.FixedRows;
  if Assigned(FValueList) then
  begin
    // Having only one row makes the fixed row paint wrong
    Grid.RowCount := Max(YOffset + FValueList.Count, 2);
    for i := 0 to FValueList.Count - 1 do
    begin
      Grid.Cells[0, YOffset + i] := FValueList.Names[i];
      Grid.Cells[1, YOffset + i] := FValueList.Values[FValueList.Names[i]];
    end;
    if SavedRow < Grid.RowCount then
      Grid.Row := SavedRow;
  end;
end;

procedure TfmCompRenameConfig.CopyGridToValues;
var
  i: Integer;
  OrgValuelist: TStringList;
  Index: Integer;
begin
  if Assigned(FValueList) then
  begin
    OrgValueList := FValueList;
    OrgValuelist.Sorted := True;
    FValueList := TStringList.Create;
    for i := Grid.FixedRows to Grid.RowCount - 1 do
      if Trim(Grid.Cells[0, i]) <> '' then
        FValueList.Add(Grid.Cells[0, i] + '=' + Grid.Cells[1, i]);
    for i := 0 to FValueList.Count - 1 do
    begin
      Index := OrgValuelist.IndexOfName(FValueList.Names[i]);
      if Index <> -1 then
      begin
        FValueList.Objects[i] := OrgValuelist.Objects[Index];
        OrgValuelist.Objects[Index] := nil;
      end;
    end;
    for i := 0 to OrgValuelist.Count - 1 do
      OrgValuelist.Objects[i].Free;
    FreeAndNil(OrgValuelist);
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
  if Grid.CanFocus then
    Grid.SetFocus;
  GridAddRow;
  FormResize(Self);
end;

procedure TfmCompRenameConfig.acDeleteExecute(Sender: TObject);
begin
  if Grid.CanFocus then
    Grid.SetFocus;
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

procedure TfmCompRenameConfig.acSortByClassExecute(Sender: TObject);
begin
  CopyGridToValues;
  FValueList.CustomSort(CompareClassFunc);
  CopyValuesToGrid;
end;

procedure TfmCompRenameConfig.acSortByRuleExecute(Sender: TObject);
begin
  CopyGridToValues;
  FValueList.CustomSort(CompareRuleFunc);
  CopyValuesToGrid;
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

procedure TfmCompRenameConfig.btnDefaultsClick(Sender: TObject);
begin
  with FValueList do begin
    Clear;
    Values['TAction']      := 'act';
    Values['TBitBtn']      := 'btn';
    Values['TButton']      := 'btn';
    Values['TCheckBox']    := 'chk';
    Values['TCheckListBox']:= 'lbx';
    Values['TComboBox']    := 'cbx';
    Values['TDrawGrid']    := 'grd';
    Values['TEdit']        := 'edt';
    Values['TGroupBox']    := 'gbx';
    Values['TImage']       := 'img';
    Values['TLabel']       := 'lbl';
    Values['TListBox']     := 'lbx';
    Values['TMaskEdit']    := 'edt';
    Values['TMemo']        := 'mmo';
    Values['TMenuItem']    := 'mnu';
    Values['TPageControl'] := 'pag';
    Values['TPanel']       := 'pnl';
    Values['TRadioButton'] := 'rdo';
    Values['TRadioGroup']  := 'rgp';
    Values['TSpeedButton'] := 'btn';
    Values['TStaticText']  := 'txt';
    Values['TStringGrid']  := 'grd';
    Values['TTabSheet']    := 'tab';
  end;
  CopyValuesToGrid;
  chkShowDialog.Checked := False;
  chkAutoAdd.Checked := True;
end;

procedure TfmCompRenameConfig.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 42);
end;

procedure TfmCompRenameConfig.acOtherPropertiesExecute(Sender: TObject);

  function RemoveSpaces(const Value: string): string;
  begin
    Result := Value;
    Result := StringReplace(Result, ' =', '=', []);
    Result := StringReplace(Result, '= ', '=', []);
  end;

var
  CompType: WideString;
  Dlg: TfmCompRenameAdvanced;
  Index: Integer;
  Additional: TStringList;
  i: Integer;
begin
  CompType := Grid.Cells[0, Grid.Row];
  Dlg := TfmCompRenameAdvanced.Create(Self);
  try
    Dlg.lblComponentClass.Caption := CompType;
    Index := FValueList.IndexOfName(CompType);
    if Index = -1 then
      Index := FValueList.Add(Grid.Cells[0, Grid.Row] + '=' + Grid.Cells[1, Grid.Row]);
    if Index <> -1 then
    begin
      Additional := FValueList.Objects[Index] as TStringList;
      if Assigned(Additional) then
        Dlg.mmoPropertyNames.Lines.Assign(Additional);
      if Dlg.ShowModal = mrOK then
      begin
        if Dlg.mmoPropertyNames.Lines.Count > 0 then
        begin
          if not Assigned(Additional) then
            Additional := TStringList.Create;
          Additional.Assign(Dlg.mmoPropertyNames.Lines);
          for i := 0 to Additional.Count - 1 do
            Additional[i] := RemoveSpaces(Additional[i]);

          FValueList.Objects[Index] := Additional;
        end
        else if Assigned(Additional) then
        begin
          FValueList.Objects[Index] := nil;
          FreeAndNil(Additional);
        end;
      end;
    end;
  finally
    FreeAndNil(Dlg);
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

