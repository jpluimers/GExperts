unit GX_ComponentGrid;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts, Classes, Controls, Forms, Grids, SortGrid,
  StdCtrls, ComCtrls, ActnList, Actions, ToolWin, GX_BaseForm;

type
  TfmComponentGrid = class(TfmBaseForm)
    ToolBar: TToolBar;
    tbnSave: TToolButton;
    tbnSep1: TToolButton;
    tbnHelp: TToolButton;
    tbnSep2: TToolButton;
    tbnPrint: TToolButton;
    lblStart: TLabel;
    lblSkipBy: TLabel;
    tbnSep3: TToolButton;
    tbnSep4: TToolButton;
    Actions: TActionList;
    actFileSave: TAction;
    actHelpHelp: TAction;
    actFilePrint: TAction;
    tbnRenumber: TToolButton;
    actFileRenumber: TAction;
    edtStart: TEdit;
    udStart: TUpDown;
    edtSkipBy: TEdit;
    udSkipBy: TUpDown;
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure StringGridResize(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileRenumberExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    FComponentList: TInterfaceList;
    FModified: Boolean;
    StringGrid: TSortGrid;
    procedure FillComponentList;
    procedure ClearComponentList;
    procedure PopulateGrid;
    procedure PrintComponentGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGridExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Graphics, Printers, Windows, Dialogs, ToolsAPI,
  GX_Consts, GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_SharedImages,
  GX_dzVclUtils;

type
  TGridProperty = record
    PropertyName: string;
    ColWidth: Integer;
  end;

const // Do not localize.
  FixedColumns = 2;
  SHelpContextPropertyName = 'HelpContext';
  STagPropertyName = 'Tag';
  SHintPropertyName = 'Hint';

  GridProperties: array[0..2] of TGridProperty = (
    (
      PropertyName: STagPropertyName;
      ColWidth: 55;
    ),
    (
      PropertyName: SHelpContextPropertyName;
      ColWidth: 75;
    ),
    (
      PropertyName: SHintPropertyName;
      ColWidth: 150;
    ));

resourcestring
  SNotAvailable = 'N/A';

procedure TfmComponentGrid.StringGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  FModified := True;
end;

procedure TfmComponentGrid.FormClose(Sender: TObject; var Action: TCloseAction);
resourcestring
  SSaveChanges = 'Would you like to save your changes before closing?';
begin
  if FModified then
    if MessageDlg(SSaveChanges, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      actFileSave.Execute;
end;

procedure TfmComponentGrid.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol > 2) and (StringGrid.Cells[ACol, ARow] = SNotAvailable) then
    CanSelect := False;
end;

procedure TfmComponentGrid.PrintComponentGrid;
var
  F: TextFile;
  Page: Integer;

  procedure PrintPageHeader(const ReportName: string);
  begin
    WriteLn(F, 'Page #' + IntToStr(Page) + #9#9#9#9 + ReportName);
    WriteLn(F, '  ');
    WriteLn(F, 'Component.Name'#9#9'Parent'#9#9'Tag'#9#9'Help ID');
  end;

var
  i: Integer;
  ReportName: string;
begin
  if not InputQuery('Report Name', 'Enter the name for this report', ReportName) then
    Exit;

  Printer.Canvas.Font.Size := 10;
  Printer.Canvas.Font.Pitch := fpFixed;
  Printer.Title := 'Component Help Context Report';
  Printer.Orientation := poPortrait;

  Page := -1;

  AssignPrn(F);
  Rewrite(F);
  try
    for i := 1 to StringGrid.RowCount -1 do
    begin
      if Page <> Printer.PageNumber then
      begin
        Page := Printer.PageNumber;
        PrintPageHeader(ReportName);
      end;

      with StringGrid do
      begin
        WriteLn(F, GetPaddedString(Cells[0, i], 23) +
                   GetPaddedString(Cells[1, i], 15) +
                   GetPaddedString(Cells[2, i], 15) +
                   Cells[3, i]);
      end;
    end;
  finally
    System.CloseFile(F);
  end;
end;

{ TGridExpert }

procedure TGridExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaCurrentlyEditingForm;;
end;

procedure TGridExpert.Execute(Sender: TObject);
var
  Dlg: TfmComponentGrid;
begin
  Dlg := TfmComponentGrid.Create(nil);
  try
    SetFormIcon(Dlg);
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
  IncCallCount;
end;

constructor TGridExpert.Create;
begin
  inherited Create;
end;

function TGridExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Component &Grid...';
begin
  Result := SMenuCaption;
end;

class function TGridExpert.GetName: string;
begin
  Result := 'ComponentGrid';
end;

function TGridExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TfmComponentGrid.ClearComponentList;
begin
  // Is anything necessary here?
end;

procedure TfmComponentGrid.FillComponentList;

  procedure FillComponentListFromComponent(const AComponent: IOTAComponent);
  var
    i: Integer;
    RetrievedComponent: IOTAComponent;
  begin
    Assert(Assigned(AComponent));

    FComponentList.Add(AComponent);
    for i := 0 to AComponent.GetComponentCount-1 do
    begin
      RetrievedComponent := AComponent.GetComponent(i);
      FillComponentListFromComponent(RetrievedComponent);
    end;
  end;

var
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
begin
  if not IsForm(GxOtaGetCurrentSourceFile) then
  begin
    MessageDlg(SOnlyForFormFiles, mtError, [mbOK], 0);
    Exit;
  end;

  FormEditor := GxOtaGetCurrentFormEditor;
  Assert(Assigned(FormEditor));

  RootComponent := FormEditor.GetRootComponent;
  Assert(Assigned(RootComponent));

  FillComponentListFromComponent(RootComponent);
end;

procedure TfmComponentGrid.PopulateGrid;

  function ComponentName(const AComponent: IOTAComponent): string;
  begin
    Result := GxOtaGetComponentName(AComponent);
    if Result = '' then
      Result := SNotAvailable;
  end;

  function ParentName(const AComponent: IOTAComponent): WideString;
  begin
    Result := GxOtaGetComponentParentName(AComponent);
    if Result = '' then
      Result := SNotAvailable;
  end;

  function ComponentProperty(const PropName: string; const AComponent: IOTAComponent): WideString;
  begin
    if GxOtaPropertyExists(AComponent, PropName) then
      Result := GxOtaGetComponentPropertyAsString(AComponent, PropName)
    else
      Result := SNotAvailable;
  end;

resourcestring
  SCellComponent = 'Component';
var
  CurrentRow: Integer;
  AComponent: IOTAComponent;
  AComponentName: WideString;
  i, j: Integer;
begin
  Assert(Assigned(FComponentList));

  StringGrid.RowCount := 1;
  StringGrid.ColCount := FixedColumns + Length(GridProperties);
  for i := Low(GridProperties) to High(GridProperties) do
    StringGrid.ColWidths[i + FixedColumns] := GridProperties[i].ColWidth;

  StringGrid.Cells[0, 0] := SCellComponent;
  StringGrid.Cells[1, 0] := 'Parent';
  for i := Low(GridProperties) to High(GridProperties) do
    StringGrid.Cells[i + FixedColumns, 0] := GridProperties[i].PropertyName;

  for i := 0 to FComponentList.Count-1 do
  begin
    AComponent := FComponentList.Items[i] as IOTAComponent;
    Assert(Assigned(AComponent));

    AComponentName := ComponentName(AComponent);
    if Length(AComponentName) = 0 then
      Continue;

    CurrentRow := StringGrid.RowCount;
    StringGrid.RowCount := StringGrid.RowCount + 1;
    StringGrid.Cells[0, CurrentRow] := AComponentName;
    StringGrid.Cells[1, CurrentRow] := ParentName(AComponent);
    StringGrid.Objects[0, CurrentRow] := Pointer(i);
    for j := Low(GridProperties) to High(GridProperties) do
      StringGrid.Cells[FixedColumns + j, CurrentRow] := ComponentProperty(GridProperties[j].PropertyName, AComponent)
  end;

  StringGrid.FixedCols := FixedColumns;
  StringGrid.FixedRows := 1;
  StringGridResize(StringGrid);
end;

constructor TfmComponentGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TControl_SetMinConstraints(Self);

  FComponentList := TInterfaceList.Create;
  FModified := False;

  FillComponentList;
  PopulateGrid;
end;

destructor TfmComponentGrid.Destroy;
begin
  ClearComponentList;
  FreeAndNil(FComponentList);

  inherited Destroy;
end;

procedure TfmComponentGrid.StringGridResize(Sender: TObject);
const
  MinimumLastColWidth = 100;
var
  SendingGrid: TSortGrid;
  i: Integer;
  WidthDelta: Integer;
  LastColWidth: Integer;
begin
  SendingGrid := Sender as TSortGrid;
  Assert(Assigned(SendingGrid));

  WidthDelta := SendingGrid.ClientWidth;
  for i := 0 to SendingGrid.ColCount - 1 do
    Dec(WidthDelta, SendingGrid.ColWidths[i]);
  Dec(WidthDelta, SendingGrid.ColCount * SendingGrid.GridLineWidth);

  LastColWidth := SendingGrid.ColWidths[SendingGrid.ColCount-1];

  Inc(LastColWidth, WidthDelta);
  if LastColWidth < MinimumLastColWidth then
    LastColWidth := MinimumLastColWidth;

  SendingGrid.ColWidths[SendingGrid.ColCount-1] := LastColWidth;
end;

procedure TfmComponentGrid.actFileSaveExecute(Sender: TObject);
var
  Row, j: Integer;
  ComponentIndex: Integer;
  AComponent: IOTAComponent;
  Value: string;
begin
  Assert(Assigned(FComponentList));

  for Row := 1 to StringGrid.RowCount - 1 do
  begin
    ComponentIndex := GXNativeInt(StringGrid.Objects[0, Row]);

    AComponent := FComponentList.Items[ComponentIndex] as IOTAComponent;
    Assert(Assigned(AComponent));

    for j := Low(GridProperties) to High(GridProperties) do begin
      Value := StringGrid.Cells[FixedColumns + j, Row];
      if Value <> SNotAvailable then
        GxOtaSetComponentPropertyAsString(AComponent, GridProperties[j].PropertyName, Value);
    end;
  end;
  FModified := False;
  ModalResult := mrOk;
end;

procedure TfmComponentGrid.actFilePrintExecute(Sender: TObject);
begin
  PrintComponentGrid;
end;

procedure TfmComponentGrid.actFileRenumberExecute(Sender: TObject);
var
  i: Integer;
  StartAt: Longint;
  SkipBy: Longint;
begin
  StartAt := udStart.Position;
  SkipBy := udSkipBy.Position;
  for i := 1 to StringGrid.RowCount - 1 do
  begin
    if StringGrid.Cells[3, i] <> SNotAvailable then
    begin
      StringGrid.Cells[3, i] := IntToStr(StartAt);
      StartAt := StartAt + SkipBy;
    end;
  end;
end;

procedure TfmComponentGrid.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 22);
end;

procedure TfmComponentGrid.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfmComponentGrid.FormCreate(Sender: TObject);
begin
  SetToolbarGradient(ToolBar);
  StringGrid := TSortGrid.Create(Self);
  with StringGrid do
  begin
    Name := 'StringGrid';
    Parent := Self;
    Left := 0;
    Top := 22;
    Width := 449;
    Height := 218;
    Align := alClient;
    ColCount := FixedColumns + Length(GridProperties);
    DefaultRowHeight := 18;
    DefaultColWidth := 130;
    FixedCols := FixedColumns;
    RowCount := 2;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs, goColSizing];
    TabOrder := 0;
    OnSelectCell := StringGridSelectCell;
    OnSetEditText := StringGridSetEditText;
    CaseSensitive := False;
    AlignmentHorz := taLeftJustify;
    AlignmentVert := taTopJustify;
    ClickSorting := True;
    BevelStyle := cbNone;
    ProportionalScrollBars := True;
    ExtendedKeys := False;
    OnResize := StringGridResize;
  end;
end;

initialization
  RegisterGX_Expert(TGridExpert);
end.

