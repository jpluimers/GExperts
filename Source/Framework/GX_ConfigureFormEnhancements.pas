unit GX_ConfigureFormEnhancements;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Forms,
  Controls,
  Dialogs,
  ImgList,
  Grids;

type
  TOnGetCellHint = procedure(_Sender: TObject; _Col, _Row: Integer; var _Hint: string) of object;
  TStringGrid = class(Grids.TStringGrid)
  private
    FImageList: TImageList;
    FOnGetCellHint: TOnGetCellHint;
    procedure SwitchCell(_Col, _Row: Integer);
    procedure CMHintShow(var _Msg: TCMHintShow); message CM_HINTSHOW;
    function doGetCellHint(_Col, _Row: Integer): string;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure Loaded; override;
  public
    property OnGetCellHint: TOnGetCellHint read FOnGetCellHint write FOnGetCellHint;
  end;

type
  TfrConfigureFormEnhancements = class(TFrame)
    sg_IdeEnhancements: TStringGrid;
    TheImageList: TImageList;
    procedure FrameResize(Sender: TObject);
  private
    procedure sg_IdeEnhancementsGetCellHint(_Sender: TObject; _Col, _Row: Integer;
      var _Hint: string);
  public
    constructor Create(_Owner: TComponent); override;
    procedure ApplyGrid;
    procedure InitGrid;
  end;

implementation

uses
  GX_IdeManagedFormHandler,
  GX_dzVclUtils;

{$R *.dfm}

{ TStringGrid }

procedure TStringGrid.CMHintShow(var _Msg: TCMHintShow);
var
  hi: PHintInfo;
  pnt: TPoint;
  c: Integer;
  r: Integer;
  s: string;
begin
  hi := _Msg.HintInfo;
  pnt := ScreenToClient(hi.HintPos);
  MouseToCell(pnt.X, pnt.Y, c, r);
  if (r = -1) or (c = -1) then
    Exit;
  s := doGetCellHint(c, r);
  if s = '' then
    Exit;
  hi.CursorRect := CellRect(c, r);
  hi.HintStr := s;
end;

function TStringGrid.doGetCellHint(_Col, _Row: Integer): string;
begin
  Result := '';
  if Assigned(FOnGetCellHint) then
    FOnGetCellHint(Self, _Col, _Row, Result);
end;

procedure TStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  s: string;
  bmp: TBitmap;
  xOff: Integer;
  YOff: Integer;
begin
  inherited;
  if not Assigned(FImageList) or (ARow = 0) then
    Exit;

  s := Cells[ACol, ARow];
  if (Length(s) <> 3) or (s[1] <> '_') or (s[3] <> '_') then
    Exit;

  Canvas.FillRect(ARect);
  bmp := TBitmap.Create;
  try
    if (s = '_1_') then
      FImageList.GetBitmap(1, bmp)
    else if s = '_0_' then
      FImageList.GetBitmap(0, bmp)
    else
      bmp.FreeImage;
    xOff := ARect.Left + ((ARect.Right - ARect.Left) - bmp.Width) div 2;
    YOff := ARect.Top + ((ARect.Bottom - ARect.Top) - bmp.Height) div 2;
    Canvas.Draw(xOff, YOff, bmp);
  finally
    FreeAndNil(bmp);
  end;
end;

procedure TStringGrid.KeyPress(var Key: Char);
begin
  inherited;
  if Key <> ' ' then
    Exit;
  SwitchCell(Col, Row);
end;

procedure TStringGrid.Loaded;
var
  cmp: TComponent;
begin
  inherited;
  cmp := Owner.FindComponent('TheImageList');
  if Assigned(cmp) and (cmp is TImageList) then
    FImageList := TImageList(cmp);
end;

procedure TStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c: Integer;
  Row: Integer;
begin
  inherited;
  MouseToCell(X, Y, c, Row);
  SwitchCell(c, Row);
end;

procedure TStringGrid.SwitchCell(_Col, _Row: Integer);
var
  s: string;
begin
  if (_Row = 0) then
    Exit;

  s := Cells[_Col, _Row];
  if s = '_1_' then
    s := '_0_'
  else if s = '_0_' then
    s := '_1_'
  else
    Exit;
  Cells[_Col, _Row] := s;
end;

const
  sfcOffset = 1;

{ TTfrConfigureFormEnhancements }

constructor TfrConfigureFormEnhancements.Create(_Owner: TComponent);
begin
  inherited;
  sg_IdeEnhancements.OnGetCellHint := sg_IdeEnhancementsGetCellHint;
  TControl_SetMinConstraints(Self);
end;

procedure TfrConfigureFormEnhancements.FrameResize(Sender: TObject);
begin
  TGrid_Resize(sg_IdeEnhancements, [roUseGridWidth, roReduceMinWidth, roUseAllRows]);
end;

procedure TfrConfigureFormEnhancements.sg_IdeEnhancementsGetCellHint(_Sender: TObject;
  _Col, _Row: Integer; var _Hint: string);
var
  Handler: TManagedFormHandler;
begin
  if (_Col < sfcOffset) or (_Row <= sg_IdeEnhancements.FixedRows) then
    Exit;

  Dec(_Row, sg_IdeEnhancements.FixedRows);
  Handler := sg_IdeEnhancements.Objects[0, _Row] as TManagedFormHandler;
  Dec(_Col, sfcOffset);
  if _Col = Ord(sfcFixForm) then
    _Hint := Handler.FormFixes
  else if _Col = Ord(sfcEnhanceForm) then
    _Hint := Handler.FormEnhancements;
end;

procedure TfrConfigureFormEnhancements.InitGrid;

  procedure SetRow(_Row: Integer; const _Columns: array of string);
  var
    i: Integer;
    Len: Integer;
  begin
    Len := Length(_Columns);
    if sg_IdeEnhancements.ColCount < Len then
      sg_IdeEnhancements.ColCount := Len;
    for i := 0 to Len - 1 do begin
      sg_IdeEnhancements.Cells[i, _Row] := _Columns[i];
    end;
  end;

  function AppendRow(const _Handler: TManagedFormHandler): Integer;
  begin
    Result := sg_IdeEnhancements.RowCount - 1;
    if sg_IdeEnhancements.Cells[0, Result] <> '' then begin
      Inc(Result);
      sg_IdeEnhancements.RowCount := Result + 1;
    end;
    sg_IdeEnhancements.Cells[0, Result] := _Handler.Description;
    sg_IdeEnhancements.Objects[0, Result] := _Handler;
  end;

var
  i: Integer;
  Handler: TManagedFormHandler;
  Row: Integer;
  Supported: TStandardFormChangesSet;
  Enabled: TStandardFormChangesSet;
  sfc: TStandardFormChanges;
  s: string;
begin
  sg_IdeEnhancements.RowCount := 2;
  SetRow(0, ['Form', 'Fix Form', 'Make sizeable', 'Store size', 'Store position', 'Store splitter',
    'Resize pic dialogs', 'Combobox Lines', 'Enhance Form']);
  for i := 0 to TManagedFormHandlerFactory.Count - 1 do begin
    Handler := TManagedFormHandlerFactory.GetHandler(i);
    Row := AppendRow(Handler);
    Supported := Handler.SupportedChanges;
    Enabled := Handler.EnabledChanges;
    for sfc := Low(TStandardFormChanges) to High(TStandardFormChanges) do begin
      if sfc in Supported then begin
        if sfc in Enabled then
          s := '_1_'
        else
          s := '_0_';
      end else
        s := '___';
      sg_IdeEnhancements.Cells[Ord(sfc) + sfcOffset, Row] := s;
    end;
  end;
  TGrid_Resize(sg_IdeEnhancements, [roUseGridWidth, roReduceMinWidth, roUseAllRows]);
end;

procedure TfrConfigureFormEnhancements.ApplyGrid;
var
  Row: Integer;
  Handler: TManagedFormHandler;
  sfc: TStandardFormChanges;
  s: string;
  Enabled: TStandardFormChangesSet;
begin
  for Row := 1 to sg_IdeEnhancements.RowCount - 1 do begin
    Handler := sg_IdeEnhancements.Objects[0, Row] as TManagedFormHandler;
    Enabled := [];
    for sfc := Low(TStandardFormChanges) to High(TStandardFormChanges) do begin
      s := sg_IdeEnhancements.Cells[Ord(sfc) + sfcOffset, Row];
      if s = '_1_' then
        Include(Enabled, sfc);
    end;
    Handler.EnabledChanges := Enabled;
  end;
end;

end.
