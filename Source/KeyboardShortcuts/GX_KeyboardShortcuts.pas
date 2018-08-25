unit GX_KeyboardShortcuts;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Grids,
  Types,
  Controls,
  Forms,
  Dialogs,
  Graphics,
  GX_Experts,
  GX_BaseForm;

type
  TfmGxKeyboardShortcuts = class(TfmBaseForm)
    sg_Actions: TStringGrid;
    procedure sg_ActionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sg_ActionsMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sg_ActionsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    function CompareShortcuts(_Idx1, _Idx2: Integer): Integer;
    procedure SwapEntries(_Idx1, _Idx2: Integer);
    procedure DrawStringGridCell(_sg: TStringGrid; const _Text: string;
      const _Rect: TRect; _State: TGridDrawState; _Duplicate: Boolean);
    function ScrollGrid(_Grid: TStringGrid; _Direction: Integer; _Shift: TShiftState;
      _MousePos: TPoint): Boolean;
  public
    class procedure Execute(_bmp: TBitmap);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  StrUtils,
  Math,
  Actions,
  ActnList,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_ActionBroker,
  GX_dzVclUtils,
  GX_dzQuicksort,
  GX_GenericUtils;

type
  TGxKeyboardShortcuts = class(TGX_Expert)
  private
  protected
  public
    function CanHaveShortCut: Boolean; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGxKeyboardShortcuts }

procedure TGxKeyboardShortcuts.Execute(Sender: TObject);
begin
  TfmGxKeyboardShortcuts.Execute(GetBitmap);
end;

function TGxKeyboardShortcuts.CanHaveShortCut: Boolean;
begin
  Result := False;
end;

constructor TGxKeyboardShortcuts.Create;
begin
  inherited Create;
end;

destructor TGxKeyboardShortcuts.Destroy;
begin
  inherited Destroy;
end;

function TGxKeyboardShortcuts.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Keyboard Shortcuts';
begin
  Result := SMenuCaption;
end;

function TGxKeyboardShortcuts.GetHelpString: string;
resourcestring
  SSampleExpertHelp =
    'List all keyboard shortcuts of registered actions in the IDE';
begin
  Result := SSampleExpertHelp;
end;

function TGxKeyboardShortcuts.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TfmGxKeyboardShortcuts }

class procedure TfmGxKeyboardShortcuts.Execute(_bmp: TBitmap);
var
  frm: TfmGxKeyboardShortcuts;
begin
  frm := TfmGxKeyboardShortcuts.Create(nil);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

function TfmGxKeyboardShortcuts.ScrollGrid(_Grid: TStringGrid; _Direction: Integer;
  _Shift: TShiftState; _MousePos: TPoint): Boolean;
var
  LScrollLines: Integer;
  NewRow: Integer;
begin
  if SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @LScrollLines, 0) then begin
    if _Shift = [ssCtrl] then
      LScrollLines := LScrollLines * 3; // Ctrl = Speed
    if _Shift = [ssShift] then
      LScrollLines := 1;

    NewRow := _Grid.Row + (_Direction * LScrollLines);
    NewRow := Max(_Grid.FixedRows, NewRow); // Limit to top row
    NewRow := Min(_Grid.RowCount - 1, NewRow); // Limit to bottom row

    _Grid.Row := NewRow;
    Result := True;
  end else
    Result := False;
end;

function ShortcutToSortText(_Shortcut: TShortCut): string;

  function ShiftToChar(_KeyIsSet: Boolean; _c: Char): Char;
  begin
    if _KeyIsSet then
      Result := _c
    else
      Result := ' ';
  end;

  function ShiftToStr(_Shift: TShiftState): string;
  begin
    // ssShift, ssAlt, ssCtrl,
    Result := ShiftToChar(ssShift in _Shift, 'S')
      + ShiftToChar(ssAlt in _Shift, 'A')
      + ShiftToChar(ssCtrl in _Shift, 'C');
  end;

  function KeyToStr(_Key: Word): string;
  begin
    Result := ShortCutToText(ShortCut(_Key, []));
    Result := RightStr(StringOfChar(' ', 20) + Result, 20);
  end;

var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(_Shortcut, Key, Shift);
  Result := ShiftToStr(Shift) + KeyToStr(Key);
end;

constructor TfmGxKeyboardShortcuts.Create(_Owner: TComponent);
var
  i: Integer;
  ContAct: TContainedAction;
  act: TCustomAction;
  Row: Integer;
  j: Integer;
  ShortCut: TShortCut;
  s: string;
begin
  inherited;

  TControl_SetMinConstraints(Self);

  sg_Actions.Cells[0, 0] := 'Shortcut';
  sg_Actions.Cells[1, 0] := 'Action';
  sg_Actions.Cells[2, 0] := 'Caption';

  sg_Actions.OnMouseWheelDown := sg_ActionsMouseWheelDown;
  sg_Actions.OnMouseWheelUp := sg_ActionsMouseWheelUp;

  Row := sg_Actions.FixedRows;
  for i := 0 to GxActionBroker.ActionCount - 1 do begin
    ContAct := GxActionBroker.Actions[i];
    if ContAct is TCustomAction then begin
      act := TCustomAction(ContAct);
      if act.ShortCut <> 0 then begin
        TGrid_SetNonfixedRowCount(sg_Actions, Row);
        sg_Actions.Objects[0, Row] := Pointer(act.ShortCut);
        sg_Actions.Cells[0, Row] := ShortCutToText(act.ShortCut);
        sg_Actions.Cells[1, Row] := act.Name;
        sg_Actions.Cells[2, Row] := act.Caption;
        Inc(Row);
      end;
      for j := 0 to act.SecondaryShortCuts.Count - 1 do begin
        ShortCut := act.SecondaryShortCuts.ShortCuts[j];
        if ShortCut = 0 then begin
          s := act.SecondaryShortCuts.Strings[j];
{$IFOPT D+}
          SendDebugFmt('secondary shortcut for action %s is 0 (text: %s)', [act.Name, s]);
{$ENDIF}
          ShortCut := TextToShortCut(s);
        end;
        if ShortCut = 0 then begin
{$IFOPT D+}
          SendDebugFmt('secondary shortcut for action %s is still 0 after TextToShortcut', [act.Name]);
{$ENDIF}
        end else begin
          TGrid_SetNonfixedRowCount(sg_Actions, Row);
          sg_Actions.Objects[0, Row] := Pointer(ShortCut);
          sg_Actions.Cells[0, Row] := s + ' *';
          sg_Actions.Cells[1, Row] := act.Name;
          sg_Actions.Cells[2, Row] := act.Caption;
          Inc(Row);
        end;
      end;
    end;
  end;
  QuickSort(sg_Actions.FixedRows, Row - 1, CompareShortcuts, SwapEntries);

  for i := sg_Actions.FixedRows to Row - 2 do begin
    if SameText(sg_Actions.Cells[0, i], sg_Actions.Cells[0, i + 1]) then begin
      sg_Actions.Objects[1, i] := Pointer(1);
      sg_Actions.Objects[1, i + 1] := Pointer(1);
    end;
  end;

  TStringGrid_AdjustRowHeight(sg_Actions);
  TGrid_Resize(sg_Actions, [roUseGridWidth, roUseAllRows, roReduceMinWidth]);
end;

function TfmGxKeyboardShortcuts.CompareShortcuts(_Idx1, _Idx2: Integer): Integer;
var
  ShortCut1: TShortCut;
  ShortCut2: TShortCut;
  s1: string;
  s2: string;
begin
  ShortCut1 := Integer(sg_Actions.Objects[0, _Idx1]);
  ShortCut2 := Integer(sg_Actions.Objects[0, _Idx2]);
  s1 := ShortcutToSortText(ShortCut1);
  s2 := ShortcutToSortText(ShortCut2);
  Result := CompareText(s1, s2);
end;

procedure TfmGxKeyboardShortcuts.SwapEntries(_Idx1, _Idx2: Integer);
var
  s: string;
  c: Integer;
  p: Pointer;
begin
  for c := 0 to sg_Actions.ColCount - 1 do begin
    s := sg_Actions.Cells[c, _Idx1];
    sg_Actions.Cells[c, _Idx1] := sg_Actions.Cells[c, _Idx2];
    sg_Actions.Cells[c, _Idx2] := s;
  end;
  p := sg_Actions.Objects[0, _Idx1];
  sg_Actions.Objects[0, _Idx1] := sg_Actions.Objects[0, _Idx2];
  sg_Actions.Objects[0, _Idx2] := p;
end;

procedure TfmGxKeyboardShortcuts.DrawStringGridCell(_sg: TStringGrid; const _Text: string;
  const _Rect: TRect; _State: TGridDrawState; _Duplicate: Boolean);
var
  cnv: TCanvas;
begin
  cnv := _sg.Canvas;
  if _Text = '' then
    cnv.Brush.Color := _sg.Color
  else begin
    if _Duplicate then begin
      cnv.Brush.Color := clYellow;
      cnv.Font.Color := clBlack;
    end;
  end;
  cnv.FillRect(_Rect);
  cnv.TextRect(_Rect, _Rect.Left + 2, _Rect.Top + 2, _Text);
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  sg: TStringGrid absolute Sender;
begin
  DrawStringGridCell(sg, sg.Cells[ACol, ARow], Rect, State, LongBool(sg.Objects[1, ARow]));
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (Sender is TStringGrid) then
    if ScrollGrid(TStringGrid(Sender), +1, Shift, MousePos) then
      Handled := True;
end;

procedure TfmGxKeyboardShortcuts.sg_ActionsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Sender is TStringGrid then
    if ScrollGrid(TStringGrid(Sender), -1, Shift, MousePos) then
      Handled := True;
end;

initialization
  RegisterGX_Expert(TGxKeyboardShortcuts);
end.
