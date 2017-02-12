unit GX_IdeGotoEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxIdeGotoEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  ToolsAPI,
  Windows,
  Messages,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  GX_OtaUtils,
  GX_dzClassUtils,
  GX_UnitPositions,
  GX_IdeFormEnhancer,
  GX_IdeDialogEnhancer;

type
  TWinControlHack = class(TWinControl)
  end;

type
  TGotoEnhancer = class(TIdeDialogEnhancer)
  private
    lb_UnitPositions: TListBox;
    FUnitPositions: TUnitPositions;
    FLineInput: TWinControlHack;
    procedure lb_UnitPositionsClick(Sender: TObject);
    procedure LineInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
  end;

var
  TheGotoEnhancer: TGotoEnhancer = nil;

{ TGxIdeGotoEnhancer }

class function TGxIdeGotoEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheGotoEnhancer);
end;

class procedure TGxIdeGotoEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheGotoEnhancer) then
      TheGotoEnhancer := TGotoEnhancer.Create
  end else
    FreeAndNil(TheGotoEnhancer);
end;

{ TGotoEnhancer }

function TGotoEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  GOTO_DIALOG_CLASS = 'TGotoLineDialog';
  GOTO_DIALOG_NAME = 'GotoLineDialog';
begin
  Result := (_Form.ClassName = GOTO_DIALOG_CLASS) and (_Form.Name = GOTO_DIALOG_NAME);
end;

procedure TGotoEnhancer.EnhanceForm(_Form: TForm);
const
  LB_UNIT_POSITIONS = 'GxIdeGotoEnhancerUnitPositionsListbox';
var
  Bevel1: TBevel;
  OkButton: TButton;
  CancelButton: TButton;
  HelpButton: TButton;
  Items: TStrings;
  i: Integer;
  cmp: TComponent;
begin
  if TComponent_FindComponent(_Form, LB_UNIT_POSITIONS, True, cmp) then
    Exit;

  if not TComponent_FindComponent(_Form, 'Bevel1', True, TComponent(Bevel1), TBevel) then
    Exit;
  if not TComponent_FindComponent(_Form, 'LineInput', True, TComponent(FLineInput), TWinControl) then
    Exit;
  if not TComponent_FindComponent(_Form, 'OKButton', True, TComponent(OkButton), TButton) then
    Exit;
  if not TComponent_FindComponent(_Form, 'CancelButton', True, TComponent(CancelButton), TButton) then
    Exit;
  if not TComponent_FindComponent(_Form, 'HelpButton', True, TComponent(HelpButton), TButton) then
    Exit;

  _Form.Height := 240;
  OkButton.Top := _Form.ClientHeight - OkButton.Height - 8;
  CancelButton.Top := _Form.ClientHeight - CancelButton.Height - 8;
  HelpButton.Top := _Form.ClientHeight - HelpButton.Height - 8;
  lb_UnitPositions := TListBox.Create(_Form);
  lb_UnitPositions.Name := LB_UNIT_POSITIONS;
  lb_UnitPositions.Parent := _Form;
  lb_UnitPositions.Top := Bevel1.Top + Bevel1.Height + 8;
  lb_UnitPositions.Left := Bevel1.Left;
  lb_UnitPositions.Width := Bevel1.Width;
  lb_UnitPositions.Height := OkButton.Top - lb_UnitPositions.Top - 8;
  lb_UnitPositions.OnClick := lb_UnitPositionsClick;
  lb_UnitPositions.TabOrder := OkButton.TabOrder;

  Items := lb_UnitPositions.Items;
  Items.BeginUpdate;
  try
    FUnitPositions := TUnitPositions.Create(GxOtaGetCurrentSourceEditor);
    for i := 0 to FUnitPositions.Count - 1 do begin
      Items.Add(FUnitPositions.Positions[i].Name);
    end;
  finally
    Items.EndUpdate;
  end;

  FLineInput.OnKeyDown := LineInputKeyDown;
end;

type
{$IFDEF GX_VER160_up}
  TStringType = WideString;
  PCharType = PWideChar;
{$ELSE}
  TStringType = string;
  PCharType = PAnsiChar;
{$ENDIF}

procedure TCombobox_SetText(_cmb: TWinControl; const _Text: TStringType);
begin
  _cmb.Perform(WM_SETTEXT, 0, Longint(PCharType(_Text)));
  _cmb.Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TCombobox_SelectAll(_cmb: TWinControl);
begin
  SendMessage(_cmb.Handle, CB_SETEDITSEL, 0, Integer($FFFF0000));
end;

function TCombobox_DroppedDown(_cmb: TWinControl): Boolean;
begin
  Result := LongBool(SendMessage(_cmb.Handle, CB_GETDROPPEDSTATE, 0, 0));
end;

procedure TGotoEnhancer.LineInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not TComboBox(FLineInput).DroppedDown then begin
    if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then begin
      SendMessage(lb_UnitPositions.Handle, WM_KEYDOWN, Key, 0);
      Key := 0;
    end;
  end;
end;

procedure TGotoEnhancer.lb_UnitPositionsClick(Sender: TObject);
var
  Idx: Integer;
  View: IOTAEditView;
  CharPos: TOTACharPos;
  CursorPos: TOTAEditPos;
begin
  Idx := lb_UnitPositions.ItemIndex;
  if Idx = -1 then
    Exit;
  View := GxOtaGetTopMostEditView;
  CharPos := GxOtaGetCharPosFromPos(FUnitPositions.Positions[Idx].Position, View);
  View.ConvertPos(False, CursorPos, CharPos);
  TCombobox_SetText(FLineInput, IntToStr(CursorPos.Line));
  TCombobox_SelectAll(FLineInput);
end;

end.
