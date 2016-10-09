unit GX_IdeGotoEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  GX_UnitPositions;

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
  GX_OtaUtils,
  ToolsAPI,
  Windows,
  Messages,
  GX_IdeFormEnhancer,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes;

type
  TWinControlHack = class(TWinControl)
  end;

type
  TGotoEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    lb_UnitPositions: TListBox;
    FUnitPositions: TUnitPositions;
    FLineInput: TWinControlHack;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsGotoForm(_Form: TCustomForm): Boolean;
    procedure lb_UnitPositionsClick(Sender: TObject);
    procedure LineInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create;
    destructor Destroy; override;
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

constructor TGotoEnhancer.Create;
begin
  inherited Create;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TGotoEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
  inherited;
end;

function TGotoEnhancer.IsGotoForm(_Form: TCustomForm): Boolean;
const
  GOTO_DIALOG_CLASS = 'TGotoLineDialog';
  GOTO_DIALOG_NAME = 'GotoLineDialog';
begin
  Result := (_Form.ClassName = GOTO_DIALOG_CLASS) and (_Form.Name = GOTO_DIALOG_NAME);
end;

function TryGetControl(_frm: TForm; _Idx: Integer; const _Name: string; out _ctrl: TControl): Boolean;
begin
  Result := False;
  if _frm.ControlCount <= _Idx then
    Exit;
  _ctrl := _frm.Controls[_Idx];
  Result := (_ctrl.Name = _Name);
end;

function TryGetButton(_frm: TForm; _Idx: Integer; const _Name: string; out _btn: TButton): Boolean;
var
  ctrl: TControl;
begin
  Result := TryGetControl(_frm, _Idx, _Name, ctrl);
  if Result and (ctrl is TButton) then
    _btn := TButton(ctrl);
end;

function TryGetBevel(_frm: TForm; _Idx: Integer; const _Name: string; out _bvl: TBevel): Boolean;
var
  ctrl: TControl;
begin
  Result := TryGetControl(_frm, _Idx, _Name, ctrl);
  if Result and (ctrl is TBevel) then
    _bvl := TBevel(ctrl);
end;

function TryGetWinControl(_frm: TForm; _Idx: Integer; const _Name: string; out _wctrl: TWinControl): Boolean;
var
  ctrl: TControl;
begin
  Result := TryGetControl(_frm, _Idx, _Name, ctrl);
  if Result and (ctrl is TWinControl) then
    _wctrl := TWinControl(ctrl);
end;

procedure TGotoEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
const
  LB_UNIT_POSITIONS = 'GxIdeGotoEnhancerUnitPositionsListbox';
var
  frm: TForm;
  Bevel1: TBevel;
  OkButton: TButton;
  CancelButton: TButton;
  HelpButton: TButton;
  Items: TStrings;
  i: Integer;
begin
  if not IsGotoForm(_Form) then begin
    Exit;
  end;

  frm := TForm(_Form);
  if not TryGetBevel(frm, 0, 'Bevel1', Bevel1) then
    Exit;
  if not TryGetWinControl(frm, 2, 'LineInput', TWinControl(FLineInput)) then
    Exit;
  if not TryGetButton(frm, 3, 'OKButton', OkButton) then
    Exit;
  if not TryGetButton(frm, 4, 'CancelButton', CancelButton) then
    Exit;
  if not TryGetButton(frm, 5, 'HelpButton', HelpButton) then
    Exit;
  if Assigned(frm.FindComponent(LB_UNIT_POSITIONS)) then
    Exit;

  frm.Height := 240;
  OkButton.Top := frm.ClientHeight - OkButton.Height - 8;
  CancelButton.Top := frm.ClientHeight - CancelButton.Height - 8;
  HelpButton.Top := frm.ClientHeight - HelpButton.Height - 8;
  lb_UnitPositions := TListBox.Create(frm);
  lb_UnitPositions.Name := LB_UNIT_POSITIONS;
  lb_UnitPositions.Parent := frm;
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
  CharPos := View.PosToCharPos(FUnitPositions.Positions[Idx].Position);
  View.ConvertPos(False, CursorPos, CharPos);
  TCombobox_SetText(FLineInput, IntToStr(CursorPos.Line));
  TCombobox_SelectAll(FLineInput);
end;

end.
