unit GX_IdeGotoEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  GX_dzCompilerAndRtlVersions;

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
  Windows,
  Classes,
  Messages,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Graphics,
  ToolsAPI,
  GX_OtaUtils,
  GX_dzClassUtils,
  GX_UnitPositions,
  GX_IdeFormEnhancer,
  GX_IdeDialogEnhancer,
  GX_IdeDetectForms,
  GX_IdeUtils;

type
  TWinControlHack = class(TWinControl)
  end;

type
  TGotoEnhancer = class(TIdeDialogEnhancer)
  private
    lb_UnitPositions: TListBox;
    FUnitPositions: TUnitPositions;
    FLineInput: TWinControlHack;
    FOkButton: TButton;
{$IF CompilerVersion = CompilerVersionDelphiX103}
    // Let's hope this gets fixed by Embarcadero, but I wouldn't hold my breath
    tim_Update: TTimer;
    FForm: TForm;
    procedure tim_UpdateTimer(_Sender: TObject);
{$IFEND}
    procedure lb_UnitPositionsClick(Sender: TObject);
    procedure lb_UnitPositionsDblClick(Sender: TObject);
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
begin
  Result := IsGotoForm(_Form);
end;

procedure TGotoEnhancer.EnhanceForm(_Form: TForm);
const
  LB_UNIT_POSITIONS = 'GxIdeGotoEnhancerUnitPositionsListbox';
var
  Bevel1: TBevel;
  CancelButton: TButton;
  HelpButton: TButton;
  Items: TStrings;
  i: Integer;
  cmp: TComponent;
  Y: Integer;
begin
  if TComponent_FindComponent(_Form, LB_UNIT_POSITIONS, True, cmp) then
    Exit;

  if not TComponent_FindComponent(_Form, 'Bevel1', True, TComponent(Bevel1), TBevel) then
    Exit;
  if not TComponent_FindComponent(_Form, 'LineInput', True, TComponent(FLineInput), TWinControl) then
    Exit;
  if not TComponent_FindComponent(_Form, 'OKButton', True, TComponent(FOkButton), TButton) then
    Exit;
  if not TComponent_FindComponent(_Form, 'CancelButton', True, TComponent(CancelButton), TButton) then
    Exit;
  if not TComponent_FindComponent(_Form, 'HelpButton', True, TComponent(HelpButton), TButton) then
    Exit;

  lb_UnitPositions := TListBox.Create(_Form);
  lb_UnitPositions.Name := LB_UNIT_POSITIONS;
  lb_UnitPositions.Parent := _Form;
  lb_UnitPositions.Top := Bevel1.Top + Bevel1.Height + 8;
  lb_UnitPositions.Left := Bevel1.Left;
  lb_UnitPositions.Width := Bevel1.Width;
  lb_UnitPositions.Height := FOkButton.Top - lb_UnitPositions.Top - 8;
  lb_UnitPositions.OnClick := lb_UnitPositionsClick;
  lb_UnitPositions.OnDblClick := lb_UnitPositionsDblClick;
  lb_UnitPositions.TabOrder := FOkButton.TabOrder;

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
  lb_UnitPositions.ClientHeight := (FUnitPositions.Count + 1) * lb_UnitPositions.ItemHeight;
  Y := lb_UnitPositions.Top + lb_UnitPositions.Height + 8;
  FOkButton.Top := Y;
  CancelButton.Top := Y;
  HelpButton.Top := Y;
  _Form.ClientHeight := Y + FOkButton.Height + 8;

  FLineInput.OnKeyDown := LineInputKeyDown;

{$IF CompilerVersion = CompilerVersionDelphiX103}
  if IsThemingEnabled then begin
    FForm := _Form;
    tim_Update := TTimer.Create(_Form);
    tim_Update.OnTimer := tim_UpdateTimer;
    tim_Update.Interval := 100;
    tim_Update.Enabled := True;
  end;
{$IFEND}
end;

{$IF CompilerVersion = CompilerVersionDelphiX103}

procedure TGotoEnhancer.tim_UpdateTimer(_Sender: TObject);
begin
  // This is a workaround for the drawing problem of the enhanced Goto dialog in
  // Delphi 10.3 Rio. The same workaround as for the Search Path dialog
  // (see TFormChangeManagerInternal.ProcessActivatedForm) did not work here, possibly
  // because the GotoDialog gets resized while the Search Path dialog doesn't.
  // So we move it one pixel to the left and then to the right.
  tim_Update.Enabled := False;
  FForm.Left := FForm.Left - 1;
  FForm.Left := FForm.Left + 1;
  FreeAndNil(tim_Update);
end;
{$IFEND}

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
  _cmb.Perform(WM_SETTEXT, 0, LPARAM(PCharType(_Text)));
  _cmb.Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TCombobox_SelectAll(_cmb: TWinControl);
begin
  SendMessage(_cmb.Handle, CB_SETEDITSEL, 0, LPARAM($FFFF0000));
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

procedure TGotoEnhancer.lb_UnitPositionsDblClick(Sender: TObject);
begin
  if lb_UnitPositions.ItemIndex < 0 then
    Exit; //==>
  lb_UnitPositionsClick(Sender);
  if Assigned(FOkButton) then
    FOkButton.Click;
end;

end.

