unit GX_ConfigureExperts;

{$I GX_CondDefine.inc}

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
  StdCtrls,
  ComCtrls,
  ExtCtrls;

type
  TScrollBox = class(Forms.TScrollBox)
//    procedure WMHScroll(var _Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var _Msg: TWMVScroll); message WM_VSCROLL;
  private
    FOnScrollVert: TNotifyEvent;
//    FOnScrollHorz: TNotifyEvent;
  public
    property OnScrollVert: TNotifyEvent read FOnScrollVert write FOnScrollVert;
//    property OnScrollHorz: TNotifyEvent read FOnScrollHorz write FOnScrollHorz;
  end;

type
  TfrConfigureExperts = class(TFrame)
    pnlExpertsFilter: TPanel;
    lblFilter: TLabel;
    edtFilter: TEdit;
    sbxExperts: TScrollBox;
    pnlExpertLayout: TPanel;
    imgExpert: TImage;
    chkExpert: TCheckBox;
    edtExpert: THotKey;
    btnExpert: TButton;
    btnEnableAll: TButton;
    btnDisableAll: TButton;
    btnClear: TButton;
    btnClearAll: TButton;
    btnSetAllDefault: TButton;
    btnDefault: TButton;
    procedure edtFilterChange(Sender: TObject);
    procedure btnEnableAllClick(Sender: TObject);
    procedure btnDisableAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FrameMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FrameMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnSetAllDefaultClick(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
  private
    FThumbSize: Integer;
    FExperts: TList;
    FVisibleExpertsCount: Integer;
    procedure ConfigureExpertClick(_Sender: TObject);
    procedure FilterVisibleExperts;
    procedure SetAllEnabled(_Value: Boolean);
    procedure SetDefaultShortcutClick(_Sender: TObject);
    procedure HandleVerticalScroll(_Sender: TObject);
    procedure SetConfigButtonHotkey;
    procedure ScrollBy(_DeltaY: Integer);
    procedure AdjustScrollRange;
    procedure PositionControls;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure Init(_Experts: TList);
    procedure SaveExperts;
  end;

implementation

{$R *.dfm}

uses
  Menus,
  Themes,
  GX_GenericUtils,
  GX_BaseExpert,
  GX_dzVclUtils,
  GX_DbugIntf;

resourcestring
  SConfigureButtonCaption = 'Configure...';
  SDefaultButtonCaption = 'Default';

function IsThemesEnabled: Boolean;
begin
{$IF CompilerVersion >= 23}
  Result := StyleServices.Enabled;
{$ELSE}
{$IF CompilerVersion >= 18}
  Result := ThemeServices.ThemesEnabled;
{$ELSE}
  Result := False;
{$IFEND}
{$IFEND}
end;

type
  THintImage = class(TImage)
    procedure CMHintShow(var _Msg: TCMHintShow); message CM_HINTSHOW;
  end;

{ THintImage }

procedure THintImage.CMHintShow(var _Msg: TCMHintShow);
var
  hi: PHintInfo;
begin
  hi := _Msg.HintInfo;
  hi.HideTimeout := -1;
  hi.HintMaxWidth := 400;
  hi.HintPos := ClientToScreen(Point(0, BoundsRect.Bottom));
end;

{ TfrConfigureEditorExperts }

constructor TfrConfigureExperts.Create(_Owner: TComponent);
begin
  inherited;
  FExperts := TList.Create;

  if IsThemesEnabled then begin
    btnClear.Top := edtFilter.Top - 1;
    btnClear.Height := edtFilter.Height + 2;
  end else begin
    btnClear.Top := edtFilter.Top;
    btnClear.Height := edtFilter.Height;
  end;

  pnlExpertsFilter.FullRepaint := False;

  sbxExperts.OnScrollVert := HandleVerticalScroll;
end;

destructor TfrConfigureExperts.Destroy;
begin
  FreeAndNil(FExperts);
  inherited;
end;

procedure TfrConfigureExperts.edtFilterChange(Sender: TObject);
begin
  FilterVisibleExperts;
end;

procedure TfrConfigureExperts.edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  DeltaY: Integer;
begin
  case Key of
    VK_DOWN:
      DeltaY := 1;
    VK_UP:
      DeltaY := -1;
    VK_NEXT:
      DeltaY := 5;
    VK_PRIOR:
      DeltaY := -5;
  else
    Exit;
  end;
  ScrollBy(DeltaY);
  Key := 0;
end;

procedure TfrConfigureExperts.ScrollBy(_DeltaY: Integer);
begin
  sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position + FThumbSize * _DeltaY;
  SetConfigButtonHotkey;
end;

function TryGetButton(_pnl: TPanel; const _Caption: string; out _btn: TButton): Boolean;
var
  i: Integer;
  ctrl: TControl;
begin
  Result := False;
  for i := 0 to _Pnl.ComponentCount - 1 do begin
    ctrl := _Pnl.Components[i] as TControl;
    if ctrl is TButton then begin
      if StripHotkey(TButton(ctrl).Caption) = _Caption then begin
        _btn := TButton(ctrl);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TryGetConfigButton(_Pnl: TPanel; out _btn: TButton): Boolean;
begin
  Result := TryGetButton(_Pnl, SConfigureButtonCaption, _btn);
end;

function TryGetDefaultButton(_Pnl: TPanel; out _btn: TButton): Boolean;
begin
  Result := TryGetButton(_Pnl, SDefaultButtonCaption, _btn);
end;

function TryGetHotkeyEdit(_pnl: TPanel; out _edt: THotKey): Boolean;
var
  i: Integer;
  ctrl: TControl;
begin
  Result := False;
  for i := 0 to _Pnl.ComponentCount - 1 do begin
    ctrl := _Pnl.Components[i] as TControl;
    if ctrl is THotKey then begin
      _edt := THotKey(ctrl);
      Result := True;
      Exit; //==>
    end;
  end;
end;

procedure TfrConfigureExperts.SetConfigButtonHotkey;
var
  h: Integer;
  PanelIdx: Integer;
  Panel: TPanel;
  btn: TButton;
begin
  h := pnlExpertLayout.Height;
  for PanelIdx := 0 to sbxExperts.ControlCount - 1 do begin
    Panel := sbxExperts.Controls[PanelIdx] as TPanel;
    if Panel <> pnlExpertLayout then begin
      if Panel.Visible then begin
        if TryGetConfigButton(Panel, btn) then begin
          if (Panel.Top >= -11) and (Panel.Top < h - 11) then begin
            btn.Caption := '&' + SConfigureButtonCaption;
          end else begin
            btn.Caption := SConfigureButtonCaption;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrConfigureExperts.HandleVerticalScroll(_Sender: TObject);
begin
  SetConfigButtonHotkey;
end;

function TryGetControl(_Owner: TControl; _CtrlClass: TControlClass; out _Ctrl: TControl): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to _Owner.ComponentCount - 1 do begin
    _Ctrl := _Owner.Components[i] as TControl;
    if _Ctrl is _CtrlClass then begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetCheckbox(_Pnl: TPanel): TCheckBox;
resourcestring
  STR_NO_CHECKBOX = 'No checkbox found.';
begin
  if not TryGetControl(_Pnl, TCheckBox, TControl(Result)) then
    raise Exception.Create(STR_NO_CHECKBOX);
end;

function GetHotkeyCtrl(_Pnl: TPanel): THotKey;
resourcestring
  STR_NO_HOTKEY = 'No hotkey control found.';
begin
  if not TryGetControl(_Pnl, THotKey, TControl(Result)) then
    raise Exception.Create(STR_NO_HOTKEY);
end;

function GetPanel(_sbx: TScrollBox; _Tag: Integer): TPanel;
resourcestring
  STR_NO_PANEL = 'No panel found.';
var
  i: Integer;
  ctrl: TControl;
begin
  for i := 0 to _sbx.ComponentCount - 1 do begin
    ctrl := _sbx.Components[i] as TControl;
    if (ctrl.Tag = _Tag) and (ctrl is TPanel) then begin
      Result := ctrl as TPanel;
      Exit;
    end;
  end;
  raise Exception.Create(STR_NO_PANEL);
end;

procedure TfrConfigureExperts.SetAllEnabled(_Value: Boolean);
var
  i: Integer;
  AControl: TControl;
begin
  for i := 0 to sbxExperts.ComponentCount - 1 do begin
    AControl := sbxExperts.Components[i] as TControl;
    if AControl is TPanel then
      GetCheckbox(AControl as TPanel).Checked := _Value;
  end;
end;

procedure TfrConfigureExperts.btnClearClick(Sender: TObject);
begin
  edtFilter.Text := '';
  edtFilter.SetFocus;
end;

procedure TfrConfigureExperts.btnDisableAllClick(Sender: TObject);
begin
  SetAllEnabled(False);
end;

procedure TfrConfigureExperts.btnEnableAllClick(Sender: TObject);
begin
  SetAllEnabled(True);
end;

procedure TfrConfigureExperts.btnClearAllClick(Sender: TObject);
resourcestring
  SWarning = 'This will clear the keyboard shortcuts for all experts.'#13#10
    + 'Are you sure you want to do that?';
var
  i: Integer;
  AControl: TControl;
begin
  if MessageDlg(SWarning, mtWarning, [mbYes, mbCancel], 0) = mrYes then
    for i := 0 to sbxExperts.ComponentCount - 1 do begin
      AControl := sbxExperts.Components[i] as TControl;
      if AControl is TPanel then
        GetHotkeyCtrl(AControl as TPanel).HotKey := 0;
    end;
end;

procedure TfrConfigureExperts.btnSetAllDefaultClick(Sender: TObject);
resourcestring
  SWarning = 'This will set the keyboard shortcuts of all experts to their defaults.'#13#10
    + 'Are you sure you want to do that?';
var
  i: Integer;
  AControl: TControl;
  AnExpert: TGX_BaseExpert;
begin
  if (MessageDlg(SWarning, mtWarning, [mbYes, mbCancel], 0) = mrYes) then
    for i := 0 to sbxExperts.ComponentCount - 1 do begin
      AControl := sbxExperts.Components[i] as TControl;
      if AControl is TPanel then begin
        AnExpert := FExperts[AControl.Tag];
        THotkey_SetHotkey(GetHotkeyCtrl(AControl as TPanel), AnExpert.GetDefaultShortCut);
      end;
    end;
end;

procedure TfrConfigureExperts.ConfigureExpertClick(_Sender: TObject);
var
  AnExpert: TGX_BaseExpert;
  Idx: Integer;
begin
  Idx := (_Sender as TButton).Tag;
  AnExpert := FExperts[Idx];
  AnExpert.Configure;
end;

procedure TfrConfigureExperts.SetDefaultShortcutClick(_Sender: TObject);
var
  AnExpert: TGX_BaseExpert;
  Idx: Integer;
begin
  Idx := (_Sender as TButton).Tag;
  AnExpert := FExperts[Idx];
  THotkey_SetHotkey(GetHotkeyCtrl(GetPanel(sbxExperts, Idx)), AnExpert.GetDefaultShortCut);
end;

type
  THotKeyHack = class(THotKey)
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  end;

{ THotKeyHack }

function AppendToCsvStr(const _AppendTo: string; const _s: string): string;
begin
  if _AppendTo = '' then
    Result := _s
  else
    Result := _AppendTo + ', ' + _s;
end;

{$IFOPT D+}

function ModifierStr(_Modifiers: THKModifiers): string;
begin
  Result := '';
  if hkShift in _Modifiers then
    Result := AppendToCsvStr(Result, 'hkShift');
  if hkCtrl in _Modifiers then
    Result := AppendToCsvStr(Result, 'hkCtrl');
  if hkAlt in _Modifiers then
    Result := AppendToCsvStr(Result, 'hkAlt');
  if hkExt in _Modifiers then
    Result := AppendToCsvStr(Result, 'hkExt');
end;

function ShiftStateToStr(_Shift: TShiftState): string;
begin
  Result := '';
  if ssShift in _Shift then
    Result := AppendToCsvStr(Result, 'ssShift');
  if ssAlt in _Shift then
    Result := AppendToCsvStr(Result, 'ssAlt');
  if ssCtrl in _Shift then
    Result := AppendToCsvStr(Result, 'ssCtrl');
  if ssLeft in _Shift then
    Result := AppendToCsvStr(Result, 'ssLeft');
  if ssRight in _Shift then
    Result := AppendToCsvStr(Result, 'ssRight');
  if ssMiddle in _Shift then
    Result := AppendToCsvStr(Result, 'ssMiddle');
  if ssDouble in _Shift then
    Result := AppendToCsvStr(Result, 'ssDouble');
end;
{$ENDIF}

procedure THotKeyHack.KeyDown(var Key: Word; Shift: TShiftState);
var
  HkMods: THKModifiers;
begin
  inherited;
  // This is a fix for THotkey behaving very oddly:
  // When pressing a key without modifiers (e.g. A) it is displayed as "A" or sometimes as "Shift + A"
  // even though InvalidKeys is [hcNone, hcShift] so neihter should be a valid hotkey.
  // This explicitly sets the Modifiers property to get the documented behaviour.
  // It has a side effect, though:
  // Pressing Shift followed by F2 will first show Alt+ followed by Shift+F2.
  // But that's better than the original behaviour.
  HkMods := Modifiers;
  if ssShift in Shift then
    Include(HkMods, hkShift)
  else
    Exclude(HkMods, hkShift);
  if ssCtrl in Shift then
    Include(HkMods, hkCtrl)
  else
    Exclude(HkMods, hkCtrl);
  if ssAlt in Shift then
    Include(HkMods, hkAlt)
  else
    Exclude(HkMods, hkAlt);
  if (HkMods = []) or (HkMods = [hkShift]) then begin
    HkMods := [hkAlt];
  end;
  Modifiers := HkMods;
{$IFOPT D+}
  SendDebugFmt('KeyDown(Key: %4x, Shift: [%s], ShortCut: %s, Modifiers: [%s]',
    [Key, ShiftStateToStr(Shift), ShortCutToText(HotKey), ModifierStr(Modifiers)]);
{$ENDIF}
end;

procedure THotKeyHack.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  inherited;
{$IFOPT D+}
  SendDebugFmt('KeyUp(Key: %4x, Shift: [%s], ShortCut: %s, Modifiers: [%s]',
    [Key, ShiftStateToStr(Shift), ShortCutToText(HotKey), ModifierStr(Modifiers)]);
{$ENDIF}
end;

procedure TfrConfigureExperts.Init(_Experts: TList);
var
  i: Integer;
  AnExpert: TGX_BaseExpert;
  RowWidth: Integer;
  RowHeight: Integer;
  pnl: TPanel;
  img: TImage;
  chk: TCheckBox;
  hk: THotKey;
  btn: TButton;
begin
  FExperts.Assign(_Experts);

  if IsThemesEnabled then begin
    btnDefault.Top := edtExpert.Top - 1;
    btnDefault.Height := edtExpert.Height + 2;
  end else begin
    btnDefault.Top := edtExpert.Top;
    btnDefault.Height := edtExpert.Height;
  end;
  btnExpert.Top := btnDefault.Top;
  btnExpert.Height := btnDefault.Height;

  RowWidth := sbxExperts.Width + 3;
  RowHeight := pnlExpertLayout.Height;
  FThumbSize := RowHeight;
  for i := 0 to FExperts.Count - 1 do begin
    AnExpert := FExperts[i];

    pnl := TPanel.Create(sbxExperts);
    pnl.Parent := sbxExperts;
    pnl.SetBounds(0, i * RowHeight, RowWidth, RowHeight);
    pnl.Anchors := pnlExpertLayout.Anchors;
    pnl.Tag := i;
    pnl.FullRepaint := False;

    img := THintImage.Create(pnl);
    img.Parent := pnl;
    img.BoundsRect := imgExpert.BoundsRect;
    img.Picture.Bitmap.Assign(AnExpert.GetBitmap);
    img.Transparent := True;
    img.Center := True;
    img.Stretch := False;
    img.Hint := AnExpert.GetHelpString;
    img.ShowHint := True;
    img.Tag := i;

    chk := TCheckBox.Create(pnl);
    chk.Parent := pnl;
    chk.BoundsRect := chkExpert.BoundsRect;
    chk.Caption := AnExpert.GetDisplayName;
    chk.Checked := AnExpert.Active;
    chk.Tag := i;

    hk := THotKeyHack.Create(pnl);
    hk.Parent := pnl;
    hk.BoundsRect := edtExpert.BoundsRect;
    hk.Anchors := edtExpert.Anchors;
    hk.DoubleBuffered := False;
    hk.InvalidKeys := [hcNone, hcShift];
    hk.Modifiers := [hkAlt];
    THotkey_SetHotkey(hk, AnExpert.ShortCut);
    hk.Visible := AnExpert.CanHaveShortCut;
    hk.Tag := i;

    btn := TButton.Create(pnl);
    btn.Parent := pnl;
    btn.BoundsRect := btnDefault.BoundsRect;
    btn.Anchors := btnDefault.Anchors;
    btn.Caption := SDefaultButtonCaption;
    if AnExpert.GetDefaultShortCut <> 0 then begin
      btn.Hint := ShortCutToText(AnExpert.GetDefaultShortCut);
      btn.ShowHint := True;
      btn.OnClick := SetDefaultShortcutClick;
    end else
      btn.Enabled := False;
    btn.Tag := i;

    hk.Width := btn.Left - hk.Left;

    if AnExpert.HasConfigOptions then begin
      btn := TButton.Create(pnl);
      btn.Parent := pnl;
      btn.Caption := SConfigureButtonCaption;
      btn.BoundsRect := btnExpert.BoundsRect;
      btn.Anchors := btnExpert.Anchors;
      btn.OnClick := ConfigureExpertClick;
      btn.Tag := i;
    end;
  end;
  pnlExpertLayout.Visible := False;

  FVisibleExpertsCount := FExperts.Count;

  PositionControls;
  AdjustScrollRange;
end;

procedure TfrConfigureExperts.SaveExperts;
var
  AControl: TControl;
  AnExpert: TGX_BaseExpert;
  i: Integer;
  pnl: TPanel;
begin
  for i := 0 to sbxExperts.ComponentCount - 1 do begin
    AControl := sbxExperts.Components[i] as TControl;
    if AControl is TPanel then begin
      AnExpert := FExperts[AControl.Tag];
      pnl := AControl as TPanel;
      AnExpert.Active := GetCheckbox(pnl).Checked;
      AnExpert.ShortCut := GetHotkeyCtrl(pnl).HotKey;
    end;
  end;

  for i := 0 to FExperts.Count - 1 do begin
    TGX_BaseExpert(FExperts[i]).SaveSettings;
  end;
end;

procedure TfrConfigureExperts.FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBy(1);
  Handled := True;
end;

procedure TfrConfigureExperts.FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBy(-1);
  Handled := True;
end;

procedure TfrConfigureExperts.FilterVisibleExperts;
var
  Panel: TPanel;
  CheckBox: TCheckBox;
  i, CurrTop: Integer;
  SubText: string;
begin
  sbxExperts.VertScrollBar.Position := 0;
  SubText := Trim(edtFilter.Text);

  FVisibleExpertsCount := 0;
  CurrTop := 0;
  for i := 0 to sbxExperts.ControlCount - 1 do begin
    Panel := sbxExperts.Controls[i] as TPanel;
    if Panel <> pnlExpertLayout then begin
      if SubText = '' then
        Panel.Visible := True
      else begin
        CheckBox := Panel.Controls[1] as TCheckBox;
        Panel.Visible := StrContains(SubText, CheckBox.Caption, False);
      end;
    end;
    if Panel.Visible then begin
      Panel.Top := CurrTop;
      Inc(CurrTop, Panel.Height);
      Inc(FVisibleExpertsCount);
    end;
  end;

  AdjustScrollRange;

  SetConfigButtonHotkey;
end;

procedure TfrConfigureExperts.AdjustScrollRange;
begin
  sbxExperts.VertScrollBar.Range := (FVisibleExpertsCount) * FThumbSize
    + sbxExperts.Height - FThumbSize - 4;
end;

procedure TfrConfigureExperts.PositionControls;
const
  GAP_WIDTH = 8;
var
  w: Integer;
  i: Integer;
  Panel: TPanel;
  btn: TButton;
  ed: THotKey;
begin
  // For whatever reason the automatic alignment via anchors does not work correctly
  // so we just do it in code.
  w := sbxExperts.ClientWidth;
  for i := 0 to sbxExperts.ControlCount - 1 do begin
    Panel := sbxExperts.Controls[i] as TPanel;
    Panel.Width := w - Panel.Left;
    if TryGetConfigButton(Panel, btn) then
      btn.Left := w - btnExpert.Width - GAP_WIDTH;
    if TryGetDefaultButton(Panel, btn) then
      btn.Left := w - btnExpert.Width - btnDefault.Width - 2 * GAP_WIDTH;
    if TryGetHotkeyEdit(Panel, ed) then
      ed.Width := w - ed.Left - btnExpert.Width - btnDefault.Width - 2 * GAP_WIDTH;
  end;
end;

procedure TfrConfigureExperts.FrameResize(Sender: TObject);
begin
  PositionControls;
  AdjustScrollRange;
end;

{ TScrollBox }

procedure TScrollBox.WMVScroll(var _Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnScrollVert) then
    FOnScrollVert(Self);
end;

end.
