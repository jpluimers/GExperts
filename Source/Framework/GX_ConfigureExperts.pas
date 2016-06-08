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
  private
    FThumbSize: Integer;
    FExperts: TList;
    procedure ConfigureExpertClick(_Sender: TObject);
    procedure FilterVisibleExperts;
    procedure SetAllEnabled(_Value: Boolean);
    procedure SetDefaultShortcutClick(_Sender: TObject);
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
  GX_dzVclUtils;

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

function TryGetControl(_Owner: TControl; _CtrlClass: TControlClass; out _ctrl: TControl): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to _Owner.ComponentCount - 1 do begin
    _ctrl := _Owner.Components[i] as TControl;
    if _ctrl is _CtrlClass then begin
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
        GetHotkeyCtrl(AControl as TPanel).HotKey := AnExpert.GetDefaultShortCut;
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
  GetHotkeyCtrl(GetPanel(sbxExperts, Idx)).HotKey := AnExpert.GetDefaultShortCut;
end;

procedure TfrConfigureExperts.Init(_Experts: TList);
resourcestring
  SConfigureButtonCaption = 'Configure...';
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

    hk := THotKey.Create(pnl);
    hk.Parent := pnl;
    hk.BoundsRect := edtExpert.BoundsRect;
    hk.Anchors := edtExpert.Anchors;
    THotkey_SetHotkey(hk, AnExpert.ShortCut);
    hk.Visible := AnExpert.CanHaveShortCut;
    hk.Tag := i;

    btn := TButton.Create(pnl);
    btn.Parent := pnl;
    btn.BoundsRect := btnDefault.BoundsRect;
    btn.Anchors := btnDefault.Anchors;
    btn.Caption := 'Default';
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
  sbxExperts.VertScrollBar.Range := FExperts.Count * RowHeight;
  pnlExpertLayout.Visible := False;
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
  sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position + FThumbSize;
  Handled := True;
end;

procedure TfrConfigureExperts.FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position - FThumbSize;
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
    end;
  end;

  sbxExperts.VertScrollBar.Range := CurrTop;
end;

end.
