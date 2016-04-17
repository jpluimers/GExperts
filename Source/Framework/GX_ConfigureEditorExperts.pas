unit GX_ConfigureEditorExperts;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls;

type
  TfrConfigureEditorExperts = class(TFrame)
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
    procedure edtFilterChange(Sender: TObject);
    procedure btnEnableAllClick(Sender: TObject);
    procedure btnDisableAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FrameMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FrameMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
  private
    FThumbSize: Integer;
    FExperts: TList;
    procedure ConfigureExpertClick(_Sender: TObject);
    procedure FilterVisibleExperts;
    procedure SetAllEnabled(_Value: Boolean);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure Init(_Experts: TList);
    procedure SaveExperts;
  end;

implementation

{$R *.dfm}

uses
  GX_GenericUtils,
  GX_BaseExpert;

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

constructor TfrConfigureEditorExperts.Create(_Owner: TComponent);
begin
  inherited;
  FExperts := TList.Create;
end;

destructor TfrConfigureEditorExperts.Destroy;
begin
  FreeAndNil(FExperts);
  inherited;
end;

procedure TfrConfigureEditorExperts.edtFilterChange(Sender: TObject);
begin
  FilterVisibleExperts;
end;

procedure TfrConfigureEditorExperts.SetAllEnabled(_Value: Boolean);
var
  i: Integer;
  AControl: TControl;
begin
  for i := 0 to sbxExperts.ComponentCount - 1 do begin
    AControl := sbxExperts.Components[i] as TControl;
    if AControl is TCheckBox then
      (AControl as TCheckBox).Checked := _Value;
  end;
end;

procedure TfrConfigureEditorExperts.btnClearClick(Sender: TObject);
begin
  edtFilter.Text := '';
  edtFilter.SetFocus;
end;

procedure TfrConfigureEditorExperts.btnDisableAllClick(Sender: TObject);
begin
  SetAllEnabled(False);
end;

procedure TfrConfigureEditorExperts.btnEnableAllClick(Sender: TObject);
begin
  SetAllEnabled(True);
end;

procedure TfrConfigureEditorExperts.ConfigureExpertClick(_Sender: TObject);
var
  AnExpert: TGX_BaseExpert;
  Idx: Integer;
begin
  Idx := (_Sender as TButton).Tag;
  AnExpert := FExperts[Idx];
  AnExpert.Configure;
end;

procedure TfrConfigureEditorExperts.Init(_Experts: TList);
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

  RowWidth := sbxExperts.Width * 3;
  RowHeight := pnlExpertLayout.Height;
  FThumbSize := RowHeight;
  for i := 0 to FExperts.Count - 1 do begin
    AnExpert := FExperts[i];

    pnl := TPanel.Create(Self);
    pnl.Parent := sbxExperts;
    pnl.SetBounds(0, i * RowHeight, RowWidth, RowHeight);
    pnl.Tag := i;
    pnl.FullRepaint := False;

    img := THintImage.Create(Self);
    img.Parent := pnl;
    img.SetBounds(imgExpert.Left, imgExpert.Top, imgExpert.Width, imgExpert.Height);
    img.Picture.Bitmap.Assign(AnExpert.GetBitmap);
    img.Transparent := True;
    img.Center := True;
    img.Stretch := False;
    img.Hint := AnExpert.GetHelpString;
    img.ShowHint := True;

    chk := TCheckBox.Create(sbxExperts);
    chk.Parent := pnl;
    chk.SetBounds(chkExpert.Left, chkExpert.Top, chkExpert.Width, chkExpert.Height);
    chk.Caption := AnExpert.GetDisplayName;
//    chk.Checked := AnExpert.Active;
    chk.Tag := i;

    hk := THotKey.Create(sbxExperts);
    hk.Parent := pnl;
    hk.SetBounds(edtExpert.Left, edtExpert.Top, edtExpert.Width, edtExpert.Height);
    hk.HotKey := AnExpert.ShortCut;
//    hk.Visible := AnExpert.HasMenuItem;
    hk.Tag := i;

    if AnExpert.HasConfigOptions then begin
      btn := TButton.Create(Self);
      with TButton.Create(Self) do begin
        btn.Parent := pnl;
        btn.Caption := SConfigureButtonCaption;
        btn.SetBounds(btnExpert.Left, btnExpert.Top, btnExpert.Width, btnExpert.Height);
        btn.OnClick := ConfigureExpertClick;
        btn.Tag := i;
      end;
    end;
  end;
  sbxExperts.VertScrollBar.Range := FExperts.Count * RowHeight;
  pnlExpertLayout.Visible := False;
end;

procedure TfrConfigureEditorExperts.SaveExperts;
var
  AControl: TControl;
  AnExpert: TGX_BaseExpert;
  i: Integer;
begin
  for i := 0 to sbxExperts.ComponentCount - 1 do begin
    AControl := sbxExperts.Components[i] as TControl;

    AnExpert := FExperts[AControl.Tag];
    if AControl is TCheckBox then
//      AnExpert.Active := TCheckBox(AControl).Checked
    else if AControl is THotKey then
      AnExpert.ShortCut := THotKey(AControl).HotKey;
  end;

  for i := 0 to FExperts.Count - 1 do begin
    TGX_BaseExpert(FExperts[i]).SaveSettings;
  end;
end;

procedure TfrConfigureEditorExperts.FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position + FThumbSize;
  Handled := True;
end;

procedure TfrConfigureEditorExperts.FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position - FThumbSize;
  Handled := True;
end;

procedure TfrConfigureEditorExperts.FilterVisibleExperts;
var
  Panel: TPanel;
  CheckBox: TCheckBox;
  i, CurrTop: Integer;
  SubText: string;
begin
  sbxExperts.VertScrollBar.Position := 0;
  SubText := Trim(edtFilter.Text);
  if SubText = '' then
    for i := 0 to sbxExperts.ControlCount - 1 do begin
      Panel := sbxExperts.Controls[i] as TPanel;
      if Panel <> pnlExpertLayout then
        Panel.Visible := True;
    end else
    for i := 0 to sbxExperts.ControlCount - 1 do begin
      Panel := sbxExperts.Controls[i] as TPanel;
      CheckBox := Panel.Controls[1] as TCheckBox;
      Panel.Visible := StrContains(SubText, CheckBox.Caption, False) and (Panel <> pnlExpertLayout);
    end;

  CurrTop := 0;
  for i := 0 to sbxExperts.ControlCount - 1 do begin
    Panel := sbxExperts.Controls[i] as TPanel;
    if Panel.Visible then begin
      Panel.Top := CurrTop;
      Inc(CurrTop, Panel.Height);
    end;
  end;
  sbxExperts.VertScrollBar.Range := CurrTop;
end;

end.
