unit GX_dzSpeedBitBtn;

interface

uses
  Windows,
  Classes,
  Buttons,
  Graphics;

type
  ///<summary>
  /// A helper component that turns a TBitBtn into a button that works similar to a TSpeedButton
  /// but can receive the focus. It allows to either set a caption or a Glyph, but will ignore
  /// the Glyph if Caption is <> ''.
  /// Clicking the button will first set its Tag property to 0 (up) or down and then call the
  /// original OnClick method.
  /// To use it create it with TdzSpeedBitBtn.Create(BitBtn) where BitBtn is an already existing
  /// TBitBtn component. TdzSpeedBitBtn will be automatically destroyed when the associated BitBtn
  /// is destroyed, so don't free it yourself.
  /// Note that this has so far only been tested with Windows 8.1. I have no idea what it looks
  /// like on Windows XP, Vista, 7 or 10. </summary>
  TdzSpeedBitBtn = class(TComponent)
  private
    FCaption: string;
    FBtn: TBitBtn;
    FOrigBmp: TBitmap;
    FOrigOnClick: TNotifyEvent;
    FUpBmp: TBitmap;
    FDownBmp: TBitmap;
    procedure doOnClick(_Sender: TObject);
    procedure HandleOnClick(_Sender: TObject);
    function GetDown: Boolean;
    procedure SetDown(const Value: Boolean);
    procedure UpdateGlyph;
  public
    constructor Create(_btn: TComponent); override;
    destructor Destroy; override;
    property Down: Boolean read GetDown write SetDown;
  end;

implementation

{ TdzSpeedBitBtn }

constructor TdzSpeedBitBtn.Create(_btn: TComponent);

  procedure PrepareBmp(w, h: Integer; _Color: TColor; _Edge: UINT; out _bmp: TBitmap);
  var
    cnv: TCanvas;
    qrc: TRect;
    TextSize: TSize;
  begin
    _bmp := TBitmap.Create;
    _bmp.Width := w;
    _bmp.Height := h;
    _bmp.TransparentColor := clFuchsia;

    cnv := _bmp.Canvas;

    cnv.Brush.Color := _Color;
    cnv.Brush.Style := bsSolid;
    cnv.FillRect(Rect(0, 0, w, h));

    qrc := Rect(0, 0, w - 1, h - 2);
    DrawEdge(cnv.Handle, qrc, _Edge, BF_RECT);

    if FCaption <> '' then begin
      TextSize := cnv.TextExtent(FCaption);
      cnv.TextOut((w - TextSize.cx) div 2, (h - TextSize.cy) div 2, FCaption);
    end else begin
      cnv.Draw((w - FOrigBmp.Width) div 2, (h - FOrigBmp.Height) div 2, FOrigBmp);
    end;

  end;

var
  w: Integer;
  h: Integer;
  ColBack1: TColor;
  ColBack2: TColor;
begin
  inherited Create(_btn);
  FBtn := _btn as TBitBtn;
  FOrigOnClick := FBtn.OnClick;
  FCaption := FBtn.Caption;

  FOrigBmp := TBitmap.Create;
  FOrigBmp.Assign(FBtn.Glyph);
  FOrigBmp.Transparent := True;

  FBtn.Caption := '';

  w := FBtn.Width - 1;
  h := FBtn.Height - 1;

  ColBack1 := rgb(240, 240, 240); // clBtnFace;
  ColBack2 := rgb(245, 245, 245); // a bit lighter than clBtnFace;

  PrepareBmp(w, h, ColBack1, EDGE_RAISED, FUpBmp);
  PrepareBmp(w, h, ColBack2, EDGE_SUNKEN, FDownBmp);

  FBtn.OnClick := HandleOnClick;

  UpdateGlyph;
end;

destructor TdzSpeedBitBtn.Destroy;
begin
  // If we get here, either the constructor failed (which automatically calls the destructor)
  // or FBtn was already destroyed, so we must not access it at all.
  FUpBmp.Free;
  FDownBmp.Free;
  FOrigBmp.Free;
  inherited;
end;

procedure TdzSpeedBitBtn.doOnClick(_Sender: TObject);
begin
  if Assigned(FOrigOnClick) then
    FOrigOnClick(_Sender);
end;

procedure TdzSpeedBitBtn.HandleOnClick(_Sender: TObject);
begin
  Down := not Down;
  doOnClick(_Sender);
end;

function TdzSpeedBitBtn.GetDown: Boolean;
begin
  Result := (FBtn.Tag <> 0);
end;

procedure TdzSpeedBitBtn.SetDown(const Value: Boolean);
begin
  if Value then
    FBtn.Tag := 1
  else
    FBtn.Tag := 0;
  UpdateGlyph;
end;

procedure TdzSpeedBitBtn.UpdateGlyph;
begin
  if FBtn.Tag <> 0 then
    FBtn.Glyph := FDownBmp
  else
    FBtn.Glyph := FUpBmp;
end;

end.

