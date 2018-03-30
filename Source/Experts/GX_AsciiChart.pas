unit GX_AsciiChart;

{$I GX_CondDefine.inc}

// Original author: Taz Higgins <taz@taz.compulink.co.uk>
// This unit has NOT been prepared for localization

interface

uses
  Forms, ExtCtrls, Controls, Classes, StdCtrls, Menus, ComCtrls, ActnList,
  ToolWin, ImgList, GX_BaseForm;

type
  TfmAsciiChart = class(TfmBaseForm)
    pmContext: TPopupMenu;
    mitSep4: TMenuItem;
    mitAbout: TMenuItem;
    mitShowLowCharacters: TMenuItem;
    mitShowHighCharacters: TMenuItem;
    mitSep3: TMenuItem;
    mitFontSize8: TMenuItem;
    mitFontSize10: TMenuItem;
    mitFontSize12: TMenuItem;
    mitSep1: TMenuItem;
    mitSep2: TMenuItem;
    mitCharAsDec: TMenuItem;
    mitCharAsHex: TMenuItem;
    HintTimer: TTimer;
    mitShowHints: TMenuItem;
    mitHelp: TMenuItem;
    Actions: TActionList;
    ToolBar: TToolBar;
    tbnCharLow: TToolButton;
    tbnCharHigh: TToolButton;
    tbnCharDec: TToolButton;
    tbnCharHex: TToolButton;
    cbxFontName: TComboBox;
    edFontSize: TEdit;
    updFontSize: TUpDown;
    actCharLow: TAction;
    actCharHigh: TAction;
    actCharDec: TAction;
    actCharHex: TAction;
    eChars: TEdit;
    actFontSize8: TAction;
    actFontSize10: TAction;
    actFontSize12: TAction;
    actShowHints: TAction;
    tbnSep1: TToolButton;
    tbnSep2: TToolButton;
    actHelpHelp: TAction;
    actHelpAbout: TAction;
    btnClear: TButton;
    procedure FormPaint(Sender: TObject);
    procedure cbxFontNameChange(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HintTimerTimer(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure updFontSizeClick(Sender: TObject; Button: TUDBtnType);
    procedure edFontSizeChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxFontNameEnter(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
    procedure actShowHintsExecute(Sender: TObject);
    procedure actFontSize8Execute(Sender: TObject);
    procedure actFontSize10Execute(Sender: TObject);
    procedure actFontSize12Execute(Sender: TObject);
    procedure actCharDecExecute(Sender: TObject);
    procedure actCharHexExecute(Sender: TObject);
    procedure actCharHexUpdate(Sender: TObject);
    procedure actCharDecUpdate(Sender: TObject);
    procedure actGenericFontSizeUpdate(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actCharHighExecute(Sender: TObject);
    procedure actCharLowExecute(Sender: TObject);
    procedure actCharLowUpdate(Sender: TObject);
    procedure actCharHighUpdate(Sender: TObject);
    procedure actShowHintsUpdate(Sender: TObject);
    procedure ToolBarResize(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FStartCharacter: Integer;
    FDisplayFontSize: Integer;
    FFontName: string;
    FShowHex: Boolean;
    FShowHints: Boolean;
    FOldHorizMult: Integer;
    FOldVertMult: Integer;
    FHint: THintWindow;
    FOldCharPos: Integer;
    FZoomFontSize: Integer;
    function TryGetCharPos(MouseX, MouseY: Integer; out CharPos: Integer): Boolean; overload;
    function TryGetCharPos(MouseX, MouseY: Integer; out CharPos: Integer;
      out XPos, YPos, HorizMult, VertMult: Integer): Boolean; overload;
    procedure DrawCharacter(const CharValue: Integer; const CharText: string;
      const HorizMult, VertMult: Integer);
    procedure GetFonts;
    procedure SetFontName(const NewFontName: string);
    procedure SetShowHex(const Value: Boolean);
    procedure SetStartCharacter(const Value: Integer);
    procedure SetDisplayFontSize(const Size: Integer);
    procedure KillHint;
    procedure DoHint(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  fmAsciiChart: TfmAsciiChart = nil;

implementation

{$R *.dfm}

uses
  Windows, Messages, SysUtils, Graphics, GX_Experts,
  GX_GExperts, GX_ConfigurationInfo, GX_GxUtils, GX_GenericUtils, GX_IdeUtils;

type
  TASCIIExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;


const
  DescLow: array[0..31] of string =
    ('Null', 'Start of Header', 'Start of Text', 'End of Text',
    'End of Transmission', 'Enquiry', 'Acknowledge', 'Bell',
    'Backspace', 'Horizontal Tab', 'Linefeed', 'Vertical Tab',
    'Form Feed', 'Carriage Return', 'Shift Out', 'Shift in',
    'Data Link Escape', 'Device Control 1 (XON resume)', 'Device Control 2',
    'Device Control 3 (XOFF pause)', 'Device Control 4', 'Negative Acknowledge',
    'Synchronous Idle', 'End Transmission Block', 'Cancel', 'End of Medium',
    'Substitute', 'Escape', 'File Separator', 'Group Separator',
    'Record Separator', 'Unit Separator');

  MinimumDisplayFontSize = 4;
  MaximumDisplayFontSize = 35;
  DefaultDisplayFontSize = 10;
  DefaultFontName = 'Tahoma';

procedure TfmAsciiChart.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Settings: TGExpertsSettings;
begin
  KillHint;
  // Do not localize any of the following lines.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteInteger(ConfigurationKey, 'Font Size', FDisplayFontSize);
    Settings.WriteString(ConfigurationKey, 'Font Name', FFontName);
    Settings.WriteInteger(ConfigurationKey, 'Font Base', FStartCharacter);
    Settings.WriteString(ConfigurationKey, 'Edit Display Text', eChars.Text);
    Settings.WriteBool(ConfigurationKey, 'Show Hex', FShowHex);
    Settings.WriteInteger(ConfigurationKey, 'Zoom Font Size', FZoomFontSize);
    Settings.WriteBool(ConfigurationKey, 'Show Hint', FShowHints);
    Settings.SaveForm(Self, ConfigurationKey + '\Window');
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmAsciiChart.FormPaint(Sender: TObject);
{ It's much quicker to draw the characters in one style, change
  styles then draw all the others in the other style than do each draw
  one after another changing styles as I go along }
var
  i, j: Integer; { general loop counters }
  X, Y: Integer; { screen pixel locations }
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  Start: Integer; { low charnum for character rendering }
begin
  if FStartCharacter = 0 then Start := 32 else Start := 0;
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - ToolBar.Height) div 16;
  Canvas.Brush.Style := bsClear;
  { draw the character value as Int or Hex on screen }
  Canvas.Font.Name := 'Tahoma';  // do not localize
  Canvas.Font.Size := 9;
  Canvas.Font.Color := clGrayText;
  { Only do the if check once for improved speed rather than every iteration }
  if FShowHex then
  begin
    for i := 0 to 127 do
    begin
      X := i div 16;
      Y := i mod 16;
      Canvas.TextOut(X * HorizMult + 2, Y * VertMult + 28, IntToHex(FStartCharacter + i, 2));
    end;
  end
  else
  begin
    for i := 0 to 127 do
    begin
      X := i div 16;
      Y := i mod 16;
      Canvas.TextOut(X * HorizMult + 2, Y * VertMult + 28, IntToStr(FStartCharacter + i));
    end;
  end;
  { Draw in the characters 0-31 if required }
  Canvas.Font.Color := clWindowText;
  if FStartCharacter = 0 then
  begin
    DrawCharacter(0,  'NUL', HorizMult, VertMult); // Ctrl @, NULL
    DrawCharacter(1,  'SOH', HorizMult, VertMult); // Ctrl A, Start of Header
    DrawCharacter(2,  'STX', HorizMult, VertMult); // Ctrl B, Start of Text
    DrawCharacter(3,  'ETX', HorizMult, VertMult); // Ctrl C, End of Text
    DrawCharacter(4,  'EOT', HorizMult, VertMult); // Ctrl D, End of Transmission
    DrawCharacter(5,  'ENQ', HorizMult, VertMult); // Ctrl E, Enquiry
    DrawCharacter(6,  'ACK', HorizMult, VertMult); // Ctrl F, Acknowlodge
    DrawCharacter(7,  'BEL', HorizMult, VertMult); // Ctrl G, Bell
    DrawCharacter(8,  'BS',  HorizMult, VertMult); // Ctrl H, Backspace
    DrawCharacter(9,  'TAB', HorizMult, VertMult); // Ctrl I, Horizontal Tab
    DrawCharacter(10, 'LF',  HorizMult, VertMult); // Ctrl J, Linefeed
    DrawCharacter(11, 'VT',  HorizMult, VertMult); // Ctrl K, Vertical Tab
    DrawCharacter(12, 'FF',  HorizMult, VertMult); // Ctrl L, Form Feed
    DrawCharacter(13, 'CR',  HorizMult, VertMult); // Ctrl M, Carridge Return
    DrawCharacter(14, 'SO',  HorizMult, VertMult); // Ctrl N, Shift Out
    DrawCharacter(15, 'SI',  HorizMult, VertMult); // Ctrl O, Shift in
    DrawCharacter(16, 'DLE', HorizMult, VertMult); // Ctrl P, Delete
    DrawCharacter(17, 'DC1', HorizMult, VertMult); // Ctrl Q, Device Control 1
    DrawCharacter(18, 'DC2', HorizMult, VertMult); // Ctrl R, Device Control 2
    DrawCharacter(19, 'DC3', HorizMult, VertMult); // Ctrl S, Device Control 3
    DrawCharacter(20, 'DC4', HorizMult, VertMult); // Ctrl T, Device Control 4
    DrawCharacter(21, 'NAK', HorizMult, VertMult); // Ctrl U, Negative Acknowledge
    DrawCharacter(22, 'SYN', HorizMult, VertMult); // Ctrl V, Synchronise
    DrawCharacter(23, 'ETB', HorizMult, VertMult); // Ctrl W, End Block ??
    DrawCharacter(24, 'CAN', HorizMult, VertMult); // Ctrl X, Cancel
    DrawCharacter(25, 'EM',  HorizMult, VertMult); // Ctrl Y, End Message
    DrawCharacter(26, 'SUB', HorizMult, VertMult); // Ctrl Z, Sub
    DrawCharacter(27, 'ESC', HorizMult, VertMult); // Ctrl [, Escape
    DrawCharacter(28, 'FS',  HorizMult, VertMult); // Ctrl \, Form Separator
    DrawCharacter(29, 'GS',  HorizMult, VertMult); // Ctrl ], Group Separator
    DrawCharacter(30, 'RS',  HorizMult, VertMult); // Ctrl ^, Record Separator
    DrawCharacter(31, 'US',  HorizMult, VertMult); // Ctrl _, Unit Separator
  end;

  { draw the character of that number on screen }
  Canvas.Font.Size := FDisplayFontSize;
  Canvas.Font.Name := FFontName;

  for i := Start to 127 do
    DrawCharacter(i, Char(FStartCharacter + i), HorizMult, VertMult);

  { Draw the boxes on the screen }
  { Only two colour assignments to canvas speeds things up }
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  { 1) draw left and top sides }
  Canvas.Pen.Color := clBtnHighlight;
  for i := 0 to 7 do
    for j := 0 to 15 do
      Canvas.PolyLine([Point(i * HorizMult, (j + 1) * VertMult + 24),
        Point(i * HorizMult, j * VertMult + 25),
          Point((i + 1) * HorizMult - 1, j * VertMult + 25)]);
  { 2) draw right and bottom sides }
  Canvas.Pen.Color := clBtnShadow;
  for i := 0 to 7 do
    for j := 0 to 15 do
      Canvas.PolyLine([Point((i + 1) * HorizMult - 1, j * VertMult + 25),
        Point((i + 1) * HorizMult - 1, (j + 1) * VertMult + 24),
          Point(i * HorizMult - 1, (j + 1) * VertMult + 24)]);
end;

procedure TfmAsciiChart.DrawCharacter(const CharValue: Integer; const CharText: string;
  const HorizMult, VertMult: Integer);
{ This draws the text on the screen at the relevant location }
var
  X, Y: Integer; { Screen Locations }
  MyRect: TRect; { general drawing reectangle }
  VOffset, HOffset: Integer; { V and H offsets for bounding box of character in font }
begin
  X := CharValue div 16;
  Y := CharValue mod 16;
  VOffset := (VertMult - Canvas.TextHeight(CharText)) div 2;
  HOffset := (HorizMult - 24 - Canvas.TextWidth(CharText)) div 2;
  MyRect.Left := X * HorizMult + 24;
  MyRect.Right := (X + 1) * HorizMult;
  MyRect.Top := Y * VertMult + 26;
  MyRect.Bottom := (Y + 1) * VertMult + 26;
  Canvas.TextRect(MyRect, MyRect.Left + HOffset, MyRect.Top + VOffset, CharText);
end;

procedure TfmAsciiChart.FormResize(Sender: TObject);
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
begin
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - ToolBar.Height) div 16;
  if (HorizMult <> FOldHorizMult) or (VertMult <> FOldVertMult) then
  begin
    FOldHorizMult := HorizMult;
    FOldVertMult := VertMult;
    Self.Refresh;
  end;
  KillHint;
end;

function TfmAsciiChart.TryGetCharPos(MouseX, MouseY: Integer; out CharPos: Integer;
  out XPos, YPos, HorizMult, VertMult: Integer): Boolean; 
begin
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - ToolBar.Height) div 16;
  XPos := MouseX div HorizMult;
  YPos := (MouseY - 25) div VertMult;
  { only generate charpos if clicking inside the boundaries of the cells
    avoids the clicking beyond the right/bottom extents of the cells }
  Result := (XPos < 8) and (YPos < 16);
  if Result then begin
    CharPos := FStartCharacter + XPos * 16 + YPos;
    Result := (CharPos > -1) and (CharPos < 256);
  end;
end;

function TfmAsciiChart.TryGetCharPos(MouseX, MouseY: Integer; out CharPos: Integer): Boolean;
var
  HorizMult, VertMult: Integer;
  XPos, YPos: Integer;
begin
  Result := TryGetCharPos(MouseX, MouseY, CharPos, XPos, YPos, HorizMult, VertMult);
end;

procedure TfmAsciiChart.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Charpos: Integer;
begin
  if Button = mbRight then
    pmContext.Popup(X + Left + GetSystemMetrics(SM_CXFRAME),
      Y + Self.Top + GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION))
  else
  begin
    if TryGetCharPos(X, Y, Charpos) then begin
      eChars.SelText := Char(CharPos);
    end;
  end;
end;

procedure TfmAsciiChart.cbxFontNameChange(Sender: TObject);
begin
  // Update the font used for drawing characters.
  FFontName := cbxFontName.Text;
  eChars.Font.Name := FFontName;
  Self.Refresh;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
                       FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Assert(Assigned(S));

  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or not SameText(S[S.Count - 1], Temp) then
    S.Add(Temp);
  Result := 1;
end;

procedure TfmAsciiChart.GetFonts;
var
  DC: HDC;
  LFont: TLogFont;
begin
  FillChar(LFont, SizeOf(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;

  DC := GetDC(0);
  try
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Longint(cbxFontName.Items), 0);
  finally
    ReleaseDC(0, DC);
  end;

  cbxFontName.Sorted := True;
end;

procedure TfmAsciiChart.SetFontName(const NewFontName: string);
var
  i: Integer;
begin
  if cbxFontName.Text = NewFontName then
    Exit;

  // Set the font name in the combo box
  // otherwise it would be blank.
  for i := 0 to cbxFontName.Items.Count - 1 do
  begin
    if SameText(cbxFontName.Items[i], NewFontName) then
    begin
      cbxFontName.ItemIndex := i;
      Break;
    end;
  end;
end;

procedure TfmAsciiChart.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{ Charpos is the ordinal value of the cell clicked on }
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  XPos, YPos: Integer; { X and Y cells clicked on }
  HintDrawingRect: TRect;
  HintText: string;
  HintScreenOffset: TPoint;
  CharPos: Integer;
begin
  if not TryGetCharPos(X, Y, CharPos, XPos, YPos, HorizMult, VertMult) then
    Exit;

  { Create my custom hint }
  if (FOldCharPos <> CharPos) and Self.Active then
  begin
    KillHint;
    FHint := THintWindow.Create(Self);
    FHint.Color := clInfoBk;
    if (FStartCharacter = 0) and (CharPos < 32) and (FOldCharPos <> CharPos) then
    begin
      HintText := DescLow[CharPos];
    end
    else
    begin
      HintText := Char(CharPos);
      with FHint.Canvas.Font do
      begin
        Charset := DEFAULT_CHARSET;
        Name := FFontName;
        Size := FZoomFontSize;
      end;
    end;
    HintDrawingRect := FHint.CalcHintRect(Screen.Width, HintText, nil);
    HintScreenOffset := ClientToScreen(Point((XPos + 1) * HorizMult - 1, (YPos + 1) * VertMult + 24));
    OffsetRect(HintDrawingRect, HintScreenOffset.x, HintScreenOffset.y);
    if FShowHints then
      FHint.ActivateHint(HintDrawingRect, HintText);
    HintTimer.Enabled := True;
    FOldCharPos := CharPos;
  end;
end;

procedure TfmAsciiChart.KillHint;
begin
  if Assigned(FHint) then
  begin
    FHint.ReleaseHandle;
    FreeAndNil(FHint);
  end;
  HintTimer.Enabled := False;
end;

procedure TfmAsciiChart.HintTimerTimer(Sender: TObject);
begin
  HintTimer.Enabled := False;
  KillHint;
end;

procedure TfmAsciiChart.DoHint(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.FormDeactivate(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.DoDeactivate(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.updFontSizeClick(Sender: TObject;
  Button: TUDBtnType);
begin
  FDisplayFontSize := updFontSize.Position;
  Self.Refresh;
end;

procedure TfmAsciiChart.edFontSizeChange(Sender: TObject);
var
  NewFontSize: Integer;
begin
  NewFontSize := StrToIntDef(edFontSize.Text, FDisplayFontSize);

  if (NewFontSize < MinimumDisplayFontSize) or
     (NewFontSize > MaximumDisplayFontSize) or
     (NewFontSize = FDisplayFontSize) then
  begin
    Exit;
  end;

  FDisplayFontSize := NewFontSize;
  Self.Refresh;
end;

procedure TfmAsciiChart.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfmAsciiChart.cbxFontNameEnter(Sender: TObject);
begin
  cbxFontName.Perform(CB_SETDROPPEDWIDTH, 220, 0);
end;

{ TASCIIExpert }

procedure TASCIIExpert.Execute(Sender: TObject);
begin
  if fmAsciiChart = nil then
  begin
    fmAsciiChart := TfmAsciiChart.Create(nil);
    SetFormIcon(fmAsciiChart);
  end;
  if fmAsciiChart.WindowState = wsMinimized then
    fmAsciiChart.WindowState := wsNormal;
  fmAsciiChart.Show;
  IncCallCount;
end;

destructor TASCIIExpert.Destroy;
begin
  FreeAndNil(fmAsciiChart);

  inherited Destroy;
end;

function TASCIIExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&ASCII Chart';
begin
  Result := SMenuCaption;
end;

class function TASCIIExpert.GetName: string;
begin
  Result := 'ASCIIChart'; // Do not localize.
end;

function TASCIIExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TASCIIExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then //FI:W505
      // Nothing to initialize here.
    else
      FreeAndNil(fmAsciiChart);
  end;
end;

procedure TfmAsciiChart.SetDisplayFontSize(const Size: Integer);
begin
  if Size = FDisplayFontSize then
    Exit;

  FDisplayFontSize := Size;
  edFontSize.Text := IntToStr(Size);
  Self.Refresh;
end;

procedure TfmAsciiChart.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinWidth := 400 + 2 * GetSystemMetrics(SM_CXFRAME);
  MinHeight := 331 + 2 * GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION);
end;

procedure TfmAsciiChart.actShowHintsExecute(Sender: TObject);
begin
  FShowHints := not FShowHints;
end;

procedure TfmAsciiChart.actFontSize8Execute(Sender: TObject);
begin
  SetDisplayFontSize(8);
end;

procedure TfmAsciiChart.actFontSize10Execute(Sender: TObject);
begin
  SetDisplayFontSize(10);
end;

procedure TfmAsciiChart.actFontSize12Execute(Sender: TObject);
begin
  SetDisplayFontSize(12);
end;

procedure TfmAsciiChart.actCharDecExecute(Sender: TObject);
begin
  SetShowHex(False);
end;

procedure TfmAsciiChart.actCharHexExecute(Sender: TObject);
begin
  SetShowHex(True);
end;

procedure TfmAsciiChart.SetShowHex(const Value: Boolean);
begin
  if Value <> FShowHex then
  begin
    FShowHex := Value;

    Self.Refresh;
  end;
end;

procedure TfmAsciiChart.actCharHexUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := FShowHex;
end;

procedure TfmAsciiChart.actCharDecUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := not FShowHex;
end;

procedure TfmAsciiChart.actGenericFontSizeUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := False;
  case FDisplayFontSize of
    8:
      if Sender = actFontSize8 then
        (Sender as TAction).Checked := True;
    10:
      if Sender = actFontSize10 then
        (Sender as TAction).Checked := True;
    12:
      if Sender = actFontSize12 then
        (Sender as TAction).Checked := True;
  end;
end;

procedure TfmAsciiChart.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 21);
end;

procedure TfmAsciiChart.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmAsciiChart.actCharHighExecute(Sender: TObject);
begin
  SetStartCharacter(128);
end;

procedure TfmAsciiChart.SetStartCharacter(const Value: Integer);
begin
  if Value <> FStartCharacter then
  begin
    FStartCharacter := Value;
    Self.Refresh;
  end;
end;

procedure TfmAsciiChart.actCharLowExecute(Sender: TObject);
begin
  SetStartCharacter(0);
end;

procedure TfmAsciiChart.actCharLowUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (FStartCharacter = 0);
end;

procedure TfmAsciiChart.actCharHighUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := (FStartCharacter = 128);
end;

procedure TfmAsciiChart.actShowHintsUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := FShowHints;
end;

procedure TfmAsciiChart.btnClearClick(Sender: TObject);
begin
  eChars.Text := '';
  eChars.SetFocus;
end;

constructor TfmAsciiChart.Create(AOwner: TComponent);
var
  Settings: TGExpertsSettings;
begin
  inherited;

  SetToolbarGradient(ToolBar);
  SetNonModalFormPopupMode(Self);
  GetFonts;
  CenterForm(Self);
  updFontSize.Max := MaximumDisplayFontSize;
  updFontSize.Min := MinimumDisplayFontSize;
  Settings := TGExpertsSettings.Create;
  try
    // Do not localize any of the following items.
    FDisplayFontSize := Settings.ReadInteger(ConfigurationKey, 'Font Size', DefaultDisplayFontSize);
    FFontName := Settings.ReadString(ConfigurationKey, 'Font Name', DefaultFontName);
    FStartCharacter := Settings.ReadInteger(ConfigurationKey, 'Font Base', 0);
    FShowHex := Settings.ReadBool(ConfigurationKey, 'Show Hex', False);
    FZoomFontSize := Settings.ReadInteger(ConfigurationKey, 'Zoom Font Size', 32);
    eChars.Text := Settings.ReadString(ConfigurationKey, 'Edit Display Text', '');
    eChars.SelStart := Length(eChars.Text);
    FShowHints := Settings.ReadBool(ConfigurationKey, 'Show Hint', True);
    Settings.LoadForm(Self, ConfigurationKey + '\Window');
  finally
    FreeAndNil(Settings);
  end;

  updFontSize.Position := FDisplayFontSize;
  SetFontName(FFontName);
  eChars.Font.Name := FFontName;
  EnsureFormVisible(Self);

  Application.OnHint := DoHint;
  Application.OnDeactivate := DoDeactivate;
end;

destructor TfmAsciiChart.Destroy;
begin
  inherited Destroy;

  fmAsciiChart := nil;
end;

procedure TfmAsciiChart.ToolBarResize(Sender: TObject);
begin
  eChars.Width := (Sender as TControl).ClientWidth - eChars.Left - btnClear.Width;
end;

function TfmAsciiChart.ConfigurationKey: string;
begin
  Result := TASCIIExpert.ConfigurationKey;
end;

initialization
  RegisterGX_Expert(TASCIIExpert);
end.

