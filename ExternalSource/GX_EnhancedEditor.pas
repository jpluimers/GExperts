unit GX_EnhancedEditor;

interface

{$I GX_CondDefine.inc}

// This is implemented as a TWinControl to ease design-time usage

uses
  Windows, Classes, Graphics, Controls,
  {$IFDEF SYNEDIT} // Edit GX_CondDefine.inc to compile without requiring SynEdit support
  SynEdit, SynMemo, SynEditTypes, SynEditTextBuffer, GX_SynMemoUtils,
  {$ENDIF SYNEDIT}

  {$IFNDEF GX_ENHANCED_EDITOR}
  ComCtrls, StdCtrls, Printers, Messages, // TRichEdit support
  {$ENDIF GX_ENHANCED_EDITOR}
  GX_GenericUtils;

type
  TGxEnhancedEditor = class(TWinControl)
  private
    {$IFDEF SYNEDIT}
    FEditor: TSynMemo;
    {$ENDIF SYNEDIT}
    {$IFNDEF GX_ENHANCED_EDITOR}
    FEditor: TRichEdit;
    {$ENDIF GX_ENHANCED_EDITOR}
    FHighlighter: TGXSyntaxHighlighter;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnKeyPress: TKeyPressWEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseDown: TMouseEvent;
    function  GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function  GetModified: Boolean;
    procedure SetModified(const Value: Boolean);
    function  GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function  GetText: string;
    procedure SetText(const Value: string);
    function  GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function  GetCaretXY: TPoint;
    procedure SetCaretXY(const Value: TPoint);
    function  GetTopLine: Integer;
    procedure SetTopLine(const Value: Integer);
    function  GetHighlighter: TGXSyntaxHighlighter;
    procedure SetHighlighter(const Value: TGXSyntaxHighlighter);
    function  GetSelText: string;
    procedure SetSelText(const Value: string);
    function  GetSelStart: Integer;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetWantTabs(const Value: Boolean);
    function  GetWantTabs: Boolean;
    function  GetNormalizedText: string;
    function GetTabWidth: Integer;
    procedure SetTabWidth(const Value: Integer);
    function GetLineCount: Integer;
    procedure SetAsAnsiString(const Value: AnsiString);
    procedure SetAsString(const Value: string);
    function GetAsAnsiString: AnsiString;
    function GetAsString: string;
    function GetAsUnicodeString: TGXUnicodeString;
    procedure SetAsUnicodeString(const Value: TGXUnicodeString);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnKeyDown(const Value: TKeyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressWEvent);
    procedure SetOnKeyUp(const Value: TKeyEvent);
    procedure SetOnMouseDown(const Value: TMouseEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipBoard;
    procedure SetSelection(AStart, ALength: Integer);
    procedure SetFocus; override;
    procedure Print(const ATitle: string);
    function Focused: Boolean; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const AFilename: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetLines(ALines: TGXUnicodeStringList);
    procedure SetLines(ALines: TGXUnicodeStringList);
    property Highlighter: TGXSyntaxHighlighter read GetHighlighter write SetHighlighter;
    property Text: string read GetText write SetText;
    property Color: TColor  read GetColor write SetColor;
    property CaretXY: TPoint  read GetCaretXY write SetCaretXY;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Modified: Boolean read GetModified write SetModified;
    property Font: TFont read GetFont write SetFont;
    property SelText: string read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart;
    property TopLine: Integer read GetTopLine write SetTopLine;
    property WantTabs: Boolean read GetWantTabs write SetWantTabs;
    property NormalizedText: string read GetNormalizedText;
    property TabWidth: Integer read GetTabWidth write SetTabWidth;
    property LineCount: Integer read GetLineCount;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsUnicodeString: TGXUnicodeString read GetAsUnicodeString write SetAsUnicodeString;
  published
    property Align;
    property Anchors;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnExit;
    property OnEnter;
    property OnKeyDown: TKeyEvent read FOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressWEvent read FOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write SetOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
  end;

procedure Register;

implementation

uses
 SysUtils, SynUnicode;

procedure Register;
begin
  RegisterComponents('GExperts', [TGxEnhancedEditor]);
end;

{ TGxEnhancedEditor }

constructor TGxEnhancedEditor.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    {$IFDEF SYNEDIT}
    FEditor := TSynMemo.Create(nil);
    FEditor.Gutter.Width := 0;
    FEditor.Options := FEditor.Options - [eoScrollPastEof, eoScrollPastEol, eoTabsToSpaces];
    (FEditor.Lines as TSynEditStringList).AppendNewLineAtEOF := False;
    {$ENDIF SYNEDIT}

    {$IFNDEF GX_ENHANCED_EDITOR}
    FEditor := TRichEdit.Create(Self);
    FEditor.ScrollBars := ssBoth;
    FEditor.PlainText := True;
    FEditor.WordWrap := False;
    {$ENDIF GX_ENHANCED_EDITOR}

    FEditor.Parent := Self;
    FEditor.Align := alClient;
    if Assigned(FOnChange) then
      FEditor.OnChange := FOnchange;
  end; // not csDesigning

  TabStop := True;
  Width := 185;
  Height := 89;
  AutoSize := False;
  ParentColor := False;
  Color := clWindow;
end;

destructor TGxEnhancedEditor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FEditor.OnChange) then
      FEditor.OnChange := nil;
    FreeAndNil(FEditor);
  end;
  inherited Destroy;
end;

procedure TGxEnhancedEditor.BeginUpdate;
begin
  FEditor.Lines.BeginUpdate;
end;

procedure TGxEnhancedEditor.EndUpdate;
begin
  FEditor.Lines.EndUpdate;
end;

procedure TGxEnhancedEditor.Clear;
begin
  {$IFDEF SYNEDIT}
  FEditor.ClearAll;
  {$ENDIF SYNEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  FEditor.Clear;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TGxEnhancedEditor.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  FEditor.OnChange := Value;
end;

procedure TGxEnhancedEditor.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
  FEditor.OnClick := Value;
end;

procedure TGxEnhancedEditor.SetOnKeyDown(const Value: TKeyEvent);
begin
  FOnKeyDown := Value;
  FEditor.OnKeyDown := Value;
end;

procedure TGxEnhancedEditor.SetOnKeyPress(const Value: TKeyPressWEvent);
begin
  FOnKeypress := Value;
  FEditor.OnKeyPress := Value;
end;

procedure TGxEnhancedEditor.SetOnKeyUp(const Value: TKeyEvent);
begin
  FOnKeyUp := Value;
  FEditor.OnKeyUp := Value;
end;

procedure TGxEnhancedEditor.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
  FEditor.OnMouseDown := Value;
end;

procedure TGxEnhancedEditor.SetFocus;
begin
  inherited SetFocus;
  FEditor.SetFocus;
end;

function TGxEnhancedEditor.Focused: Boolean;
begin
  Result := False;
  if FEditor.Focused then
    Result := True;
end;

procedure TGxEnhancedEditor.CopyToClipboard;
begin
  FEditor.CopyToClipboard;
end;

procedure TGxEnhancedEditor.CutToClipboard;
begin
  FEditor.CutToClipboard;
end;

procedure TGxEnhancedEditor.PasteFromClipBoard;
begin
  FEditor.PasteFromClipBoard;
end;

procedure TGxEnhancedEditor.Print(const ATitle: string);
{$IFNDEF GX_ENHANCED_EDITOR}
var
  PrintBuf: TextFile;
  i: Integer;
{$ENDIF GX_ENHANCED_EDITOR}
begin
  {$IFDEF SYNEDIT}
  // TODO: Implement SynEdit printing
  {$ENDIF SYNEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
    AssignPrn(PrintBuf);
    Rewrite(PrintBuf);
    try
      for i := 0 to FEditor.Lines.Count-1 do
        WriteLn(PrintBuf, FEditor.Lines[i]);
    finally
      CloseFile(PrintBuf);
    end;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

function TGxEnhancedEditor.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(FEditor.Lines.Text);
end;

function TGxEnhancedEditor.GetAsString: string;
begin
  Result := FEditor.Lines.Text;
end;

function TGxEnhancedEditor.GetAsUnicodeString: TGXUnicodeString;
begin
  Result := FEditor.Lines.Text;
end;

function TGxEnhancedEditor.GetCaretXY: TPoint;
begin
  Assert(False, 'Not implemented');
  Result := Point(0, 0);
end;

procedure TGxEnhancedEditor.SetAsAnsiString(const Value: AnsiString);
begin
  {$IFDEF SYNEDIT}
  FEditor.Lines.Text := UnicodeString(Value);
  {$ELSE}
  FEditor.Lines.Text := string(Value);
  {$ENDIF}
end;

procedure TGxEnhancedEditor.SetAsString(const Value: string);
begin
  FEditor.Lines.Text := Value;
end;

procedure TGxEnhancedEditor.SetAsUnicodeString(const Value: TGXUnicodeString);
begin
  FEditor.Lines.Text := Value;
end;

procedure TGxEnhancedEditor.SetCaretXY(const Value: TPoint);
begin
  {$IFDEF SYNEDIT}
  //FEditor.TopLine := Value.Y + 1;
  // This code requires SynEdit 1.2 or later, otherwise use this line:
  // FEditor.CaretXY := Point(1, Value.Y + 1);
  FEditor.CaretXY := BufferCoord(1, Value.Y + 1);
  {$ENDIF SYNEDIT}

  {$IFNDEF GX_ENHANCED_EDITOR}
  SendMessage(FEditor.Handle, EM_LINESCROLL, 0, -9999);
  SendMessage(FEditor.Handle, EM_LINESCROLL, 0, Value.y);
  {$ENDIF GX_ENHANCED_EDITOR}
end;

function TGxEnhancedEditor.GetTopLine: Integer;
begin
  {$IFDEF SYNEDIT}
  Result := FEditor.TopLine;
  {$ENDIF SYNEDIT}
  {$IFNDEF GX_ENHANCED_EDITOR}
  Result := 0; // TODO Implement
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TGxEnhancedEditor.SetTopLine(const Value: Integer);
begin
  {$IFDEF SYNEDIT}
  FEditor.TopLine := Value;
  {$ENDIF SYNEDIT}

  {$IFNDEF GX_ENHANCED_EDITOR}
  SendMessage(FEditor.Handle, EM_LINESCROLL, 0, -9999);
  SendMessage(FEditor.Handle, EM_LINESCROLL, 0, Value);
  {$ENDIF GX_ENHANCED_EDITOR}
end;

function TGxEnhancedEditor.GetModified: Boolean;
begin
  Result := FEditor.Modified
end;

procedure TGxEnhancedEditor.SetModified(const Value: Boolean);
begin
  FEditor.Modified := Value;
end;

function TGxEnhancedEditor.GetLineCount: Integer;
begin
  Result := FEditor.Lines.Count;
end;

procedure TGxEnhancedEditor.GetLines(ALines: TGXUnicodeStringList);
begin
  ALines.Assign(FEditor.Lines);
end;

function TGxEnhancedEditor.GetReadOnly: Boolean;
begin
  Result := FEditor.ReadOnly
end;

procedure TGxEnhancedEditor.SetReadOnly(const Value: Boolean);
begin
  FEditor.ReadOnly := Value;
end;

procedure TGxEnhancedEditor.SetWantTabs(const Value: Boolean);
begin
  FEditor.WantTabs := Value;
end;

function TGxEnhancedEditor.GetWantTabs: Boolean;
begin
  Result := FEditor.WantTabs;
end;

procedure TGxEnhancedEditor.LoadFromFile(const AFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TGxEnhancedEditor.LoadFromStream(Stream: TStream);
begin
  FEditor.Lines.LoadFromStream(Stream);
end;

function TGxEnhancedEditor.GetColor: TColor;
begin
  Result := FEditor.Color;
end;

procedure TGxEnhancedEditor.SetColor(const Value: TColor);
begin
  FEditor.Color := Value;
end;

function TGxEnhancedEditor.GetText: string;
begin
  Result := FEditor.Text;
end;

procedure TGxEnhancedEditor.SetText(const Value: string);
begin
  FEditor.Text := Value;
end;

function TGxEnhancedEditor.GetSelStart: Integer;
begin
  Result := FEditor.SelStart;
end;

procedure TGxEnhancedEditor.SetSelection(AStart, ALength: Integer);
begin
  {$IFDEF SYNEDIT}
  FEditor.SelStart := AStart;
  FEditor.SelEnd := AStart + ALength;
  {$ENDIF SYNEDIT}

  {$IFNDEF GX_ENHANCED_EDITOR}
  FEditor.Perform(EM_SETSEL, AStart, AStart + ALength);
  FEditor.Perform(EM_SCROLLCARET, 0, 0);
  {$ENDIF GX_ENHANCED_EDITOR}
end;

function TGxEnhancedEditor.GetFont: TFont;
begin
  Result := FEditor.Font;
end;

procedure TGxEnhancedEditor.SetFont(const Value: TFont);
begin
  FEditor.Font.Assign(Value);
end;

function TGxEnhancedEditor.GetSelText: string;
begin
  Result := FEditor.SelText;
end;

procedure TGxEnhancedEditor.SetSelText(const Value: string);
begin
  FEditor.SelText := Value;
end;

function TGxEnhancedEditor.GetHighlighter: TGXSyntaxHighlighter;
begin
  Result := FHighlighter;
end;

procedure TGxEnhancedEditor.SetHighlighter(const Value: TGXSyntaxHighlighter);
begin
  {$IFDEF SYNEDIT}
  SetSynEditHighlighter(FEditor, Value);
  FHighlighter := Value;
  {$ENDIF SYNEDIT}

  {$IFNDEF GX_ENHANCED_EDITOR}
  FHighlighter := gxpNone;
  {$ENDIF GX_ENHANCED_EDITOR}
end;

procedure TGxEnhancedEditor.SetLines(ALines: TGXUnicodeStringList);
begin
  FEditor.Lines.Assign(ALines);
end;

function TGxEnhancedEditor.GetNormalizedText: string;
begin
  Result := FEditor.Lines.Text;
  {$IFDEF SYNEDIT}
  // Workaround the string list always appending an extra CRLF
  RemoveLastEOL(Result);
  {$ENDIF SYNEDIT}
end;

function TGxEnhancedEditor.GetTabWidth: Integer;
begin
  {$IFDEF SYNEDIT}
  Result := FEditor.TabWidth;
  {$ELSE}
  Result := 0;
  {$ENDIF SYNEDIT}
end;

procedure TGxEnhancedEditor.SetTabWidth(const Value: Integer);
{$IFNDEF SYNEDIT}
var
  AvgWidth: Integer;
  DialogUnitsX: Longint;
  Units: Integer;
{$ENDIF}
begin
  {$IFDEF SYNEDIT}
  FEditor.TabWidth := Value;
  {$ELSE}
  DialogUnitsX := LoWord(GetDialogBaseUnits);
  AvgWidth := GetAverageCharWidth(FEditor);
  Units := Value * (AvgWidth * 4) div DialogUnitsX;
  SendMessage(FEditor.Handle, EM_SETTABSTOPS, WPARAM(1), LPARAM(@Units));
  FEditor.Refresh;
  {$ENDIF SYNEDIT}
end;

end.

