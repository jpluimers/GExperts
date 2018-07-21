unit GX_eConvertStrings;

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
  ExtCtrls,
  GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TPasteAsType = (paRaw, paStringArray, paAdd, paSLineBreak,
    paChar10, paChar13, paChars1310, paCRLF, paCR_LF);

type
  TfmEConvertStrings = class(TfmBaseForm)
    m_Input: TMemo;
    l_Input: TLabel;
    m_Output: TMemo;
    l_Output: TLabel;
    chk_QuoteStrings: TCheckBox;
    chk_AppendSpace: TCheckBox;
    b_CopyToClipboard: TButton;
    b_Insert: TButton;
    b_Close: TButton;
    chk_ExtractRaw: TCheckBox;
    rg_ConvertType: TRadioGroup;
    l_Prefix: TLabel;
    ed_Prefix: TEdit;
    b_PasteFromClipboard: TButton;
    b_ToSQL: TButton;
    b_ToTStrings: TButton;
    chk_TrimLeft: TCheckBox;
    chk_TrimRight: TCheckBox;
    chk_Indent: TCheckBox;
    procedure chk_ExtractRawClick(Sender: TObject);
    procedure rg_ConvertTypeClick(Sender: TObject);
    procedure b_CopyToClipboardClick(Sender: TObject);
    procedure ed_PrefixChange(Sender: TObject);
    procedure m_InputChange(Sender: TObject);
    procedure b_InsertClick(Sender: TObject);
    procedure chk_QuoteStringsClick(Sender: TObject);
    procedure chk_AppendSpaceClick(Sender: TObject);
    procedure b_PasteFromClipboardClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure b_ToSQLClick(Sender: TObject);
    procedure b_ToTStringsClick(Sender: TObject);
    procedure chk_TrimLeftClick(Sender: TObject);
    procedure chk_TrimRightClick(Sender: TObject);
    procedure chk_IndentClick(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure SetData(_sl: TStrings);
    procedure ConvertStrings;
    procedure ExtractRawStrings(_sl: TStrings; _AddBaseIndent, _TrimLeft, _TrimRight: Boolean);
    function DetermineIndent(_sl: TStrings): Integer;
    procedure ConvertToCode(_sl: TStrings; _Indent: Boolean; _PasteAsType: TPasteAsType;
      _QuoteStrings: Boolean; _AppendSpace: Boolean; const _Prefix: string);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure TrimStrings(_sl: TStrings; _TrimLeft, _TrimRight: Boolean);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute(_bmp: TBitmap; _sl: TStrings);
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  Clipbrd,
  GX_dzVclUtils,
  GX_GenericUtils,
  GX_OtaUtils,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  ToolsAPI;

type
  TConvertStringsExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    // Returns false
    function HasConfigOptions: Boolean; override;
  end;

const
  SINGLE_QUOTE = '''';

const
  cPasteAsTypeText: array[TPasteAsType] of string = (
    '%s', '%s,', 'Add(%s);', '%s + sLineBreak +',
    '%s + #10 +', '%s + #13 +', '%s + #13#10 +', '%s + CRLF +', '%s + CR_LF +');

class procedure TfmEConvertStrings.Execute(_bmp: TBitmap; _sl: TStrings);
var
  frm: TfmEConvertStrings;
begin
  frm := TfmEConvertStrings.Create(Application);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.SetData(_sl);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmEConvertStrings.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

destructor TfmEConvertStrings.Destroy;
begin
  try
    SaveSettings;
  except
    // we are being called in the destructor -> ignore any exceptions
  end;
  inherited;
end;

procedure TfmEConvertStrings.FormResize(Sender: TObject);
var
  cw: Integer;
  w: Integer;
  x: Integer;
  m: Integer;
begin
  m := m_Input.Left;
  cw := ClientWidth;
  x := (cw - rg_ConvertType.Width) div 2;
  chk_ExtractRaw.Left := x;
  chk_TrimLeft.Left := x;
  chk_TrimRight.Left := x;
  chk_Indent.Left := X;
  rg_ConvertType.Left := x;
  chk_QuoteStrings.Left := x;
  chk_AppendSpace.Left := x;
  l_Prefix.Left := x;
  ed_Prefix.Left := x;
  b_ToSQL.Left := x;
  b_ToTStrings.Left := x + b_ToSQL.Width + m;

  w := x - 2 * m;
  m_Input.Width := w;
  m_Output.Width := w;

  x := cw - w - m;
  l_Output.Left := x;
  m_Output.Left := x;
end;

procedure TfmEConvertStrings.SaveSettings;
var
  GXSettings: TGExpertsSettings;
  Settings: TExpertSettings;
begin
  // Do not localize any of the following lines.
  Settings := nil;
  GXSettings := TGExpertsSettings.Create;
  try
    Settings := TExpertSettings.Create(GXSettings, TConvertStringsExpert.ConfigurationKey);
    Settings.SaveForm('Window', Self);
    Settings.WriteBool('ExtractRaw', chk_ExtractRaw.Checked);
    Settings.WriteBool('TrimLeft', chk_TrimLeft.Checked);
    Settings.WriteBool('TrimRight', chk_TrimRight.Checked);
    Settings.WriteInteger('ConvertType', rg_ConvertType.ItemIndex);
    Settings.WriteBool('QuoteStrings', chk_QuoteStrings.Checked);
    Settings.WriteBool('AppendSpace', chk_AppendSpace.Checked);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GXSettings);
  end;
end;

procedure TfmEConvertStrings.SetData(_sl: TStrings);
var
  Prefix: string;
  p: Integer;
begin
  FUpdating := True;
  try
    m_Input.Lines.Assign(_sl);

    Prefix := Trim(GxOtaGetCurrentSelection(False));
    if Prefix = '' then begin
      Prefix := Trim(GxOtaGetCurrentLine);
      GxOtaSelectCurrentLine(GxOtaGetCurrentSourceEditor);
    end;
    // multiple lines? Only take the first
    p := Pos(CR, Prefix);
    if p > 0 then
      Prefix := LeftStr(Prefix, p - 1);
    if Prefix <> '' then begin
      // Does it contain a '('? -> cut it there, we don't want parameters
      p := Pos('(', Prefix);
      if p > 0 then
        Prefix := LeftStr(Prefix, p - 1);
      // now look up the last '.', that's where we append the .Add()
      p := LastDelimiter('.', Prefix);
      if p > 0 then
        Prefix := LeftStr(Prefix, p)
      else begin
        // no '.'? -> add one
        Prefix := Prefix + '.';
      end;
      ed_Prefix.Text := Prefix;
    end;

    LoadSettings;
  finally
    FUpdating := False;
  end;
  ConvertStrings;
end;

procedure TfmEConvertStrings.LoadSettings;
var
  GXSettings: TGExpertsSettings;
  Settings: TExpertSettings;
begin
  // Do not localize any of the following lines.
  Settings := nil;
  GXSettings := TGExpertsSettings.Create;
  try
    Settings := TExpertSettings.Create(GXSettings, TConvertStringsExpert.ConfigurationKey);
    Settings.LoadForm('Window', Self);
    chk_ExtractRaw.Checked := Settings.ReadBool('ExtractRaw', True);
    chk_TrimLeft.Checked := Settings.ReadBool('TrimLeft', True);
    chk_TrimRight.Checked := Settings.ReadBool('TrimRight', True);
    rg_ConvertType.ItemIndex := Settings.ReadInteger('ConvertType', Ord(paAdd));
    chk_QuoteStrings.Checked := Settings.ReadBool('QuoteStrings', True);
    chk_AppendSpace.Checked := Settings.ReadBool('AppendSpace', True);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GXSettings);
  end;
end;

function TfmEConvertStrings.DetermineIndent(_sl: TStrings): Integer;
var
  i: Integer;
  Line: string;
  FCP: Integer;
begin
  Result := MaxInt;
  for i := 0 to _sl.Count - 1 do begin
    Line := _sl[i];
    FCP := GetFirstCharPos(Line, [' ', #09], False);
    if FCP < Result then
      Result := FCP;
  end;
end;

procedure TfmEConvertStrings.ExtractRawStrings(_sl: TStrings; _AddBaseIndent, _TrimLeft, _TrimRight: Boolean);
var
  i, FirstCharPos, FirstQuotePos, LastQuotePos: Integer;
  Line, BaseIndent: string;
begin
  if _sl.Count = 0 then
    Exit;

  FirstCharPos := DetermineIndent(_sl);
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(_sl[0], FirstCharPos - 1);

  for i := 0 to _sl.Count - 1 do begin
    Line := Copy(_sl[i], FirstCharPos);
    if _TrimLeft then
      Line := TrimLeft(Line);
    if _TrimRight then
      Line := TrimRight(Line);

    FirstQuotePos := GetFirstCharPos(Line, [SINGLE_QUOTE], True);
    LastQuotePos := GetLastCharPos(Line, [SINGLE_QUOTE], True);
    if (FirstQuotePos > 0) and (LastQuotePos > 0) then begin
      Line := Copy(Line, FirstQuotePos, LastQuotePos - FirstQuotePos + 1);
      Line := AnsiDequotedStr(Line, SINGLE_QUOTE);
      Line := TrimRight(Line);
      if _AddBaseIndent then
        Line := BaseIndent + Line;
      _sl[i] := Line;
    end;
  end;
end;

procedure TfmEConvertStrings.ConvertToCode(_sl: TStrings; _Indent: Boolean; _PasteAsType: TPasteAsType;
  _QuoteStrings: Boolean; _AppendSpace: Boolean; const _Prefix: string);
var
  i, FirstCharPos: Integer;
  ALine, BaseIndent, ALineStart, ALineEnd, ALineStartBase, AAddDot: string;
begin
  if _Indent then
    FirstCharPos := DetermineIndent(_sl)
  else
    FirstCharPos := 1;
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(_sl[0], FirstCharPos - 1);

  ALineStart := '';
  ALineEnd := '';
  ALineStartBase := '';
  AAddDot := '';
  case _PasteAsType of
    paRaw: ; // no change
    paStringArray: ALineEnd := ',';
    paAdd: begin
        ALineStart := _Prefix + 'Add(';
        ALineEnd := ');';
      end;
    paSLineBreak: ALineEnd := ' + sLineBreak +';
    paChar10: ALineEnd := '#10 +';
    paChar13: ALineEnd := '#13 +';
    paChars1310: ALineEnd := '#13#10 +';
    paCRLF: ALineEnd := ' + CRLF +';
    paCR_LF: ALineEnd := ' + CR_LF +';
  end;

  for i := 0 to _sl.Count - 1 do begin
    ALine := Copy(_sl[i], FirstCharPos);

    if _QuoteStrings then
      ALine := AnsiQuotedStr(ALine + IfThen(_AppendSpace, ' '), SINGLE_QUOTE);

    ALine := ALineStart + ALine;
    if ALineStartBase <> '' then
      ALine := IfThen(i = 0, AAddDot, ALineStartBase) + ALine;
    if (i < _sl.Count - 1) or (_PasteAsType = paAdd) then
      ALine := ALine + ALineEnd;

    _sl[i] := BaseIndent + ALine;
  end;
end;

procedure TfmEConvertStrings.TrimStrings(_sl: TStrings; _TrimLeft, _TrimRight: Boolean);
var
  i: Integer;
  s: string;
begin
  if not _TrimLeft and not _TrimRight then
    Exit; //==>

  for i := 0 to _sl.Count - 1 do begin
    s := _sl[i];
    if _TrimLeft then begin
      if _TrimRight then
        s := Trim(s)
      else
        s := TrimLeft(s);
    end else begin
      if _TrimRight then
        s := TrimRight(s);
    end;
    _sl[i] := s;
  end;
end;

procedure TfmEConvertStrings.ConvertStrings;
var
  sl: TStrings;
  PasteAsType: TPasteAsType;
  QuoteStrings: Boolean;
  AppendSpace: Boolean;
begin
  if FUpdating then
    Exit;

  sl := TStringList.Create;
  try
    PasteAsType := TPasteAsType(rg_ConvertType.ItemIndex);
    QuoteStrings := chk_QuoteStrings.Checked;
    AppendSpace := chk_AppendSpace.Checked;

    sl.Assign(m_Input.Lines);
    if chk_ExtractRaw.Checked then
      ExtractRawStrings(sl, chk_Indent.Checked, chk_TrimLeft.Checked, chk_TrimRight.Checked)
    else
      TrimStrings(sl, chk_TrimLeft.Checked, chk_TrimRight.Checked);
    if sl.Count > 0 then begin
      ConvertToCode(sl, chk_Indent.Checked, PasteAsType, QuoteStrings, AppendSpace, ed_Prefix.Text);
    end;
    m_Output.Lines.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmEConvertStrings.m_InputChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.ed_PrefixChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.rg_ConvertTypeClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_AppendSpaceClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_ExtractRawClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_IndentClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_QuoteStringsClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_TrimLeftClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_TrimRightClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.b_CopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := m_Output.Lines.Text;
end;

procedure TfmEConvertStrings.b_InsertClick(Sender: TObject);
var
  i: Integer;
  Lines: TStrings;
begin
  Lines := m_Output.Lines;
  for i := 0 to Lines.Count - 1 do begin
    GxOtaInsertLineIntoEditor(Lines[i] + sLineBreak);
  end;
  ModalResult := mrOk;
end;

procedure TfmEConvertStrings.b_PasteFromClipboardClick(Sender: TObject);
begin
  m_Input.Lines.Text := Clipboard.AsText;
end;

procedure TfmEConvertStrings.b_ToTStringsClick(Sender: TObject);
begin
  FUpdating := True;
  try
    chk_ExtractRaw.Checked := True;
    rg_ConvertType.ItemIndex := Integer(paAdd);
    chk_QuoteStrings.Checked := True;
    chk_AppendSpace.Checked := True;
  finally
    FUpdating := False;
  end;
  ConvertStrings;
end;

procedure TfmEConvertStrings.b_ToSQLClick(Sender: TObject);
begin
  FUpdating := True;
  try
    chk_ExtractRaw.Checked := True;
    rg_ConvertType.ItemIndex := Integer(paRaw);
    chk_QuoteStrings.Checked := False;
    chk_AppendSpace.Checked := False;
  finally
    FUpdating := False;
  end;
  ConvertStrings;
end;

{ TConvertStringsExpert }

procedure TConvertStringsExpert.Execute(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := GxOtaGetCurrentSelection(False);
    if sl.Count = 0 then
      sl.Text := Clipboard.AsText;
    TfmEConvertStrings.Execute(GetBitmap, sl);
  finally
    FreeAndNil(sl);
  end;
  IncCallCount;
end;

function TConvertStringsExpert.GetDisplayName: string;
resourcestring
  SConvertStringsName = 'Convert Strings';
begin
  Result := SConvertStringsName;
end;

function TConvertStringsExpert.GetHelpString: string;
resourcestring
  SConvertStringsHelp =
    '  This expert takes the selected code lines (or the text on the clipboard), ' +
    'optionally removes the strings that are used to make them proper Delphi code, ' +
    'leaving you with just the raw strings.' + sLineBreak +
    '  It then uses the selected string prefix/suffix combination to create new strings, ' +
    'that can be pasted back the editor or copied to the clipboard.' + sLineBreak +
    '  To use it, select the string constants in the Delphi editor and ' +
    'activate this expert.';
begin
  Result := SConvertStringsHelp;
end;

class function TConvertStringsExpert.GetName: string;
begin
  Result := 'ConvertStrings';
end;

function TConvertStringsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterEditorExpert(TConvertStringsExpert);
end.
