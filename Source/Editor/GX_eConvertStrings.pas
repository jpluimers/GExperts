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
  GX_BaseForm;

type
  TPasteAsType = (paRaw, paStringArray, paAdd, paSLineBreak,
    paChar10, paChar13, paChars1310, paCRLF, paCR_LF);

type
  TfmERawStrings = class(TfmBaseForm)
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
  private
    FUpdating: Boolean;
    procedure SetData(_sl: TStrings);
    procedure ConvertStrings;
    procedure ExtractRawStrings(_sl: TStrings; _AddBaseIndent: Boolean);
    function DetermineIndent(_sl: TStrings): Integer;
    procedure ConvertToCode(_sl: TStrings; _PasteAsType: TPasteAsType; _QuoteStrings: Boolean;
      _AppendSpace: Boolean; const _Prefix: string);
    procedure LoadSettings;
    procedure SaveSettings;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute(_Owner: TComponent; _sl: TStrings);
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
  protected
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
  end;

const
  SINGLE_QUOTE = '''';

const
  cPasteAsTypeText: array[TPasteAsType] of string = (
    '%s', '%s,', 'Add(%s);', '%s + sLineBreak +',
    '%s + #10 +', '%s + #13 +', '%s + #13#10 +', '%s + CRLF +', '%s + CR_LF +');

class procedure TfmERawStrings.Execute(_Owner: TComponent; _sl: TStrings);
var
  frm: TfmERawStrings;
begin
  frm := TfmERawStrings.Create(_Owner);
  try
    frm.SetData(_sl);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmERawStrings.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

destructor TfmERawStrings.Destroy;
begin
  try
    SaveSettings;
  except
    // we are being called in the destructor -> ignore any exceptions
  end;
  inherited;
end;

procedure TfmERawStrings.FormResize(Sender: TObject);
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

procedure TfmERawStrings.SaveSettings;
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
    Settings.WriteInteger('ConvertType', rg_ConvertType.ItemIndex);
    Settings.WriteBool('QuoteStrings', chk_QuoteStrings.Checked);
    Settings.WriteBool('AppendSpace', chk_AppendSpace.Checked);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GXSettings);
  end;
end;

procedure TfmERawStrings.SetData(_sl: TStrings);
var
  Prefix: string;
  p: Integer;
begin
  FUpdating := True;
  try
    m_Input.Lines.Assign(_sl);

    Prefix := Trim(GxOtaGetCurrentSelection(False));
    if Prefix = '' then
      Prefix := Trim(GxOtaGetCurrentLine);
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

procedure TfmERawStrings.LoadSettings;
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
    rg_ConvertType.ItemIndex := Settings.ReadInteger('ConvertType', Ord(paAdd));
    chk_QuoteStrings.Checked := Settings.ReadBool('QuoteStrings', True);
    chk_AppendSpace.Checked := Settings.ReadBool('AppendSpace', True);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GXSettings);
  end;
end;

function TfmERawStrings.DetermineIndent(_sl: TStrings): Integer;
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

procedure TfmERawStrings.ExtractRawStrings(_sl: TStrings; _AddBaseIndent: Boolean);
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
    Line := Trim(Copy(_sl[i], FirstCharPos, MaxInt));

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

procedure TfmERawStrings.ConvertToCode(_sl: TStrings; _PasteAsType: TPasteAsType; _QuoteStrings: Boolean;
  _AppendSpace: Boolean; const _Prefix: string);
var
  i, FirstCharPos: Integer;
  ALine, BaseIndent, ALineStart, ALineEnd, ALineStartBase, AAddDot: string;
begin
  FirstCharPos := DetermineIndent(_sl);
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
    ALine := Copy(_sl[i], FirstCharPos, MaxInt);

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

procedure TfmERawStrings.ConvertStrings;
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
      ExtractRawStrings(sl, True);
    if sl.Count > 0 then begin
      ConvertToCode(sl, PasteAsType, QuoteStrings, AppendSpace, ed_Prefix.Text);
    end;
    m_Output.Lines.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmERawStrings.m_InputChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.ed_PrefixChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.rg_ConvertTypeClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.chk_AppendSpaceClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.chk_ExtractRawClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.chk_QuoteStringsClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmERawStrings.b_CopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := m_Output.Lines.Text;
end;

procedure TfmERawStrings.b_InsertClick(Sender: TObject);
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

procedure TfmERawStrings.b_PasteFromClipboardClick(Sender: TObject);
begin
  m_Input.Lines.Text := Clipboard.AsText;
end;

procedure TfmERawStrings.b_ToTStringsClick(Sender: TObject);
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

procedure TfmERawStrings.b_ToSQLClick(Sender: TObject);
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

procedure TConvertStringsExpert.Configure;
begin
  inherited;

end;

procedure TConvertStringsExpert.Execute(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := GxOtaGetCurrentSelection(False);
    if sl.Count = 0 then
      sl.Text := Clipboard.AsText;
    TfmERawStrings.Execute(nil, sl);
  finally
    FreeAndNil(sl);
  end;
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
    'activate this expert.' + sLineBreak +
    '  You can configure this expert to use different string prefix/suffix combinations.';
begin
  Result := SConvertStringsHelp;
end;

class function TConvertStringsExpert.GetName: string;
begin
  Result := 'ConvertStrings';
end;

procedure TConvertStringsExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;

end;

procedure TConvertStringsExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;

end;

initialization
  RegisterEditorExpert(TConvertStringsExpert);
end.
