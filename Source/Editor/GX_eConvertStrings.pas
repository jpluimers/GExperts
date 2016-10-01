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
    m_InputStrings: TMemo;
    l_InputStrings: TLabel;
    m_OutputStrings: TMemo;
    l_ConvertedStrings: TLabel;
    chk_QuoteStrings: TCheckBox;
    chk_AppendSpace: TCheckBox;
    b_CopyConverted: TButton;
    b_InsertConverted: TButton;
    b_Cancel: TButton;
    chk_ExtractRaw: TCheckBox;
    rg_ConvertType: TRadioGroup;
    l_Prefix: TLabel;
    ed_Prefix: TEdit;
    procedure chk_ExtractRawClick(Sender: TObject);
    procedure rg_ConvertTypeClick(Sender: TObject);
    procedure b_CopyConvertedClick(Sender: TObject);
    procedure ed_PrefixChange(Sender: TObject);
    procedure m_InputStringsChange(Sender: TObject);
    procedure b_InsertConvertedClick(Sender: TObject);
    procedure chk_QuoteStringsClick(Sender: TObject);
    procedure chk_AppendSpaceClick(Sender: TObject);
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
    function ConfigurationKey: string;
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
var
  ALineStartBase: string;
  p: Integer;
begin
  inherited;
  TControl_SetMinConstraints(Self);

  FUpdating := True;
  try
    ALineStartBase := Trim(GxOtaGetCurrentSelection(False));
    // multiple lines? Only take the first
    p := Pos(CR, ALineStartBase);
    if p > 0 then
      ALineStartBase := LeftStr(ALineStartBase, p - 1);
    if ALineStartBase <> '' then begin
      // Does it contain a '('? -> cut it there, we don't want parameters
      p := Pos('(', ALineStartBase);
      if p > 0 then
        ALineStartBase := LeftStr(ALineStartBase, p - 1);
      // now look up the last '.', that's where we append the .Add()
      p := LastDelimiter('.', ALineStartBase);
      if p > 0 then
        ALineStartBase := LeftStr(ALineStartBase, p)
      else begin
        // no '.'? -> add one
        ALineStartBase := ALineStartBase + '.';
      end;
      ed_Prefix.Text := ALineStartBase;
    end;

    LoadSettings;
  finally
    FUpdating := False;
  end;
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

function TfmERawStrings.ConfigurationKey: string;
begin
  Result := TConvertStringsExpert.ConfigurationKey;
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
    Settings := TExpertSettings.Create(GXSettings, ConfigurationKey);
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
begin

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
    Settings := TExpertSettings.Create(GXSettings, ConfigurationKey);
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

    sl.Assign(m_InputStrings.Lines);
    if chk_ExtractRaw.Checked then
      ExtractRawStrings(sl, True);
    if sl.Count > 0 then begin
      ConvertToCode(sl, PasteAsType, QuoteStrings, AppendSpace, ed_Prefix.Text);
    end;
    m_OutputStrings.Lines.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmERawStrings.m_InputStringsChange(Sender: TObject);
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

procedure TfmERawStrings.b_CopyConvertedClick(Sender: TObject);
begin
  Clipboard.AsText := m_OutputStrings.Lines.Text;
  ModalResult := mrOk;
end;

procedure TfmERawStrings.b_InsertConvertedClick(Sender: TObject);
var
  i: Integer;
  Lines: TStrings;
begin
  Lines := m_OutputStrings.Lines;
  for i := 0 to Lines.Count - 1 do begin
    GxOtaInsertLineIntoEditor(Lines[i] + sLineBreak);
  end;
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
  SConvertStringsName = 'Convert Raw Strings';
begin
  Result := SConvertStringsName;
end;

function TConvertStringsExpert.GetHelpString: string;
resourcestring
  SConvertStringsHelp =
    '  This expert takes the selected code lines (or the text on the clipboard, ' +
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

end.
