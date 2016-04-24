unit GX_PasteAs;

interface

uses
  Classes, GX_ConfigurationInfo, GX_EditorExpert;

type
  TPasteAsType = (paStringArray, paAdd, paSLineBreak,
    paChar10, paChar13, paChars1310, paCRLF, paCR_LF);

  TPasteAsHandler = class
  private
    FCreateQuotedString: Boolean;
    FPasteAsType: TPasteAsType;
    FAddExtraSpaceAtTheEnd: Boolean;
    FShowOptions: Boolean;
    function DetermineIndent(ALines: TStrings): Integer;
  protected
  public
    constructor Create;
    procedure LoadSettings(Settings: TExpertSettings);
    procedure SaveSettings(Settings: TExpertSettings);
    procedure ConvertToCode(ALines: TStrings; AOnlyUpdateLines: Boolean);
    procedure ExtractRawStrings(ALines: TStrings; ADoAddBaseIndent: Boolean);
    function  ExecuteConfig(AConfigExpert: TEditorExpert; ForceShow: Boolean): Boolean;
    class procedure GetTypeText(AList: TStrings);
    property CreateQuotedString: Boolean read FCreateQuotedString write FCreateQuotedString default True;
    property PasteAsType: TPasteAsType read FPasteAsType write FPasteAsType default paStringArray;
    property AddExtraSpaceAtTheEnd: Boolean read FAddExtraSpaceAtTheEnd write FAddExtraSpaceAtTheEnd default True;
    property ShowOptions: Boolean read FShowOptions write FShowOptions;
  end;

var
  PasteAsHandler: TPasteAsHandler;

implementation

uses
  Windows, SysUtils, StrUtils, Controls, GX_OtaUtils, GX_ePasteAs,
  GX_GenericUtils;

const
  cPasteAsTypeText: array[TPasteAsType] of String = (
    '%s,', 'Add(%s);', '%s + sLineBreak +',
    '%s + #10 +', '%s + #13 +', '%s + #13#10 +', '%s + CRLF +', '%s + CR_LF +');
  cStringSep = '''';

{ TPasteAsHandler }

constructor TPasteAsHandler.Create;
begin
  inherited Create;
  FCreateQuotedString := True;
  FPasteAsType := paStringArray;
  FAddExtraSpaceAtTheEnd := True;
end;

class procedure TPasteAsHandler.GetTypeText(AList: TStrings);
var
  AType: TPasteAsType;
begin
  for AType := Low(TPasteAsType) to High(TPasteAsType) do
    AList.AddObject(cPasteAsTypeText[AType], TObject(Integer(AType)));
end;

procedure TPasteAsHandler.LoadSettings(Settings: TExpertSettings);
begin
  PasteAsType := TPasteAsType(Settings.ReadEnumerated('PasteAsType', TypeInfo(TPasteAsType), Ord(paStringArray)));
  CreateQuotedString := Settings.ReadBool('CreateQuotedString', True);
  AddExtraSpaceAtTheEnd := Settings.ReadBool('AddExtraSpaceAtTheEnd', True);
  ShowOptions := Settings.ReadBool('ShowOptions', True);
end;

procedure TPasteAsHandler.SaveSettings(Settings: TExpertSettings);
begin
  Settings.WriteEnumerated('PasteAsType', TypeInfo(TPasteAsType), Ord(FPasteAsType));
  Settings.WriteBool('CreateQuotedString', FCreateQuotedString);
  Settings.WriteBool('AddExtraSpaceAtTheEnd', FAddExtraSpaceAtTheEnd);
  Settings.WriteBool('ShowOptions', FShowOptions);
end;

function TPasteAsHandler.DetermineIndent(ALines: TStrings): Integer;
var
  i: Integer;
  Line: string;
  FCP: Integer;
begin
  Result := MaxInt;
  for i := 0 to ALines.Count-1 do
  begin
    Line := ALines[i];
    FCP := GetFirstCharPos(Line, [' ', #09], False);
    if FCP < Result then
      Result := FCP;
  end;
end;

procedure TPasteAsHandler.ConvertToCode(ALines: TStrings; AOnlyUpdateLines: Boolean);
var
  I, FirstCharPos: Integer;
  ALine, BaseIndent, ALineStart, ALineEnd, ALineStartBase, AAddDot: String;
begin
  FirstCharPos := DetermineIndent(ALines);
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(ALines[0], FirstCharPos - 1);

  ALineStart := '';
  ALineEnd := '';
  ALineStartBase := '';
  AAddDot := '';
  case FPasteAsType of
    paStringArray: ALineEnd := ',';
    paAdd:
    begin
      if not AOnlyUpdateLines then
      begin
        ALineStartBase := Trim(GxOtaGetCurrentSelection(False));
        if (ALineStartBase <> '') and (ALineStartBase[Length(ALineStartBase)] <> '.') then
          AAddDot := '.';
        ALineStartBase := ALineStartBase + AAddDot;
      end;
      ALineStart := 'Add(';
      ALineEnd := ');';
    end;
    paSLineBreak: ALineEnd := ' + sLineBreak +';
    paChar10: ALineEnd := '#10 +';
    paChar13: ALineEnd := '#13 +';
    paChars1310: ALineEnd := '#13#10 +';
    paCRLF: ALineEnd := ' + CRLF +';
    paCR_LF: ALineEnd := ' + CR_LF +';
  end;

  for I := 0 to ALines.Count-1 do
  begin
    ALine := Copy(ALines[I], FirstCharPos, MaxInt);

    if FCreateQuotedString then
      ALine := AnsiQuotedStr(ALine + IfThen(FAddExtraSpaceAtTheEnd, ' '), cStringSep);

    ALine := ALineStart + ALine;
    if ALineStartBase <> '' then
      ALine := IfThen(I = 0, AAddDot, ALineStartBase) + ALine;
    if (I < ALines.Count-1) or (FPasteAsType = paAdd) then
      ALine := ALine + ALineEnd;

    ALines[I] := BaseIndent + ALine;

    if not AOnlyUpdateLines then
      GxOtaInsertLineIntoEditor(ALine + sLineBreak);
  end;
end;

procedure TPasteAsHandler.ExtractRawStrings(ALines: TStrings; ADoAddBaseIndent: Boolean);
var
  i, FirstCharPos, FirstQuotePos, LastQuotePos: Integer;
  Line, BaseIndent: String;
  sl: TStringList;
begin
  FirstCharPos := DetermineIndent(ALines);
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(ALines[0], FirstCharPos - 1);

  sl := TStringList.Create;
  try
    for i := 0 to ALines.Count-1 do
    begin
      Line := Trim(Copy(ALines[i], FirstCharPos, MaxInt));

      FirstQuotePos := GetFirstCharPos(Line, [cStringSep], True);
      LastQuotePos := GetLastCharPos(Line, [cStringSep], True);
      if (FirstQuotePos > 0) and (LastQuotePos > 0) then
      begin
        Line := Copy(Line, FirstQuotePos, LastQuotePos - FirstQuotePos + 1);
        // It's not "not FCreateQuotedString" because this is the ExtractRawStrings method
        // the ConvertToCode will add the quotes again, if FCreateQuotedString is true.
        if FCreateQuotedString then
          Line := AnsiDequotedStr(Line, cStringSep);
       sl.Add(IfThen(ADoAddBaseIndent, BaseIndent) + TrimRight(Line));
      end;
    end;

    ALines.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TPasteAsHandler.ExecuteConfig(AConfigExpert: TEditorExpert; ForceShow: Boolean): Boolean;
var
  Dlg: TfmPasteAsConfig;
begin
  Result := True;
  if not FShowOptions and not ForceShow then
    Exit;

  Dlg := TfmPasteAsConfig.Create(nil);
  try
    GetTypeText(Dlg.cbPasteAsType.Items);
    Dlg.cbPasteAsType.ItemIndex := Integer(PasteAsType);
    Dlg.chkCreateQuotedStrings.Checked := CreateQuotedString;
    Dlg.chkAddExtraSpaceAtTheEnd.Checked := AddExtraSpaceAtTheEnd;
    Dlg.chkShowOptions.Checked := ShowOptions;

    Result := Dlg.ShowModal = mrOk;
    if Result then
    begin
      PasteAsType := TPasteAsType(Dlg.cbPasteAsType.ItemIndex);
      CreateQuotedString := Dlg.chkCreateQuotedStrings.Checked;
      AddExtraSpaceAtTheEnd := Dlg.chkAddExtraSpaceAtTheEnd.Checked;
      ShowOptions := Dlg.chkShowOptions.Checked;

      if Assigned(AConfigExpert) then
        AConfigExpert.SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

initialization
  PasteAsHandler := TPasteAsHandler.Create;

finalization
  FreeAndNil(PasteAsHandler);

end.

