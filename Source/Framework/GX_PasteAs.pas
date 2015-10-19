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
  protected
  public
    constructor Create;
    procedure LoadSettings(Settings: TGExpertsSettings; AConfigKey: String);
    procedure SaveSettings(Settings: TGExpertsSettings; AConfigKey: String);
    procedure ConvertToString(ALines: TStrings; AOnlyUpdateLines: Boolean);
    function  ConvertFromString(ALines: TStrings; ADoAddBaseIndent: Boolean): String;
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

procedure TPasteAsHandler.LoadSettings(Settings: TGExpertsSettings; AConfigKey: String);
begin
  PasteAsType := TPasteAsType(Settings.ReadEnumerated(AConfigKey, 'PasteAsType', TypeInfo(TPasteAsType), Ord(paStringArray)));
  CreateQuotedString := Settings.ReadBool(AConfigKey, 'CreateQuotedString', True);
  AddExtraSpaceAtTheEnd := Settings.ReadBool(AConfigKey, 'AddExtraSpaceAtTheEnd', True);
  ShowOptions := Settings.ReadBool(AConfigKey, 'ShowOptions', True);
end;

procedure TPasteAsHandler.SaveSettings(Settings: TGExpertsSettings; AConfigKey: String);
begin
  Settings.WriteEnumerated(AConfigKey, 'PasteAsType', TypeInfo(TPasteAsType), Ord(FPasteAsType));
  Settings.WriteBool(AConfigKey, 'CreateQuotedString', FCreateQuotedString);
  Settings.WriteBool(AConfigKey, 'AddExtraSpaceAtTheEnd', FAddExtraSpaceAtTheEnd);
  Settings.WriteBool(AConfigKey, 'ShowOptions', FShowOptions);
end;

procedure TPasteAsHandler.ConvertToString(ALines: TStrings; AOnlyUpdateLines: Boolean);
var
  I, FirstCharPos: Integer;
  ALine, BaseIndent, ALineStart, ALineEnd, ALineStartBase, AAddDot: String;
begin
  ALine := ALines[0];
  FirstCharPos := GetFirstCharPos(ALine, [' ', #09], False);
  BaseIndent := LeftStr(ALine, FirstCharPos - 1);

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

function TPasteAsHandler.ConvertFromString(ALines: TStrings; ADoAddBaseIndent: Boolean): String;
var
  I, FirstCharPos, FirstString, LastString: Integer;
  ALine, BaseIndent: String;
begin
  ALine := ALines[0];
  FirstCharPos := GetFirstCharPos(ALine, [' ', #09], False);
  BaseIndent := LeftStr(ALine, FirstCharPos - 1);

  for I := 0 to ALines.Count-1 do
  begin
    ALine := Trim(Copy(ALines[I], FirstCharPos, MaxInt));

    FirstString := GetFirstCharPos(ALine, [cStringSep], True);
    LastString := GetLastCharPos(ALine, [cStringSep], True);
    ALine := AnsiDequotedStr( Copy(ALine, FirstString, LastString - FirstString + 1), cStringSep );

    ALines[I] := IfThen(ADoAddBaseIndent, BaseIndent) + TrimRight(ALine);
  end;

  Result := ALines.Text;
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
    PasteAsHandler.GetTypeText(Dlg.cbPasteAsType.Items);
    Dlg.cbPasteAsType.ItemIndex := Integer(PasteAsHandler.PasteAsType);
    Dlg.chkCreateQuotedStrings.Checked := PasteAsHandler.CreateQuotedString;
    Dlg.chkAddExtraSpaceAtTheEnd.Checked := PasteAsHandler.AddExtraSpaceAtTheEnd;
    Dlg.chkShowOptions.Checked := PasteAsHandler.ShowOptions;

    Result := Dlg.ShowModal = mrOk;
    if Result then
    begin
      PasteAsHandler.PasteAsType := TPasteAsType(Dlg.cbPasteAsType.ItemIndex);
      PasteAsHandler.CreateQuotedString := Dlg.chkCreateQuotedStrings.Checked;
      PasteAsHandler.AddExtraSpaceAtTheEnd := Dlg.chkAddExtraSpaceAtTheEnd.Checked;
      PasteAsHandler.ShowOptions := Dlg.chkShowOptions.Checked;

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

