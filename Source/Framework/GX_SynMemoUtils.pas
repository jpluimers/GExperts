unit GX_SynMemoUtils;

{$I GX_CondDefine.inc}

interface

uses  // If you get errors here, edit GX_CondDefine.inc or add SynEdit to your library path
  {$IFDEF SYNEDIT} SynEdit, SynUnicode, {$ENDIF SYNEDIT}
  GX_GenericUtils, GX_IDEUtils;

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;

{$IFDEF SYNEDIT}
  procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);
{$ENDIF SYNEDIT}

implementation

uses
  {$IFDEF SYNEDIT}
  SynEditHighlighter, SynHighlighterPas, SynHighlighterCpp, SynHighlighterHtml,
  SynHighlighterSql, SynHighlighterCS, SynHighlighterXML, SynHighlighterGeneral,
  GX_VerDepConst, Classes,
  {$ENDIF SYNEDIT}
  SysUtils, ToolsAPI, GX_OtaUtils, Graphics;

{$IFDEF SYNEDIT}
procedure GxGetIDEHighLigherSettings(Highlighter: TSynCustomHighlighter; const Prefer: string);
var
  Elements: TStrings;
  PreferIndex: Integer;
begin
  Elements := TStringList.Create;
  try
    Highlighter.EnumUserSettings(Elements);
    PreferIndex := Elements.IndexOf(Prefer);
    if PreferIndex > -1 then
      Highlighter.UseUserSettings(PreferIndex)
    else if Elements.Count > 0 then
      Highlighter.UseUserSettings(Elements.Count - 1);
  finally
    FreeAndNil(Elements);
  end;
end;

procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);
var
  i: Integer;
begin
  Assert(SynEdit <> nil);
  SynEdit.Highlighter.Free;
  SynEdit.Highlighter := nil;

  case Highlighter of
    gxpPAS:  SynEdit.Highlighter := TSynPasSyn.Create(SynEdit);
    gxpCPP:  SynEdit.Highlighter := TSynCppSyn.Create(SynEdit);
    gxpHTML: SynEdit.Highlighter := TSynHtmlSyn.Create(SynEdit);
    gxpCS:   SynEdit.Highlighter := TSynCSSyn.Create(SynEdit);
    gxpXML:  SynEdit.Highlighter := TSynXMLSyn.Create(SynEdit);
    gxpSQL:
      begin 
        SynEdit.Highlighter := TSynSqlSyn.Create(SynEdit);
        for i := 0 to SynEdit.Highlighter.AttrCount - 1 do
        begin
          if SynEdit.Highlighter.Attribute[i].Name = 'Number' then
            SynEdit.Highlighter.Attribute[i].Foreground := clMaroon
          else if SynEdit.Highlighter.Attribute[i].Name = 'String' then
            SynEdit.Highlighter.Attribute[i].Foreground := clTeal
          else if SynEdit.Highlighter.Attribute[i].Name = 'Comment' then
          begin
            SynEdit.Highlighter.Attribute[i].Foreground := clBlue;
            SynEdit.Highlighter.Attribute[i].Style := SynEdit.Highlighter.Attribute[i].Style - [fsItalic];
          end;
        end;
      end;
  else
    SynEdit.Highlighter := TSynGeneralSyn.Create(SynEdit);
  end;
  if Highlighter in [gxpPAS, gxpCPP, gxpSQL, gxpCS] then
    GxGetIDEHighLigherSettings(SynEdit.Highlighter, GetIDEVersionID);
end;

{$ENDIF SYNEDIT}

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;
var
  SourceEditor: IOTASourceEditor;
begin
  Result := gxpNone;

  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;

  Result := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
end;

end.

