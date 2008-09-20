unit GX_SynMemoUtils;

{$I GX_CondDefine.inc}

interface

uses  // If you get errors here, edit GX_CondDefine.inc or add SynEdit to your library path
  {$IFDEF SYNEDIT} SynEdit, SynUnicode, {$ENDIF SYNEDIT}
  GX_GenericUtils;

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;

{$IFDEF SYNEDIT}
  procedure SetSynEditHighlighter(SynEdit: TCustomSynEdit; Highlighter: TGXSyntaxHighlighter);
{$ENDIF SYNEDIT}

const
  {$IFDEF GX_VER160_up}
  IDEEditorEncoding = seUTF8;
  {$ELSE}
  IDEEditorEncoding = seAnsi;
  {$ENDIF}

implementation

uses
  {$IFDEF SYNEDIT}
  SynEditHighlighter, SynHighlighterPas, SynHighlighterCpp, SynHighlighterHtml,
  SynHighlighterSql, SynHighlighterCS, SynHighlighterXML, SynHighlighterGeneral,
  GX_VerDepConst, Classes,
  {$ENDIF SYNEDIT}
  SysUtils, ToolsAPI, GX_OtaUtils;

{$IFDEF SYNEDIT}
procedure GxGetIDEHighLigherSettings(Highlighter: TSynCustomHighlighter; Prefer: string);
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
begin
  Assert(SynEdit <> nil);
  SynEdit.Highlighter.Free;
  SynEdit.Highlighter := nil;

  case Highlighter of
    gxpPAS:  SynEdit.Highlighter := TSynPasSyn.Create(SynEdit);
    gxpCPP:  SynEdit.Highlighter := TSynCppSyn.Create(SynEdit);
    gxpHTML: SynEdit.Highlighter := TSynHtmlSyn.Create(SynEdit);
    gxpSQL:  SynEdit.Highlighter := TSynSqlSyn.Create(SynEdit);
    gxpCS:   SynEdit.Highlighter := TSynCSSyn.Create(SynEdit);
    gxpXML:  SynEdit.Highlighter := TSynXMLSyn.Create(SynEdit);
  else
    SynEdit.Highlighter := TSynGeneralSyn.Create(SynEdit);
  end;
  if Highlighter in [gxpPAS, gxpCPP, gxpSQL, gxpCS] then
    GxGetIDEHighLigherSettings(SynEdit.Highlighter, GX_VerDepConst.MajorVersionNumberChar + '.0');
end;

{$ENDIF SYNEDIT}

function GetGXHighlighterForCurrentSourceEditor: TGXSyntaxHighlighter;
var
  SourceEditor: IOTASourceEditor;
begin
  Result := gxpNone;

  SourceEditor := GxOtaGetCurrentSourceEditor;
  if SourceEditor = nil then
    Exit;

  Result := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
end;

end.

