// the actual code formatter expert, called either from GX_Formatter or GX_eCodeFormatter
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
// Contributors:
// * Ulrich Gerhardt - 2007-08-11: added determining the settings via an external .ini file
// * Ulrich Gerhardt - 2009-09-06: support for stand alone formatter
unit GX_CodeFormatterExpert;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  SysUtils,
  GX_GenericUtils,
  GX_ConfigurationInfo,
  GX_CodeFormatterEngine;

type
  TCodeFormatterExpert = class
  private
    FEngine: TCodeFormatterEngine;
    function GetSettingsName(const AFileName: string; FullText: TGXUnicodeStringList): string;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Configure;
    procedure Execute;
    procedure InternalLoadSettings(Settings: TExpertSettings);
    procedure InternalSaveSettings(Settings: TExpertSettings);
    function FormatFile(const _FileName: string): Boolean;
    procedure AddToCapitalization(const _Identifier: TGXUnicodeString);
  end;

implementation

uses
  IniFiles,
  ToolsAPI,
  GX_OtaUtils,
  GX_DbugIntf,
  GX_CodeFormatterGXConfigWrapper,
  GX_CodeFormatterTypes,
  GX_CodeFormatterConfig,
  GX_CodeFormatterBookmarks,
  GX_CodeFormatterBreakpoints,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterSettings,
  GX_MessageBox;

procedure XSendDebug(const Msg: string);
begin //FI:W519
{$IFOPT D+}SendDebug('GXFormatter: ' + Msg);
{$ENDIF}
end;

{ TCodeFormatterExpert }

procedure TCodeFormatterExpert.AddToCapitalization(const _Identifier: TGXUnicodeString);
var
  Idx: Integer;
  sl: TStringList;
begin
  sl := FEngine.Settings.CapNames;
  if sl.Find(_Identifier, Idx) then
    sl.Delete(Idx);
  sl.Add(_Identifier);
end;

procedure TCodeFormatterExpert.Configure;
begin
  TfmCodeFormatterConfig.Execute(FEngine.Settings);
end;

constructor TCodeFormatterExpert.Create;
var
  Settings: TCodeFormatterEngineSettings;
begin
  inherited Create;

  Settings := BorlandDefaults;
  FEngine := TCodeFormatterEngine.Create;
  FEngine.Settings.Settings := Settings;
end;

destructor TCodeFormatterExpert.Destroy;
begin
  FreeAndNil(FEngine);
  inherited;
end;

function FileNameMatches(const FileName, Pattern: string): Boolean;
var
  sr: TSearchRec;
begin
  Result := False;
  if FindFirst(Pattern, 0, sr) = 0 then begin
    repeat
      if SameText(FileName, sr.Name) then begin
        Result := True;
        Break;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function TCodeFormatterExpert.GetSettingsName(const AFileName: string; FullText: TGXUnicodeStringList): string;

  function GetFromConfigFile: string;
  var
    ConfigFileName, ConfiguredFileName, Path: string;
    IniFile: TIniFile;
    ConfiguredFileNames: TStringList;
    i: Integer;
  begin
    Result := '';
    Path := AddSlash(ExtractFilePath(ExpandFileName(AFileName)));
    ConfigFileName := Path + 'GXFormatter.ini'; // Do not localize.
    IniFile := TIniFile.Create(ConfigFileName);
    try
      ConfiguredFileNames := TStringList.Create;
      try
        // Read section manually to preserve order:
        IniFile.ReadSection('FileSettings', ConfiguredFileNames);
        for i := 0 to Pred(ConfiguredFileNames.Count) do begin
          ConfiguredFileName := ConfiguredFileNames[i];
          if FileNameMatches(ExtractFileName(AFileName), Path + ConfiguredFileName) then begin
            Result := IniFile.ReadString('FileSettings', ConfiguredFileName, '');
            XSendDebug(Format('Settings "%s" from rule %s in %s', [Result, ConfiguredFileName, ConfigFileName]));
            Break;
          end;
        end;
      finally
        FreeAndNil(ConfiguredFileNames);
      end;
    finally
      FreeAndNil(IniFile);
    end;
  end;

  function GetFromSource: string;
  const
    cConfigDirective = '{GXFormatter.config='; // Do not localize.
  var
    FirstLine: string;
  begin
    FirstLine := Trim(FullText[0]);
    if SameText(Copy(FirstLine, 1, Length(cConfigDirective)), cConfigDirective) and
      (FirstLine[Length(FirstLine)] = '}') then begin
      Result := Trim(Copy(FirstLine, Length(cConfigDirective) + 1, Length(FirstLine) - Length(cConfigDirective) - 1));
      XSendDebug(Format('Settings "%s" from in-source directive', [Result]));
    end else
      Result := '';
  end;

  function GetSettingNameFor(_Precedence: TConfigPrecedenceEnum; out _Name: string): Boolean;
  begin
    case _Precedence of
      cpDirective: begin
          _Name := GetFromSource;
          Result := _Name <> '';
        end;
      cpIniFile: begin
          _Name := GetFromConfigFile;
          Result := _Name <> '';
        end;
      cpMyConfig: begin
          _Name := '';
          Result := True;
        end else
      Result := False;
    end;
  end;

var
  i: Integer;
begin
  for i := Low(TOneToThree) to High(TOneToThree) do
    if GetSettingNameFor(FEngine.Settings.ConfigPrecedence[i], Result) then
      Exit;
  Result := '';
end;

type
  TCodeFormatterDone = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TCodeFormatterDone }

function TCodeFormatterDone.GetMessage: string;
resourcestring
  str_FileHasBeenFormatted = 'The file has been reformatted.';
begin
  Result := str_FileHasBeenFormatted;
end;

procedure TCodeFormatterExpert.Execute;
resourcestring
  str_NoEditor = 'No source editor';
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
  str_UnableToGetContentsS = 'Unable to get contents of %s';
const
  FORMATTED_BLOCK_START = '****formatted block start****';
  FORMATTED_BLOCK_END = '****formatted block end****';
var
  SourceEditor: IOTASourceEditor;
  FileName: string;
  FullTextStr: TGXUnicodeString;
  FullText: TGXUnicodeStringList;
  Bookmarks: TBookmarkHandler;
  Breakpoints: TBreakpointHandler;
  i: Integer;
  TempSettings: TCodeFormatterSettings;
  OrigSettings: TCodeFormatterEngineSettings;
  SettingsName: string;
  BlockStart: TOTAEditPos;
  BlockEnd: TOTAEditPos;
  SelStart: Integer;
  SelLength: Integer;
  FormatSelection: Boolean;
  Position: Integer;
  FormattedBlockStart: string;
  FormattedBlockEnd: string;
begin
  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    raise ECodeFormatter.Create(str_NoEditor);
  FileName := SourceEditor.FileName;
  if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

  XSendDebug(Format('Formatting requested for "%s"', [FileName]));
  TempSettings := nil;
  FullText := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorTextAsUnicodeString(FullTextStr, False) then
      raise ECodeFormatter.CreateFmt(str_UnableToGetContentsS, [FileName]);
    if FullTextStr = '' then
      Exit;
    FullText.Text := FullTextStr;

    Breakpoints := nil;
    Bookmarks := TBookmarkHandler.Create;
    try
      Breakpoints := TBreakpointHandler.Create;
      Breakpoints.SaveItems;
      Bookmarks.SaveItems;
      SettingsName := Trim(GetSettingsName(FileName, FullText));
      if SettingsName <> '-' then begin
        if SettingsName <> '' then begin
          XSendDebug(Format('Use settings "%s"', [SettingsName]));
          TempSettings := TCodeFormatterSettings.Create;
          if TCodeFormatterConfigHandler.GetDefaultConfig(SettingsName, TempSettings) then begin
            OrigSettings := FEngine.Settings.Settings;
            FEngine.Settings.Settings := TempSettings.Settings;
          end else
            FreeAndNil(TempSettings);
        end else
          XSendDebug('Use default settings');

        FormatSelection := False;
        if GxOtaGetSelection(GxOtaGetTopMostEditView(SourceEditor), BlockStart, BlockEnd, SelStart, SelLength) then begin
          FormatSelection := (BlockStart.Line < BlockEnd.Line);
          if FormatSelection then begin
            FormattedBlockStart := FORMATTED_BLOCK_START;
            FormattedBlockEnd := FORMATTED_BLOCK_END;
            while Pos(FormattedBlockStart, FullTextStr) <> 0 do
              FormattedBlockStart := '*' + FormattedBlockStart + '*';
            while Pos(FormattedBlockEnd, FullTextStr) <> 0 do
              FormattedBlockEnd := '*' + FormattedBlockEnd + '*';
            FormattedBlockStart := '{' + FormattedBlockStart + '}';
            FormattedBlockEnd := '{' + FormattedBlockEnd + '}';

            BlockStart.Col := 1;
            if BlockEnd.Col > 1 then begin
              BlockEnd.Col := 1;
              BlockEnd.Line := BlockEnd.Line + 1;
            end;
            FullText.Insert(BlockEnd.Line - 1, FormattedBlockEnd);
            FullText.Insert(BlockStart.Line - 1, FormattedBlockStart);
          end;
        end;
        FullTextStr := ''; // might save some memory
        if FEngine.Execute(FullText) then begin
          if FormatSelection then begin
            FullTextStr := FullText.Text;
            Position := Pos(FormattedBlockEnd, FullTextStr);
            FullTextStr := Copy(FullTextStr, 1, Position - 1);
            Position := Pos(FormattedBlockStart, FullTextStr);
            FullTextStr := Copy(FullTextStr, Position + Length(FormattedBlockStart), MaxInt);
            if Copy(FullTextStr, 1, 2) = CRLF then
              FullTextStr := Copy(FullTextStr, 3, MaxInt);
            GxOtaSelectBlock(SourceEditor, BlockStart, BlockEnd);
            GxOtaReplaceSelection(SourceEditor, 0, FullTextStr);
          end else
            GxOtaReplaceEditorTextWithUnicodeString(SourceEditor, FullText.Text);
          Breakpoints.RestoreItems;
          Bookmarks.RestoreItems;
          for i := 0 to SourceEditor.EditViewCount - 1 do
            SourceEditor.EditViews[i].Paint;
          ShowGxMessageBox(TCodeFormatterDone);
        end;
      end else
        XSendDebug('Ignoring request, no settings name available');
    finally
      FreeAndNil(Breakpoints);
      FreeAndNil(Bookmarks);
    end;

  finally
    FreeAndNil(FullText);
    if Assigned(TempSettings) then begin
      FreeAndNil(TempSettings);
      FEngine.Settings.Settings := OrigSettings;
    end;
  end;
end;

procedure TCodeFormatterExpert.InternalLoadSettings(Settings: TExpertSettings);
var
  Reader: IConfigReader;
begin
  Reader := TGxConfigWrapper.Create(Settings);
  TCodeFormatterConfigHandler.ReadSettings(Reader, FEngine.Settings);
end;

procedure TCodeFormatterExpert.InternalSaveSettings(Settings: TExpertSettings);
var
  Writer: IConfigWriter;
begin
  Writer := TGxConfigWrapper.Create(Settings);
  TCodeFormatterConfigHandler.WriteSettings(Writer, FEngine.Settings);
end;

function TCodeFormatterExpert.FormatFile(const _FileName: string): Boolean;
resourcestring
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
var
  FullText: TGXUnicodeStringList;
  TempSettings: TCodeFormatterSettings;
  OrigSettings: TCodeFormatterEngineSettings;
  SettingsName: string;
begin
  Result := False;
  if not (IsPascalSourceFile(_FileName) or IsDelphiPackage(_FileName) or FileMatchesExtension(_FileName, '.tpl')) then
    raise ECodeFormatter.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(_FileName)]);

  XSendDebug(Format('Formatting requested for "%s"', [_FileName]));
  TempSettings := nil;
  FullText := TGXUnicodeStringList.Create;
  try
    FullText.LoadFromFile(_FileName);

    SettingsName := Trim(GetSettingsName(_FileName, FullText));
    if SettingsName <> '-' then begin
      if SettingsName <> '' then begin
        XSendDebug(Format('Use settings "%s"', [SettingsName]));
        TempSettings := TCodeFormatterSettings.Create;
        if TCodeFormatterConfigHandler.GetDefaultConfig(SettingsName, TempSettings) then begin
          OrigSettings := FEngine.Settings.Settings;
          FEngine.Settings.Settings := TempSettings.Settings;
        end else
          FreeAndNil(TempSettings);
      end else
        XSendDebug('Use default settings');

      Result := FEngine.Execute(FullText);
      if Result then begin
        XSendDebug('Saving changed file');
        // add an empty line to be compatible with running in the IDE
        FullText.Add('');
        FullText.SaveToFile(_FileName);
        ShowGxMessageBox(TCodeFormatterDone);
      end else begin
        XSendDebug('Nothing changed');
      end;
    end else
      XSendDebug('Ignoring request, no settings name available');
  finally
    FreeAndNil(FullText);
    if Assigned(TempSettings) then begin
      FreeAndNil(TempSettings);
      FEngine.Settings.Settings := OrigSettings;
    end;
  end;
end;

end.
