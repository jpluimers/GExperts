// handles reading and writing of the code formatter settings to an ini file or
// any other implementation of the IConfigReader and IConfigWriter interface
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterConfigHandler;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  IniFiles,
  GX_CodeFormatterSettings,
  GX_CodeFormatterTypes;

const
  FORMATTER_CONFIG_PREFIX = 'FormatterSettings-';

type
  IConfigReader = interface //FI:W523 - we don't need a GUID
    function ReadBool(const _Name: string; _Default: Boolean): Boolean;
    function ReadInteger(const _Name: string; _Default: Integer): Integer;
    function ReadString(const _Name, _Default: string): string;
    procedure ReadStrings(const _Section: string; const _List: TStrings);
  end;

type
  IConfigWriter = interface //FI:W523 - we don't need a GUID
    procedure WriteBool(const _Name: string; _Value: Boolean);
    procedure WriteInteger(const _Name: string; _Value: Integer);
    procedure WriteString(const _Name: string; const _Value: string);
    procedure WriteStrings(const _Section: string; const _List: TStrings);
  end;

type
  TIniFileWrapper = class(TInterfacedObject, IConfigReader, IConfigWriter)
  protected
    FIni: TCustomIniFile;
  protected // implementation of IConfigReader
    function ReadBool(const _Name: string; _Default: Boolean): Boolean;
    function ReadInteger(const _Name: string; _Default: Integer): Integer;
    function ReadString(const _Name, _Default: string): string;
    procedure ReadStrings(const _Section: string; const _List: TStrings);
  protected // implementatio of IConfigWriter
    procedure WriteBool(const _Name: string; _Value: Boolean);
    procedure WriteInteger(const _Name: string; _Value: Integer);
    procedure WriteString(const _Name: string; const _Value: string);
    procedure WriteStrings(const _Section: string; const _List: TStrings);
  public
    constructor Create(_IniFile: TCustomIniFile);
  end;

type
  TCodeFormatterConfigHandler = class
  public
    class procedure WriteSettings(_Writer: IConfigWriter; _Settings: TCodeFormatterSettings);
    class procedure ReadSettings(_Reader: IConfigReader; _Settings: TCodeFormatterSettings);
    class procedure ExportToFile(const _Filename: string; _Settings: TCodeFormatterSettings);
    class procedure ImportFromFile(const _Filename: string; _Settings: TCodeFormatterSettings);
    class procedure GetDefaultsList(_Defaults: TStrings);
    class function GetDefaultConfig(const _Name: string; _Settings: TCodeFormatterSettings): Boolean;
  end;

implementation

{ TIniFileWrapper }

constructor TIniFileWrapper.Create(_IniFile: TCustomIniFile);
begin
  inherited Create;
  FIni := _IniFile;
end;

function TIniFileWrapper.ReadBool(const _Name: string; _Default: Boolean): Boolean;
begin
  Result := FIni.ReadBool('settings', _Name, _Default)
end;

function TIniFileWrapper.ReadInteger(const _Name: string; _Default: Integer): Integer;
begin
  Result := FIni.ReadInteger('settings', _Name, _Default)
end;

function TIniFileWrapper.ReadString(const _Name, _Default: string): string;
begin
  Result := FIni.ReadString('settings', _Name, _Default)
end;

procedure TIniFileWrapper.ReadStrings(const _Section: string; const _List: TStrings);
var
  Cnt: Integer;
  i: Integer;
  s: string;
begin
  _List.Clear;
  Cnt := FIni.ReadInteger(_Section, 'Count', 0);
  for i := 0 to Cnt - 1 do begin
    s := FIni.ReadString(_Section, Format('Item%.4d', [i]), '');
    if s <> '' then
      _List.Add(s);
  end;
end;

procedure TIniFileWrapper.WriteBool(const _Name: string; _Value: Boolean);
begin
  FIni.WriteInteger('settings', _Name, Ord(_Value));
end;

procedure TIniFileWrapper.WriteInteger(const _Name: string; _Value: Integer);
begin
  FIni.WriteInteger('settings', _Name, _Value);
end;

procedure TIniFileWrapper.WriteString(const _Name, _Value: string);
begin
  FIni.WriteString('settings', _Name, _Value);
end;

procedure TIniFileWrapper.WriteStrings(const _Section: string; const _List: TStrings);
var
  i: Integer;
begin
  FIni.WriteInteger(_Section, 'Count', _List.Count);
  for i := 0 to _List.Count - 1 do begin
    FIni.WriteString(_Section, Format('Items%.4d', [i]), _List[i]);
  end;
end;

{ TCodeFormatterConfigHandler }

class procedure TCodeFormatterConfigHandler.ReadSettings(_Reader: IConfigReader; _Settings: TCodeFormatterSettings);

  function ReadSpaceSet(const _Name: string; _Default: TSpaceSet): TSpaceSet;
  begin
    Result := IntToSpaceSet(_Reader.ReadInteger(_Name, SpaceSetToInt(_Default)));
  end;

var
  ES: TCodeFormatterEngineSettings;
  cps: set of TConfigPrecedenceEnum;
  cp: TConfigPrecedenceEnum;
begin
  _Settings.UseCapitalizationFile := _Reader.ReadBool('UseCapitalizationFile', False);
  _Settings.CapitalizationFile := _Reader.ReadString('CapitalizationFile', '');
  _Settings.CapNames.Clear;
  if _Settings.UseCapitalizationFile and (_Settings.CapitalizationFile <> '') and FileExists(_Settings.CapitalizationFile) then
    _Settings.CapNames.LoadFromFile(_Settings.CapitalizationFile)
  else
    _Reader.ReadStrings('Capitalization', _Settings.CapNames);

  _Settings.ConfigPrecedence[1] := IntToConfigPrecedence(_Reader.ReadInteger('Precedence1', Ord(cpDirective)));
  _Settings.ConfigPrecedence[2] := IntToConfigPrecedence(_Reader.ReadInteger('Precedence2', Ord(cpIniFile)));
  _Settings.ConfigPrecedence[3] := IntToConfigPrecedence(_Reader.ReadInteger('Precedence3', Ord(cpMyConfig)));

  // make sure the setting is valid
  cps := [cpDirective, cpIniFile, cpMyConfig];
  Exclude(cps, _Settings.ConfigPrecedence[1]);
  if not (_Settings.ConfigPrecedence[2] in cps) then begin
    _Settings.ConfigPrecedence[2] := _Settings.ConfigPrecedence[3];
    if not (_Settings.ConfigPrecedence[2] in cps) then begin
      for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
        if cp in cps then begin
          _Settings.ConfigPrecedence[2] := cp;
          Break;
        end;
      end;
    end;
  end;
  Exclude(cps, _Settings.ConfigPrecedence[2]);
  if not (_Settings.ConfigPrecedence[3] in cps) then begin
    for cp := Low(TConfigPrecedenceEnum) to High(TConfigPrecedenceEnum) do begin
      if cp in cps then begin
        _Settings.ConfigPrecedence[3] := cp;
        Break;
      end;
    end;
  end;

  ES := _Settings.Settings;
  ES.SpaceOperators := ReadSpaceSet('SpaceOperators', ES.SpaceOperators);
  ES.SpaceColon := ReadSpaceSet('SpaceColon', ES.SpaceColon);
  ES.SpaceSemiColon := ReadSpaceSet('SpaceSemiColon', ES.SpaceSemiColon);
  ES.SpaceComma := ReadSpaceSet('SpaceComma', ES.SpaceComma);
  ES.SpaceLeftBr := ReadSpaceSet('SpaceLeftBr', ES.SpaceLeftBr);
  ES.SpaceRightBr := ReadSpaceSet('SpaceRightBr', ES.SpaceRightBr);
  ES.SpaceLeftHook := ReadSpaceSet('SpaceLeftHook', ES.SpaceLeftHook);
  ES.SpaceRightHook := ReadSpaceSet('SpaceRightHook', ES.SpaceRightHook);
  ES.SpaceEqualOper := ReadSpaceSet('SpaceEqualOper', ES.SpaceEqualOper);
  ES.UpperCompDirectives := _Reader.ReadBool('UpperCompDirectives', ES.UpperCompDirectives); //: Boolean;
  ES.UpperNumbers := _Reader.ReadBool('UpperNumbers', ES.UpperNumbers); //: Boolean;
  ES.ReservedCase := TCase(_Reader.ReadInteger('ReservedCase', Ord(ES.ReservedCase))); //: TCase;
  ES.StandDirectivesCase := TCase(_Reader.ReadInteger('StandDirectivesCase', Ord(ES.StandDirectivesCase))); //: TCase;
  ES.IdentifiersCase := TCase(_Reader.ReadInteger('IdentifiersCase', Ord(ES.IdentifiersCase))); // TCase
  ES.ChangeIndent := _Reader.ReadBool('ChangeIndent', ES.ChangeIndent); //: Boolean;
  ES.NoIndentElseIf := _Reader.ReadBool('NoIndentElseIf', ES.NoIndentElseIf); //: Boolean;
  ES.NoIndentVarDecl := _Reader.ReadBool('NoIndentVarDecl', ES.NoIndentVarDecl); //: Boolean;
  ES.NoIndentUsesComma := _Reader.ReadBool('NoIndentUsesComma', ES.NoIndentUsesComma); //: Boolean;
  ES.IndentBegin := _Reader.ReadBool('IndentBegin', ES.IndentBegin); //: Boolean;
  ES.IndentTry := _Reader.ReadBool('IndentTry', ES.IndentTry); //: Boolean;
  ES.IndentTryElse := _Reader.ReadBool('IndentTryElse', ES.IndentTryElse); //: Boolean;
  ES.IndentCaseElse := _Reader.ReadBool('IndentCaseElse', ES.IndentCaseElse); //: Boolean;
  ES.IndentComments := _Reader.ReadBool('IndentComments', ES.IndentComments); //: Boolean;
  ES.IndentCompDirectives := _Reader.ReadBool('IndentCompDirectives', ES.IndentCompDirectives); //: Boolean;
  ES.BlankProc := _Reader.ReadBool('BlankProc', ES.BlankProc); //: Boolean;
  ES.BlankSubProc := _Reader.ReadBool('BlankSubProc', ES.BlankSubProc); //: Boolean;
  ES.RemoveDoubleBlank := _Reader.ReadBool('RemoveDoubleBlank', ES.RemoveDoubleBlank); //: Boolean;
  ES.SpacePerIndent := _Reader.ReadInteger('SpacePerIndent', ES.SpacePerIndent); //: Integer;
  ES.FeedRoundBegin := TFeedBegin(_Reader.ReadInteger('FeedRoundBegin', Ord(ES.FeedRoundBegin)));
  ES.FeedRoundTry := TFeedBegin(_Reader.ReadInteger('FeedRoundTry', Ord(ES.FeedRoundTry)));
  ES.FeedBeforeElse := _Reader.ReadBool('FeedBeforeElse', ES.FeedBeforeElse); //: Boolean;
  ES.FeedBeforeEnd := _Reader.ReadBool('FeedBeforeEnd', ES.FeedBeforeEnd); //: Boolean;
  ES.FeedAfterThen := _Reader.ReadBool('FeedAfterThen', ES.FeedAfterThen); //: Boolean;
  ES.ExceptSingle := _Reader.ReadBool('ExceptSingle', ES.ExceptSingle); //: Boolean;
  ES.FeedAfterVar := _Reader.ReadBool('FeedAfterVar', ES.FeedAfterVar); //: Boolean;
  ES.FeedEachUnit := _Reader.ReadBool('FeedEachUnit', ES.FeedEachUnit); //: Boolean;
  ES.NoFeedBeforeThen := _Reader.ReadBool('NoFeedBeforeThen', ES.NoFeedBeforeThen); //: Boolean;
  ES.FeedElseIf := _Reader.ReadBool('FeedElseIf', ES.FeedElseIf); //: Boolean;
  ES.FillNewWords := IntToCapfileMode(_Reader.ReadInteger('FillNewWords', CapfileModeToInt(ES.FillNewWords)));
  ES.FeedAfterSemiColon := _Reader.ReadBool('FeedAfterSemiColon', ES.FeedAfterSemiColon); //: Boolean;
  ES.StartCommentOut := _Reader.ReadString('StartCommentOut', ES.StartCommentOut);
  ES.EndCommentOut := _Reader.ReadString('EndCommentOut', ES.EndCommentOut);
  ES.CommentFunction := _Reader.ReadBool('CommentFunction', ES.CommentFunction); //: Boolean;
  ES.CommentUnit := _Reader.ReadBool('CommentUnit', ES.CommentUnit); //: Boolean;
  ES.WrapLines := _Reader.ReadBool('WrapLines', ES.WrapLines); //: Boolean;
  ES.WrapPosition := _Reader.ReadInteger('WrapPosition', ES.WrapPosition); //: Byte;
  ES.AlignCommentPos := _Reader.ReadInteger('AlignCommentPos', ES.AlignCommentPos); //: Byte;
  ES.AlignComments := _Reader.ReadBool('AlignComments', ES.AlignComments); //: Boolean;
  ES.AlignVarPos := _Reader.ReadInteger('AlignVarPos', ES.AlignVarPos); //: Byte;
  ES.AlignVar := _Reader.ReadBool('AlignVar', ES.AlignVar); //: Boolean;
  _Settings.Settings := ES;
end;

class procedure TCodeFormatterConfigHandler.WriteSettings(_Writer: IConfigWriter; _Settings: TCodeFormatterSettings);

  procedure WriteSpaceSet(const _Name: string; _Value: TSpaceSet);
  begin
    _Writer.WriteInteger(_Name, SpaceSetToInt(_Value));
  end;

begin
  WriteSpaceSet('SpaceOperators', _Settings.SpaceOperators);
  WriteSpaceSet('SpaceColon', _Settings.SpaceColon);
  WriteSpaceSet('SpaceSemiColon', _Settings.SpaceSemiColon);
  WriteSpaceSet('SpaceComma', _Settings.SpaceComma);
  WriteSpaceSet('SpaceLeftBr', _Settings.SpaceLeftBr);
  WriteSpaceSet('SpaceRightBr', _Settings.SpaceRightBr);
  WriteSpaceSet('SpaceLeftHook', _Settings.SpaceLeftHook);
  WriteSpaceSet('SpaceRightHook', _Settings.SpaceRightHook);
  WriteSpaceSet('SpaceEqualOper', _Settings.SpaceEqualOper);
  _Writer.WriteBool('UpperCompDirectives', _Settings.UpperCompDirectives); //: Boolean;
  _Writer.WriteBool('UpperNumbers', _Settings.UpperNumbers); //: Boolean;
  _Writer.WriteInteger('ReservedCase', Ord(_Settings.ReservedCase)); //: TCase;
  _Writer.WriteInteger('StandDirectivesCase', Ord(_Settings.StandDirectivesCase)); //: TCase;
  _Writer.WriteInteger('IdentifiersCase', Ord(_Settings.IdentifiersCase));
  _Writer.WriteBool('ChangeIndent', _Settings.ChangeIndent); //: Boolean;
  _Writer.WriteBool('NoIndentElseIf', _Settings.NoIndentElseIf); //: Boolean;
  _Writer.WriteBool('NoIndentVarDecl', _Settings.NoIndentVarDecl); //: Boolean;
  _Writer.WriteBool('NoIndentUsesComma', _Settings.NoIndentUsesComma); //: Boolean;
  _Writer.WriteBool('IndentBegin', _Settings.IndentBegin); //: Boolean;
  _Writer.WriteBool('IndentTry', _Settings.IndentTry); //: Boolean;
  _Writer.WriteBool('IndentTryElse', _Settings.IndentTryElse); //: Boolean;
  _Writer.WriteBool('IndentCaseElse', _Settings.IndentCaseElse); //: Boolean;
  _Writer.WriteBool('IndentComments', _Settings.IndentComments); //: Boolean;
  _Writer.WriteBool('IndentCompDirectives', _Settings.IndentCompDirectives); //: Boolean;
  _Writer.WriteBool('BlankProc', _Settings.BlankProc); //: Boolean;
  _Writer.WriteBool('BlankSubProc', _Settings.BlankSubProc); //: Boolean;
  _Writer.WriteBool('RemoveDoubleBlank', _Settings.RemoveDoubleBlank); //: Boolean;
  _Writer.WriteInteger('SpacePerIndent', _Settings.SpacePerIndent); //: Integer;
  _Writer.WriteInteger('FeedRoundBegin', Ord(_Settings.FeedRoundBegin)); //: TFeedBegin;
  _Writer.WriteInteger('FeedRoundTry', Ord(_Settings.FeedRoundTry)); //: TFeedBegin;
  _Writer.WriteBool('FeedBeforeElse', _Settings.FeedBeforeElse); //: Boolean;
  _Writer.WriteBool('FeedBeforeEnd', _Settings.FeedBeforeEnd); //: Boolean;
  _Writer.WriteBool('FeedAfterThen', _Settings.FeedAfterThen); //: Boolean;
  _Writer.WriteBool('ExceptSingle', _Settings.ExceptSingle); //: Boolean;
  _Writer.WriteBool('FeedAfterVar', _Settings.FeedAfterVar); //: Boolean;
  _Writer.WriteBool('FeedEachUnit', _Settings.FeedEachUnit); //: Boolean;
  _Writer.WriteBool('NoFeedBeforeThen', _Settings.NoFeedBeforeThen); //: Boolean;
  _Writer.WriteBool('FeedElseIf', _Settings.FeedElseIf); //: Boolean;
  _Writer.WriteInteger('FillNewWords', CapfileModeToInt(_Settings.FillNewWords));
  _Writer.WriteBool('FeedAfterSemiColon', _Settings.FeedAfterSemiColon); //: Boolean;
  _Writer.WriteString('StartCommentOut', string(_Settings.StartCommentOut)); //: TCommentArray;
  _Writer.WriteString('EndCommentOut', string(_Settings.EndCommentOut)); //: TCommentArray;
  _Writer.WriteBool('CommentFunction', _Settings.CommentFunction); //: Boolean;
  _Writer.WriteBool('CommentUnit', _Settings.CommentUnit); //: Boolean;
  _Writer.WriteBool('WrapLines', _Settings.WrapLines); //: Boolean;
  _Writer.WriteInteger('WrapPosition', _Settings.WrapPosition); //: Byte;
  _Writer.WriteInteger('AlignCommentPos', _Settings.AlignCommentPos); //: Byte;
  _Writer.WriteBool('AlignComments', _Settings.AlignComments); //: Boolean;
  _Writer.WriteInteger('AlignVarPos', _Settings.AlignVarPos); //: Byte;
  _Writer.WriteBool('AlignVar', _Settings.AlignVar); //: Boolean;

  _Writer.WriteInteger('Precedence1', Ord(_Settings.ConfigPrecedence[1]));
  _Writer.WriteInteger('Precedence2', Ord(_Settings.ConfigPrecedence[2]));
  _Writer.WriteInteger('Precedence3', Ord(_Settings.ConfigPrecedence[3]));

  _Writer.WriteBool('UseCapitalizationFile', _Settings.UseCapitalizationFile);
  _Writer.WriteString('CapitalizationFile', string(_Settings.CapitalizationFile));

  if _Settings.UseCapitalizationFile and (_Settings.CapitalizationFile <> '') then begin
    try
      _Settings.CapNames.SaveToFile(string(_Settings.CapitalizationFile));
    except //FI:W501
        // ignore, file might be readonly
    end;
  end else
    _Writer.WriteStrings('Capitalization', _Settings.CapNames);
end;

class procedure TCodeFormatterConfigHandler.ExportToFile(const _Filename: string; _Settings: TCodeFormatterSettings);
var
  Writer: IConfigWriter;
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(_Filename);
  try
    Writer := TIniFileWrapper.Create(Ini);
    WriteSettings(Writer, _Settings);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function GetModulePath: string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

class function TCodeFormatterConfigHandler.GetDefaultConfig(const _Name: string; _Settings: TCodeFormatterSettings): Boolean;
var
  FileName: string;
begin
  FileName := GetModulePath + FORMATTER_CONFIG_PREFIX + _Name + '.ini';
  Result := FileExists(FileName);
  if Result then
    ImportFromFile(FileName, _Settings);
end;

class procedure TCodeFormatterConfigHandler.GetDefaultsList(_Defaults: TStrings);
var
  Path: string;
  sr: TSearchRec;
  s: string;
begin
  Path := GetModulePath;
  if 0 = FindFirst(Path + FORMATTER_CONFIG_PREFIX + '*.ini', faAnyFile, sr) then begin
    try
      repeat
        s := ChangeFileExt(sr.Name, '');
        Delete(s, 1, Length(FORMATTER_CONFIG_PREFIX));
        _Defaults.Add(s);
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
  end;
end;

class procedure TCodeFormatterConfigHandler.ImportFromFile(const _Filename: string; _Settings: TCodeFormatterSettings);
var
  Reader: IConfigReader;
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(_Filename);
  try
    Reader := TIniFileWrapper.Create(Ini);
    ReadSettings(Reader, _Settings);
  finally
    Ini.Free;
  end;
end;

end.

