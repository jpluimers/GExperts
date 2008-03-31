unit GX_ProofreaderData;

{ Original Author: Alex Petrov - Alex@krona.obninsk.ru }
{ Modified 30 January 1999 by Francois Sorrentino for localized keyboard support }

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, OmniXML, GX_ProofreaderUtils;

type
  TProofreaderData = class
  private
    FHistory: TCorrectionHistory;
    FDefaultLanguage: TReplacementSource;
    FAutoTypeWriterNotifier: IUnknown;
    // Options for the Code Proofreader
    FReplacerActive: Boolean;
    FDictionaryActive: Boolean;
    FCompilerActive: Boolean;
    FDictionaryCaseMayDiffer: Boolean;
    FOneCharIncorrect: Boolean;
    FMustBeNearbyLetter: Boolean;
    FAllowOneCharacterMissing: Boolean;
    FAllowExtraChar: Boolean;
    FAllowSwitchedChars: Boolean;
    FFirstCharMustBeCorrect: Boolean;
    FBeepOnReplace: Boolean;
    FActiveTab: Integer;

    FReplaceLists: array[TReplacementSource] of TReplacementStringList;
    FDictionaryLists: array[TReplacementSource] of TStringList;
    procedure LoadReplacement(const Doc: IXMLDocument;
      const RulesNode: IXMLNode; ReplacementSource: TReplacementSource);
    procedure LoadDictionary(const Doc: IXMLDocument;
      const RulesNode: IXMLNode; ReplacementSource: TReplacementSource);
    procedure SaveReplacement(const Doc: IXMLDocument;
      const RulesNode: IXMLElement; ReplacementSource: TReplacementSource);
    procedure SaveDictionary(const Doc: IXMLDocument;
      const RulesNode: IXMLElement; ReplacementSource: TReplacementSource);
    function GetXmlFileName: string;
    procedure LoadError(const E: Exception);
    function GetDictionary(Dictionary: TReplacementSource): TStringList;
    procedure ValidateDictionaryIndex(Dict: TStringList; Index: Integer);
    procedure ResortReplacements(ReplacementList: TReplacementStringList);
    function GetReplacements(Dictionary: TReplacementSource): TReplacementStringList;
    procedure ValidateReplacementIndex(ReplacementList: TReplacementStringList; Index: Integer);
  public
    constructor Create(const ConfigurationKey: string);
    destructor Destroy; override;
    procedure SaveData;
    procedure ReloadData;

    function FindExactDictionary(const Zone: TReplacementSource; const AWord: string): Boolean;
    function FindDictionary(const Zone: TReplacementSource; const AWord: string): string;
    function FindInStrings(Strings: TStrings; const ListIsSorted: Boolean; const Word: string): string;
    function FindReplacementIndex(const Zone: TReplacementSource; const TypedString: string): Integer;
    function FindReplacement(const Zone: TReplacementSource; const TypedString: string): TReplacementItem;

    procedure SaveSettings(const ConfigurationKey: string);
    procedure LoadSettings(const ConfigurationKey: string);
    property ReplacerActive: Boolean read FReplacerActive write FReplacerActive;
    property DictionaryActive: Boolean read FDictionaryActive write FDictionaryActive;
    property CompilerActive: Boolean read FCompilerActive write FCompilerActive;
    property DictionaryCaseMayDiffer: Boolean read FDictionaryCaseMayDiffer write FDictionaryCaseMayDiffer;
    property OneCharIncorrect: Boolean read FOneCharIncorrect write FOneCharIncorrect;
    property MustBeNearbyLetter: Boolean read FMustBeNearbyLetter write FMustBeNearbyLetter;
    property AllowOneCharacterMissing: Boolean read FAllowOneCharacterMissing write FAllowOneCharacterMissing;
    property AllowExtraChar: Boolean read FAllowExtraChar write FAllowExtraChar;
    property AllowSwitchedChars: Boolean read FAllowSwitchedChars write FAllowSwitchedChars;
    property FirstCharMustBeCorrect: Boolean read FFirstCharMustBeCorrect write FFirstCharMustBeCorrect;
    property BeepOnReplace: Boolean read FBeepOnReplace write FBeepOnReplace;
    property ActiveTab: Integer read FActiveTab write FActiveTab;

    property History: TCorrectionHistory read FHistory;

    property DefaultLanguage: TReplacementSource read FDefaultLanguage write FDefaultLanguage;
    property XmlFileName: string read GetXmlFileName;
    function AddToDictionary(Dictionary: TReplacementSource; const Word: string; Resort: Boolean = True): Integer;
    procedure ResortDictionary(Dictionary: TReplacementSource);
    procedure SetDictionaryEntry(Dictionary: TReplacementSource; Index: Integer; const OldWord, NewWord: string);
    function GetDictionaryCount(Dictionary: TReplacementSource): Integer;
    procedure DeleteDictionaryEntry(Dictionary: TReplacementSource; Index: Integer; const Word: string);
    function GetDictionaryEntry(Dictionary: TReplacementSource; Index: Integer): string;
    function AddToReplacements(Dictionary: TReplacementSource;
      const TypedString: string; ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string): Integer;
    procedure SetReplacementEntry(Dictionary: TReplacementSource; Index: Integer;
      const OldTypedString, NewTypedString: string; ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string);
    function GetReplacementCount(Dictionary: TReplacementSource): Integer;
    procedure DeleteReplacementEntry(Dictionary: TReplacementSource; Index: Integer; const TypedString: string);
    function GetReplacementEntry(Dictionary: TReplacementSource; Index: Integer): TReplacementItem;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Dialogs,
  GX_ProofreaderCorrection, GX_ProofreaderKeyboard, GX_GenericUtils,
  GX_ConfigurationInfo, GX_XmlUtils;

type
  EProofreader = class(Exception);

const // Do not localize any of the strings in this const section:
  cXmlAttributeLanguage = 'Language';
  cXmlAttributeWhere = 'Where';
  cXmlNodeRoot = 'CodeProofreader';
  cXmlNodeRules = 'Rules';
  cXmlNodeAutoCorrect = 'AutoCorrectRules';
  cXmlNodeAutoCorrection = 'AutoCorrectRule';
  cXmlNodeReplaceWhat = 'ReplaceWhat';
  cXmlNodeReplaceWith = 'ReplaceWith';
  cXmlNodeDictionary = 'DictionaryEntries';
  cXmlNodeDictionaryEntry = 'Entry';
  cXmlNodeDictionaryEntryOld = 'DictionaryEntry'; // This was too long for large dictionaries

function BinaryCaseSensitiveSearch(const Strings: TStrings; const AWord: string): Integer;
var
  i: Integer;
  LowIndex: Integer;
  HighIndex: Integer;
  MiddleIndex: Integer;
begin
  Assert(Assigned(Strings));

  Result := -1;
  LowIndex := 0;
  HighIndex := Strings.Count - 1;
  if HighIndex >= 0 then
  begin
    if AnsiCompareStr(Strings[HighIndex], AWord) = 0 then
    begin
      Result := HighIndex;
      Exit;
    end
    else
      while True do
      begin
        MiddleIndex := (LowIndex + HighIndex) div 2;
        i := AnsiCompareStr(Strings[MiddleIndex], AWord);
        if i = 0 then
        begin
          Result := MiddleIndex;
          Exit;
        end
        else
        begin
          if i > 0 then
          begin
            if HighIndex = MiddleIndex then
              Exit
            else
              HighIndex := MiddleIndex
          end
          else
          begin
            if LowIndex = MiddleIndex then
              Exit
            else
              LowIndex := MiddleIndex;
          end;
        end;
      end;
  end;
end;

function LinearCaseSensitiveSearch(const Strings: TStrings; const AWord: string): Integer;
var
  i: Integer;
begin
  Assert(Assigned(Strings));

  Result := -1;

  for i := 0 to Strings.Count - 1 do
  begin
    if AnsiCompareStr(Strings[i], AWord) = 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function LinearCaseInSensitiveSearch(const Strings: TStrings; const AWord: string): Integer;
var
  i: Integer;
begin
  Assert(Assigned(Strings));

  Result := -1;

  for i := 0 to Strings.Count - 1 do
  begin
    if AnsiCompareText(Strings[i], AWord) = 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function FindWordInStrings(const Word: string; const Strings: TStrings;
  const ListIsSorted, CaseSensitiveSearch: Boolean): Integer;
begin
  if CaseSensitiveSearch then
  begin
    if ListIsSorted then
    begin
      // String list is sorted case sensitively, so we
      // can employ a binary search.
      Result := BinaryCaseSensitiveSearch(Strings, Word);
    end
    else
      Result := LinearCaseSensitiveSearch(Strings, Word);
  end
  else
    Result := LinearCaseInSensitiveSearch(Strings, Word);
end;

function IsWordInStrings(const Word: string; const Strings: TStrings;
  const ListIsSorted, CaseSensitiveSearch: Boolean): Boolean;
begin
  Result := FindWordInStrings(Word, Strings, ListIsSorted, CaseSensitiveSearch) >= 0;
end;

{ TProofreaderData }

constructor TProofreaderData.Create(const ConfigurationKey: string);
var
  i: TReplacementSource;
begin
  inherited Create;
  LoadSettings(ConfigurationKey);

  for i := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    FReplaceLists[i] := TReplacementStringList.Create;
    FDictionaryLists[i] := TStringList.Create;
  end;
  FHistory := TCorrectionHistory.Create;
  FAutoTypeWriterNotifier := GetNewAutoTypeWriterNotifier(Self);
end;

destructor TProofreaderData.Destroy;
var
  i: TReplacementSource;
begin
  if Assigned(FAutoTypeWriterNotifier) then
  begin
    (FAutoTypeWriterNotifier as IAutoTypeWriterNotifier).Detach;
    FAutoTypeWriterNotifier := nil;
  end;

  for i := Low(TReplacementSource) to High(TReplacementSource) do
  begin
    FreeAndNil(FReplaceLists[i]);
    FreeAndNil(FDictionaryLists[i]);
  end;
  FreeAndNil(FHistory);

  inherited Destroy;
end;

function TProofreaderData.GetXmlFileName: string;
begin
  Result := AddSlash(ConfigInfo.ConfigPath) + 'CodeProofreader.xml'
end;

function XmlStringToReplacementSource(const LanguageName: string): TReplacementSource;
var
  Source: TReplacementSource;
begin
  for Source := Low(TReplacementSource) to High(TReplacementSource) do
    if ReplacementSourceXmlStrings[Source] = LanguageName then
    begin
      Result := Source;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown Language "%s"', [LanguageName]);
end;

function XmlStringToWhereReplace(const WhereReplaceString: string): TGXWhereReplace;
var
  Where: TGXWhereReplace;
begin
  for Where := Low(TGXWhereReplace) to High(TGXWhereReplace) do
    if GXWhereReplaceXmlStrings[Where] = WhereReplaceString then
    begin
      Result := Where;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown WhereReplace value "%s"', [WhereReplaceString]);
end;

function GetLanguageAttribute(Node: IXMLNode): TReplacementSource;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(cXmlAttributeLanguage);
  if Assigned(AttrNode) then
    Result := XmlStringToReplacementSource(AttrNode.NodeValue)
  else
    raise EProofreader.Create('Missing Language attribute');
end;

function GetWhereReplaceAttribute(Node: IXMLNode): TGXWhereReplace;
var
  AttrNode: IXMLNode;
begin
  AttrNode := Node.Attributes.GetNamedItem(cXmlAttributeWhere);
  if Assigned(AttrNode) then
    Result := XmlStringToWhereReplace(AttrNode.NodeValue)
  else
    raise EProofreader.Create('Missing WhereReplace attribute');
end;

procedure TProofreaderData.LoadError(const E: Exception);
resourcestring
  SLoadError = 'Error while loading %s' + sLineBreak;
begin
  GxLogAndShowException(E, Format(SLoadError, [GetXmlFileName]));
end;

procedure TProofreaderData.LoadReplacement(const Doc: IXMLDocument;
  const RulesNode: IXMLNode; ReplacementSource: TReplacementSource);
var
  AutoCorrectEntryNodes: IXMLNodeList;
  i: Integer;
  AutoCorrectNode, AutoCorrectEntryNode, ChildNode: IXMLNode;
  ReplaceWhatString, ReplaceWithString: string;
  WhereReplace: TGXWhereReplace;
begin
  AutoCorrectNode := RulesNode.SelectSingleNode(cXmlNodeAutoCorrect);
  if not Assigned(AutoCorrectNode) then
    Exit;

  AutoCorrectEntryNodes := AutoCorrectNode.SelectNodes(cXmlNodeAutoCorrection);
  for i := 0 to AutoCorrectEntryNodes.Length - 1 do
  begin
    AutoCorrectEntryNode := AutoCorrectEntryNodes.Item[i];
    Assert(AutoCorrectEntryNode.NodeType = ELEMENT_NODE);
    Assert(AutoCorrectEntryNode.NodeName = cXmlNodeAutoCorrection);

    ChildNode := AutoCorrectEntryNode.SelectSingleNode(cXmlNodeReplaceWhat);
    if Assigned(ChildNode) then
      ReplaceWhatString := ChildNode.Text
    else
      ReplaceWhatString := '';

    ChildNode := AutoCorrectEntryNode.SelectSingleNode(cXmlNodeReplaceWith);
    if Assigned(ChildNode) then
      ReplaceWithString := ChildNode.Text
    else
      ReplaceWithString := '';

    try
      WhereReplace := GetWhereReplaceAttribute(AutoCorrectEntryNode);

      if (Trim(ReplaceWhatString) <> '') and (Trim(ReplaceWithString) <> '') then
        FReplaceLists[ReplacementSource].Add(ReplaceWhatString, WhereReplace, ReplaceWithString);
    except
      on e: EProofreader do
        LoadError(e);
      on e: EConvertError do
        LoadError(e);
    end;
  end;
end;

procedure TProofreaderData.LoadDictionary(const Doc: IXMLDocument;
  const RulesNode: IXMLNode; ReplacementSource: TReplacementSource);
var
  DictionaryEntryNodes: IXMLNodeList;
  i: Integer;
  DictionaryNode, DictionaryEntryNode: IXMLNode;
  DictionaryWordString: string;
begin
  DictionaryNode := RulesNode.SelectSingleNode(cXmlNodeDictionary);
  if not Assigned(DictionaryNode) then
    Exit;

  DictionaryEntryNodes := DictionaryNode.SelectNodes(cXmlNodeDictionaryEntry);
  if DictionaryEntryNodes.Length < 1 then
    DictionaryEntryNodes := DictionaryNode.SelectNodes(cXmlNodeDictionaryEntryOld);

  FDictionaryLists[ReplacementSource].BeginUpdate;
  try
    for i := 0 to DictionaryEntryNodes.Length - 1 do
    begin
      DictionaryEntryNode := DictionaryEntryNodes.Item[i];
      Assert(DictionaryEntryNode.NodeType = ELEMENT_NODE);

      DictionaryWordString := DictionaryEntryNode.Text;
      if Trim(DictionaryWordString) <> '' then
        FDictionaryLists[ReplacementSource].Add(DictionaryWordString);
    end;
  finally
    FDictionaryLists[ReplacementSource].EndUpdate;
  end;
end;

procedure TProofreaderData.ReloadData;
var
  Doc: IXMLDocument;
  RulesNodes: IXMLNodeList;
  RulesNode: IXMLNode;
  FileName: string;
  Replacement: TReplacementSource;
  i: Integer;
begin
  FileName := GetXmlFileName;

  if FileExists(FileName) then
  begin
    for Replacement := Low(TReplacementSource) to High(TReplacementSource) do
    begin
      Assert(FDictionaryLists[Replacement] <> nil);
      FDictionaryLists[Replacement].Clear;
      Assert(FReplaceLists[Replacement] <> nil);
      FReplaceLists[Replacement].Clear;
    end;

    Doc := CreateXMLDoc;
    Doc.Load(FileName);

    RulesNodes := Doc.DocumentElement.SelectNodes(cXmlNodeRules);
    for i := 0 to RulesNodes.Length - 1 do
    begin
      RulesNode := RulesNodes.Item[i];
      try
        Replacement := GetLanguageAttribute(RulesNode);
        LoadReplacement(Doc, RulesNode, Replacement);
        LoadDictionary(Doc, RulesNode, Replacement);
      except
        on e: EProofreader do
          LoadError(e);
        on e: EConvertError do
          LoadError(e);
      end;
    end;

    for Replacement := Low(TReplacementSource) to High(TReplacementSource) do
    begin
      SortStringsCaseSensitive(FDictionaryLists[Replacement]);
      SortStringsCaseSensitive(FReplaceLists[Replacement]);
    end;
  end;
end;

procedure TProofreaderData.SaveReplacement(const Doc: IXMLDocument;
  const RulesNode: IXMLElement; ReplacementSource: TReplacementSource);
var
  i: Integer;
  AutoCorrectNode, ChildNode: IXMLNode;
  AutoCorrectEntryNode: IXMLElement;
  AReplacement: TReplacementItem;
begin
  AutoCorrectNode := Doc.CreateElement(cXmlNodeAutoCorrect);
  RulesNode.AppendChild(AutoCorrectNode);
  for i := 0 to FReplaceLists[ReplacementSource].Count - 1 do
  begin
    AutoCorrectEntryNode := Doc.CreateElement(cXmlNodeAutoCorrection);
    AutoCorrectNode.AppendChild(AutoCorrectEntryNode);
    AReplacement := FReplaceLists[ReplacementSource].Objects[i] as TReplacementItem;
    AutoCorrectEntryNode.SetAttribute(cXmlAttributeWhere, GXWhereReplaceXmlStrings[AReplacement.Where]);

    ChildNode := Doc.CreateElement(cXmlNodeReplaceWhat);
    AutoCorrectEntryNode.AppendChild(ChildNode);
    ChildNode.Text := AReplacement.Typed;

    ChildNode := Doc.CreateElement(cXmlNodeReplaceWith);
    AutoCorrectEntryNode.AppendChild(ChildNode);
    ChildNode.Text := AReplacement.Replace;
  end;
end;

procedure TProofreaderData.SaveDictionary(const Doc: IXMLDocument;
  const RulesNode: IXMLElement; ReplacementSource: TReplacementSource);
var
  i: Integer;
  DictionaryNode: IXMLNode;
  DictionaryEntryNode: IXMLElement;
begin
  DictionaryNode := Doc.CreateElement(cXmlNodeDictionary);
  RulesNode.AppendChild(DictionaryNode);
  for i := 0 to FDictionaryLists[ReplacementSource].Count - 1 do
  begin
    DictionaryEntryNode := Doc.CreateElement(cXmlNodeDictionaryEntry);
    DictionaryEntryNode.Text := FDictionaryLists[ReplacementSource][i];
    DictionaryNode.AppendChild(DictionaryEntryNode);
  end;
end;

procedure TProofreaderData.SaveData;
var
  Doc: IXMLDocument;
  RootNode, RulesNode: IXMLElement;
  Replacement: TReplacementSource;
begin
  Doc := CreateXMLDoc;
  AddXMLHeader(Doc);
  RootNode := Doc.CreateElement(cXmlNodeRoot);
  Doc.DocumentElement := RootNode;

  for Replacement := Low(TReplacementSource) to High(TReplacementSource) do
    if (FDictionaryLists[Replacement].Count > 0) or (FReplaceLists[Replacement].Count > 0) then
    begin
      RulesNode := Doc.CreateElement(cXmlNodeRules);
      RulesNode.SetAttribute(cXmlAttributeLanguage, ReplacementSourceXmlStrings[Replacement]);
      RootNode.AppendChild(RulesNode);
      SaveReplacement(Doc, RulesNode, Replacement);
      SaveDictionary(Doc, RulesNode, Replacement);
    end;

  Doc.Save(GetXmlFileName, ofIndent);
end;

procedure TProofreaderData.LoadSettings(const ConfigurationKey: string);
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    // Do not localize any of the items below:
    FReplacerActive := Settings.ReadBool(ConfigurationKey, 'ReplacerActive', False);
    FDictionaryActive := Settings.ReadBool(ConfigurationKey, 'DictionaryActive', False);
    {$IFNDEF GX_BCB}
    // C++Builder does not have support for compiler-assisted proofreading
    FCompilerActive := Settings.ReadBool(ConfigurationKey, 'CompilerActive', False);
    {$ENDIF GX_BCB}
    FDictionaryCaseMayDiffer := Settings.ReadBool(ConfigurationKey, 'DictionaryCaseDiffer', True);
    FOneCharIncorrect := Settings.ReadBool(ConfigurationKey, 'OtherLetter', True);
    FMustBeNearbyLetter := Settings.ReadBool(ConfigurationKey, 'NearbyLetter', False);
    FAllowOneCharacterMissing := Settings.ReadBool(ConfigurationKey, 'NoLetter', True);
    FAllowExtraChar := Settings.ReadBool(ConfigurationKey, 'MoreLetter', True);
    FAllowSwitchedChars := Settings.ReadBool(ConfigurationKey, 'MixedLetter', True);
    FFirstCharMustBeCorrect := Settings.ReadBool(ConfigurationKey, 'NoFirstOther', True);
    FBeepOnReplace := Settings.ReadBool(ConfigurationKey, 'BeepOnReplace', True);
    FActiveTab := Settings.ReadInteger(ConfigurationKey, 'ActiveTab', 2);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TProofreaderData.SaveSettings(const ConfigurationKey: string);
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    // Do not localize any of the items below:
    Settings.WriteBool(ConfigurationKey, 'ReplacerActive', FReplacerActive);
    Settings.WriteBool(ConfigurationKey, 'DictionaryActive', FDictionaryActive);
    {$IFNDEF GX_BCB}
    Settings.WriteBool(ConfigurationKey, 'CompilerActive', FCompilerActive);
    {$ENDIF GX_BCB}
    Settings.WriteBool(ConfigurationKey, 'DictionaryCaseDiffer', FDictionaryCaseMayDiffer);
    Settings.WriteBool(ConfigurationKey, 'OtherLetter', FOneCharIncorrect);
    Settings.WriteBool(ConfigurationKey, 'NearbyLetter', FMustBeNearbyLetter);
    Settings.WriteBool(ConfigurationKey, 'NoLetter', FAllowOneCharacterMissing);
    Settings.WriteBool(ConfigurationKey, 'MoreLetter', FAllowExtraChar);
    Settings.WriteBool(ConfigurationKey, 'MixedLetter', FAllowSwitchedChars);
    Settings.WriteBool(ConfigurationKey, 'NoFirstOther', FFirstCharMustBeCorrect);
    Settings.WriteBool(ConfigurationKey, 'BeepOnReplace', FBeepOnReplace);
    Settings.WriteInteger(ConfigurationKey, 'ActiveTab', FActiveTab);
  finally
    FreeAndNil(Settings);
  end;
end;

function TProofreaderData.FindDictionary(const Zone: TReplacementSource; const AWord: string): string;
begin
  Result := FindInStrings(FDictionaryLists[Zone], FDictionaryLists[Zone].Sorted, AWord);
end;

function TProofreaderData.FindReplacement(const Zone: TReplacementSource; const TypedString: string): TReplacementItem;
var
  Index: Integer;
begin
  Index := FindReplacementIndex(Zone, TypedString);
  if Index < 0 then
    Result := nil
  else
    Result := FReplaceLists[Zone].Objects[Index] as TReplacementItem;
end;

function TProofreaderData.FindReplacementIndex(const Zone: TReplacementSource; const TypedString: string): Integer;
begin
  Result := FReplaceLists[Zone].Find(TypedString);
end;

function TProofreaderData.FindInStrings(Strings: TStrings; const ListIsSorted: Boolean; const Word: string): string;
var
  MaxErrs: Integer;

  function ErrCount(CurErrLevel: Integer; const InDict, Typed: string): Integer;
  var
    DifferCharIndex: Integer;
    nInDict, nTyped, minN: Integer;
    ResultCandidate: Integer;

    procedure ConsiderResultCandidate;
    begin
      if ResultCandidate < Result then
        Result := ResultCandidate;
    end;

  begin
    // Return if the InDict and Typed are the same the CurErrLevel
    if InDict = Typed then
    begin
      Result := CurErrLevel;
      Exit;
    end;

    Result := 1000;
    if CurErrLevel > MaxErrs then
      Exit;

    nInDict := Length(InDict);
    nTyped := Length(Typed);
    minN := Min(nInDict, nTyped);

    // Skip words that have too many additional characters.
    if Abs(nInDict - nTyped) * 10 + CurErrLevel > MaxErrs then
      Exit;

    DifferCharIndex := 1;
    while (DifferCharIndex <= minN) and (InDict[DifferCharIndex] = Typed[DifferCharIndex]) do
      Inc(DifferCharIndex);
    // The chars are now the same until DifferCharIndex

    // If InDict or Typed has run out of letters.
    if (DifferCharIndex = minN) and (nInDict <> nTyped) then
    begin
      // If the more letters are allowed and there are still letters in Typed
      if (nTyped <> DifferCharIndex) and AllowExtraChar then
        // Return for every addional letter 20 more error levels.
        Result := (nTyped - DifferCharIndex) * 20 + CurErrLevel
      // If there are still letters in InDict and missing letters are allowed.
      else if (nInDict <> DifferCharIndex) and AllowOneCharacterMissing then
        // Return for every addional letter 20 more error levels.
        Result := (nInDict - DifferCharIndex) * 20 + CurErrLevel
    end;

    // If other letters are allowed and the chars are near on the keyboard.
    if OneCharIncorrect and
      ((not MustBeNearbyLetter) or (((DifferCharIndex <= Length(Typed)) and (DifferCharIndex <= Length(InDict))) and
      (CharsAreNearOnKeyboard(Typed[DifferCharIndex], InDict[DifferCharIndex])))) then
    begin
      // The Result is the errorcount +14 found in the resulting string.
      ResultCandidate := ErrCount(CurErrLevel + 14,
        Copy(InDict, DifferCharIndex + 1, nInDict - DifferCharIndex),
        Copy(Typed, DifferCharIndex + 1, nTyped - DifferCharIndex));
      ConsiderResultCandidate;
    end;

    if AllowOneCharacterMissing then
    begin
      // Delete one character from the InDict string and get resulting
      // error count + 14.
      ResultCandidate := ErrCount(CurErrLevel + 14,
        Copy(InDict, DifferCharIndex + 1, nInDict - DifferCharIndex),
        Copy(Typed, DifferCharIndex, nTyped - DifferCharIndex + 1));
      ConsiderResultCandidate;
    end;

    if AllowExtraChar then
    begin
      ResultCandidate := ErrCount(CurErrLevel + 16,
        Copy(InDict, DifferCharIndex, nInDict - DifferCharIndex + 1),
        Copy(Typed, DifferCharIndex + 1, nTyped - DifferCharIndex));
      ConsiderResultCandidate;
    end;

    if AllowSwitchedChars and (nInDict > DifferCharIndex) and
      (nTyped > DifferCharIndex) and
      (Typed[DifferCharIndex] = InDict[DifferCharIndex + 1]) and
      (Typed[DifferCharIndex + 1] = InDict[DifferCharIndex]) then
    begin
      ResultCandidate := ErrCount(CurErrLevel + 13, // 13 is a "magic" tunable constant
        Copy(InDict, DifferCharIndex + 2, nInDict - DifferCharIndex - 1),
        Copy(Typed, DifferCharIndex + 2, nTyped - DifferCharIndex - 1));
      ConsiderResultCandidate;
    end;
  end;

var
  i: Integer;
  Err: Integer;
  StringInDict: string;
  LocateInDict: string;
  ResultIsAmbiguous: Boolean;
begin
  Assert(Assigned(Strings));

  Result := '';

  if (Trim(Word) = '') or (ListIsSorted and IsWordInStrings(Word, Strings, ListIsSorted, DictionaryCaseMayDiffer)) then
    Exit;

  if DictionaryCaseMayDiffer then
    LocateInDict := AnsiLowerCase(Word)
  else
    LocateInDict := Word;
  Assert(Length(LocateInDict) > 0);

  MaxErrs := Length(Word) + 12; // Warning: Subjective magic number
  ResultIsAmbiguous := False;

  // Do a linear scan through the dictionary entries looking for matches
  for i := 0 to Strings.Count - 1 do
  begin
    StringInDict := Strings[i];

    Assert(Length(StringInDict) > 0);
    if not (FirstCharMustBeCorrect and (AnsiCompareText(StringInDict[1], LocateInDict[1]) <> 0)) then
    begin
      if DictionaryCaseMayDiffer then
        Err := ErrCount(0, AnsiLowerCase(StringInDict), LocateInDict)
      else
        Err := ErrCount(0, StringInDict, LocateInDict);

      // Possibly save the errorcount as the max error count
      if Err <= MaxErrs then
      begin
        if (Err = MaxErrs) and (Result <> StringInDict) and (NotEmpty(Result)) then
          ResultIsAmbiguous := True
        else
        begin
          MaxErrs := Err;
          Result := StringInDict;
          ResultIsAmbiguous := False;
        end;
      end;
    end;

    if Result = Word then
    begin
      Result := '';
      Exit;
    end;
  end;

  if ResultIsAmbiguous then
  begin
    {$IFOPT D+} SendDebugFmtEx('Ambiguity: %s --> %s or %s', [Word, Result, StringInDict], mtWarning); {$ENDIF}
    Result := '';
  end;
end;

function TProofreaderData.AddToDictionary(Dictionary: TReplacementSource;
  const Word: string; Resort: Boolean): Integer;
var
  Dict: TStringList;
begin
  Result := -1;
  Dict := GetDictionary(Dictionary);
  // No exception for duplicates since there is a bulk import process calling this
  if not IsWordInStrings(Word, Dict, True, True) then
  begin
    Result := Dict.Add(Word);
    if Resort then
      ResortDictionary(Dictionary);
  end;
end;

function TProofreaderData.GetDictionary(Dictionary: TReplacementSource): TStringList;
begin
  Result := FDictionaryLists[Dictionary];
  Assert(Assigned(Result));
end;

procedure TProofreaderData.SetDictionaryEntry(Dictionary: TReplacementSource;
  Index: Integer; const OldWord, NewWord: string);
var
  Dict: TStringList;
begin
  Dict := GetDictionary(Dictionary);
  ValidateDictionaryIndex(Dict, Index);
  if Dict[Index] <> OldWord then
    raise Exception.CreateFmt('Attempt to change node %d from %s to %s when original value is %s', [Index, OldWord, NewWord, Dict[Index]]);
  Dict[Index] := NewWord;
  ResortDictionary(Dictionary);
end;

procedure TProofreaderData.ResortDictionary(Dictionary: TReplacementSource);
begin
  SortStringsCaseSensitive(GetDictionary(Dictionary));
end;

function TProofreaderData.GetDictionaryCount(Dictionary: TReplacementSource): Integer;
begin
  Result := GetDictionary(Dictionary).Count;
end;

procedure TProofreaderData.DeleteDictionaryEntry(Dictionary: TReplacementSource;
  Index: Integer; const Word: string);
var
  Dict: TStringList;
begin
  Dict := GetDictionary(Dictionary);
  ValidateDictionaryIndex(Dict, Index);
  Assert(Dict[Index] = Word);
  Dict.Delete(Index);
end;

function TProofreaderData.GetDictionaryEntry(Dictionary: TReplacementSource; Index: Integer): string;
var
  Dict: TStringList;
begin
  Dict := GetDictionary(Dictionary);
  ValidateDictionaryIndex(Dict, Index);
  Result := Dict[Index];
end;

procedure TProofreaderData.ValidateDictionaryIndex(Dict: TStringList; Index: Integer);
begin
  if (Index < 0) or (Index >= Dict.Count) then
    raise Exception.Create('Invalid dictionary index requested');
end;

function TProofreaderData.AddToReplacements(Dictionary: TReplacementSource;
  const TypedString: string; ReplaceWhere: TGXWhereReplace;
  const ReplaceWithString: string): Integer;
var
  ReplacementList: TReplacementStringList;
begin
  ReplacementList := GetReplacements(Dictionary);
  if IsWordInStrings(AnsiUpperCase(TypedString), ReplacementList, True, True) then
    raise EProofreader.CreateFmt('There is already an AutoCorrect entry for "%s"', [TypedString])
  else
  begin
    Result := ReplacementList.Add(TypedString, ReplaceWhere, ReplaceWithString);
    ResortReplacements(ReplacementList);
  end;
end;

function TProofreaderData.GetReplacements(Dictionary: TReplacementSource): TReplacementStringList;
begin
  Result := FReplaceLists[Dictionary];
  Assert(Assigned(Result));
end;

procedure TProofreaderData.SetReplacementEntry(Dictionary: TReplacementSource;
  Index: Integer; const OldTypedString, NewTypedString: string;
  ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string);
var
  ReplacementList: TReplacementStringList;
  StoredTypedString: string;
begin
  ReplacementList := GetReplacements(Dictionary);
  ValidateReplacementIndex(ReplacementList, Index);
  StoredTypedString := (ReplacementList.Objects[Index] as TReplacementItem).Typed;
  if StoredTypedString <> OldTypedString then
    raise Exception.CreateFmt('Attempt to change node %d from %s to %s when original value is %s', [Index, OldTypedString, NewTypedString, StoredTypedString]);
  ReplacementList.Put(Index, NewTypedString, ReplaceWhere, ReplaceWithString);
  ResortReplacements(ReplacementList);
end;

procedure TProofreaderData.ResortReplacements(ReplacementList: TReplacementStringList);
begin
  Assert(Assigned(ReplacementList));
  SortStringsCaseSensitive(ReplacementList);
end;

function TProofreaderData.GetReplacementCount(Dictionary: TReplacementSource): Integer;
begin
  Result := GetReplacements(Dictionary).Count;
end;

procedure TProofreaderData.DeleteReplacementEntry(Dictionary: TReplacementSource;
  Index: Integer; const TypedString: string);
var
  ReplacementList: TReplacementStringList;
begin
  ReplacementList := GetReplacements(Dictionary);
  ValidateReplacementIndex(ReplacementList, Index);
  Assert(GetReplacementEntry(Dictionary, Index).Typed = TypedString, Format('"%s" should equal "%s"', [ReplacementList[Index], TypedString]));
  ReplacementList.Delete(Index);
end;

function TProofreaderData.GetReplacementEntry(Dictionary: TReplacementSource; Index: Integer): TReplacementItem;
var
  ReplacementList: TReplacementStringList;
begin
  ReplacementList := GetReplacements(Dictionary);
  ValidateReplacementIndex(ReplacementList, Index);
  Result := ReplacementList.Objects[Index] as TReplacementItem;
end;

procedure TProofreaderData.ValidateReplacementIndex(ReplacementList: TReplacementStringList; Index: Integer);
begin
  if (Index < 0) or (Index >= ReplacementList.Count) then
    raise Exception.Create('Invalid replacement index requested');
end;

function TProofreaderData.FindExactDictionary(const Zone: TReplacementSource; const AWord: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := FDictionaryLists[Zone].IndexOf(AWord);
  if (Index >= 0) and (FDictionaryLists[Zone][Index] = AWord) then
    Result := True;
end;

end.

