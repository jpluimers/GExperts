unit GX_ProofreaderCorrection;

{$I GX_CondDefine.inc}

interface

uses
  GX_ProofreaderData, GX_EditorChangeServices, GX_ProofreaderExpert;

type
  IAutoTypeWriterNotifier = interface(IGxEditorNotification)
  ['{D66A2AD2-27F4-11D4-A87C-000000000000}']
    procedure Detach;
  end;

function GetNewAutoTypeWriterNotifier(const ProofreaderData: TProofreaderData): IAutoTypeWriterNotifier;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Classes, ToolsAPI,
  GX_OtaUtils, GX_GenericUtils, GX_ProofreaderUtils,
  GX_EditorFormServices, GX_KibitzComp, GX_IdeUtils, StrUtils;

type
  IEditorPositionInformation = interface(IUnknown)
    ['{35CA9CB1-33E2-11D4-A8A7-000000000000}']
    function GetBufferCharIndex: Longint;
    function GetCharPos: TOTACharPos;
    function GetCursorPos: TOTAEditPos;
    function GetEditView: IOTAEditView;
    function GetRelativePlacement: Boolean;
    function GetRelativePosition: TOTAEditPos;
    procedure SetBufferCharIndex(const Value: Longint);
    procedure SetCharPos(const Value: TOTACharPos);
    procedure SetCursorPos(const Value: TOTAEditPos);
    procedure SetRelativePlacement(const Value: Boolean);
    procedure SetRelativePosition(const Value: TOTAEditPos);

    property EditView: IOTAEditView read GetEditView;
    property CursorPos: TOTAEditPos read GetCursorPos write SetCursorPos;
    property CharPos: TOTACharPos read GetCharPos write SetCharPos;
    property BufferCharIndex: Longint read GetBufferCharIndex write SetBufferCharIndex;

    property RelativePlacement: Boolean read GetRelativePlacement write SetRelativePlacement;
    property RelativePosition: TOTAEditPos read GetRelativePosition write SetRelativePosition;
  end;

  TEditorPositionInformation = class(TInterfacedObject, IEditorPositionInformation)
  private
    FEditView: IOTAEditView;
    FCursorPos: TOTAEditPos;
    FCharPos: TOTACharPos;
    FBufferCharIndex: Longint;
    FRelativePlacement: Boolean;
    FRelativePosition: TOTAEditPos;
    function GetBufferCharIndex: Longint;
    function GetCharPos: TOTACharPos;
    function GetCursorPos: TOTAEditPos;
    function GetEditView: IOTAEditView;
    function GetRelativePlacement: Boolean;
    function GetRelativePosition: TOTAEditPos;
    procedure SetBufferCharIndex(const Value: Longint);
    procedure SetCharPos(const Value: TOTACharPos);
    procedure SetCursorPos(const Value: TOTAEditPos);
    procedure SetRelativePlacement(const Value: Boolean);
    procedure SetRelativePosition(const Value: TOTAEditPos);
  public
    constructor Create(const EditView: IOTAEditView);
    destructor Destroy; override;
  end;

type
  TAutoTypeWriterNotifier = class(TNotifierObject, IGxEditorNotification, IAutoTypeWriterNotifier)
  private
    FChangeServiceNotifierIndex: Integer;
    FProofreaderData: TProofreaderData;
  private
    // When the proofreader corrects text it will
    // cause another editor notification message
    // to be sent. This might result in re-entrancy.
    // The FModifyingSelf Boolean flag prevents the
    // re-entrancy problem by exiting if True.
    FModifyingSelf: Boolean;
    //
    procedure AppendHistory(const CorrectionKind: TCorrectionKind;
                            const SourceLanguage: TReplacementSource;
                            const InfoString, OriginalText: string);
    procedure AppendAutoCorrectHistory(const SourceLanguage: TReplacementSource;
                                       const FromString, ToString: string);
    procedure AppendWordHistory(const SourceLanguage: TReplacementSource;
                                const FromString, ToString: string);
    procedure AppendKibitzHistory(const SourceLanguage: TReplacementSource;
                                  const FromString, ToString: string);
    //

    procedure BeepOnDemand;

    function DetermineReplacementSource(const Element: Integer;
      const SyntaxHighlighter: TGXSyntaxHighlighter; var Source: TReplacementSource): Boolean;
  private
    function PerformDictionaryReplacement(const SourceEditor: IOTASourceEditor;
      const EditorPositionInformation: IEditorPositionInformation;
      const ReplacementSourceTable: TReplacementSource;
      const TrailingCharacters, SourceString: string): Boolean;

    function PerformKibitzReplacement(const SourceEditor: IOTASourceEditor;
      const EditorPositionInformation: IEditorPositionInformation;
      const ReplacementSourceTable: TReplacementSource;
      const TrailingCharacters, SourceString, OriginalSourceString: string): Boolean;

    function PerformReplacementAtEnd(const SourceEditor: IOTASourceEditor;
      const EditorPositionInformation: IEditorPositionInformation;
      const ReplacementSourceTable: TReplacementSource;
      const TrailingCharacters, SourceString: string): Boolean;

    function PerformReplacementAtBeginning(const SourceEditor: IOTASourceEditor;
      const EditorPositionInformation: IEditorPositionInformation;
      const ReplacementSourceTable: TReplacementSource;
      const SourceString: string): Boolean;

    function SetupProofing(const SourceEditor: IOTASourceEditor;
      var EditorPositionInformation: IEditorPositionInformation;
      var ReplacementSourceTable: TReplacementSource;
      var SourceString: string): Boolean;
  protected
    // IGxEditorNotification
    procedure NewModuleOpened(const Module: IOTAModule);
    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: string);
    function EditorKeyPressed(const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
    function GetIndex: Integer;
  protected
    procedure Attach;
  public
    constructor Create(const Client: TProofreaderData);
    destructor Destroy; override;

    procedure Detach;
  end;

function GetNewAutoTypeWriterNotifier(const ProofreaderData: TProofreaderData): IAutoTypeWriterNotifier;
begin
  Result := TAutoTypeWriterNotifier.Create(ProofreaderData) as IAutoTypeWriterNotifier;
end;

{ TAutoTypeWriterNotifier}

constructor TAutoTypeWriterNotifier.Create(const Client: TProofreaderData);
begin
  inherited Create;

  FProofreaderData := Client;
  FChangeServiceNotifierIndex := -1;
  Attach;
end;

destructor TAutoTypeWriterNotifier.Destroy;
begin
  Detach;

  inherited Destroy;
end;

function TAutoTypeWriterNotifier.DetermineReplacementSource(const Element: Integer;
  const SyntaxHighlighter: TGXSyntaxHighlighter; var Source: TReplacementSource): Boolean;
const
  shsSource = [atWhiteSpace, atReservedWord, atIdentifier, atSymbol, atNumber,
    atFloat, atOctal, atHex, atCharacter, atIllegal, SyntaxOff];
begin
  Result := True;

  if (SyntaxHighlighter = gxpPAS) and (Element in shsSource) then
    Source := rtPasSrc
  else if (SyntaxHighlighter = gxpCPP) and (Element in shsSource) then
    Source := rtCPPSrc
  else if (SyntaxHighlighter = gxpCPP) and (Element in [atPreproc]) then
    Source := rtPreproc
  else if (SyntaxHighlighter = gxpSQL) and (Element in shsSource) then
    Source := rtSQLSrc
  else if (SyntaxHighlighter = gxpCS) and (Element in shsSource) then
    Source := rtCSSrc
  else if Element in [atAssembler] then
    Source := rtAssembler
  else if Element in [atString] then
    Source := rtString
  else if Element in [atComment] then
    Source := rtComment
  else
    Result := False
end;

procedure TAutoTypeWriterNotifier.AppendHistory(const CorrectionKind: TCorrectionKind;
  const SourceLanguage: TReplacementSource; const InfoString, OriginalText: string);
var
  Correction: TCorrectionItem;
begin
  try
    Correction := TCorrectionItem.Create;
    try
      Correction.CorrectionKind := CorrectionKind;
      Correction.SourceLanguage := SourceLanguage;
      Correction.OriginalText := OriginalText;
      Correction.InfoString := InfoString;
      Correction.Time := Now;
    except
      on E: Exception do
      begin
        FreeAndNil(Correction);
      end;
    end;
    FProofreaderData.History.Add(Correction);
  except
    on E: Exception do
    begin
      // Swallow exceptions
    end;
  end;
end;

procedure TAutoTypeWriterNotifier.AppendAutoCorrectHistory(const SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckAutoCorrection, SourceLanguage,
    Format('AutoCorrect: "%s" --> "%s"', [FromString, ToString]), FromString);
end;

procedure TAutoTypeWriterNotifier.AppendWordHistory(const SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckWord, SourceLanguage,
    Format('Dictionary Word: "%s" --> "%s"', [FromString, ToString]), FromString);
end;

procedure TAutoTypeWriterNotifier.AppendKibitzHistory(const SourceLanguage: TReplacementSource;
  const FromString, ToString: string);
begin
  AppendHistory(ckWord, SourceLanguage,
    Format('Compiler Correct: "%s" --> "%s"', [FromString, ToString]), FromString);
end;

procedure TAutoTypeWriterNotifier.BeepOnDemand;
begin
  if FProofreaderData.BeepOnReplace then
    Beep;
end;

function InternalNeedsReplacement(const ReplaceItem: TReplacementItem;
  const ReplaceString: string; Beginning: Boolean): Boolean;
var
  PartialReplaceString: string;
  CharIndex: Integer;
  PrevCharIsIdent: Boolean;
begin
  if ReplaceItem = nil then
  begin
    Result := False;
    Exit;
  end;

// todo: Fix ':;' -> ';'
// If I replace this ...
  PartialReplaceString := Copy(ReplaceString,
    Length(ReplaceString) - Length(ReplaceItem.Replace) + 1,
    Length(ReplaceItem.Replace));
// ... with this ...
//  PartialReplaceString := Copy(ReplaceString,
//    Length(ReplaceString) - Length(ReplaceItem.Typed) + 1,
//    Length(ReplaceItem.Typed));
// ... it is possible to automatically replace ':;' with ';'. Otherwise it won't work.
// But I am not sure whether this has negative side effects. In particular the part after "or"
// in the following statement does no longer make sense then.
 // --2018-06-03 twm

  Result := (AnsiCompareStr(ReplaceItem.Replace, PartialReplaceString) <> 0)
    or ((PartialReplaceString = '') and (ReplaceItem.Replace = '')); // Allow replace with nothing

  CharIndex := Length(ReplaceString) - Length(ReplaceItem.Typed);
  Assert(CharIndex >= 0);
  PrevCharIsIdent := (CharIndex > 0) and IsCharIdentifier(ReplaceString[CharIndex]);

  if Beginning then
    Result := Result and ((ReplaceItem.Where = wrAnywhere) or
      (ReplaceItem.Where = wrWordBegin) and (not PrevCharIsIdent))
  else
    Result := Result and ((ReplaceItem.Where = wrWordEnd) or
      (ReplaceItem.Where = wrWholeWord) and (not PrevCharIsIdent));
end;

function NeedsReplacementEnd(const ReplaceItem: TReplacementItem; const ReplaceString: string): Boolean;
begin
  Result := InternalNeedsReplacement(ReplaceItem, ReplaceString, False);
end;

function NeedsReplacementBegin(const ReplaceItem: TReplacementItem; const ReplaceString: string): Boolean;
begin
  Result := InternalNeedsReplacement(ReplaceItem, ReplaceString, True);
end;

// Adjusts the cursor position for the editor column based on
// the text that was replaced and the text that was inserted
// to substitute the replaced text.
procedure AdjustEditorPosition(const EditorPositionInformation: IEditorPositionInformation;
  const Replacement, ReplacedText: string);
var
  NewCursorPos: TOTAEditPos;
  ColumnOffset: Integer;
begin
  Assert(Assigned(EditorPositionInformation));

  NewCursorPos := EditorPositionInformation.CursorPos;

  if EditorPositionInformation.RelativePlacement then
  begin
    Inc(NewCursorPos.Col, EditorPositionInformation.RelativePosition.Col - Length(ReplacedText));
    Inc(NewCursorPos.Line, EditorPositionInformation.RelativePosition.Line);
  end
  else
  begin
    if EditorPositionInformation.CharPos.CharIndex > 0 then
    begin
      ColumnOffset := Length(Replacement) - Length(ReplacedText);
      Inc(NewCursorPos.Col, ColumnOffset);
    end;
  end;

  EditorPositionInformation.EditView.CursorPos := NewCursorPos;
  EditorPositionInformation.EditView.MoveViewToCursor;
  EditorPositionInformation.EditView.Paint;
end;

procedure ReplaceTextAt(const SourceEditor: IOTASourceEditor;
  const EditorPositionInformation: IEditorPositionInformation;
  const TextToReplace, Replacement, TrailingCharacters: string);
var
  EditWriter: IOTAEditWriter;
  EditorCharIndex: Longint;
begin
  Assert(Assigned(SourceEditor));
  Assert(Assigned(EditorPositionInformation));

  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  Assert(Assigned(EditWriter));

  EditorCharIndex := EditorPositionInformation.BufferCharIndex;
  EditWriter.CopyTo(EditorCharIndex - Length(TextToReplace) - Length(TrailingCharacters));
  EditWriter.DeleteTo(EditorCharIndex);
  // Note: Insert doesn't preserve trailing spaces
  EditWriter.Insert(PAnsiChar(ConvertToIDEEditorString(Replacement + TrailingCharacters)));

  EditWriter := nil; // Explicitly release interface here and now
end;

function CharPosAreEqual(const a, b: TOTACharPos): Boolean;
begin
  Result := (a.Line = b.Line) and (a.CharIndex = b.CharIndex);
end;

function TAutoTypeWriterNotifier.PerformDictionaryReplacement(const SourceEditor: IOTASourceEditor;
  const EditorPositionInformation: IEditorPositionInformation;
  const ReplacementSourceTable: TReplacementSource;
  const TrailingCharacters, SourceString: string): Boolean;
var
  ReplacementString: string;
begin
  Result := False;

  ReplacementString := FProofreaderData.FindDictionary(ReplacementSourceTable, SourceString);
  if ReplacementString <> '' then
  begin
    ReplaceTextAt(SourceEditor, EditorPositionInformation,
      SourceString, ReplacementString, TrailingCharacters);

    BeepOnDemand;

    AdjustEditorPosition(EditorPositionInformation, ReplacementString, SourceString);

    AppendWordHistory(ReplacementSourceTable, SourceString, ReplacementString);

    Result := True;
  end;
end;

function TAutoTypeWriterNotifier.PerformKibitzReplacement(const SourceEditor: IOTASourceEditor;
  const EditorPositionInformation: IEditorPositionInformation;
  const ReplacementSourceTable: TReplacementSource;
  const TrailingCharacters, SourceString, OriginalSourceString: string): Boolean;
var
  GxEditFormProxy: IGxEditFormProxy;
  CharPos: TOTACharPos;
  ReplacementString: string;
  slKibitz: TStrings;
  //OriginalPos: TOTAEditPos;
begin
  Result := False;

  GxEditFormProxy := GxEditorFormServices.CurrentProxy;
  Assert(Assigned(GxEditFormProxy));
  if not GxEditFormProxy.IsSourceEditor then
    Exit;

  // When using Kibitz OTA support, it only works well when the cursor position
  // matches the position you want to gather code completion for.  So we have to
  // either backup the cursor one character to the end of the previous identifier
  // (which breaks parameter hints), or limit the corrections to only happen
  // after whitespace is typed, when the IDE seems to correct better, so we only
  // support correcting in Delphi files after whitespace.
  if KibitzOta and (GxOtaGetCurrentSyntaxHighlighter(SourceEditor) = gxpPAS) and NotEmpty(OriginalSourceString) and (not IsCharWhiteSpace(RightStr(OriginalSourceString, 1)[1])) then
    Exit;

  Assert(Assigned(EditorPositionInformation));
  CharPos := EditorPositionInformation.CharPos;

  {
  // Moving of the cursor allows the more accurate IDE Kibitz info to be obtained,
  // but disables parameter insight, for example
  OriginalPos := EditorPositionInformation.EditView.CursorPos;
  if KibitzOta then // We need to back the cursor up one character, so it gets the code completion for the previous identifier
    GxOtaMoveEditCursorColumn(EditorPositionInformation.EditView, -1);

  try
  }
    slKibitz := TStringList.Create;
    try
      GetKibitzSymbols(SourceEditor, GxEditFormProxy.EditControl,
        EditorPositionInformation.EditView,
        CharPos.CharIndex - Length(TrailingCharacters), CharPos.Line, SourceString, TrailingCharacters, slKibitz);

      ReplacementString := FProofreaderData.FindInStrings(slKibitz, False, SourceString);
    finally
      FreeAndNil(slKibitz);
    end;
  {
  except
    if KibitzOta then
      EditorPositionInformation.EditView.CursorPos := OriginalPos;
    raise;
  end;
  }

  if ReplacementString <> '' then
  begin
    ReplaceTextAt(SourceEditor, EditorPositionInformation,
      SourceString, ReplacementString, TrailingCharacters);

    BeepOnDemand;

    AdjustEditorPosition(EditorPositionInformation, ReplacementString, SourceString);

    AppendKibitzHistory(ReplacementSourceTable, SourceString, ReplacementString);

    Result := True;
  end
  {
  else if KibitzOta then
    EditorPositionInformation.EditView.CursorPos := OriginalPos;
  }
end;

// Return the offset of the caret locator from left, top in string
// This routine is agnostic to line-break styles.
function RemoveCaretPositioningFromString(var s: string; out RelativeCaretCoord: TOTAEditPos): Boolean;
const
  CaretLocator = '|';
var
  CaretPosition: Integer;
  Iterator: Integer;
  Source: PChar;
  MultiByteSequenceLen: Cardinal;
begin
  CaretPosition := Pos(CaretLocator, s);
  Result := (CaretPosition > 0);
  if Result then
  begin
    RelativeCaretCoord.Line := 0;
    RelativeCaretCoord.Col := 0;

    Iterator := CaretPosition;

    Source := Pointer(S);
    while Iterator > 1 do // Do not count the CaretLocator itself - hence not > 0
    begin
      case Source^ of
        #10: begin
               RelativeCaretCoord.Col := 0;
               Inc(RelativeCaretCoord.Line);
               Dec(Iterator);
               Inc(Source);
             end;

        #13: begin
               RelativeCaretCoord.Col := 0;
               Inc(RelativeCaretCoord.Line);

               if Source[1] = #10 then
               begin
                 Dec(Iterator);
                 Inc(Source);
               end;
               Dec(Iterator);
               Inc(Source);
             end;
      else
        if IsLeadChar(Source^) then
        begin
          MultiByteSequenceLen := StrNextChar(Source) - Source;

          { TODO 4 -cIssue -oAnyone: Does the editor count a multi-byte character sequence
                  as a
                    * single column    -> Inc(Result.Col, 1);
                    * multiple columns -> Inc(Result.Col, MultiByteSequenceLen);
          }
          Inc(RelativeCaretCoord.Col);

          Dec(Iterator, MultiByteSequenceLen);
          Inc(Source, MultiByteSequenceLen)
        end
        else
        begin
          Inc(RelativeCaretCoord.Col);

          Dec(Iterator);
          Inc(Source);
        end;
      end;
    end;

    Delete(s, CaretPosition, Length(CaretLocator));
  end;
end;

function TAutoTypeWriterNotifier.PerformReplacementAtEnd(const SourceEditor: IOTASourceEditor;
  const EditorPositionInformation: IEditorPositionInformation;
  const ReplacementSourceTable: TReplacementSource;
  const TrailingCharacters, SourceString: string): Boolean;
var
  ReplaceItem: TReplacementItem;
  ReplacementString: string;
  ReplacedPartOfSource: string;
  CaretLocation: TOTAEditPos;
begin
  Result := False;

  ReplaceItem := FProofreaderData.FindReplacement(ReplacementSourceTable, SourceString);
  if NeedsReplacementEnd(ReplaceItem, SourceString) then
  begin
    ReplacementString := ReplaceItem.Replace;
    if RemoveCaretPositioningFromString(ReplacementString, CaretLocation) then
    begin
      EditorPositionInformation.RelativePlacement := True;
      Dec(CaretLocation.Col, Length(TrailingCharacters));
      EditorPositionInformation.RelativePosition := CaretLocation;
    end;

    ReplaceTextAt(SourceEditor, EditorPositionInformation,
      ReplaceItem.Typed, ReplacementString, TrailingCharacters);

    BeepOnDemand;

    AdjustEditorPosition(EditorPositionInformation, ReplaceItem.Replace, ReplaceItem.Typed);

    ReplacedPartOfSource := Copy(SourceString,
      Length(SourceString) - Length(ReplaceItem.Typed) + 1, Length(SourceString));

    AppendAutoCorrectHistory(ReplacementSourceTable, ReplacedPartOfSource, ReplaceItem.Replace);

    Result := True;
  end;
end;

function TAutoTypeWriterNotifier.PerformReplacementAtBeginning(const SourceEditor: IOTASourceEditor;
  const EditorPositionInformation: IEditorPositionInformation;
  const ReplacementSourceTable: TReplacementSource;
  const SourceString: string): Boolean;
var
  ReplaceItem: TReplacementItem;
  ReplacementString: string;
  ReplacedPartOfSource: string;
  CaretLocation: TOTAEditPos;
begin
  Result := False;

  // Checking SourceString for needing replacement
  ReplaceItem := FProofreaderData.FindReplacement(ReplacementSourceTable, SourceString);
  if NeedsReplacementBegin(ReplaceItem, SourceString) then
  begin
    ReplacementString := ReplaceItem.Replace;
    if RemoveCaretPositioningFromString(ReplacementString, CaretLocation) then
    begin
      EditorPositionInformation.RelativePlacement := True;
      EditorPositionInformation.RelativePosition := CaretLocation;
    end;

    ReplaceTextAt(SourceEditor, EditorPositionInformation, ReplaceItem.Typed, ReplacementString, '');
    BeepOnDemand;

    if EditorPositionInformation.RelativePlacement then
      AdjustEditorPosition(EditorPositionInformation, ReplacementString, ReplaceItem.Typed);

    ReplacedPartOfSource := Copy(SourceString,
      Length(SourceString) - Length(ReplaceItem.Typed) + 1, Length(SourceString));

    AppendAutoCorrectHistory(ReplacementSourceTable, ReplacedPartOfSource, ReplaceItem.Replace);

    Result := True;
  end;
end;

function TAutoTypeWriterNotifier.SetupProofing(const SourceEditor: IOTASourceEditor;
  var EditorPositionInformation: IEditorPositionInformation;
  var ReplacementSourceTable: TReplacementSource;
  var SourceString: string): Boolean;
var
  SyntaxHighlighter: TGXSyntaxHighlighter;
  Element: Integer;
  Element2: Integer;
  LineFlag: Integer;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  Result := False;

  if not CharPosAreEqual(SourceEditor.BlockStart, SourceEditor.BlockAfter) then
    Exit;

  // Editor change notifiers may fire if the IDE auto-creates some
  // code behind the scenes. Since we only want to react on user
  // input, we exit if there is no edit view active.
  if not GxOtaTryGetTopMostEditView(SourceEditor, EditView) then
    Exit;

  EditorPositionInformation := TEditorPositionInformation.Create(EditView);
  EditorPositionInformation.CursorPos := EditView.CursorPos;

  // Convert from a CursorPos to a CharPos
  CursorPos := EditorPositionInformation.CursorPos;
  EditView.ConvertPos(True, CursorPos, CharPos);
  EditorPositionInformation.CharPos := CharPos;

  // Read the part of the text in the current source line
  // that is located in front of the current cursor position.
  SourceString := GxOtaGetPreceedingCharactersInLine(EditView);
  if IsEmpty(SourceString) then
    Exit;

  EditView.GetAttributeAtPos(CursorPos, False, Element, LineFlag);

  // Get the element type of one char to the left:
  Dec(CursorPos.Col);
  EditView.GetAttributeAtPos(CursorPos, False, Element2, LineFlag);

  if Element = atWhiteSpace then
  begin
    if GxOtaIsWhiteSpaceInComment(EditView, EditorPositionInformation.CursorPos) then
      Element := atComment
    else if GxOtaIsWhiteSpaceInString(EditView, EditorPositionInformation.CursorPos) then
      Element := atString;
  end;

  // Don't mess with compiler directives or comments:
  if (Element = atWhiteSpace) and (Element2 in [atComment, atPreproc]) then
  begin
    Exit;
  end;

  SyntaxHighlighter := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
  if not DetermineReplacementSource(Element, SyntaxHighlighter, ReplacementSourceTable) then
    Exit;

  Result := True;
end;

procedure TAutoTypeWriterNotifier.SourceEditorModified(const SourceEditor: IOTASourceEditor);
var
  TrailingChars: string;
  SourceString: string;
  OriginalSourceString: string;
  EditorPositionInformation: IEditorPositionInformation;
  ReplacementSourceTable: TReplacementSource;
  n: Integer;
  DoCompiler: Boolean;
begin
  if FModifyingSelf then
    Exit;
  FModifyingSelf := True;

  try
    if IsReplaceConfirmDialogOnScreen then
      Exit;

    // Only correct when the active form is an IDE TEditWindow.
    // This prevents some unwanted corrections when other experts,
    // the form designer, or the IDE features make changes.
    if not GxOtaCurrentlyEditingSource then
      Exit;

    try
      if not FProofreaderData.ReplacerActive and not FProofreaderData.DictionaryActive and
         not FProofreaderData.CompilerActive then
      begin
        Exit;
      end;

      if not SetupProofing(
        // In:
        SourceEditor,
        // Out:
        EditorPositionInformation, ReplacementSourceTable, SourceString) then
      begin
        Exit;
      end;
      OriginalSourceString := SourceString;

      // No point in continuing checking for a correction of an empty string
      if Length(SourceString) = 0 then
        Exit;

      // Check the replacement table for matches not requiring a
      // whole word match ("AutoCorrect" functionality)
      if FProofreaderData.ReplacerActive then
      begin
        if PerformReplacementAtBeginning(SourceEditor, EditorPositionInformation,
          ReplacementSourceTable, SourceString) then
        begin
          Exit;
        end;
      end;

      // The rest of the replacement tests replace whole words only
      // and cannot possibly match if the last char is alphanumeric
      n := Length(SourceString); // We know from the Exit above that Length(SourceString) > 0.

      // Check the last character in the buffer that we copied.
      //
      // Usually[*] this will be the character that the user typed
      // into the editor buffer. [*] The user may have pasted a long text.
      //
      // If that last character is in the set of alpha-numeric characters,
      // the user is not done typing the word, identifier, or statement.
      // Do not make an attempt to correct anything in that case.
      if IsCharIdentifier(SourceString[n]) then
        Exit;

      // We now split the buffer we retrieved into two halves:
      // * At the end of the buffer, we find what the user just typed in
      //   (almost always a single char - see above)
      // * At the beginning of the buffer we find a completed word
      //   that we look at for proofing.

      // Only copy the LAST character; this is slightly wrong, as the user
      // may have pasted the string "(i: Integer)" following "showmessage".
      // In this case, we simply do not detect that the word "showmessage"
      // has been completed and needs to be corrected.
      // Unfortunately, we have no means to do it any better, so it will
      // have to remain slightly wrong for the time being.
      TrailingChars := Copy(SourceString, n, n);
      Delete(SourceString, n, n);
      if (Length(SourceString) = 0) or
         not (IsCharIdentifier(SourceString[Length(SourceString)])) then
      begin
        Exit;
      end;

      if FProofreaderData.ReplacerActive then
      begin
        if PerformReplacementAtEnd(SourceEditor, EditorPositionInformation,
          ReplacementSourceTable, TrailingChars, SourceString) then
        begin
          EditorPositionInformation.EditView.Paint;
          CodeProofreaderExpert.IncCallCount;
          Exit;
        end;
      end;

      // Make sure that we only have alphanumeric characters left in s by
      // stripping out all leading non-alphanumeric characters.
      n := Length(SourceString);
      while (n > 0) and IsCharIdentifier(SourceString[n]) do
        Dec(n);

      Delete(SourceString, 1, n);

      // At this stage, s contains the word that we are
      // looking at for proofreading. c contains all the
      // characters that follow that word on this line -
      // typically (if not always), this is only a single
      // character, and typically it will be a space or bracket.
      // Example:
      //   Assuming that we read the character sequence
      //     "  showmessage("
      //   from the editor buffer, the variables s and c have
      //   the values
      //     s = 'showmessage'
      //     c = '('

      if FProofreaderData.CompilerActive and
         (ReplacementSourceTable in [rtPasSrc, rtCPPSrc]) then
      begin
        DoCompiler := True;
        // If the dictionary contains the typed word, don't bother with compiler
        // replacement, since the dictionary serves as an override to not
        // correct things the compiler might be bad at, like keywords (then, etc.)
        // We could also ignore certain element types, such as atReservedWord, atNumber, etc.
        if FProofreaderData.DictionaryActive then
          DoCompiler := not FProofreaderData.FindExactDictionary(ReplacementSourceTable, SourceString);
        if DoCompiler and PerformKibitzReplacement(SourceEditor, EditorPositionInformation,
          ReplacementSourceTable, TrailingChars, SourceString, OriginalSourceString) then
        begin
          EditorPositionInformation.EditView.Paint;
          CodeProofreaderExpert.IncCallCount;
          Exit;
        end;
      end;

      if FProofreaderData.DictionaryActive then
      begin
        if PerformDictionaryReplacement(SourceEditor, EditorPositionInformation,
          ReplacementSourceTable, TrailingChars, SourceString) then
        begin
          EditorPositionInformation.EditView.Paint;
          CodeProofreaderExpert.IncCallCount;
          // Exit;
        end;
      end;

    except
      on E: Exception do
      begin
        GxLogException(E, 'Proofreader exception in SourceEditorModified');
        {$IFOPT D+} SendDebug('Proofreader exception: ' + E.Message); {$ENDIF}
        // Swallow exception
      end;
    end;
  finally
    FModifyingSelf := False;
  end;
end;

procedure TAutoTypeWriterNotifier.FormEditorModified(const FormEditor: IOTAFormEditor);
begin
  //{$IFOPT D+} SendDebug('Proofreader: Form modified'); {$ENDIF}
end;

procedure TAutoTypeWriterNotifier.NewModuleOpened(const Module: IOTAModule);
begin
  //{$IFOPT D+} SendDebug('Proofreader: New module opened'); {$ENDIF}
end;

procedure TAutoTypeWriterNotifier.Detach;
begin
  GxEditorChangeServices.RemoveNotifierIfNecessary(FChangeServiceNotifierIndex);
end;

procedure TAutoTypeWriterNotifier.Attach;
begin
  Assert(FChangeServiceNotifierIndex = -1);
  FChangeServiceNotifierIndex := GxEditorChangeServices.AddNotifier(Self);
end;

procedure TAutoTypeWriterNotifier.ComponentRenamed(
  const FormEditor: IOTAFormEditor; Component: IOTAComponent;
  const OldName, NewName: string);
begin
  // Nothing
end;

function TAutoTypeWriterNotifier.GetIndex: Integer;
begin
  Result := FChangeServiceNotifierIndex;
end;

function TAutoTypeWriterNotifier.EditorKeyPressed(
  const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
begin
  Result := False;
  // Do nothing for now
end;

{ TEditorPositionInformation }

const
  InvalidBufferCharIndex = -1;

constructor TEditorPositionInformation.Create(const EditView: IOTAEditView);
begin
  inherited Create;

  FBufferCharIndex := InvalidBufferCharIndex;
  FEditView := EditView;
end;

destructor TEditorPositionInformation.Destroy;
begin
  FEditView := nil;

  inherited Destroy;
end;

function TEditorPositionInformation.GetBufferCharIndex: Longint;
begin
  Result := FBufferCharIndex;

  if Result = InvalidBufferCharIndex then
  begin
    Assert(Assigned(FEditView));
    // We perform a delayed calculation of the character index
    // and remember it for the future.
    // For D4 we were forced to determine BufferCharIndex at a
    // much earlier state - we have a cached copy of that
    // calculation already.

    Result := FEditView.CharPosToPos(FCharPos);
    FBufferCharIndex := Result;
  end;
end;

function TEditorPositionInformation.GetCharPos: TOTACharPos;
begin
  Result := FCharPos;
end;

function TEditorPositionInformation.GetCursorPos: TOTAEditPos;
begin
  Result := FCursorPos;
end;

function TEditorPositionInformation.GetEditView: IOTAEditView;
begin
  Result := FEditView;
end;

function TEditorPositionInformation.GetRelativePlacement: Boolean;
begin
  Result := FRelativePlacement;
end;

function TEditorPositionInformation.GetRelativePosition: TOTAEditPos;
begin
  Result := FRelativePosition;
end;

procedure TEditorPositionInformation.SetBufferCharIndex(const Value: Longint);
begin
  FBufferCharIndex := Value;
end;

procedure TEditorPositionInformation.SetCharPos(const Value: TOTACharPos);
begin
  FCharPos := Value;
end;

procedure TEditorPositionInformation.SetCursorPos(const Value: TOTAEditPos);
begin
  FCursorPos := Value;
end;

procedure TEditorPositionInformation.SetRelativePlacement(const Value: Boolean);
begin
  FRelativePlacement := Value;
end;

procedure TEditorPositionInformation.SetRelativePosition(const Value: TOTAEditPos);
begin
  FRelativePosition := Value;
end;

end.

