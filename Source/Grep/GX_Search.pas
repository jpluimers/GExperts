unit GX_Search;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils, Classes, ToolsAPI,
  GX_EditReader, GX_MessageBox;

type
  TSearchOption = (soCaseSensitive, soWholeWord, soRegEx);

  TSearchOptions = set of TSearchOption;

  TFileComment = (fcNone, fcPas, fcCPP);

  TFoundEvent = procedure(Sender: TObject; LineNo: Integer; const Line: string; SPos, FEditReaderPos: Integer) of object;

  ELineTooLong = class(Exception);

  TLineTooLongMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

  // We separate the grep code from the file management code in TSearcher
  TBaseSearcher = class(TObject)
  private
    procedure SetANSICompatible(const Value: Boolean);
    procedure SetBufSize(New: Integer);
  protected
    FOnFound: TFoundEvent;
    FOnStartSearch: TNotifyEvent;
    procedure SignalStartSearch; virtual;
    procedure SignalFoundMatch(LineNo: Integer; const Line: string; SPos, FEditReaderPos: Integer); virtual;
  protected
    BLine: PAnsiChar; // The current search line, case-converted if requested
    OrigLinePos: array of Integer;
    OrgLine: PAnsiChar; // The current search line, without case-conversion
    FLineNo: Integer;
    FEof: Boolean;
    FSearchBuffer: PAnsiChar;
    FBufSize: Integer;
    FBufferSearchPos: Integer;
    FBufferDataCount: Integer;
    FSearchLineStart: Integer;
    FSearchInCode: Boolean;
    FSearchInStrings: Boolean;
    FSearchInComments: Boolean;
    FSectionInterface: Boolean;
    FSectionImplementation: Boolean;
    FSectionInitialization: Boolean;
    FSectionFinalization: Boolean;
    FCurlyCommentActive: Boolean;
    FSearchOptions: TSearchOptions;
    FStarCommentActive: Boolean;
    FSlashCommentActive: Boolean;
    FQuoteActive : Boolean;
    FDoubleQuoteActive: Boolean;
    FPattern: PAnsiChar;
    FFileName: string;
    LoCase: function(const Ch: AnsiChar): AnsiChar;
    procedure DoSearch(FileComment: TFileComment);
    procedure FillBuffer;
    procedure PatternMatch;
    procedure ReadIntoBuffer(AmountOfBytesToRead: Cardinal); virtual; abstract;
    procedure Seek(Offset: Longint; Direction: Integer); virtual; abstract;
    procedure AfterFill; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPattern(const Source: AnsiString);
    property ANSICompatible: Boolean write SetANSICompatible;
    property BufSize: Integer read FBufSize write SetBufSize;
    property Code: Boolean read FSearchInCode write FSearchInCode;
    property Strings: Boolean read FSearchInStrings write FSearchInStrings;
    property Comments: Boolean read FSearchInComments write FSearchInComments;
    property SectionInterface: Boolean read FSectionInterface write FSectionInterface;
    property SectionImplementation: Boolean read FSectionImplementation write FSectionImplementation;
    property SectionInitialization: Boolean read FSectionInitialization write FSectionInitialization;
    property SectionFinalization: Boolean read FSectionFinalization write FSectionFinalization;
    property Pattern: PAnsiChar read FPattern;
    property SearchOptions: TSearchOptions read FSearchOptions write FSearchOptions;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property OnStartSearch: TNotifyEvent read FOnStartSearch write FOnStartSearch;
  end;

  TSearcher = class(TBaseSearcher)
  private
    FEditorIntf: IOTASourceEditor;
    FModuleIntf: IOTAModule;
    FEditReader: IOTAEditReader;
    FSearchStream: TStream;
    FEditReaderPos: Integer;
    FMode: TModuleMode;
    FIncludeForms: Boolean;
    procedure Reset;
  protected
    procedure SetFileName(const Value: string);
    procedure SearchForm(ExactFileName: Boolean);
    procedure FreeObjects;
  protected
    procedure ReadIntoBuffer(AmountOfBytesToRead: Cardinal); override;
    procedure Seek(Offset: Longint; Direction: Integer); override;
    procedure AfterFill; override;
  public
    constructor Create(const SearchFileName: string);
    destructor Destroy; override;
    procedure Execute;
    property FileName: string read FFileName write SetFileName;
    property IncludeForms: Boolean read FIncludeForms write FIncludeForms;
    property Mode: TModuleMode read FMode;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows,
  GX_GenericUtils, GX_OtaUtils;

const
  // Pattern matching tokens
  opChar = Char(1);
  opBOL = Char(2);
  opEOL = Char(3);
  opAny = Char(4);
  opClass = Char(5);
  opNClass = Char(6);
  opAlpha = Char(7);
  opDigit = Char(8);
  opAlphaNum = Char(9);
  opPunct = Char(10);
  opRange = Char(11);
  opEndPat = Char(12);

  LastPatternChar = opEndPat;

  GrepPatternSize = 1024;
  SearchLineSize = 31 * 1024; // Max editor line length: D6: 1K, BDS 2006: 4K
  DefaultBufferSize = 31 * 1024;

{ Generic routines }

function ANSILoCase(const Ch: AnsiChar): AnsiChar;
var
  w: Word;
begin
  w := MakeWord(Ord(Ch), 0);
  CharLower(PChar(@w));
  Result := AnsiChar(Lo(w));
end;

function ASCIILoCase(const Ch: AnsiChar): AnsiChar;
const
  ASCIIUpperAlphaChars  = ['A'..'Z'];
begin
  if Ch in ASCIIUpperAlphaChars then
    Result := AnsiChar(Ord(Ch) + 32)
  else
    Result := Ch;
end;

{ TSearcher }

constructor TSearcher.Create(const SearchFileName: string);
begin
  inherited Create;

  FMode := mmModule;

  if SearchFileName <> '' then
    SetFileName(SearchFileName);
end;

destructor TSearcher.Destroy;
begin
  if Mode = mmFile then
    FreeAndNil(FSearchStream)
  else
  begin
    FEditReader := nil;
    FEditorIntf := nil;
    FModuleIntf := nil;
  end;

  inherited Destroy;
end;

procedure TSearcher.SearchForm(ExactFileName: Boolean);
var
  FormIntf: IOTAFormEditor;
  Editor: IOTASourceEditor;
  FormStream: TStream;
  Buf: array[0..2] of Byte;
  KeepStream: Boolean;
  Bytes: Integer;
begin
  if RunningInsideIDE then
    FModuleIntf := GxOtaGetModule(FFileName);
  if FModuleIntf <> nil then // The module is open in the IDE
  begin
    FMode := mmModule;
    FormIntf := GxOtaGetFormEditorFromModule(FModuleIntf);
    if FormIntf <> nil then
    begin
      FreeObjects;
      FSearchStream := TMemoryStream.Create;
      GxOtaGetFormAsText(FormIntf, FSearchStream);
      FSearchStream.Position := 0;
      FMode := mmFile;
      FFileName := FormIntf.FileName;
    end
    else // Is the form opened as text already and doesn't have a form interface?
    begin
      Editor := GxOtaGetSourceEditorFromModule(FModuleIntf);
      if Assigned(Editor) and IsForm(Editor.FileName) then
      begin
        // See TSearcher.SetFileName.GetModuleInterface for comments on this hack
        FFileName := Editor.FileName;
        FEditReader := nil;
        FEditReader := Editor.CreateReader;
        Reset;
        DoSearch(fcNone);
      end;
    end
  end
  else  // The module is not open in the IDE
  begin
    FreeObjects;
    // Prefer DFMs here?  What if both exist?
    if not ExactFileName then
    begin
      FFileName := ChangeFileExt(FFileName, '.dfm');
      if not FileExists(FileName) then
        FFileName := ChangeFileExt(FFileName, '.nfm');
      if not FileExists(FileName) then
        FFileName := ChangeFileExt(FFileName, '.xfm');
    end;
    if FileExists(FFileName) then
    begin
      KeepStream := False;
      FormStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      try
        FormStream.Position := 0;
        Bytes := FormStream.Read(Buf, SizeOf(Buf));
        FormStream.Position := 0;
        // If we have a binary format form, we convert it to text
        if Bytes >= 3 then begin
          if (Buf[0] = $FF) and (Buf[1] = $0A) and (Buf[2] = $00) then
          begin
            Assert(not Assigned(FSearchStream));
            FSearchStream := TMemoryStream.Create;
            try
              ObjectResourceToText(FormStream, FSearchStream);
            except
              on E: Exception do
              begin
                GxLogAndShowException(E, Format('Error during text conversion of form %s:%s', [FFileName, sLineBreak + E.Message]));
                Exit;
              end;
            end;
          end
          else
          begin
            Assert(not Assigned(FSearchStream));
            FSearchStream := FormStream;
            KeepStream := True;
          end;
        end;
      finally
        if not KeepStream then
          FreeAndNil(FormStream);
      end;
    end;
  end;

  if FSearchStream <> nil then
  begin
    Reset;
    DoSearch(fcNone);
  end;
end;

procedure TSearcher.FreeObjects;
begin
  if FFileName <> '' then
  begin
    FreeAndNil(FSearchStream);
    FEditReader := nil;
    FEditorIntf := nil;
    FModuleIntf := nil;
  end;
end;

procedure TSearcher.SetFileName(const Value: string);

  function GetModuleInterface: Boolean;
  var
    UpperFileExt: string;
    //ReaderStream: TGxEditorReadStream;
    //Lst: TStringList;
    //i: Integer;
  begin
    Result := False;

    // Get Editor Interface
    FModuleIntf := GxOtaGetModule(FFileName);
    if FModuleIntf <> nil then
    begin
      FMode := mmModule;

      FEditorIntf := GxOtaGetSourceEditorFromModule(FModuleIntf, FFileName);
      if FEditorIntf = nil then
        Exit;

      // Sometimes we've obtained a pas module interface, but the text in the
      // editor is really the text of the module's form.  When this happens,
      // "correct" the filename.  Note there appears to be no way to get the
      // actual source module's text when in this situation.  This is a bad
      // hack, and should be fixed some other way.  Also see SearchForm above.
      UpperFileExt := ExtractUpperFileExt(FFileName);
      if StringInArray(UpperFileExt, ['.PAS', '.CPP', '.H']) and IsForm(FEditorIntf.FileName) then
        FFileName := FEditorIntf.FileName;

      // Get Reader interface
      FEditReader := FEditorIntf.CreateReader;
      if FEditReader = nil then
        Exit;
      Result := True;
    end;
  end;

  function GetFileInterface: Boolean;
  begin
    Result := False;
    if not FileExists(FFileName) then
      Exit;

    FMode := mmFile;
    try
      Assert(not Assigned(FSearchStream));
      FSearchStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      Result := True;
    except
      on E: Exception do
      begin
        {$IFOPT D+} SendDebugError('Grep Error: GetFileInterface - ' + E.Message); {$ENDIF}
      end;
    end;
  end;

begin
  FreeObjects;
  FFileName := Value;

  if RunningInsideIDE then
  begin
    if not GetModuleInterface and not GetFileInterface then
      FFileName := '';
  end
  else
    if not GetFileInterface then FFileName := '';

  if FFileName <> '' then
    Reset;
end;

procedure TSearcher.Reset;
resourcestring
  SSearcherReset = 'Reset exception:' + sLineBreak;
begin
  try
    if FFileName = '' then
      Exit;

    FEditReaderPos := 0;
    FBufferSearchPos := 0;
    FBufferDataCount := 0;
    FLineNo := 0;
    FEof := False;
    FCurlyCommentActive := False;
    FStarCommentActive := False;
    FSlashCommentActive := False;
    FQuoteActive := False;
    FDoubleQuoteActive := False;

    if FMode = mmFile then
      FSearchStream.Position := 0;
  except
    on E: Exception do
      GxLogAndShowException(E, SSearcherReset);
  end;
end;

procedure TSearcher.Execute;
var
  UpperFileExt: string;
begin
  Reset;

  UpperFileExt := ExtractUpperFileExt(FFileName);
  {$IFOPT D+} SendDebug('Grep: Searching file ' + FFileName); {$ENDIF}

  if IsForm(FFileName) then
    SearchForm(True)
  else
  begin
    if StringInArray(UpperFileExt, ['.PAS', '.INC', '.DPR']) then
      DoSearch(fcPas)
    else if StringInArray(UpperFileExt, ['.CPP', '.C', '.HPP', '.H']) then
      DoSearch(fcCPP)
    else
      DoSearch(fcNone);

    if FIncludeForms and StringInArray(UpperFileExt, ['.PAS', '.CPP']) then
      SearchForm(False);
  end;
end;

procedure TSearcher.ReadIntoBuffer(AmountOfBytesToRead: Cardinal);
begin
  if Mode = mmFile then
    FBufferDataCount := FSearchStream.Read(FSearchBuffer^, AmountOfBytesToRead)
  else
  begin
    Assert(Assigned(FEditReader), 'No FEditReader in TSearcher.ReadIntoBuffer for: ' + FileName);
    FBufferDataCount := FEditReader.GetText(FEditReaderPos, FSearchBuffer, AmountOfBytesToRead);
  end;
end;

procedure TSearcher.Seek(Offset, Direction: Integer);
begin
  if Mode = mmFile then
    FSearchStream.Seek(Offset, Direction);
end;

procedure TSearcher.AfterFill;
begin
  // Adapt current "end" position of *IDE* reading stream
  // to the new stream (after potentially correcting
  // for partially read lines
  Inc(FEditReaderPos, FBufferDataCount);
end;

{ TBaseSearcher }

constructor TBaseSearcher.Create;
begin
  inherited Create;

  FBufSize := DefaultBufferSize;
  BLine := AnsiStrAlloc(SearchLineSize);
  SetLength(OrigLinePos, SearchLineSize);
  OrgLine := AnsiStrAlloc(SearchLineSize);
  FPattern := AnsiStrAlloc(GrepPatternSize);
  LoCase := ASCIILoCase;
end;

destructor TBaseSearcher.Destroy;
begin
  StrDispose(FSearchBuffer);
  FSearchBuffer := nil;

  SetLength(OrigLinePos, 0);

  StrDispose(BLine);
  BLine := nil;

  StrDispose(OrgLine);
  OrgLine := nil;

  StrDispose(FPattern);
  FPattern := nil;

  inherited Destroy;
end;

procedure TBaseSearcher.SetANSICompatible(const Value: Boolean);
begin
  if Value then
    LoCase := ANSILoCase
  else
    LoCase := ASCIILoCase;
end;

procedure TBaseSearcher.FillBuffer;
resourcestring
  SLineLengthError = 'Grep detected a line longer than %d characters in:'+sLineBreak+
                     '%s.' +sLineBreak+
                     'Likely, this is an unsupported binary file type.';
var
  AmountOfBytesToRead: Integer;
  SkippedCharactersCount: Integer;
  LineEndScanner: PAnsiChar;
begin
  if FSearchBuffer = nil then
    FSearchBuffer := AnsiStrAlloc(FBufSize);
  FSearchBuffer[0] := #0;

  // Read at most (FBufSize - 1) bytes
  AmountOfBytesToRead := FBufSize - 1;

  ReadIntoBuffer(AmountOfBytesToRead);

  FEof := (FBufferDataCount = 0);

  // Reset buffer position to zero
  FBufferSearchPos := 0;

  // If we filled our buffer completely, there is a chance that
  // the last line was read only partially.
  // Since our search algorithm is line-based,
  // skip back to the end of the last completely read line.
  if FBufferDataCount = AmountOfBytesToRead then
  begin
    // Get pointer on last character of read data
    LineEndScanner := FSearchBuffer + FBufferDataCount - 1;
    // We have not skipped any characters yet
    SkippedCharactersCount := 0;
    // While we still have data in the buffer,
    // do scan for a line break as characterised
    // by a #13#10 or #10#13 or a single #10.
    // Which sequence exactly we hit is not important,
    // we just need to find and line terminating
    // sequence.
    while FBufferDataCount > 0 do
    begin
      if LineEndScanner^ = #10 then
      begin
        Seek(-SkippedCharactersCount, soFromCurrent);

        // Done with finding last complete line
        Break;
      end;

      Inc(SkippedCharactersCount);
      Dec(FBufferDataCount);
      Dec(LineEndScanner);
    end;

    // With FBufferPos = 0 we have scanned back in our
    // buffer and not found any line break; this means
    // that we cannot employ our pattern matcher on a
    // complete line -> Internal Error.
    if FBufferDataCount = 0 then
      raise ELineTooLong.CreateFmt(SLineLengthError, [FBufSize - 1, FFileName]);
  end;

  AfterFill;

  // Cut off everything beyond the line break
  // Assert(FBufferDataCount >= 0);
  FSearchBuffer[FBufferDataCount] := #0;
end;

type
  TUnitSection = (usHeader, usInterface, usImplementation, usInitialization, usFinalization, usEnd);
  TUnitSections = set of TUnitSection;
  
const
  CSections: array [TUnitSection] of AnsiString = (
    '',
    'interface',
    'implementation',
    'initialization',
    'finalization',
    'end.');

procedure TBaseSearcher.DoSearch(FileComment: TFileComment);
var
  i: Integer;
  LPos: Integer;
  UseChar: Boolean;
  ActiveSection: TUnitSection;
  SectionsToSearch: TUnitSections;
begin
  // Determine which sections are to be scanned :
  SectionsToSearch := [usHeader]; // Everything before 'interface' is always considered
  if FSectionInterface then
    Include(SectionsToSearch, usInterface);
  if FSectionImplementation then
    Include(SectionsToSearch, usImplementation);
  if FSectionInitialization then
    Include(SectionsToSearch, usInitialization);
  if FSectionFinalization then
    Include(SectionsToSearch, usFinalization);
  {if FSectionEndDot then}
    Include(SectionsToSearch, usEnd); // Everything past 'end.' is always considered

  // Assume scanning starts in the header of the unit :
  ActiveSection := usHeader;

  SignalStartSearch;

  while not FEof do
  begin
    // Read new data in
    if (FBufferSearchPos >= FBufferDataCount) or (FBufferDataCount = 0) then
    begin
      try
        FillBuffer;
      except on E: ELineTooLong do
        begin
          ShowGxMessageBox(TLineTooLongMessage, E.Message);
          Exit;
        end;
      end;
    end;
    if FEof then
      Exit;
    LPos := 0;
    FSearchLineStart := FBufferSearchPos;
    i := FBufferSearchPos;
    UseChar := False;
    while i < FBufferDataCount do
    begin
      case FSearchBuffer[i] of
        #0:
          begin
            FBufferSearchPos := FBufferDataCount + 1;
            Break;
          end;
        #10:
          begin
            FBufferSearchPos := i + 1;

            FSlashCommentActive := False;
            if FileComment = fcCPP then
            begin
              if (i > 1) and (FSearchBuffer[i - 1] <> '\') then
              begin
                FDoubleQuoteActive := False;
                FQuoteActive := False;
              end;
            end
            else
              FQuoteActive := False;

            Break;
          end;
        #13:
          begin
            FBufferSearchPos := i + 1;
            if FSearchBuffer[FBufferSearchPos] = #10 then
              Inc(FBufferSearchPos);

            FSlashCommentActive := False;
            if FileComment = fcCPP then
            begin
              if (i > 1) and (FSearchBuffer[i - 1] <> '\') then
              begin
                FDoubleQuoteActive := False;
                FQuoteActive := False;
              end;
            end
            else
              FQuoteActive := False;

            Break;
          end;
      else // case FSearchBuffer[i]
        case FileComment of
          fcPas:
            begin
              // Initialy, use the character if the active section is selected :
              UseChar := (ActiveSection in SectionsToSearch);

              if FQuoteActive then
              begin
                UseChar := UseChar and FSearchInStrings;
                // FQuoteActive isn't updated properly when '' is inside a
                // string, but this doesn't affect the actual search results
                if FSearchBuffer[i] = '''' then
                  FQuoteActive := False;
              end
              else if FCurlyCommentActive then
              begin
                UseChar := UseChar and FSearchInComments;
                if FSearchBuffer[i] = '}' then
                  FCurlyCommentActive := False;
              end
              else if FStarCommentActive then
              begin
                UseChar := UseChar and FSearchInComments;
                if (FSearchBuffer[i] = ')') and (i > 1) and (FSearchBuffer[i - 1] = '*') then
                  FStarCommentActive := False;
              end
              else if FSlashCommentActive then
              begin
                UseChar := UseChar and FSearchInComments;
                // SlashComments end automatically at end-of-line (see #10/#13 above)
              end
              else
              begin
                // We're in 'Code active' mode here. Detect section-changes :

                // Are there any sections after the active one?
                if  (ActiveSection < usEnd)
                  // Is the previous character not part of the string?
                  and ((i = 0) or (not IsCharIdentifier(Char(FSearchBuffer[i - 1]))))
                  // Is the next section keyword present here?
                  and (StrLIComp(PAnsiChar(FSearchBuffer) + i, PAnsiChar(CSections[Succ(ActiveSection)]), Length(CSections[Succ(ActiveSection)])) = 0)
                  // Is the following character not part of the string?
                  and (not IsCharIdentifier(Char(FSearchBuffer[i+Length(CSections[Succ(ActiveSection)])]))) then
                begin
                  // Then skip to the next section :
                  ActiveSection := Succ(ActiveSection);
                  Inc(i, Length(CSections[ActiveSection]));
                  // Update the use of this character, if the (new) active section is selected :
                  UseChar := (ActiveSection in SectionsToSearch);
                end;

                // The following cases reduces the usage of the character to selected types of text :
                case FSearchBuffer[i] of
                  '''':
                    begin
                      FQuoteActive := True;
                      UseChar := UseChar and (FSearchInStrings or FSearchInCode);
                    end;
                  '(':
                    if (FSearchBuffer[i + 1] = '*') then
                    begin
                      FStarCommentActive := True;
                      Inc(i);
                      UseChar := UseChar and (FSearchInComments or FSearchInCode);
                    end;
                  '/':
                    if (FSearchBuffer[i + 1] = '/') then
                    begin
                      FSlashCommentActive := True;
                      Inc(i);
                      UseChar := UseChar and (FSearchInComments or FSearchInCode);
                    end;
                  '{':
                    begin
                      FCurlyCommentActive := True;
                      UseChar := UseChar and (FSearchInComments or FSearchInCode);
                    end;
                else
                  // Normal code :
                  UseChar := UseChar and FSearchInCode;
                end;
              end;
            end;
          fcCPP:
            begin
              // The following odd pattern can confuse the comment parser:
              // /* a comment *\
              // /  ShowMessage("coucou");
              if FQuoteActive then
              begin
                UseChar := FSearchInStrings;
                if (FSearchBuffer[i] = #39) and ((i = 1) or (FSearchBuffer[i - 1] <> '\')) then
                  FQuoteActive := False;
              end
              else if FDoubleQuoteActive then
              begin
                UseChar := FSearchInStrings;
                if (FSearchBuffer[i] = '"') and ((i = 1) or (FSearchBuffer[i - 1] <> '\')) then
                  FDoubleQuoteActive := False;
              end
              else if FStarCommentActive then
              begin
                UseChar := FSearchInComments;
                if (FSearchBuffer[i] = '/') and (i > 1) and (FSearchBuffer[i - 1] = '*') then
                  FStarCommentActive := False;
              end
              else if FSlashCommentActive then
              begin
                UseChar := FSearchInComments;
                // SlashComments end automatically at end-of-line (see #10/#13 above)
              end
              else
                case FSearchBuffer[i] of
                  #39:
                    begin
                      FQuoteActive := True;
                      UseChar := FSearchInStrings or FSearchInCode; // Include first char in code too
                    end;
                  '"':
                    begin
                      FDoubleQuoteActive := True;
                      UseChar := FSearchInStrings or FSearchInCode;
                    end;
                  '/':
                    case FSearchBuffer[i + 1] of
                      '*': begin
                             FStarCommentActive := True;
                             i := i + 1;
                             UseChar := FSearchInComments or FSearchInCode;
                           end;
                      '/': begin
                             FSlashCommentActive := True;
                             UseChar := FSearchInComments or FSearchInCode;
                           end;
                    else
                      UseChar := FSearchInCode;
                    end;
                else
                  UseChar := FSearchInCode;
                end;
            end;
        else // case FileComment
          UseChar := True;
        end;
      end; // case FSearchBuffer[i]

      if UseChar then
      begin
        if soCaseSensitive in SearchOptions then
          BLine[LPos] := FSearchBuffer[i]
        else
          BLine[LPos] := LoCase(FSearchBuffer[i]);
        // Calculate the location of this character in the original source-line :
        OrigLinePos[LPos] := i - FSearchLineStart;
        Inc(LPos);
        if LPos >= SearchLineSize-1 then // Enforce maximum line length constraint
          Exit; // Binary, not text file
      end;

      Inc(i);
    end; // while i < FBufferDataCount
    StrLCopy(OrgLine, PAnsiChar(FSearchBuffer) + FSearchLineStart, i - FSearchLineStart);    
    if FSearchBuffer[i] <> #0 then
      Inc(FLineNo);
    BLine[LPos] := #0;
    if BLine[0] <> #0 then
      PatternMatch;
    if FBufferSearchPos < i then
      FBufferSearchPos := i;
  end; // while not FEof
end;

procedure TBaseSearcher.SetBufSize(New: Integer);
begin
  if (FSearchBuffer = nil) and (New <> FBufSize) then
    FBufSize := New;
end;

procedure TBaseSearcher.SetPattern(const Source: AnsiString);
resourcestring
  SClassNotTerminated = 'Class at %d did not terminate properly';
var
  PatternCharIndex: Integer;
  SourceCharIndex: Integer;

  procedure Store(Ch: AnsiChar);
  begin
    Assert(PatternCharIndex < GrepPatternSize, 'Buffer overrun!');
    if not (soCaseSensitive in SearchOptions) then
      FPattern[PatternCharIndex] := LoCase(Ch)
    else
      FPattern[PatternCharIndex] := Ch;
    Inc(PatternCharIndex);
  end;

  procedure cclass;
  var
    cstart: Integer;
  begin
    cstart := SourceCharIndex;
    Inc(SourceCharIndex);
    if Source[SourceCharIndex] = '^' then
    begin
      Inc(SourceCharIndex);
      Store(opNClass);
    end
    else
      Store(opClass);

    while (SourceCharIndex <= Length(Source)) and (Source[SourceCharIndex] <> ']') do
    begin
      if (Source[SourceCharIndex] = '-') and
        (SourceCharIndex - cstart > 1) and
        (Source[SourceCharIndex + 1] <> ']') and
        (SourceCharIndex < Length(Source)) then
      begin
        Dec(PatternCharIndex, 2);
        Store(opRange);
        Store(Source[SourceCharIndex - 1]);
        Store(Source[SourceCharIndex + 1]);
        Inc(SourceCharIndex, 2);
      end
      else
      begin
        Store(Source[SourceCharIndex]);
        Inc(SourceCharIndex);
      end;
    end;

    if (Source[SourceCharIndex] <> ']') or (SourceCharIndex > Length(Source)) then
      raise Exception.CreateFmt(SClassNotTerminated, [cstart]);

    Inc(SourceCharIndex); // To push past close bracket
  end;

resourcestring
  SPatternTooLong = 'Grep pattern too long. (> 500 characters)';
  SInvalidGrepSearchCriteria = 'Character immediately following: at %d is not a valid grep search criteria';
  SSenselessEscape = 'Escape character ("\") without a following character does not make sense';
begin
  // Warning: this does not properly protect against pattern overruns
  // A better solution needs to be found for this, possibly by sacrificing
  // a bit of performance for a test in the pattern storage code where a
  // new Assert has been introduced.
  if Length(Source) > 500 then
    raise Exception.Create(SPatternTooLong);

  try
    SourceCharIndex := 1;
    PatternCharIndex := 0;
    while SourceCharIndex <= Length(Source) do
    begin
      if not (soRegEx in SearchOptions) then
      begin
        Store(opChar);
        Store(Source[SourceCharIndex]);
        Inc(SourceCharIndex);
      end
      else
      begin
        case Source[SourceCharIndex] of
          '^':
            begin
              Store(opBOL);
              Inc(SourceCharIndex);
            end;

          '$':
            begin
              Store(opEOL);
              Inc(SourceCharIndex);
            end;

          '.':
            begin
              Store(opAny);
              Inc(SourceCharIndex);
            end;

          '[':
            cclass;

          ':':
            begin
              if SourceCharIndex < Length(Source) then
              begin
                case UpCase(Source[SourceCharIndex + 1]) of
                  'A': Store(opAlpha);
                  'D': Store(opDigit);
                  'N': Store(opAlphaNum);
                  ' ': Store(opPunct);
                else
                  Store(opEndPat);
                  raise Exception.CreateFmt(SInvalidGrepSearchCriteria, [SourceCharIndex]);
                end;
                Inc(SourceCharIndex, 2);
              end
              else
              begin
                Store(opChar);
                Store(Source[SourceCharIndex]);
                Inc(SourceCharIndex);
              end;
            end;

          '\':
            begin
              if SourceCharIndex >= Length(Source) then
                raise Exception.Create(SSenselessEscape);

              Store(opChar);
              Store(Source[SourceCharIndex + 1]);
              Inc(SourceCharIndex, 2);
            end;
        else
          Store(opChar);
          Store(Source[SourceCharIndex]);
          Inc(SourceCharIndex);
        end; // case
      end;
    end;
  finally
    Store(opEndPat);
    Store(#0);
  end;
end;

procedure TBaseSearcher.PatternMatch;
var
  l, p: Integer; // Line and pattern pointers
  op: AnsiChar; // Pattern operation
  LinePos: Integer;

  procedure IsFound;
  var
    S: Integer;
    E: Integer;
    TestChar: AnsiChar;
  begin
    if soWholeWord in SearchOptions then
    begin
      S := LinePos - 2;
      E := l;
      if (S >= 0) then
      begin
        TestChar := BLine[S];
        if IsCharIdentifier(TestChar) then
          Exit;
      end;
      TestChar := BLine[E];
      if TestChar <> #0 then
      begin
        if IsCharIdentifier(TestChar) then
          Exit;
      end;
    end;

    SignalFoundMatch(FLineNo, string(OrgLine), OrigLinePos[LinePos], OrigLinePos[l]);
  end;

begin
  if FPattern[0] = opEndPat then
    Exit;
  LinePos := 0;

  // Don't bother pattern matching if first search is opChar, just go to first
  // match directly.  This results in a 5-10% speed increase.
  if (FPattern[0] = opChar) and not (soCaseSensitive in SearchOptions) then
    while (FPattern[1] <> BLine[LinePos]) and (BLine[LinePos] <> #0) do
      Inc(LinePos);

  while BLine[LinePos] <> #0 do
  begin
    l := LinePos;
    p := 0;
    op := FPattern[p];
    while op <> opEndPat do
    begin
      case op of
        opChar:
          begin
            if not (BLine[l] = FPattern[p + 1]) then
              Break;
            Inc(p, 2);
          end;

        opBOL:
          begin
            Inc(p);
          end;

        opEOL:
          begin
            if BLine[l] in [#0, #10, #13] then
              Inc(p)
            else
              Break;
          end;

        opAny:
          begin
            if BLine[l] in [#0, #10, #13] then
              Break;
            Inc(p);
          end;

        opClass:
          begin
            Inc(p);
            // Compare letters to find a match
            while (FPattern[p] > LastPatternChar) and (FPattern[p] <> BLine[l]) do
              Inc(p);
            // Was a match found?
            if FPattern[p] <= LastPatternChar then
              Break;
            // Move pattern pointer to next opcode 
            while FPattern[p] > LastPatternChar do
              Inc(p);
          end;

        opNClass:
          begin
            Inc(p);
            // Compare letters to find a match
            while (FPattern[p] > LastPatternChar) and (FPattern[p] <> BLine[l]) do
              Inc(p);
            if FPattern[p] > LastPatternChar then
              Break;
          end;

        opAlpha:
          begin
            if not IsCharAlphaA(BLine[l]) then
              Break;
            Inc(p);
          end;

        opDigit:
          begin
            if not IsCharNumeric(BLine[l]) then
              Break;
            Inc(p);
          end;

        opAlphaNum:
          begin
            if IsCharAlphaNumeric(BLine[l]) then
              Inc(p)
            else
              Break;
          end;

        opPunct:
          begin
            if (BLine[l] = ' ') or (BLine[l] > #64) then
              Break;
            Inc(p);
          end;

        opRange:
          begin
            if (BLine[l] < FPattern[p + 1]) or (BLine[l] > FPattern[p + 2]) then
              Break;
            Inc(p, 3);
          end;
      else
        Inc(p);
      end; // case

      if (op = opBOL) and not (BLine[l] in [#9, #32]) then
        Exit; // Means that we did not match at start.

      op := FPattern[p];
      Inc(l);
    end; // while op <> opEndPat
    Inc(LinePos);
    if op = opEndPat then
      IsFound;
  end; // while BLine[LinePos] <> #0
end;

procedure TBaseSearcher.SignalStartSearch;
begin
  if Assigned(FOnStartSearch) then
    FOnStartSearch(Self);
end;

procedure TBaseSearcher.SignalFoundMatch(LineNo: Integer; const Line: string;
  SPos, FEditReaderPos: Integer);
begin
  if Assigned(FOnFound) then
    FOnFound(Self, LineNo, Line, SPos, FEditReaderPos);
end;

{ TLineTooLongMessage }

function TLineTooLongMessage.GetMessage: string;
begin
  Result := FData;
end;

end.

