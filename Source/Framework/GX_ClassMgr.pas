unit GX_ClassMgr;

{
  Missing Features:
   - Support for nested class declarations
   - Full support for old-style object declarations
   - Jumping to overloaded procedures does not work (it goes to the first one)
}

interface

{$I GX_CondDefine.inc}

uses
  Classes,
  GX_EditReader,
  mwPasParser;

type
  TOnParseFile = procedure(const FileName: string; FileIndex, FileCount: Integer) of object;

  TMethodDeclare = (cdPrivate, cdProtected, cdPublic, cdPublished);

  TMethodType = (ctConstant, ctMethod, ctType, ctVariable, ctProperty);

  TBrowseClassInfoCollection = class;

  TOList = class(TList);
  TClassList = class;

  TBrowseMethodInfoItem = class(TCollectionItem)
  private
    // Full declaration of the member for display
    FDName: string;
    // Identifier only of the member for searching
    FRName: string;
    // Member visibility
    FMethodDeclare: TMethodDeclare;
    FMethodType: TMethodType;
    FLineNo: Integer;
    FcOverride: Boolean;
    FcAbstract: Boolean;
    FcVirtual: Boolean;
    FcMessage: Boolean;
    FcReintroduce: Boolean;
    FcOverload: Boolean;
    function GetCollection: TBrowseClassInfoCollection;
  public
    function GetInterfaceLine: Integer;
    function GetImplementationLine: Integer;
    property Collection: TBrowseClassInfoCollection read GetCollection;
    property DName: string read FDName;
    property RName: string read FRName;
    property MethodDeclare: TMethodDeclare read FMethodDeclare;
    property MethodType: TMethodType read FMethodType;
    property LineNo: Integer read FLineNo;
    property cOverride: Boolean read FcOverride;
    property cAbstract: Boolean read FcAbstract;
    property cVirtual: Boolean read FcVirtual;
    property cMessage: Boolean read FcMessage;
    property cReintroduce: Boolean read FcReintroduce;
    property cOverload: Boolean read FcOverload;
  end;

  TBrowseClassInfoCollection = class(TCollection)
  private
    FClassList: TClassList;
    // The complete list of object/interface ancestors
    FDerivedFrom: string;
    FSourceName: string;
    FLineNo: Integer;
    FFileName: string;
    FIsLoaded: Boolean;
    FName: string;
    procedure GetMethods(Parser: TmPasParser);
    function GetItem(Index: Integer): TBrowseMethodInfoItem;
    function GetText: string;
    procedure SetText(const New: string);
    procedure SetClassList(AClassList: TClassList);
    function GetObjectDerivedFrom: string;
  public
    constructor Create;
    procedure LoadMethods;
    function Add: TBrowseMethodInfoItem;
    function SetParser(Parser: TmPasParser): Boolean; // Sets parser on interface line
    function RefreshLineNo: Integer;
    property Items[Index: Integer]: TBrowseMethodInfoItem read GetItem; default;
    property ClassList: TClassList read FClassList write SetClassList;
    property DerivedFrom: string read FDerivedFrom;
    property ObjectDerivedFrom: string read GetObjectDerivedFrom;
    property FileName: string read FFileName;
    property SourceName: string read FSourceName;
    property LineNo: Integer read FLineNo;
    property IsLoaded: Boolean read FIsLoaded;
    property Name: string read FName;
    property AsText: string read GetText;
  end;

  TClassItem = class(TCollectionItem)
  private
    FProjectCount: Integer;
    FOList: TOList;
    FIsProject: Boolean;
    FDirectory: string;
    FRecurse: Boolean;
    FName: string;
    procedure LoadFromDir(const Dir: string; const Recurse: Boolean);
    procedure LoadFromProject;
    function GetClassCount: Integer;
    function GetClassItem(Index: Integer): TBrowseClassInfoCollection;
    procedure LoadClass(const FileName: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure SaveToFile;
    procedure LoadFromFile;
    function ObjectByName(const ObjectName: string): TBrowseClassInfoCollection;
    property Directory: string read FDirectory write FDirectory;
    property Recurse: Boolean read FRecurse write FRecurse;
    property IsProject: Boolean read FIsProject write FIsProject;
    property Name: string read FName write FName;
    property ClassCount: Integer read GetClassCount;
    property ClassItem[Index: Integer]: TBrowseClassInfoCollection read GetClassItem;
  end;

  TClassList = class(TCollection)
  private
    FOnParseFile: TOnParseFile;
    FOnEndParse: TNotifyEvent;
    FStoragePath: string;
    function GetItem(Index: Integer): TClassItem;
  protected
    EditRead: TEditReader;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TClassItem;
    procedure LoadFromFile;
    procedure SaveToFile(CreateIfNecessary: Boolean);
    function CheckStorageDirectory(CreateIfNecessary: Boolean): Boolean;
    property Items[Index: Integer]: TClassItem read GetItem; default;
    property StoragePath: string read FStoragePath write FStoragePath;
    property OnParseFile: TOnParseFile read FOnParseFile write FOnParseFile;
    property OnEndParse: TNotifyEvent read FOnEndParse write FOnEndParse;
  end;

implementation

uses
  SysUtils, Dialogs, Controls, IniFiles, mwEPTokenList,
  ToolsAPI,
  GX_GenericUtils, GX_OtaUtils;

{ TClassList }

constructor TClassList.Create;
begin
  inherited Create(TClassItem);

  EditRead := TEditReader.Create('');
end;

destructor TClassList.Destroy;
begin
  FreeAndNil(EditRead);
  
  inherited Destroy;
end;

function TClassList.Add: TClassItem;
begin
  Result := TClassItem(inherited Add);
end;

function TClassList.GetItem(Index: Integer): TClassItem;
begin
  Result := TClassItem(inherited GetItem(Index));
end;

procedure TClassList.LoadFromFile;
var
  IniFile: TMemIniFile;
  IniDatabaseItemCount: Integer;
  i: Integer;
  Item: TClassItem;
begin
  if not CheckStorageDirectory(True) then
    Exit;
  IniFile := TMemIniFile.Create(AddSlash(FStoragePath) + 'Classes.ini'); // Do not localize.
  try
    IniDatabaseItemCount := IniFile.ReadInteger('Databases', 'Count', 0); // Do not localize.
    if IniDatabaseItemCount > 0 then
    begin
      for i := 0 to IniDatabaseItemCount-1 do
      begin
        Item := Self.Add;
        Item.FName := IniFile.ReadString('Databases', 'Name' + IntToStr(i), 'Unknown'); // do not localize
        Item.FDirectory := AddSlash(IniFile.ReadString('Databases', 'Database' + IntToStr(i), 'Unknown')); // Do not localize.
        Item.LoadFromFile;
      end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TClassList.CheckStorageDirectory(CreateIfNecessary: Boolean): Boolean;
resourcestring
  SConfirmCreateFolder = 'Do you want to create the folder %s to store class information?';
  SUnableCreateFolder =  'Unable to create folder %s.';
begin
  Result := True;
  if not DirectoryExists(FStoragePath) then
  begin
    if CreateIfNecessary and (MessageDlg(Format(SConfirmCreateFolder, [FStoragePath]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      ForceDirectories(FStoragePath);
      if not DirectoryExists(FStoragePath) then
      begin
        MessageDlg(Format(SUnableCreateFolder, [FStoragePath]), mtError, [mbOK], 0);
        Result := False;
      end;
    end
    else
      Result := False;
  end;
end;

procedure TClassList.SaveToFile(CreateIfNecessary: Boolean);
var
  IniFile: TMemIniFile;
  i: Integer;
begin
  if not CheckStorageDirectory(CreateIfNecessary) then
    Exit;
  IniFile := TMemIniFile.Create(AddSlash(FStoragePath) + 'Classes.ini'); // Do not localize.
  try
    IniFile.WriteInteger('Databases', 'Count', Count); // do not localize
    for i := 0 to Self.Count-1 do
    begin
      if not Items[i].IsProject then
      begin
        IniFile.WriteString('Databases', 'Name' + IntToStr(i), Items[i].Name); // do not localize
        IniFile.WriteString('Databases', 'Database' + IntToStr(i), AddSlash(Items[i].Directory)); // Do not localize.
        Items[i].SaveToFile;
      end;
    end;
    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

{ TClassItem }

constructor TClassItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOList := TOList.Create;
end;

destructor TClassItem.Destroy;
begin
  Clear;
  FreeAndNil(FOList);
  inherited Destroy;
end;

procedure TClassItem.Clear;
var
  i: Integer;
begin
  if FOList <> nil then
  begin
    for i := 0 to FOList.Count-1 do
      TBrowseClassInfoCollection(FOList.Items[i]).Free;

    FOList.Clear;
  end;
end;

function TClassItem.GetClassCount: Integer;
begin
  Result := FOList.Count;
end;

function TClassItem.GetClassItem(Index: Integer): TBrowseClassInfoCollection;
begin
  Result := TBrowseClassInfoCollection(FOList.Items[Index]);
end;

function TClassItem.ObjectByName(const ObjectName: string): TBrowseClassInfoCollection;
var
  i: Integer;
  InfoCollection: TBrowseClassInfoCollection;
begin
  Result := nil;
  for i := 0 to FOList.Count-1 do
  begin
    InfoCollection := TBrowseClassInfoCollection(FOList.Items[i]);
    if SameText(InfoCollection.Name, ObjectName) then
    begin
      Result := InfoCollection;
      Break;
    end;
  end;
end;

procedure TClassItem.Load;
begin
  Clear;
  if not FIsProject then
    LoadFromDir(Directory, FRecurse)
  else
    LoadFromProject;
end;

resourcestring
  SParsingError = 'A parsing error occurred in ';

procedure TClassItem.LoadFromProject;
var
  ClassList: TClassList;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  ModuleFileName: string;
  ModuleFileCount: Integer;
  i: Integer;
begin
  ClassList := Collection as TClassList;
  FProjectCount := 0;

  CurrentProject := GxOtaGetCurrentProject;
  Assert(Assigned(CurrentProject));

  ModuleFileCount := CurrentProject.GetModuleCount;
  for i := 0 to ModuleFileCount-1 do
  begin
    ModuleInfo := CurrentProject.GetModule(i);
    Assert(Assigned(ModuleInfo));

    ModuleFileName := ModuleInfo.FileName;
    if IsPascalSourceFile(ModuleFileName) then
    begin
      try
        LoadClass(ModuleFileName);
      except
        on E: Exception do
        begin
          GxLogAndShowException(E, SParsingError +sLineBreak+ ModuleFileName +sLineBreak);
          // Swallow exception
        end;
      end;

      Inc(FProjectCount);

      if Assigned(ClassList.OnParseFile) then
        ClassList.OnParseFile(ModuleFileName, FProjectCount, ModuleFileCount);
    end;
  end;

  if Assigned(ClassList.OnEndParse) then
    ClassList.OnEndParse(ClassList);

  ClassList.EditRead.FileName := '';
end;

procedure TClassItem.LoadFromDir(const Dir: string; const Recurse: Boolean);
var
  Mask: string;
  FileList: TStringList;

  procedure CollectFiles(Path: string);
  var
    Result: Integer;
    Search: TSearchRec;
  begin
    if Length(Path) > 0 then
      Path := AddSlash(Path);

    Result := FindFirst(Path + AllFilesWildCard, faAnyFile, Search);
    try
      while Result = 0 do
      begin
        if (Search.Attr and faDirectory) <> 0 then
        begin
          if Recurse and (Search.Name <> '.') and (Search.Name <> '..') then
            CollectFiles(Path + Search.Name);
        end
        else
          if AnsiCaseInsensitivePos('*' + ExtractFileExt(Search.Name), Mask) <> 0 then
          begin
            FileList.Add(Path + Search.Name);
          end;

        Result := FindNext(Search);
      end;
    finally
      SysUtils.FindClose(Search);
    end;
  end;

var
  i: Integer;
begin
  Mask := '*.pas;*.dpr;*.inc';

  FileList := TStringList.Create;
  try
    CollectFiles(Dir);

    { Now parse files }
    for i := 0 to FileList.Count-1 do
    begin
      try
        LoadClass(FileList[i]);
      except
        on E: Exception do
        begin
          GxLogAndShowException(E, SParsingError +sLineBreak+ FileList[i] +sLineBreak);
          // Swallow exception
        end;
      end;
      if Assigned(TClassList(Collection).OnParseFile) then
        TClassList(Collection).OnParseFile(FileList[i], i + 1, FileList.Count);
    end;
  finally
    FreeAndNil(FileList);

    if Assigned(TClassList(Collection).OnEndParse) then
      TClassList(Collection).OnEndParse(Self);
  end;
end;

procedure TClassItem.LoadClass(const FileName: string);
var
  FileContent: string;
  Parser: TmEPTokenList;
  p1, i, j: Integer;
  ObjectInfo: TBrowseClassInfoCollection;
  LocalEditReader: TEditReader;
begin
  if not IsPascalSourceFile(FileName) then
    Exit;

  LocalEditReader := TClassList(Collection).EditRead;
  LocalEditReader.FileName := FileName;
  LocalEditReader.Reset;

  FileContent := LocalEditReader.GetText;
  LocalEditReader.FreeFileData;
  Parser := TmEPTokenList.Create;
  try
    Parser.SetOrigin(@FileContent[1], Length(FileContent));
    for j := 0 to Parser.Searcher.ClassList.Count - 1 do
    begin
      Parser.RunIndex := Parser.Searcher.ClassList[j];
      case Parser.GetClassKind of
        ikClass,
        ikClEmpty:
          begin
            ObjectInfo := TBrowseClassInfoCollection.Create;
            FOList.Add(ObjectInfo);
            ObjectInfo.ClassList := TClassList(Collection);
            p1 := 0;
            for i := 1 to Length(Parser.Info.Data) do
            begin
              case Parser.Info.Data[i] of
                '=': ObjectInfo.FName := Trim(Copy(Parser.Info.Data, 1, i - 1));
                '(': p1 := i;
                ')': begin
                       ObjectInfo.FDerivedFrom := Trim(Copy(Parser.Info.Data, p1 + 1, i - p1 - 1));
                       // Handle the shortcut implied TObject
                       if Trim(ObjectInfo.FDerivedFrom) = '' then
                         ObjectInfo.FDerivedFrom := 'TObject';
                       Break;
                     end;
              end;
            end;
            // ObjectInfo.LineNo:=Parser.Info.LineNumber;
            ObjectInfo.FFileName := LocalEditReader.FileName;
            if Parser.Info.AI <> nil then
              ObjectInfo.FSourceName := Parser.Info.AI.aiUnit
            else
              ObjectInfo.FSourceName := ExtractPureFileName(ObjectInfo.FileName);
          end;
      end; // case
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TClassItem.SaveToFile;
const
  BoolValues: array[Boolean] of string = ('0', '1');
var
  FileName: string;
  i: Integer;
  sl: TStringList;
//  Cnt1: Int64;
//  Cnt2: Int64;
//  PFreq: Int64;
begin
  if not DirectoryExists(Directory) then
    Exit;
  FileName := AddSlash(Directory) + Name + '.gex'; // Do not localize.

//  QueryPerformanceFrequency(PFreq);

//  QueryPerformanceCounter(Cnt1);
  // Using TStringlist.SaveToFile is much(!) faster than TIniFile and still 50 times faster
  // than TMemIniFile.
  // I left the timing code commented out, so you can see for yourself, if you want.
  sl := TStringList.Create;
  try
    sl.Add('[General]');
    sl.Add('Name=' + Name);
    sl.Add('Project=' + BoolValues[IsProject]);
    sl.Add('[Classes]');
    sl.Add('Count=' + IntToStr(ClassCount));
    for i := 0 to ClassCount - 1 do // do not localize
      sl.Add('Class' + IntToStr(i) + '=' + ClassItem[i].AsText);
    sl.SaveToFile(FileName);
  finally
    FreeAndNil(sl);
  end;
//  QueryPerformanceCounter(Cnt2);
//{$IFOPT D+}SendDebugFmt('Writing with TStringList.SaveToFile took %.3f s', [(Cnt2 - Cnt1) / PFreq]);{$ENDIF}

//  QueryPerformanceCounter(Cnt1);
//  sl := TStringList.Create;
//  try
//    sl.Add('[General]');
//    sl.Add('Name=' + Name);
//    sl.Add('Project=' + BoolValues[IsProject]);
//    sl.Add('[Classes]');
//    sl.Add('Count=' + IntToStr(ClassCount));
//    for i := 0 to ClassCount-1 do // do not localize
//      sl.Add('Class' + IntToStr(i) + '=' + ClassItem[i].AsText);
//    with TMemIniFile.Create(FileName) do
//    try
//      SetStrings(sl);
//      UpdateFile;
//    finally
//      Free;
//    end;
//  finally
//    FreeAndNil(sl);
//  end;
//  QueryPerformanceCounter(Cnt2);
//{$IFOPT D+}SendDebugFmt('Writing with TMemIniFile.SetStrings took %.3f s', [(Cnt2 - Cnt1) / PFreq]);{$ENDIF}

//  QueryPerformanceCounter(Cnt1);
//  with TMemIniFile.Create(FileName) do
//  try
//    WriteString('General', 'Name', Name); // do not localize
//    WriteBool('General', 'Project', IsProject); // do not localize
//    WriteInteger('Classes', 'Count', ClassCount); // do not localize
//    for i := 0 to ClassCount-1 do // do not localize
//      WriteString('Classes', 'Class' + IntToStr(i), ClassItem[i].AsText); // do not localize
//    UpdateFile;
//  finally
//    Free;
//  end;
//  QueryPerformanceCounter(Cnt2);
//{$IFOPT D+}SendDebugFmt('Writing with TMemIniFile took %.3f s', [(Cnt2 - Cnt1) / PFreq]);{$ENDIF}

//  QueryPerformanceCounter(Cnt1);
  with TIniFile.Create(FileName) do
//  try
//    WriteString('General', 'Name', Name); // do not localize
//    WriteBool('General', 'Project', IsProject); // do not localize
//    WriteInteger('Classes', 'Count', ClassCount); // do not localize
//    for i := 0 to ClassCount-1 do // do not localize
//      WriteString('Classes', 'Class' + IntToStr(i), ClassItem[i].AsText); // do not localize
//    UpdateFile;
//  finally
//    Free;
//  end;
//  QueryPerformanceCounter(Cnt2);
//{$IFOPT D+}SendDebugFmt('Writing with TIniFile took %.3f s', [(Cnt2 - Cnt1) / PFreq]);{$ENDIF}
end;

procedure TClassItem.LoadFromFile;
var
  IniFile: TMemIniFile;
  IniClassCount: Integer;
  FileName: string;
  TempClassName: string;
  i: Integer;
  OInfo: TBrowseClassInfoCollection;
begin
  FileName := AddSlash(ExtractFilePath(Directory)) + Name + '.gex'; // Do not localize.
  if not FileExists(FileName) then
    Exit;
  IniFile := TMemIniFile.Create(FileName);
  try
    FName := IniFile.ReadString('General', 'Name', Name); // do not localize
    FIsProject := IniFile.ReadBool('General', 'Project', IsProject); // do not localize
    IniClassCount := IniFile.ReadInteger('Classes', 'Count', 0); // do not localize
    with FOList do
      Capacity := Capacity + IniClassCount;
    for i := 0 to IniClassCount-1 do
    begin
      OInfo := TBrowseClassInfoCollection.Create;
      FOList.Add(OInfo);
      OInfo.FClassList := TClassList(Collection);
      TempClassName := IniFile.ReadString('Classes', 'Class' + IntToStr(i), '');// do not localize
      if TempClassName = '' then
        raise Exception.Create('Invalid class file format');
      if TempClassName[1] <> '"' then TempClassName := '"' + TempClassName;
      if TempClassName[Length(TempClassName)] <> '"' then TempClassName := TempClassName + '"';
      OInfo.SetText(TempClassName);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

{ TBrowseClassInfoCollection }

constructor TBrowseClassInfoCollection.Create;
begin
  inherited Create(TBrowseMethodInfoItem);
end;

function TBrowseClassInfoCollection.Add: TBrowseMethodInfoItem;
begin
  Result := TBrowseMethodInfoItem(inherited Add);
end;

function TBrowseClassInfoCollection.GetItem(Index: Integer): TBrowseMethodInfoItem;
begin
  Result := TBrowseMethodInfoItem(inherited GetItem(Index));
end;

function TBrowseClassInfoCollection.GetText: string;
begin
  Result := #34 + Name + #34 + ',' + #34 + DerivedFrom + #34 + ',' +
    #34 + SourceName + #34 + ',' + #34 + IntToStr(LineNo) + #34 + ',' + #34 + FileName + #34;
end;

procedure TBrowseClassInfoCollection.SetText(const New: string);
var
  i: Cardinal;
  CopyStartPos: Cardinal;

      function ExtractSubString: string;
      begin
        Result := Copy(New, CopyStartPos, i - CopyStartPos);
      end;

var
  TypeCounter: Cardinal;
  TextActive: Boolean;
begin
  try
    TextActive := False;
    TypeCounter := 0;
    CopyStartPos := 1;
    for i := 1 to Length(New) do
    begin
      if New[i] = '"' then
      begin
        if TextActive then
        begin
          case TypeCounter of
            0: FName        := ExtractSubString;
            1: FDerivedFrom := ExtractSubString;
            2: FSourceName  := ExtractSubString;
            3: FLineNo      := StrToInt(ExtractSubString);
            4: FFileName    := ExtractSubString;
          else
            Assert(False, 'Bad file format encountered.');
          end; // case
          Inc(TypeCounter);
          TextActive := False;
        end // if TextActive
        else
        begin
          CopyStartPos := i + 1;
          TextActive := True;
        end;
      end; // if New[i] = '"'
    end; // for
  except
    on E: Exception do
    begin
      // Ignore exceptions
    end;
  end;
end;

procedure TBrowseClassInfoCollection.SetClassList(AClassList: TClassList);
begin
  Assert(FClassList = nil, 'Memory leak assigning class list');
  FClassList := AClassList;
end;

function TBrowseClassInfoCollection.SetParser(Parser: TmPasParser): Boolean;

      function GetInfo: TmInfoKind;
      begin
        Parser.NextNonSpace;
        case Parser.Token.ID of
          tkNull:      Result := ikUnknown;
          tkSemiColon: Result := ikClForward;
          tkOf:        Result := ikClReference;
        else
                       Result := ikClass;
        end;
      end;

begin
  Result := False;
  while Parser.Token.ID <> tkNull do
  begin
    Parser.NextClassLine;
    if GetInfo = ikClass then
    begin
      Parser.RunPos := Parser.LastIdentPos;
      if SameText(Parser.Token.Data, Name) then
      begin
        FLineNo := Parser.Token.LineNumber; // Update LineNumber
        Result := True;
        Exit;
      end
      else
        Parser.NextNonJunk;
    end;
  end;
end;

function TBrowseClassInfoCollection.RefreshLineNo: Integer;
var
  FileContent: string;
  Parser: TmPasParser;
  LocalEditReader: TEditReader;
begin
  Result := 0;

  LocalEditReader := FClassList.EditRead;
  LocalEditReader.FileName := FileName;
  LocalEditReader.Reset;

  FileContent := LocalEditReader.GetText;
  LocalEditReader.FreeFileData;
  Parser := TmPasParser.Create;
  try
    Parser.Origin := @FileContent[1];
    if SetParser(Parser) then
    begin
      Result := Parser.Token.LineNumber;
      FLineNo := Result;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

procedure TBrowseClassInfoCollection.LoadMethods;
var
  FileContent: string;
  Parser: TmPasParser;
  LocalEditReader: TEditReader;
begin
  if not FileExists(FileName) and (not GxOtaIsFileOpen(FileName)) then
    Exit;
  Clear;

  LocalEditReader := FClassList.EditRead;
  LocalEditReader.FileName := FileName;
  LocalEditReader.Reset;

  FIsLoaded := True;
  FileContent := LocalEditReader.GetText;
  Parser := TmPasParser.Create;
  try
    Parser.Origin := @FileContent[1];
    if SetParser(Parser) then
    begin
      while (Parser.Token.ID <> tkNull) and (Parser.Token.ID <> tkClass) do
        Parser.NextNonJunk;
      if Parser.Token.ID = tkClass then
        Parser.NextNonJunk;
      if Parser.Token.ID = tkRoundOpen then
        while (Parser.Token.ID <> tkNull) and (Parser.Token.ID <> tkRoundClose) do
          Parser.NextNonJunk;
      if Parser.Token.ID = tkRoundClose then
        Parser.NextNonJunk;
      if Parser.Token.ID <> tkSemiColon then
        GetMethods(Parser);
    end;
  finally
    LocalEditReader.FreeFileData;
    FreeAndNil(Parser);
  end;
end;

procedure TBrowseClassInfoCollection.GetMethods(Parser: TmPasParser);
var
  MethodVisibility: TTokenKind;

  procedure SetVisibility(MInfo: TBrowseMethodInfoItem);
  begin
    case MethodVisibility of
      tkPublished: MInfo.FMethodDeclare := cdPublished;
      tkPublic:    MInfo.FMethodDeclare := cdPublic;
      tkProtected: MInfo.FMethodDeclare := cdProtected;
      tkPrivate:   MInfo.FMethodDeclare := cdPrivate;
    end;
  end;

  procedure LoadProc;
  var
    RoundOpen: Integer;
    MInfo: TBrowseMethodInfoItem;
    BuildRName: Boolean;
    SavePos: Longint;
    ProcedureToken: TTokenKind;
    RenamedEqualPosition: Integer;
  begin
    ProcedureToken := Parser.Token.ID;
    if ProcedureToken in MethodMarkers then
    begin
      RoundOpen := 0;
      MInfo := Self.Add;
      MInfo.FDName := Parser.Token.Data;
      MInfo.FLineNo := Parser.Token.LineNumber;
      MInfo.FMethodType := ctMethod;
      SetVisibility(MInfo);
      Parser.NextToken;
      BuildRName := True;
      while (Parser.Token.ID <> tkNull) and
        (not ((Parser.Token.ID = tkSemiColon) and (RoundOpen = 0))) do
      begin
        case Parser.Token.ID of
          tkCRLF,
          tkSpace:
            MInfo.FDName := MInfo.FDName + ' ';

          // Accounts for "function xyz: Integer" which was not
          // previously parsed correctly
          tkColon:
            begin
              MInfo.FDName := MInfo.FDName + Parser.Token.Data;
              if (RoundOpen = 0) and (ProcedureToken = tkFunction) then
                BuildRName := False;
            end;

          tkRoundOpen:
            begin
              MInfo.FDName := MInfo.FDName + Parser.Token.Data;
              BuildRName := False;
              Inc(RoundOpen);
            end;

          tkRoundClose:
            begin
              MInfo.FDName := MInfo.FDName + Parser.Token.Data;
              Dec(RoundOpen);
            end;

          tkAnsiComment, tkBorComment, tkSlashesComment:
            begin
              // Ignore comments inside procedure declarations
            end;
        else
          begin
            MInfo.FDName := MInfo.DName + Parser.Token.Data;
            if BuildRName then
              MInfo.FRName := MInfo.FRName + Parser.Token.Data;
          end;
        end;
        Parser.NextToken;
      end; // while
      SavePos := 0;
      while (Parser.Token.ID = tkSemiColon) and (Parser.Token.ID <> tkNull) do
      begin
        SavePos := Parser.RunPos;
        Parser.NextNonJunk;
        case Parser.Token.ID of
          tkCRLF,
          tkSpace:
            MInfo.FDName := MInfo.FDName + ' ';
          tkAbstract:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcAbstract := True;
              Parser.NextNonJunk;
            end;
          tkOverride:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcOverride := True;
              Parser.NextNonJunk;
            end;
          tkMessage:
            begin
              MInfo.FcMessage := True;
              MInfo.FDName := MInfo.FDName + '; ';
              while (Parser.Token.ID <> tkSemiColon) and (Parser.Token.ID <> tkNull) do
              begin
                MInfo.FDName := MInfo.FDName + Parser.Token.Data;
                Parser.NextToken;
              end;
            end;
          tkVirtual:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcVirtual := True;
              Parser.NextNonJunk;
            end;
          tkReintroduce:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcReintroduce := True;
              Parser.NextNonJunk;
            end;
          tkOverload:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              MInfo.FcOverload := True;
              Parser.NextNonJunk;
            end;
          tkStdcall, tkCdecl, tkPascal, tkRegister, tkSafeCall:
            begin
              MInfo.FDName := MInfo.FDName + '; ' + Parser.Token.Data;
              Parser.NextNonJunk;
            end;
        end; // case
      end; // while
      if SavePos <> 0 then Parser.RunPos := SavePos;
      RenamedEqualPosition := Pos('=', MInfo.RName);
      // For renamed/redirected methods, RName should jump to the renamed method
      if RenamedEqualPosition > 0 then
        MInfo.FRName := Trim(Copy(MInfo.RName, RenamedEqualPosition + 1, 9999));
    end;
  end;

  procedure LoadVariable;
  var
    SquareOpen, RoundOpen: Integer;
    MInfo: TBrowseMethodInfoItem;
    BuildRName: Boolean;
    CommaPosition: Integer;
  begin
    SquareOpen := 0; RoundOpen := 0;
    MInfo := Self.Add;
    MInfo.FLineNo := Parser.Token.LineNumber;
    MInfo.FMethodType := ctVariable;
    BuildRName := True;
    SetVisibility(MInfo);
    while (not ((Parser.Token.ID = tkSemiColon) and (RoundOpen = 0) and (SquareOpen = 0))) and
      (Parser.Token.ID <> tkNull) do
    begin
      MInfo.FDName := MInfo.FDName + Parser.Token.Data;
      if BuildRName then
        if not (Parser.Token.ID in [tkSpace, tkCRLF, tkColon]) then
          MInfo.FRName := MInfo.FRName + Parser.Token.Data;
      case Parser.Token.ID of
        tkRoundOpen:   Inc(RoundOpen);
        tkRoundClose:  Dec(RoundOpen);
        tkSquareOpen:  Inc(SquareOpen);
        tkSquareClose: Dec(SquareOpen);
        tkColon:       BuildRName := False;
      end;
      Parser.NextToken;
    end;
    CommaPosition := Pos(',', MInfo.RName);
    // With multiple var declarations on the same line, RName should match
    // only the first identifier when searching for a match later
    if CommaPosition > 0 then
      MInfo.FRName := Copy(MInfo.RName, 1, CommaPosition - 1);
  end;

  procedure LoadProperty;
  var
    SquareOpen, RoundOpen: Integer;
    MInfo: TBrowseMethodInfoItem;
    BuildRName: Boolean;
  begin
    SquareOpen := 0; RoundOpen := 0;
    MInfo := Self.Add;
    MInfo.FLineNo := Parser.Token.LineNumber;
    MInfo.FMethodType := ctProperty;
    BuildRName := False;
    SetVisibility(MInfo);
    while (not ((Parser.Token.ID = tkSemiColon) and (RoundOpen = 0) and (SquareOpen = 0))) and
      (Parser.Token.ID <> tkNull) do
    begin
      MInfo.FDName := MInfo.FDName + Parser.Token.Data;
      if BuildRName then
        if not (Parser.Token.ID in [tkSpace, tkCRLF, tkColon, tkSquareOpen, tkDefault, tkStored, tkRead, tkWrite]) then
          MInfo.FRName := MInfo.FRName + Parser.Token.Data;
      case Parser.Token.ID of
        tkRoundOpen:   Inc(RoundOpen);
        tkRoundClose:  Dec(RoundOpen);
        // Stop building RName when encountering a '['
        tkSquareOpen:
          begin
            Inc(SquareOpen);
            BuildRName := False;
          end;
        tkSquareClose: Dec(SquareOpen);
        tkProperty:    BuildRName := True;
        tkColon:       BuildRName := False;
        // Stop building RName when encountering default/stored/read/write
        tkDefault:     BuildRName := False;
        tkStored:      BuildRName := False;
        tkRead:        BuildRName := False;
        tkWrite:       BuildRName := False;
      end;
      Parser.NextToken;
    end;
  end;

begin
  MethodVisibility := tkPublished;
  while not (Parser.Token.ID in [tkEnd, tkNull]) do
  begin
    case Parser.Token.ID of
      tkProcedure,
      tkFunction,
      tkConstructor,
      tkDestructor,
      tkOperator:      LoadProc;

      tkPrivate,
      tkProtected,
      tkPublished,
      tkPublic:        MethodVisibility := Parser.Token.ID;

      tkIdentifier:    LoadVariable;
      tkProperty:      LoadProperty;
      tkStrict:        // Ignore for now
    end; // case
    Parser.NextToken;
  end;
end;

function TBrowseMethodInfoItem.GetCollection: TBrowseClassInfoCollection;
begin
  Result := TBrowseClassInfoCollection(inherited Collection);
end;

function TBrowseMethodInfoItem.GetInterfaceLine: Integer;

  function TokenMatchesMethodType(Token: TTokenKind; MethodType: TMethodType): Boolean;
  begin
    Result := True;
    case MethodType of
      ctProperty:  Result := Token = tkProperty;
      ctMethod:    Result := Token in [tkFunction, tkProcedure, tkOperator];
    end;
  end;

var
  FileContent: string;
  Parser: TmPasParser;
  LocalEditReader: TEditReader;
  LastTokenType: TTokenKind;
begin
  Result := 0;

  LocalEditReader := Collection.ClassList.EditRead;
  LocalEditReader.FileName := Collection.FileName;
  LocalEditReader.Reset;

  FileContent := LocalEditReader.GetText;
  LocalEditReader.FreeFileData;
  Parser := TmPasParser.Create;
  try
    Parser.Origin := @FileContent[1];
    if Collection.SetParser(Parser) then
    begin
      // Temporarily set the line number in case we don't find a perfect match
      Result := Parser.Token.LineNumber;
      LastTokenType := Parser.Token.ID;
      while not (Parser.Token.ID in [tkEnd, tkRoundClose, tkPrivate, tkPublic, tkProtected, tkClass, tkColon, tkNull]) do
      begin
        LastTokenType := Parser.Token.ID;
        Parser.NextNonJunk;
      end;
      while not (Parser.Token.ID in [tkEnd, tkNull]) do
      begin
        if SameText(Parser.Token.Data, Self.RName) and TokenMatchesMethodType(LastTokenType, Self.FMethodType) then
        begin
          FLineNo := Parser.Token.LineNumber; // Might as well update line number in object
          Result := Parser.Token.LineNumber;
          Exit;
        end;
        LastTokenType := Parser.Token.ID;
        Parser.NextNonJunk;
      end;
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

function TBrowseMethodInfoItem.GetImplementationLine: Integer;
var
  FileContent: string;
  Parser: TmPasParser;
  LocalEditReader: TEditReader;
begin
  LocalEditReader := Collection.ClassList.EditRead;
  LocalEditReader.FileName := Collection.FileName;
  LocalEditReader.Reset;

  FileContent := LocalEditReader.GetText;
  LocalEditReader.FreeFileData;
  Parser := TmPasParser.Create;
  try
    Parser.Origin := @FileContent[1];
    Result := Parser.GetMethodImpLine(Collection.Name, Self.RName);
  finally
    FreeAndNil(Parser);
  end;
end;

function TBrowseClassInfoCollection.GetObjectDerivedFrom: string;
var
  FirstAncestorEndPos: Integer;
begin
  FirstAncestorEndPos := Pos(',', DerivedFrom);
  if FirstAncestorEndPos > 0 then
    Result := Trim(Copy(FDerivedFrom, 1, FirstAncestorEndPos - 1))
  else
    Result := DerivedFrom;
end;

end.

