unit GX_MacroLibrary;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, Consts,
  Controls, Forms, Dialogs, Graphics,
  ComCtrls, StdCtrls, ExtCtrls, Menus,
  OmniXML,
  GX_Experts, GX_ConfigurationInfo, GX_KbdShortCutBroker, GX_GenericUtils,
  GX_IdeDock, ActnList, ImgList, ToolWin, ToolsAPI, ImageList, Actions,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

const
  KeyboardMacroMagic = $524F5054;

type
  TMacroSpecialKey = (
    mskInvalid,
    mskBkSp, mskTab, mskEsc, mskEnter,
    mskPgUp, mskPgDn, mskEnd, mskHome, mskLeft, mskUp, mskRight, mskDown,
    mskIns, mskDel,
    mskF1, mskF2, mskF3, mskF4, mskF5, mskF6, mskF7, mskF8, mskF9, mskF10, mskF11, mskF12);

var // these resource strings are declared in unit Consts
  MacroSpecialKeyStrings: array[TMacroSpecialKey] of string = (
    srUnknown,
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter,
    SmkcPgUp, SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight, SmkcDown,
    SmkcIns, SmkcDel,
    'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12');

type
  TMacroKey = packed record
    case byte of
    0: (
      Code: Word;
      Modifier: Word;
    );
    1: (
      wc: WideChar;
    );
    3: (
      Full: LongWord;
    );
    4: (
      LoByte: Byte;
    );
    5: (
      AsPointer: Pointer;
    );
  end;

type
  TMacroInfo = class(TObject)
  private
    FStream: TMemoryStream;
    FTimeStamp: TDateTime;
    FName: string;
    FDescription: string;
    procedure SetStream(const Value: TStream);
    function GetStream: TStream;
  public
    class function EncodeMacroKey(wc: WideChar; Special: TMacroSpecialKey;
      Ctrl, Shift, Alt: Boolean): TMacroKey;
    class procedure DecodeMacroKey(MacroKey: TMacroKey;
      out wc: WideChar; out Special: TMacroSpecialKey; out Ctrl, Shift, Alt: Boolean);
    class function MacroKeyToText(MacroKey: TMacroKey): TGXUnicodeString;
    class function TryDecode(AStream: TStream; AStrings: TGXUnicodeStringList): Boolean; overload;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToXML(Node: IXMLElement);
    procedure LoadFromXML(Node: IXMLElement);
    function TryDecode(AStrings: TGXUnicodeStringList): boolean; overload;
    procedure Encode(AStrings: TGXUnicodeStringList);
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Stream: TStream read GetStream write SetStream;
  end;

  TfmMacroLibrary = class(TfmIdeDockForm)
    lvMacros: TListView;
    Toolbar: TToolBar;
    tbnClear: TToolButton;
    tbnCopy: TToolButton;
    Actions: TActionList;
    actEditCopy: TAction;
    actEditClear: TAction;
    actViewToolbar: TAction;
    tbnDelete: TToolButton;
    mnuMacroPop: TPopupMenu;
    mitClear: TMenuItem;
    mitCopy: TMenuItem;
    actEditDelete: TAction;
    mitDelete: TMenuItem;
    mitSep1: TMenuItem;
    mitShowToolbar: TMenuItem;
    actViewDescription: TAction;
    mitShowDescription: TMenuItem;
    ilLarge: TImageList;
    ilSmall: TImageList;
    actFileSave: TAction;
    actFileLoad: TAction;
    tbnSep3: TToolButton;
    tbnSave: TToolButton;
    tbnLoad: TToolButton;
    mitSep2: TMenuItem;
    mitLoadmacro: TMenuItem;
    mitSavemacro: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    actEditSuspend: TAction;
    btnSuspend: TToolButton;
    tbnSep4: TToolButton;
    mitSep3: TMenuItem;
    mitSuspend: TMenuItem;
    tbnSep2: TToolButton;
    tbnRecord: TToolButton;
    tbnPlayback: TToolButton;
    tbnSep1: TToolButton;
    actRecord: TAction;
    actPlayback: TAction;
    mitRename: TMenuItem;
    tbEdit: TToolButton;
    pnlDescription: TPanel;
    tbnSep5: TToolButton;
    tbnHelp: TToolButton;
    actHelp: TAction;
    actEditRename: TAction;
    actFileClose: TAction;
    mmoMacroDescription: TMemo;
    lblMacroDesc: TLabel;
    Splitter: TSplitter;
    actPromptForName: TAction;
    mitPromptforName: TMenuItem;
    mitSep5: TMenuItem;
    actEditMacro: TAction;
    miShowContent: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvMacrosDblClick(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditClearExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actViewToolbarExecute(Sender: TObject);
    procedure lvMacrosKeyPress(Sender: TObject; var Key: Char);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lvMacrosEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure lvMacrosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mmoMacroDescriptionChange(Sender: TObject);
    procedure lvMacrosChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure actViewDescriptionExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileLoadExecute(Sender: TObject);
    procedure actEditSuspendExecute(Sender: TObject);
    procedure actRecordExecute(Sender: TObject);
    procedure actPlaybackExecute(Sender: TObject);
    procedure actEditRenameExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure lvMacrosInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
    procedure actPromptForNameExecute(Sender: TObject);
    procedure actEditMacroExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
  private
    FDataList: TList;
    FSBWidth: Integer;
    FSuspended: Boolean;
    FShortCut: IGxKeyboardShortCut;
    FPromptForName: Boolean;
    procedure InsertMacro(Index: Integer; Info: TMacroInfo);
    procedure ClearDataList;
    procedure LoadMacros;
    procedure SaveMacros;
    procedure CopyMacroToPlayback;
    procedure UpdateMemoState;
    procedure ResizeColumns;
    procedure InstallKeyboardBindings;
    procedure RemoveKeyboardBindings;
    function  MacroInfoForItem(Item: TListItem): TMacroInfo;
    function  MacroInfoFromPointer(Ptr: Pointer): TMacroInfo;
    function  RegistryKey: string;
    function HaveSelectedMacro: Boolean;
    function SelectedMacroIndex: Integer;
    procedure SelectMacro(Index: Integer);
    procedure SelectFirstMacro;
    procedure SetDescriptionVisible(const Value: Boolean);
    function GetDescriptionVisible: Boolean;
    property DescriptionVisible: Boolean read GetDescriptionVisible write SetDescriptionVisible;
    procedure RecordShortcutCallback(Sender: TObject);
    procedure GetRecordBitmap(_bmp: TBitmap);
    procedure GetPlayBitmap(_bmp: TBitmap);
  protected
    procedure AddToMacroLibrary(CR: IOTARecord);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

  TMacroLibExpert = class(TGX_Expert)
  private
    FStoragePath: string;
    function GetStorageFile: string;
  protected
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
    function GetBitmapFileName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    function HasConfigOptions: Boolean; override;
    property StorageFile: string read GetStorageFile;
    function IsDefaultActive: Boolean; override;
  end;

  TMacroLibRecordExpert = class(TGX_Expert)
  protected
    procedure SetShortCut(Value: TShortCut); override;
  public
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
    function GetBitmap: Graphics.TBitmap; override;
  end;

  TMacroLibPlaybackExpert = class(TGX_Expert)
  protected
    procedure SetShortCut(Value: TShortCut); override;
  public
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetActionCaption: string; override;
    class function ConfigurationKey: string; override;
    class function GetName: string; override;
    function GetHelpString: string; override;
    function GetBitmap: Graphics.TBitmap; override;
  end;

var
  fmMacroLibrary: TfmMacroLibrary = nil;
  MacroLibExpert: TMacroLibExpert = nil;

implementation

{$R *.dfm}

uses
  ActiveX,
  GX_GxUtils, GX_OtaUtils,
  GX_SharedImages, GX_XmlUtils,
  GX_MacroLibraryNamePrompt, GX_MacroLibraryConfig, Math, GX_IdeUtils,
  GX_MessageBox, GX_dzVclUtils;

type
  TIDEMacroBugMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

const
  MacroLibraryStorageFileName = 'MacroLibrary.xml';
  RegistrySection = 'Settings';
  KeyboardMacrosElement = 'KeyboardMacros';
  KeyboardMacroElement = 'KeyboardMacro';
  NameAttribute = 'Name';
  DateAttribute = 'Date';
  DescriptionElement = 'Description';
  MacroElement = 'Macro';

resourcestring
  SMacroLibCaption = '&Keyboard Macro Library';

const
  UnknownName = 'Unknown';
  NameColMinMidth = 100;

{ Utility functions }

function GetMacroLibraryForm: TfmMacroLibrary;
begin
  if fmMacroLibrary = nil then
    fmMacroLibrary := TfmMacroLibrary.Create(nil);
  Result := fmMacroLibrary;
  Result.SelectFirstMacro;
end;

procedure EncodeStream(Stream: TStream; var Str: string);
var
  Buffer: Byte;
begin
  Str := '';
  Stream.Position := 0;
  while Stream.Read(Buffer, 1) = 1 do
    Str := Str + Format('%2.2x', [Buffer]);
end;

procedure DecodeString(const Str: string; Stream: TStream);
var
  Buffer: Byte;
  HexStr: string;
  i: Integer;
begin
  i := 1;
  while i < Length(Str) do
  begin
    HexStr := '$' + Copy(Str, i, 2);
    Buffer := StrToIntDef(HexStr, $00);
    Stream.Write(Buffer, 1);
    Inc(i, 2);
  end;
  Stream.Position := 0;
end;

{ TMacroInfo }

constructor TMacroInfo.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TMacroInfo.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TMacroInfo.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TMacroInfo.LoadFromFile(const FileName: string);
var
  Doc: IXMLDocument;
begin
  Doc := CreateXMLDoc;
  Doc.PreserveWhiteSpace := True;
  if FileExists(FileName) then
    if Doc.Load(FileName) then
      LoadFromXML(Doc.DocumentElement);
end;

procedure TMacroInfo.LoadFromXML(Node: IXmlElement);
var
  StreamStr: string;
  MS: TMemoryStream;
  A, N: IXMLNode;
begin
  // Name
  A := Node.Attributes.GetNamedItem(NameAttribute);
  if Assigned(A) then
    Name := A.NodeValue
  else
    Name := UnknownName;
  // Date
  A := Node.Attributes.GetNamedItem(DateAttribute);
  if Assigned(A) then
    TimeStamp := IsoStringToDateTimeDef(A.NodeValue, Now)
  else
    TimeStamp := Now;
  // Description
  N := Node.SelectSingleNode(DescriptionElement);
  if Assigned(N) then
    Description := N.Text;
  // Stream
  N := Node.SelectSingleNode(MacroElement);
  if Assigned(N) then
    StreamStr := N.Text
  else
    StreamStr := '54504F5200'; // An empty macro stream
  MS := TMemoryStream.Create;
  try
    DecodeString(StreamStr, MS);
    Stream := MS;
  finally
    FreeAndNil(MS);
  end;
end;

procedure TMacroInfo.SaveToFile(const FileName: string);
var
  Doc: IXMLDocument;
  Root: IXMLElement;
begin
  Doc := CreateXMLDoc;
  Doc.PreserveWhiteSpace := True;
  AddXMLHeader(Doc);
  Root := Doc.CreateElement(KeyboardMacroElement);
  SaveToXML(Root);
  Doc.AppendChild(Root);
  Doc.Save(FileName, ofFlat);
end;

procedure TMacroInfo.SaveToXML(Node: IXMLElement);
var
  StreamStr: string;
  E: IXMLElement;
begin
  Node.SetAttribute(NameAttribute, Name);
  Node.SetAttribute(DateAttribute, IsoDateTimeToStr(TimeStamp));
  E := Node.OwnerDocument.CreateElement(DescriptionElement);
  E.Text := Description;
  Node.AppendChild(E);
  E := Node.OwnerDocument.CreateElement(MacroElement);
  EncodeStream(Stream, StreamStr);
  E.Text := StreamStr;
  Node.AppendChild(E);
end;

procedure TMacroInfo.SetStream(const Value: TStream);
begin
  Value.Position := 0;
  FStream.LoadFromStream(Value);
  FStream.Position := 0;
end;

// http://blog.dummzeuch.de/2017/03/04/interpreting-delphi-ide-keyboard-macros/

class procedure TMacroInfo.DecodeMacroKey(MacroKey: TMacroKey;
  out wc: WideChar; out Special: TMacroSpecialKey; out Ctrl, Shift, Alt: Boolean);
begin
  Special := mskInvalid;
  wc :=#0;
  // special keys
  Ctrl := (MacroKey.Modifier and $10) <> 0;
  Shift := (MacroKey.Modifier and $40) <> 0;
  Alt := (MacroKey.Modifier and $20) <> 0;
  if (MacroKey.Modifier and $0088) = $0088 then begin
    case MacroKey.Code of
      // $0000..$0020 -> invalid
      $0021..$0028: // mskPgUp .. mskDown
        Special := TMacroSpecialKey(Ord(mskPgUp) + MacroKey.LoByte - $0021);
      // $29..$2C -> invalid
      $002D:
        Special := mskIns;
      $002E:
        Special := mskDel;
      // $2F..$6F  -> invalid
      $0070..$0087: // mskF1 .. mskF12
        Special := TMacroSpecialKey(Ord(mskF1) + MacroKey.LoByte - $006F);
      // $88..$FF -> invalid
    else
      asm nop end; // invalid
    end;
  end else if MacroKey.Modifier = $0080 then begin
    case MacroKey.Code of
      $0008:
        Special := mskBkSp;
      $0009:
        Special := mskTab;
      $000D:
       Special := mskEnter;
    else
      asm nop end; // invalid
    end;
  end else if MacroKey.Modifier <> 0 then begin
    asm nop end; // invalid
  end else begin
    if (MacroKey.Code < $001F) then
      asm nop end // invalid
    else if MacroKey.Code = $001F then
      Special := mskEsc
    else if MacroKey.Code = $007F then
      asm nop end // invalid
    else
      wc := MacroKey.wc;
  end;
end;

class function TMacroInfo.EncodeMacroKey(wc: WideChar; Special: TMacroSpecialKey; Ctrl, Shift, Alt: Boolean): TMacroKey;
begin
  Result.Full := 0;
  case Special of
    mskInvalid: begin
        Result.wc := wc;
      end;
    mskBkSp: begin
      Result.Code := $0008;
      Result.Modifier := $0080;
    end;
    mskTab:  begin
      Result.Code := $0009;
      Result.Modifier := $0080;
    end;
    mskEsc:  begin
      Result.Code := $001F;
    end;
    mskEnter: begin
      Result.Code := $000D;
      Result.Modifier := $0080;
    end;
    mskPgUp..mskDown: begin
        Result.Code := Ord(Special) - Ord(mskPgUp) + $0021;
        Result.Modifier := $0088;
      end;
    mskIns: begin
        Result.Code := $002D;
        Result.Modifier := $0088;
      end;
    mskDel: begin
        Result.Code := $002E;
        Result.Modifier := $0088;
      end;
    mskF1..mskF12: begin
      Result.Code := Ord(Special) - Ord(mskF1) + $0070;
      Result.Modifier := $0088;
    end;
  end;
  if Ctrl then
    Result.Modifier := Result.Modifier or $10;
  if Shift then
    Result.Modifier := Result.Modifier or $40;
  if Alt then
    Result.Modifier := Result.Modifier or $20;
end;

class function TMacroInfo.MacroKeyToText(MacroKey: TMacroKey): TGXUnicodeString;
var
  wc: WideChar;
  Special: TMacroSpecialKey;
  Ctrl: Boolean;
  Shift: Boolean;
  Alt: Boolean;
begin
  DecodeMacroKey(MacroKey, wc, Special, Ctrl, Shift, Alt);
  if Special = mskInvalid then begin
    if wc <> #0 then
      Result := wc
    else
      Result := Format('unkown (%.4x)', [MacroKey.Code]);
  end else begin
    Result := MacroSpecialKeyStrings[Special];
    if Ctrl then
      Result := SmkcCtrl + Result;
    if Shift then
      Result := SmkcShift + Result;
    if Alt then
      Result := SmkcAlt + Result;
  end;
end;

class function TMacroInfo.TryDecode(AStream: TStream; AStrings: TGXUnicodeStringList): boolean;

  function Read(var Buffer; Count: Longint): Boolean;
  begin
    Result := (AStream.Read(Buffer, Count) = Count);
  end;

var
  Magic: Longword;
  Flag: Byte;
  MacroKey: TMacroKey;
  Start: TGXUnicodeString;
  s: TGXUnicodeString;
begin
  Start := '';
  Result := False;
  AStream.Position := 0;
  if not Read(Magic, SizeOf(Magic)) or (Magic <> KeyboardMacroMagic) then
    Exit;
  if not Read(Flag, SizeOf(Flag)) then
    Exit;
  while Flag = $01 do begin
    if not Read(MacroKey, SizeOf(MacroKey)) then
      Exit;
    s := MacroKeyToText(MacroKey);
    if Length(s) = 1 then begin
      Start := Start + s;
    end else begin
      if Start <> '' then begin
        AStrings.Add(Start);
        Start := '';
      end;
      AStrings.AddObject(s, MacroKey.AsPointer);
    end;
    if not Read(Flag, SizeOf(Flag)) then
      Exit;
  end;
  if Start <> '' then
    AStrings.Add(Start);
  Result := True;
end;

function TMacroInfo.TryDecode(AStrings: TGXUnicodeStringList): Boolean;
begin
  Result := TryDecode(FStream, AStrings);
end;

procedure TMacroInfo.Encode(AStrings: TGXUnicodeStringList);
const
  KeyByte: Byte = $01;
  EndByte: Byte = $00;
var
  Magic: LongWord;
  i: Integer;
  MacroKey: TMacroKey;
  ws: TGXUnicodeString;
  j: Integer;
begin
  FStream.Clear;
  Magic := KeyboardMacroMagic;
  FStream.WriteBuffer(Magic, SizeOf(Magic));
  for i := 0 to AStrings.Count - 1 do begin
    MacroKey.AsPointer := AStrings.Objects[i];
    if MacroKey.Full <> 0 then begin
      FStream.WriteBuffer(KeyByte, SizeOf(KeyByte));
      FStream.WriteBuffer(MacroKey.Full, SizeOf(MacroKey.Full));
    end else begin
      ws := AStrings[i];
      for j := 1 to Length(ws) do begin
        MacroKey.Full := 0;
        MacroKey.wc := ws[j];
        FStream.WriteBuffer(KeyByte, SizeOf(KeyByte));
        FStream.WriteBuffer(MacroKey.Full, SizeOf(MacroKey.Full));
      end;
    end;
  end;
  FStream.WriteBuffer(EndByte, SizeOf(EndByte));
end;

{ TfmMacroLibrary }

procedure TfmMacroLibrary.RecordShortcutCallback(Sender: TObject);
var
  KS: IOTAKeyboardServices;
begin
  KS := GxOtaGetKeyboardServices;
  Assert(Assigned(KS));
  if not KS.CurrentPlayback.IsPlaying then
    if KS.CurrentRecord.IsRecording then
    begin
      KS.ResumeRecord;
      // Add to macro library
      if Assigned(fmMacroLibrary) and MacroLibExpert.Active and not fmMacroLibrary.FSuspended then
        fmMacroLibrary.AddToMacroLibrary(KS.CurrentRecord);
    end else
      KS.ResumeRecord;
end;

procedure TfmMacroLibrary.ClearDataList;
var
  i: Integer;
begin
  lvMacros.Items.Clear;
  mmoMacroDescription.Lines.Text := '';
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
      MacroInfoFromPointer(FDataList[i]).Free;
    FDataList.Clear;
  end;
  UpdateMemoState;
end;

procedure TfmMacroLibrary.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create(AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + RegistryKey);
  try
    Settings.SaveForm(Self, 'Window');
    Settings.WriteInteger('Window', 'ViewStyle', Integer(lvMacros.ViewStyle));
    Settings.WriteBool(RegistrySection, 'Suspended', FSuspended);
    Settings.WriteBool('Window', 'ViewToolbar', Toolbar.Visible);
    Settings.WriteBool(RegistrySection, 'ViewDescription', DescriptionVisible);
    Settings.WriteInteger('Window', 'DescriptionSize', pnlDescription.Height);
    Settings.WriteBool(RegistrySection, 'PromptForName', FPromptForName);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmMacroLibrary.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create(AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + RegistryKey);
  try
    Settings.LoadForm(Self, 'Window');
    FSuspended := Settings.ReadBool(RegistrySection, 'Suspended', False);
    //lvMacros.ViewStyle := TViewStyle(RegIni.ReadEnumerated(RegistrySection, 'ViewStyle', TypeInfo(TViewStyle), Integer(vsReport)));
    Toolbar.Visible := Settings.ReadBool('Window', 'ViewToolbar', True);
    DescriptionVisible := Settings.ReadBool(RegistrySection, 'ViewDescription', True);
    pnlDescription.Height := Settings.ReadInteger('Window', 'DescriptionSize', pnlDescription.Height);
    FPromptForName := Settings.ReadBool(RegistrySection, 'PromptForName', False);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmMacroLibrary.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfmMacroLibrary.lvMacrosDblClick(Sender: TObject);
begin
  actPlayback.Execute;
end;

procedure TfmMacroLibrary.SaveMacros;
var
  Doc: IXMLDocument;
  Root: IXMLElement;
  i: Integer;
  Info: TMacroInfo;
  MacroItem: IXMLElement;
begin
  // We are calling SaveMacros from the destructor where
  // we may be in a forced clean-up due to an exception.
  if ExceptObject <> nil then
    Exit;

  Doc := CreateXMLDoc;
  Doc.PreserveWhiteSpace := True;
  AddXMLHeader(Doc);
  Root := Doc.CreateElement(KeyboardMacrosElement);
  Doc.AppendChild(Root);
  for i := 0 to FDataList.Count - 1 do
  begin
    Info := MacroInfoFromPointer(FDataList[i]);
    MacroItem := Doc.CreateElement(KeyboardMacroElement);
    Info.SaveToXML(MacroItem);
    Root.AppendChild(MacroItem);
  end;

  if PrepareDirectoryForWriting(ExtractFileDir(MacroLibExpert.StorageFile)) then
    Doc.Save(MacroLibExpert.StorageFile, ofFlat);
end;

procedure TfmMacroLibrary.LoadMacros;
var
  i: Integer;
  Info: TMacroInfo;
  Doc: IXMLDocument;
  Nodes: IXMLNodeList;
begin
  ClearDataList;
  Doc := CreateXMLDoc;
  Doc.PreserveWhiteSpace := True;
  if FileExists(MacroLibExpert.StorageFile) then begin
    Doc.Load(MacroLibExpert.StorageFile);
    Nodes := Doc.DocumentElement.SelectNodes(KeyboardMacroElement);
    for i := 0 to Nodes.Length - 1 do
    begin
      // Create and populate the macro info object
      Info := TMacroInfo.Create;
      Info.LoadFromXML(Nodes.Item[i] as IXMLElement);
      FDataList.Add(Info);
      // Add it to the list view
      InsertMacro(-1, Info);
    end;
    SelectFirstMacro;
  end;
end;

constructor TfmMacroLibrary.Create(AOwner: TComponent);
resourcestring
  SLoadingFailed = 'Loading of stored macros failed.';
begin
  inherited;

  TControl_SetMinConstraints(Self);

  FSuspended := False;
  FDataList := TList.Create;
  LoadSettings;
  FSBWidth := GetSystemMetrics(SM_CXVSCROLL);
  ResizeColumns;
  CenterForm(Self);

  InstallKeyboardBindings;
  Assert(Assigned(MacroLibExpert));
  MacroLibExpert.SetFormIcon(Self);

  // Since we do not require macros, continue in the event of an error
  try
    LoadMacros;
  except
    on E: Exception do
    begin
      GxLogAndShowException(E, SLoadingFailed);
      // Swallow exceptions
    end;
  end;
end;

destructor TfmMacroLibrary.Destroy;
begin
  RemoveKeyboardBindings;
  SaveMacros;
  ClearDataList;
  FreeAndNil(FDataList);
  SaveSettings;

  inherited;
  fmMacroLibrary := nil;
end;

procedure TfmMacroLibrary.actEditCopyExecute(Sender: TObject);
begin
  CopyMacroToPlayback;
end;

procedure TfmMacroLibrary.actEditClearExecute(Sender: TObject);
resourcestring
  SClearConfirm = 'Delete all macros from the library?';
begin
  if MessageDlg(SClearConfirm, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    ClearDataList;
end;

procedure TfmMacroLibrary.lvMacrosKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    actEditCopy.Execute;
end;

procedure TfmMacroLibrary.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  IsMacroSelected: Boolean;
begin
  IsMacroSelected := HaveSelectedMacro;
  actEditClear.Enabled   := lvMacros.Items.Count > 0;
  actEditDelete.Enabled  := IsMacroSelected;
  actEditCopy.Enabled    := IsMacroSelected;
  actEditMacro.Enabled := IsMacroSelected;
  actEditRename.Enabled  := IsMacroSelected;
  actFileSave.Enabled    := IsMacroSelected;
  actEditSuspend.Checked := FSuspended;
  actRecord.Checked      := GxOtaEditorIsRecordingMacro;
  actViewToolbar.Checked := Toolbar.Visible;
  actViewDescription.Checked    := DescriptionVisible;
  actPromptForName.Checked := FPromptForName;
end;

procedure TfmMacroLibrary.actViewToolbarExecute(Sender: TObject);
begin
  Toolbar.Visible := not Toolbar.Visible;
end;

procedure TfmMacroLibrary.CopyMacroToPlayback;
var
  OldInfo: TMacroInfo;
  Index: Integer;
  Stream: IStream;
  KS: IOTAKeyboardServices;
begin
  Index := SelectedMacroIndex;
  if Index <> -1 then
  begin
    OldInfo := MacroInfoFromPointer(FDataList[Index]);
    OldInfo.Stream.Position := 0;

    Stream := TStreamAdapter.Create(OldInfo.Stream);
    KS := GxOtaGetKeyboardServices;
    Assert(Assigned(KS));
    if not KS.CurrentPlayback.IsPlaying and not KS.CurrentRecord.IsRecording then
    begin
      try
        KS.CurrentPlayback.Clear;
        KS.CurrentPlayback.ReadFromStream(Stream);
      except
        Application.HandleException(Self);
      end;
    end;
    lvMacros.Items.Delete(Index);
    FDataList.Delete(Index);
    FDataList.Insert(0, OldInfo);

    InsertMacro(0, OldInfo);
    SelectFirstMacro;
    GxOtaShowCurrentSourceEditor;
  end;
end;

procedure TfmMacroLibrary.lvMacrosEdited(Sender: TObject; Item: TListItem; var S: String);
begin
  if Assigned(Item) then
    MacroInfoForItem(Item).Name := S;
end;

procedure TfmMacroLibrary.actEditDeleteExecute(Sender: TObject);
var
  Index : Integer;
begin
  Index := SelectedMacroIndex;
  if Index <> -1 then
  begin
    lvMacros.Items.Delete(Index);
    MacroInfoFromPointer(FDataList[Index]).Free;
    FDataList.Delete(Index);
    mmoMacroDescription.Lines.Text := '';
    if Assigned(lvMacros.ItemFocused) then
      SelectMacro(lvMacros.ItemFocused.Index);
    UpdateMemoState;
  end;
end;

procedure TfmMacroLibrary.lvMacrosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F2) and HaveSelectedMacro then
    lvMacros.Selected.EditCaption;
end;

procedure TfmMacroLibrary.mmoMacroDescriptionChange(Sender: TObject);
begin
  if HaveSelectedMacro then
    MacroInfoForItem(lvMacros.Selected).Description := mmoMacroDescription.Lines.Text;
end;

procedure TfmMacroLibrary.lvMacrosChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Change = ctState) and Assigned(Item) and Item.Selected then
    if Assigned(Item) then
      mmoMacroDescription.Text := MacroInfoForItem(Item).Description;
  UpdateMemoState;
end;

procedure TfmMacroLibrary.actEditMacroExecute(Sender: TObject);
var
  Item: TListItem;
  sl: TGXUnicodeStringList;
  mi: TMacroInfo;
begin
  Item := lvMacros.Selected;
  if not Assigned(Item) then
    Exit;
  sl := TGXUnicodeStringList.Create;
  try
    mi := MacroInfoForItem(Item);
    if not mi.TryDecode(sl) then
      Exit;
    TfmMacroLibraryNamePrompt.Execute(Self, mi.FName, mi.FDescription, sl);
    mi.Encode(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmMacroLibrary.UpdateMemoState;
begin
  if not HaveSelectedMacro then
    mmoMacroDescription.Text := '';
  mmoMacroDescription.ReadOnly := (lvMacros.Items.Count = 0) or not HaveSelectedMacro;
end;

procedure TfmMacroLibrary.actViewDescriptionExecute(Sender: TObject);
begin
  DescriptionVisible := not DescriptionVisible;
end;

procedure TfmMacroLibrary.FormResize(Sender: TObject);
begin
  inherited;
  ResizeColumns;
  lvMacros.Invalidate;
end;

procedure TfmMacroLibrary.actFileSaveExecute(Sender: TObject);
var
  Index: Integer;
  Info: TMacroInfo;
begin
  Index := SelectedMacroIndex;
  if Index <> -1 then
  begin
    Info := MacroInfoFromPointer(FDataList[Index]);
    dlgSave.FileName := Info.Name;
    if dlgSave.Execute then
    begin
      if Pos(UnknownName, Info.Name) = 1 then
      begin
        Info.Name := ChangeFileExt(ExtractFileName(dlgSave.FileName), '');
        lvMacros.Selected.Caption := Info.Name;
      end;
      Info.SaveToFile(dlgSave.FileName);
    end;
  end;
end;

procedure TfmMacroLibrary.actFileCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMacroLibrary.actFileLoadExecute(Sender: TObject);
var
  Info: TMacroInfo;
begin
  if dlgOpen.Execute then
  begin
    Info := TMacroInfo.Create;
    FDataList.Insert(0, Info);
    Info.LoadFromFile(dlgOpen.FileName);

    InsertMacro(0, Info);
    SelectFirstMacro;
  end;
end;

procedure TfmMacroLibrary.ResizeColumns;
var
  ColWidth: Integer;
begin
  ColWidth := Max(lvMacros.Width - fSBWidth - 6 - lvMacros.Columns[1].MinWidth, NameColMinMidth);
  lvMacros.Columns[0].MaxWidth := ColWidth;
  lvMacros.Columns[0].MinWidth := ColWidth;
  lvMacros.Columns[0].Width := ColWidth;
end;

procedure TfmMacroLibrary.actEditSuspendExecute(Sender: TObject);
begin
  FSuspended := not FSuspended;
end;

procedure TfmMacroLibrary.InstallKeyboardBindings;
begin
  Assert(not Assigned(FShortCut), 'FShortCut is already assigend');
  FShortCut := GxKeyboardShortCutBroker.RequestOneKeyShortCut(RecordShortcutCallback, ShortCut(Ord('R'), [ssCtrl, ssShift]));
end;

procedure TfmMacroLibrary.RemoveKeyboardBindings;
begin
  FShortCut := nil;
end;

function TfmMacroLibrary.MacroInfoForItem(Item: TListItem): TMacroInfo;
begin
  Assert(Assigned(Item));
  Assert(Assigned(Item.Data));
  Result := MacroInfoFromPointer(Item.Data);
end;

function TfmMacroLibrary.MacroInfoFromPointer(Ptr: Pointer): TMacroInfo;
begin
  Assert(Assigned(Ptr));
  Result := TObject(Ptr) as TMacroInfo;
end;

function TfmMacroLibrary.RegistryKey: string;
begin
  Result := TMacroLibExpert.GetName;
end;

function TfmMacroLibrary.HaveSelectedMacro: Boolean;
begin
  Result := Assigned(lvMacros.Selected);
end;

function TfmMacroLibrary.SelectedMacroIndex: Integer;
begin
  Result := -1;
  if HaveSelectedMacro then
    Result := lvMacros.Selected.Index;
end;

procedure TfmMacroLibrary.SelectMacro(Index: Integer);
begin
  if (Index >= 0) and (Index < lvMacros.Items.Count) then
  begin
    lvMacros.Selected := lvMacros.Items[Index];
    lvMacros.ItemFocused := lvMacros.Selected;
  end;
end;

procedure TfmMacroLibrary.SelectFirstMacro;
begin
  SelectMacro(0);
end;

procedure TfmMacroLibrary.actRecordExecute(Sender: TObject);
var
  KS : IOTAKeyboardServices;
begin
  KS := GxOtaGetKeyboardServices;
  if not Assigned(KS) then
    Exit;

  GxOtaFocusCurrentIDEEditControl;
  if not KS.CurrentPlayback.IsPlaying then
  begin
    if KS.CurrentRecord.IsRecording then
    begin
      // Stop recording
      KS.ResumeRecord;
      // Add to macro library
      AddToMacroLibrary(KS.CurrentRecord);
    end else
    begin
      // Start recording
      KS.ResumeRecord;
      GxOtaShowCurrentSourceEditor;
    end;
  end;
end;

procedure TfmMacroLibrary.actPlaybackExecute(Sender: TObject);
begin
  actEditCopy.Execute;
  GxOtaFocusCurrentIDEEditControl;
  GxOtaGetKeyboardServices.ResumePlayback;

  MacroLibExpert.IncCallCount;
end;

procedure TfmMacroLibrary.actPromptForNameExecute(Sender: TObject);
begin
  FPromptForName := not FPromptForName;
end;

procedure TfmMacroLibrary.AddToMacroLibrary(CR: IOTARecord);
var
  Stream: IStream;
  MemStream: TMemoryStream;
  Info: TMacroInfo;
  MacroName: string;
  MacroDesc: string;
  sl: TGXUnicodeStringList;
begin
  sl := nil;
  MemStream := TMemoryStream.Create;
  try
    MacroName := UnknownName + Format('%2.2d', [FDataList.Count]);
    MacroDesc := '';
    Stream := TStreamAdapter.Create(MemStream);
    CR.WriteToStream(Stream);
    sl := TGXUnicodeStringList.Create;
    if not TMacroInfo.TryDecode(MemStream, sl) then
      sl.Clear;
    if FPromptForName then begin
      if not TfmMacroLibraryNamePrompt.Execute(Self, True, MacroName, MacroDesc, sl, FPromptForName) then
        Exit; //==>
    end;

    Info := TMacroInfo.Create;
    FDataList.Insert(0, Info);
    Info.Name := MacroName;
    Info.Description := MacroDesc;
    Info.TimeStamp := Now;
    Info.Stream := MemStream;
    InsertMacro(0, Info);
    SelectFirstMacro;

    SaveMacros;
  finally
    FreeAndNil(sl);
    FreeAndNil(MemStream)
  end;

  MacroLibExpert.IncCallCount;
end;

procedure TfmMacroLibrary.FormCreate(Sender: TObject);
begin
  inherited;
  SetToolbarGradient(ToolBar);
  //lvMacros.DoubleBuffered := True;  // Causing D2005 paint issues for docked/pinned windows
end;

procedure TfmMacroLibrary.InsertMacro(Index: Integer; Info: TMacroInfo);
var
  MacroItem : TListItem;
begin
  MacroItem := lvMacros.Items.AddItem(nil, Index);
  MacroItem.ImageIndex := 0;
  MacroItem.Caption := Info.Name;
  MacroItem.SubItems.Add(DateTimeToStr(Info.TimeStamp));
  MacroItem.Data := Info;
end;

procedure TfmMacroLibrary.lvMacrosInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
begin
  InfoTip := MacroInfoForItem(Item).Description;
end;

procedure TfmMacroLibrary.actHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 44);
end;

procedure TfmMacroLibrary.actEditRenameExecute(Sender: TObject);
begin
  if Assigned(lvMacros.Selected) then
    lvMacros.Selected.EditCaption;
end;

procedure TfmMacroLibrary.SetDescriptionVisible(const Value: Boolean);
begin
  pnlDescription.Visible := Value;
  Splitter.Visible := Value;
end;

function TfmMacroLibrary.GetDescriptionVisible: Boolean;
begin
  Result := pnlDescription.Visible;
end;

procedure TfmMacroLibrary.GetPlayBitmap(_bmp: TBitmap);
begin
  Toolbar.Images.GetBitmap(actPlayback.ImageIndex, _bmp);
end;

procedure TfmMacroLibrary.GetRecordBitmap(_bmp: TBitmap);
begin
  Toolbar.Images.GetBitmap(actRecord.ImageIndex, _bmp);
end;

{ TMacroLibExpert }

constructor TMacroLibExpert.Create;
begin
  inherited;
  FStoragePath := ConfigInfo.ConfigPath;
  FreeAndNil(MacroLibExpert);
  MacroLibExpert := Self;
end;

destructor TMacroLibExpert.Destroy;
begin
  FreeAndNil(fmMacroLibrary);
  MacroLibExpert := nil;
  inherited;
end;

function TMacroLibExpert.GetActionCaption: string;
begin
  Result := SMacroLibCaption;
end;

class function TMacroLibExpert.GetName: string;
begin
  Result := 'MacroLibrary';
end;

procedure TMacroLibExpert.Execute(Sender: TObject);
begin
  // If the form does not exist, create it
  if fmMacroLibrary = nil then
    fmMacroLibrary := TfmMacroLibrary.Create(nil);
  IdeDockManager.ShowForm(fmMacroLibrary);
  fmMacroLibrary.SelectFirstMacro;
  fmMacroLibrary.lvMacros.SetFocus;
  if RunningRS2009 then
    ShowGxMessageBox(TIDEMacroBugMessage);
end;

procedure TMacroLibExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  // This procedure is only called once, so it is safe to
  // register the form for docking here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmMacroLibrary, fmMacroLibrary, 'fmMacroLibrary');

  if (fmMacroLibrary = nil) then
    fmMacroLibrary := TfmMacroLibrary.Create(nil);
end;

function TMacroLibExpert.GetBitmapFileName: string;
begin
  Result := 'MacroLibrary'; // Do not localize.
end;

function TMacroLibExpert.GetHelpString: string;
resourcestring
  SHelpString =
  '  Manage keyboard macros.';
begin
  Result := SHelpString;
end;

function TMacroLibExpert.GetStorageFile: string;
begin
  Result := FStoragePath + MacroLibraryStorageFileName;
end;

function TMacroLibExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TMacroLibExpert.Configure;
begin
  if TfmGxMacroLibraryConfig.Execute(fmMacroLibrary.FPromptForName) then
    fmMacroLibrary.SaveSettings;
end;

function TMacroLibExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningRS2009;
end;

{ TMacroLibRecordExpert }

class function TMacroLibRecordExpert.ConfigurationKey: string;
begin
  Result := 'MacroLibRecord';
end;

function TMacroLibRecordExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TMacroLibRecordExpert.Execute(Sender: TObject);
begin
  GetMacroLibraryForm.actRecord.Execute;
end;

function TMacroLibRecordExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Keyboard Macro Record Start/Stop';
begin
  Result := SMenuCaption;
end;

function TMacroLibRecordExpert.GetBitmap: Graphics.TBitmap;
begin
  if not Assigned(FBitmap) then begin
    FBitmap := Graphics.TBitmap.Create;
    GetMacroLibraryForm.GetRecordBitmap(FBitmap);
  end;
  result := FBitmap;
end;

function TMacroLibRecordExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Ord('R'), [ssCtrl, ssShift]);
end;

function TMacroLibRecordExpert.GetHelpString: string;
resourcestring
  SHelpString =
  '  Start or stop keyboard macro recording.';
begin
  Result := SHelpString;
end;

class function TMacroLibRecordExpert.GetName: string;
begin
  Result := 'MacroLibRecord';
end;

procedure TMacroLibRecordExpert.SetShortCut(Value: TShortCut);
begin
  inherited;
  if (GetMacroLibraryForm = nil) then
    Exit; //==>
  GetMacroLibraryForm.actRecord.ShortCut := Value;
end;

{ TMacroLibPlaybackExpert }

class function TMacroLibPlaybackExpert.ConfigurationKey: string;
begin
  Result := 'MacroLibPlayback';
end;

function TMacroLibPlaybackExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TMacroLibPlaybackExpert.Execute(Sender: TObject);
begin
  GetMacroLibraryForm.actPlayback.Execute;
end;

function TMacroLibPlaybackExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Keyboard Macro Playback';
begin
  Result := SMenuCaption;
end;

function TMacroLibPlaybackExpert.GetBitmap: Graphics.TBitmap;
begin
  if not Assigned(FBitmap) then begin
    FBitmap := Graphics.TBitmap.Create;
    GetMacroLibraryForm.GetPlayBitmap(FBitmap);
  end;
  result := FBitmap;
end;

function TMacroLibPlaybackExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Ord('P'), [ssCtrl, ssShift]);
end;

function TMacroLibPlaybackExpert.GetHelpString: string;
resourcestring
  SHelpString =
  '  Playback recorded keyboard macro.';
begin
  Result := SHelpString;
end;

class function TMacroLibPlaybackExpert.GetName: string;
begin
  Result := 'MacroLibPlayback';
end;

procedure TMacroLibPlaybackExpert.SetShortCut(Value: TShortCut);
begin
  inherited;
  if (GetMacroLibraryForm = nil) then
    Exit; //==>
  GetMacroLibraryForm.actPlayback.ShortCut := Value;
end;

{ TIDEMacroBugMessage }

function TIDEMacroBugMessage.GetMessage: string;
begin
  Result := 'RAD Studio 2009 and RAD Studio 2010 before update 2 have bugs preventing record'
  +' and playback of macros using this tool. '
  +' See QC 78289 for more details: http://qc.embarcadero.com/wc/qcmain.aspx?d=78289';
end;

initialization
  RegisterGX_Expert(TMacroLibExpert);
  RegisterGX_Expert(TMacroLibRecordExpert);
  RegisterGX_Expert(TMacroLibPlaybackExpert);

end.

