unit GX_MacroLibrary;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes,
  GX_Experts, GX_ConfigurationInfo, GX_KbdShortCutBroker,
  Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, OmniXML,
  ComCtrls, GX_IdeDock, ActnList, ImgList, ToolWin, ToolsAPI;

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
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveToXML(Node: IXMLElement);
    procedure LoadFromXML(Node: IXMLElement);
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Stream: TStream read GetStream write SetStream;
  end;

  TMacroLibExpert = class;

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
    actViewStyleLargeIcon: TAction;
    actViewStyleSmallIcon: TAction;
    actViewStyleList: TAction;
    actViewStyleDetails: TAction;
    mitViewStyle: TMenuItem;
    mitLargeIcons: TMenuItem;
    mitSmallIcons: TMenuItem;
    mitList: TMenuItem;
    mitDetails: TMenuItem;
    mitSep4: TMenuItem;
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
    pnlDescription: TPanel;
    tbnSep5: TToolButton;
    tbnHelp: TToolButton;
    actHelp: TAction;
    actEditRename: TAction;
    mmoMacroDescription: TMemo;
    lblMacroDesc: TLabel;
    Splitter: TSplitter;
    actPromptForName: TAction;
    mitPromptforName: TMenuItem;
    mitSep5: TMenuItem;
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
    procedure actViewStyleExecute(Sender: TObject);
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
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function BitmapFileName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    function HasConfigOptions: Boolean; override;
    property StorageFile: string read GetStorageFile;
    function IsDefaultActive: Boolean; override;
  end;

var
  fmMacroLibrary: TfmMacroLibrary = nil;
  MacroLibExpert: TMacroLibExpert = nil;

implementation

{$R *.dfm}

uses
  ActiveX,
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils,
  GX_SharedImages, GX_XmlUtils,
  GX_MacroLibraryNamePrompt, GX_MacroLibraryConfig, Math, GX_IdeUtils,
  GX_MessageBox;

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
  SMacroLibCaption = 'Macro &Library';
  SMacroName = 'Macro name';

const
  UnknownName = 'Unknown';
  NameColMinMidth = 100;

{ Utility functions }

procedure EncodeStream(Stream: TStream; var Str: string);
var
  Buffer: Byte;
begin
  Str := '';
  Stream.Position := 0;
  while Stream.Read(Buffer, 1) = 1 do
    Str := Str + Format('%2.2x', [Buffer]);
end;

procedure DecodeString(Str: string; Stream: TStream);
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

procedure TMacroInfo.LoadFromFile(FileName: string);
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

procedure TMacroInfo.SaveToFile(FileName: string);
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
begin
  actEditClear.Enabled   := lvMacros.Items.Count > 0;
  actEditDelete.Enabled  := HaveSelectedMacro;
  actEditCopy.Enabled    := HaveSelectedMacro;
  actEditRename.Enabled  := HaveSelectedMacro;
  actFileSave.Enabled    := HaveSelectedMacro;
  actEditSuspend.Checked := FSuspended;
  actRecord.Checked      := GxOtaEditorIsRecordingMacro;
  actViewToolbar.Checked := Toolbar.Visible;
  actViewDescription.Checked    := DescriptionVisible;
  actViewStyleLargeIcon.Checked := lvMacros.ViewStyle = vsIcon;
  actViewStyleSmallIcon.Checked := lvMacros.ViewStyle = vsSmallIcon;
  actViewStyleList.Checked      := lvMacros.ViewStyle = vsList;
  actViewStyleDetails.Checked   := lvMacros.ViewStyle = vsReport;
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

procedure TfmMacroLibrary.actViewStyleExecute(Sender: TObject);
begin
  if Sender is TComponent then
  begin
    case TComponent(Sender).Tag of
      1: lvMacros.ViewStyle := vsIcon;
      2: lvMacros.ViewStyle := vsSmallIcon;
      3: lvMacros.ViewStyle := vsList;
    else lvMacros.ViewStyle := vsReport;
    end;
  end;
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
  Assert(not Assigned(FShortCut));
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
end;

procedure TfmMacroLibrary.actPromptForNameExecute(Sender: TObject);
begin
  FPromptForName := not FPromptForName;
end;

procedure TfmMacroLibrary.AddToMacroLibrary(CR: IOTARecord);
var
  Stream: IStream;
  Info: TMacroInfo;
  MacroName: string;
  MacroDesc: string;
begin
  MacroName := UnknownName + Format('%2.2d', [FDataList.Count]);
  MacroDesc := '';
  if FPromptForName then begin
    if not TfmMacroLibraryNamePrompt.Execute(Self, MacroName, MacroDesc, FPromptForName) then
      Exit;
  end;

  Info := TMacroInfo.Create;
  FDataList.Insert(0, Info);
  Info.Name := MacroName;
  Info.Description := MacroDesc;
  Info.TimeStamp := Now;

  Stream := TStreamAdapter.Create(Info.Stream);
  CR.WriteToStream(Stream);

  InsertMacro(0, Info);
  SelectFirstMacro;

  SaveMacros;
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

procedure TMacroLibExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  // This procedure is only called once, so it is safe to
  // register the form for docking here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmMacroLibrary, fmMacroLibrary, 'fmMacroLibrary');

  if (fmMacroLibrary = nil) then
    fmMacroLibrary := TfmMacroLibrary.Create(nil);
end;

function TMacroLibExpert.BitmapFileName: string;
begin
  Result := 'MacroLibrary'; // Do not localize.
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

{ TIDEMacroBugMessage }

function TIDEMacroBugMessage.GetMessage: string;
begin
  Result := 'RAD Studio 2009 and RAD Studio 2010 before update 2 have bugs preventing record and playback of macros using this tool.  See QC 78289 for more details: http://qc.embarcadero.com/wc/qcmain.aspx?d=78289';
end;

initialization
  RegisterGX_Expert(TMacroLibExpert);

end.

