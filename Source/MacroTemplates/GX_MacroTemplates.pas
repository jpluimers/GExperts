// Main confiuration form for Macro Templates
// Original Author: Piotr Likus

unit GX_MacroTemplates;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, ComCtrls, ExtCtrls,
  StdCtrls, Menus, Dialogs, ActnList, StdActns,
  GX_ConfigurationInfo, GX_MacroFile, GX_EnhancedEditor, GX_GenericUtils;

const
  DefaultMacroFileName = 'MacroTemplates.xml';
  MinExpandDelay = 100; // ms
  DefExpandDelay = 500; // ms
  ExpandDelayNumerator = 100;

type
  // Programmer-specific static data
  TProgrammerInfo = record
    FullName: string;
    Initials: string;
  end;

  // Stores the configuration settings for the Macro Templates expert
  TTemplateSettings = class
  private
    FExpandWithChar: Boolean;
    FExpandDelay: Integer;
    procedure SetExpandWithChar(const Value: Boolean);
    procedure SetExpandDelay(const Value: Integer);
  protected
    FOwner: TObject;
    FForm: TCustomForm;
    FMacroFileName: string;
    FProgrammerName: string;
    FProgrammerInitials: string;
    procedure SetProgrammerName(const AValue: string); virtual;
    procedure SetProgrammerInitials(const AValue: string); virtual;
    procedure Changed; virtual;
  public
    constructor Create(const AOwner: TObject);
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    procedure Reload; virtual;
    property ProgrammerName: string read FProgrammerName write SetProgrammerName;
    property ProgrammerInitials: string read FProgrammerInitials write SetProgrammerInitials;
    property ExpandWithChar: Boolean read FExpandWithChar write SetExpandWithChar;
    property ExpandDelay: Integer read FExpandDelay write SetExpandDelay;
    property MacroFileName: string read FMacroFileName;
    property Form: TCustomForm read FForm write FForm;
  end;

  TMacroTemplatesIni = class(TGExpertsSettings)
  protected
    FBaseKey: string;
  public
    constructor Create;
  end;

  TfmMacroTemplates = class(TForm)
    PageControl: TPageControl;
    pmMacros: TPopupMenu;
    PROCNAMEMacro1: TMenuItem;
    PROCCLASS1: TMenuItem;
    N1: TMenuItem;
    PROJECTDIRMacro1: TMenuItem;
    PROJECTNAME1: TMenuItem;
    UNITMacro1: TMenuItem;
    N2: TMenuItem;
    DATETIMEMacro1: TMenuItem;
    DATE1: TMenuItem;
    N3: TMenuItem;
    USER1: TMenuItem;
    INPUTVARMacro1: TMenuItem;
    N4: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PROGRAMMERNAMEMacro1: TMenuItem;
    PROGRAMMERINITIALSMacro1: TMenuItem;
    pmUses: TPopupMenu;
    miAddToUses: TMenuItem;
    miDeleteUses: TMenuItem;
    tabTemplates: TTabSheet;
    tabConfig: TTabSheet;
    edProgrammerName: TEdit;
    edInitials: TEdit;
    pnlList: TPanel;
    lvTemplates: TListView;
    Panel2: TPanel;
    splTemplates: TSplitter;
    pnlMacroDetails: TPanel;
    pnlEditMacro: TPanel;
    splUses: TSplitter;
    pnlMacroText: TPanel;
    pnlUses: TPanel;
    pnlFooter: TPanel;
    pnlFooterButtons: TPanel;
    PGENMacro1: TMenuItem;
    CLASSMacro1: TMenuItem;
    DateMacros1: TMenuItem;
    TimeMacros1: TMenuItem;
    YEARMacro1: TMenuItem;
    MONTHMacro1: TMenuItem;
    DAYMacro1: TMenuItem;
    HOURMacro1: TMenuItem;
    MINUTEMacro1: TMenuItem;
    SECONDMacro1: TMenuItem;
    PROJECTGROUPNAMEMacro1: TMenuItem;
    PROJECTGROUPDIRMacro1: TMenuItem;
    N6: TMenuItem;
    MONTHSHORTNAMEMacro1: TMenuItem;
    MONTHLONGNAMEMacro1: TMenuItem;
    DAYSHORTNAMEMacro1: TMenuItem;
    DAYLONGNAMEMacro1: TMenuItem;
    IDENTMacro1: TMenuItem;
    ProjectMacros1: TMenuItem;
    grpSequenceNumber: TGroupBox;
    lblCurrentValue: TLabel;
    edGenValue: TEdit;
    btnChange: TButton;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnExport: TButton;
    btnImport: TButton;
    btnClear: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    INTERFACEMacro1: TMenuItem;
    pnlUsesImplementation: TPanel;
    lvLocalUses: TListView;
    pnlImplementationHeader: TPanel;
    pnlUsesInterface: TPanel;
    lvGlobalUses: TListView;
    pnlInterfaceHeader: TPanel;
    mitNone: TMenuItem;
    mitPas: TMenuItem;
    mitCPP: TMenuItem;
    mitHTML: TMenuItem;
    mitSQL: TMenuItem;
    Actions: TActionList;
    actSyntaxNone: TAction;
    actSyntaxPas: TAction;
    actSyntaxCPP: TAction;
    actSyntaxHTML: TAction;
    actSyntaxSQL: TAction;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditPaste: TEditPaste;
    UserIDMacros1: TMenuItem;
    Other1: TMenuItem;
    BEFORE1: TMenuItem;
    VersionMacros1: TMenuItem;
    VERPRODUCTVERSIONMacro1: TMenuItem;
    VERFILEVERSION1: TMenuItem;
    VERMAJORMacro1: TMenuItem;
    VERMINORMacro1: TMenuItem;
    VERRELEASEMacro1: TMenuItem;
    VERBUILD1: TMenuItem;
    VERPRODUCTNAME1: TMenuItem;
    VERINTERNALNAME1: TMenuItem;
    VERFILEDESCRIPTIONMacro1: TMenuItem;
    N7: TMenuItem;
    actInsertCursorPos: TAction;
    actInsertUnitStart: TAction;
    actInsertLineStart: TAction;
    actInsertLineEnd: TAction;
    N8: TMenuItem;
    CLIPBOARDMacro1: TMenuItem;
    SELECTIONMacro1: TMenuItem;
    N5: TMenuItem;
    grpUserDetails: TGroupBox;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actImport: TAction;
    actExport: TAction;
    actClear: TAction;
    lblFullName: TLabel;
    lblInitials: TLabel;
    btnHelp: TButton;
    Procedurefunctionheader1: TMenuItem;
    ARGUMENTSMacro1: TMenuItem;
    RESULTMacro1: TMenuItem;
    N9: TMenuItem;
    BEGINPARAMLISTMacro1: TMenuItem;
    ENDPARAMLISTMacro1: TMenuItem;
    N10: TMenuItem;
    PARAMNAMEMacro1: TMenuItem;
    PARAMTYPEMacro1: TMenuItem;
    PARAMDEFMacro1: TMenuItem;
    N11: TMenuItem;
    CLIPBOARD1Macro1: TMenuItem;
    COPYSELECTIONMacro1: TMenuItem;
    gbxExpansion: TGroupBox;
    lblExpandDelay: TLabel;
    lb01Sec: TLabel;
    lbl2Sec: TLabel;
    cbExpandWithChar: TCheckBox;
    tbExpandDelay: TTrackBar;
    lbl1Sec: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure TemplateCodeEnter(Sender: TObject);
    procedure TemplateCodeExit(Sender: TObject);
    procedure InsertMacroClick(Sender: TObject);
    procedure INPUTVARMacro1Click(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmUsesPopup(Sender: TObject);
    procedure miAddToUsesClick(Sender: TObject);
    procedure miDeleteUsesClick(Sender: TObject);
    procedure lvGlobalUsesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lvGlobalUsesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvGlobalUsesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvGlobalUsesInsert(Sender: TObject; Item: TListItem);
    procedure lvTemplatesClick(Sender: TObject);
    procedure edProgrammerNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvTemplatesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnChangeClick(Sender: TObject);
    procedure SetHighlighterClick(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure pmMacrosPopup(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure pnlUsesResize(Sender: TObject);
    procedure lvTemplatesDblClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    FSettings: TTemplateSettings;
    FMacroFile: TMacroFile;
    FCurrentSyntaxMode: TGXSyntaxHighlighter;
    FTemplateText: TGXEnhancedEditor;
    FTemplateShortCut: TShortCut;
    FModified: Boolean;
    FTextModified: Boolean;
    procedure UpdateMacroValues(AMacroIndex: Integer);
    procedure ReadMacroValues(AMacroIndex: Integer);
    procedure ClearMacro;
    procedure ClearAllMacros;
    procedure ChangeMacro;
    procedure ClearControls;
    procedure FillControls;
    procedure ImportMacros(ASourceFile: TMacroFile; AAppend: Boolean);
    function CreateMacroObject: TMacroObject;
    procedure ChangeSeqValue;
    procedure SetupTemplateMemo;
    procedure SetEditing(AFlag: Boolean);
    function  GetInsertPos: TTemplateInsertPos;
    procedure SetInsertPos(APos: TTemplateInsertPos);
    procedure SaveFormLayout;
    procedure LoadFormLayout;
    procedure SetCurrentSyntaxMode(const Value: TGXSyntaxHighlighter);
    procedure SetHighlighter(AIndex: Integer);
    procedure UpdateSyntaxMenus;
    function IsEmptySelection: Boolean;
    procedure SelectMacro(Index: Integer);
    procedure MarkModified;
    procedure ClearModified;
    procedure MarkTextModified;
  protected
    procedure SaveSettings;
    procedure LoadSettings;
    procedure SaveMacroFile(const AFilename: string);
    procedure LoadMacroFile(const AFilename: string; AAppend: Boolean);
    procedure AddUses(AUnits: TStringList; AListView: TListView);
    procedure LoadUsesToMacroObject(AUnits: TStringList; AListView: TListView);
    procedure SaveTemplateChanges;
    procedure AddMacroToControls(AMacroObject: TMacroObject);
    procedure UpdateMacroListValues(AMacroIndex: Integer; AMacroObject: TMacroObject);
    procedure DeleteMacro(AMacroIndex: Integer);
    function SelectedIndex: Integer;
    procedure SetSettings(const AValue: TTemplateSettings);
    class function ConfigurationKey: string;
    procedure TemplateTextChanged(Sender: TObject);
  public
    property Settings: TTemplateSettings read FSettings write SetSettings;
    property CurrentSyntaxMode: TGXSyntaxHighlighter read FCurrentSyntaxMode write SetCurrentSyntaxMode;
  end;

TGetProgrammerInfo = function(var VInfo: TProgrammerInfo): Boolean of object;

procedure RegisterProgrammerInfoProc(AProc: TGetProgrammerInfo);
function GetProgrammerInfo(var VInfo: TProgrammerInfo): Boolean;
function GetInitials(const AFullName: string): string;
function GenProgrammerSeq: Integer;
function GenSeqNewValue(const ASeqName: string): Integer;
function MacroTemplatesBaseKey: string;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Graphics, Clipbrd, GX_SharedImages,
  GX_GxUtils, GX_MacroParser, GX_MacroTemplateEdit, GX_OtaUtils, GX_IdeUtils;

var
  ProgInfoProc: TGetProgrammerInfo;
  InternalMacroLibrary: TBasicMacroLibrary = nil;

const
  SequenceConfigurationKey = 'Sequences';

resourcestring
  SConfirmClear = 'Are you sure you want to delete all macros ?';

type
  TProgrammerMacroLibrary = class(TBasicMacroLibrary)
  public
    function GetUserNameTag: string;
    function ParseMacro(AMacroText: string; var VResult: string): Boolean; override;
  end;

procedure RegisterProgrammerInfoProc(AProc: TGetProgrammerInfo);
begin
  ProgInfoProc := AProc;
end;

// Returns True if the programmer information was accessible and VInfo is filled
function GetProgrammerInfo(var VInfo: TProgrammerInfo): Boolean;
begin
  if @ProgInfoProc <> nil then
    Result := ProgInfoProc(VInfo)
  else
    Result := False;
end;

// Returns the initials of a full name
function GetInitials(const AFullName: string): string;
var
  WorkCopy, NamePart: string;
begin
  Result := '';
  WorkCopy := AFullName;
  repeat
    NamePart := RemoveCharsBefore(WorkCopy, ' ');
    if NamePart <> '' then
      Result := Result + NamePart[1];
  until WorkCopy = '';
end;

// Returns name of the registry key for a programmer's sequence number
function GetProgrammerSeqKey: string;
var
  PrgInfo: TProgrammerInfo;
begin
  if GetProgrammerInfo(PrgInfo) then
    Result := 'PGEN_' + PrgInfo.Initials
  else
    Result := '';
end;

// Gets a programmer's sequence number
function ReadSeqValue(const ASeqName: string): Integer;
begin
  with TMacroTemplatesIni.Create do
  try
    Result := ReadInteger(SequenceConfigurationKey, ASeqName, 0);
  finally
    Free;
  end;
end;

function ReadProgrammerSeqValue: Integer;
begin
  Result := ReadSeqValue(GetProgrammerSeqKey);
end;

// Returns the sequence value for the current programmer
function GenProgrammerSeq: Integer;
var
  PrgInfo: TProgrammerInfo;
begin
  if GetProgrammerInfo(PrgInfo) then
    Result := GenSeqNewValue('PGEN_' + PrgInfo.Initials)
  else
    Result := -1;
end;

// Generate new sequence number
function GenSeqNewValue(const ASeqName: string): Integer;
begin
  with TMacroTemplatesIni.Create do
  try
    Result := ReadInteger(SequenceConfigurationKey, ASeqName, 0);
    Inc(Result);
    WriteInteger(SequenceConfigurationKey, ASeqName, Result);
  finally
    Free;
  end;
end;

procedure SetSeqValue(const ASeqName: string; ANewValue: Integer);
begin
  with TMacroTemplatesIni.Create do
  try
    WriteInteger(SequenceConfigurationKey, ASeqName, ANewValue);
  finally
    Free;
  end;
end;

function MacroTemplatesBaseKey: string;
begin
  Result := ConfigInfo.GExpertsIdeRootRegistryKey +
    PathDelim + 'EditorExperts' + PathDelim + 'MacroTemplates';
end;

{ TTemplateSettings }

constructor TTemplateSettings.Create(const AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TTemplateSettings.Reload;
begin // empty - non-required method
end;

procedure TTemplateSettings.SetProgrammerName(const AValue: string);
begin
  if AValue <> FProgrammerName then
  begin
    FProgrammerName := AValue;
    Changed;
  end;
end;

procedure TTemplateSettings.SetProgrammerInitials(const AValue: string);
begin
  if AValue <> FProgrammerInitials then
  begin
    FProgrammerInitials := AValue;
    Changed;
  end;
end;

procedure TTemplateSettings.Changed;
begin // Empty (abstract not supported in BCB ?)
end;

procedure TTemplateSettings.LoadSettings;
begin // Empty (abstract not supported in BCB ?)
end;

procedure TTemplateSettings.SaveSettings;
begin // Empty (abstract not supported in BCB ?)
end;

procedure TTemplateSettings.SetExpandWithChar(const Value: Boolean);
begin
  if FExpandWithChar <> Value then
  begin
    FExpandWithChar := Value;
    Changed;
  end;
end;

procedure TTemplateSettings.SetExpandDelay(const Value: Integer);
begin
  if FExpandDelay <> Value then
  begin
    FExpandDelay := Value;
    Changed;
  end;
end;

{ TfmMacroTemplates }

procedure TfmMacroTemplates.FormCreate(Sender: TObject);
begin
  SetupTemplateMemo;
  pnlMacroText.Caption := ''; // hide design-time text
  PageControl.ActivePage := PageControl.Pages[0];
  LoadSettings;
  pmMacros.AutoHotKeys := maManual;
  pnlUsesResize(pnlUses);
end;

procedure TfmMacroTemplates.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMacroFile);
  inherited;
end;

procedure TfmMacroTemplates.AddMacroToControls(AMacroObject: TMacroObject);
var
  ListItem: TListItem;
begin
  ListItem := lvTemplates.Items.Add;
  ListItem.Caption := AMacroObject.Name;
  ListItem.SubItems.Add(AMacroObject.Desc);
  ListItem.SubItems.Add(ShortCutToText(AMacroObject.ShortCut));
  ListItem.SubItems.Add(InsertPosToText(AMacroObject.InsertPos));
  ListItem.Data := AMacroObject;
end;

procedure TfmMacroTemplates.UpdateMacroListValues(AMacroIndex: Integer; AMacroObject: TMacroObject);
var
  ListItem: TListItem;
begin
  ListItem := lvTemplates.Items[AMacroIndex];
  ListItem.Caption := AMacroObject.Name;
  ListItem.SubItems[0] := AMacroObject.Desc;
  ListItem.SubItems[1] := ShortCutToText(AMacroObject.ShortCut);
  ListItem.SubItems[2] := InsertPosToText(AMacroObject.InsertPos);
end;

procedure TfmMacroTemplates.DeleteMacro(AMacroIndex: Integer);
begin
  lvTemplates.Items[AMacroIndex].Delete;
end;

function TfmMacroTemplates.CreateMacroObject: TMacroObject;
begin
  Result := TMacroObject.Create('');
end;

function TfmMacroTemplates.SelectedIndex: Integer;
begin
  Result := -1;
  if lvTemplates.Selected <> nil then
    Result := lvTemplates.Selected.Index;
end;

procedure TfmMacroTemplates.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
  Close;
end;

procedure TfmMacroTemplates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveFormLayout;
  Action := caFree;
  if Assigned(FSettings) then
    FSettings.Form := nil;
end;

// Populates the controls with a macro's values
procedure TfmMacroTemplates.ReadMacroValues(AMacroIndex: Integer);
var
  MacroObject: TMacroObject;
begin
  if AMacroIndex < 0 then
    Exit;
  MacroObject := TMacroObject(lvTemplates.Items[AMacroIndex].Data);
  if MacroObject <> nil then
  begin
    FTemplateText.AsString := MacroObject.Text;
    AddUses(MacroObject.PubUnits, lvGlobalUses);
    AddUses(MacroObject.PrivUnits, lvLocalUses);
    SetEditing(True);
    SetInsertPos(MacroObject.InsertPos);
    FTemplateShortCut := MacroObject.ShortCut;
    ClearModified;
  end;
end;

// Update a macro's data with values from controls
procedure TfmMacroTemplates.UpdateMacroValues(AMacroIndex: Integer);
var
  MacroObject: TMacroObject;
  MacroText: string;
begin
  if AMacroIndex < 0 then
    Exit;
  MacroObject := TMacroObject(lvTemplates.Items[AMacroIndex].Data);
  if MacroObject <> nil then
  begin
    MacroText := FTemplateText.Text;
    MacroObject.Text := MacroText;
    LoadUsesToMacroObject(MacroObject.PubUnits, lvGlobalUses);
    LoadUsesToMacroObject(MacroObject.PrivUnits, lvLocalUses);
    MacroObject.InsertPos := GetInsertPos;
    MacroObject.ShortCut := FTemplateShortCut;
  end;
end;

// The selected macro changed
procedure TfmMacroTemplates.ChangeMacro;
var
  Index: Integer;
begin
  Index := SelectedIndex;
  if Index > -1 then
    ReadMacroValues(Index);
end;

procedure TfmMacroTemplates.lvTemplatesClick(Sender: TObject);
begin
  ChangeMacro;
end;

procedure TfmMacroTemplates.TemplateCodeEnter(Sender: TObject);
begin
  btnOK.Default := False;
end;

procedure TfmMacroTemplates.TemplateCodeExit(Sender: TObject);
begin
  SaveTemplateChanges;
  btnOK.Default := True;
end;

procedure TfmMacroTemplates.SaveTemplateChanges;
var
  Index: Integer;
  MacroObject: TMacroObject;
begin
  if not FModified then
    Exit;
  Index := SelectedIndex;
  if Index > -1 then
  begin
    UpdateMacroValues(Index);

    MacroObject := TMacroObject(lvTemplates.Items[Index].Data);
    if MacroObject = nil then
      Exit;
    UpdateMacroListValues(Index, MacroObject);
    ClearModified;
  end;
end;

// Display the list of macros
procedure TfmMacroTemplates.FillControls;
var
  i: Integer;
begin
  ClearControls;
  for i := 0 to FMacroFile.MacroCount - 1 do
    AddMacroToControls(FMacroFile.MacroItems[i]);
  SelectMacro(0);
end;

procedure TfmMacroTemplates.ClearAllMacros;
begin
  if FMacroFile <> nil then
    FMacroFile.Clear;
  ClearControls;
end;

// Unselect the template and clear the controls
// No template is selected after this operation
procedure TfmMacroTemplates.ClearMacro;
begin
  lvTemplates.Selected := nil;
  FTemplateText.Clear;
  FTemplateShortCut := EmptyShortCut;
  lvGlobalUses.Items.Clear;
  lvLocalUses.Items.Clear;
  SetEditing(False);
end;

// Clear the data in the macro controls
procedure TfmMacroTemplates.ClearControls;
var
  Idx: Integer;
begin
  Idx := SelectedIndex;
  lvTemplates.Items.Clear;
  if Idx > 0 then
    lvTemplates.Selected := nil;
  FTemplateText.Clear;
  SetEditing(False);
end;

procedure TfmMacroTemplates.LoadFormLayout;
begin
  with TGExpertsSettings.Create(MacroTemplatesBaseKey) do
  try
    LoadForm(Self, ConfigurationKey);
    pnlList.Height := ReadInteger(ConfigurationKey, 'ListSplitter', pnlList.Height);
    pnlUses.Width := ReadInteger(ConfigurationKey, 'UsesSplitter', pnlUses.Width);
    pnlUsesImplementation.Height := ReadInteger(ConfigurationKey, 'UsesSecSplitter', pnlUsesImplementation.Height);
    lvTemplates.Columns[0].Width := ReadInteger(ConfigurationKey, 'NameWidth', lvTemplates.Columns[0].Width);
    lvTemplates.Columns[1].Width := ReadInteger(ConfigurationKey, 'DescriptionWidth', lvTemplates.Columns[1].Width);
    lvTemplates.Columns[2].Width := ReadInteger(ConfigurationKey, 'ShortCutWidth', lvTemplates.Columns[2].Width);
    CurrentSyntaxMode := TGXSyntaxHighlighter(ReadEnumerated(ConfigurationKey, 'SyntaxHighlighter', TypeInfo(TGXSyntaxHighlighter), Ord(FCurrentSyntaxMode)));
  finally
    Free;
  end;
end;

procedure TfmMacroTemplates.SaveFormLayout;
begin
  with TGExpertsSettings.Create(MacroTemplatesBaseKey) do
  try
    if WindowState = wsNormal then // Save only if not maximized/minimized
      SaveForm(Self, ConfigurationKey);
    WriteInteger(ConfigurationKey, 'ListSplitter', pnlList.Height);
    WriteInteger(ConfigurationKey, 'UsesSplitter', pnlUses.Width);
    WriteInteger(ConfigurationKey, 'UsesSecSplitter', pnlUsesImplementation.Height);
    WriteInteger(ConfigurationKey, 'NameWidth', lvTemplates.Columns[0].Width);
    WriteInteger(ConfigurationKey, 'DescriptionWidth', lvTemplates.Columns[1].Width);
    WriteInteger(ConfigurationKey, 'ShortCutWidth', lvTemplates.Columns[2].Width);
    WriteInteger(ConfigurationKey, 'SyntaxHighlighter', Ord(FCurrentSyntaxMode));
  finally
    Free;
  end;
end;

procedure TfmMacroTemplates.LoadSettings;
begin
  LoadFormLayout;
  ClearAllMacros;
  if Settings <> nil then
  begin
    if FileExists(Settings.MacroFileName) then
      LoadMacroFile(Settings.MacroFileName, False);
    Settings.LoadSettings;
    edProgrammerName.Text := Settings.ProgrammerName;
    edInitials.Text := Settings.ProgrammerInitials;
    edGenValue.Text := IntToStr(ReadProgrammerSeqValue);
    cbExpandWithChar.Checked := Settings.ExpandWithChar;

    if Settings.ExpandDelay >= 0 then
      tbExpandDelay.Position := Settings.ExpandDelay
    else
      tbExpandDelay.Position := DefExpandDelay div ExpandDelayNumerator;
  end;
end;

procedure TfmMacroTemplates.SaveSettings;
begin
  if Settings <> nil then
  begin
    SaveMacroFile(Settings.MacroFileName);
    SetSeqValue(GetProgrammerSeqKey, StrToInt(edGenValue.Text));

    Settings.ProgrammerName := edProgrammerName.Text;
    Settings.ProgrammerInitials := edInitials.Text;

    Settings.ExpandWithChar := cbExpandWithChar.Checked;
    Settings.ExpandDelay := tbExpandDelay.Position;

    Settings.SaveSettings;
    Settings.Reload;
  end;
end;

procedure TfmMacroTemplates.LoadMacroFile(const AFilename: string; AAppend: Boolean);
var
  ImportFile: TMacroFile;
begin
  if FileExists(AFilename) then
  begin
    ImportFile := TMacroFile.Create;
    try
      ImportFile.FileName := AFilename;
      ImportFile.LoadFromFile;
      ImportMacros(ImportFile, AAppend);
      FillControls;
    finally
      FreeAndNil(ImportFile);
    end;
  end
  else
    MessageDlg('Unable to load macro templates file from:' + sLineBreak + AFileName, mtError, [mbOk], 0);
end;

// Add macros from the file to the internal list of macros
procedure TfmMacroTemplates.ImportMacros(ASourceFile: TMacroFile; AAppend: Boolean);

  // Add a new macro to current list (with copy)
  procedure AddMacroToFile(ASourceObj: TMacroObject);
  begin
    FMacroFile.AddMacro(ASourceObj);
  end;

var
  AskOverwrite: Boolean;
  i: Integer;
  ImportName: string;
  Idx: Integer;
  FoundMacro: TMacroObject;
  DialogResult: Integer;
begin
  if not AAppend then
    FMacroFile.Clear;

  AskOverwrite := True;
  for i := 0 to ASourceFile.MacroCount - 1 do
  begin
    ImportName := ASourceFile[i].Name;
    Idx := FMacroFile.IndexOf(ImportName);
    if Idx >= 0 then
    begin
      FoundMacro := FMacroFile[Idx];
      if AskOverwrite then
        DialogResult := MessageDlg('A macro named "' + ImportName +
          '" already exists in list of macros.' + sLineBreak +
          'Do you want to replace it ?', mtConfirmation,
          [mbYes, mbYesToAll, mbIgnore, mbAbort], 0)
      else
        DialogResult := mrYes;

      case DialogResult of
        mrYes, mrYesToAll:
          begin
            FMacroFile.RemoveMacro(FoundMacro);
            if DialogResult = mrYesToAll then
              AskOverwrite := False;
            AddMacroToFile(ASourceFile[i]);
          end;
        mrIgnore: ; // do nothing
        mrAbort: Break;
      end; // case
    end
    else // not found
      AddMacroToFile(ASourceFile[i]);
  end; // for i
end; // ImportMacros

procedure TfmMacroTemplates.SetEditing(AFlag: Boolean);
begin
  FTemplateText.ReadOnly := not AFlag;
end;

procedure TfmMacroTemplates.SetInsertPos(APos: TTemplateInsertPos);
begin
  actInsertCursorPos.Checked := (APos = tipCursorPos);
  actInsertUnitStart.Checked := (APos = tipUnitStart);
  actInsertLineStart.Checked := (APos = tipLineStart);
  actInsertLineEnd.Checked   := (APos = tipLineEnd);
end;

function TfmMacroTemplates.GetInsertPos: TTemplateInsertPos;
begin
  Result := tipCursorPos;
  if actInsertUnitStart.Checked then
    Result := tipUnitStart
  else if actInsertLineStart.Checked then
    Result := tipLineStart
  else if actInsertLineEnd.Checked then
    Result := tipLineEnd;
end;

procedure TfmMacroTemplates.InsertMacroClick(Sender: TObject);
var
  MacroName: string;
begin
  if FTemplateText.ReadOnly then
    Exit;
  MacroName := (Sender as TMenuItem).Caption;
  MacroName := StringReplace(MacroName, '&', '', [rfReplaceAll]);
  MacroName := Copy(MacroName, 1, Pos(' ', MacroName) - 1);
  MacroName := '%' + MacroName + '%';
  FTemplateText.SelText := MacroName;
end;

procedure TfmMacroTemplates.INPUTVARMacro1Click(Sender: TObject);
var
  VarCaption, MacroText, VarName: string;
begin
  if FTemplateText.ReadOnly then
    Exit;
  VarCaption := InputBox('Variable Prompt Message', 'Variable Prompt Message', 'Variable Value');
  if VarCaption = '' then
    Exit;

  VarName := InputBox('Variable Name', 'Variable Name', '');
  if VarName = '' then
    Exit;

  MacroText := '%' + InputVarName + ',' + AnsiUpperCase(VarName) + ',' + VarCaption + '%';
  FTemplateText.SelText := MacroText;
end;

procedure TfmMacroTemplates.miCopyClick(Sender: TObject);
begin
  FTemplateText.CopyToClipboard;
end;

procedure TfmMacroTemplates.miCutClick(Sender: TObject);
begin
  FTemplateText.CutToClipboard;
end;

procedure TfmMacroTemplates.miPasteClick(Sender: TObject);
begin
  FTemplateText.PasteFromClipboard;
end;

procedure TfmMacroTemplates.FormShow(Sender: TObject);
begin
  // Loading executed here, because file name is a property (not added to Create)
  if FMacroFile = nil then
    FMacroFile := TMacroFile.Create;
  LoadSettings;
end;

procedure TfmMacroTemplates.pmUsesPopup(Sender: TObject);
var
  DeleteEnabled, TemplateReadOnly: Boolean;
begin
  DeleteEnabled := Assigned((pmUses.PopupComponent as TListView).Selected);
  TemplateReadOnly := FTemplateText.ReadOnly;

  miDeleteUses.Enabled := DeleteEnabled and (not TemplateReadOnly);
  miAddToUses.Enabled := not TemplateReadOnly;

  if (pmUses.PopupComponent as TListView) = lvGlobalUses then
    miAddToUses.Caption := 'Add to Interface'
  else if (pmUses.PopupComponent as TListView) = lvLocalUses then
    miAddToUses.Caption := 'Add to Implementation';
end;

procedure TfmMacroTemplates.miAddToUsesClick(Sender: TObject);
var
  ListView: TListView;
  UnitName: string;
  ListItem: TListItem;
begin
  ListView := (pmUses.PopupComponent as TListView);

  UnitName := InputBox('Unit Name', 'Unit Name', '');
  if UnitName <> '' then
  begin
    ListItem := ListView.Items.Add;
    ListItem.Caption := UnitName;
    MarkModified;
  end;
end;

procedure TfmMacroTemplates.miDeleteUsesClick(Sender: TObject);
var
  ListView: TListView;
begin
  ListView := (pmUses.PopupComponent as TListView);
  if ListView.Selected <> nil then
  begin
    ListView.Selected.Delete;
    MarkModified;
  end;
end;

procedure TfmMacroTemplates.lvGlobalUsesDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TListView;
end;

procedure TfmMacroTemplates.lvGlobalUsesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceList, DestList: TListView;
  ListItem: TListItem;
  MovedItem: TListItem;
begin
  if (Sender is TListView) and (Source is TListView) then
  begin
    SourceList := Source as TListView;
    DestList := Sender as TListView;
    if SourceList.Selected = nil then
      Exit;
    MovedItem := SourceList.Selected;

    ListItem := DestList.Items.Add;
    ListItem.Caption := MovedItem.Caption;

    MovedItem.Delete;
  end;
end;

// Copy uses list from AUnits to AListView
procedure TfmMacroTemplates.AddUses(AUnits: TStringList; AListView: TListView);
var
  ListItem: TListItem;
  UnitName: string;
  t: Integer;
begin
  AListView.Items.Clear;
  for t := 0 to AUnits.Count - 1 do
  begin
    UnitName := AUnits.Strings[t];
    ListItem := AListView.Items.Add;
    ListItem.Caption := UnitName;
  end;
end;

// Copy uses list from from AListsView to AUnits
procedure TfmMacroTemplates.LoadUsesToMacroObject(AUnits: TStringList; AListView: TListView);
var
  ListItem: TListItem;
  t: Integer;
begin
  AUnits.Clear;
  for t := 0 to AListView.Items.Count - 1 do
  begin
    ListItem := AListView.Items[t];
    AUnits.Add(ListItem.Caption);
  end;
end;

procedure TfmMacroTemplates.lvGlobalUsesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  SaveTemplateChanges;
end;

procedure TfmMacroTemplates.lvGlobalUsesInsert(Sender: TObject; Item: TListItem);
begin
  SaveTemplateChanges;
end;

procedure TfmMacroTemplates.SaveMacroFile(const AFilename: string);
begin
  FMacroFile.SaveToFile(AFilename);
end;

procedure TfmMacroTemplates.SetSettings(const AValue: TTemplateSettings);
begin
  if FSettings <> AValue then
  begin
    if Assigned(FSettings) and (FSettings.Form = Self) then
      FSettings.Form := nil;
    FSettings := AValue;
    if Assigned(FSettings) then
      FSettings.Form := Self;
  end;
end;

procedure TfmMacroTemplates.edProgrammerNameChange(Sender: TObject);
begin
  edInitials.Text := GetInitials(edProgrammerName.Text);
end;

procedure TfmMacroTemplates.lvTemplatesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ChangeMacro;
end;

procedure TfmMacroTemplates.btnChangeClick(Sender: TObject);
begin
  ChangeSeqValue;
end;

// Changes the value of the sequence number
procedure TfmMacroTemplates.ChangeSeqValue;
var
  SeqValue: string;
  SeqValueInt: Integer;
begin
  SeqValue := IntToStr(ReadProgrammerSeqValue);
  SeqValue := InputBox('New Sequence Value', 'Value', SeqValue);
  SeqValueInt := StrToInt(SeqValue);
  edGenValue.Text := IntToStr(SeqValueInt);
end;

procedure TfmMacroTemplates.SetupTemplateMemo;
begin
  if RunningCPPBuilder or GxOtaCurrentProjectIsNativeCpp then
    FCurrentSyntaxMode := gxpCPP
  else
    FCurrentSyntaxMode := gxpPas;

  FTemplateText := TGXEnhancedEditor.Create(Self);
  FTemplateText.Highlighter := FCurrentSyntaxMode;
  FTemplateText.Align := alClient;
  FTemplateText.PopupMenu := pmMacros;
  FTemplateText.OnExit := TemplateCodeExit;
  FTemplateText.OnEnter := TemplateCodeEnter;
  FTemplateText.Parent := pnlMacroText;
  FTemplateText.ReadOnly := True;
  FTemplateText.Font.Height := -11;
  FTemplateText.Font.Name := 'Courier New';
  FTemplateText.WantTabs := True;
  FTemplateText.OnChange := TemplateTextChanged;
end;

procedure TfmMacroTemplates.SetHighlighterClick(Sender: TObject);
begin
  SetHighlighter((Sender as TComponent).Tag);
end;

procedure TfmMacroTemplates.SetHighlighter(AIndex: Integer);
const
  HIGHLIGHTERS: array[0..4] of TGXSyntaxHighlighter =
  (gxpNone, gxpPAS, gxpCPP, gxpHTML, gxpSQL);
begin
  CurrentSyntaxMode := HIGHLIGHTERS[AIndex];
end;

procedure TfmMacroTemplates.UpdateSyntaxMenus;
begin
  actSyntaxNone.Checked := (FCurrentSyntaxMode = gxpNone);
  actSyntaxPas.Checked := (FCurrentSyntaxMode = gxpPAS);
  actSyntaxCpp.Checked := (FCurrentSyntaxMode = gxpCPP);
  actSyntaxHtml.Checked := (FCurrentSyntaxMode = gxpHTML);
  actSyntaxSql.Checked := (FCurrentSyntaxMode = gxpSQL);
end;

procedure TfmMacroTemplates.SetCurrentSyntaxMode(const Value: TGXSyntaxHighlighter);
begin
  if FCurrentSyntaxMode <> Value then
  begin
    FCurrentSyntaxMode := Value;
    if Assigned(FTemplateText) then
      FTemplateText.Highlighter := FCurrentSyntaxMode;
  end;
end;

procedure TfmMacroTemplates.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  MacroSelected: Boolean;
  FileEmpty: Boolean;
  TextSelected: Boolean;
begin
  TextSelected := Length(FTemplateText.SelText) > 0;
  MacroSelected := (SelectedIndex > -1);
  FileEmpty := (Assigned(FMacroFile) and (FMacroFile.MacroCount > 0));

  actEditCut.Enabled := TextSelected;
  actEditCopy.Enabled := TextSelected;
  actEditPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and (not FTemplateText.ReadOnly));
  UpdateSyntaxMenus;
  actEdit.Enabled := MacroSelected;
  actDelete.Enabled := MacroSelected;
  actClear.Enabled := FileEmpty;
  actExport.Enabled := FileEmpty;
  Handled := True;
end;

// Returns True if no macro is selected
function TfmMacroTemplates.IsEmptySelection: Boolean;
begin
  Result := FTemplateText.ReadOnly;
end;

procedure TfmMacroTemplates.pmMacrosPopup(Sender: TObject);
begin
  if IsEmptySelection then
    Abort;
end;

procedure TfmMacroTemplates.actInsertExecute(Sender: TObject);
begin
  SetInsertPos(TTemplateInsertPos((Sender as TComponent).Tag));
  SaveTemplateChanges;
end;

procedure TfmMacroTemplates.SelectMacro(Index: Integer);
begin
  if Index <= (lvTemplates.Items.Count - 1) then
  begin
    lvTemplates.Selected := lvTemplates.Items[Index];
    lvTemplates.Selected.MakeVisible(False);
    lvTemplates.ItemFocused := lvTemplates.Selected;
  end;
end;

class function TfmMacroTemplates.ConfigurationKey: string;
begin
  Result := 'MacroTemplates';
end;

procedure TfmMacroTemplates.MarkModified;
begin
  FModified := True;
end;

procedure TfmMacroTemplates.TemplateTextChanged(Sender: TObject);
begin
  MarkTextModified;
end;

procedure TfmMacroTemplates.ClearModified;
begin
  FModified := False;
  FTextModified := False;
end;

procedure TfmMacroTemplates.MarkTextModified;
begin
  FTextModified := True;
  FModified := True;
end;

{ TMacroTemplatesIni }

constructor TMacroTemplatesIni.Create;
begin
  FBaseKey := ConfigInfo.GExpertsIdeRootRegistryKey + PathDelim +
    'EditorExperts' + PathDelim + TfmMacroTemplates.ConfigurationKey;
  inherited Create(FBaseKey);
end;

{ TProgrammerMacroLibrary }

function TProgrammerMacroLibrary.GetUserNameTag: string;
begin
  Result := GetCurrentUser;
  if Trim(Result) = '' then
    Result := GetUnknownNameResult;
end;

function TProgrammerMacroLibrary.ParseMacro(AMacroText: string; var VResult: string): Boolean;
var
  UpMacroText: string;
  PrgInfo: TProgrammerInfo;
begin
  Result := True;
  UpMacroText := AnsiUpperCase(AMacroText);
  if UpMacroText = '%PROGRAMMERNAME%' then
  begin
    if GetProgrammerInfo(PrgInfo) then
      VResult := PrgInfo.FullName
    else
      VResult := GetUserNameTag;
  end
  else if UpMacroText = '%PROGRAMMERINITIALS%' then
  begin
    if GetProgrammerInfo(PrgInfo) then
      VResult := PrgInfo.Initials
    else
      VResult := GetInitials(GetUserNameTag);
  end
  else if UpMacroText = '%PGEN%' then
    VResult := IntToStr(GenProgrammerSeq)
  else
    Result := False;
end;

procedure TfmMacroTemplates.pnlUsesResize(Sender: TObject);
begin
  pnlUsesImplementation.Height := Max(Trunc(pnlUses.ClientHeight / 2) - 4, 0);
end;

procedure TfmMacroTemplates.lvTemplatesDblClick(Sender: TObject);
begin
  actEdit.Execute;
end;

procedure TfmMacroTemplates.actAddExecute(Sender: TObject);
var
  NewMacro, RealMacroObject: TMacroObject;
  MacroFound: Boolean;
begin
  NewMacro := CreateMacroObject;
  try
    MacroFound := False;
    repeat
      if not EditMacroObject(NewMacro) then
        Break;
      MacroFound := (FMacroFile.IndexOf(NewMacro.Name) > -1);
      if MacroFound then
        MessageDlg('Macro names must be unique', mtWarning, [mbOK], 0)
      else
      begin
        RealMacroObject := FMacroFile.AddMacro(NewMacro);
        AddMacroToControls(RealMacroObject);
        lvTemplates.Selected := lvTemplates.FindCaption(0, NewMacro.Name, False, True, False);
        FTemplateText.Text := '';
        FTemplateText.SetFocus;
        SetEditing(True);
        Exit;
      end;
    until not MacroFound;
  finally
    FreeAndNil(NewMacro);
  end;
end;

procedure TfmMacroTemplates.actEditExecute(Sender: TObject);
var
  MacroObject: TMacroObject;
  MacroIdx: Integer;
begin
  MacroIdx := SelectedIndex;
  if MacroIdx < 0 then
    Exit;
  MacroObject := TMacroObject(lvTemplates.Items[MacroIdx].Data);
  if MacroObject = nil then
    Exit;
  if EditMacroObject(MacroObject) then begin
    UpdateMacroListValues(MacroIdx, MacroObject);
    MarkModified;
  end;
end;

procedure TfmMacroTemplates.actDeleteExecute(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := SelectedIndex;
  if Idx > -1 then
  begin
    FMacroFile.RemoveMacro(TMacroObject(lvTemplates.Items[Idx].Data));
    DeleteMacro(Idx);
    if Idx > 0 then
    begin
      lvTemplates.Selected := lvTemplates.Items[Idx - 1];
      lvTemplates.Selected.MakeVisible(False);
    end
    else
    begin
      lvTemplates.Selected := nil;
      ClearMacro;
    end;
    lvTemplates.ItemFocused := lvTemplates.Selected;
  end;
end;

procedure TfmMacroTemplates.actImportExecute(Sender: TObject);
var
  ImportFileName: string;
begin
  OpenDialog.FileName := DefaultMacroFileName;
  if OpenDialog.Execute then
  begin
    ImportFileName := OpenDialog.FileName;
    LoadMacroFile(ImportFileName, True);
  end;
end;

procedure TfmMacroTemplates.actExportExecute(Sender: TObject);
var
  SaveFileName: string;
begin
  SaveDialog.FileName := DefaultMacroFileName;
  if SaveDialog.Execute then
  begin
    SaveFileName := SaveDialog.FileName;
    SaveMacroFile(SaveFileName);
  end;
end;

procedure TfmMacroTemplates.actClearExecute(Sender: TObject);
begin
  if MessageDlg(SConfirmClear, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ClearAllMacros;
end;

procedure TfmMacroTemplates.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 40);
end;

initialization
  InternalMacroLibrary := TProgrammerMacroLibrary.Create;
  RegisterMacroLibrary(InternalMacroLibrary);

finalization
  UnregisterMacroLibrary(InternalMacroLibrary);
  FreeAndNil(InternalMacroLibrary);

end.

