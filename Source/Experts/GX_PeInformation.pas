unit GX_PeInformation;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GX_PeInfo, ComCtrls, Menus,
  GX_IdeDock, DropTarget, DropSource, ActnList, ToolWin;

type
  TfmPeInformation = class(TfmIdeDockForm)
    dlgOpen: TOpenDialog;
    pcMain: TPageControl;
    tshMSDOS: TTabSheet;
    tshImport: TTabSheet;
    tshExports: TTabSheet;
    tshPEHEader: TTabSheet;
    lvMSDOS: TListView;
    lvPEHeader: TListView;
    lvImports: TListView;
    splImport: TSplitter;
    lvImportFunctions: TListView;
    lvExportFunctions: TListView;
    tshPEOptional: TTabSheet;
    lvPEOptionalHeader: TListView;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileOpen: TMenuItem;
    mitFileSep2: TMenuItem;
    mitFileExit: TMenuItem;
    mitOptions: TMenuItem;
    mitOptionsDecimal: TMenuItem;
    mitOptionsHex: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitFilePrint: TMenuItem;
    mitFileSep1: TMenuItem;
    mitFilePrinterSetup: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    ToolBar: TToolBar;
    tbnOpen: TToolButton;
    tbnPrint: TToolButton;
    tbnCopy: TToolButton;
    tbnHelp: TToolButton;
    Actions: TActionList;
    actFileOpen: TAction;
    actFilePrinterSetup: TAction;
    actFilePrint: TAction;
    actFileExit: TAction;
    actOptionsDecimal: TAction;
    actOptionsHex: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    mitHelpContents: TMenuItem;
    actEditCopy: TAction;
    mitEdit: TMenuItem;
    mitEditCopy: TMenuItem;
    tbnSep1: TToolButton;
    tbnSep2: TToolButton;
    procedure lvImportsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormResize(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure lvMSDOSData(Sender: TObject; Item: TListItem);
    procedure lvPEHeaderData(Sender: TObject; Item: TListItem);
    procedure lvPEOptionalHeaderData(Sender: TObject; Item: TListItem);
    procedure lvImportsData(Sender: TObject; Item: TListItem);
    procedure lvExportFunctionsData(Sender: TObject; Item: TListItem);
    procedure lvImportFunctionsData(Sender: TObject; Item: TListItem);
    procedure FormActivate(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFilePrinterSetupExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actOptionsDecimalExecute(Sender: TObject);
    procedure actOptionsHexExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    PEInfo: TPEFileInfo;
    FNumberType: TNumberType;
    FFileName: string;
    FBlockEvents: Boolean;
    FileDrop: TDropFileTarget;
    procedure LoadPEInfo(const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure SetNumberType(const Value: TNumberType);
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read FFileName;
    property NumberType: TNumberType read FNumberType write SetNumberType;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_GxUtils, GX_GenericUtils, GX_Experts,
  GX_ConfigurationInfo, GX_GExperts, Clipbrd, GX_SharedImages, Math;

type
  TPEExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

var
  fmPeInformation: TfmPeInformation = nil;
  PeExpert: TPEExpert;

procedure SetListViewItem(AItem: TListItem; AValue: string);
var
  ItemList: TStrings;
  i: Integer;
begin
  AValue := '"' + AValue + '"';

  AValue := StringReplace(AValue, #9, '","', [rfReplaceAll]);

  ItemList := TStringList.Create;
  try
    ItemList.CommaText := AValue;
    if ItemList.Count > 0 then
      AItem.Caption := ItemList[0];
    for i := 1 to ItemList.Count - 1 do
      AItem.SubItems.Add(ItemList[i]);
  finally
    FreeAndNil(ItemList);
  end;
end;

procedure SetListViewItems(ListView: TListView; Items: TStrings);
begin
  Assert(Assigned(ListView));
  Assert(Assigned(Items));

  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    ListView.Items.Count := Items.Count;
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TfmPeInformation.LoadPEInfo(const FileName: string);
resourcestring
  SFormCaption = 'PE Information - ';
  SPENoDirectories = 'PE information is not available for directories';
begin
  if DirectoryExists(FileName) then
    raise Exception.Create(SPENoDirectories);
  Screen.Cursor := crHourglass;
  FBlockEvents := True;
  try
    Self.FFileName := FileName;
    Caption := SFormCaption + ExtractFileName(FileName);
    pcMain.ActivePage := tshMSDOS;

    FreeAndNil(PEInfo);

    PEInfo := TPEFileInfo.Create(FileName, NumberType);
    lvImportFunctions.Items.Clear;
    SetListViewItems(lvMSDOS, PEInfo.MSDOSHeader);
    SetListViewItems(lvPEHeader, PEInfo.PEHeaderList);
    SetListViewItems(lvPEOptionalHeader, PEInfo.PEOptionalHeaderList);
    SetListViewItems(lvImports, PEInfo.ImportList);
    SetListViewItems(lvExportFunctions, PEInfo.ExportList);
    FBlockEvents := False;
  finally
    Screen.Cursor := crDefault;
  end;
  FormResize(Self);
end;

procedure TfmPeInformation.lvImportsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  ImpExp: TImportExport;
begin
  if Change = ctState then
  begin
    lvImportFunctions.Items.BeginUpdate;
    try
      try
        lvImportFunctions.Items.Clear;

        if lvImports.Selected = nil then
          Exit;

        ImpExp := TImportExport(PEInfo.ImportList.Objects[lvImports.Selected.Index]);
        Assert(Assigned(ImpExp));
        lvImportFunctions.Items.Count := ImpExp.Count;
      except
        on E: Exception do
          GxLogAndShowException(E);
      end;
    finally
      lvImportFunctions.Items.EndUpdate;
    end;
  end;
  FormResize(Self);
end;

procedure TfmPeInformation.FormResize(Sender: TObject);
begin
  try
    with lvMSDOS do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvPEOptionalHeader do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
    with lvImportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - 1, 0);
    with lvExportFunctions do
      Columns.Items[0].Width := Max(ClientWidth - Columns.Items[1].Width - Columns.Items[2].Width - 1, 0);
  except
    on E: Exception do
    begin
      // Swallow exceptions.
    end;
  end;
end;

procedure TfmPeInformation.SetNumberType(const Value: TNumberType);
begin
  FNumberType := Value;

  if PEInfo <> nil then
    LoadPEInfo(FFileName);
end;

procedure TfmPeInformation.SaveSettings;
begin
  // do not localize any of the below lines
  with TGExpertsSettings.Create do
  try
    SaveForm(Self, ConfigurationKey + '\Window');
    WriteInteger(ConfigurationKey, 'Numbers', Integer(NumberType));
    WriteString(ConfigurationKey, 'BinPath', ExtractFilePath(dlgOpen.FileName));
  finally
    Free;
  end;
end;

procedure TfmPeInformation.LoadSettings;
begin
  // do not localize any of the below lines
  with TGExpertsSettings.Create do
  try
    LoadForm(Self, ConfigurationKey + '\Window');
    NumberType := TNumberType(ReadInteger(ConfigurationKey, 'Numbers', Ord(ntHex)));
    dlgOpen.InitialDir := ReadString(ConfigurationKey, 'BinPath', '');
  finally
    Free;
  end;
  EnsureFormVisible(Self);
end;

procedure TfmPeInformation.pcMainChange(Sender: TObject);
begin
  // Let the listview update so the columns size right
  Application.ProcessMessages;
  FormResize(Self);
end;

procedure TfmPeInformation.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  if (FileDrop.Files = nil) or (FileDrop.Files.Count < 1) then
    Exit;
  LoadPEInfo(FileDrop.Files[0]);
end;

procedure TfmPeInformation.lvMSDOSData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.MSDOSHeader));

  SetListViewItem(Item, PEInfo.MSDOSHeader[Item.Index]);
end;

procedure TfmPeInformation.lvPEHeaderData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.PEHeaderList));

  SetListViewItem(Item, PEInfo.PEHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvPEOptionalHeaderData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.PEOptionalHeaderList));

  SetListViewItem(Item, PEInfo.PEOptionalHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvImportsData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.ImportList));

  SetListViewItem(Item, PEInfo.ImportList[Item.Index]);
end;

procedure TfmPeInformation.lvExportFunctionsData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.ExportList));

  SetListViewItem(Item, PEInfo.ExportList[Item.Index]);
end;

procedure TfmPeInformation.lvImportFunctionsData(Sender: TObject; Item: TListItem);
var
  ImpExp: TImportExport;
  SelectedListItem: TListItem;
begin
  if FBlockEvents then
    Exit;

  SelectedListItem := lvImports.Selected;
  if not Assigned(SelectedListItem) then
    Exit;

  Assert(Assigned(Item));
  Assert(Assigned(PEInfo));
  Assert(Assigned(PEInfo.ImportList));

  ImpExp := TImportExport(PEInfo.ImportList.Objects[SelectedListItem.Index]);

  Assert(Assigned(ImpExp));

  Item.Caption := ImpExp.Items[Item.Index].FunctionName;
  Item.SubItems.Add(PEInfo.IntToNum(ImpExp.Items[Item.Index].Ordinal));
end;

procedure TfmPeInformation.FormActivate(Sender: TObject);
begin
  // Needed later because docking cancels the registration??
  //if FileDrop <> nil then
  //  FileDrop.Register(pcMain);
end;

procedure TfmPeInformation.actEditCopyExecute(Sender: TObject);
var
  List: TListView;
  i, j: Integer;
  ItemString: string;
  PELines: TStringList;
begin
  List := nil;
  if ActiveControl is TListView then
    List := ActiveControl as TListView
  else
    for i := pcMain.ActivePage.ControlCount - 1 downto 0 do
      if pcMain.ActivePage.Controls[i] is TListView then
      begin
        List := pcMain.ActivePage.Controls[i] as TListView;
        Break;
      end;
  if List = nil then
    Exit;
  PELines := TStringList.Create;
  try
    for i := 0 to List.Items.Count - 1 do
    begin
      ItemString := List.Items.Item[i].Caption;
      for j := 0 to List.Items.Item[i].SubItems.Count - 1 do
        ItemString := ItemString + #09 + List.Items.Item[i].SubItems.Strings[j];
      PELines.Add(ItemString);
    end;
    Clipboard.AsText := PELines.Text;
  finally
    FreeAndNil(PELines);
  end;
end;

procedure TfmPeInformation.actFileOpenExecute(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if GetOpenSaveDialogExecute(dlgOpen) then
      LoadPEInfo(dlgOpen.FileName);
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmPeInformation.actFilePrinterSetupExecute(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;

procedure TfmPeInformation.actFilePrintExecute(Sender: TObject);
resourcestring
  SPeInfoFor = 'PE Information for ';
  SMsDosHeader = 'MS-DOS Header';
  SPeHeader = 'PE Header';
  SPeOptionalHeader = 'PE Optional Header';
  SImports = 'Imports';
  SFunction = 'Function';
  SOrdinal = 'Ordinal';
  SExports = 'Exports';

var
  RichEdit: TRichEdit;
  Line: string;
  i, j: Integer;
  ImpExp: TImportExport;

  procedure PrintHeader(LV: TListView; const Header: string);
  var
    i: Integer;
  begin
    with RichEdit do
    begin
      RichEdit.SelAttributes.Style := [fsBold];
      Lines.Add(Header);

      RichEdit.SelAttributes.Style := [];
      for i := 0 to LV.Items.Count - 1 do
      begin
        Lines.Add('   ' + LV.Items[i].Caption + #9 + ':   ' + LV.Items[i].SubItems[0]);
      end;
      Lines.Add('');
    end;
  end;

begin
  if PEInfo = nil then
    Exit;
  try
    RichEdit := TRichEdit.Create(Self);
    Screen.Cursor := crHourglass;
    try
      with RichEdit do
      begin
        Visible := False;
        Parent := Self;
        Clear;
        DefAttributes.Name := 'Arial';
        DefAttributes.Size := 10;

        Paragraph.TabCount := 1;
        Paragraph.Tab[0]   := 200;
      end;

      // Document header
      RichEdit.Lines.Add('PE Header information for ' + FFileName);
      // AJB: I would like some file info here, date/time, version...
      RichEdit.Lines.Add('');
      // MS-DOS Header
      PrintHeader(lvMSDOS, SMsDosHeader);
      // PE Header
      PrintHeader(lvPEHeader, SPeHeader);
      // PE Optional Header
      PrintHeader(lvPEOptionalHeader, SPeOptionalHeader);

      // Imports
      with RichEdit do
      begin
        RichEdit.Paragraph.TabCount := 2;
        RichEdit.Paragraph.Tab[0]   := 80;
        RichEdit.Paragraph.Tab[1]   := 300;
        RichEdit.SelAttributes.Style := [fsBold];
        SelText := SImports;

        for j := 0 to lvImports.Items.Count - 1 do
        begin
          SelAttributes.Style := [fsUnderline];
          Lines.Add('   ' + lvImports.Items[j].Caption + #09 + SFunction + #09 + SOrdinal);
          SelAttributes.Style := [];

          ImpExp := TImportExport(PEInfo.ImportList.Objects[j]);

          for i := 0 to ImpExp.Count - 1 do
          begin
            Line := ImpExp.Items[i].FunctionName;
            if Length(Line) > 32 then
              Line := Copy(ImpExp.Items[i].FunctionName, 1, 32) + '...';
            Lines.Add(#09 + Line + #09 + IntToStr(ImpExp.Items[i].Ordinal));
          end;
          Lines.Add('');
        end;
      end;

      // Exports
      with RichEdit do
      begin
        RichEdit.Paragraph.TabCount := 3;
        RichEdit.Paragraph.Tab[0]   := 20;
        RichEdit.Paragraph.Tab[1]   := 280;
        RichEdit.Paragraph.Tab[2]   := 380;

        RichEdit.SelAttributes.Style := [fsBold];
        SelText := SExports;
        RichEdit.SelAttributes.Style := [];
        for i := 0 to lvExportFunctions.Items.Count - 1 do
        begin
          Lines.Add(#09 + lvExportFunctions.Items[i].Caption + #09 +
            lvExportFunctions.Items[i].SubItems[0] + #09 + lvExportFunctions.Items[i].SubItems[1]);
        end;
      end;

      RichEdit.Print(SPeInfoFor + ExtractFileName(dlgOpen.FileName));
    finally
      Screen.Cursor := crDefault;
      FreeAndNil(RichEdit);
    end;
  except
    on E: Exception do
      GxLogAndShowException(E);
  end;

end;

procedure TfmPeInformation.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmPeInformation.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 16);
end;

procedure TfmPeInformation.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmPeInformation.actOptionsDecimalExecute(Sender: TObject);
begin
  NumberType := ntDecimal;
end;

procedure TfmPeInformation.actOptionsHexExecute(Sender: TObject);
begin
  NumberType := ntHex;
end;

constructor TfmPeInformation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetToolbarGradient(ToolBar);

  if Assigned(PeExpert) then
    PeExpert.SetFormIcon(Self);

  FileDrop := TDropFileTarget.Create(nil);
  FileDrop.OnDrop := DropFiles;
  FileDrop.DragTypes := [dtCopy, dtMove, dtLink];
  FileDrop.ShowImage := True;
  FileDrop.Register(pcMain);

  pcMain.ActivePage := tshMSDOS;
  CenterForm(Self);
  LoadSettings;
end;

destructor TfmPeInformation.Destroy;
begin
  SaveSettings;

  FreeAndNil(PEInfo);

  if Assigned(FileDrop) then
  begin
    FileDrop.Unregister;
    FreeAndNil(FileDrop);
  end;

  inherited Destroy;

  fmPeInformation := nil;
end;

procedure TfmPeInformation.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmPeInformation.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actOptionsDecimal.Checked := (NumberType = ntDecimal);
  actOptionsHex.Checked := (NumberType = ntHex);
end;

procedure TfmPeInformation.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

function TfmPeInformation.ConfigurationKey: string;
begin
   Result := TPEExpert.ConfigurationKey;
end;

{ TPEExpert }

constructor TPEExpert.Create;
begin
  inherited Create;
  ShortCut := 0;
  PeExpert := Self;
end;

destructor TPEExpert.Destroy;
begin
  PeExpert := nil;
  inherited;
end;

procedure TPEExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      IdeDockManager.RegisterDockableForm(TfmPeInformation, fmPeInformation, 'fmPeInformation')
    else
    begin
      IdeDockManager.UnRegisterDockableForm(fmPeInformation, 'fmPeInformation');
      FreeAndNil(fmPeInformation);
    end;
  end;
end;

function TPEExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'P&E Information';
begin
  Result := SMenuCaption;
end;

class function TPEExpert.GetName: string;
begin
  Result := 'PEInformation';
end;

procedure TPEExpert.Click(Sender: TObject);
begin
  if fmPeInformation = nil then
    fmPeInformation := TfmPeInformation.Create(nil);
  IdeDockManager.ShowForm(fmPeInformation);
  EnsureFormVisible(fmPeInformation);
end;

function TPEExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterGX_Expert(TPEExpert);
end.

