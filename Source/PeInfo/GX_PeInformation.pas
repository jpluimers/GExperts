unit GX_PeInformation;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GX_PeInfo, ComCtrls, Menus,
  GX_IdeDock, DropTarget, DropSource, ActnList, ToolWin, StdCtrls, SysUtils;

type
  TfmPeInformation = class(TfmIdeDockForm)
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
    tshVersionInfo: TTabSheet;
    lvVersionInfo: TListView;
    tshPackageInfo: TTabSheet;
    splPackageInfo: TSplitter;
    lbPackageInfoType: TListBox;
    lbPackageInfo: TListBox;
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
    procedure lbPackageInfoTypeClick(Sender: TObject);
  private
    PEInfo: TPEFileInfo;
    FNumberType: TNumberType;
    FFileName: string;
    FBlockEvents: Boolean;
    FileDrop: TDropFileTarget;
    procedure LoadPEInfo(const AFileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure SetNumberType(const Value: TNumberType);
    function ConfigurationKey: string;
    procedure SetVersionInfo(const AFilename: string);
    procedure SetPackageInfo(const AFilename: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read FFileName;
    property NumberType: TNumberType read FNumberType write SetNumberType;
  end;

procedure ShowPeInfo(CmdLine: PAnsiChar); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

{$R *.dfm}

uses
  StrUtils, GX_GxUtils, GX_GenericUtils, GX_Experts,
  GX_ConfigurationInfo, GX_GExperts, Clipbrd, GX_SharedImages, Math,
  GX_DbugIntf, GX_dzVersionInfo, GX_dzPackageInfo, GX_dzClassUtils,
  GX_dzVclUtils, GX_PeInfoPrint;

type
  TPEExpert = class(TGX_Expert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
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

procedure TfmPeInformation.LoadPEInfo(const AFileName: string);
resourcestring
  SFormCaption = 'PE Information - ';
  SPENoDirectories = 'PE information is not available for directories';
begin
  lvVersionInfo.Items.Clear;
  TListbox_ClearWithObjects(lbPackageInfoType);
  lbPackageInfo.Items.Clear;

  if DirectoryExists(AFileName) then
    raise Exception.Create(SPENoDirectories);
  Screen.Cursor := crHourglass;
  FBlockEvents := True;
  try
    Self.FFileName := AFileName;
    Caption := SFormCaption + ExtractFileName(AFileName);
    pcMain.ActivePage := tshMSDOS;

    FreeAndNil(PEInfo);

    PEInfo := TPEFileInfo.Create(AFileName, NumberType);
    lvImportFunctions.Items.Clear;
    SetListViewItems(lvMSDOS, PEInfo.MSDOSHeader);
    SetListViewItems(lvPEHeader, PEInfo.PEHeaderList);
    SetListViewItems(lvPEOptionalHeader, PEInfo.PEOptionalHeaderList);
    SetListViewItems(lvImports, PEInfo.ImportList);
    SetListViewItems(lvExportFunctions, PEInfo.ExportList);

    SetVersionInfo(AFilename);
    SetPackageInfo(AFilename);

  finally
    FBlockEvents := False;
    Screen.Cursor := crDefault;
  end;
  FormResize(Self);
end;

procedure TfmPeInformation.SetVersionInfo(const AFilename: string);
var
  VerItems: TListItems;

  procedure AddItem(const ACaption, AValue: string);
  var
    li: TListItem;
  begin
      li := VerItems.Add;
      li.Caption := ACaption;
      li.SubItems.Add(AValue);
  end;

resourcestring
  SNoVersionInfo = 'no version info';
var
  VerInfo: IFileInfo;
begin
  VerInfo := TFileInfo.Create(AFileName);
  VerItems := lvVersionInfo.Items;
  VerItems.BeginUpdate;
  try
    VerItems.Clear;
    if not VerInfo.HasVersionInfo then begin
      AddItem(SNoVersionInfo, '');
    end else begin
      AddItem('Filename', VerInfo.Filename);
      AddItem('FileDir', VerInfo.FileDir);
      AddItem('Description', VerInfo.FileDescription);
      AddItem('Version', VerInfo.FileVersion);
      AddItem('Product', VerInfo.ProductName);
      AddItem('Product Version', VerInfo.ProductVersion);
      AddItem('Company', VerInfo.CompanyName);
      AddItem('Copyright', VerInfo.LegalCopyRight);
      AddItem('Trademarks', VerInfo.LegalTradeMarks);
      AddItem('Internal Name', VerInfo.InternalName);
      AddItem('Original Filename', VerInfo.OriginalFilename);
    end;
  finally
    VerItems.EndUpdate;
  end;
end;

{$IFNDEF GX_VER170_up}

function StartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiStartsText(ASubText, AText);
end;
{$ENDIF}

procedure TfmPeInformation.SetPackageInfo(const AFilename: string);
var
  sl: TStringList;
  pitItems: TStrings;
  Info: TPackageInfo;
  i: Integer;
  s: string;
  p: Integer;
begin
  Info := TPackageInfo.Create(AFilename);
  try
    pitItems := lbPackageInfoType.Items;
    pitItems.BeginUpdate;
    try
      TListbox_ClearWithObjects(lbPackageInfoType);

      sl := TStringList.Create;
      sl.Add(Info.Description);
      pitItems.AddObject('Description', sl);

      sl := TStringList.Create;
      sl.Assign(Info.Units);
      pitItems.AddObject('Units', sl);

      sl := TStringList.Create;
      sl.Assign(Info.Required);
      pitItems.AddObject('Required Packages', sl);

      sl := TStringList.Create;
      for i := 0 to PEInfo.ExportList.Count - 1 do begin
        s := PEInfo.ExportList[i];
        if StartsText('@$xp$', s) then begin
          s := Copy(s, 2, 255);
          p := Pos('@', s);
          if p > 0 then begin
            s := Copy(s, p + 1, 255);
            p := Pos('$', s);
            if p > 0 then
              s := Copy(s, 1, p - 1);
            sl.Add(s);
          end;
        end;
      end;
      pitItems.AddObject('Exported Classes', sl);
    finally
      pitItems.EndUpdate;
    end;
  finally
    FreeAndNil(Info);
  end;
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
    with lvVersionInfo do
      Columns.Items[1].Width := Max(ClientWidth - Columns.Items[0].Width - 1, 0);
  except
    on E: Exception do
    begin
      // Swallow exceptions.
    end;
  end;
end;

procedure TfmPeInformation.lbPackageInfoTypeClick(Sender: TObject);
var
  sl: TStringList;
  Idx: Integer;
  InfoItems: TStrings;
begin
  InfoItems := lbPackageInfo.Items;
  InfoItems.BeginUpdate;
  try
    InfoItems.Clear;
    Idx := lbPackageInfoType.ItemIndex;
    if Idx = -1 then
      Exit;
    sl := lbPackageInfoType.Items.Objects[Idx] as TStringList;
    if not Assigned(sl) then
      Exit;
    InfoItems.Assign(sl);
  finally
    InfoItems.EndUpdate;
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
    WriteString(ConfigurationKey, 'BinPath', ExtractFilePath(FFileName));
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
    FFileName := ReadString(ConfigurationKey, 'BinPath', '');
    FFileName :=  IncludeTrailingPathDelimiter(FFileName) + 'SomeExecutable.exe';
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
  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.MSDOSHeader) then
    Exit;
  SetListViewItem(Item, PEInfo.MSDOSHeader[Item.Index]);
end;

procedure TfmPeInformation.lvPEHeaderData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;
  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.PEHeaderList) then
    Exit;
  SetListViewItem(Item, PEInfo.PEHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvPEOptionalHeaderData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;
  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.PEOptionalHeaderList) then
    Exit;
  SetListViewItem(Item, PEInfo.PEOptionalHeaderList[Item.Index]);
end;

procedure TfmPeInformation.lvImportsData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;
  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.ImportList) then
    Exit;
  SetListViewItem(Item, PEInfo.ImportList[Item.Index]);
end;

procedure TfmPeInformation.lvExportFunctionsData(Sender: TObject; Item: TListItem);
begin
  if FBlockEvents then
    Exit;
  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.ExportList) then
    Exit;
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

  if not Assigned(Item) or not Assigned(PEInfo) or not assigned(PEInfo.ImportList) then
    Exit;

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
  else begin
    if pcMain.ActivePage = tshPackageInfo then begin
      PELines := TStringList.Create;
      try
        for i := 0 to lbPackageInfoType.Items.Count - 1 do begin
          if i > 0 then
            PELines.Add('');
          PELines.Add(lbPackageInfoType.Items[i] + ':');
          PELines.AddStrings(TStrings(lbPackageInfoType.Items.Objects[i]));
        end;
        Clipboard.AsText := PELines.Text;
      finally
        PELines.Free;
      end;
      Exit;
    end;

    for i := pcMain.ActivePage.ControlCount - 1 downto 0 do
      if pcMain.ActivePage.Controls[i] is TListView then
      begin
        List := pcMain.ActivePage.Controls[i] as TListView;
        Break;
      end;
  end;
  if (List = nil) then
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
  fn: string;
begin
  fn := FFileName;
  if ShowOpenDialog('Open file to examine', 'exe', fn,
    'PE Binary Files (*.exe, *.dll, *.bpl, *.dpl, *.ocx)|*.exe;*.dll;*.bpl;*.dpl;*.ocx|'
    + 'EXE Files (*.exe)|*.EXE|'
    + 'DLL Files (*.dll)|*.dll|'
    + 'CodeGear Packages (*.bpl, *.dpl)|*.dpl;*.bpl|'
    + 'OCX Controls (*.ocx)|*.ocx') then
    LoadPEInfo(fn);
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
  SVersionInfo = 'Version Info';
  SPackageInfo = 'Package Info';
  SNoPackageInformationAvailable = 'No package information available';

var
  RichEdit: TRichEdit;

  procedure PrintHeader(LV: TListView; const Header: string);
  var
    i: Integer;
    Line: string;
  begin
    with RichEdit do
    begin
      RichEdit.SelAttributes.Style := [fsBold];
      Lines.Add(Header);

      RichEdit.SelAttributes.Style := [];
      for i := 0 to LV.Items.Count - 1 do
      begin
        Line := LV.Items[i].Caption;
        if LV.Items[i].SubItems.Count > 0 then
          Line := Line + #9 + ':   ' + LV.Items[i].SubItems[0];
        Lines.Add('   ' + Line);
      end;
      Lines.Add('');
    end;
  end;

var
  Tabs: TPeInfoTabSet;
  Line: string;
  i, j: Integer;
  ImpExp: TImportExport;
  sl: TStrings;
  li: TListItem;
begin
  if PEInfo = nil then
    Exit;

  if pcMain.ActivePage = tshMSDOS then
    Tabs := [pitMsDos]
  else if pcMain.ActivePage = tshPEHEader then
    Tabs := [pitPeHeader]
  else if pcMain.ActivePage = tshPEHEader then
    Tabs := [pitPeHeader]
  else if pcMain.ActivePage = tshPEOptional then
    Tabs := [pitPeOptHeader]
  else if pcMain.ActivePage = tshImport then
    Tabs := [pitImports]
  else if pcMain.ActivePage = tshExports then
    Tabs := [pitExports]
  else if pcMain.ActivePage = tshVersionInfo then
    Tabs := [pitVersionInfo]
  else
    Tabs := [pitPackageInfo];

  if not Tf_PeInfoPrint.Execute(Self, Tabs) or (Tabs = []) then
    Exit;

  try

    RichEdit := TRichEdit.Create(Self);
    Screen.Cursor := crHourglass;
    try
      RichEdit.Visible := False;
      RichEdit.Parent := Self;
      RichEdit.Clear;
      RichEdit.DefAttributes.Name := 'Arial';
      RichEdit.DefAttributes.Size := 10;

      RichEdit.Paragraph.TabCount := 1;
      RichEdit.Paragraph.Tab[0]   := 200;

      // Document header
      RichEdit.Lines.Add('PE Header information for ' + FFileName);
      // AJB: I would like some file info here, date/time, version...
      RichEdit.Lines.Add('');

      if pitMsDos in Tabs then begin
        // MS-DOS Header
        PrintHeader(lvMSDOS, SMsDosHeader);
      end;

      if pitPeHeader in Tabs then begin
        // PE Header
        PrintHeader(lvPEHeader, SPeHeader);
      end;

      if pitPeOptHeader in Tabs then begin
        // PE Optional Header
        PrintHeader(lvPEOptionalHeader, SPeOptionalHeader);
      end;

      if pitImports in Tabs then begin
        // Imports
        RichEdit.Paragraph.TabCount := 2;
        RichEdit.Paragraph.Tab[0]   := 80;
        RichEdit.Paragraph.Tab[1]   := 300;
        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SImports;

        for j := 0 to lvImports.Items.Count - 1 do
        begin
          RichEdit.SelAttributes.Style := [fsUnderline];
          Line := PEInfo.ImportList[j] + #09 + SFunction + #09 + SOrdinal;
          RichEdit.Lines.Add('   ' + Line + '   ');
          RichEdit.SelAttributes.Style := [];

          ImpExp := TImportExport(PEInfo.ImportList.Objects[j]);

          for i := 0 to ImpExp.Count - 1 do
          begin
            Line := ImpExp.Items[i].FunctionName;
            if Length(Line) > 32 then
              Line := Copy(Line, 1, 32) + '...';
            RichEdit.Lines.Add(#09 + Line + #09 + IntToStr(ImpExp.Items[i].Ordinal));
          end;
          RichEdit.Lines.Add('');
        end;
      end;

      if pitExports in Tabs then begin
        // Exports
        RichEdit.Paragraph.TabCount := 3;
        RichEdit.Paragraph.Tab[0]   := 20;
        RichEdit.Paragraph.Tab[1]   := 280;
        RichEdit.Paragraph.Tab[2]   := 380;

        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SExports;
        RichEdit.SelAttributes.Style := [];
        for i := 0 to lvExportFunctions.Items.Count - 1 do
        begin
          li := lvExportFunctions.Items[i];
          Line := li.Caption + #09 + li.SubItems[0] + #09 + li.SubItems[1];
          RichEdit.Lines.Add(#09 + Line);
        end;

        RichEdit.Lines.Add('');
        RichEdit.Lines.Add('');
      end;

      if pitVersionInfo in Tabs then begin
        // Version information
        RichEdit.Paragraph.TabCount := 1;
        RichEdit.Paragraph.Tab[0]   := 200;

        PrintHeader(lvVersionInfo, SVersionInfo);

        RichEdit.Lines.Add('');
      end;

      if pitPackageInfo in Tabs then begin
        // Package Info
        RichEdit.Paragraph.TabCount := 1;
        RichEdit.Paragraph.Tab[0]   := 100;
        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.SelText := SPackageInfo;

        if lbPackageInfoType.Items.Count = 0 then begin
          RichEdit.SelAttributes.Style := [];
          RichEdit.Lines.Add(SNoPackageInformationAvailable);
          RichEdit.Lines.Add('');
        end else begin
          for j := 0 to lbPackageInfoType.Items.Count - 1 do begin
            RichEdit.SelAttributes.Style := [fsUnderline];
            RichEdit.Lines.Add(lbPackageInfoType.Items[j]);

            RichEdit.SelAttributes.Style := [];
            sl := TStrings(lbPackageInfoType.Items.Objects[j]);
            for i := 0 to sl.Count - 1 do begin
              Line := sl[i];
              RichEdit.Lines.Add(#09 + Line);
            end;
            RichEdit.Lines.Add('');
          end;
        end;
      end;

      RichEdit.Print(SPeInfoFor + ExtractFileName(FFileName));
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

  TControl_SetMinConstraints(Self);

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

  TListbox_ClearWithObjects(lbPackageInfoType);

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

procedure TPEExpert.Execute(Sender: TObject);
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

procedure ShowPeInfo(CmdLine: PAnsiChar); cdecl; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
var
  PEExpertStandAlone: TPEExpert;
  fn: string;
begin
  {$IFOPT D+}SendDebug('Showing PE Information'); {$ENDIF}
  PEExpertStandAlone := nil;
  InitSharedResources;
  try
    {$IFOPT D+} SendDebug('Created CodeLib window'); {$ENDIF}
    PEExpertStandAlone := TPEExpert.Create;
    PEExpertStandAlone.LoadSettings;
    fmPeInformation := TfmPeInformation.Create(nil);
    if Assigned(CmdLine)  then begin
      fn := string(cmdline);
      if fn <> '' then
        fmPeInformation.LoadPEInfo(fn);
    end;
    fmPeInformation.ShowModal;
    PEExpertStandAlone.SaveSettings;
  finally
    // Destroying the form will result in an access violation in
    // TfmIdeDockForm.Destroy which I could not debug and prevent properly.
    // Just setting it to nil creates a memory leak, but we are shutting down anyway.
    // -- 2016-06-05 twm
    fmPeInformation := nil;
    FreeAndNil(PEExpertStandAlone);
    FreeSharedResources;
  end;
end;

initialization
  RegisterGX_Expert(TPEExpert);
end.

