unit GX_OpenFileConfig;

interface

uses
  Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls, ExtCtrls,
  GX_ConfigurationInfo, GX_BaseForm;

type
  TIDEOverride = class(TStringList)
  private
    function GetOverrideOpenUnit: Boolean;
    procedure SetOverrideOpenUnit(Value: Boolean);
    function GetOverrideOpenForm: Boolean;
    procedure SetOverrideOpenForm(Value: Boolean);
    function GetOverrideOpenProject: Boolean;
    procedure SetOverrideOpenProject(Value: Boolean);
    function BooleanString(Value: Boolean): string;
    function GetOpenUnitDefaultType: string;
    procedure SetOpenUnitDefaultType(const Value: string);
    function GetOpenProjectDefaultType: string;
    procedure SetOpenProjectDefaultType(const Value: string);
    function GetOpenFormDefaultType: string;
    procedure SetOpenFormDefaultType(const Value: string);
  public
    property OverrideOpenForm: Boolean read GetOverrideOpenForm write SetOverrideOpenForm;
    property OverrideOpenProject: Boolean read GetOverrideOpenProject write SetOverrideOpenProject;
    property OpenUnitDefaultType: string read GetOpenUnitDefaultType write SetOpenUnitDefaultType;
    property OpenFormDefaultType: string read GetOpenFormDefaultType write SetOpenFormDefaultType;
    property OpenProjectDefaultType: string read GetOpenProjectDefaultType write SetOpenProjectDefaultType;
    property OverrideOpenUnit: Boolean read GetOverrideOpenUnit write SetOverrideOpenUnit;
  end;

  TFileType = class(TCollectionItem)
  private
    FFileTypeName: string;
    FExtensions: string;
    FPaths: TStringList;
    FCustomDirectories: Boolean;
    FRecursive: Boolean;
    FRecentFiles: TStringList;
    FMaxRecentFiles: Integer;
    FFavorites: TStringList;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property FileTypeName: string read FFileTypeName write FFileTypeName;
    property Extensions: string read FExtensions write FExtensions;
    property Paths: TStringList read FPaths write FPaths;
    property CustomDirectories: Boolean read FCustomDirectories write FCustomDirectories;
    property Recursive: Boolean read FRecursive write FRecursive;
    property RecentFiles: TStringList read FRecentFiles write FRecentFiles;
    property MaxRecentFiles: Integer read FMaxRecentFiles write FMaxRecentFiles;
    property Favorites: TStringList read FFavorites write FFavorites;
  end;

  TFileTypes = class(TCollection)
  private
    function GetItem(Index: Integer): TFileType;
  public
    property Items[Index: Integer]: TFileType read GetItem; default;
  end;

  TOpenFileSettings = class(TObject)
  private
    procedure LoadDefaultSettings(FileTypes: TFileTypes);
  public
    MatchAnywhere: Boolean;
    DefaultFileType: string;
    ShowMapTab: Boolean;
    IDEOverride: TIDEOverride;
    FileTypes: TFileTypes;
    ConfigurationKey: string;
    LastTabIndex: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure InternalLoadSettings(Settings: TExpertSettings);
    procedure SaveToRegistry;
    procedure InternalSaveSettings(Settings: TExpertSettings);
  end;

  TfmOpenFileConfig = class(TfmBaseForm)
    pnlButtons: TPanel;
    pnlConfig: TPanel;
    pnlButtonsRight: TPanel;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    pcConfigPages: TPageControl;
    tsTypes: TTabSheet;
    lblExtension: TLabel;
    lblMaxRecentFiles: TLabel;
    gbxCustomDirectory: TGroupBox;
    btnDirectory: TSpeedButton;
    lbxDirectoryList: TListBox;
    btnDirectoryAdd: TBitBtn;
    btnDirectoryDelete: TBitBtn;
    chkRecursive: TCheckBox;
    btnDirectoryReplace: TBitBtn;
    edtDirectory: TEdit;
    lbxTypeList: TListBox;
    edtExtension: TEdit;
    btnTypeAdd: TBitBtn;
    btnTypeDelete: TBitBtn;
    edtMaxRecentFiles: TEdit;
    tsIDEOptions: TTabSheet;
    gbxIDEMenuItems: TGroupBox;
    lblMenuItem: TLabel;
    lblFileGroup: TLabel;
    bvlRow: TBevel;
    chkOverrideViewUnit: TCheckBox;
    chkOverrideViewForm: TCheckBox;
    cbxViewUnitType: TComboBox;
    cbxViewFormType: TComboBox;
    chkOverrideOpenProject: TCheckBox;
    cbxOpenProjectType: TComboBox;
    gbxGeneralSettings: TGroupBox;
    lblDefault: TLabel;
    cbxDefaultFileTypes: TComboBox;
    chkMatchAnywhere: TCheckBox;
    chkCustomDirectoryList: TCheckBox;
    chkMapTab: TCheckBox;
    procedure chkCustomDirectoryListClick(Sender: TObject);
    procedure btnTypeAddClick(Sender: TObject);
    procedure lbxTypeListClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnTypeDeleteClick(Sender: TObject);
    procedure btnDirectoryClick(Sender: TObject);
    procedure btnDirectoryAddClick(Sender: TObject);
    procedure btnDirectoryDeleteClick(Sender: TObject);
    procedure btnDirectoryReplaceClick(Sender: TObject);
    procedure lbxDirectoryListClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCurrentFileType: TFileType;
    FSettings: TOpenFileSettings;
    procedure SaveConfigValue;
    function SelectDirectory: Boolean;
    procedure UseSettings(Settings: TOpenFileSettings);
    procedure SaveSettings(Settings: TOpenFileSettings);
    function GetFileTypes: TFileTypes;
    function IDEOverride: TIDEOverride;
    procedure lbxDirectoryListFilesDropped(Sender: TObject; Files: TStrings);
  public
    class function ExecuteWithSettings(Settings: TOpenFileSettings): Boolean;
    property FileTypes: TFileTypes read GetFileTypes;
  end;

implementation

uses
  SysUtils, Dialogs, GX_GenericUtils, GX_GxUtils, GX_dzVclUtils;

{$R *.dfm}

const
  DefaultMaxMRU = 100;

{ TFileType }

constructor TFileType.Create(Collection: TCollection);
begin
  inherited;
  FPaths := TStringList.Create;
  FRecentFiles := TStringList.Create;
  FRecentFiles.Sorted := True;
  FRecentFiles.Duplicates := dupIgnore;
  FFavorites := TStringList.Create;
  FFavorites.Sorted := True;
  FFavorites.Duplicates := dupIgnore;
end;

destructor TFileType.Destroy;
begin
  FreeAndNil(FPaths);
  FreeAndNil(FRecentFiles);
  FreeAndNil(FFavorites);
  inherited;
end;

{ TFileTypes }

function TFileTypes.GetItem(Index: Integer): TFileType;
begin
  Assert((Index > -1) and (Index < Count));
  Result := inherited GetItem(Index) as TFileType;
end;

{TIDEOverride}

function TIDEOverride.GetOverrideOpenUnit: Boolean;
begin
  Result := (Values['OverrideOpenUnit'] = 'True');
end;

procedure TIDEOverride.SetOverrideOpenUnit(Value: Boolean);
begin
  if Values['OverrideOpenUnit'] = '' then
    Add('OverrideOpenUnit=' + BooleanString(Value))
  else
    Values['OverrideOpenUnit'] := BooleanString(Value);
end;

function TIDEOverride.GetOverrideOpenForm: Boolean;
begin
  Result := (Values['OverrideOpenForm'] = 'True');
end;

procedure TIDEOverride.SetOverrideOpenForm(Value: Boolean);
begin
  if Values['OverrideOpenForm'] = '' then
    Add('OverrideOpenForm=' + BooleanString(Value))
  else
    Values['OverrideOpenForm'] := BooleanString(Value);
end;

function TIDEOverride.GetOverrideOpenProject: Boolean;
begin
  Result := (Values['OverrideOpenProject'] = 'True');
end;

procedure TIDEOverride.SetOverrideOpenProject(Value: Boolean);
begin
  if Values['OverrideOpenProject'] = '' then
    Add('OverrideOpenProject=' + BooleanString(Value))
  else
    Values['OverrideOpenProject'] := BooleanString(Value);
end;

function TIDEOverride.BooleanString(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function TIDEOverride.GetOpenUnitDefaultType: string;
begin
  Result := Values['OverrideOpenUnitDefault'];
end;

procedure TIDEOverride.SetOpenUnitDefaultType(const Value: string);
begin
  if Values['OverrideOpenUnitDefault'] = '' then
    Add('OverrideOpenUnitDefault=' + Value)
  else
    Values['OverrideOpenUnitDefault'] := Value;
end;

function TIDEOverride.GetOpenProjectDefaultType: string;
begin
  Result := Values['OverrideOpenProjectDefault'];
end;

procedure TIDEOverride.SetOpenProjectDefaultType(const Value: string);
begin
  if Values['OverrideOpenProjectDefault'] = '' then
    Add('OverrideOpenProjectDefault=' + Value)
  else
    Values['OverrideOpenProjectDefault'] := Value;
end;

function TIDEOverride.GetOpenFormDefaultType: string;
begin
  Result := Values['OverrideOpenFormDefault'];
end;

procedure TIDEOverride.SetOpenFormDefaultType(const Value: string);
begin
  if Values['OverrideOpenFormDefault'] = '' then
    Add('OverrideOpenFormDefault=' + Value)
  else
    Values['OverrideOpenFormDefault'] := Value;
end;

{ TOpenFileSettings }

constructor TOpenFileSettings.Create;
begin
  inherited;
  IDEOverride := TIDEOverride.Create;
  FileTypes := TFileTypes.Create(TFileType);
end;

destructor TOpenFileSettings.Destroy;
begin
  FreeAndNil(IDEOverride);
  FreeAndNil(FileTypes);
  inherited;
end;

procedure TOpenFileSettings.LoadDefaultSettings(FileTypes: TFileTypes);
var
  FileType: TFileType;
begin
  FileTypes.Clear;
  FileType := TFileType.Create(FileTypes);
  FileType.FileTypeName := 'Delphi Units';
  FileType.Extensions := '*.pas;*.inc';
  FileType.MaxRecentFiles := DefaultMaxMRU;

  FileType := TFileType.Create(FileTypes);
  FileType.FileTypeName := 'Delphi Forms';
  FileType.Extensions := '*.dfm;*.fmx;*.nfm;*.xfm';
  FileType.MaxRecentFiles := DefaultMaxMRU;

  FileType := TFileType.Create(FileTypes);
  FileType.FileTypeName := 'Delphi Projects';
  FileType.Extensions := '*.dpr;*.dpk;*.bpg;*.bdsproj;*.dproj;*.groupproj';
  FileType.MaxRecentFiles := DefaultMaxMRU;

  FileType := TFileType.Create(FileTypes);
  FileType.FileTypeName := 'Text Files';
  FileType.Extensions := '*.txt;*.html;*.htm;*.sql;*.py;*.iss;*.ini;*.bat;*.log;*.map;*.conf;*.dsk;*.rc;*.xml;*.pl';
  FileType.MaxRecentFiles := DefaultMaxMRU;
end;

procedure TOpenFileSettings.InternalLoadSettings(Settings: TExpertSettings);
const
  InvalidTypeName = '<invalid>';
var
  NumFileTypes: Integer;
  i: Integer;
  KeyString: string;
  FileType: TFileType;
  TypeName: string;
  FileTypeSettings: TExpertSettings;
begin
  Assert(Assigned(Settings));
  FileTypes.Clear;
  MatchAnywhere := Settings.ReadBool('MatchAnywhere', True);
  ShowMapTab := Settings.ReadBool('ShowMapTab', False);
  IDEOverride.Text := Settings.ReadString('IDEOverride', '');
  DefaultFileType := Settings.ReadString('DefaultFileType', '');
  NumFileTypes := Settings.ReadInteger('NumberFileTypes', 0);
  LastTabIndex := Settings.ReadInteger('LastTabIndex', 0);
  IDEOverride.CommaText := Settings.ReadString('IDEOverride', '');
  if NumFileTypes > 0 then
  begin
    FileTypeSettings := Settings.CreateExpertSettings('FileTypes');
    try
      for i := 0 to NumFileTypes - 1 do
      begin
        KeyString := IntToStr(i);
        TypeName := FileTypeSettings.ReadString('FileType' + KeyString, InvalidTypeName);
        if TypeName = InvalidTypeName then
          Continue;
        FileType := TFileType.Create(FileTypes);
        FileType.FileTypeName := TypeName;
        FileTypeSettings.ReadStrings('Paths' + KeyString, FileType.Paths);
        FileType.Extensions := FileTypeSettings.ReadString('Extensions' + KeyString, AllFilesWildCard);
        FileType.CustomDirectories := FileTypeSettings.ReadBool('CustomDirectory' + KeyString, False);
        FileType.Recursive := FileTypeSettings.ReadBool('RecursiveDirectory' + KeyString, False);
        FileTypeSettings.ReadStrings('RecentFiles' + KeyString, FileType.RecentFiles);
        FileTypeSettings.ReadStrings('Favorites' + KeyString, FileType.Favorites);
        FileType.MaxRecentFiles := FileTypeSettings.ReadInteger('MaxRecentFiles' + KeyString, DefaultMaxMRU);
      end;
    finally
      FreeAndNil(FileTypeSettings);
    end;
  end;
  if FileTypes.Count < 1 then
    LoadDefaultSettings(FileTypes);
end;

procedure TOpenFileSettings.InternalSaveSettings(Settings: TExpertSettings);
var
  j: Integer;
  i: Integer;
  FileType: TFileType;
  KeyString: string;
  FileTypeSettings: TExpertSettings;
begin
  // Do not localize any of the following lines
  Settings.WriteBool('MatchAnywhere', MatchAnywhere);
  Settings.WriteBool('ShowMapTab', ShowMapTab);
  Settings.WriteString('DefaultFileType', DefaultFileType);
  Settings.WriteInteger('NumberFileTypes', FileTypes.Count);
  Settings.WriteString('IDEOverride', IDEOverride.CommaText);
  Settings.WriteInteger('LastTabIndex', LastTabIndex);
  FileTypeSettings := Settings.CreateExpertSettings('FileTypes');
  try
    for i := 0 to FileTypes.Count - 1 do
    begin
      FileType := FileTypes[i];
      KeyString := IntToStr(i);
      FileTypeSettings.WriteString('FileType' + KeyString, FileType.FileTypeName);
      FileTypeSettings.WriteStrings('Paths' + KeyString, FileType.Paths);
      FileTypeSettings.WriteString('Extensions' + KeyString, FileType.Extensions);
      FileTypeSettings.WriteBool('CustomDirectory' + KeyString, FileType.CustomDirectories);
      FileTypeSettings.WriteBool('RecursiveDirectory' + KeyString, FileType.Recursive);
      FileTypeSettings.WriteInteger('MaxRecentFiles' + KeyString, FileType.MaxRecentFiles);
      FileTypeSettings.WriteStrings('Favorites' + KeyString, FileType.Favorites);
      for j := FileType.RecentFiles.Count - 1 downto FileType.MaxRecentFiles do
        FileType.RecentFiles.Delete(j);
      FileTypeSettings.WriteStrings('RecentFiles' + KeyString, FileType.RecentFiles);
    end;
  finally
    FreeAndNil(FileTypeSettings);
  end;
end;

procedure TOpenFileSettings.SaveToRegistry;
var
  Registry: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  ExpSettings := nil;
  Registry := TGExpertsSettings.Create('');
  try
    ExpSettings := Registry.CreateExpertSettings(ConfigurationKey);
    InternalSaveSettings(Expsettings);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Registry);
  end;
end;

{ TfmOpenFileConfig }

procedure TfmOpenFileConfig.lbxDirectoryListFilesDropped(Sender: TObject; Files: TStrings);
var
  i: integer;
  s: string;
begin
  for i := 0 to Files.Count - 1 do begin
    s := Files[i];
    while (s <> '') and not DirectoryExists(s) do begin
      s := ExtractFileDir(s);
    end;
    if s <> '' then
      EnsureStringInList(lbxDirectoryList.Items, AddSlash(s))
  end;
end;

function TfmOpenFileConfig.GetFileTypes: TFileTypes;
begin
  Assert(Assigned(FSettings));
  Result := FSettings.FileTypes;
end;

function TfmOpenFileConfig.IDEOverride: TIDEOverride;
begin
  Assert(Assigned(FSettings));
  Assert(Assigned(FSettings.IDEOverride));
  Result := FSettings.IDEOverride;
end;

procedure TfmOpenFileConfig.chkCustomDirectoryListClick(Sender: TObject);
begin
  SetEnabledOnControlAndChildren(gbxCustomDirectory, chkCustomDirectoryList.Checked);
  chkCustomDirectoryList.Enabled := True;
  gbxCustomDirectory.Enabled := True;
end;

procedure TfmOpenFileConfig.btnTypeAddClick(Sender: TObject);
var
  GroupName: string;
  FileType: TFileType;
begin
  if InputQuery('File Group Name', 'File Group Name', GroupName) and (GroupName <> '') then
  begin
    if lbxTypeList.Items.IndexOf(GroupName) = -1 then
    begin
      FileType := TFileType.Create(FileTypes);
      FileType.FileTypeName := GroupName;
      lbxTypeList.Items.Add(FileType.FileTypeName);
      lbxTypeList.ItemIndex := lbxTypeList.Items.IndexOf(FileType.FileTypeName);
      lbxTypeList.OnClick(lbxTypeList);
    end;
  end;
end;

procedure TfmOpenFileConfig.lbxTypeListClick(Sender: TObject);
var
  i: Integer;
begin
  SaveConfigValue;
  for i := 0 to FileTypes.Count - 1 do
    if lbxTypeList.Items[lbxTypeList.ItemIndex] = FileTypes[i].FileTypeName then
    begin
      FCurrentFileType := FileTypes[i];
      Break;
    end;
  edtExtension.Text := FCurrentFileType.Extensions;
  chkCustomDirectoryList.Checked := FCurrentFileType.CustomDirectories;
  chkCustomDirectoryListClick(chkCustomDirectoryList);
  lbxDirectoryList.Items.Assign(FCurrentFileType.Paths);
  chkRecursive.Checked := FCurrentFileType.Recursive;
  edtMaxRecentFiles.Text := IntToStr(FCurrentFileType.MaxRecentFiles);
end;

procedure TfmOpenFileConfig.SaveConfigValue;
begin
  if Assigned(FCurrentFileType) then
  begin
    FCurrentFileType.Extensions := edtExtension.Text;
    FCurrentFileType.CustomDirectories := chkCustomDirectoryList.Checked;
    FCurrentFileType.Recursive := chkRecursive.Checked;
    FCurrentFileType.Paths.Assign(lbxDirectoryList.Items);
    FCurrentFileType.MaxRecentFiles := StrToIntDef(edtMaxRecentFiles.Text, DefaultMaxMRU);
  end;
end;

procedure TfmOpenFileConfig.btnOKClick(Sender: TObject);
begin
  if chkOverrideViewUnit.Checked and (cbxViewUnitType.ItemIndex = -1) then
    raise Exception.Create('A default type must be assigned to override View Unit');

  if chkOverrideViewForm.Checked and (cbxViewFormType.ItemIndex = -1) then
    raise Exception.Create('A default type must be assigned to override View Form');

  if chkOverrideOpenProject.Checked and (cbxOpenProjectType.ItemIndex = -1) then
    raise Exception.Create('A default type must be assigned to override Open Project');

  SaveConfigValue;
  SaveSettings(FSettings);
  ModalResult := mrOK;
end;

procedure TfmOpenFileConfig.btnTypeDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if (lbxTypeList.Items.Count > 0) and
    (MessageDlg('Delete selected item?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    for i := 0 to FileTypes.Count - 1 do
      if lbxTypeList.Items[lbxTypeList.ItemIndex] = FileTypes[i].FileTypeName then
      begin
        FileTypes.Delete(i);
        lbxTypeList.Items.Delete(lbxTypeList.ItemIndex);
        FCurrentFileType := nil;
        if lbxTypeList.Items.Count > 0 then
        begin
          lbxTypeList.ItemIndex := 0;
          lbxTypeList.OnClick(lbxTypeList);
        end;
        Break;
      end;
  end;
end;

procedure TfmOpenFileConfig.btnDirectoryClick(Sender: TObject);
begin
  SelectDirectory;
end;

procedure TfmOpenFileConfig.btnDirectoryAddClick(Sender: TObject);

  procedure AddDirectory;
  begin
    EnsureStringInList(lbxDirectoryList.Items, AddSlash(Trim(edtDirectory.Text)))
  end;

begin
  if Trim(edtDirectory.Text) <> '' then
    AddDirectory
  else if SelectDirectory then
    AddDirectory;
end;

procedure TfmOpenFileConfig.btnDirectoryDeleteClick(Sender: TObject);
begin
  if lbxDirectoryList.ItemIndex <> -1 then
    lbxDirectoryList.Items.Delete(lbxDirectoryList.ItemIndex)
  else
    DeleteStringFromList(lbxDirectoryList.Items, edtDirectory.Text);
end;

procedure TfmOpenFileConfig.btnDirectoryReplaceClick(Sender: TObject);
begin
  if lbxDirectoryList.ItemIndex <> -1 then
    lbxDirectoryList.Items[lbxDirectoryList.ItemIndex] := AddSlash(Trim(edtDirectory.Text));
end;

procedure TfmOpenFileConfig.lbxDirectoryListClick(Sender: TObject);
begin
  edtDirectory.Text := lbxDirectoryList.Items[lbxDirectoryList.ItemIndex];
end;

function TfmOpenFileConfig.SelectDirectory: Boolean;
var
  NewDirectory: string;
begin
  NewDirectory := Trim(edtDirectory.Text);
  Result := GetDirectory(NewDirectory);
  if Result then
    edtDirectory.Text := AddSlash(NewDirectory);
end;

class function TfmOpenFileConfig.ExecuteWithSettings(Settings: TOpenFileSettings): Boolean;
var
  Form: TfmOpenFileConfig;
begin
  Assert(Assigned(Settings));
  Form := TfmOpenFileConfig.Create(nil);
  try
    Form.UseSettings(Settings);
    Result := Form.ShowModal = mrOK;
    if Result then
    begin
      Form.SaveSettings(Settings);
      Settings.SaveToRegistry;
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TfmOpenFileConfig.FormCreate(Sender: TObject);
begin
  TControl_SetMinConstraints(Self);

  SetParentBackgroundValue(gbxCustomDirectory, True);
  SetParentBackgroundValue(gbxGeneralSettings, True);
  SetParentBackgroundValue(gbxIDEMenuItems, True);
  TWinControl_ActivateDropFiles(lbxDirectoryList, lbxDirectoryListFilesDropped)
end;

procedure TfmOpenFileConfig.SaveSettings(Settings: TOpenFileSettings);
begin
  Assert(Assigned(Settings));
  Settings.MatchAnywhere := chkMatchAnywhere.Checked;
  Settings.DefaultFileType := cbxDefaultFileTypes.Text;
  Settings.ShowMapTab := chkMapTab.Checked;

  IDEOverride.Clear;
  IDEOverride.OverrideOpenUnit := chkOverrideViewUnit.Checked;
  IDEOverride.OpenUnitDefaultType := cbxViewUnitType.Text;
  IDEOverride.OverrideOpenForm := chkOverrideViewForm.Checked;
  IDEOverride.OpenFormDefaultType := cbxViewFormType.Text;
  IDEOverride.OverrideOpenProject := chkOverrideOpenProject.Checked;
  IDEOverride.OpenProjectDefaultType := cbxOpenProjectType.Text;
end;

procedure TfmOpenFileConfig.UseSettings(Settings: TOpenFileSettings);
var
  i: Integer;
begin
  Assert(Assigned(Settings));
  FSettings := Settings;
  chkMatchAnywhere.Checked := Settings.MatchAnywhere;
  chkMapTab.Checked := Settings.ShowMapTab;

  for i := 0 to FileTypes.Count - 1 do
    lbxTypeList.Items.Add(FileTypes[i].FileTypeName);
  lbxTypeList.ItemIndex := 0;
  lbxTypeList.OnClick(lbxTypeList);

  cbxDefaultFileTypes.Items.Assign(lbxTypeList.Items);
  cbxViewUnitType.Items.Assign(cbxDefaultFileTypes.Items);
  cbxViewFormType.Items.Assign(cbxDefaultFileTypes.Items);
  cbxOpenProjectType.Items.Assign(cbxDefaultFileTypes.Items);

  cbxDefaultFileTypes.ItemIndex := cbxDefaultFileTypes.Items.IndexOf(Settings.DefaultFileType);
  cbxViewUnitType.ItemIndex := cbxViewUnitType.Items.IndexOf(IDEOverride.OpenUnitDefaultType);
  cbxViewFormType.ItemIndex := cbxViewFormType.Items.IndexOf(IDEOverride.OpenFormDefaultType);
  cbxOpenProjectType.ItemIndex := cbxOpenProjectType.Items.IndexOf(IDEOverride.OpenProjectDefaultType);
  chkOverrideViewUnit.Checked := IDEOverride.OverrideOpenUnit;
  chkOverrideViewForm.Checked := IDEOverride.OverrideOpenForm;
  chkOverrideOpenProject.Checked := IDEOverride.OverrideOpenProject;
end;

procedure TfmOpenFileConfig.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 46);
end;

end.

