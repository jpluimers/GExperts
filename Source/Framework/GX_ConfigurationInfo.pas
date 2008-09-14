unit GX_ConfigurationInfo;

{$I GX_CondDefine.inc}

interface

uses
  {$IFDEF LINUX}
    IniFiles,
  {$ELSE}
    Registry,
  {$ENDIF}
  Graphics, Classes, TypInfo, Forms, ComCtrls;

type
  IConfigInfo = interface(IUnknown)
    procedure SaveSettings;
    procedure SetAlphabetizeMenu(const Value: Boolean);
    procedure SetConfigPath(const Value: string);
    procedure SetGExpertsPath(const Value: string);
    procedure SetEditorExpertsEnabled(const Value: Boolean);
    procedure SetHelpFileLocation(const Value: string);
    procedure SetVclPath(const Value: string);
    procedure SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
    function GetAlphabetizeMenu: Boolean;
    function GetConfigPath: string;
    function GetGExpertsPath: string;
    function GetEditorExpertsEnabled: Boolean;
    function GetGExpertsIdeRootRegistryKey: string;
    function GetHelpFileLocation: string;
    function GetIdeRootRegistryKey: string;
    function GetVclPath: string;
    function GetPlaceGxMainMenuInToolsMenu: Boolean;
    function GetEnableKeyboardShortcuts: Boolean;

    // Return the IDE's base registry key without a
    // trailing backslash, e.g.
    //    SOFTWARE\Borland\Delphi\6.0
    property IdeRootRegistryKey: string read GetIdeRootRegistryKey;
    // Return the GExperts base registry key within
    // the IDE without a trailing backslash, e.g.
    //    SOFTWARE\Borland\Delphi\6.0\GExperts
    property GExpertsIdeRootRegistryKey: string read GetGExpertsIdeRootRegistryKey;
    // Return the location of the GExperts help file.
    property HelpFile: string read GetHelpFileLocation write SetHelpFileLocation;
    // Return the location of the VCL source code.
    // Path is guaranteed to have a trailing backslash.
    property VCLPath: string read GetVclPath write SetVclPath;
    // Return the path to the configuration files;
    // is guaranteed to have a trailing backslash.
    property ConfigPath: string read GetConfigPath write SetConfigPath;
    // Return whether the editor experts are enabled.
    property EditorExpertsEnabled: Boolean read GetEditorExpertsEnabled write SetEditorExpertsEnabled;
    // Determines whether the entries in the GExperts top level menu
    // are sorted alphabetically or historically.
    property AlphabetizeMenu: Boolean read GetAlphabetizeMenu write SetAlphabetizeMenu;
    // Determine whether the GExperts top-level Menu should be placed
    // as a menu item in the IDE's top-level Tools main menu, or whether
    // it should be a top-level menu of its own.
    property PlaceGxMainMenuInToolsMenu: Boolean read GetPlaceGxMainMenuInToolsMenu write SetPlaceGxMainMenuInToolsMenu;
    property GExpertsPath: string read GetGExpertsPath;
    property EnableKeyboardShortcuts: Boolean read GetEnableKeyboardShortcuts;
  end;

  TGXFontFlag = (ffColor);
  TGXFontFlags = set of TGXFontFlag;

  TFormSaveFlag = (fsSize, fsPosition);
  TFormSaveFlags = set of TFormSaveFlag;

{$IFDEF LINUX}
  TGExpertsBaseSettings = class(TIniFile);
{$ELSE}
  TGExpertsBaseSettings = class(TRegistryIniFile);
{$ENDIF}

  TGExpertsSettings = class(TGExpertsBaseSettings)
  public
    // Save settings of Font to the registry under Section.
    procedure SaveFont(const Section: string; const Font: TFont; Flags: TGXFontFlags = []);
    // From Section, load settings into Font from the registry.
    procedure LoadFont(const Section: string; const Font: TFont; Flags: TGXFontFlags = []);
    // Write List to registry at Section, using Ident as the prefix for the string values
    procedure ReadStrings(const List: TStrings; const Section, Ident: string);
    // Read from registry at Section strings into List, using Ident as the prefix for the string values.
    procedure WriteStrings(const List: TStrings; const Section, Ident: string);
    function ReadEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Default: Longint): Longint;
    procedure WriteEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Value: Longint);
    procedure SaveForm(Form: TCustomForm; Section: string = '';
      FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    procedure LoadForm(Form: TCustomForm; Section: string = '';
      FormSaveFlags: TFormSaveFlags = [fsSize, fsPosition]);
    constructor Create(const FileName: string = '');
  end;

function ConfigInfo: IConfigInfo;

//
// Registry persistence utility functions
//

// Save settings of Font to the registry under Key.
procedure RegSaveFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);

// From Key, load settings into Font from the registry.
procedure RegLoadFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);

// Write List to registry at Key, using SubKey as the prefix for the string values
procedure RegReadStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);

// Read from registry at Key strings into List, using SubKey as the prefix for the string values.
procedure RegWriteStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);

type
  TTreeSaveOption = (stsSelected, stsExpanded);
  TTreeSaveOptions = set of TTreeSaveOption;

const
  TreeSaveAll = [stsSelected, stsExpanded];

// Save/restore the selected and expanded state of a TTreeView
procedure SaveTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; SettingsKey: string);
procedure LoadTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; const SettingsKey: string);

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils,
  GX_EditorEnhancements, GX_MessageBox,
  GX_GenericUtils, GX_GenericClasses, GX_IdeUtils, GX_OtaUtils, GX_VerDepConst;

type
  TConfigInfo = class(TSingletonInterfacedObject, IConfigInfo)
  private
    FVclPath: string;
    FGExpertsPath: string;
    FConfigPath: string;
    FHelpFileLocation: string;
    FIdeRootRegistryKey: string;
    FEditorExpertsEnabled: Boolean;
    FAlphabetizeMenu: Boolean;
    FPlaceGxMainMenuInToolsMenu: Boolean;
    FEnableKeyboardShortcuts: Boolean;
    procedure LoadSettings;
    function DefaultConfigPath: string;
  protected
    // IConfigInfo
    procedure SaveSettings;
    procedure SetAlphabetizeMenu(const Value: Boolean);
    procedure SetConfigPath(const Value: string);
    procedure SetGExpertsPath(const Value: string);
    procedure SetEditorExpertsEnabled(const Value: Boolean);
    procedure SetHelpFileLocation(const Value: string);
    procedure SetVclPath(const Value: string);
    procedure SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
    function GetAlphabetizeMenu: Boolean;
    function GetConfigPath: string;
    function GetEditorExpertsEnabled: Boolean;
    function GetGExpertsIdeRootRegistryKey: string;
    function GetHelpFileLocation: string;
    function GetIdeRootRegistryKey: string;
    function GetVclPath: string;
    function GetGExpertsPath: string;
    function GetPlaceGxMainMenuInToolsMenu: Boolean;
    function ConfigurationKey: string;
    function GetEnableKeyboardShortcuts: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TShowBadDirectoryMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  FPrivateConfigurationInfo: TConfigInfo = nil;

function ConfigInfo: IConfigInfo;
begin
  if not Assigned(FPrivateConfigurationInfo) then
    FPrivateConfigurationInfo := TConfigInfo.Create;

  Result := FPrivateConfigurationInfo;
end;

procedure RegSaveFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);
begin
  Settings.SaveFont(Key, Font);
end;

procedure RegLoadFont(const Settings: TGExpertsSettings; const Key: string; const Font: TFont);
begin
  Settings.LoadFont(Key, Font);
end;

procedure RegReadStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);
begin
  Settings.ReadStrings(List, Key, SubKey);
end;

procedure RegWriteStrings(const Settings: TGExpertsSettings; const List: TStrings; const Key, SubKey: string);
begin
  Settings.WriteStrings(List, Key, SubKey);
end;

// Return a text path to the node passed in
// Format:
//       Parent
//         Child1
//            Child2
// returns
//      Parent|Child1|Child2
//
// This is based on the nodes' text (!) property
// not on the node (component) names.
function GetNodePath(Node: TTreeNode): string;
begin
  Assert(Node <> nil);
  Result := Node.Text;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    Result := Node.Text + '|' + Result;
  end;
end;

procedure SaveTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; SettingsKey: string);
var
  Settings: TGExpertsSettings;
  CurrentNode: TTreeNode;
  i: Cardinal;
begin
  if Tree = nil then Exit;

  if SettingsKey = '' then Exit;

  if (stsSelected in Options) and (Tree.Selected <> nil) then
  begin
    Settings := TGExpertsSettings.Create(SettingsKey);
    try
      {$IFOPT D+} SendDebug('FileMgr: Saving selection ' + GetNodePath(Tree.Selected)); {$ENDIF D-}
      Settings.WriteString(Tree.Name, 'Selected', GetNodePath(Tree.Selected)); // Do not localize.
    finally
      FreeAndNil(Settings);
    end;
  end;

  if (stsExpanded in Options) and (Tree.Items.Count > 0) then
  begin
    SettingsKey := AddSlash(SettingsKey) + Tree.Name;
    Settings := TGExpertsSettings.Create(SettingsKey); // Do not localize.
    try
      Settings.EraseSection('Expanded');

      CurrentNode := Tree.Items.GetFirstNode;
      i := 0;
      while CurrentNode <> nil do
      begin
        if CurrentNode.Expanded then
        begin
          {$IFOPT D+} SendDebug('FileMgr: Writing expanded ' + GetNodePath(CurrentNode)); {$ENDIF}
          Settings.WriteString('Expanded', Format('Value%d', [i]), GetNodePath(CurrentNode));
          Inc(i);
        end;
        CurrentNode := CurrentNode.GetNext;
      end;
    finally
      FreeAndNil(Settings);
    end;
  end;
  {$IFOPT D+} SendDebug('FileMgr: Done writing tree state'); {$ENDIF}
end;

procedure LoadTreeSettings(Tree: TTreeView; Options: TTreeSaveOptions; const SettingsKey: string);

  function FindAtParentLevel(ParentNode: TTreeNode; const NodeName: string): TTreeNode;
  var
    CurrentNode: TTreeNode;
  begin
    Result := nil;

    if ParentNode = nil then
      CurrentNode := Tree.Items.GetFirstNode
    else
      CurrentNode := ParentNode.GetFirstChild;

    while CurrentNode <> nil do
    begin
      if SameText(CurrentNode.Text, NodeName) then
      begin
        Result := CurrentNode;
        Break;
      end;
      CurrentNode := CurrentNode.GetNextSibling;
    end;
  end;

  function GetNode(const NodePath: string): TTreeNode;
  var
    PathList: TStrings;
    CurrentNode: TTreeNode;
    j: Integer;
  begin
    Result := nil;
    PathList := TStringList.Create;
    try
      AnsiStrTok(NodePath, '|', PathList);
      if PathList.Count = 0 then
        Exit;

      CurrentNode := nil;
      for j := 0 to PathList.Count - 2 do
      begin
        CurrentNode := FindAtParentLevel(CurrentNode, PathList[j]);
        if CurrentNode = nil then
          Exit;
      end;
      Result := FindAtParentLevel(CurrentNode, PathList[PathList.Count - 1]);
    finally
      FreeAndNil(PathList);
    end;
  end;

var
  Path: string;
  Settings: TGExpertsSettings;
  FoundNode: TTreeNode;
  i: Integer;
  Expand: TStrings;
begin
  if (Tree = nil) or (Tree.Items.Count <= 0) then Exit;

  if SettingsKey = '' then Exit;

  if stsExpanded in Options then
  begin
    Settings := TGExpertsSettings.Create(AddSlash(SettingsKey) + Tree.Name);
    try
      Expand := TStringList.Create;
      try
        Settings.ReadSectionValues('Expanded', Expand);
        for i := 0 to Expand.Count - 1 do
        begin
          {$IFOPT D+} SendDebug('FileMgr: Getting for expansion ' + Expand[i]); {$ENDIF}
          FoundNode := GetNode(Expand[i]);
          if FoundNode <> nil then
            FoundNode.Expand(False);
        end;
      finally
        FreeAndNil(Expand);
      end;
    finally
      FreeAndNil(Settings);
    end;
  end;

  if stsSelected in Options then
  begin
    Settings := TGExpertsSettings.Create(SettingsKey);
    try
      Path := Settings.ReadString(Tree.Name, 'Selected', ''); // Do not localize.
      {$IFOPT D+} SendDebug('FileMgr: Restoring selection ' + Path); {$ENDIF}
      FoundNode := GetNode(Path);
      if FoundNode <> nil then
        Tree.Selected := FoundNode;
    finally
      FreeAndNil(Settings);
    end;
  end;
end;

{ TConfigInfo }

constructor TConfigInfo.Create;
begin
  // Don't do anything significant here, because this gets recreated many times
  {$IFOPT D+} SendDebug('Creating configuration info'); {$ENDIF D+}
  inherited Create;
  FPrivateConfigurationInfo := Self;

  FIdeRootRegistryKey := GxOtaGetIdeBaseRegistryKey;
  FVclPath := AddSlash(GetIdeRootDirectory) +
              AddSlash('Source') +
              {$IFDEF GX_VER170_up} AddSlash('Win32') + {$ENDIF}
              AddSlash('VCL'); // Do not localize.

  if RunningLinux then
  begin
    FGExpertsPath := AddSlash(GetGExpertsIdeRootRegistryKey);
    FConfigPath := AddSlash(GetGExpertsIdeRootRegistryKey);
  end
  else // Windows
  begin
    FGExpertsPath := AddSlash(ExtractFilePath(ThisDllName));
    FConfigPath := DefaultConfigPath;
  end;

  EditorEnhancements.Enabled := False;

  LoadSettings;
  ShowGxMessageBox(TShowBadDirectoryMessage, FConfigPath);
end;

function TConfigInfo.DefaultConfigPath: string;
begin
  Result := AddSlash(GetUserApplicationDataFolder) + AddSlash('GExperts') + IDEEnglishName;
end;

destructor TConfigInfo.Destroy;
begin
  {$IFOPT D+} SendDebug('TConfigInfo.Destroy'); {$ENDIF D+}
  //SaveSettings; // Call this below to prevent re-creating TConfigInfo
  FreeEditorEnhancements;

  inherited Destroy;
end;

procedure TConfigInfo.LoadSettings;
var
  Settings: TGExpertsSettings;
  Setting: Boolean;
begin
  {$IFOPT D+} SendDebug('Loading configuration info settings'); {$ENDIF D+}

  // Do not localize any of the following strings.
  Settings := TGExpertsSettings.Create;
  try
    FVclPath := AddSlash(Settings.ReadString(ConfigurationKey, 'VCLPath', FVclPath));
    FConfigPath := AddSlash(Settings.ReadString(ConfigurationKey, 'ConfigPath', FConfigPath));
    FHelpFileLocation := Settings.ReadString(ConfigurationKey, 'HelpFile', FGExpertsPath + 'GExperts.chm');
    if SameText(ExtractFileExt(FHelpFileLocation), '.hlp') then
      FHelpFileLocation := ChangeFileExt(FHelpFileLocation, '.chm');
    FAlphabetizeMenu := Settings.ReadBool(ConfigurationKey, 'AlphabetizeMenu', True);
    FEditorExpertsEnabled := Settings.ReadBool(ConfigurationKey, 'EditorExpertsEnabled', True);
    FPlaceGxMainMenuInToolsMenu := Settings.ReadBool(ConfigurationKey, 'PlaceGxMainMenuInToolsMenu', False);
    FEnableKeyboardShortcuts := Settings.ReadBool(ConfigurationKey, 'EnableKeyboardShortcuts', True);

    Setting := Settings.ReadBool(ConfigurationKey, 'EditorEnhancementsEnabled', False);
    EditorEnhancements.Enabled := Setting and not IsStandAlone;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TConfigInfo.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  if IsStandAlone then
    Exit;

  // Do not localize any of the following strings.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteString(ConfigurationKey, 'VCLPath', FVclPath);
    Settings.WriteString(ConfigurationKey, 'ConfigPath', FConfigPath);
    Settings.WriteString(ConfigurationKey, 'HelpFile', FHelpFileLocation);
    Settings.WriteBool(ConfigurationKey, 'AlphabetizeMenu', FAlphabetizeMenu);
    Settings.WriteBool(ConfigurationKey, 'EditorExpertsEnabled', FEditorExpertsEnabled);
    Settings.WriteBool(ConfigurationKey, 'PlaceGxMainMenuInToolsMenu', FPlaceGxMainMenuInToolsMenu);
    Settings.WriteBool(ConfigurationKey, 'EditorEnhancementsEnabled', EditorEnhancements.Enabled);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TConfigInfo.SetVclPath(const Value: string);
begin
  FVclPath := AddSlash(Value);
end;

function TConfigInfo.GetGExpertsIdeRootRegistryKey: string;
const
  SGExpertsString = 'GExperts-1.3';
begin
  if RunningLinux then
    Result := AddSlash(ExpandFileName('~/.borland')) + SGExpertsString
  else
    Result := AddSlash(FIdeRootRegistryKey) + SGExpertsString;
end;

procedure TConfigInfo.SetConfigPath(const Value: string);
begin
  FConfigPath := AddSlash(Value);
end;

function TConfigInfo.GetAlphabetizeMenu: Boolean;
begin
  Assert(not IsStandAlone);

  Result := FAlphabetizeMenu;
end;

function TConfigInfo.GetConfigPath: string;
begin
  Result := FConfigPath;
end;

function TConfigInfo.GetEditorExpertsEnabled: Boolean;
begin
  Assert(not IsStandAlone);

  Result := FEditorExpertsEnabled;
end;

function TConfigInfo.GetHelpFileLocation: string;
begin
  Result := FHelpFileLocation;
end;

function TConfigInfo.GetIdeRootRegistryKey: string;
begin
  Result := FIdeRootRegistryKey;
end;

function TConfigInfo.GetVclPath: string;
begin
  Result := FVclPath;
end;

procedure TConfigInfo.SetAlphabetizeMenu(const Value: Boolean);
begin
  Assert(not IsStandAlone);

  FAlphabetizeMenu := Value;
end;

procedure TConfigInfo.SetEditorExpertsEnabled(const Value: Boolean);
begin
  Assert(not IsStandAlone);

  FEditorExpertsEnabled := Value;
end;

procedure TConfigInfo.SetHelpFileLocation(const Value: string);
begin
  FHelpFileLocation := Value;
end;

function TConfigInfo.GetPlaceGxMainMenuInToolsMenu: Boolean;
begin
  Result := FPlaceGxMainMenuInToolsMenu;
end;

procedure TConfigInfo.SetPlaceGxMainMenuInToolsMenu(const Value: Boolean);
begin
  FPlaceGxMainMenuInToolsMenu := Value;
end;

function TConfigInfo.GetGExpertsPath: string;
begin
  Result := ExtractFilePath(FGExpertsPath);
end;

procedure TConfigInfo.SetGExpertsPath(const Value: string);
begin
  FGExpertsPath := AddSlash(Value);
end;

function TConfigInfo.ConfigurationKey: string;
begin
  Result := 'Misc';
end;

function TConfigInfo.GetEnableKeyboardShortcuts: Boolean;
begin
  Result := FEnableKeyboardShortcuts;
end;

{ TShowBadDirectoryMessage }

function TShowBadDirectoryMessage.GetMessage: string;
resourcestring
  SBadConfigPath =
    'The storage directory defined in the GExperts configuration dialog does ' +
    'not exist or is read-only. Under Windows Vista or later, this may be due ' +
    'to being a limited user and not having write access to this directory. ' +
    'Please correct the problem or you may find that some GExperts data files ' +
    'are not being saved.' + sLineBreak +
    'Current Directory: %s' + sLineBreak +
    'Suggested Directory: %s';
begin
  Assert(Assigned(FPrivateConfigurationInfo));
  Result := Format(SBadConfigPath, [FData, FPrivateConfigurationInfo.DefaultConfigPath]);
end;

var
  ShownOnce: Boolean = False;

function TShowBadDirectoryMessage.ShouldShow: Boolean;
begin
  Result := not ShownOnce;
  // The directory must exist and be writeable to be valid.
  if Result then
    Result := not PrepareDirectoryForWriting(ConfigInfo.ConfigPath);
  ShownOnce := True;
end;

const
  FontNameIdent = 'Name'; // Do not localize.
  FontSizeIdent = 'Size'; // Do not localize.
  FontColorIdent = 'Color'; // Do not localize.
  FontStyleBoldIdent = 'Bold'; // Do not localize.
  FontStyleItalicIdent = 'Italic'; // Do not localize.
  FontStyleUnderlineIdent = 'Underline'; // Do not localize.
  CountIdent = 'Count'; // Do not localize.

{ TGExpertsSettings }

constructor TGExpertsSettings.Create(const FileName: string);
var
  UseFileName: string;
  UseFileDir: string;
begin
  if RunningLinux then
  begin
    if FileExists(FileName) and not DirectoryExists(FileName) then
      UseFileName := FileName
    else
    begin
      if FileName = '' then
        UseFileName := ConfigInfo.GExpertsIdeRootRegistryKey
      else
        UseFileName := FileName;
      UseFileName := AddSlash(UseFileName) + 'GExpertsRegistry.txt';

      UseFileDir := ExtractFileDir(UseFileName);
      if not DirectoryExists(UseFileDir) then
        ForceDirectories(UseFileDir);
      {$IFOPT D+} SendDebug('Settings FileName: '+ UseFileName); {$ENDIF}
    end;
    inherited Create(UseFileName);
  end
  else // Windows
  begin
    if FileName = '' then
      inherited Create(ConfigInfo.GExpertsIdeRootRegistryKey)
    else
      inherited Create(FileName);
  end;
end;

procedure TGExpertsSettings.SaveFont(const Section: string; const Font: TFont; Flags: TGXFontFlags);
begin
  Assert(Assigned(Font), 'nil font in TGExpertsSettings.SaveFont');
  WriteString(Section, FontNameIdent, Font.Name);
  WriteInteger(Section, FontSizeIdent, Font.Size);
  WriteBool(Section, FontStyleBoldIdent, (fsBold in Font.Style));
  WriteBool(Section, FontStyleItalicIdent, (fsItalic in Font.Style));
  WriteBool(Section, FontStyleUnderlineIdent, (fsUnderline in Font.Style));
  if ffColor in Flags then
    WriteInteger(Section, FontColorIdent, Font.Color);
end;

procedure TGExpertsSettings.LoadFont(const Section: string; const Font: TFont; Flags: TGXFontFlags);
begin
  Assert(Assigned(Font),  'nil font in TGExpertsSettings.LoadFont');
  Font.Name := ReadString(Section, FontNameIdent, Font.Name);
  Font.Size := ReadInteger(Section, FontSizeIdent, Font.Size);
  if ReadBool(Section, FontStyleBoldIdent, (fsBold in Font.Style)) then
    Font.Style := Font.Style + [fsBold];
  if ReadBool(Section, FontStyleItalicIdent, (fsItalic in Font.Style)) then
    Font.Style := Font.Style + [fsItalic];
  if ReadBool(Section, FontStyleUnderlineIdent, (fsUnderline in Font.Style)) then
    Font.Style := Font.Style + [fsUnderline];
  if ffColor in Flags then
    Font.Color := ReadInteger(Section, FontColorIdent, Font.Color);
end;

procedure TGExpertsSettings.ReadStrings(const List: TStrings; const Section, Ident: string);
var
  i: Integer;
  RegistryValueCount: Integer;
  ListString: string;
  Identifier: string;
begin
  Assert(Assigned(List));
  RegistryValueCount := Max(0, ReadInteger(Section, CountIdent, 0));
  for i := 0 to RegistryValueCount - 1 do
  begin
    Identifier := Ident + IntToStr(i);
    if ValueExists(Section, Identifier) then
    begin
      ListString := ReadString(Section, Identifier, '');
      List.Add(ListString);
    end
    {$IFOPT D+}
    else SendDebugError('The Count value for ' +Section+ ':' +Ident+ ' is invalid');
    {$ENDIF}
  end;
end;

procedure TGExpertsSettings.WriteStrings(const List: TStrings; const Section, Ident: string);
var
  i: Integer;
begin
  Assert(Assigned(List), 'nil string list in TGExpertsSettings.WriteStrings');
  EraseSection(Section);
  // Assume brutally that a write will fail.
  WriteInteger(Section, CountIdent, 0);
  for i := 0 to List.Count - 1 do
  begin
    // We do never run into a conflict with the "Count" value,
    // as the Ident always gets a number appended.
    WriteString(Section, Format('%s%d', [Ident, i]), List.Strings[i]);
  end;
  // Record the amount of values written.
  WriteInteger(Section, CountIdent, List.Count);
end;

function TGExpertsSettings.ReadEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Default: Longint): Longint;
var
  TypeData: PTypeData;
begin
  Assert(TypeInfo^.Kind = tkEnumeration, 'Non-enumerated type passed to ReadEnumerated');
  TypeData := GetTypeData(TypeInfo);
  Assert(Default in [TypeData^.MinValue..TypeData^.MaxValue], 'Bad default value in ReadEnumerated');
  Result := ReadInteger(Section, Ident, Default);
  if not (Result in [TypeData^.MinValue..TypeData^.MaxValue]) then
  begin
    {$IFOPT D+} SendDebugError('Read in an invalid value for ' + Section + Ident); {$ENDIF D+}
    Result := Default;
  end;
end;

procedure TGExpertsSettings.WriteEnumerated(const Section, Ident: string; TypeInfo: PTypeInfo; Value: Longint);
begin
  Assert(TypeInfo^.Kind = tkEnumeration, 'Non-enumerated type passed to WriteEnumerated');
  Assert(Value in [GetTypeData(TypeInfo)^.MinValue..GetTypeData(TypeInfo)^.MaxValue], 'Bad default value in WriteEnumerated');
  WriteInteger(Section, Ident, Value);
end;

procedure TGExpertsSettings.LoadForm(Form: TCustomForm; Section: string; FormSaveFlags: TFormSaveFlags);
var
  StorageSection: string;
begin
  if Section = '' then
    StorageSection := Form.ClassName
  else
    StorageSection := Section;
  if fsPosition in FormSaveFlags then
  begin
    Form.Left := ReadInteger(StorageSection, 'Left', Form.Left);
    Form.Top := ReadInteger(StorageSection, 'Top', Form.Top);
  end;
  if fsSize in FormSaveFlags then
  begin
    Form.Width := ReadInteger(StorageSection, 'Width', Form.Width);
    Form.Height := ReadInteger(StorageSection, 'Height', Form.Height);
  end;
end;

procedure TGExpertsSettings.SaveForm(Form: TCustomForm; Section: string;
  FormSaveFlags: TFormSaveFlags);
var
  StorageSection: string;
begin
  if Section = '' then
    StorageSection := Form.ClassName
  else
    StorageSection := Section;
  if fsPosition in FormSaveFlags then
  begin
    WriteInteger(StorageSection, 'Left', Form.Left);
    WriteInteger(StorageSection, 'Top', Form.Top);
  end;
  if fsSize in FormSaveFlags then
  begin
    WriteInteger(StorageSection, 'Width', Form.Width);
    WriteInteger(StorageSection, 'Height', Form.Height);
  end;
end;

procedure FinalizeConfigurationInfo;
begin
  if Assigned(FPrivateConfigurationInfo) then
    FPrivateConfigurationInfo.SaveSettings;
  FreeAndNil(FPrivateConfigurationInfo);
end;

initialization
  FPrivateConfigurationInfo := nil;

finalization
  FinalizeConfigurationInfo;

end.

