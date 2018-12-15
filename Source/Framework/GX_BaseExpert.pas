unit GX_BaseExpert;

interface

uses
  Classes, Graphics, GX_ConfigurationInfo;

type
  TGX_BaseExpert = class(TObject)
  private
    FCallCount: Integer;
    function GetTotalCallCount: Integer;
    procedure SetTotalCallCount(Value: Integer);
  protected
    FBitmap: TBitmap;
    FActive: Boolean;
    function GetShortCut: TShortCut; virtual; abstract;
    procedure SetActive(New: Boolean); virtual;
    procedure SetShortCut(Value: TShortCut); virtual; abstract;
    // Subkey used to store configuration details in the registry
    // defaults to GetName
    class function ConfigurationKey: string; virtual;
    // Return the file name of an icon associated with
    // the expert. Do not specify a path.
    // This bitmap must be included in the
    // GXIcons.rc file which in turn can be created
    // from all .bmp files located in the Images
    // directory by calling the _CreateGXIconsRc.bat
    // script located in that directory.
    // It is possible to return an empty string. This
    // signals that no icon file is available.
    // Defaults to GetName.
    function GetBitmapFileName: string; virtual;
    // Overrride to load any configuration settings
    procedure InternalLoadSettings(Settings: TExpertSettings); overload; virtual;
    procedure InternalLoadSettings(_GxSettings: TGExpertsSettings); overload;
    // Overrride to save any configuration settings
    procedure InternalSaveSettings(Settings: TExpertSettings); overload; virtual;
    procedure InternalSaveSettings(_GxSettings: TGExpertsSettings); overload;
    // do nothing, overridden by TGX_Expert and TEditorExpert because
    // theses settings are "traditionally" stored differently.
    procedure LoadActiveAndShortCut(Settings: TGExpertsSettings); virtual;
    // do nothing, overridden by TGX_Expert and TEditorExpert because
    // theses settings are "traditionally" stored differently.
    procedure SaveActiveAndShortCut(Settings: TGExpertsSettings); virtual;
  public
    // Internal name of expert for expert identification.
    class function GetName: string; virtual;
    constructor Create;
    destructor Destroy; override;
    function CanHaveShortCut: boolean; virtual; abstract;
    // displays a dialog saying there are no configuration options
    // see also HasConfigOptions
    procedure Configure; virtual;
    // @returns true, if the expert maintains a call count. Most experts return true, with
    // the notable exception of the Grep Search expert because it's calls are counted
    // by the Grep Results expert.
    function HasCallCount: Boolean; virtual;
    procedure IncCallCount;
    procedure Execute(Sender: TObject); virtual; abstract;
    // Get a reference to the bitmap for menu items, buttons, etc.
    // Note that this bitmap is loaded and unloaded on demand;
    // you will have to .Assign the bitmap to get a permanent copy.
    function GetBitmap: Graphics.TBitmap; virtual;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this might be different from
    // the action caption (GetActionCaption) but is the default
    // for that as well.
    function GetDisplayName: string; virtual;
    // Describes the purpose of the expert. Displayed
    // as hint if the user points the mouse on the expert's icon
    // in the configuration dialog.
    function GetHelpString: string; virtual;
    // Returns 0, meaning: No default shortcut
    function GetDefaultShortCut: TShortCut; virtual;
    // Returns true, see also Configure
    function HasConfigOptions: Boolean; virtual;
    // Creates a Settings object and passes it to
    // the virtual methods
    // LoadActiveAndShortCut and InternalLoadSettings
    // override InternalLoadSettings to actually load the settings.
    procedure LoadSettings; overload;
    procedure LoadSettings(_GxSettings: TGExpertsSettings); overload;
    // Creates a Settings object and passes it to
    // the virtual methods
    // SaveActiveAndShortCut and InternalSaveSettings
    // override InternalSaveSettings to actually save the settings.
    procedure SaveSettings; overload;
    procedure SaveSettings(_GxSettings: TGExpertsSettings); overload;
    // Returns an empty string, overridden by TGX_Expert and TEditorExpert
    // to return the correct values.
    function GetOptionsBaseRegistryKey: string; virtual;
    // Defaults to True
    function IsDefaultActive: Boolean; virtual;
    procedure ClearCallCounts;
    property Active: Boolean read FActive write SetActive;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property CallCount: Integer read FCallCount;
    property TotalCallCount: Integer read GetTotalCallCount;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Dialogs, GX_GxUtils, GX_MessageBox, GX_IconMessageBox;

{ TGX_BaseExpert }

constructor TGX_BaseExpert.Create;
begin
  inherited Create;
  FCallCount := 0;
end;

destructor TGX_BaseExpert.Destroy;
begin
  FreeAndNil(FBitmap);
  try
    SetTotalCallCount(GetTotalCallCount + FCallCount);
  except
    // ignore exceptions in the destructor
  end;
  inherited;
end;

procedure TGX_BaseExpert.ClearCallCounts;
begin
  SetTotalCallCount(0);
  FCallCount := 0;
end;

class function TGX_BaseExpert.ConfigurationKey: string;
begin
  Result := GetName;
end;

procedure TGX_BaseExpert.Configure;
resourcestring
  SNoConfigurationOptions = 'There are no configuration options for this expert.';
begin
  MessageDlg(SNoConfigurationOptions, mtInformation, [mbOK], 0);
end;

function TGX_BaseExpert.GetBitmap: Graphics.TBitmap;
var
  BitmapFile: string;
begin
  if not Assigned(FBitmap) then
  begin
    // Locate and load the bitmap for the expert if it exists.
    BitmapFile := GetBitmapFileName;
    if BitmapFile <> '' then
    begin
      if not GxLoadBitmapForExpert(BitmapFile, FBitmap) then
      begin
        {$IFOPT D+} SendDebugError('Missing bitmap ' + BitmapFile + ' for ' + Self.ClassName); {$ENDIF}
        ShowGxMessageBox(TShowMissingIconMessage, ChangeFileExt(BitmapFile, '.bmp'));
      end;
    end
    else
    begin
      {$IFOPT D+} SendDebugError('Bitmap missing for expert ' + Self.ClassName); {$ENDIF D+}
    end;
  end;

  Result := FBitmap;
end;

function TGX_BaseExpert.GetBitmapFileName: string;
begin
  Result := GetName;
end;

function TGX_BaseExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
end;

function TGX_BaseExpert.GetDisplayName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a human-readable name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

function TGX_BaseExpert.GetHelpString: string;
begin
  Result := '';
end;

class function TGX_BaseExpert.GetName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a Name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

function TGX_BaseExpert.GetOptionsBaseRegistryKey: string;
begin
  Result := '';
end;

function TGX_BaseExpert.HasCallCount: Boolean;
begin
  Result := True;
end;

function TGX_BaseExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TGX_BaseExpert.IncCallCount;
begin
  Inc(FCallCount);
end;

procedure TGX_BaseExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  // do nothing
end;

procedure TGX_BaseExpert.InternalLoadSettings(_GxSettings: TGExpertsSettings);
var
  ExpSettings: TExpertSettings;
begin
  ExpSettings := _GxSettings.CreateExpertSettings(ConfigurationKey);
  try
    InternalLoadSettings(ExpSettings);
  finally
    FreeAndNil(ExpSettings);
  end;
end;

procedure TGX_BaseExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  // do nothing
end;

procedure TGX_BaseExpert.InternalSaveSettings(_GxSettings: TGExpertsSettings);
var
  ExpSettings: TExpertSettings;
begin
  ExpSettings := _GxSettings.CreateExpertSettings(ConfigurationKey);
  try
    InternalSaveSettings(ExpSettings);
  finally
    FreeAndNil(ExpSettings);
  end;
end;

function TGX_BaseExpert.IsDefaultActive: Boolean;
begin
  Result := True;
end;

procedure TGX_BaseExpert.LoadActiveAndShortCut(Settings: TGExpertsSettings);
begin
  // do nothing here, overridden by TGX_Expert and TEditorExpert because
  // theses settings are "traditionally" stored differently.
end;

procedure TGX_BaseExpert.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create(GetOptionsBaseRegistryKey);
  try
    LoadSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_BaseExpert.LoadSettings(_GxSettings: TGExpertsSettings);
begin
  LoadActiveAndShortCut(_GxSettings);
  InternalLoadSettings(_GxSettings);
end;

function TGX_BaseExpert.GetTotalCallCount: Integer;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  Settings := TGExpertsSettings.Create(GetOptionsBaseRegistryKey);
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    try
      Result := ExpSettings.ReadInteger('TotalCallCount', 0);
    finally
      FreeAndNil(ExpSettings);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_BaseExpert.SaveActiveAndShortCut(Settings: TGExpertsSettings);
begin
  // do nothing here, overridden by TGX_Expert and TEditorExpert because
  // theses settings are "traditionally" stored differently.
end;

procedure TGX_BaseExpert.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create(GetOptionsBaseRegistryKey);
  try
    SaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_BaseExpert.SaveSettings(_GxSettings: TGExpertsSettings);
begin
  SaveActiveAndShortCut(_GxSettings);
  InternalSaveSettings(_GxSettings);
end;

procedure TGX_BaseExpert.SetTotalCallCount(Value: Integer);
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  Settings := TGExpertsSettings.Create(GetOptionsBaseRegistryKey);
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    try
      ExpSettings.WriteInteger('TotalCallCount', Value);
    finally
      FreeAndNil(ExpSettings);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_BaseExpert.SetActive(New: Boolean);
begin
  FActive := New;
end;

end.
