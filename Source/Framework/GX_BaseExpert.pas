unit GX_BaseExpert;

interface

uses
  Classes, Graphics, GX_ConfigurationInfo;

type
  TGX_BaseExpert = class(TObject)
  private
    FBitmap: TBitmap;
  protected
    FActive: Boolean;
    function GetShortCut: TShortCut; virtual; abstract;
    procedure SetActive(New: Boolean); virtual;
    procedure SetShortCut(Value: TShortCut); virtual; abstract;
    // Return the file name of an icon associated with
    // the expert. Do not specify a path.
    // This bitmap must be included in the
    // GXIcons.rc file which in turn can be created
    // from all .bmp files located in the Images
    // directory by calling the _CreateGXIconsRc.bat
    // script located in that directory.
    // It is possible to return an empty string. This
    // signals that no icon file is available.
    function GetBitmapFileName: string; virtual;
    // Overrride to load any configuration settings
    procedure InternalLoadSettings(Settings: TGExpertsSettings); virtual;
    // Overrride to save any configuration settings
    procedure InternalSaveSettings(Settings: TGExpertsSettings); virtual;
    // do nothing, overridden by TGX_Expert and TEditorExpert because
    // theses settings are "traditionally" stored differently.
    procedure LoadActiveAndShortCut(Settings: TGExpertsSettings); virtual;
    // do nothing, overridden by TGX_Expert and TEditorExpert because
    // theses settings are "traditionally" stored differently.
    procedure SaveActiveAndShortCut(Settings: TGExpertsSettings); virtual;
  public
    // Internal name of expert for expert identification.
    class function GetName: string; virtual;
    destructor Destroy; override;
    function CanHaveShortCut: boolean; virtual; abstract;
    // displays a dialog saying there are no configuration options
    // see also HasConfigOptions
    procedure Configure; virtual;
    procedure Execute(Sender: TObject); virtual; abstract;
    // Get a reference to the bitmap for menu items, buttons, etc.
    // Note that this bitmap is loaded and unloaded on demand;
    // you will have to .Assign the bitmap to get a permanent copy.
    function GetBitmap: Graphics.TBitmap; virtual;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this mithgt be different from
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
    procedure LoadSettings;
    // Creates a Settings object and passes it to
    // the virtual methods
    // SaveActiveAndShortCut and InternalSaveSettings
    // override InternalSaveSettings to actually save the settings.
    procedure SaveSettings;
    // Returns an empty string, overridden by TGX_Expert and TEditorExpert
    // to return the correct values.
    function GetOptionsBaseRegistryKey: string; virtual;
    property Active: Boolean read FActive write SetActive;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Dialogs, GX_GxUtils, GX_MessageBox, GX_IconMessageBox;

{ TGX_BaseExpert }

destructor TGX_BaseExpert.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
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

function TGX_BaseExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TGX_BaseExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  // do nothing
end;

procedure TGX_BaseExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  // do nothing
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
    LoadActiveAndShortCut(Settings);
    InternalLoadSettings(Settings);
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
    SaveActiveAndShortCut(Settings);
    InternalSaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TGX_BaseExpert.SetActive(New: Boolean);
begin
  FActive := New;
end;

end.
