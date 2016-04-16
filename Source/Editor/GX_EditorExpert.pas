unit GX_EditorExpert;

interface

uses
  Classes, Graphics,
  GX_Actions, GX_ConfigurationInfo, GX_BaseExpert;

type
  TEditorExpert = class(TGX_BaseExpert)
  private
    FGxAction: IGxAction;
    FActionName: string;
    function GetOptionsBaseRegistryKey: string;
  protected
    function GetShortCut: TShortCut; override;
    procedure SetShortCut(Value: TShortCut); override;
    // Return a string that will be used to present
    // the editor expert to the user, for instance
    // in configuration or selection dialogs.
//    function GetDisplayName: string; virtual; // declared in TGX_BaseExpert
    // defaults to GetName
    class function ConfigurationKey: string; virtual;
    // defaults to ClassName
    function GetBitmapFileName: string; override;
    // Overrride to load any configuration settings
    procedure InternalLoadSettings(Settings: TGExpertsSettings); virtual;
    // Overrride to save any configuration settings
    procedure InternalSaveSettings(Settings: TGExpertsSettings); virtual;
    procedure ActionOnUpdate(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Configure; virtual;
    procedure DoExecute(Sender: TObject);
//    procedure Execute(Sender: TObject); virtual;  // declared in TGX_BaseExpert
    procedure GetHelpString(List: TStrings); virtual;
    function HasConfigOptions: Boolean; virtual;
    function GetActionName: string;

    // Return a string that names the editor expert.
    // This string will be used to construct an action
    // name and therefore must be a valid identifier
    // name.
//    class function GetName: string; virtual; // declared in TGX_BaseExpert

    // Override the InternalXXX versions in descendents to get a Settings object
    procedure LoadSettings;
    procedure SaveSettings;

    property OptionsBaseRegistryKey: string read GetOptionsBaseRegistryKey;
    property DisplayName: string read GetDisplayName;
  end;

  TEditorExpertClass = class of TEditorExpert;

function EditorExpertClassList: TList;

procedure RegisterEditorExpert(AClass: TEditorExpertClass);
function GetExpertClass(const ClassName: string): TEditorExpertClass;
function GetExpertClassByIndex(const Index: Integer): TEditorExpertClass;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Dialogs, ActnList,
  GX_ActionBroker, GX_OtaUtils, GX_GxUtils, GX_GenericUtils,
  GX_MessageBox, GX_IconMessageBox;

{ Global utility functions }

procedure RegisterEditorExpert(AClass: TEditorExpertClass);
var
  ClassName: string;
begin
  Assert(Assigned(AClass));
  ClassName := AClass.ClassName;
  if GetExpertClass(ClassName) <> nil then
  begin
    {raise EFilerError.CreateFmt(SDuplicateClass, [ClassName]);}
    Exit;
  end;
  EditorExpertClassList.Add(AClass);
end;

function GetExpertClass(const ClassName: string): TEditorExpertClass;
var
  I: Integer;
begin
  Assert(Length(ClassName) > 0);

  for I := 0 to EditorExpertClassList.Count - 1 do
  begin
    Result := EditorExpertClassList[I];
    if Result.ClassNameIs(ClassName) then
      Exit;
  end;
  Result := nil;
end;

function GetExpertClassByIndex(const Index: Integer): TEditorExpertClass;
begin
  Assert((Index >= 0) and (Index <= EditorExpertClassList.Count - 1));

  Result := EditorExpertClassList[Index];
end;

{ TEditorExpert }

procedure TEditorExpert.Configure;
resourcestring
  SNoConfigurationOptions = 'There are no configuration options for this expert.';
begin
  MessageDlg(SNoConfigurationOptions, mtInformation, [mbOK], 0);
end;

constructor TEditorExpert.Create;
const
  EditorExpertPrefix = 'EditorExpert'; // Do not localize.
begin
  inherited Create;

  Assert(IsValidIdent(GetName),
    Format('%s needs to specify a valid name; currently it is "%s"', [Self.ClassName, GetName]));

  FActionName := EditorExpertPrefix + GetName;

  FGxAction := GxActionBroker.RequestAction(FActionName, GetBitmap);
  FGxAction.OnExecute := Self.DoExecute;
  FGxAction.Caption := DisplayName;
  FGxAction.OnUpdate := ActionOnUpdate;

  ShortCut := GetDefaultShortCut;
end;

destructor TEditorExpert.Destroy;
begin
  FGxAction := nil; // Clear out interface reference.

  inherited Destroy;
end;

procedure TEditorExpert.GetHelpString(List: TStrings);
resourcestring
  SNoHelpAvailable = 'No help available.';
begin
  Assert(Assigned(List));
  List.Text := SNoHelpAvailable;
end;

function TEditorExpert.GetOptionsBaseRegistryKey: string;
begin
  Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + 'EditorExperts'; // Do not localize.
end;

function TEditorExpert.GetShortCut: TShortCut;
begin
  Assert(Assigned(FGxAction));
  Result := FGxAction.ShortCut
end;

function TEditorExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TEditorExpert.InternalLoadSettings;
begin
  ShortCut := Settings.ReadInteger(GetName, 'ShortCut', ShortCut);
end;

procedure TEditorExpert.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create(OptionsBaseRegistryKey);
  try
    InternalLoadSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEditorExpert.InternalSaveSettings;
begin
  Settings.WriteInteger(ConfigurationKey, 'ShortCut', ShortCut);
end;

procedure TEditorExpert.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create(OptionsBaseRegistryKey);
  try
    InternalSaveSettings(Settings);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TEditorExpert.SetShortCut(Value: TShortCut);
begin
  Assert(Assigned(FGxAction));
  FGxAction.ShortCut := Value;
end;

class function TEditorExpert.ConfigurationKey: string;
begin
  Result := GetName;
end;

var
  PrivateEditorExpertClassList: TList;

function EditorExpertClassList: TList;
begin
  Result := PrivateEditorExpertClassList;
end;

function TEditorExpert.GetBitmapFileName: string;
begin
  Result := ClassName;
end;

function TEditorExpert.GetActionName: string;
begin
  Result := GExpertsActionCategory + GxGenericActionQualifier + FActionName;
end;

procedure TEditorExpert.ActionOnUpdate(Sender: TObject);
var
  SendingAction: TCustomAction;
begin
  // All editor experts require a current edit view
  SendingAction := Sender as TCustomAction;
  Assert(Assigned(SendingAction));

  SendingAction.Enabled := (GxOtaGetTopMostEditView <> nil);
end;

procedure TEditorExpert.DoExecute(Sender: TObject);
begin
  try
    Execute(Sender);
  except
    // Trap exceptions because the editor will continue to insert the pressed
    // shortcut character into the editor (in D6 at least)
    on E: Exception do
      ShowError(E.Message);
  end;
end;

initialization
  PrivateEditorExpertClassList := TList.Create;

finalization
  FreeAndNil(PrivateEditorExpertClassList);

end.

