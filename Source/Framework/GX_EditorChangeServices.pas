unit GX_EditorChangeServices;

{
  The basic goal of this unit is to flatten the
  nice hierarchical structure of what the OTA
  has to offer.

  Flattening the hierarchy has the advantage of
  centralising all "attach" and "detach" code
  into one closed part of the code. It also
  makes it easier for clients to receive
  notification by drastically reducing
  admininistrative overhead.
}

interface

{$I GX_CondDefine.inc}

{$UNDEF UseInternalTestClient}
{.$DEFINE UseInternalTestClient}

uses
  ToolsAPI,
  GX_ConfigurationInfo; // needed only for STARTUP_LAYOUT_FIX_ENABLED

type
  { Implement the IGxEditorNotification interface to learn of
    interesting events that happen deep down inside the IDE.
    The notifier is installed via GxEditorChangeServices. }
  IGxEditorNotification = interface(IInterface)
    ['{EF8D8061-F41E-11D3-A7CF-B41D01000000}']
    // A new module has been opened in the IDE
    procedure NewModuleOpened(const Module: IOTAModule);
    // The editor buffer SourceEditor has been modified.
    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    // The form buffer FormEditor has been modified.
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    // A component on a form was renamed
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: string);
    // A key was pressed inside the source editor
    function EditorKeyPressed(const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
    // The index that is used to unregister this notifier from GExperts
    function GetIndex: Integer;
  end;

type
  IGxEditorChangeServices = interface(IInterface)
    ['{AB0A1721-F1BD-11D3-A7C6-0E7655000000}']
    // Installs a notifier to receive change services; returns
    // an index >= 0 to be used for removing the notifier.
    function AddNotifier(Notifier: IGxEditorNotification): Integer;
    // Removes an installed notifier; use the index returned by
    // AddNotifier to identify the notifier to be removed.
    procedure RemoveNotifierIfNecessary(var Index: Integer);
  end;

function GxEditorChangeServices: IGxEditorChangeServices;
procedure ReleaseEditorChangeServices;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, TypInfo, {$ENDIF}
  SysUtils, Windows, Classes, Messages, Controls, Forms,
  StdCtrls, // needed only for STARTUP_LAYOUT_FIX_ENABLED
  GX_GenericUtils, GX_GenericClasses, GX_IdeUtils, GX_OtaUtils;

type
  TInternalIdeNotification = (iinNewModule, iinSourceEditorModified, iinFormEditorModified, iinEditorKeyPressed);

{$IFOPT D+}
var
  TEditorChangeServicesCount: Integer = 0;
{$ENDIF D+}

type
  TEditorChangeServices = class(TSingletonInterfacedObject, IGxEditorChangeServices)
  private
    FNotifiers: TInterfaceList;
    FNextIndex: Integer;
    procedure ClearNotifiers;
  private
    FIdeNotifierIndex: Integer;
    procedure InstallInternalIdeNotifier;
    procedure RemoveInternalIdeNotifier;

  private
    procedure GenericInternalIdeNotification(const NotifyType: TInternalIdeNotification;
      const GenericParameter: IInterface; CharCode: Word = 0; KeyData: Integer = 0);
  public
    constructor Create;
    destructor Destroy; override;

    function AddNotifier(Notifier: IGxEditorNotification): Integer;
    procedure RemoveNotifierIfNecessary(var Index: Integer);
  end;

{$IFOPT D+}
var
  TGxIdeNotifierCount: Integer = 0;
{$ENDIF D+}

type
  TGxIdeNotifier = class(TNotifierObject, IOTAIdeNotifier, IOTAIDENotifier50)
  private
    FChangeServicesClient: TEditorChangeServices;
    FInstalledModuleNotifiers: TInterfaceList;
    procedure ClearInstalledModuleNotifiers;
    procedure RemoveInstalledModuleNotifiers;
    procedure InstallModuleNotifiersIntoExistingModules;
    procedure InstallModuleNotifier(const IdeNotifiedFileName: string); overload;
    procedure InstallModuleNotifier(const IModule: IOTAModule); overload;
    function HasNotifierBeenInstalled(const FileName: string): Boolean;
    procedure NotifyClientNewModule(const Module: IOTAModule);
    function ValidModuleFileName(const FileName: string): Boolean;
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
    // Fix for the IDE always switching to the Startup Layout
    procedure SaveUserDesktop;
    procedure RestoreUserDesktop;
{$ENDIF STARTUP_LAYOUT_FIX_ENABLED}
  protected
    // IOTAIdeNotifier
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    // IOTAIDENotifier50
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  public
    constructor Create(Client: TEditorChangeServices);
    destructor Destroy; override;
  end;

  // The IGxNotifierUninstallation interface is used to
  // instruct an installed notifier instance to remove
  // itself, and all notifiers it makes uses of, from
  // the Open Tools API.
  // This will be used to completely detach from the IDE
  // when a package is (dynamically) uninstalled and
  // notifiers are still hooked into the IDE.
  IGxNotifierUninstallation = interface(IInterface)
    ['{E26441D1-250E-11D4-A872-66A3BC000000}']
    procedure UninstallNotifier;
  end;

  IGxModuleNotifier = interface(IOTAModuleNotifier)
    ['{838446E1-F454-11D3-A7CF-B41D01000000}']
    function GetAssociatedModule: IOTAModule;

    property AssociatedModule: IOTAModule read GetAssociatedModule;
  end;

{$IFOPT D+}
var
  TGxModuleNotifierCount: Integer = 0;
{$ENDIF D+}

type
  TGxModuleNotifier = class(TInterfacedObject, IOTANotifier,
                            IOTAModuleNotifier, IGxModuleNotifier,
                            IGxNotifierUninstallation)
  private
    FIdeClient: TGxIdeNotifier;
  private
    FSelfNotifierIndex: Integer;
    procedure RemoveSelfNotifier;
  private
    FCachedSourceEditors: array of IOTASourceEditor;
    // Note that this list of editor notifiers contains both
    // IOTAEditorNotifier and IOTAFormNotifier interface instances.
    FInstalledEditorNotifiers: TInterfaceList;
  private
    FAssociatedModule: IOTAModule;
    FFileName: string;
    procedure InstallEditorNotifiers;
    procedure RemoveEditorNotifiers;
    procedure RemoveSelfFromClient;
  protected
    // IOTANotifier - just there for playing
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
    // IGxModuleNotifier
    function GetAssociatedModule: IOTAModule;
    // IGxNotifierUninstallation
    procedure UninstallNotifier;
  public
    constructor Create(AssociatedModule: IOTAModule; Client: TGxIdeNotifier);
    destructor Destroy; override;
  end;

type
  TGxBaseEditorNotifier = class(TNotifierObject, IOTANotifier)
  private
    FSelfNotifierIndex: Integer;
    FModuleClient: TGxModuleNotifier;
    procedure GenericNotification(const NotifyType: TInternalIdeNotification;
      const GenericParameter: IInterface; CharCode: Word = 0; KeyData: Integer = 0);
  protected
    // Base implementation of IGxNotifierUninstallation
    FFileName: string;
    procedure UninstallNotifier;
  protected
    procedure RemoveSelfFromClient;
    procedure RemoveSelfNotifier; virtual; abstract;
  end;

{$IFOPT D+}
var
  TGxEditorNotifierCount: Integer = 0;
{$ENDIF D+}

type
  TGxEditorNotifier = class(TGxBaseEditorNotifier, IOTANotifier,
                            IOTAEditorNotifier,
                            IGxNotifierUninstallation)
  private
    FAssociatedSourceEditor: IOTASourceEditor;
    FNewEditControlWndProc: Pointer;
    FOldEditControlWndProc: Integer;
    //FEditorHandle: THandle;
    procedure EditControlWndProc(var Msg: TMessage);
    function GetEditControl: TWinControl;
    procedure HookEditorWndProc;
    procedure UnhookEditorWndProc;
  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAEditorNotifier
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    procedure RemoveSelfNotifier; override;
  public
    constructor Create(const Client: TGxModuleNotifier; const ISourceEditor: IOTASourceEditor);
    destructor Destroy; override;
  end;

{$IFOPT D+}
var
  TGxFormNotifierCount: Integer = 0;
{$ENDIF D+}

type
  TGxFormNotifier = class(TGxBaseEditorNotifier, IOTANotifier,
                          IOTAFormNotifier,
                          IGxNotifierUninstallation)
  private
    FAssociatedFormEditor: IOTAFormEditor;
  protected
    // IOTANotifier - just there for playing;
    // doesn't do anything and is not used in a meaningful way.
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  protected
    // IOTAFormNotifier
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
  protected
    procedure RemoveSelfNotifier; override;
  public
    constructor Create(const Client: TGxModuleNotifier; const IFormEditor: IOTAFormEditor);
    destructor Destroy; override;
  end;

var
  PrivateEditorChangeServices: TEditorChangeServices;

function GxEditorChangeServices: IGxEditorChangeServices;
begin
  if not Assigned(PrivateEditorChangeServices) then
    PrivateEditorChangeServices := TEditorChangeServices.Create;

  Result := PrivateEditorChangeServices;
end;

procedure ReleaseEditorChangeServices;
begin
  FreeAndNil(PrivateEditorChangeServices);
end;

{ TEditorChangeServices }

function TEditorChangeServices.AddNotifier(Notifier: IGxEditorNotification): Integer;
begin
  Assert(Assigned(Notifier));
  FNotifiers.Add(Notifier);
  Result := FNextIndex;
  Inc(FNextIndex);
  Assert(Result >= 0);
end;

procedure TEditorChangeServices.ClearNotifiers;
begin
  if not Assigned(FNotifiers) then
    Exit;

  {$IFOPT D+}
    if FNotifiers.Count > 0 then
    begin
      MessageBox(0, PChar(Format('TEditorChangeServices has %d dangling notifiers',
                 [FNotifiers.Count])), 'GExperts warning', MB_ICONHAND or MB_OK);
    end;
  {$ENDIF D+}

  FNotifiers.Clear;
end;

constructor TEditorChangeServices.Create;
begin
  {$IFOPT D+} SendDebug('Creating TEditorChangeServices'); {$ENDIF}
  {$IFOPT D+} Inc(TEditorChangeServicesCount); {$ENDIF}

  inherited Create;

  FIdeNotifierIndex := InvalidNotifierIndex;

  FNotifiers := TInterfaceList.Create;

  InstallInternalIdeNotifier;
  {$IFOPT D+} SendDebug('Created TEditorChangeServices'); {$ENDIF}
end;

destructor TEditorChangeServices.Destroy;
begin
  RemoveInternalIdeNotifier;
  //TODO 3 -oAnyone -cFeature: Unhook all editor windows

  ClearNotifiers;
  FreeAndNil(FNotifiers);

  inherited Destroy;
  {$IFOPT D+} Dec(TEditorChangeServicesCount); {$ENDIF}
end;

procedure TEditorChangeServices.GenericInternalIdeNotification(
  const NotifyType: TInternalIdeNotification;
  const GenericParameter: IInterface; CharCode: Word; KeyData: Integer);
var
  i: Integer;
  INotification: IGxEditorNotification;
begin
  Assert(Assigned(FNotifiers));
  Assert(Assigned(GenericParameter));

  for i := 0 to FNotifiers.Count-1 do
  begin
    try
      INotification := FNotifiers[i] as IGxEditorNotification;

      case NotifyType of
        iinNewModule:
          INotification.NewModuleOpened(GenericParameter as IOTAModule);

        iinSourceEditorModified:
          INotification.SourceEditorModified(GenericParameter as IOTASourceEditor);

        iinEditorKeyPressed:
          INotification.EditorKeyPressed(GenericParameter as IOTASourceEditor, CharCode, KeyData);

        iinFormEditorModified:
          INotification.FormEditorModified(GenericParameter as IOTAFormEditor);
      else
        Assert(False, 'Unknown generic internal IDE notify type sent');
      end;

    except
      on E: Exception do
      begin
        // Silently process the exception and let the IDE take care of the rest
        GxLogException(E);
        Application.HandleException(Self);
        // We deliberately swallow the exception since we do not
        // want a failed notifier to mess up anything.
      end;
    end;
  end;
end;

procedure TEditorChangeServices.InstallInternalIdeNotifier;
var
  IdeNotifier: TGxIdeNotifier;
begin
  IdeNotifier := TGxIdeNotifier.Create(Self);
  FIdeNotifierIndex := GxOtaGetIDEServices.AddNotifier(IdeNotifier);

  Assert(FIdeNotifierIndex >= 0);
end;

procedure TEditorChangeServices.RemoveInternalIdeNotifier;
begin
  if FIdeNotifierIndex <> InvalidNotifierIndex then
  begin
    GxOtaGetIDEServices.RemoveNotifier(FIdeNotifierIndex);
    FIdeNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TEditorChangeServices.RemoveNotifierIfNecessary(var Index: Integer);
var
  i: Integer;
begin
  if Index = InvalidNotifierIndex then
    Exit;

  Assert(Assigned(FNotifiers));
  Assert(Index >= 0);
  for i := 0 to FNotifiers.Count - 1 do begin
    if ((FNotifiers.Items[i] as IGxEditorNotification).GetIndex = Index) then
    begin
      FNotifiers.Delete(i);
      Break;
    end;
  end;
  Index := InvalidNotifierIndex;
end;

{ TGxIdeNotifier }

procedure TGxIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
  // Do nothing
end;

procedure TGxIdeNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
  // Do nothing
end;

procedure TGxIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  // Do nothing
end;

procedure TGxIdeNotifier.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  // Do nothing
end;

procedure TGxIdeNotifier.ClearInstalledModuleNotifiers;
{$IFOPT D+}
var
  i: Integer;
  UnaccountedModules: string;
{$ENDIF D+}
begin
  if not Assigned(FInstalledModuleNotifiers) then
    Exit;

  {$IFOPT D+}
    if FInstalledModuleNotifiers.Count > 0 then
    begin
      // If we still have module notifiers installed at this
      // stage, it means that some cleanup logic failed.
      for i := 0 to FInstalledModuleNotifiers.Count - 1 do
      begin
        with FInstalledModuleNotifiers[i] as IGxModuleNotifier do
          UnaccountedModules := UnaccountedModules + AssociatedModule.FileName + '; ';
      end;
      MessageBox(0, PChar(Format('TGxIdeNotifier has %d unaccounted modules (%s)',
                 [FInstalledModuleNotifiers.Count, UnaccountedModules])),
                 'GExperts warning', MB_ICONHAND or MB_OK);
    end;
  {$ENDIF D+}

  FInstalledModuleNotifiers.Clear;
end;

constructor TGxIdeNotifier.Create(Client: TEditorChangeServices);
begin
  {$IFOPT D+} Inc(TGxIdeNotifierCount); {$ENDIF}
  {$IFOPT D+} SendDebug('Creating TGxIdeNotifier'); {$ENDIF}

  inherited Create;

  FChangeServicesClient := Client;

  FInstalledModuleNotifiers := TInterfaceList.Create;

  // Install notifiers into all already loaded modules.
  InstallModuleNotifiersIntoExistingModules;
  {$IFOPT D+} SendDebug('Created TGxIdeNotifier'); {$ENDIF}
end;

destructor TGxIdeNotifier.Destroy;
begin
  {$IFOPT D+} SendDebug('Destroying TGxIdeNotifier'); {$ENDIF}

  // Sometimes we never get destroy notifications for DFMs open as text
  // This check prevents us from AVing on shutdown trying to remove the
  // notifiers for these by-now-closed units
  if not Application.Terminated then
  begin
    RemoveInstalledModuleNotifiers;
    ClearInstalledModuleNotifiers;
  end;

  {$IFOPT D+} SendDebug('Cleared installed module notifiers'); {$ENDIF}

  FreeAndNil(FInstalledModuleNotifiers);

  {$IFOPT D+} SendDebug('Freed installed module notifiers'); {$ENDIF}

  if Assigned(FChangeServicesClient) then
  begin
    FChangeServicesClient.FIdeNotifierIndex := InvalidNotifierIndex;
    FChangeServicesClient := nil;
  end;

  inherited Destroy;
  {$IFOPT D+} SendDebug('TGxIdeNotifier destroyed'); {$ENDIF}
  {$IFOPT D+} Dec(TGxIdeNotifierCount); {$ENDIF}
end;

procedure TGxIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  {$IFOPT D+}
    SendDebug(Format('File notification %s for "%s"',
                     [GetEnumName(TypeInfo(TOTAFileNotification), Integer(NotifyCode)), FileName]));
  {$ENDIF D+}

  case NotifyCode of
    ofnFileOpened:
      InstallModuleNotifier(FileName);
{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
    ofnProjectDesktopSave:
      SaveUserDesktop;

    ofnActiveProjectChanged:
      RestoreUserDesktop;
{$ENDIF STARTUP_LAYOUT_FIX_ENABLED}
  else // case  // FI:W506
    // Do nothing
  end;
end;

function TGxIdeNotifier.HasNotifierBeenInstalled(const FileName: string): Boolean;
var
  GxModuleNotifier: IGxModuleNotifier;
  AModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(FInstalledModuleNotifiers));

  Result := False;
  for i := 0 to FInstalledModuleNotifiers.Count-1 do
  begin
    GxModuleNotifier := FInstalledModuleNotifiers[i] as IGxModuleNotifier;
    Assert(Assigned(GxModuleNotifier));

    AModule := GxModuleNotifier.AssociatedModule;
    Assert(Assigned(AModule));

    Result := SameFileName(AModule.FileName, FileName);
    if Result then
      Break;
  end;
end;

procedure TGxIdeNotifier.InstallModuleNotifier(const IdeNotifiedFileName: string);
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
begin
  if not ValidModuleFileName(IdeNotifiedFileName) then
    Exit;

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  IModule := GxOtaGetModule(IdeNotifiedFileName);
  if not Assigned(IModule) then begin
    {$IFOPT D+} SendDebugError('Module interface not found by the IDE: ' + IdeNotifiedFileName); {$ENDIF}
    Exit;
  end;

  // Sometimes, we are double-notified of modules being opened, so check here
  if not HasNotifierBeenInstalled(IdeNotifiedFileName) then
    InstallModuleNotifier(IModule);
end;

procedure TGxIdeNotifier.InstallModuleNotifier(const IModule: IOTAModule);
var
  GxModuleNotifier: TGxModuleNotifier;
begin
  Assert(Assigned(IModule));
  if not ValidModuleFileName(IModule.FileName) then
    Exit;
  GxModuleNotifier := TGxModuleNotifier.Create(IModule, Self);
  FInstalledModuleNotifiers.Add(GxModuleNotifier);

  NotifyClientNewModule(IModule);
end;

procedure TGxIdeNotifier.InstallModuleNotifiersIntoExistingModules;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;

  i: Integer;
begin
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    Assert(Assigned(IModule));
    InstallModuleNotifier(IModule);
  end;
end;

procedure TGxIdeNotifier.NotifyClientNewModule(const Module: IOTAModule);
begin
  Assert(Assigned(FChangeServicesClient));
  Assert(Assigned(Module));
  {$IFOPT D+} SendDebug('Change services advertise new module ' + Module.FileName); {$ENDIF}

  FChangeServicesClient.GenericInternalIdeNotification(iinNewModule, Module);
end;

procedure TGxIdeNotifier.RemoveInstalledModuleNotifiers;
var
  INotifierUninstallation: IGxNotifierUninstallation;
begin
  if not Assigned(FInstalledModuleNotifiers) then
    Exit;

  while FInstalledModuleNotifiers.Count > 0 do
  begin
    INotifierUninstallation := FInstalledModuleNotifiers[0] as IGxNotifierUninstallation;

    Assert(Assigned(INotifierUninstallation));
    INotifierUninstallation.UninstallNotifier;
    // The interface object upon which we call UninstallNotifier
    // will remove itself from the list of editor notifiers, i.e.
    // from FInstalledEditorNotifiers, thereby reducing
    // FInstalledEditorNotifiers.Count down to zero.
  end;
end;

{$IFDEF STARTUP_LAYOUT_FIX_ENABLED}
type
  TComboBoxHack = class(TComboBox);

procedure TGxIdeNotifier.RestoreUserDesktop;
var
  Settings     : TGExpertsSettings;
  LDesktopName : string;
begin
  if not ConfigInfo.GetForceDesktopOnStartup then
    Exit; //==>

  LDesktopName := ConfigInfo.GetForcedStartupDesktop;
  if LDesktopName = '' then begin
    Settings := TGExpertsSettings.Create;
    try
      LDesktopName := Settings.ReadString('Desktop', 'Layout', '');
    finally
      FreeAndNil(Settings);
    end;
  end;

  if Length(LDesktopName) > 0 then
    SetIdeDesktop(LDesktopName);
end;

procedure TGxIdeNotifier.SaveUserDesktop;
var
  cbDesktop   : TComboBox;
  ndx         : Integer;
  LDesktopName: string;
  LIgnoreName : string;
  Settings    : TGExpertsSettings;
begin
  if not TryGetDesktopCombo(cbDesktop) then
    Exit; //==>
  ndx := cbDesktop.ItemIndex;
  if ndx > 0 then
  begin
    LDesktopName := cbDesktop.Items.Strings[ndx];

    // do not store layouts with these names:
    LIgnoreName := AnsiUpperCase(LDesktopName);
    if (AnsiPos('DEFAULT', LIgnoreName)=1)
    or (AnsiPos('STARTUP', LIgnoreName)=1)
    or (AnsiPos('DEBUG', LIgnoreName) > 0)
    then Exit;

    Settings := TGExpertsSettings.Create;
    try
      Settings.WriteString('Desktop', 'Layout', LDesktopName);
    finally
      FreeAndNil(Settings);
    end;
  end;
end;
{$ENDIF STARTUP_LAYOUT_FIX_ENABLED}

function TGxIdeNotifier.ValidModuleFileName(const FileName: string): Boolean;
begin
  Result := not FileIsWelcomePage(FileName);
end;

{ TGxModuleNotifier }

procedure TGxModuleNotifier.AfterSave;
begin
  // Do nothing
end;

procedure TGxModuleNotifier.BeforeSave;
begin
  // Do nothing
end;

function TGxModuleNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

constructor TGxModuleNotifier.Create(AssociatedModule: IOTAModule; Client: TGxIdeNotifier);
begin
  {$IFOPT D+} Inc(TGxModuleNotifierCount); {$ENDIF}
  {$IFOPT D+} SendDebug('Creating TGxModuleNotifier for ' + AssociatedModule.FileName); {$ENDIF}
  inherited Create;
  FFileName := AssociatedModule.FileName;

  SetLength(FCachedSourceEditors, 0);
  FSelfNotifierIndex := InvalidNotifierIndex;

  Assert(Assigned(Client));
  FIdeClient := Client;

  FInstalledEditorNotifiers := TInterfaceList.Create;

  FSelfNotifierIndex := AssociatedModule.AddNotifier(Self);

  // Keep a copy of the associated module around
  // without any reference-counting.
  NoRefCount(FAssociatedModule) := NoRefCount(AssociatedModule);

  InstallEditorNotifiers;

  {$IFOPT D+} SendDebug('Created TGxModuleNotifier for ' + AssociatedModule.FileName); {$ENDIF}
end;

destructor TGxModuleNotifier.Destroy;
var
  i: Integer;
begin
  {$IFOPT D+} SendDebug('Destroying TGxModuleNotifier for ' + FFileName); {$ENDIF}

  for i := 0 to Length(FCachedSourceEditors)-1 do
    NoRefCount(FCachedSourceEditors[i]) := nil;

  RemoveEditorNotifiers;

  FreeAndNil(FInstalledEditorNotifiers);

  RemoveSelfNotifier;
  NoRefCount(FAssociatedModule) := nil;
  RemoveSelfFromClient;

  {$IFOPT D+} SendDebug('TGxModuleNotifier destroyed for ' + FFileName); {$ENDIF}
  {$IFOPT D+} Dec(TGxModuleNotifierCount); {$ENDIF}
  inherited;
end;

procedure TGxModuleNotifier.Destroyed;
begin
  RemoveSelfNotifier;
  // Clear the associated module without invoking
  // any reference-counting.
  NoRefCount(FAssociatedModule) := nil;
  RemoveSelfFromClient;
end;

function TGxModuleNotifier.GetAssociatedModule: IOTAModule;
begin
  Result := FAssociatedModule;
end;

procedure TGxModuleNotifier.InstallEditorNotifiers;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
  IFormEditor: IOTAFormEditor;
  INotifier: IOTANotifier;
begin
  Assert(Assigned(FAssociatedModule));

  for i := 0 to FAssociatedModule.GetModuleFileCount - 1 do
  begin
    IEditor := GxOtaGetFileEditorForModule(FAssociatedModule, i);
    Assert(Assigned(IEditor));

    if Trim(IEditor.FileName) = '' then
    begin
      {$IFOPT D+} SendDebugWarning('Blank filename in module: ' + FAssociatedModule.FileName); {$ENDIF}
      Continue; // Don't register notifiers for blank file names (like with TWebModule in D5)
    end;

    if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
    begin
      if Assigned(ISourceEditor) then
      begin
        INotifier := TGxEditorNotifier.Create(Self, ISourceEditor);
        FInstalledEditorNotifiers.Add(INotifier);
      end;
    end;

    if Supports(IEditor, IOTAFormEditor, IFormEditor) then
    begin
      if Assigned(IFormEditor) then
      begin
        INotifier := TGxFormNotifier.Create(Self, IFormEditor);
        FInstalledEditorNotifiers.Add(INotifier);
      end;
    end;
  end;
end;

procedure TGxModuleNotifier.Modified;
begin
end;

procedure TGxModuleNotifier.ModuleRenamed(const NewName: string);
begin
  // Do nothing
end;

procedure TGxModuleNotifier.RemoveEditorNotifiers;
var
  INotifierUninstallation: IGxNotifierUninstallation;
begin
  Assert(Assigned(FInstalledEditorNotifiers));
  while FInstalledEditorNotifiers.Count > 0 do
  begin
    INotifierUninstallation := FInstalledEditorNotifiers[0] as IGxNotifierUninstallation;
    Assert(Assigned(INotifierUninstallation));

    INotifierUninstallation.UninstallNotifier;
    // The interface object upon which we call UninstallNotifier
    // will remove itself from the list of editor notifiers, i.e.
    // from FInstalledEditorNotifiers, thereby reducing
    // FInstalledEditorNotifiers.Count down to zero.
  end;
end;

procedure TGxModuleNotifier.RemoveSelfFromClient;
var
  RemovalIndex: Integer;
begin
  if Assigned(FIdeClient) then
  begin
    Assert(Assigned(FIdeClient.FInstalledModuleNotifiers));

    RemovalIndex := FIdeClient.FInstalledModuleNotifiers.Remove(Self);
    Assert(RemovalIndex <> -1);

    FIdeClient := nil;
  end;
end;

procedure TGxModuleNotifier.RemoveSelfNotifier;
begin
  if FSelfNotifierIndex <> InvalidNotifierIndex then
  begin
    if Assigned(FAssociatedModule) then
      FAssociatedModule.RemoveNotifier(FSelfNotifierIndex);
    FSelfNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TGxModuleNotifier.UninstallNotifier;
begin
  RemoveSelfNotifier;
  RemoveEditorNotifiers;
  RemoveSelfFromClient;
end;

{ TGxBaseEditorNotifier }

procedure TGxBaseEditorNotifier.GenericNotification(const NotifyType: TInternalIdeNotification;
  const GenericParameter: IInterface; CharCode: Word; KeyData: Integer);
begin
  Assert(Assigned(FModuleClient));
  Assert(Assigned(FModuleClient.FIdeClient));
  Assert(Assigned(FModuleClient.FIdeClient.FChangeServicesClient));
  FModuleClient.FIdeClient.FChangeServicesClient.GenericInternalIdeNotification(NotifyType, GenericParameter, CharCode, KeyData);
end;

procedure TGxBaseEditorNotifier.RemoveSelfFromClient;
var
  RemovalIndex: Integer;
  ISelfNotifier: IOTANotifier;
begin
  if Assigned(FModuleClient) then
  begin
    ISelfNotifier := Self as IOTANotifier;

    RemovalIndex := FModuleClient.FInstalledEditorNotifiers.Remove(ISelfNotifier);
    Assert(RemovalIndex <> -1);
    FModuleClient := nil;
  end;
end;

procedure TGxBaseEditorNotifier.UninstallNotifier;
begin
  {$IFOPT D+} SendDebug('UninstallNotifier for ' + FFileName); {$ENDIF}
  RemoveSelfNotifier;
  RemoveSelfFromClient;
end;

{ TGxEditorNotifier }

procedure TGxEditorNotifier.AfterSave;
begin
  // Do nothing
end;

procedure TGxEditorNotifier.BeforeSave;
begin
  // Do nothing
end;

constructor TGxEditorNotifier.Create(const Client: TGxModuleNotifier; const ISourceEditor: IOTASourceEditor);
begin
  {$IFOPT D+} Inc(TGxEditorNotifierCount); {$ENDIF}
  {$IFOPT D+} SendDebug('Creating TGxEditorNotifier for ' + ISourceEditor.FileName); {$ENDIF}
  inherited Create;
  FSelfNotifierIndex := InvalidNotifierIndex;
  FFileName := ISourceEditor.FileName;

  FModuleClient := Client;

  Assert(Assigned(ISourceEditor));
  NoRefCount(FAssociatedSourceEditor) := NoRefCount(ISourceEditor);

  FSelfNotifierIndex := ISourceEditor.AddNotifier(Self);
  if False then
    HookEditorWndProc;

  {$IFOPT D+} SendDebug('Created TGxEditorNotifier for ' + ISourceEditor.FileName); {$ENDIF}
end;

destructor TGxEditorNotifier.Destroy;
begin
  {$IFOPT D+} SendDebug('Destroying TGxEditorNotifier for ' + FFileName); {$ENDIF}
  //UnhookEditorWndProc;
  FModuleClient := nil;

  NoRefCount(FAssociatedSourceEditor) := nil;
  inherited;
  {$IFOPT D+} Dec(TGxEditorNotifierCount); {$ENDIF}
end;

procedure TGxEditorNotifier.Destroyed;
begin
  {$IFOPT D+}
  if Assigned(FAssociatedSourceEditor) then
    SendDebug('Destroyed notification for: ' + FAssociatedSourceEditor.FileName)
  else
    SendDebugWarning('Destroyed notification for: <unknown source editor>');
  {$ENDIF}
  //UnhookEditorWndProc;
  NoRefCount(FAssociatedSourceEditor) := nil;
end;

procedure TGxEditorNotifier.HookEditorWndProc;
var
  EditControl: TWinControl;
begin
  if False then
    UnhookEditorWndProc;
  EditControl := GetEditControl;
  if Assigned(EditControl) then
  begin
    //FEditorHandle := EditControl.Handle;
    FNewEditControlWndProc := Classes.MakeObjectInstance(EditControlWndProc);
    FOldEditControlWndProc := SetWindowLong(EditControl.Handle, GWL_WNDPROC, NativeInt(FNewEditControlWndProc));
    if FOldEditControlWndProc = 0 then
    begin
      {$IFOPT D+} SendDebugError('Windows error hooking EditorWndProc'); {$ENDIF}
    end;
  end
  else
  begin
    {$IFOPT D+}
    if Assigned(FAssociatedSourceEditor) then
      SendDebug('Unable too hook edit control for ' + FAssociatedSourceEditor.FileName)
    else
      SendDebugWarning('Unable too hook edit control for <unknown source editor>');
    {$ENDIF}
  end;
end;

procedure TGxEditorNotifier.UnhookEditorWndProc;
var
  EditorHandle: HWND;
begin
  {$IFOPT D+}
  if Assigned(FAssociatedSourceEditor) then
    SendDebug('Attempting to unhook EditorWndProc for: ' + FAssociatedSourceEditor.FileName)
  else
    SendDebugWarning('Attempting to unhook EditorWndProc for: <unknown source editor>');
  {$ENDIF}
  if {(FEditorHandle <> 0) and} (FOldEditControlWndProc <> 0) then
  begin
    {$IFOPT D+} SendDebug('Starting unhook procedure'); {$ENDIF}
    EditorHandle := GetEditControl.Handle;
    if SetWindowLong({F}EditorHandle, GWL_WNDPROC, NativeInt(FOldEditControlWndProc)) = 0 then
    begin
      {$IFOPT D+} SendDebugError('Windows error unhooking EditorWndProc'); {$ENDIF}
    end;
    if Assigned(FNewEditControlWndProc) then
      Classes.FreeObjectInstance(FNewEditControlWndProc);
    FOldEditControlWndProc := 0;
    //FEditorHandle := 0;
    {$IFOPT D+} SendDebug('Successful unhook procedure'); {$ENDIF}
  end;
end;

procedure TGxEditorNotifier.EditControlWndProc(var Msg: TMessage);
var
  EditorHandle: HWND;
begin
  //{$IFOPT D+} SendDebug('Editor Message coming...'); {$ENDIF}
  {$IFOPT D+} SendDebug('Editor Message: ' + MessageName(Msg.Msg) + ' for ' + FAssociatedSourceEditor.FileName); {$ENDIF}
  if Msg.Msg = WM_DESTROY then
    //UnhookEditorWndProc
  else if Msg.Msg = WM_KEYUP then
    GenericNotification(iinEditorKeyPressed, FAssociatedSourceEditor, TWMKey(Msg).CharCode, TWMKey(Msg).KeyData);
  try
    if FOldEditControlWndProc <> 0 then
    begin
      EditorHandle := GetEditControl.Handle;
      Msg.Result := CallWindowProc(Pointer(FOldEditControlWndProc), {F}EditorHandle, Msg.Msg, Msg.WParam, Msg.LParam);
    end;
  except
    on E: Exception do
    begin
      E.Message := E.Message + ' (GExperts EditControlWndProc Message: ' +IntToStr(Msg.Msg)+ '+)';
      raise;
    end;
  end;
end;

function TGxEditorNotifier.GetEditControl: TWinControl;
var
  EditView: IOTAEditView;
  EditWindow: INTAEditWindow;
  EditForm: TCustomForm;
begin
  Result := nil;
  if Assigned(FAssociatedSourceEditor) then
  begin
    if GxOtaTryGetTopMostEditView(FAssociatedSourceEditor, EditView) then
    begin
      EditWindow := EditView.GetEditWindow;
      if Assigned(EditWindow) then
      begin
        EditForm := EditWindow.Form;
        if Assigned(EditForm) then
          Result := GetIDEEditControl(EditForm);
      end;
    end;
  end;
end;

procedure TGxEditorNotifier.Modified;
begin
  GenericNotification(iinSourceEditorModified, FAssociatedSourceEditor);
end;

procedure TGxEditorNotifier.RemoveSelfNotifier;
begin
  Assert(Assigned(FAssociatedSourceEditor));

  if FSelfNotifierIndex <> InvalidNotifierIndex then
  begin
    FAssociatedSourceEditor.RemoveNotifier(FSelfNotifierIndex);
    FSelfNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TGxEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  // Do nothing
end;

procedure TGxEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Assigned(View) then
  begin
    if (Operation = opInsert) then
    begin
      {$IFOPT D+}
      if Assigned(View.Buffer) then
        SendDebug('Insert notification for ' + View.Buffer.FileName)
      else
        SendDebugWarning('Insert notification for <unknown buffer>');
      {$ENDIF}
      // TODO 3 -cBug -oAnyone: We need to hook secondary edit views, but that requires separate tracking of FEditorHandle, etc.
      //HookEditorWndProc;
    end
    else if Operation = opRemove then
    begin
      {$IFOPT D+}
      if Assigned(View.Buffer) then
        SendDebug('Removal notification for ' + View.Buffer.FileName)
      else
        SendDebugWarning('Removal notification for <unknown buffer>');
      {$ENDIF}
      //UnHookEditorWndProc;
    end;
  end;
end;

{ TGxFormNotifier }

procedure TGxFormNotifier.AfterSave;
begin
  // Do nothing
end;

procedure TGxFormNotifier.BeforeSave;
begin
  // Do nothing
end;

procedure TGxFormNotifier.ComponentRenamed(ComponentHandle: TOTAHandle;
  const OldName, NewName: string);
var
  i: Integer;
  INotification: IGxEditorNotification;
  IComponent: IOTAComponent;
begin
  Assert(Assigned(PrivateEditorChangeServices.FNotifiers));
  for i := 0 to PrivateEditorChangeServices.FNotifiers.Count - 1 do
  begin
    try
      INotification := PrivateEditorChangeServices.FNotifiers[i] as IGxEditorNotification;
      IComponent := nil;
      if Assigned(FAssociatedFormEditor) then
      begin
        IComponent := FAssociatedFormEditor.GetComponentFromHandle(ComponentHandle);
        Assert(Assigned(IComponent));
      end;
      INotification.ComponentRenamed(FAssociatedFormEditor, IComponent, OldName, NewName);
    except
      on E: Exception do
      begin
        GxLogException(E);
        Application.HandleException(Self);
      end;
    end;
  end;
end;

constructor TGxFormNotifier.Create(const Client: TGxModuleNotifier; const IFormEditor: IOTAFormEditor);
begin
  {$IFOPT D+} Inc(TGxFormNotifierCount); {$ENDIF}
  {$IFOPT D+} SendDebug('Creating TGxFormNotifier'); {$ENDIF}
  inherited Create;

  FModuleClient := Client;

  Assert(Assigned(IFormEditor));
  NoRefCount(FAssociatedFormEditor) := NoRefCount(IFormEditor);

  FSelfNotifierIndex := IFormEditor.AddNotifier(Self);

  {$IFOPT D+} SendDebug('Created TGxFormNotifier'); {$ENDIF}
end;

destructor TGxFormNotifier.Destroy;
begin
  {$IFOPT D+} SendDebug('Destroying TGxFormNotifier'); {$ENDIF}
  NoRefCount(FAssociatedFormEditor) := nil;
  FModuleClient := nil;

  inherited Destroy;
  {$IFOPT D+} SendDebug('TGxFormNotifier destroyed'); {$ENDIF}
  {$IFOPT D+} Dec(TGxFormNotifierCount); {$ENDIF}
end;

procedure TGxFormNotifier.Destroyed;
begin
  NoRefCount(FAssociatedFormEditor) := nil;
end;

procedure TGxFormNotifier.FormActivated;
begin
  // Do nothing
end;

procedure TGxFormNotifier.FormSaving;
begin
  // Do nothing
end;

procedure TGxFormNotifier.Modified;
begin
  // This notifier is fired each time a form is modified.
  Assert(Assigned(FModuleClient));
  Assert(Assigned(FModuleClient.FIdeClient));
  Assert(Assigned(FModuleClient.FIdeClient.FChangeServicesClient));
  FModuleClient.FIdeClient.FChangeServicesClient.GenericInternalIdeNotification(iinFormEditorModified, FAssociatedFormEditor);
end;

procedure TGxFormNotifier.RemoveSelfNotifier;
begin
  if not Assigned(FAssociatedFormEditor) then
  begin
    {$IFOPT D+} SendDebugError('RemoveSelfNotifier called with nil FAssociatedFormEditor for ' + FFileName); {$ENDIF}
    Exit;
  end;

  if FSelfNotifierIndex <> InvalidNotifierIndex then
  begin
    FAssociatedFormEditor.RemoveNotifier(FSelfNotifierIndex);
    FSelfNotifierIndex := InvalidNotifierIndex;
  end;
end;

{$IFDEF UseInternalTestClient}
type
  TestClient = class(TInterfacedObject, IGxEditorNotification)
  private
    procedure NewModuleOpened(const Module: IOTAModule);

    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
  end;

{ TestClient }

procedure TestClient.FormEditorModified(const FormEditor: IOTAFormEditor);
begin
  Assert(Assigned(FormEditor));
  WriteSimpleMessage('TestClient Form Modified: ' + FormEditor.FileName);
end;

procedure TestClient.NewModuleOpened(const Module: IOTAModule);
begin
  WriteTitleMessage('TestClient got new module message:');
  WriteSimpleMessage('  ' + Module.FileName);
end;

procedure TestClient.SourceEditorModified(const SourceEditor: IOTASourceEditor);
begin
  Assert(Assigned(SourceEditor));
  WriteSimpleMessage('TestClient Source Modified: ' + SourceEditor.FileName);
end;
{$ENDIF UseInternalTestClient}

{$IFDEF UseInternalTestClient}
var
  NotifierIndex: Integer;

procedure Initialize;
begin
  NotifierIndex := GxEditorChangeServices.AddNotifier(TestClient.Create);
end;

procedure Finalize;
var
  Temp: IGxEditorChangeServices;
begin
  // We must take care that the interface that
  // GxEditorChangeServices delivers is _IntfClear'ed
  // before we release the underlying class instance.
  // (Via ReleaseEditorChangeServices)
  Temp := GxEditorChangeServices;
  Temp.RemoveNotifier(NotifierIndex);
  Temp := nil; // Force destruction, just in case
end;
{$ENDIF UseInternalTestClient}

initialization

  {$IFDEF UseInternalTestClient}
    Initialize;
  {$ENDIF UseInternalTestClient}

finalization

  {$IFDEF UseInternalTestClient}
    Finalize;
  {$ENDIF UseInternalTestClient}

  ReleaseEditorChangeServices;

{$IFOPT D+}
  if TEditorChangeServicesCount <> 0 then
    SendDebug(Format('Dangling references to TEditorChangeServicesCount: %d', [TEditorChangeServicesCount]));

  if TGxIdeNotifierCount <> 0 then
    SendDebug(Format('Dangling references to TGxIdeNotifierCount: %d', [TGxIdeNotifierCount]));

  if TGxModuleNotifierCount <> 0 then
    SendDebug(Format('Dangling references to TGxModuleNotifierCount: %d', [TGxModuleNotifierCount]));

  if TGxEditorNotifierCount <> 0 then
    SendDebug(Format('Dangling references to TGxEditorNotifierCount: %d', [TGxEditorNotifierCount]));

  if TGxFormNotifierCount <> 0 then
    SendDebug(Format('Dangling references to TGxFormNotifierCount: %d', [TGxFormNotifierCount]));
{$ENDIF D+}

end.
