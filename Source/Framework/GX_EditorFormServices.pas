unit GX_EditorFormServices;

{$I GX_CondDefine.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Forms, Controls;

type
  { Interface to a component that GExperts places on
    each and every single editor form the IDE creates.
    This "proxy" provides access services to the various
    controls present on the form. Among these controls are,
    for instance, the editor control itself, tab controls,
    and docking panels.
    Do not hold an interface for any prolonged period
    of time, as the user may close editor forms any time,
    thus invalidating the proxy. }
  IGxEditFormProxy = interface(IUnknown)
    ['{5A31EA50-D292-11D3-A948-22D821000000}']
    function GetEditorForm: TCustomForm;
    function GetEditControl: TWinControl;
    function GetIsSourceEditor: Boolean;

    // Return the actual IDE's editor form.
    property EditorForm: TCustomForm read GetEditorForm;
    // Return the editor control.
    property EditControl: TWinControl read GetEditControl;
    // Return if the edit proxy is for a source editor or another type, such as the welcome page
    property IsSourceEditor: Boolean read GetIsSourceEditor;
  end;

type
  TEditFormEventCode = (efAfterCreate, efBeforeActivate);

  TEditFormNotifier = procedure(EventCode: TEditFormEventCode;
    EditFormProxy: IGxEditFormProxy) of object;

  { Allow listeners to attach to interesting events that
    may happen with the editor form.
    Currently handled are

      * efAfterCreate
           The editor form has just been created and the
           GExperts edit form proxy has just been installed.

      * efBeforeActivate
           The editor form is being activated.

    Additionally, access to all editor form proxies, representing
    all editor forms in the IDE is provided. }
  IGxEditorFormServices = interface(IUnknown)
    ['{38F24310-D1D9-11D3-A944-DA3A65000000}']
    procedure AddListener(Listener: TEditFormNotifier);
    procedure RemoveListener(Listener: TEditFormNotifier);
    function GetCurrentProxy: IGxEditFormProxy;
    function GetEditorFormProxyCount: Integer;
    function GetEditorFormProxy(Index: Integer): IGxEditFormProxy;

    // Proxy on form that is currently active or was last active
    // if the current IDE form is not an editor form.
    // Returns nil if no editor form is open.
    property CurrentProxy: IGxEditFormProxy read GetCurrentProxy;
    // Retrieve number of proxies installed (one per form, strictly)
    // and allow to retrieve the proxy itself.
    property EditorFormProxyCount: Integer read GetEditorFormProxyCount;
    property EditorFormProxies[Index: Integer]: IGxEditFormProxy read GetEditorFormProxy;
  end;

function GxEditorFormServices: IGxEditorFormServices;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, TypInfo, {$ENDIF}
{$IFDEF LINUX}
  WinUtils,
{$ENDIF LINUX}
  Windows, SysUtils, Classes,
  GX_GenericUtils, GX_IdeUtils, GX_GenericClasses;

type
  TListenerRecord = record
    ListenerMethod: TEditFormNotifier;
  end;
  PListenerRecord = ^TListenerRecord;

type
  TGxEditorFormHostComponent = class(TComponent, IGxEditFormProxy)
  private
    FRefCount: Integer;
    FEditControl: TWinControl;
  protected
    // Overriden IUnknown implementations to implement life-time
    // management in a non-TInterfacedObject descendant.
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    // IGxEditFormProxy
    function GetEditorForm: TCustomForm;
    function GetEditControl: TWinControl;
    function GetIsSourceEditor: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  // Implementation of IGxEditorFormServices.
  // We need to descend from TComponent as this implementation
  // registers itself via Notification with editor forms to
  // learn of proxies being destroyed.
  TGxEditorFormServices = class(TComponent, IGxEditorFormServices)
  private
    FCurrentProxyHost: Integer;
    FHostComponents: TGxObjectList;
    FListeners: TList;
    procedure ClearListeners;
    procedure ClearHostComponents;
    procedure BeforeSetFocusEditorForm(const WindowHandle: THandle);
    procedure FireEvent(const EventCode: TEditFormEventCode; const EditFormProxy: TGxEditorFormHostComponent);
    procedure InitializeExistingForms;
    procedure InvalidateCurrentProxyHost;
    procedure UpdateCurrentProxyHost(const EditFormProxy: TGxEditorFormHostComponent);
    procedure FindAndAssignCurrentProxyHost;
    function IsInvalidCurrentProxyHost: Boolean;
    function InjectEditorFormHostComponent(const EditForm: TCustomForm): TGxEditorFormHostComponent;
  protected
    // Overriden IUnknown implementations for
    // singleton life-time management
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    // IGxEditorFormServices
    procedure AddListener(Listener: TEditFormNotifier);
    procedure RemoveListener(Listener: TEditFormNotifier);
    function GetCurrentProxy: IGxEditFormProxy;
    function GetEditorFormProxyCount: Integer;
    function GetEditorFormProxy(Index: Integer): IGxEditFormProxy;
  protected
    // Keeps track of editor form proxies as they are
    // removed by virtue of their containing editor
    // form being destroyed.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  InvalidCurrentProxyHost = -1;

var
  PrivateGxEditorFormServices: TGxEditorFormServices;

function GxEditorFormServices: IGxEditorFormServices;
begin
  if not Assigned(PrivateGxEditorFormServices) then
    PrivateGxEditorFormServices := TGxEditorFormServices.Create(nil);

  Result := PrivateGxEditorFormServices;
end;

procedure FreeEditorFormServices;
begin
  FreeAndNil(PrivateGxEditorFormServices);
end;

{ TGxEditorFormHostComponent }

constructor TGxEditorFormHostComponent.Create(AOwner: TComponent);
begin
  Assert(AOwner is TCustomForm);
  Assert(AOwner.ClassNameIs(EditorFormClassName));

  inherited Create(AOwner);

  // Since FindComponent searches by name, our marker component
  // needs a name. For simplicity, we simply take the ClassName.
  // Note that this will cause exceptions to be thrown if two
  // components with the same name are owned by the same owner;
  // incidentally, this is just what we want for improved
  // runtime program correctness verification.
  Name := ClassName;

  // Cache control information from the form:
  FEditControl := AOwner.FindComponent('Editor') as TWinControl;
  if Assigned(FEditControl) then
    Assert(FEditControl.ClassNameIs(EditorControlClassName));
end;

destructor TGxEditorFormHostComponent.Destroy;
begin
  // We don't want anyone to retain references
  // when we are going away.
  {$IFOPT D+} SendDebug('Inside TGxEditorFormHostComponent.Destroy'); {$ENDIF}
  Assert(FRefCount = 0);
  FRefCount := -1; // Trigger for assert upon double free, for instance.

  inherited Destroy;
end;

var
  CBTHook: HHOOK;

function CBTHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
const
  ClassNameBufferSize = 120;
var
  ClassNameBuffer: array[0..ClassNameBufferSize-1] of Char;
  ApiResult: Integer;
  ComparisonResult: Integer;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(CBTHook, nCode, wParam, lParam);
    Exit;
  end;

  try
    case nCode of
      HCBT_SETFOCUS: // A control with a window handle was focused
        if wParam > 0 then  // wParam is 0 if an Exception happens in HCBT_Setfocus
        begin
          ApiResult := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          Win32Check(ApiResult <> 0);
          Assert(ApiResult < ClassNameBufferSize, 'Found class name larger than fixed buffer size');

          {$IFOPT D+} SendDebug('SetFocus hook found: ' + StrPas(ClassNameBuffer)); {$ENDIF}
          ComparisonResult := StrLIComp(ClassNameBuffer, EditorControlClassName, ClassNameBufferSize);
          if ComparisonResult = 0 then
          begin
            {$IFOPT D+} SendDebug('SetFocus hook found an editor control'); {$ENDIF}
            Assert(Assigned(PrivateGxEditorFormServices));
            PrivateGxEditorFormServices.BeforeSetFocusEditorForm(wParam);
          end;
        end;
      (*  This may be useful later to replace the Screen.OnActiveFormChanged hook 
      HCBT_ACTIVATE: // A top-level window was focused
        if wParam > 0 then  // wParam is 0 if an Exception happens in HCBT_ACTIVATE?
        begin
          ApiResult := GetClassName(wParam, ClassNameBuffer, SizeOf(ClassNameBuffer));
          Win32Check(ApiResult <> 0);
          Assert(ApiResult < ClassNameBufferSize, 'Found class name larger than fixed buffer size');

          {$IFOPT D+} SendDebug('Window hook found: ' + StrPas(ClassNameBuffer)); {$ENDIF}
          ComparisonResult := StrLIComp(ClassNameBuffer, 'TDefaultEnvironmentDialog', ClassNameBufferSize);
          if ComparisonResult = 0 then
          begin
            {$IFOPT D+} SendDebug('SetFocus hook found a form to resize'); {$ENDIF}
            Assert(Assigned(PrivateGxEditorFormServices));
            //PrivateGxEditorFormServices.BeforeSetFocusEditorForm(wParam);
          end;
        end;
      *)
    else
      // Nothing.
    end;

  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugError('CBTHookProc Exception: '+ E.Message); {$ENDIF D+}
      GxLogException(E);
      // Swallow exception.
    end;
  end;
  Result := CallNextHookEx(CBTHook, nCode, wParam, lParam);
end;

function TGxEditorFormHostComponent.GetEditControl: TWinControl;
begin
  Result := FEditControl;
end;

function TGxEditorFormHostComponent.GetEditorForm: TCustomForm;
begin
  Result := Owner as TCustomForm;
  Assert(Assigned(Result));
  Assert(Result.ClassNameIs(EditorFormClassName));
end;

function TGxEditorFormHostComponent.GetIsSourceEditor: Boolean;
begin
  Result := Assigned(FEditControl);
end;

procedure TGxEditorFormHostComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  //{$IFOPT D+} if Assigned(AComponent) then SendDebugFmt('TGxEditorFormHostComponent.Notification: %s for "%s" of class %s',[GetEnumName(TypeInfo(TOperation), Integer(Operation)), AComponent.Name, AComponent.ClassName]); {$ENDIF}
  Assert((Operation = opRemove) or (Operation = opInsert));
  if Operation = opRemove then
    if AComponent = FEditControl then
      FEditControl := nil;

  inherited Notification(AComponent, Operation);
end;

function TGxEditorFormHostComponent._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGxEditorFormHostComponent._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
end;

{ TGxEditorFormServices }

procedure TGxEditorFormServices.AddListener(Listener: TEditFormNotifier);
var
  AListenerRecord: PListenerRecord;
begin
  Assert(Assigned(FListeners));
  Assert(Assigned(Listener));

  New(AListenerRecord);
  FListeners.Add(AListenerRecord);
  AListenerRecord^.ListenerMethod := Listener;
end;

procedure TGxEditorFormServices.BeforeSetFocusEditorForm(const WindowHandle: THandle);
var
  i: Integer;
  AForm: TCustomForm;
  HostComponent: TComponent;
  NewHostComponentCreated: Boolean;
  AComp: TComponent;
begin
  // Find the EditWindow that owns the Editor Control pointed to by WindowHandle
  AForm := nil;
  i := Screen.FormCount -1;
  while i >= 0 do
  begin
    AForm := Screen.Forms[i];
    Assert(Assigned(AForm));

    if IsIdeEditorForm(AForm) then
    begin
      {$IFOPT D+}SendDebug('Editor form found - '+ AForm.Name);{$ENDIF}
      AComp := AForm.FindComponent('Editor');
      if Assigned(AComp) and (AComp is TWinControl) then
      begin
        if TWinControl(AComp).Handle = WindowHandle then
          Break;
      end;
    end;
    Dec(i);
  end;
  Assert(i >= 0);
  Assert(Assigned(AForm));

  if csDesigning in AForm.ComponentState then
    Exit;
  HostComponent := AForm.FindComponent(TGxEditorFormHostComponent.ClassName);
  if not Assigned(HostComponent) then
  begin
    //if not ComponentOwnsClass(AForm, EditorControlName) then
    //  Exit;
    HostComponent := InjectEditorFormHostComponent(AForm);
    NewHostComponentCreated := True;
  end
  else
    NewHostComponentCreated := False;

  UpdateCurrentProxyHost(HostComponent as TGxEditorFormHostComponent);

  if NewHostComponentCreated then
    FireEvent(efAfterCreate, HostComponent as TGxEditorFormHostComponent);

  FireEvent(efBeforeActivate, HostComponent as TGxEditorFormHostComponent);
end;

procedure TGxEditorFormServices.ClearHostComponents;
var
  HostComponent: TComponent;
  HostOwner: TComponent;
begin
  if Assigned(FHostComponents) then
  begin
    {$IFOPT D+}
      if FHostComponents.Count > 0 then
      begin
        MessageBox(0, PChar(Format('EditorFormServices has %d dangling host components', [FHostComponents.Count])),
                      'Warning', MB_OK or MB_ICONHAND);
      end;
    {$ENDIF D+}

    // We now remove our installed host components from
    // the form - this is particularly necessary if
    // GExperts is used in a package and unloaded
    // dynamically.
    while FHostComponents.Count > 0 do
    begin
      HostComponent := FHostComponents[0] as TComponent;

      // Remove the host component from the owner's component
      // list; this will trigger TGxEditorFormServices.Notification
      // with opRemove which in turn will remove HostComponent from
      // FHostComponents list.
      Assert(Assigned(HostComponent));
      HostOwner := HostComponent.Owner;

      Assert(Assigned(HostOwner));
      HostOwner.RemoveComponent(HostComponent);

      // Now that no one refers to the component any longer,
      // destroy it.
      HostComponent.Destroy;
    end;
    InvalidateCurrentProxyHost;
    Assert(FHostComponents.Count = 0);
  end;
end;

procedure TGxEditorFormServices.ClearListeners;
var
  i: Integer;
  AListenerRecord: PListenerRecord;
begin
  if Assigned(FListeners) then
  begin
    {$IFOPT D+}
      if FListeners.Count > 0 then
      begin
        MessageBox(0, PChar(Format('EditorFormServices has %d dangling listeners', [FListeners.Count])),
                      'Warning', MB_OK or MB_ICONHAND);
      end;
    {$ENDIF D+}
    for i := 0 to FListeners.Count-1 do
    begin
      AListenerRecord := FListeners[i];
      Dispose(AListenerRecord);
    end;
    FListeners.Clear;
  end;
end;

constructor TGxEditorFormServices.Create(AOwner: TComponent);
const
  OwnsObjects = True;
begin
  Assert(AOwner = nil);

  inherited Create(AOwner);

  FListeners := TList.Create;
  FHostComponents := TGxObjectList.Create(not OwnsObjects);
  InvalidateCurrentProxyHost;

  CBTHook := SetWindowsHookEx(WH_CBT, CBTHookProc, 0, GetCurrentThreadID);
  Win32Check(CBTHook <> 0);

  InitializeExistingForms;
end;

destructor TGxEditorFormServices.Destroy;
begin
  if CBTHook <> 0 then
  begin
    Win32Check(UnhookWindowsHookEx(CBTHook));
    CBTHook := 0;
  end;

  ClearHostComponents;
  FreeAndNil(FHostComponents);

  ClearListeners;
  FreeAndNil(FListeners);

  inherited Destroy;
end;

function CustomFormIsInFrontOf(const QueriedForm, BaseForm: TCustomForm): Boolean;
var
  i: Integer;
  ScreenCustomForm: TCustomForm;
begin
  if not Assigned(QueriedForm) then
  begin
    Result := False;
    Exit;
  end;

  if not Assigned(BaseForm) {and Assigned(QueriedForm)} then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  for i := 0 to Screen.CustomFormCount do
  begin
    ScreenCustomForm := Screen.CustomForms[i];

    Result := (ScreenCustomForm = QueriedForm);
    if Result then Break;

    if ScreenCustomForm = BaseForm then
      Break;
  end;
end;

procedure TGxEditorFormServices.FindAndAssignCurrentProxyHost;
var
  i: Integer;
  TopMostHostComponent: TGxEditorFormHostComponent;
  AHostComponent: TGxEditorFormHostComponent;
begin
  Assert(Assigned(FHostComponents));
  // We do have a number of host components installed.
  // Iterate over these and find the one which is associated
  // with the top-most editor form. This is then the
  // current proxy host.

  // Optimization: No proxy, nothing current. Do nothing.
  if FHostComponents.Count = 0 then
    Exit;

  FCurrentProxyHost := 0; // First item/proxy (index)

  // Optimization: If we only have one proxy, we know
  // that this is the one and only current (first) one.
  if FHostComponents.Count = 1 then
    Exit;

  TopMostHostComponent := FHostComponents[FCurrentProxyHost] as TGxEditorFormHostComponent;
  for i := 1 to FHostComponents.Count-1 do
  begin
    AHostComponent := FHostComponents[i] as TGxEditorFormHostComponent;
    if CustomFormIsInFrontOf(AHostComponent.GetEditorForm, TopMostHostComponent.GetEditorForm) then
    begin
      FCurrentProxyHost := i;
      TopMostHostComponent := AHostComponent;
    end;
  end;
end;

procedure TGxEditorFormServices.FireEvent(const EventCode: TEditFormEventCode;
  const EditFormProxy: TGxEditorFormHostComponent);
var
  i: Integer;
  AListenerRecord: PListenerRecord;
begin
  {$IFOPT D+} SendDebug(Format('Firing form event %s', [GetEnumName(TypeInfo(TEditFormEventCode), Integer(EventCode))])); {$ENDIF}
  Assert(Assigned(FListeners));
  Assert(Assigned(EditFormProxy));

  for i := 0 to FListeners.Count-1 do
  begin
    AListenerRecord := FListeners[i];
    Assert(Assigned(AListenerRecord));

    try
      {$IFOPT D+} SendDebug(Format('Firing to editor form listener %d', [i])); {$ENDIF}
      AListenerRecord^.ListenerMethod(EventCode, EditFormProxy);
    except
      on E: Exception do
      begin
        GxLogException(E);
        // Swallow exception.
      end;
    end;
  end;
end;

function TGxEditorFormServices.GetCurrentProxy: IGxEditFormProxy;
begin
  Assert(Assigned(FHostComponents));

  // No editor forms have been created (yet - or all
  // available ones have been destroyed).
  if FHostComponents.Count = 0 then
  begin
    Result := nil;
    Exit;
  end;

  if IsInvalidCurrentProxyHost then
  begin
    // We do not have a valid current proxy host
    // (and hence editor form) yet. This may be
    // due to editor form removal or it may happen
    // if we just installed ourselves into the
    // existing editor forms after a request
    // for editor services.
    // (Note that editor form services are only
    // created upon first usage, not up-front on
    // installation.)

    FindAndAssignCurrentProxyHost;

    // If after an attempt to find the current proxy / editor
    // form we still don't have one, give up and return nothing.
    // This really should not happen.
    if IsInvalidCurrentProxyHost then
    begin
      Result := nil;
      Assert(False, 'No current editor form proxy was found although one was requested');
      Exit;
    end;
  end;

  Assert(FCurrentProxyHost >= 0);
  Assert(FCurrentProxyHost < FHostComponents.Count);

  Result := FHostComponents[FCurrentProxyHost] as TGxEditorFormHostComponent;
end;

function TGxEditorFormServices.GetEditorFormProxy(Index: Integer): IGxEditFormProxy;
begin
  Assert(Assigned(FHostComponents));
  Result := FHostComponents[Index] as TGxEditorFormHostComponent;
end;

function TGxEditorFormServices.GetEditorFormProxyCount: Integer;
begin
  Assert(Assigned(FHostComponents));
  Result := FHostComponents.Count;
end;

procedure TGxEditorFormServices.InitializeExistingForms;
var
  i: Integer;
  AForm: TCustomForm;
begin
  {$IFOPT D+} SendDebug('Begin injecting host components into existing forms'); {$ENDIF}
  for i := 0 to Screen.FormCount-1 do
  begin
    AForm := Screen.Forms[i];
    if IsIdeEditorForm(AForm) then
      InjectEditorFormHostComponent(AForm);
  end;
end;

function TGxEditorFormServices.InjectEditorFormHostComponent(
  const EditForm: TCustomForm): TGxEditorFormHostComponent;
var
  AHostComponent: TGxEditorFormHostComponent;
begin
  Assert(Assigned(EditForm));

  AHostComponent := TGxEditorFormHostComponent.Create(EditForm);
  FHostComponents.Add(AHostComponent);
  AHostComponent.FreeNotification(Self);

  {$IFOPT D+} SendDebug('Injected host component into ' + EditForm.Name); {$ENDIF}

  Result := AHostComponent;
end;

procedure TGxEditorFormServices.InvalidateCurrentProxyHost;
begin
  FCurrentProxyHost := InvalidCurrentProxyHost;
end;

function TGxEditorFormServices.IsInvalidCurrentProxyHost: Boolean;
begin
  Result := (FCurrentProxyHost = InvalidCurrentProxyHost);
end;

procedure TGxEditorFormServices.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index: Integer;
begin
  if Operation = opRemove then
  begin
    Assert(Assigned(FHostComponents));

    Index := FHostComponents.IndexOf(AComponent);
    if Index >= 0 then
    begin
      FHostComponents.Delete(Index);
      InvalidateCurrentProxyHost;
    end;
  end;

  inherited Notification(AComponent, Operation);
end;

procedure TGxEditorFormServices.RemoveListener(Listener: TEditFormNotifier);
var
  i: Integer;
  AListenerRecord: PListenerRecord;
begin
  Assert(Assigned(FListeners));
  Assert(Assigned(Listener));

  for i := 0 to FListeners.Count-1 do
  begin
    AListenerRecord := FListeners[i];
    if @AListenerRecord^.ListenerMethod = @Listener then
    begin
      Dispose(AListenerRecord);
      FListeners.Delete(i);

      Break;
    end;
  end;
end;

procedure TGxEditorFormServices.UpdateCurrentProxyHost(
  const EditFormProxy: TGxEditorFormHostComponent);
begin
  Assert(Assigned(EditFormProxy));
  FCurrentProxyHost := FHostComponents.IndexOf(EditFormProxy);
  Assert(FCurrentProxyHost >= 0);
end;

function TGxEditorFormServices._AddRef: Integer;
begin
  Result := 1;
end;

function TGxEditorFormServices._Release: Integer;
begin
  Result := 1;
end;

initialization

finalization

  FreeEditorFormServices;

end.
