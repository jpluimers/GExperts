unit GX_IdeFormChangeManager;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils,
  Classes,
  Controls,
  Forms;

type
  TFormChangeHandle = class end;
  TControlChangeHandle = class end;
  TOnActiveFormChanged = procedure(_Sender: TObject; _NewForm: TCustomForm) of object;
  TOnActiveControlChanged = procedure(_Sender: TObject; _Form: TCustomForm; _Control: TWinControl) of object;

type
  TIDEFormChangeManager = class
  public
    class function RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
    class procedure UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
    class function RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
    class procedure UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
  end;

implementation

uses
  Windows,
  Dialogs,
  ExtCtrls,
  ActnList,
  Menus,
  ComCtrls,
  Messages,
  GX_GenericUtils,
  GX_ConfigurationInfo,
  GX_EventHook,
  GX_dzVclUtils,
  GX_IdeDetectForms,
  GX_IdeUtils;

type
  TFormChangeCallbackItem = class
  private
    FHandle: Pointer;
    FCallback: TOnActiveFormChanged;
  public
    constructor Create(_Handle: Pointer; _Callback: TOnActiveFormChanged);
  end;

  TControlChangeCallbackItem = class
  private
    FHandle: Pointer;
    FCallback: TOnActiveControlChanged;
  public
    constructor Create(_Handle: Pointer; _Callback: TOnActiveControlChanged);
  end;

  TFormChangeManagerInternal = class
  private
    FScreenActiveFormChangeHook: TNotifyEventHook;
    FScreenActiveControlChangeHook: TNotifyEventHook;
    FFormChangeCallbacks: TList;
    FIsInFormChangeCallback: Boolean;
    FControlChangeCallbacks: TList;
    FIsInControlChangeCallback: Boolean;
    FAllowResize: Boolean;
    FRememberPosition: Boolean;
    procedure HookActiveControlChanged;
    procedure ProcessActivatedControl(Form: TCustomForm; Control: TWinControl);
  protected
    constructor Create;
    destructor Destroy; override;
    procedure ScreenActiveFormChange(Sender: TObject);
    procedure ScreenActiveControlChange(Sender: TObject);
    procedure ProcessActivatedForm(Form: TCustomForm);
    ///<summary>
    /// @Result is <> NIL if the registration worked, NIL otherwise </summary>
    function RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
    procedure UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
    ///<summary>
    /// @Result is <> NIL if the registration worked, NIL otherwise </summary>
    function RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
    procedure UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
    property AllowResize: Boolean read FAllowResize write FAllowResize;
    property RememberPosition: Boolean read FRememberPosition write FRememberPosition;
  end;

var
  FormChangeManager: TFormChangeManagerInternal = nil;

function PrivateFormChangeManager: TFormChangeManagerInternal;
begin
  if not Assigned(FormChangeManager) then
    FormChangeManager := TFormChangeManagerInternal.Create;
  Result := FormChangeManager;
end;

{ TFormChangeManagerInternal }

constructor TFormChangeManagerInternal.Create;
begin
  inherited;
  FFormChangeCallbacks := TList.Create;
  FControlChangeCallbacks := TList.Create;

  // hook Screen.OnActiveFormChange using the method describe here:
  // http://blog.dummzeuch.de/safe-event-hooking-for-delphi-ide-plugins/
  FScreenActiveFormChangeHook := TScreenActiveFormChangeHook.Install(ScreenActiveFormChange);

  // hook Screen.OnActiveControlChange
  HookActiveControlChanged;
end;

destructor TFormChangeManagerInternal.Destroy;
var
  i: Integer;
begin
  // unhook first, to be sure none of these hooks is being called after we freed our lists
  if Assigned(FScreenActiveControlChangeHook) then
    TScreenActiveControlChangeHook.Remove(FScreenActiveControlChangeHook);

  if Assigned(FScreenActiveFormChangeHook) then
    TScreenActiveFormChangeHook.Remove(FScreenActiveFormChangeHook);

  if Assigned(FControlChangeCallbacks) then begin
    for i := 0 to FControlChangeCallbacks.Count - 1 do
      TObject(FControlChangeCallbacks[i]).Free;
    FreeAndNil(FControlChangeCallbacks);
  end;

  if Assigned(FFormChangeCallbacks) then begin
    for i := 0 to FFormChangeCallbacks.Count - 1 do
      TObject(FFormChangeCallbacks[i]).Free;
    FreeAndNil(FFormChangeCallbacks);
  end;

  inherited;
end;

procedure TFormChangeManagerInternal.HookActiveControlChanged;
begin
  if Assigned(FScreenActiveControlChangeHook) then
    TScreenActiveControlChangeHook.Remove(FScreenActiveControlChangeHook);
  FScreenActiveControlChangeHook := TScreenActiveControlChangeHook.Install(ScreenActiveControlChange)
end;

procedure TFormChangeManagerInternal.ProcessActivatedControl(Form: TCustomForm; Control: TWinControl);
var
  i: Integer;
begin
  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  FIsInControlChangeCallback := True;
  try
    for i := FControlChangeCallbacks.Count - 1 downto 0 do
      TControlChangeCallbackItem(FControlChangeCallbacks[i]).FCallback(Self, Form, Control);
  finally
    FIsInControlChangeCallback := False;
  end;

end;

{$IFDEF SEARCH_PATH_REDRAW_FIX_ENABLED}

function HasRedrawProblems(_Form: TCustomForm): Boolean;
begin
  Result := IsSarchPathForm(_Form) and IsThemingEnabled;
end;
{$ELSE ~SEARCH_PATH_REDRAW_FIX_ENABLED}

function HasRedrawProblems(_Form: TCustomForm): Boolean;
begin
  Result := False;
end;
{$ENDIF SEARCH_PATH_REDRAW_FIX_ENABLED}

procedure TFormChangeManagerInternal.ProcessActivatedForm(Form: TCustomForm);
var
  i: Integer;
  NeedsWorkaround: Boolean;
begin
  Assert(Assigned(Form));

  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  FIsInFormChangeCallback := True;
  try
    // Workaround for redraw issue in Delphi 10.2 when theming is enabled:
    // https://sourceforge.net/p/gexperts/bugs/86/
    // Disable redraws while we change the window
    // Unfortunately this seems to work only for the Search Path dialog, but not for the Goto dialog.
    NeedsWorkaround := HasRedrawProblems(Form);
    if NeedsWorkaround then
      SendMessage(Form.Handle, WM_SETREDRAW, WParam(False), 0);
    try
      for i := FFormChangeCallbacks.Count - 1 downto 0 do begin
        try
          TFormChangeCallbackItem(FFormChangeCallbacks[i]).FCallback(Self, Form);
        except
          // just so we can have a look at any exceptions that might be raised ...
          raise;
        end;
      end;
    finally
      if NeedsWorkaround then begin
        // Allow and force a redraw of the whole window
        SendMessage(Form.Handle, WM_SETREDRAW, WParam(True), 0);
        RedrawWindow(Form.Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
        // see Remarks on
        // https://docs.microsoft.com/en-us/windows/desktop/gdi/wm-setredraw
      end;
    end;
  finally
    FIsInFormChangeCallback := False;
  end;
end;

function TFormChangeManagerInternal.RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
begin
  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  if FIsInControlChangeCallback then
    raise Exception.Create('Cannot register new ControlChange callback while callbacks are active.');
  Result := Pointer(FControlChangeCallbacks.Count + 1);
  FControlChangeCallbacks.Add(TControlChangeCallbackItem.Create(Result, _Callback));
  HookActiveControlChanged;
end;

function TFormChangeManagerInternal.RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
begin
  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  if FIsInFormChangeCallback then
    raise Exception.Create('Cannot register new FormChange callback while callbacks are active.');
  Result := Pointer(FFormChangeCallbacks.Count + 1);
  FFormChangeCallbacks.Add(TFormChangeCallbackItem.Create(Result, _Callback));
end;

procedure TFormChangeManagerInternal.ScreenActiveControlChange(Sender: TObject);
begin
  if Assigned(Screen) then
    if Assigned(Screen.ActiveCustomForm) and Assigned(Screen.ActiveControl) then
      ProcessActivatedControl(Screen.ActiveCustomForm, Screen.ActiveControl);
end;

procedure TFormChangeManagerInternal.ScreenActiveFormChange(Sender: TObject);
begin
  if Assigned(Screen) then
    if Assigned(Screen.ActiveCustomForm) then
      ProcessActivatedForm(Screen.ActiveCustomForm);
end;

procedure TFormChangeManagerInternal.UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
var
  i: Integer;
  Item: TFormChangeCallbackItem;
begin
  for i := 0 to FControlChangeCallbacks.Count - 1 do begin
    Item := FControlChangeCallbacks[i];
    if Item.FHandle = _Handle then begin
      FControlChangeCallbacks.Delete(i);
      Item.Free;
      Exit;
    end;
  end;
end;

procedure TFormChangeManagerInternal.UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
var
  i: Integer;
  Item: TFormChangeCallbackItem;
begin
  for i := 0 to FFormChangeCallbacks.Count - 1 do begin
    Item := FFormChangeCallbacks[i];
    if Item.FHandle = _Handle then begin
      FFormChangeCallbacks.Delete(i);
      Item.Free;
      Exit;
    end;
  end;
end;

class function TIDEFormChangeManager.RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
begin
  Result := PrivateFormChangeManager.RegisterFormChangeCallback(_Callback)
end;

class procedure TIDEFormChangeManager.UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
begin
  PrivateFormChangeManager.UnregisterFormChangeCallback(_Handle)
end;

class function TIDEFormChangeManager.RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
begin
  Result := PrivateFormChangeManager.RegisterControlChangeCallback(_Callback)
end;

class procedure TIDEFormChangeManager.UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
begin
  PrivateFormChangeManager.UnregisterControlChangeCallback(_Handle)
end;

{ TFormChangeCallbackItem }

constructor TFormChangeCallbackItem.Create(_Handle: Pointer; _Callback: TOnActiveFormChanged);
begin
  inherited Create;
  FHandle := _Handle;
  FCallback := _Callback;
end;

{ TControlChangeCallbackItem }

constructor TControlChangeCallbackItem.Create(_Handle: Pointer; _Callback: TOnActiveControlChanged);
begin
  inherited Create;
  FHandle := _Handle;
  FCallback := _Callback;
end;

initialization
finalization
  FreeAndNil(FormChangeManager);
end.
