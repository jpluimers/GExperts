unit GX_EventHook;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms;

type
  ///<summary>
  /// This class is meant to be used as the destination of a TNotifyEvent.
  /// It is kept very simple on purpose. Use it as is, do *not* add to it or derive from it, do
  /// *not* rename the class.
  /// See the procedure below on how to use it.
  TNotifyEventHook = class
  public
    ///<summary>
    /// Store the original event here. </summary>
    OrigEvent: TMethod;
    ///<summary>
    /// Store the event you want to call here. </summary>
    HookEvent: TMethod;
    ///<summary>
    /// Assign this method to the Event you want to hook.
    /// It first calls HookEvent and afterwards OrigEvent. </summary>
    procedure HandleEvent(_Sender: TObject); virtual;
    constructor Create(_OrigEvent: TMethod; _HookEvent: TMethod);
  end;

type
  ///<summary>
  /// This class is meant to be used as the destination of a TMessageEvent.
  /// It is kept very simple on purpose. Use it as is, do *not* add to it or derive from it, do
  /// *not* rename the class.
  /// See the procedure below on how to use it.
  TMessageEventHook = class
  public
    ///<summary>
    /// Store the original event here. </summary>
    OrigEvent: TMethod;
    ///<summary>
    /// Store the event you want to call here. </summary>
    HookEvent: TMethod;
    ///<summary>
    /// Assign this method to the Event you want to hook.
    /// It first calls HookEvent and afterwards OrigEvent. </summary>
    procedure HandleEvent(var _Msg: TMsg; var _Handled: Boolean); virtual;
    constructor Create(_OrigEvent: TMethod; _HookEvent: TMethod);
  end;

type
  ///<summary>
  /// This class provides the abstract declarations of two class methods:
  /// GetEvent returns the original event handler, typecasted to TMethod
  /// SetEvent sets the replacement event handler, typecated from TMethod
  /// Both methods must be overriden to access an actual event. </summary>
  TSecureEventHook = class
  protected
    class function GetEvent: TMethod; virtual; abstract;
    class procedure SetEvent(_Value: TMethod); virtual; abstract;
  end;

  ///<summary>
  /// Provides the Methods Install and Remove to hook/unhook an event of type TNotifyEvent
  /// It uses the inherited virtual methods GetEvent and SetEvent to access the actual
  /// event. Since GetEvent/SetEvent are still abstract, a descendant is required
  /// that implements them. </summary>
  TSecureNotifyEventHook = class(TSecureEventHook)
  public
    class function Install(_HookEvent: TNotifyEvent): TNotifyEventHook;
    class procedure Remove(_Hook: TNotifyEventHook);
  end;

type
  ///<summary>
  /// Implements the GetEvent/SetEvent methods to access Screen.ActiveFormChange </summary>
  TScreenActiveFormChangeHook = class(TSecureNotifyEventHook)
  protected
    class function GetEvent: TMethod; override;
    class procedure SetEvent(_Value: TMethod); override;
  end;

type
  ///<summary>
  /// Implements the GetEvent/SetEvent methods to access Screen.ActiveControlChange </summary>
  TScreenActiveControlChangeHook = class(TSecureNotifyEventHook)
  protected
    class function GetEvent: TMethod; override;
    class procedure SetEvent(_Value: TMethod); override;
  end;

type
  ///<summary>
  /// Provides the Methods Install and Remove to hook/unhook an event of type TMessageEvent
  /// It uses the inherited virtual methods GetEvent and SetEvent to access the actual
  /// event. Since GetEvent/SetEvent are still abstract, a descendant is required
  /// that implements them. </summary>
  TSecureMessageEventHook = class(TSecureEventHook)
    class function Install(_HookEvent: TMessageEvent): TMessageEventHook;
    class procedure Remove(_Hook: TMessageEventHook);
  end;

type
  ///<summary>
  /// Implements the GetEvent/SetEvent methods to access Application.OnMessage </summary>
  TApplicationOnMessageHook = class(TSecureMessageEventHook)
    class function GetEvent: TMethod; override;
    class procedure SetEvent(_Value: TMethod); override;
  end;

implementation

{ TNotifyEventHook }

constructor TNotifyEventHook.Create(_OrigEvent, _HookEvent: TMethod);
begin
  inherited Create;
  OrigEvent := _OrigEvent;
  HookEvent := _HookEvent;
end;

procedure TNotifyEventHook.HandleEvent(_Sender: TObject);
begin
  if Assigned(HookEvent.Data) and Assigned(HookEvent.Code) then
    TNotifyEvent(HookEvent)(_Sender);
  if Assigned(OrigEvent.Data) and Assigned(OrigEvent.Code) then
    TNotifyEvent(OrigEvent)(_Sender);
end;

{ TMessageEventHook }

constructor TMessageEventHook.Create(_OrigEvent, _HookEvent: TMethod);
begin
  inherited Create;
  OrigEvent := _OrigEvent;
  HookEvent := _HookEvent;
end;

procedure TMessageEventHook.HandleEvent(var _Msg: TMsg; var _Handled: Boolean);
begin
  if Assigned(HookEvent.Data) and Assigned(HookEvent.Code) then
    TMessageEvent(HookEvent)(_Msg, _Handled);
  if Assigned(OrigEvent.Data) and Assigned(OrigEvent.Code) then
    TMessageEvent(OrigEvent)(_Msg, _Handled);
end;

{ TSecureNotifyEventHook }

class function TSecureNotifyEventHook.Install(_HookEvent: TNotifyEvent): TNotifyEventHook;
var
  evt: TNotifyEvent;
begin
  Result := TNotifyEventHook.Create(GetEvent, TMethod(_HookEvent));
  evt := Result.HandleEvent;
  SetEvent(TMethod(evt));
end;

class procedure TSecureNotifyEventHook.Remove(_Hook: TNotifyEventHook);
var
  Ptr: TMethod;
begin
  if not Assigned(_Hook) then begin
   // Just in case somebody did not check whether HookScreenActiveFormChange actually returned
   // a valid object or simply didn't call it.
    Exit;
  end;

  Ptr := GetEvent;
  if not Assigned(Ptr.Data) and not Assigned(Ptr.Code) then begin
    // Somebody has assigned NIL to the event.
    // It's probably safe to assume that there will be no reference to our hook left, so we just
    // free the object and be done.
    _Hook.Free;
    Exit;
  end;

  while TObject(Ptr.Data).ClassNameIs('TNotifyEventHook') do begin
    // Somebody who knows about this standard has hooked the event.
    // (Remember: Do not change the class name or the class structure. Otherwise this
    //  check will fail!)
    // Let's check whether we can find our own hook in the chain.
    if Ptr.Data = _Hook then begin
      // We are lucky, nobody has tampered with the event, we can just assign the original event,
      // free the hook object and be done with it.
      SetEvent(_Hook.OrigEvent);
      _Hook.Free;
      Exit;
    end;
    // check the next event in the chain
    Ptr := TMethod(TNotifyEventHook(Ptr.Data).OrigEvent);
  end;

  // If we get here, somebody who does not adhere to this standard has changed the event.
  // The best thing we can do, is Assign NIL to the HookEvent so it no longer gets called.
  // We cannot free the hook because somebody might still have reference
  // to _Hook.HandleEvent.
  _Hook.HookEvent.Code := nil;
  _Hook.HookEvent.Data := nil;
end;

/// Assume you want to hook Screen.OnActiveFormChange (which is a TNotifyEvent) with
/// the method HandleActiveFormChange of your already existing object MyObject.
/// So you call HookScreenActiveFormChange like this:
/// Hook := TScreenActiveFormChangeHook.Hook(MyObject.HandleActiveFormChange);

class function TScreenActiveFormChangeHook.GetEvent: TMethod;
begin
  Result := TMethod(Screen.OnActiveFormChange);
end;

class procedure TScreenActiveFormChangeHook.SetEvent(_Value: TMethod);
begin
  Screen.OnActiveFormChange := TNotifyEvent(_Value);
end;

/// Assume you want to hook Screen.OnActiveControlChange (which is a TNotifyEvent) with
/// the method HandleActiveFormChange of your already existing object MyObject.
/// So you call HookScreenActiveFormChange like this:
/// Hook := TScreenActiveControlChangeHook.Hook(MyObject.HandleActiveFormChange);

class function TScreenActiveControlChangeHook.GetEvent: TMethod;
begin
  Result := TMethod(Screen.OnActiveControlChange);
end;

class procedure TScreenActiveControlChangeHook.SetEvent(_Value: TMethod);
begin
  Screen.OnActiveControlChange := TNotifyEvent(_Value);
end;

class function TSecureMessageEventHook.Install(_HookEvent: TMessageEvent): TMessageEventHook;
var
  evt: TMessageEvent;
begin
  Result := TMessageEventHook.Create(GetEvent, TMethod(_HookEvent));
  evt := Result.HandleEvent;
  SetEvent(TMethod(evt));
end;

class procedure TSecureMessageEventHook.Remove(_Hook: TMessageEventHook);
var
  Ptr: TMethod;
begin
  if not Assigned(_Hook) then begin
   // Just in case somebody did not check whether HookApplicationOnMessage actually returned
   // a valid object or simply didn't call it.
    Exit;
  end;

  Ptr := GetEvent;
  if not Assigned(Ptr.Data) and not Assigned(Ptr.Code) then begin
    // Somebody has assigned NIL to the event.
    // It's probably safe to assume that there will be no reference to our hook left, so we just
    // free the object and be done.
    _Hook.Free;
    Exit;
  end;

  while TObject(Ptr.Data).ClassNameIs('TMessageEventHook') do begin
    // Somebody who knows about this standard has hooked the event.
    // (Remember: Do not change the class name or the class structure. Otherwise this
    //  check will fail!)
    // Let's check whether we can find our own hook in the chain.
    if Ptr.Data = _Hook then begin
      // We are lucky, nobody has tampered with the event, we can just assign the original event,
      // free the hook object and be done with it.
      SetEvent(_Hook.OrigEvent);
      _Hook.Free;
      Exit;
    end;
    // check the next event in the chain
    Ptr := TMethod(TMessageEventHook(Ptr.Data).OrigEvent);
  end;

  // If we get here, somebody who does not adhere to this standard has changed the event.
  // The best thing we can do, is Assign NIL to the HookEvent so it no longer gets called.
  // We cannot free the hook because somebody might still have reference
  // to _Hook.HandleEvent.
  _Hook.HookEvent.Code := nil;
  _Hook.HookEvent.Data := nil;
end;

class function TApplicationOnMessageHook.GetEvent: TMethod;
begin
  Result := TMethod(Application.OnMessage);
end;

class procedure TApplicationOnMessageHook.SetEvent(_Value: TMethod);
begin
  Application.OnMessage := TMessageEvent(_Value);
end;

end.
