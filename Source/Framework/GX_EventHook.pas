unit GX_EventHook;

interface

uses
  SysUtils,
  Classes;

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

function HookScreenActiveFormChange(_HookEvent: TNotifyEvent): TNotifyEventHook;
procedure UnhookScreenActiveFormChange(_Hook: TNotifyEventHook);

function HookScreenActiveControlChange(_HookEvent: TNotifyEvent): TNotifyEventHook;
procedure UnhookScreenActiveControlChange(_Hook: TNotifyEventHook);

implementation

uses
  Forms;

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

/// Assume you want to hook Screen.OnActiveFormChange (which is a TNotifyEvent) with
/// the method HandleActiveFormChange of your already existing object MyObject.
/// So you call HookScreenActiveFormChange like this:
/// Hook := HookScreenActiveFormChange(TMethod(MyObject.HandleActiveFormChange);

function HookScreenActiveFormChange(_HookEvent: TNotifyEvent): TNotifyEventHook;
begin
  Result := TNotifyEventHook.Create(TMethod(Screen.OnActiveFormChange), TMethod(_HookEvent));
  Screen.OnActiveFormChange := Result.HandleEvent;
end;

procedure UnhookScreenActiveFormChange(_Hook: TNotifyEventHook);
var
  Ptr: TMethod;
begin
 if not Assigned(_Hook) then begin
   // Just in case somebody did not check whether HookScreenActiveFormChange actually returned
   // a valid object or simply didn't call it.
   Exit;
 end;

  Ptr := TMethod(Screen.OnActiveFormChange);
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
      Screen.OnActiveFormChange := TNotifyEvent(_Hook.OrigEvent);
      _Hook.Free;
      Exit;
    end;
    // check the next event in the chain
    Ptr := TMethod(TNotifyEventHook(Ptr.Data).OrigEvent);
  end;

  // If we get here, somebody who does not adhere to this standard has changed the event.
  // The best thing we can do, is Assign NIL to the HookEvent so it no longer gets called.
  // We cannot free the hook because there might the somebody might still have reference
  // to _Hook.HandleEvent.
  _Hook.HookEvent.Code := nil;
  _Hook.HookEvent.Data := nil;
end;

/// Assume you want to hook Screen.OnActiveControlChange (which is a TNotifyEvent) with
/// the method HandleActiveFormChange of your already existing object MyObject.
/// So you call HookScreenActiveFormChange like this:
/// Hook := HookScreenActiveFormChange(TMethod(MyObject.HandleActiveFormChange);

function HookScreenActiveControlChange(_HookEvent: TNotifyEvent): TNotifyEventHook;
begin
  Result := TNotifyEventHook.Create(TMethod(Screen.OnActiveControlChange), TMethod(_HookEvent));
  Screen.OnActiveControlChange := Result.HandleEvent;
end;

procedure UnhookScreenActiveControlChange(_Hook: TNotifyEventHook);
var
  Ptr: TMethod;
begin
 if not Assigned(_Hook) then begin
   // Just in case somebody did not check whether HookScreenActiveFormChange actually returned
   // a valid object or simply didn't call it.
   Exit;
 end;

  Ptr := TMethod(Screen.OnActiveControlChange);
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
      Screen.OnActiveControlChange := TNotifyEvent(_Hook.OrigEvent);
      _Hook.Free;
      Exit;
    end;
    // check the next event in the chain
    Ptr := TMethod(TNotifyEventHook(Ptr.Data).OrigEvent);
  end;

  // If we get here, somebody who does not adhere to this standard has changed the event.
  // The best thing we can do, is Assign NIL to the HookEvent so it no longer gets called.
  // We cannot free the hook because there might the somebody might still have reference
  // to _Hook.HandleEvent.
  _Hook.HookEvent.Code := nil;
  _Hook.HookEvent.Data := nil;
end;

end.




