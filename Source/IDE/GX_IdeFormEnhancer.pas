unit GX_IdeFormEnhancer;

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
  TIDEFormEnhancements = class
  public
    class function GetEnabled: Boolean;// static;
    class procedure SetEnabled(const Value: Boolean); //static;
    class function GetAllowResize: Boolean; //static;
    class procedure SetAllowResize(const Value: Boolean); //static;
    class function GetRememberPosition: Boolean; //static;
    class procedure SetRememberPosition(const Value: Boolean); //static;
    // Delphi 8- can't handle class properties
    //class property Enabled: Boolean read GetEnabled write SetEnabled;
    class function RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
    class procedure UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
    class function RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
    class procedure UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
  end;

implementation

uses Windows, Dialogs, ExtCtrls,
  ActnList, Menus, ExtDlgs, StdCtrls, TypInfo, ComCtrls, Contnrs,
  GX_GenericUtils, GX_ConfigurationInfo, GX_IdeUtils, GX_EventHook,
  GX_dzVclUtils, GX_dzClassUtils, GX_IdeManagedForm;

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

  TFormChangeManager = class(TObject)
  private
    FScreenActiveFormChangeHook: TNotifyEventHook;
    FScreenActiveControlChangeHook: TNotifyEventHook;
    FFormChangeCallbacks: TList;
    FIsInFormChangeCallback: Boolean;
    FControlChangeCallbacks: TList;
    FIsInControlChangeCallback: Boolean;
    FAllowResize: boolean;
    FRememberPosition: Boolean;
    procedure HookActiveControlChanged;
    procedure ProcessActivatedControl(Form: TCustomForm; Control: TWinControl);
  protected
    constructor Create;
    destructor Destroy; override;
    function ShouldManageForm(Form: TCustomForm; var Changes: TFormChanges): Boolean;
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

const
  FormsToChange: array[0..10] of TFormChanges = (
    (
      FormClassNames: 'TSrchDialog';
      FormEnhancer: TManagedFormSrchDialog;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: True;
      RememberPosition: False;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TRplcDialog';
      FormEnhancer: TManagedFormRplcDialog;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: True;
      RememberPosition: False;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TDefaultEnvironmentDialog';
      FormEnhancer: TManagedForm;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: 'Together;Modeling';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TImageListEditor';
      FormEnhancer: TManagedFormImageListEditor;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: True;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TPictureEditorDlg';
      FormEnhancer: TManagedFormPictureEditDlg;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: True;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TProjectOptionsDialog;TDelphiProjectOptionsDialog;'
        + 'TLoadProcessDialog;TDotNetOptionForm;TPasEditorPropertyDialog;'
        + 'TCppProjOptsDlg;TReopenMenuPropertiesDialog;'
        + 'TActionListDesigner;TFieldsEditor;TDBGridColumnsEditor;TDriverSettingsForm';
      FormEnhancer: TManagedForm;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TDelphiProjectOptionsDialog';
      FormEnhancer: TManagedForm;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 0; // changing the dropdown count seems to result in intermittent invalid pointer operations in Delphi 10.1
    ),
    (
      FormClassNames: 'TPasEnvironmentDialog';
      FormEnhancer: TManagedFormPasEnvironmentDialog;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TConnEditForm';
      FormEnhancer: TManagedFormConnEditForm;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TPakComponentsDlg';
      FormEnhancer: TManagedFormPakComponentsDlg;
      MakeResizable: True;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: True;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TOrderedListEditDlg;TInheritedListEditDlg;TBufferListFrm;TMenuBuilder';
      FormEnhancer: TManagedForm;
      MakeResizable: False;
      RememberSize: True;
      RememberWidth: False;
      RememberPosition: True;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    )
  );

var
  PrivateFormChangeManager: TFormChangeManager = nil;

{ TFormChangeManager }

constructor TFormChangeManager.Create;
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

destructor TFormChangeManager.Destroy;
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

procedure TFormChangeManager.HookActiveControlChanged;
begin
  if Assigned(FScreenActiveControlChangeHook) then
    TScreenActiveControlChangeHook.Remove(FScreenActiveControlChangeHook);
  FScreenActiveControlChangeHook := TScreenActiveControlChangeHook.Install(ScreenActiveControlChange)
end;

procedure TFormChangeManager.ProcessActivatedControl(Form: TCustomForm; Control: TWinControl);
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

procedure TFormChangeManager.ProcessActivatedForm(Form: TCustomForm);
var
  Changes: TFormChanges;
  i: Integer;
  Enhancer: TManagedForm;
begin
  Assert(Assigned(Form));

  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  FIsInFormChangeCallback := True;
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
    FIsInFormChangeCallback := False;
  end;

  if ShouldManageForm(Form, Changes) then
  begin
    Changes.MakeResizable := Changes.MakeResizable and AllowResize;
    Changes.RememberSize := Changes.RememberSize and AllowResize;
    Changes.RememberPosition := Changes.RememberPosition and RememberPosition;
    Changes.RememberSplitterPosition := Changes.RememberSplitterPosition;
    Changes.RememberWidth := Changes.RememberWidth and AllowResize;
    Changes.CollapseTreeNodes := Changes.CollapseTreeNodes;
    Changes.ResizePictureDialogs := Changes.ResizePictureDialogs;
    Changes.ComboDropDownCount := Changes.ComboDropDownCount;
    Enhancer := Changes.FormEnhancer.Create(Form);
    Enhancer.Init(Changes);
  end;
end;

function TFormChangeManager.RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
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

function TFormChangeManager.RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
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

procedure TFormChangeManager.ScreenActiveControlChange(Sender: TObject);
begin
  if Assigned(Screen) then
    if Assigned(Screen.ActiveCustomForm) and Assigned(Screen.ActiveControl) then
      ProcessActivatedControl(Screen.ActiveCustomForm, Screen.ActiveControl);
end;

procedure TFormChangeManager.ScreenActiveFormChange(Sender: TObject);
begin
  if Assigned(Screen) then
    if Assigned(Screen.ActiveCustomForm) then
      ProcessActivatedForm(Screen.ActiveCustomForm);
end;

// fix for RSP-13229: File -> New -> Other opens on different monitor
//     and RSP-13230: on dual monitor Project -> Resources and images gets shown on primary monitor
// which occur in Delphi 2009 to 10.0 Berlin
{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
procedure FixFormPositioning1(_frm: TForm);
begin
  if (_frm.ClassName = 'TGalleryBrowseDlg') or (_frm.ClassName = 'TProjectResourcesDlg') then begin
    try
      // this results in an EInvalidOperation exception the first time it is called ...
      _frm.Position := poDesigned;
    except
      on EInvalidOperation do
        ; // ... which we ignore
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
// Replace Dialog is placed on the primary monitor, occurs in Delphi 2005 to 2007
procedure FixFormPositioning2(_frm: TForm);
var
  AppBuilder: TCustomForm;
  Monitor: tmonitor;
begin
  if (_frm.ClassName = 'TRplcDialog') then begin
    AppBuilder := GetIdeMainForm;
    Assert(Assigned(AppBuilder));
    Monitor := AppBuilder.Monitor;
    _frm.Left := Monitor.Left + (Monitor.Width - _frm.Width) div 2;
    _frm.Top := Monitor.Top + (Monitor.Height - _frm.Height) div 2;
  end;
end;
{$ENDIF}
{$ENDIF}


function TFormChangeManager.ShouldManageForm(Form: TCustomForm;
  var Changes: TFormChanges): Boolean;
var
  i: Integer;
  Control: TControl;
  ClsName: string;
  cmp: TComponent;
begin
  Assert(Assigned(Form));
  Result := False;
  ClsName := Form.ClassName;
  if (csDesigning in Form.ComponentState)
    or StringInArray(ClsName, ['TAppBuilder', 'TMessageForm', 'TPropertyInspector', 'TObjectTreeView', 'TEditWindow']) then
    Exit;
  if (ClsName = 'TAboutBox') then  // Or TAboutBoxHiColor in D7-
  begin
    if RunningDelphi8OrGreater then
    begin
      Control := Form.FindChildControl('InstalledProducts');
      if Assigned(Control) then
      begin // Remove the horizontal scrollbar
        Control.Left := Control.Left - 1;
        Control.Width := Control.Width + 4;
      end;
    end;
  end;

{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
  FixFormPositioning2(TForm(Form));
{$ENDIF}
{$ENDIF}

{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
  FixFormPositioning1(TForm(Form));
{$ENDIF}
{$ENDIF}

  if not TComponent_FindComponent(Form, TManagedForm.GenerateName(Form.Name), False, cmp, TManagedForm) then begin
    for i := Low(FormsToChange) to High(FormsToChange) do begin
      if StrContains(ClsName + ';', FormsToChange[i].FormClassNames + ';') then begin
        Result := True;
        Changes := FormsToChange[i];
        Break;
      end;
    end;
  end;
end;

procedure TFormChangeManager.UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
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

procedure TFormChangeManager.UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
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

{ TIDEFormEnhancements }

class function TIDEFormEnhancements.GetAllowResize: Boolean;
begin
  Result := Assigned(PrivateFormChangeManager) and PrivateFormChangeManager.AllowResize;
end;

class function TIDEFormEnhancements.GetEnabled: Boolean;
begin
  Result := Assigned(PrivateFormChangeManager);
end;

class function TIDEFormEnhancements.GetRememberPosition: Boolean;
begin
  Result := Assigned(PrivateFormChangeManager) and PrivateFormChangeManager.RememberPosition;
end;

class procedure TIDEFormEnhancements.SetAllowResize(const Value: Boolean);
begin
  if Assigned(PrivateFormChangeManager) then
    PrivateFormChangeManager.AllowResize := Value;
end;

class procedure TIDEFormEnhancements.SetEnabled(const Value: Boolean);
begin
  if Value and (not Assigned(PrivateFormChangeManager)) then
    PrivateFormChangeManager := TFormChangeManager.Create
  else if not Value then
    FreeAndNil(PrivateFormChangeManager);
end;

class procedure TIDEFormEnhancements.SetRememberPosition(const Value: Boolean);
begin
  if Assigned(PrivateFormChangeManager) then
    PrivateFormChangeManager.RememberPosition := Value;
end;

class function TIDEFormEnhancements.RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
begin
  if Assigned(PrivateFormChangeManager) then
    Result := PrivateFormChangeManager.RegisterFormChangeCallback(_Callback)
  else
    Result := nil;
end;

class procedure TIDEFormEnhancements.UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
begin
  if Assigned(PrivateFormChangeManager) then
    PrivateFormChangeManager.UnregisterFormChangeCallback(_Handle)
end;

class function TIDEFormEnhancements.RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
begin
  if Assigned(PrivateFormChangeManager) then
    Result := PrivateFormChangeManager.RegisterControlChangeCallback(_Callback)
  else
    Result := nil;
end;

class procedure TIDEFormEnhancements.UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
begin
  if Assigned(PrivateFormChangeManager) then
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
  TIDEFormEnhancements.SetEnabled(False);

end.

