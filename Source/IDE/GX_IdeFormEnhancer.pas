unit GX_IdeFormEnhancer;

interface

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
  GX_GenericUtils, GX_ConfigurationInfo, GX_IdeUtils, GX_EventHook;

type
  THackForm = class(TCustomForm);

  TFormChanges = record
    // Source-defined
    FormClassNames: string;
    MakeResizable: Boolean;
    RememberSize: Boolean;
    RememberWidth: Boolean;
    RememberPosition: Boolean;
    RememberSplitterPosition: Boolean;
    CollapseTreeNodes: string;
    ResizePictureDialogs: Boolean;
    ComboDropDownCount: Integer;
  end;

  TManagedForm = class(TObject)
    // Source-defined
    FormClassName: string;
    MakeResizable: Boolean;
    RememberSize: Boolean;
    RememberWidth: Boolean;
    RememberPosition: Boolean;
    RememberSplitterPosition: Boolean;
    CollapseTreeNodes: string;
    ResizePictureDialogs: Boolean;
    ComboDropDownCount: Integer;
    // Runtime properties
    Handle: HWnd;
    OldDestroy: TNotifyEvent;
    // Methods
    destructor Destroy; override;
    function GetRegistryKey: string;
    function FindSplitPanel(Form: TCustomForm): TCustomPanel;
    procedure SetComboDropDownCount(Control: TControl);

    procedure DoMakeResizable(Form: TCustomForm);
    procedure DoCollapseTreeNodes(Form: TCustomForm);
    procedure DoSaveFormState(Form: TCustomForm);
    procedure DoLoadFormState(Form: TCustomForm);
    procedure DoResizePictureDialogs(Form: TCustomForm);
    procedure DoComboDropDownCount(Form: TCustomForm);
  end;

  TManagedFormList = class(TObjectList)
    function GetForm(Form: TCustomForm): TManagedForm; overload;
    function GetForm(Index: Integer): TManagedForm; overload;
  end;

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
    FManagedForms: TManagedFormList;
    FFormChangeCallbacks: TList;
    FIsInFormChangeCallback: boolean;
    FControlChangeCallbacks: TList;
    FIsInControlChangeCallback: boolean;
    procedure HookActiveControlChanged;
    procedure ProcessActivatedControl(Form: TCustomForm; Control: TWinControl);
  protected
    constructor Create;
    destructor Destroy; override;
    function ShouldManageForm(Form: TCustomForm; var Changes: TFormChanges): Boolean;
    procedure ScreenActiveFormChange(Sender: TObject);
    procedure ScreenActiveControlChange(Sender: TObject);
    procedure ProcessActivatedForm(Form: TCustomForm);
    function FindScreenForm(const FormClassName: string; Handle: HWND): TCustomForm;
    procedure FormDestroy(Sender: TObject);
    function FindManagedForm(Form: TCustomForm; var ManagedForm: TManagedForm): Boolean;
    function IsManagingForm(Form: TCustomForm): Boolean;
    procedure ModifyForm(Form: TCustomForm; ManagedForm: TManagedForm);
    ///<summary>
    /// @Result is <> NIL if the registration worked, NIL otherwise </summary>
    function RegisterFormChangeCallback(_Callback: TOnActiveFormChanged): TFormChangeHandle;
    procedure UnregisterFormChangeCallback(_Handle: TFormChangeHandle);
    ///<summary>
    /// @Result is <> NIL if the registration worked, NIL otherwise </summary>
    function RegisterControlChangeCallback(_Callback: TOnActiveControlChanged): TControlChangeHandle;
    procedure UnregisterControlChangeCallback(_Handle: TControlChangeHandle);
  end;

const
  FormsToChange: array[0..4] of TFormChanges = (
    (
      FormClassNames: 'TSrchDialog';
      MakeResizable: False;
      RememberSize: False;
      RememberWidth: True;
      RememberPosition: False;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: False;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TDefaultEnvironmentDialog';
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
      FormClassNames: 'TImageListEditor;TPictureEditorDlg';
      MakeResizable: False;
      RememberSize: False;
      RememberWidth: False;
      RememberPosition: False;
      RememberSplitterPosition: False;
      CollapseTreeNodes: '';
      ResizePictureDialogs: True;
      ComboDropDownCount: 15;
    ),
    (
      FormClassNames: 'TProjectOptionsDialog;TDelphiProjectOptionsDialog;TLoadProcessDialog;TDotNetOptionForm;TPasEnvironmentDialog;TPasEditorPropertyDialog;TCppProjOptsDlg;TReopenMenuPropertiesDialog';
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
      FormClassNames: 'TOrderedListEditDlg;TInheritedListEditDlg';
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

  WidthIdent = 'Width';
  HeightIdent = 'Height';
  TopIdent = 'Top';
  LeftIdent = 'Left';
  SplitPosIdent = 'SplitPos';

var
  PrivateFormChangeManager: TFormChangeManager = nil;

destructor TManagedForm.Destroy;
var
  Form: TCustomForm;
begin
  Form := PrivateFormChangeManager.FindScreenForm(FormClassName, Handle);
  if Assigned(Form) then
    THackForm(Form).OnDestroy := OldDestroy;
  inherited;
end;

function TManagedForm.FindSplitPanel(Form: TCustomForm): TCustomPanel;
var
  i: Integer;
  Control: TControl;
begin
  Result := nil;
  for i := 0 to Form.ControlCount - 1 do
  begin
    Control := Form.Controls[i];
    if (Control is TCustomPanel) and (Control.Align = alLeft) then
    begin
      Result := Control as TCustomPanel;
      Break;
    end;
  end;
end;

function TManagedForm.GetRegistryKey: string;
begin
  Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + 'IDEForms';
end;

procedure TManagedForm.DoCollapseTreeNodes(Form: TCustomForm);
var
  LeftPanel: TCustomPanel;
  Node: TTreeNode;
  Nodes: TStringList;
  i: Integer;
begin
  if IsEmpty(CollapseTreeNodes) then
    Exit;
  Nodes := TStringList.Create;
  try
    AnsiStrTok(CollapseTreeNodes, ';', Nodes);
    for i := 0 to Nodes.Count - 1 do
    begin
      Node := nil;
      LeftPanel := FindSplitPanel(Form);
      if leftPanel.ControlCount > 0 then
        if LeftPanel.Controls[0] is TTreeView then
          Node := FindTreeNode(LeftPanel.Controls[0] as TTreeView, Nodes[i]);
      if Assigned(Node) then
        Node.Collapse(False);
    end;
  finally
    FreeAndNil(Nodes);
  end;
end;

procedure TManagedForm.SetComboDropDownCount(Control: TControl);
begin
  if StrContains('Combo', Control.ClassName) or StrContains('ColorBox', Control.ClassName) then
  begin
    if TypInfo.IsPublishedProp(Control, 'DropDownCount') then
      if TypInfo.PropIsType(Control, 'DropDownCount', tkInteger) then
        if GetPropValue(Control, 'DropDownCount', False) < ComboDropDownCount then
          TypInfo.SetPropValue(Control, 'DropDownCount', ComboDropDownCount);
  end;
end;

procedure TManagedForm.DoComboDropDownCount(Form: TCustomForm);
begin
  if ComboDropDownCount > 0 then
    IterateOverControls(Form, SetComboDropDownCount);
end;

procedure TManagedForm.DoLoadFormState(Form: TCustomForm);
var
  Settings: TGExpertsSettings;
  Section: string;
  Panel: TCustomPanel;
begin
  Assert(Assigned(Form));
  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    Section := Form.ClassName;
    if RememberSize then
    begin
      Form.Width := Settings.ReadInteger(Section, WidthIdent, Form.Width);
      Form.Height := Settings.ReadInteger(Section, HeightIdent, Form.Height);
    end;
    if RememberPosition then
    begin
      Form.Top := Settings.ReadInteger(Section, TopIdent, Form.Top);
      Form.Left := Settings.ReadInteger(Section, LeftIdent, Form.Left);
    end;
    if RememberSize or RememberPosition then
      EnsureFormVisible(Form);
    if RememberSplitterPosition then
    begin
      Panel := FindSplitPanel(Form);
      if Assigned(Panel) then
        Panel.Width := Settings.ReadInteger(Section, SplitPosIdent, Panel.Width);
    end;
    if RememberWidth and (not RememberSize) then
    begin
      Form.Width := Settings.ReadInteger(Section, WidthIdent, Form.Width);
      if not RememberPosition then
        CenterForm(Form);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TManagedForm.DoMakeResizable(Form: TCustomForm);
begin
  Assert(Assigned(Form));
  if MakeResizable and (Form.BorderStyle <> bsSizeable) then
    if RunningBDS2006OrGreater then // Delphi 6/7/8/2005 fail with an exception: "Can't change Visible in OnShow"
      Form.BorderStyle := bsSizeable;
end;

procedure TManagedForm.DoResizePictureDialogs(Form: TCustomForm);
var
  i: Integer;
  PictureDialog: TOpenPictureDialog;
begin
  if ResizePictureDialogs then
  begin
    for i := 0 to Form.ComponentCount - 1 do
    begin
      if Form.Components[i] is TOpenPictureDialog then
      begin
        PictureDialog := TOpenPictureDialog(Form.Components[i]);
        PictureDialog.Options := PictureDialog.Options + [ofEnableSizing];
      end;
    end;
  end;
end;

procedure TManagedForm.DoSaveFormState(Form: TCustomForm);
var
  Settings: TGExpertsSettings;
  Section: string;
  Panel: TCustomPanel;
begin
  Assert(Assigned(Form));
  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    Section := Form.ClassName;
    if RememberSize then
    begin
      Settings.WriteInteger(Section, WidthIdent, Form.Width);
      Settings.WriteInteger(Section, HeightIdent, Form.Height);
    end;
    if RememberPosition then
    begin
      Settings.WriteInteger(Section, TopIdent, Form.Top);
      Settings.WriteInteger(Section, LeftIdent, Form.Left);
    end;
    if RememberSplitterPosition then
    begin
      Panel := FindSplitPanel(Form);
      if Assigned(Panel) then
        Settings.WriteInteger(Section, SplitPosIdent, Panel.Width)
      else
        Settings.DeleteKey(Section, SplitPosIdent);
    end;
    if RememberWidth and (not RememberSize) then
      Settings.WriteInteger(Section, WidthIdent, Form.Width);
  finally
    FreeAndNil(Settings);
  end;
end;

{ TFormChangeManager }

constructor TFormChangeManager.Create;
begin
  inherited;
  FManagedForms := TManagedFormList.Create(True);
  FFormChangeCallbacks := TList.Create;
  FControlChangeCallbacks := TList.Create;

  // hook Screen.OnActiveFormChange using the method describe here:
  // http://blog.dummzeuch.de/safe-event-hooking-for-delphi-ide-plugins/
  FScreenActiveFormChangeHook := HookScreenActiveFormChange(ScreenActiveFormChange);

  // hook Screen.OnActiveControlChange
  HookActiveControlChanged;
end;

destructor TFormChangeManager.Destroy;
var
  i: Integer;
begin
  // unhook first, to be sure none of these hooks is being called after we freed our lists
  if Assigned(FScreenActiveControlChangeHook) then
    UnhookScreenActiveControlChange(FScreenActiveControlChangeHook);

  if Assigned(FScreenActiveFormChangeHook) then
    UnhookScreenActiveFormChange(FScreenActiveFormChangeHook);

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

  FreeAndNil(FManagedForms); // This frees/unhooks any managed form objects

  inherited;
end;

function IsSameMethod(_Method1, _Method2: TNotifyEvent): boolean;
begin
  result := (TMethod(_Method1).Code = TMethod(_Method2).Code)
    and (TMethod(_Method1).Data = TMethod(_Method2).Data);
end;

procedure TFormChangeManager.HookActiveControlChanged;
begin
  if Assigned(FScreenActiveControlChangeHook) then
    UnhookScreenActiveControlChange(FScreenActiveControlChangeHook);
  FScreenActiveControlChangeHook := HookScreenActiveControlChange(ScreenActiveControlChange)
end;

function TFormChangeManager.FindManagedForm(Form: TCustomForm; var ManagedForm: TManagedForm): Boolean;
var
  i: Integer;
  TempManagedForm: TManagedForm;
begin
  Result := False;
  for i := 0 to FManagedForms.Count - 1 do
  begin
    TempManagedForm := FManagedForms.GetForm(i);
    if (Form.ClassName = TempManagedForm.FormClassName) and (Form.Handle = TempManagedForm.Handle) then
    begin
      ManagedForm := TempManagedForm;
      Result := True;
      Exit;
    end;
  end;
end;

function TFormChangeManager.FindScreenForm(const FormClassName: string; Handle: HWND): TCustomForm;
var
  i: Integer;
  Form: TCustomForm;
begin
  Result := nil;
  for i := Screen.CustomFormCount - 1 downto 0 do
  begin
    Form := Screen.CustomForms[i];
    if (Form.ClassName = FormClassName) and (Form.Handle = Handle) then
    begin
      Result := Form;
      Break;
    end;
  end;
end;

procedure TFormChangeManager.FormDestroy(Sender: TObject);
var
  ManagedForm: TManagedForm;
  Form: TCustomForm;
begin
  Assert(Sender is TCustomForm);
  Form := Sender as TCustomForm;
  if not FindManagedForm(Form, ManagedForm) then
    raise Exception.Create('Destroyed managed form not found: ' + Form.ClassName);
  ManagedForm.DoSaveFormState(Form);
  if Assigned(ManagedForm.OldDestroy) then
    ManagedForm.OldDestroy(Sender);
  FManagedForms.Remove(ManagedForm); // Resets using OldDestroy
end;

function TFormChangeManager.IsManagingForm(Form: TCustomForm): Boolean;
begin
  Result := Assigned(FManagedForms.GetForm(Form));
end;

procedure TFormChangeManager.ModifyForm(Form: TCustomForm; ManagedForm: TManagedForm);
begin
  Assert(Assigned(Form));
  Assert(Assigned(ManagedForm));
  Assert(Form.ClassName = ManagedForm.FormClassName);
  Assert(Form.Handle = ManagedForm.Handle);
  ManagedForm.DoMakeResizable(Form);
  ManagedForm.DoLoadFormState(Form);
  ManagedForm.DoCollapseTreeNodes(Form);
  ManagedForm.DoResizePictureDialogs(Form);
  ManagedForm.DoComboDropDownCount(Form);
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
  ManagedForm: TManagedForm;
  i: Integer;
begin
  Assert(Assigned(Form));

  // We are executing the callbacks in last registered first order to allow callbacks to
  // unregister themselves. (And we don't check whether a callback only unregisters itself, so
  // please be cautios!). That means registering a new callback while executing a callback
  // is not allowed.
  FIsInFormChangeCallback := True;
  try
    for i := FFormChangeCallbacks.Count - 1 downto 0 do
      TFormChangeCallbackItem(FFormChangeCallbacks[i]).FCallback(Self, Form);
  finally
    FIsInFormChangeCallback := False;
  end;

  if ShouldManageForm(Form, Changes) then
  begin
    ManagedForm := TManagedForm.Create;
    ManagedForm.FormClassName := Form.ClassName;
    ManagedForm.Handle := Form.Handle;
    ManagedForm.MakeResizable := Changes.MakeResizable;
    ManagedForm.RememberSize := Changes.RememberSize;
    ManagedForm.RememberPosition := Changes.RememberPosition;
    ManagedForm.RememberSplitterPosition := Changes.RememberSplitterPosition;
    ManagedForm.RememberWidth := Changes.RememberWidth;
    ManagedForm.CollapseTreeNodes := Changes.CollapseTreeNodes;
    ManagedForm.ResizePictureDialogs := Changes.ResizePictureDialogs;
    ManagedForm.ComboDropDownCount := Changes.ComboDropDownCount;
    ManagedForm.OldDestroy := THackForm(Form).OnDestroy;
    THackForm(Form).OnDestroy := FormDestroy;
    FManagedForms.Add(ManagedForm);
    ModifyForm(Form, ManagedForm);
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

function TFormChangeManager.ShouldManageForm(Form: TCustomForm;
  var Changes: TFormChanges): Boolean;
var
  i: Integer;
  Control: TControl;
begin
  Assert(Assigned(Form));
  Result := False;
  if (csDesigning in Form.ComponentState)
    or StringInArray(Form.ClassName, ['TAppBuilder', 'TMessageForm', 'TPropertyInspector', 'TObjectTreeView', 'TEditWindow']) then
    Exit;
  if (Form.ClassName = 'TAboutBox') then  // Or TAboutBoxHiColor in D7-
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
  for i := Low(FormsToChange) to High(FormsToChange) do
  begin
    if StrContains(Form.ClassName + ';', FormsToChange[i].FormClassNames + ';') then
    begin
      if not IsManagingForm(Form) then
      begin
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


{ TManagedFormList }

function TManagedFormList.GetForm(Form: TCustomForm): TManagedForm;
var
  i: Integer;
  ManagedForm: TManagedForm;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
  begin
    ManagedForm := GetForm(i);
    if (ManagedForm.FormClassName = Form.ClassName) and (ManagedForm.Handle = Form.Handle) then
    begin
      Result := ManagedForm;
      Break;
    end;
  end;
end;

function TManagedFormList.GetForm(Index: Integer): TManagedForm;
begin
  Result := (Items[Index] as TManagedForm);
end;

{ TIDEFormEnhancements }

class function TIDEFormEnhancements.GetEnabled: Boolean;
begin
  Result := Assigned(PrivateFormChangeManager);
end;

class procedure TIDEFormEnhancements.SetEnabled(const Value: Boolean);
begin
  if Value and (not Assigned(PrivateFormChangeManager)) then
    PrivateFormChangeManager := TFormChangeManager.Create
  else if not Value then
    FreeAndNil(PrivateFormChangeManager);
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

