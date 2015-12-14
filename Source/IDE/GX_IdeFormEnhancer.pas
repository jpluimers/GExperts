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
    FMinHeight: Integer; // if negative: Height is fixed
    FMinWidth: Integer;
    FItems: TStrings;
    // Methods
    constructor Create;
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
  private
    procedure MakeSearchFormResizable(Form: TCustomForm);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure MakeReplaceFormResizable(Form: TCustomForm);
    procedure MakePasEnvironmentDialogResizable(Form: TCustomForm);
    procedure ForceVisibleToBeSizable(WndHandle: HWND);
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
    property AllowResize: Boolean read FAllowResize write FAllowResize;
    property RememberPosition: Boolean read FRememberPosition write FRememberPosition;
  end;

const
  FormsToChange: array[0..4] of TFormChanges = (
    (
      FormClassNames: 'TSrchDialog;TRplcDialog';
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
      FormClassNames: 'TProjectOptionsDialog;TDelphiProjectOptionsDialog;'
        + 'TLoadProcessDialog;TDotNetOptionForm;TPasEditorPropertyDialog;'
        + 'TCppProjOptsDlg;TPasEnvironmentDialog;TReopenMenuPropertiesDialog';
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

constructor TManagedForm.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TManagedForm.Destroy;
var
  Form: TCustomForm;
begin
  Form := PrivateFormChangeManager.FindScreenForm(FormClassName, Handle);
  if Assigned(Form) then
    THackForm(Form).OnDestroy := OldDestroy;
  FItems.Free;
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
  Flags: TFormSaveFlags;
begin
  Assert(Assigned(Form));
  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    Section := Form.ClassName;
    Flags := [];
    if RememberSize then
      Include(Flags, fsSize);
    if RememberPosition then
      Include(Flags, fsPosition);
    if Flags <> [] then
    begin
      Settings.LoadForm(Form, Section, Flags);
      EnsureFormVisible(Form);
    end;

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

type
  TCustomFormHack = class(TCustomForm)
  end;

procedure TManagedForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
  var Resize: Boolean);
begin
  if NewWidth < FMinWidth then
    NewWidth := FMinWidth;
  if (FMinHeight < 0) or (NewHeight < FMinHeight) then
    NewHeight := Abs(FMinHeight);
end;

procedure TManagedForm.MakeSearchFormResizable(Form: TCustomForm);
var
  frm: TForm;
  i: Integer;
  cmp: TComponent;
begin
  // This is only ever called in Delphi6 because the Search form of later
  // versions is already resizable.
  frm := TForm(Form);
  FMinHeight := -frm.Height;
  FMinWidth := frm.Width;
  frm.OnCanResize := FormCanResize;
  for i := 0 to Form.ComponentCount - 1 do begin
    cmp := Form.Components[i];
    if cmp is TPageControl then begin
      TPageControl(cmp).Anchors := [akLeft, akTop, akRight, akBottom];
    end else if cmp is TButton then begin
      TButton(cmp).Anchors := [akRight, akBottom];
    end else if cmp is TCustomComboBox then begin
      TCombobox(cmp).Anchors := [akLeft, akRight, akTop];
    end else if cmp is TGroupBox then begin
      if cmp.Name = 'GroupBox4' then begin
        // stupid developer forgot to give it a meaningful name :-(
        TGroupBox(cmp).Anchors := [akLeft, akRight, akTop];
      end;
    end;
  end;
end;

procedure TManagedForm.MakeReplaceFormResizable(Form: TCustomForm);
var
  frm: TForm;
  i: Integer;
  cmp: TComponent;
begin
  // This is only ever called in Delphi6 because the Replace form of later
  // versions is already resizable.
  frm := TForm(Form);
  FMinHeight := -frm.Height;
  FMinWidth := frm.Width;
  frm.OnCanResize := FormCanResize;
  for i := 0 to Form.ComponentCount - 1 do begin
    cmp := Form.Components[i];
    if cmp is TPageControl then begin
      TPageControl(cmp).Anchors := [akLeft, akTop, akRight, akBottom];
    end else if cmp is TButton then begin
      TButton(cmp).Anchors := [akRight, akBottom];
    end else if cmp is TCustomComboBox then begin
      TCombobox(cmp).Anchors := [akLeft, akRight, akTop];
    end;
  end;
end;

procedure TManagedForm.MakePasEnvironmentDialogResizable(Form: TCustomForm);
var
  VariableOptionsFrame: TWinControl;

  function IsInVarOptionsFrame(_cmp: TComponent): boolean;
  begin
    Result := Assigned(VariableOptionsFrame) and (_cmp is TControl);
    if Result then
      Result := VariableOptionsFrame.ContainsControl(TControl(_cmp));
  end;

  procedure HandleComponent(_cmp: TComponent);
  var
    i: Integer;
  begin
    if _cmp.ClassNameIs('TVariableOptionsFrame') then begin
      if SameText(_cmp.Name, 'VariableOptionsFrame') then begin
        VariableOptionsFrame := TWinControl(_cmp);
        VariableOptionsFrame.Align := alClient;
      end;
    end else if _cmp is TPageControl then begin
      TPageControl(_cmp).Anchors := [akLeft, akTop, akRight, akBottom];
    end else if _cmp is TPanel then begin
      if SameText(_cmp.Name, 'ToolListPanel') or IsInVarOptionsFrame(_cmp) then
        TPanel(_cmp).Anchors := [akLeft, akTop, akRight, akBottom];
    end else if _cmp is TButton then begin
      TButton(_cmp).Anchors := [akRight, akBottom];
    end else if _cmp is TColorBox then begin
      if SameText(_cmp.Name, 'cbColorPicker') then
        TColorBox(_cmp).Anchors := [akLeft, akBottom];
    end else if _cmp is TCustomComboBox then begin
      if SameText(_cmp.Name, 'ecLibraryPath') or SameText(_cmp.Name, 'ecDLLOutput')
        or SameText(_cmp.Name, 'ecDCPOutput') or SameText(_cmp.Name, 'ecBrowsing') then begin
        TCombobox(_cmp).Anchors := [akLeft, akRight, akTop];
      end;
    end else if _cmp is TGroupBox then begin
      if SameText(_cmp.Name, 'GroupBox10') or SameText(_cmp.Name, 'Repository') then begin
        TGroupBox(_cmp).Anchors := [akLeft, akTop, akRight];
      end else if SameText(_cmp.Name, 'gbColors') then begin
        TGroupBox(_cmp).Anchors := [akLeft, akTop, akBottom];
      end else if IsInVarOptionsFrame(_cmp) then begin
        if SameText(_cmp.Name, 'GroupBox1') then begin
          TGroupBox(_cmp).Anchors := [akLeft, akTop, akBottom];
        end else if SameText(_cmp.Name, 'GroupBox2') then begin
          TGroupBox(_cmp).Anchors := [akLeft, akBottom];
        end;
      end;
    end else if _cmp is TListBox then begin
      if SameText(_cmp.Name, 'lbColors')  then begin
        TListBox(_cmp).Anchors := [akLeft, akTop, akBottom];
      end else if SameText(_cmp.Name, 'PageListBox') then begin
        TListBox(_cmp).Anchors := [akLeft, akTop, akBottom];
        if (TListBox(_cmp).Items.Count = 0) and (FItems.Count <> 0) then
          TListBox(_cmp).Items.Assign(FItems);
      end;
    end else if _cmp.ClassNameIs('TPropEdit') then begin
      if SameText(_cmp.Name, 'ecRepositoryDir') then begin
        TCustomEdit(_cmp).Anchors := [akLeft, akTop, akRight];
      end;
    end else if _cmp.ClassNameIs('THintListView') then begin
      if IsInVarOptionsFrame(_cmp) then
        TWinControl(_cmp).Anchors := [akLeft, akTop, akRight];
    end;
    for i := 0 to _cmp.ComponentCount - 1 do begin
      HandleComponent(_cmp.Components[i]);
    end;
  end;

var
  frm: TForm;
begin
  // This is called in Delphi6 and Delphi 7 because the Environment form of later
  // versions is already resizable.
  frm := TForm(Form);
  FMinHeight := frm.Height;
  FMinWidth := frm.Width;
  frm.OnCanResize := FormCanResize;
  HandleComponent(frm);
end;

procedure TManagedForm.ForceVisibleToBeSizable(WndHandle: HWND);
begin
  // this is taken from http://stackoverflow.com/a/34255563/49925
  SetWindowLong(WndHandle, GWL_STYLE,
    GetWindowLong(WndHandle, GWL_STYLE) and not WS_POPUP or WS_THICKFRAME);
  SetWindowLong(WndHandle, GWL_EXSTYLE,
    GetWindowLong(WndHandle, GWL_EXSTYLE) and not WS_EX_DLGMODALFRAME);
  InsertMenu(GetSystemMenu(WndHandle, False), 1, MF_STRING or MF_BYPOSITION, SC_SIZE, 'Size');
  SetWindowPos(WndHandle, 0, 0, 0, 0, 0,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_FRAMECHANGED);
  DrawMenuBar(WndHandle);
end;

procedure TManagedForm.DoMakeResizable(Form: TCustomForm);
var
  WasShowing: boolean;
begin
  Assert(Assigned(Form));
  if MakeResizable and (Form.BorderStyle <> bsSizeable) then begin
    WasShowing := (fsShowing in TCustomFormHack(Form).FFormState);
    if WasShowing then
      Exclude(TCustomFormHack(Form).FFormState, fsShowing);
    Handle := Form.Handle;
    if WasShowing then begin
      ForceVisibleToBeSizable(Handle);
    end else begin
      Form.BorderStyle := bsSizeable;
    end;
    if Form.ClassName = 'TSrchDialog' then
      MakeSearchFormResizable(Form)
    else if Form.ClassName = 'TRplcDialog' then
      MakeReplaceFormResizable(Form)
    else if Form.ClassName = 'TPasEnvironmentDialog' then
      MakePasEnvironmentDialogResizable(Form);
    if WasShowing then begin
      SendMessage(Handle, CM_SHOWINGCHANGED, 0, 0);
    end;
  end;
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

function IsSameMethod(_Method1, _Method2: TNotifyEvent): Boolean;
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
    ManagedForm.MakeResizable := Changes.MakeResizable and AllowResize;
    ManagedForm.RememberSize := Changes.RememberSize and AllowResize;
    ManagedForm.RememberPosition := Changes.RememberPosition and RememberPosition;
    ManagedForm.RememberSplitterPosition := Changes.RememberSplitterPosition;
    ManagedForm.RememberWidth := Changes.RememberWidth and AllowResize;
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

