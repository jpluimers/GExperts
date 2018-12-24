unit GX_IdeManagedForm;

interface

{$I GX_CondDefine.inc}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  Windows,
  StdCtrls;

type
  TManagedForm = class;
  TManagedFormClass = class of TManagedForm;

  TFormChanges = record
    // Source-defined
    FormClassNames: string;
    FormEnhancer: TManagedFormClass;
    MakeResizable: Boolean;
    RememberSize: Boolean;
    RememberWidth: Boolean;
    RememberPosition: Boolean;
    RememberSplitterPosition: Boolean;
    ResizePictureDialogs: Boolean;
    ComboDropDownCount: Integer;
  end;

  // to support the GX_IdeFormEnhancer and GX_IdeDialogEnhancer units that enhance/fix IDE forms and dialogs
  // like allowing the forms to resize, saving/loading form state, collapse treenodes, etc.
  // usage roots are InitializeFormHandlers, TIdeDialogEnhancer.Create, TIDEFormEnhancer.Create
  TManagedForm = class(TComponent)
  private
    FForm: TForm;
    FMinHeight: Integer; // if negative: Height is fixed
    FMinWidth: Integer;
    FOrigOnDestroy: TNotifyEvent;
    function GetRegistryKey: string;
    function FindSplitPanel: TCustomPanel;
    procedure SetComboDropDownCount(Control: TControl);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ForceVisibleToBeSizable(WndHandle: HWND);
    procedure FormDestroy(Sender: TObject);
    ///</summary>
    /// Creates a unique comonent name based on the form name  </summary>
    class function GenerateName(const _FormName: string): string;
  protected
    procedure DoHookOnFormDestroy; virtual;
    procedure DoFixFormErrors; virtual;
    procedure DoMakeResizable; virtual;
    procedure DoLoadFormState;
    procedure DoSaveFormState;
    procedure DoResizePictureDialogs;
    procedure DoComboDropDownCount;
    procedure MakeComponentsResizable; virtual;
    procedure DoCollapseTreeNodes; virtual;
    function AddMadeSizeablePanel: TPanel;
  public
    FFormChanges: TFormChanges;
    ///<summary>
    /// Checks whether the given form already contains a component with this GenerateName
    /// @returns true, if it does, false otherwise </summary>
    class function AlreadyExists(const _Form: TCustomForm): Boolean;
    constructor Create(_Owner: TComponent); override;
    procedure Init(const _FormChanges: TFormChanges);
  end;

type
  TManagedFormImageListEditor = class(TManagedForm)
  protected
{$IFNDEF GX_VER150_up} // Delphi 7
    procedure MakeComponentsResizable; override;
{$ENDIF}
  end;

type
  TManagedFormPictureEditDlg = class(TManagedForm)
  protected
    procedure MakeComponentsResizable; override;
  end;

type
  TManagedFormSrchDialog = class(TManagedForm)
  protected
{$IFNDEF GX_VER150_up} // Delphi 7
    procedure MakeComponentsResizable; override;
{$ENDIF}
  end;

type
  TManagedFormRplcDialog = class(TManagedForm)
  protected
{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
    procedure DoFixFormErrors; override;
{$ENDIF}
{$ENDIF}
{$IFNDEF GX_VER150_up} // Delphi 7
    procedure MakeComponentsResizable; override;
{$ENDIF}
  end;

type
  TManagedFormDefaultEnvironmentDialog = class(TManagedForm)
  // Options dialog in Delphi 2005 and later:
  // Collapse some tree branches.
{$IFNDEF GX_VER170_up} // Delphi 2005
    procedure DoCollapseTreeNodes; override;
{$ENDIF}
  end;

type
  TManagedFormConnEditForm = class(TManagedForm)
  private
    FConnectionStringEdit: TEdit;
    function EditConnectionString(_ParentHandle: THandle; var _ConnectionString: string): Boolean;
    procedure BuildButtonClick(_Sender: TObject);
  protected
    procedure MakeComponentsResizable; override;
  end;

type
  TManagedFormPakComponentsDlg = class(TManagedForm)
  protected
    procedure MakeComponentsResizable; override;
  end;

type
  TManagedFormPasEnvironmentDialog = class(TManagedForm)
  private
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
    FItems: TStringList;
  protected
    procedure MakeComponentsResizable; override;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
{$ENDIF}
  end;

type
  TManagedFormProjectOptionsDialog = class(TManagedForm)
  protected
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
    procedure MakeComponentsResizable; override;
{$ENDIF}
  end;

type
  TManagedFormFixFormPositioningOnly = class(TManagedForm)
  protected
// fix for RSP-13229: File -> New -> Other opens on different monitor
//     and RSP-13230: on dual monitor Project -> Resources and images gets shown on primary monitor
// which occur in Delphi 2009 to 10.0 Berlin
{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
    procedure DoFixFormErrors; override;
{$ENDIF}
{$ENDIF}
  end;

type
  TManagedFormAboutBox = class(TManagedForm)
{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  protected
    procedure DoFixFormErrors; override;
{$ENDIF}
{$ENDIF}
  end;

implementation

uses
  ComCtrls,
  TypInfo,
  ExtDlgs,
  Dialogs,
  Grids,
  CheckLst,
  OleDB,
  ComObj,
  ActiveX,
  GX_ConfigurationInfo,
  GX_GenericUtils,
  GX_dzClassUtils,
  GX_IdeUtils,
  GX_dzNamedThread;

const
  WidthIdent = 'Width';
  HeightIdent = 'Height';
  TopIdent = 'Top';
  LeftIdent = 'Left';
  SplitPosIdent = 'SplitPos';

type
  TCustomFormHack = class(TCustomForm)
  end;

type
  TAnchorInfo = class
  private
    FCtrl: TControl;
    FAnchors: TAnchors;
  public
    constructor Create(_Ctrl: TControl; _Anchors: TAnchors);
    procedure Apply;
  end;

constructor TAnchorInfo.Create(_Ctrl: TControl; _Anchors: TAnchors);
begin
  inherited Create;
  FCtrl := _Ctrl;
  FAnchors := _Anchors;
end;

procedure TAnchorInfo.Apply;
begin
  FCtrl.Anchors := FAnchors;
end;

type
  EMakeResizableError = class(Exception)
  end;

  ECtrlNotFound = class(EMakeResizableError)
  end;

  EParentIsWrongType = class(EMakeResizableError)
  end;

type
  TAnchorInfoList = class
  private
    FList: TList;
    FForm: TForm;
  public
    constructor Create(_Form: TForm);
    destructor Destroy; override;
    function AddControl(const _Name: string; _Anchors: TAnchors; _CmpClass: TComponentClass): TControl; overload;
    procedure AddControl(_Ctrl: TControl; _Anchors: TAnchors; _CmpClass: TComponentClass = nil); overload;
    procedure ApplyAnchors;
  end;

constructor TAnchorInfoList.Create(_Form: TForm);
begin
  inherited Create;
  FForm := _Form;
  FList := TList.Create;
end;

destructor TAnchorInfoList.Destroy;
begin
  TList_FreeWithObjects(FList);
  inherited;
end;

function TAnchorInfoList.AddControl(const _Name: string; _Anchors: TAnchors;
  _CmpClass: TComponentClass): TControl;
begin
  if not TComponent_FindComponent(FForm, _Name, True, TComponent(Result), _CmpClass) then
    raise ECtrlNotFound.CreateFmt('Control %s not found in form', [_Name]);
  FList.Add(TAnchorInfo.Create(Result, _Anchors));
end;

procedure TAnchorInfoList.AddControl(_Ctrl: TControl; _Anchors: TAnchors; _CmpClass: TComponentClass = nil);
begin
  if (_CmpClass <> nil) and not (_Ctrl is _CmpClass) then
    raise EParentIsWrongType.CreateFmt('%s is not %s', [_Ctrl.Name, _CmpClass.ClassName]);
  FList.Add(TAnchorInfo.Create(_Ctrl, _Anchors));
end;

procedure TAnchorInfoList.ApplyAnchors;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TAnchorInfo(FList[i]).Apply;
end;

{ TGxIdeFormEnhancerBase }

class function TManagedForm.GenerateName(const _FormName: string): string;
begin
  Result := 'GXManagedForm_' + _FormName;
end;

class function TManagedForm.AlreadyExists(const _Form: TCustomForm): Boolean;
var
  cmp: TComponent;
begin
  Result := TComponent_FindComponent(_Form, GenerateName(_Form.Name), False, cmp, TManagedForm);
end;

constructor TManagedForm.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  FForm := _Owner as TForm;
  Name := GenerateName(FForm.Name)
end;

function TManagedForm.AddMadeSizeablePanel: TPanel;
var
  Size: TSize;
begin
  Result := TPanel.Create(FForm);
  Result.Parent := FForm;
  Result.Caption := 'Form made sizeable by GExperts';
  Result.Hint := 'You can turn this off in the GExperts configuration on the IDE tab.';
  Result.showhint := True;
  Size := FForm.Canvas.TextExtent(Result.Caption);
  Result.Height := Size.cy + 4;
  Result.Width := Size.cx + 4;
  Result.Left := 0;
  Result.Top := FForm.ClientHeight - Result.Height;
  Result.BevelOuter := bvNone;
  Result.Anchors := [akLeft, akBottom];
end;

function TManagedForm.FindSplitPanel: TCustomPanel;
var
  i: Integer;
  Control: TControl;
begin
  Assert(Assigned(FForm));

  Result := nil;
  for i := 0 to FForm.ControlCount - 1 do begin
    Control := FForm.Controls[i];
    if (Control is TCustomPanel) and (Control.Align = alLeft) then begin
      Result := Control as TCustomPanel;
      Break;
    end;
  end;
end;

function TManagedForm.GetRegistryKey: string;
begin
  Result := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + 'IDEForms';
end;

procedure TManagedForm.DoCollapseTreeNodes;
begin
  // does nothing, is overridden in decendant forms if necessary
end;

procedure TManagedForm.SetComboDropDownCount(Control: TControl);
var
  ClsName: string;
  CtrlName: string;
begin
  ClsName := Control.ClassName;
  if not StrContains('Combo', ClsName) and not StrContains('ColorBox', ClsName) then
    Exit;

  CtrlName := Control.Name;

  // Fix issue #26:
  // Exclude some controls from being changed:
  if (Control is TCustomComboBox)
    and (
    SameText(CtrlName, 'TTargetsComboBox')
    or SameText(CtrlName, 'cbPlatformConfigurations')
    or SameText(CtrlName, 'cbConfig')
    ) then begin
    Exit;
  end;
  if (Control is TCustomComboboxEx)
    and SameText(CtrlName, 'cbPlatformConfigurations') then
    Exit;

  if TypInfo.IsPublishedProp(Control, 'DropDownCount') then
    if TypInfo.PropIsType(Control, 'DropDownCount', tkInteger) then
      if TypInfo.GetPropValue(Control, 'DropDownCount', False) < FFormChanges.ComboDropDownCount then
        TypInfo.SetPropValue(Control, 'DropDownCount', FFormChanges.ComboDropDownCount);
end;

procedure TManagedForm.DoComboDropDownCount;
begin
  if FFormChanges.ComboDropDownCount > 0 then
    IterateOverControls(FForm, SetComboDropDownCount);
end;

procedure TManagedForm.DoFixFormErrors;
begin
  // nothing to do in base class
end;

procedure TManagedForm.DoLoadFormState;
var
  Settings: TGExpertsSettings;
  Section: string;
  Panel: TCustomPanel;
  Flags: TFormSaveFlags;
begin
  Assert(Assigned(FForm));
  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    Section := FForm.ClassName;
    Flags := [];
    if FFormChanges.RememberSize then
      Include(Flags, fsSize);
    if FFormChanges.RememberPosition then
      Include(Flags, fsPosition);
    if Flags <> [] then begin
      Settings.LoadForm(FForm, Section, Flags);
      EnsureFormVisible(FForm);
    end;

    if FFormChanges.RememberSplitterPosition then begin
      Panel := FindSplitPanel;
      if Assigned(Panel) then
        Panel.Width := Settings.ReadInteger(Section, SplitPosIdent, Panel.Width);
    end;

    if FFormChanges.RememberWidth and (not FFormChanges.RememberSize) then begin
      FForm.Width := Settings.ReadInteger(Section, WidthIdent, FForm.Width);
      if not FFormChanges.RememberPosition then
        CenterForm(FForm);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TManagedForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
  var Resize: Boolean);
begin
  if NewWidth < FMinWidth then
    NewWidth := FMinWidth;
  if (FMinHeight < 0) or (NewHeight < FMinHeight) then
    NewHeight := Abs(FMinHeight);
end;

{ TManagedFormImageListEditor }

{$IFNDEF GX_VER150_up} // Delphi 7
// necessary only in Delphi 6

procedure TManagedFormImageListEditor.MakeComponentsResizable;
var
  List: TAnchorInfoList;
begin
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('ImageGroup', [akLeft, akTop, akRight], TGroupBox);
    List.AddControl('TransparentColor', [akLeft, akTop, akRight], TComboBox);
    List.AddControl('FillColor', [akLeft, akTop, akRight], TComboBox);
    List.AddControl('OptionsGroup', [akRight, akTop], TRadioGroup);
    List.AddControl('GroupBox1', [akLeft, akTop, akRight, akBottom], TGroupBox);
    List.AddControl('ImageView', [akLeft, akTop, akRight, akBottom], TListView);
    List.AddControl('Add', [akRight, akBottom], TButton);
    List.AddControl('Delete', [akRight, akBottom], TButton);
    List.AddControl('Clear', [akRight, akBottom], TButton);
    List.AddControl('ExportBtn', [akRight, akBottom], TButton);
    List.AddControl('OK', [akRight, akTop], TButton);
    List.AddControl('Cancel', [akRight, akTop], TButton);
    List.AddControl('Apply', [akRight, akTop], TButton);
    List.AddControl('Help', [akRight, akTop], TButton);
    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;
  // unfortunately we must restrict the height, otherwise the sort order of the icons gets
  // messed up. It's probably possible to fix that, but that's quite a lot of work for
  // the oldest supported IDE Delphi 6
  FMinHeight := -FMinHeight;
// if set, the icon order gets messed up
//  ImageView.IconOptions.Arrangement := iaTop;

  AddMadeSizeablePanel;
end;
{$ENDIF}

{ TManagedFormPictureEditDlg }

procedure TManagedFormPictureEditDlg.MakeComponentsResizable;
var
  List: TAnchorInfoList;
begin
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('GroupBox1', [akLeft, akTop, akRight, akBottom], TGroupBox);
    List.AddControl('ImagePanel', [akLeft, akTop, akRight, akBottom], TPanel);
    List.AddControl('Load', [akRight, akBottom], TButton);
    List.AddControl('Save', [akRight, akBottom], TButton);
    List.AddControl('Clear', [akRight, akBottom], TButton);
    List.AddControl('OKButton', [akRight, akTop], TButton);
    List.AddControl('CancelButton', [akRight, akTop], TButton);
    List.AddControl('HelpButton', [akRight, akTop], TButton);
    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;

  AddMadeSizeablePanel;
end;

{$IFNDEF GX_VER150_up} // Delphi 7
// This is only ever called in Delphi6 because the Search form of later
// versions is already resizable.

procedure TManagedFormSrchDialog.MakeComponentsResizable;
var
  List: TAnchorInfoList;
begin
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('PageControl', [akLeft, akTop, akRight, akBottom], TPageControl);
    List.AddControl('SearchText', [akLeft, akRight, akTop], TCustomComboBox);
    List.AddControl('FileSearchText', [akLeft, akRight, akTop], TCustomComboBox);
    List.AddControl('GroupBox4', [akLeft, akRight, akTop], TGroupBox);
    List.AddControl('DirSpec', [akLeft, akRight, akTop], TCustomComboBox);
    List.AddControl('BrowseButton', [akRight, akBottom], TButton);
    List.AddControl('OKButton', [akRight, akBottom], TButton);
    List.AddControl('CancelButton', [akRight, akBottom], TButton);
    List.AddControl('HelpButton', [akRight, akBottom], TButton);
    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;
  FMinHeight := -FMinHeight;

  AddMadeSizeablePanel;
end;
{$ENDIF}

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)

procedure TManagedFormRplcDialog.DoFixFormErrors;
var
  AppBuilder: TCustomForm;
  Monitor: tmonitor;
begin
  // Replace Dialog is placed on the primary monitor, occurs in Delphi 2005 to 2007
  AppBuilder := GetIdeMainForm;
  if Assigned(AppBuilder) then begin
    Monitor := AppBuilder.Monitor;
    if Assigned(Monitor) then begin
      FForm.Left := Monitor.Left + (Monitor.Width - FForm.Width) div 2;
      FForm.Top := Monitor.Top + (Monitor.Height - FForm.Height) div 2;
    end;
  end;
  inherited;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF GX_VER150_up} // Delphi 7
// This is only ever called in Delphi6 because the Replace form of later
// versions is already resizable.

procedure TManagedFormRplcDialog.MakeComponentsResizable;
var
  List: TAnchorInfoList;
begin
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('SearchText', [akLeft, akRight, akTop], TCustomComboBox);
    List.AddControl('ReplaceText', [akLeft, akRight, akTop], TCustomComboBox);
    List.AddControl('OKButton', [akRight, akBottom], TButton);
    List.AddControl('ChangeAll', [akRight, akBottom], TButton);
    List.AddControl('CancelButton', [akRight, akBottom], TButton);
    List.AddControl('HelpButton', [akRight, akBottom], TButton);
    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;
  FMinHeight := -FMinHeight;

  AddMadeSizeablePanel;
end;
{$ENDIF}

{ TManagedFormDefaultEnvironmentDialog }

{$IFNDEF GX_VER170_up} // Delphi 2005

procedure TManagedFormDefaultEnvironmentDialog.DoCollapseTreeNodes;

  function TryFindTreeView(out _tv: TTreeView): Boolean;
  var
    LeftPanel: TCustomPanel;
  begin
    Result := False;
    LeftPanel := FindSplitPanel;
    if LeftPanel.ControlCount > 0 then
      if LeftPanel.Controls[0] is TTreeView then begin
        _tv := TTreeView(LeftPanel.Controls[0]);
        Result := True;
      end;
  end;

  procedure CollapseNode(_TreeView: TTreeView; const _NodeName: string);
  var
    Node: TTreeNode;
  begin
    Node := FindTreeNode(_TreeView, _NodeName);
    if Assigned(Node) then
      Node.Collapse(False);
  end;

var
  TreeView: TTreeView;
  SelectedNode: TTreeNode;
begin
  if not TryFindTreeView(TreeView) then
    Exit;
  SelectedNode := TreeView.Selected;
  CollapseNode(TreeView, 'Together');
  CollapseNode(TreeView, 'Modeling');
  CollapseNode(TreeView, 'Version Control');
  CollapseNode(TreeView, 'Formatter');
  CollapseNode(TreeView, 'Translation Tools Options');
  CollapseNode(TreeView, 'HTML Options');
  if Assigned(SelectedNode) then
    SelectedNode.MakeVisible;
end;
{$ENDIF GX_VER170_up}

type
  TMoveWindowThread = class(TNamedThread)
  private
    FParentHandle: HWND;
    FParentCenterX: Integer;
    FParentCenterY: Integer;
    procedure CenterWindow(wHandle: HWND);
  protected
    procedure Execute; override;
  public
    constructor Create(_ParentHandle: HWND);
  end;

{ TMoveWindowThread }

constructor TMoveWindowThread.Create(_ParentHandle: HWND);
begin
  FreeOnTerminate := True;
  FParentHandle := _ParentHandle;
  inherited Create(False);
end;

procedure TMoveWindowThread.CenterWindow(wHandle: HWND);
var
  Rect: TRect;
  WindowCenterX: Integer;
  WindowCenterY: Integer;
  MoveByX: Integer;
  MoveByY: Integer;
begin
  GetWindowRect(wHandle, Rect);
  WindowCenterX := Round(Rect.Left / 2 + Rect.Right / 2);
  WindowCenterY := Round(Rect.Top / 2 + Rect.Bottom / 2);
  MoveByX := WindowCenterX - FParentCenterX;
  MoveByY := WindowCenterY - FParentCenterY;
  MoveWindow(wHandle, Rect.Left - MoveByX, Rect.Top - MoveByY,
    Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, False);
end;

procedure TMoveWindowThread.Execute;
var
  Rect: TRect;
  MaxTickCount: DWORD;
  ThreadInfo: TGUIThreadinfo;
begin
  inherited;

  GetWindowRect(FParentHandle, Rect);
  FParentCenterX := Round(Rect.Left / 2 + Rect.Right / 2);
  FParentCenterY := Round(Rect.Top / 2 + Rect.Bottom / 2);

  ThreadInfo.cbSize := SizeOf(ThreadInfo);
  MaxTickCount := GetTickCount + 10000; // 10 Seconds should be plenty
  while MaxTickCount > GetTickCount do begin
    Sleep(50);
    if GetGUIThreadInfo(MainThreadID, ThreadInfo) then begin
      if ThreadInfo.hwndActive <> FParentHandle then begin
        CenterWindow(ThreadInfo.hwndActive);
        Exit;
      end;
    end;
  end;
end;

function TManagedFormConnEditForm.EditConnectionString(_ParentHandle: THandle;
  var _ConnectionString: string): Boolean;
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
  s: WideString;
begin
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if _ConnectionString <> '' then begin
    s := _ConnectionString;
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(s), IUnknown, DataSource);
  end;
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;
  if _ParentHandle <> 0 then begin
    // This is a hack to make the dialog appear centered on the parent window
    // According to https://msdn.microsoft.com/en-us/library/ms725392(v=vs.85).aspx
    // the dialog should automatically be centered on the passed parent handle,
    // but if the parent window is not on the primary monitor this does not work.
    // So, we start a background thread that waits for the dialog to appear and then
    // moves it to the correct position.
    TMoveWindowThread.Create(_ParentHandle);
  end;
  Result := Succeeded(DBPrompt.PromptDataSource(nil, _ParentHandle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource));
  if Result then begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    _ConnectionString := InitStr;
  end;
end;

procedure TManagedFormConnEditForm.BuildButtonClick(_Sender: TObject);
var
  ConnectionString: string;
begin
  ConnectionString := FConnectionStringEdit.Text;
  if EditConnectionString(FForm.Handle, ConnectionString) then
    FConnectionStringEdit.Text := ConnectionString;
end;

procedure TManagedFormConnEditForm.MakeComponentsResizable;
var
  List: TAnchorInfoList;
  BuildBtn: TControl;
begin
  // There are two forms with that name. The one shown for TAdoConnection is not resizable
  // If it's not the TAdoConnection one, a silent exception will be raised and no
  // chages will be made.
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('SourceOfConnection', [akLeft, akTop, akRight], TGroupBox);
    List.AddControl('DataLinkFile', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('Browse', [akTop, akRight], TButton);
    FConnectionStringEdit := List.AddControl('ConnectionString', [akLeft, akTop, akRight], TEdit) as TEdit;
    BuildBtn := List.AddControl('Build', [akTop, akRight], TButton);
    List.AddControl('OKButton', [akBottom, akRight], TButton);
    List.AddControl('CancelButton', [akBottom, akRight], TButton);
    List.AddControl('HelpButton', [akBottom, akRight], TButton);
    List.ApplyAnchors;

    // Workaround for the bug that the build connection string dialog isn't automatically centered on the parent window
    // (which the WinAPI documentation claims it should be)
    TButton(BuildBtn).OnClick := BuildButtonClick;
  finally
    FreeAndNil(List);
  end;
  FMinHeight := -FMinHeight;

  AddMadeSizeablePanel;
end;

procedure TManagedFormPakComponentsDlg.MakeComponentsResizable;
var
  List: TAnchorInfoList;
  Cancel: TButton;
begin
  List := TAnchorInfoList.Create(FForm);
  try
    List.AddControl('InstalledGroupBox', [akTop, akLeft, akRight, akBottom], TGroupBox);
    List.AddControl('ComponentsListBox', [akTop, akLeft, akRight, akBottom], TListBox);
    List.AddControl('OKButton', [akRight, akBottom], TButton);
    List.AddControl('Button1', [akRight, akBottom], TButton);
    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;
  if TComponent_FindComponent(FForm, 'Cancel', True, TComponent(Cancel), TButton) then begin
    // they forgot to remove or hide that unused button ...
    Cancel.Visible := False;
  end;

  AddMadeSizeablePanel;
end;

{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
// This is called in Delphi6 and Delphi 7 because the Environment form of later
// versions is already resizable.

constructor TManagedFormPasEnvironmentDialog.Create(_Owner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TManagedFormPasEnvironmentDialog.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TManagedFormPasEnvironmentDialog.MakeComponentsResizable;
var
  VariableOptionsFrame: TWinControl;

  function IsInVarOptionsFrame(_cmp: TComponent): Boolean;
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
        TComboBox(_cmp).Anchors := [akLeft, akRight, akTop];
      end;
    end else if _cmp is TGroupBox then begin
      if SameText(_cmp.Name, 'GroupBox10') or SameText(_cmp.Name, 'Repository') then begin
        TGroupBox(_cmp).Anchors := [akLeft, akTop, akRight];
      end else if SameText(_cmp.Name, 'gbColors') then begin
        TGroupBox(_cmp).Anchors := [akLeft, akTop, akBottom];
      end else if IsInVarOptionsFrame(_cmp) then begin
        if SameText(_cmp.Name, 'GroupBox1') then begin
          TGroupBox(_cmp).Anchors := [akLeft, akTop, akBottom, akRight];
        end else if SameText(_cmp.Name, 'GroupBox2') then begin
          TGroupBox(_cmp).Anchors := [akLeft, akBottom, akRight];
        end;
      end;
    end else if _cmp is TListBox then begin
      if SameText(_cmp.Name, 'lbColors') then begin
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
begin
  HandleComponent(FForm);
  AddMadeSizeablePanel;
end;
{$ENDIF}

{ TManagedFormProjectOptionsDialog }

{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)

procedure TManagedFormProjectOptionsDialog.MakeComponentsResizable;
var
  List: TAnchorInfoList;

{$IFDEF GX_VER150_up} // Delphi 7
// Delphi 6 does not have that page

  procedure MakeCompilerMessagesResizable;
  var
    ctrl: TControl;
  begin
    ctrl := List.AddControl('WarningsList', [akLeft, akTop, akRight, akBottom], TCheckListBox);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight, akBottom], TGroupBox);
    List.AddControl('gbGeneral', [akLeft, akTop, akRight], TGroupBox);
  end;
{$ENDIF}

  procedure MakeDirectoriesResizable;
  var
    ctrl: TControl;
  begin
    ctrl := List.AddControl('ddOutputDir', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight], TGroupBox);
    List.AddControl('bOutputDir', [akTop, akRight], TButton);
    List.AddControl('ddUnitOutput', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('bUnitOutputDir', [akTop, akRight], TButton);
    List.AddControl('ddSearchPath', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('bDirSearch', [akTop, akRight], TButton);
    List.AddControl('ddDebugSourcePath', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('bDirDebugSource', [akTop, akRight], TButton);
    List.AddControl('ddDPLOutputDir', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('bDplOutputDir', [akTop, akRight], TButton);
    List.AddControl('ddDCPOutputDir', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl('bDCPOutputDir', [akTop, akRight], TButton);

    ctrl := List.AddControl('ddConditionals', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight], TGroupBox);
    List.AddControl('bConditionals', [akTop, akRight], TButton);

    ctrl := List.AddControl('ddUnitAliases', [akLeft, akTop, akRight], TCustomComboBox);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight], TGroupBox);
    List.AddControl('bUnitAliases', [akTop, akRight], TButton);
  end;

  procedure MakeVersionInfoResizable;
  var
    ctrl: TControl;
  begin
    ctrl := List.AddControl('ValueGrid', [akLeft, akTop, akRight, akBottom], TStringGrid);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight, akBottom], TPanel);
  end;

  procedure MakePackagesResizable;
  var
    Panel1: TPanel;
    ctrl: TControl;
  begin
    ctrl := List.AddControl('DesignPackageList', [akLeft, akTop, akRight, akBottom], TCheckListBox);
    List.AddControl(ctrl.Parent, [akLeft, akTop, akRight, akBottom], TGroupBox);
    if not TComponent_FindComponent(FForm, 'LabelPackageFile', True, TComponent(ctrl), TLabel) then
      raise ECtrlNotFound.CreateFmt('Control %s not found in form', ['LabelPackageFile']);
    List.AddControl(ctrl.Parent, [akLeft, akRight, akBottom], TPanel);
    Panel1 := TPanel(ctrl.Parent);

    List.AddControl('AddButton', [akRight, akBottom], TButton);
    List.AddControl('RemoveButton', [akRight, akBottom], TButton);
    List.AddControl('EditButton', [akRight, akBottom], TButton);
    List.AddControl('ButtonComponents', [akRight, akBottom], TButton);

    ctrl := List.AddControl('EditPackages', [akLeft, akTop, akRight], TEdit);
    List.AddControl(ctrl.Parent, [akLeft, akRight, akBottom], TGroupBox);
    List.AddControl('ModifyRuntimeButton', [akTop, akRight], TButton);

    Panel1.Caption := ''; // normally that caption is behind the label, but if we resize it, it becomes visible
  end;

begin
  List := TAnchorInfoList.Create(FForm);
  try
{$IFDEF GX_VER150_up} // Delphi 7
    MakeCompilerMessagesResizable;
{$ENDIF}
    MakeDirectoriesResizable;
    MakeVersionInfoResizable;
    MakePackagesResizable;

    List.AddControl('OkButton', [akRight, akBottom], TButton);
    List.AddControl('CancelButton', [akRight, akBottom], TButton);
    List.AddControl('HelpButton', [akRight, akBottom], TButton);

    List.ApplyAnchors;
  finally
    FreeAndNil(List);
  end;
  AddMadeSizeablePanel;
end;
{$ENDIF}

{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)

procedure TManagedFormFixFormPositioningOnly.DoFixFormErrors;
begin
  // fix for RSP-13229: File -> New -> Other opens on different monitor
  //     and RSP-13230: on dual monitor Project -> Resources and images gets shown on primary monitor
  // which occur in Delphi 2009 to 10.0 Berlin
  try
    // this results in an EInvalidOperation exception the first time it is called ...
    FForm.Position := poDesigned;
  except
    on EInvalidOperation do
      ; // ... which we ignore
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)

procedure TManagedFormAboutBox.DoFixFormErrors;
var
  Control: TControl;
begin
  // Remove the horizontal scrollbar, Delphi 2005 and 2006 only
  Control := FForm.FindChildControl('InstalledProducts');
  if Assigned(Control) then begin
    Control.Left := Control.Left - 1;
    Control.Width := Control.Width + 4;
  end;
end;
{$ENDIF}
{$ENDIF}

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

procedure TManagedForm.DoMakeResizable;
var
  WasShowing: Boolean;
  ClsName: string;
begin
  Assert(Assigned(FForm));

  if FFormChanges.MakeResizable and (FForm.BorderStyle <> bsSizeable) then begin
    FMinHeight := FForm.Height;
    FMinWidth := FForm.Width;
    try
      MakeComponentsResizable;
    except
      on e: EMakeResizableError do begin
{$IFOPT D+}SendDebugError(e.Message);
{$ENDIF}
      end;
    end;
    ClsName := FForm.ClassName;
    WasShowing := (fsShowing in TCustomFormHack(FForm).FFormState);
    if WasShowing then
      Exclude(TCustomFormHack(FForm).FFormState, fsShowing);
    if WasShowing then begin
      ForceVisibleToBeSizable(FForm.Handle);
    end else begin
      FForm.BorderStyle := bsSizeable;
      FForm.HandleNeeded;
    end;
    FForm.OnCanResize := FormCanResize;
    if WasShowing then begin
      SendMessage(FForm.Handle, CM_SHOWINGCHANGED, 0, 0);
    end;
  end;
end;

procedure TManagedForm.DoResizePictureDialogs;
var
  i: Integer;
  PictureDialog: TOpenPictureDialog;
  cmp: TComponent;
begin
  Assert(Assigned(FForm));

  if FFormChanges.ResizePictureDialogs then begin
    for i := 0 to FForm.ComponentCount - 1 do begin
      cmp := FForm.Components[i];
      if cmp is TOpenPictureDialog then begin
        PictureDialog := TOpenPictureDialog(cmp);
        PictureDialog.Options := PictureDialog.Options + [ofEnableSizing];
      end;
    end;
  end;
end;

procedure TManagedForm.DoSaveFormState;
var
  Settings: TGExpertsSettings;
  Section: string;
  Panel: TCustomPanel;
begin
  Assert(Assigned(FForm));

  Settings := TGExpertsSettings.Create(GetRegistryKey);
  try
    Section := FForm.ClassName;
    if FFormChanges.RememberSize then begin
      Settings.WriteInteger(Section, WidthIdent, FForm.Width);
      Settings.WriteInteger(Section, HeightIdent, FForm.Height);
    end;
    if FFormChanges.RememberPosition then begin
      Settings.WriteInteger(Section, TopIdent, FForm.Top);
      Settings.WriteInteger(Section, LeftIdent, FForm.Left);
    end;
    if FFormChanges.RememberSplitterPosition then begin
      Panel := FindSplitPanel;
      if Assigned(Panel) then
        Settings.WriteInteger(Section, SplitPosIdent, Panel.Width)
      else
        Settings.DeleteKey(Section, SplitPosIdent);
    end;
    if FFormChanges.RememberWidth and (not FFormChanges.RememberSize) then
      Settings.WriteInteger(Section, WidthIdent, FForm.Width);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TManagedForm.DoHookOnFormDestroy;
begin
  FOrigOnDestroy := FForm.OnDestroy;
  FForm.OnDestroy := FormDestroy;
end;

procedure TManagedForm.Init(const _FormChanges: TFormChanges);
begin
  Assert(Assigned(FForm));

  FFormChanges := _FormChanges;

  DoHookOnFormDestroy;
  DoMakeResizable;
  DoFixFormErrors;
  DoLoadFormState;
  DoCollapseTreeNodes;
  DoResizePictureDialogs;
  DoComboDropDownCount;
end;

procedure TManagedForm.FormDestroy(Sender: TObject);
var
  OrigOnDestroy: TNotifyEvent;
begin
  Assert(Assigned(FForm));
  Assert(FForm = Sender);

  // restore the original OnDestroy event
  // It's not being called, but there might be code in other plugins
  // (e.g. IdeFixpack or cnpack) that checks for it.
  FForm.OnDestroy := FOrigOnDestroy;
  // save it for later (see below)
  OrigOnDestroy := FOrigOnDestroy;
  // just in case, set FOrigOnDestroy to nil
  FOrigOnDestroy := nil;

  DoSaveFormState;

  if Assigned(OrigOnDestroy) then begin
    // Restoring the FForm.OnDestroy to its original value does not call OrigOnDestroy.
    // The reason is that FormDestroy gets called from OnDestroy;
    // OnDestroy is called from TCustomForm.DoDestroy which will never call OnDestroy restored from OrigOnDestroy.
    // So we need to explicitly call the old one:
    OrigOnDestroy(Sender);
    // Note we still restore the old OnDestroy value, just in case there is a check
    // for the value (some interceptors actively perform that check).
  end;
end;

procedure TManagedForm.MakeComponentsResizable;
begin
  // does nothing, is overridden in descendant forms if necessary
end;

end.
