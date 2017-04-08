unit GX_IdeManagedForm;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  Windows;

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
  protected
    procedure DoFixFormErrors; virtual;
    procedure DoMakeResizable; virtual;
    procedure DoLoadFormState;
    procedure DoSaveFormState;
    procedure DoResizePictureDialogs;
    procedure DoComboDropDownCount;
    procedure MakeComponentsResizable; virtual;
    procedure DoCollapseTreeNodes; virtual;
  public
    FFormChanges: TFormChanges;
    class function GenerateName(const _FormName: string): string;
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
    procedure DoCollapseTreeNodes; override;
  end;

type
  TManagedFormConnEditForm = class(TManagedForm)
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
{$ENDIF}
  protected
    procedure MakeComponentsResizable; override;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
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
  StdCtrls,
  GX_ConfigurationInfo,
  GX_GenericUtils,
  GX_dzClassUtils,
  GX_IdeUtils;

const
  WidthIdent = 'Width';
  HeightIdent = 'Height';
  TopIdent = 'Top';
  LeftIdent = 'Left';
  SplitPosIdent = 'SplitPos';

type
  TCustomFormHack = class(TCustomForm)
  end;

{ TGxIdeFormEnhancerBase }

class function TManagedForm.GenerateName(const _FormName: string): string;
begin
  Result := 'GXManagedForm_' + _FormName;
end;

constructor TManagedForm.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  FForm := _Owner as TForm;
  Name := GenerateName(FForm.Name)
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

procedure TManagedFormImageListEditor.MakeComponentsResizable;
var
  ImageGroup: TGroupBox;
  TransparentColor: TComboBox;
  FillColor: TComboBox;
  OptionsGroup: TRadioGroup;
  GroupBox1: TGroupBox;
  ImageView: TListView;
  AddBtn: TButton;
  DeleteBtn: TButton;
  ClearBtn: TButton;
  ExportBtn: TButton;
  OkBtn: TButton;
  CancelBtn: TButton;
  ApplyBtn: TButton;
  HelpBtn: TButton;
begin
  // necessary only in Delphi 6
  if not TComponent_FindComponent(FForm, 'ImageGroup', False, TComponent(ImageGroup), TGroupBox)
    or not TComponent_FindComponent(FForm, 'TransparentColor', False, TComponent(TransparentColor), TComboBox)
    or not TComponent_FindComponent(FForm, 'FillColor', False, TComponent(FillColor), TComboBox)
    or not TComponent_FindComponent(FForm, 'OptionsGroup', False, TComponent(OptionsGroup), TRadioGroup)
    or not TComponent_FindComponent(FForm, 'GroupBox1', False, TComponent(GroupBox1), TGroupBox)
    or not TComponent_FindComponent(FForm, 'ImageView', False, TComponent(ImageView), TListView)
    or not TComponent_FindComponent(FForm, 'Add', False, TComponent(AddBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Delete', False, TComponent(DeleteBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Clear', False, TComponent(ClearBtn), TButton)
    or not TComponent_FindComponent(FForm, 'ExportBtn', False, TComponent(ExportBtn), TButton)
    or not TComponent_FindComponent(FForm, 'OK', False, TComponent(OkBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Cancel', False, TComponent(CancelBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Apply', False, TComponent(ApplyBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Help', False, TComponent(HelpBtn), TButton) then
    Exit; //==>

  // unfortunately we must restrict the height, otherwise the sort order of the icons gets
  // messed up. It's probably possible to fix that, but that's quite a lot of work for
  // the oldest supported IDE Delphi 6
  FMinHeight := -FMinHeight;

  ImageGroup.Anchors := [akLeft, akTop, akRight];
  TransparentColor.Anchors := [akLeft, akTop, akRight];
  FillColor.Anchors := [akLeft, akTop, akRight];
  OptionsGroup.Anchors := [akRight, akTop];
  GroupBox1.Anchors := [akLeft, akTop, akRight, akBottom];
  ImageView.Anchors := [akLeft, akTop, akRight, akBottom];
// if set, the icon order gets messed up
//  ImageView.IconOptions.Arrangement := iaTop;
  AddBtn.Anchors := [akRight, akBottom];
  DeleteBtn.Anchors := [akRight, akBottom];
  ClearBtn.Anchors := [akRight, akBottom];
  ExportBtn.Anchors := [akRight, akBottom];
  OkBtn.Anchors := [akRight, akTop];
  CancelBtn.Anchors := [akRight, akTop];
  ApplyBtn.Anchors := [akRight, akTop];
  HelpBtn.Anchors := [akRight, akTop];
end;
{$ENDIF}

{ TManagedFormPictureEditDlg }

procedure TManagedFormPictureEditDlg.MakeComponentsResizable;
var
  GroupBox1: TGroupBox;
  ImagePanel: TPanel;
  LoadBtn: TButton;
  SaveBtn: TButton;
  ClearBtn: TButton;
  OkBtn: TButton;
  CancelBtn: TButton;
  HelpBtn: TButton;
begin
  if not TComponent_FindComponent(FForm, 'GroupBox1', False, TComponent(GroupBox1), TGroupBox)
    or not TComponent_FindComponent(FForm, 'ImagePanel', False, TComponent(ImagePanel), TPanel)
    or not TComponent_FindComponent(FForm, 'Load', False, TComponent(LoadBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Save', False, TComponent(SaveBtn), TButton)
    or not TComponent_FindComponent(FForm, 'Clear', False, TComponent(ClearBtn), TButton)
    or not TComponent_FindComponent(FForm, 'OKButton', False, TComponent(OkBtn), TButton)
    or not TComponent_FindComponent(FForm, 'CancelButton', False, TComponent(CancelBtn), TButton)
    or not TComponent_FindComponent(FForm, 'HelpButton', False, TComponent(HelpBtn), TButton) then
    Exit; //==>

  GroupBox1.Anchors := [akLeft, akTop, akRight, akBottom];
  ImagePanel.Anchors := [akLeft, akTop, akRight, akBottom];
  LoadBtn.Anchors := [akRight, akBottom];
  SaveBtn.Anchors := [akRight, akBottom];
  ClearBtn.Anchors := [akRight, akBottom];
  OkBtn.Anchors := [akRight, akTop];
  CancelBtn.Anchors := [akRight, akTop];
  HelpBtn.Anchors := [akRight, akTop];
end;

{$IFNDEF GX_VER150_up} // Delphi 7

procedure TManagedFormSrchDialog.MakeComponentsResizable;
var
  PageControl: TPageControl;
  SearchText: TComboBox;
  FileSearchText: TComboBox;
  GroupBox4: TGroupBox; // stupid developer forgot to give it a meaningful name :-(
  DirSpec: TComboBox;
  BrowseButton: TButton;
  OKButton: TButton;
  CancelButton: TButton;
  HelpButton: TButton;
begin
  // This is only ever called in Delphi6 because the Search form of later
  // versions is already resizable.
  if not TComponent_FindComponent(FForm, 'PageControl', False, TComponent(PageControl), TPageControl)
    or not TComponent_FindComponent(FForm, 'SearchText', False, TComponent(SearchText), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'FileSearchText', False, TComponent(FileSearchText), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'GroupBox4', False, TComponent(GroupBox4), TGroupBox)
    or not TComponent_FindComponent(FForm, 'DirSpec', False, TComponent(DirSpec), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'BrowseButton', False, TComponent(BrowseButton), TButton)
    or not TComponent_FindComponent(FForm, 'OKButton', False, TComponent(OKButton), TButton)
    or not TComponent_FindComponent(FForm, 'CancelButton', False, TComponent(CancelButton), TButton)
    or not TComponent_FindComponent(FForm, 'HelpButton', False, TComponent(HelpButton), TButton) then
    Exit;

  FMinHeight := -FMinHeight;

  PageControl.Anchors := [akLeft, akTop, akRight, akBottom];
  SearchText.Anchors := [akLeft, akRight, akTop];
  FileSearchText.Anchors := [akLeft, akRight, akTop];
  GroupBox4.Anchors := [akLeft, akRight, akTop];
  DirSpec.Anchors := [akLeft, akRight, akTop];
  BrowseButton.Anchors := [akRight, akBottom];
  OKButton.Anchors := [akRight, akBottom];
  CancelButton.Anchors := [akRight, akBottom];
  HelpButton.Anchors := [akRight, akBottom];
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

procedure TManagedFormRplcDialog.MakeComponentsResizable;
var
  SearchText: TComboBox;
  ReplaceText: TComboBox;
  OKButton: TButton;
  ChangeAll: TButton;
  CancelButton: TButton;
  HelpButton: TButton;
begin
  // This is only ever called in Delphi6 because the Replace form of later
  // versions is already resizable.
  if not TComponent_FindComponent(FForm, 'SearchText', False, TComponent(SearchText), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'ReplaceText', False, TComponent(ReplaceText), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'OKButton', False, TComponent(OKButton), TButton)
    or not TComponent_FindComponent(FForm, 'ChangeAll', False, TComponent(ChangeAll), TButton)
    or not TComponent_FindComponent(FForm, 'CancelButton', False, TComponent(CancelButton), TButton)
    or not TComponent_FindComponent(FForm, 'HelpButton', False, TComponent(HelpButton), TButton) then
    Exit;

  FMinHeight := -FMinHeight;

  SearchText.Anchors := [akLeft, akRight, akTop];
  ReplaceText.Anchors := [akLeft, akRight, akTop];
  OKButton.Anchors := [akRight, akBottom];
  ChangeAll.Anchors := [akRight, akBottom];
  CancelButton.Anchors := [akRight, akBottom];
  HelpButton.Anchors := [akRight, akBottom];
end;
{$ENDIF}

{ TManagedFormDefaultEnvironmentDialog }

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

procedure TManagedFormConnEditForm.MakeComponentsResizable;
var
  SourceOfConnection: TGroupBox;
  DataLinkFile: TComboBox;
  Browse: TButton;
  ConnectionString: TEdit;
  Build: TButton;
  OKButton: TButton;
  CancelButton: TButton;
  HelpButton: TButton;
begin
  // There are two forms with that name. The one shown for TAdoConnection is not resizable
  if not TComponent_FindComponent(FForm, 'SourceOfConnection', False, TComponent(SourceOfConnection), TGroupBox)
    or not TComponent_FindComponent(FForm, 'DataLinkFile', False, TComponent(DataLinkFile), TCustomComboBox)
    or not TComponent_FindComponent(FForm, 'Browse', False, TComponent(Browse), TButton)
    or not TComponent_FindComponent(FForm, 'ConnectionString', False, TComponent(ConnectionString), TEdit)
    or not TComponent_FindComponent(FForm, 'Build', False, TComponent(Build), TButton)
    or not TComponent_FindComponent(FForm, 'OKButton', False, TComponent(OKButton), TButton)
    or not TComponent_FindComponent(FForm, 'CancelButton', False, TComponent(CancelButton), TButton)
    or not TComponent_FindComponent(FForm, 'HelpButton', False, TComponent(HelpButton), TButton) then
    Exit; // it's not the TAdoConnection one, we don't need to do anything

  FMinHeight := -FMinHeight;

  SourceOfConnection.Anchors := [akLeft, akTop, akRight];
  DataLinkFile.Anchors := [akLeft, akTop, akRight];
  Browse.Anchors := [akTop, akRight];
  ConnectionString.Anchors := [akLeft, akTop, akRight];
  Build.Anchors := [akTop, akRight];
  OKButton.Anchors := [akBottom, akRight];
  CancelButton.Anchors := [akBottom, akRight];
  HelpButton.Anchors := [akBottom, akRight];
end;

procedure TManagedFormPakComponentsDlg.MakeComponentsResizable;
var
  InstalledGroupBox: TGroupBox;
  ComponentsListBox: TListBox;
  OKButton: TButton;
  Button1: TButton; // Help button
  Cancel: TButton;
begin
  if not TComponent_FindComponent(FForm, 'InstalledGroupBox', False, TComponent(InstalledGroupBox), TGroupBox)
    or not TComponent_FindComponent(FForm, 'ComponentsListBox', False, TComponent(ComponentsListBox), TListBox)
    or not TComponent_FindComponent(FForm, 'OKButton', False, TComponent(OKButton), TButton)
    or not TComponent_FindComponent(FForm, 'Button1', False, TComponent(Button1), TButton)
    or not TComponent_FindComponent(FForm, 'Cancel', False, TComponent(Cancel), TButton) then
    Exit;

  InstalledGroupBox.Anchors := [akTop, akLeft, akRight, akBottom];
  ComponentsListBox.Anchors := [akTop, akLeft, akRight, akBottom];
  OKButton.Anchors := [akRight, akBottom];
  Button1.Anchors := [akRight, akBottom];
  Cancel.Visible := False;
end;

constructor TManagedFormPasEnvironmentDialog.Create(_Owner: TComponent);
begin
  inherited;
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
  FItems := TStringList.Create;
{$ENDIF}
end;

destructor TManagedFormPasEnvironmentDialog.Destroy;
begin
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
  FreeAndNil(FItems);
{$ENDIF}
  inherited;
end;

procedure TManagedFormPasEnvironmentDialog.MakeComponentsResizable;
// This is called in Delphi6 and Delphi 7 because the Environment form of later
// versions is already resizable.
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
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
{$ENDIF}
begin
{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
  HandleComponent(FForm);
{$ENDIF}
end;

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
    FMinHeight := FForm.Height;
    FMinWidth := FForm.Width;
    FForm.OnCanResize := FormCanResize;
    MakeComponentsResizable;
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

procedure TManagedForm.Init(const _FormChanges: TFormChanges);
begin
  Assert(Assigned(FForm));

  FFormChanges := _FormChanges;
  FOrigOnDestroy := FForm.OnDestroy;
  FForm.OnDestroy := FormDestroy;

  DoMakeResizable;
  DoFixFormErrors;
  DoLoadFormState;
  DoCollapseTreeNodes;
  DoResizePictureDialogs;
  DoComboDropDownCount;
end;

procedure TManagedForm.FormDestroy(Sender: TObject);
begin
  Assert(Assigned(FForm));
  Assert(FForm = Sender);

  FForm.OnDestroy := FOrigOnDestroy;
  FOrigOnDestroy := nil;
  DoSaveFormState;
end;

procedure TManagedForm.MakeComponentsResizable;
begin
  // does nothing, is overridden in decendant forms if necessary
end;

end.
