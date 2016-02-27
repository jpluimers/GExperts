unit GX_IdeInstallPackagesEnhancer;

interface

uses
  SysUtils;

type
  TGxIdeInstallPackagesEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  GX_IdeFormEnhancer,
  GX_OtaUtils,
  StrUtils,
  Forms,
  ExtCtrls,
  Controls,
  StdCtrls,
  CheckLst,
  Dialogs,
  ShellApi,
  Windows,
  Registry,
  GX_GenericUtils,
  Classes,
  GX_IdeUtils;

type
  TInstallPackagesEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    FPkgListBox: TCheckListBox;
    FPkgNameLabel: TLabel;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsProjectOptionsForm(_Form: TCustomForm): Boolean;
    function IsPackageChecklistForm(_Form: TCustomForm): Boolean;
    procedure EnhancePackageChecklistForm(_Form: TCustomForm);
    procedure EnhanceProjectOptionsForm(_Form: TCustomForm);
    function TryGetPackageInstallControlsOnProjectOptions(_Form: TCustomForm;
      out _grp: TGroupBox; out _AddBtn: TButton; out _ChkLst: TCheckListBox;
      out _NamePnl: TPanel; out _NameLabel: TLabel): Boolean;
//    procedure b_RenameClick(_Sender: TObject);
    procedure b_SelectClick(_Sender: TObject);
    function TryGetPackageInstallControlsOnPackageChecklist(_Form: TCustomForm;
      out _BtnPanel: TPanel; out _AddBtn: TButton; out _ChkLst: TCheckListBox;
      out _NamePnl: TPanel;
      out _NameLabel: TLabel): Boolean;
    procedure EnhanceForm(_Form: TCustomForm; _BtnParent: TWinControl;
      _AddBtn: TButton; _FilenamePanel: TPanel);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheInstallPackagesEnhancer: TInstallPackagesEnhancer = nil;

{ TGxIdeInstallPackagesEnhancer }

class function TGxIdeInstallPackagesEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheInstallPackagesEnhancer);
end;

class procedure TGxIdeInstallPackagesEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheInstallPackagesEnhancer) then
      TheInstallPackagesEnhancer := TInstallPackagesEnhancer.Create
  end else
    FreeAndNil(TheInstallPackagesEnhancer);
end;

{ TInstallPackagesEnhancer }

constructor TInstallPackagesEnhancer.Create;
begin
  inherited Create;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TInstallPackagesEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
  inherited;
end;

procedure TInstallPackagesEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
begin
  if IsProjectOptionsForm(_Form) then begin
    EnhanceProjectOptionsForm(_Form);
  end else if IsPackageChecklistForm(_Form) then begin
    EnhancePackageChecklistForm(_Form);
  end;
end;

function TInstallPackagesEnhancer.IsPackageChecklistForm(_Form: TCustomForm): Boolean;
begin
  Result := (_Form.ClassName = 'TDlgPackageCheckList') and (_Form.Name = 'DlgPackageCheckList');
end;

function TInstallPackagesEnhancer.IsProjectOptionsForm(_Form: TCustomForm): Boolean;
begin
  Result := (_Form.ClassName = 'TDelphiProjectOptionsDialog') and (_Form.Name = 'DelphiProjectOptionsDialog')
    or (_Form.ClassName = 'TProjectOptionsDialog') and (_Form.Name = 'ProjectOptionsDialog');
end;

function TInstallPackagesEnhancer.TryGetPackageInstallControlsOnProjectOptions(_Form: TCustomForm;
  out _grp: TGroupBox; out _AddBtn: TButton; out _ChkLst: TCheckListBox;
  out _NamePnl: TPanel; out _NameLabel: TLabel): Boolean;
var
  ctrl: TControl;
  wctrl: TWinControl;
  i: Integer;
  PackageInstall: TWinControl;
begin
  Result := False;

  wctrl := nil;
  for i := 0 to _Form.ControlCount - 1 do begin
    ctrl := _Form.Controls[i];
    if (ctrl.ClassName = 'TPanel') and (ctrl.Name = 'Panel2') then begin
      wctrl := TWinControl(ctrl);
       Break;
    end;
  end;
  if not Assigned(wctrl) then
    Exit;

  if (wctrl.ControlCount > 0) and (wctrl.Controls[0].ClassName = 'TPropertySheetControl') then
    ctrl := wctrl.Controls[0]
  else if (wctrl.ControlCount > 1) and (wctrl.Controls[1].ClassName = 'TPropertySheetControl') then
    ctrl := wctrl.Controls[1]
  else
    exit;
  wctrl := TWinControl(ctrl);

  PackageInstall := nil;
  for i := 0 to wctrl.ControlCount - 1 do begin
    ctrl := wctrl.Controls[i];
    if ctrl.ClassName = 'TPackageInstall' then begin
      PackageInstall := TWinControl(ctrl);
      break;
    end;
  end;
  if (PackageInstall = nil) or (PackageInstall.ControlCount < 1) then
    Exit;
  ctrl := PackageInstall.Controls[0];
  if ctrl.ClassName <> 'TGroupBox' then
    Exit;
  _grp := TGroupBox(ctrl);

  if _grp.ControlCount <> 6 then
    Exit;

  ctrl := _grp.Controls[0];
  if (ctrl.ClassName <> 'TCheckListBox') or (ctrl.Name <> 'DesignPackageList') then
    Exit;
  _ChkLst := TCheckListBox(ctrl);

  ctrl := _grp.Controls[2];
  if (ctrl.ClassName <> 'TButton') or (ctrl.Name <> 'AddButton') then
    Exit;
  _AddBtn := TButton(ctrl);

  ctrl := _grp.Controls[1];
  if (ctrl.ClassName <> 'TPanel') or (ctrl.Name <> 'Panel1') then
    Exit;

  _NamePnl := TPanel(ctrl);
  if _NamePnl.ControlCount < 1 then
    Exit;
  ctrl := _NamePnl.Controls[0];
  if (ctrl.ClassName <> 'TLabel') or (ctrl.Name <> 'LabelPackageFile') then
    Exit;
  _NameLabel := TLabel(ctrl);

  Result := True;
end;

// when called via Component -> Install Packages and a project is loaded
// or via Project -> Options:
// DelphiProjectOptionsDialog !or! ProjectOptionsDialog: TDelphiProjectOptionsDialog !or! TProjectOptionsDialog
// -> [1, 2 or 3] Panel2: TPanel
//    -> [0 or 1] PropertySheetControl1 !or! PropSheetControl: TpropertySheetControl
//       -> [??] '': TPackageInstall
//          -> [0] GroupBox1: TGroupBox !or! [0] gbDesignPackages: TGroupBox
//             -> [0] DesignPackageList: TCheckListBox
//             -> [1] Panel1: TPanel
//                -> [0] LabelPackageFile: TLabel
//             -> [2] AddButton: TButton
//             -> [3] RemoveButton: TButton
//             -> [4] ButtonComponents: TButton
//             -> [5] EditButton: TButton

procedure TInstallPackagesEnhancer.EnhanceForm(_Form: TCustomForm; _BtnParent: TWinControl;
  _AddBtn: TButton; _FilenamePanel: TPanel);
resourcestring
  rcSelectAModule = 'Open an explorer window with this package selected.';
const
  ExplorerButtonName = 'GXExplorerButton';
var
//  RenameBtn: TButton;
  SelectBtn: TButton;
  cmp: TComponent;
begin
// Unfortunately renaming packages (see b_RenameClick method) does not work.
// So we don't add the Rename button.
//  RenameBtn := TButton.Create(_BtnParent);
//  RenameBtn.Name := '';
//  RenameBtn.Parent := _BtnParent;
//  RenameBtn.Left := _AddBtn.Left - RenameBtn.Width - 8;
//  RenameBtn.Top := _AddBtn.Top;
//  RenameBtn.Caption := 'Rename';
//  RenameBtn.Anchors := _AddBtn.Anchors;
//  RenameBtn.TabOrder := _AddBtn.TabOrder;
//  RenameBtn.OnClick := b_RenameClick;

  cmp := _FilenamePanel.FindComponent(ExplorerButtonName);
  if Assigned(cmp) then
    exit; // the button already exists

  SelectBtn := TButton.Create(_FilenamePanel);
  SelectBtn.Name := ExplorerButtonName;
  SelectBtn.Parent := _FilenamePanel;
  SelectBtn.Width := _FilenamePanel.ClientHeight;
  SelectBtn.Caption := '...';
  SelectBtn.Align := alRight;
  SelectBtn.OnClick := b_SelectClick;
  SelectBtn.Hint := rcSelectAModule;
  SelectBtn.ShowHint := True;
end;

procedure TInstallPackagesEnhancer.EnhanceProjectOptionsForm(_Form: TCustomForm);
var
  grp: TGroupBox;
  pnl: TPanel;
  AddBtn: TButton;
begin
  if not TryGetPackageInstallControlsOnProjectOptions(_Form, grp, AddBtn, FPkgListBox, pnl, FPkgNameLabel) then
    Exit;
  EnhanceForm(_Form, grp, AddBtn, pnl);
end;

procedure TInstallPackagesEnhancer.b_SelectClick(_Sender: TObject);
begin
  // open an explrer window and select the package file there
  ShellExecute(0, nil, 'explorer.exe', PChar('/select,' + FPkgNameLabel.Caption), nil, SW_SHOWNORMAL)
end;

//procedure TInstallPackagesEnhancer.b_RenameClick(_Sender: TObject);
//var
//  Description: string;
//  fn: string;
//  idx: Integer;
//  Reg: TRegistry;
//  BDS: string;
//  fno: string;
//  Desc: string;
//begin
//  // Unfortunately the IDE seems to ignore the string that is used in the registry.
//  // Maybe it reads the description directly from the package? So this doesn't work as expected.
//  idx := FPkgListBox.ItemIndex;
//  if idx = -1 then
//    Exit;
//  fn := FPkgNameLabel.Caption;
//  Description := FPkgListBox.Items[idx];
//  Reg := TRegistry.Create;
//  try
//    Reg.RootKey := HKEY_CURRENT_USER;
//    if not Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + '\Known Packages', True) then
//      Exit;
//    try
//      if not Reg.ValueExists(fn) then begin
//        BDS := RemoveSlash(GetIdeRootDirectory);
//
//        fno := ExtractFileName(fn);
//        if not SameText(fn, BDS + '\bin\' + fno) then
//          Exit;
//        fn := '$(BDS)\bin\' + fno;
//        if not Reg.ValueExists(fn) then
//          Exit;
//      end;
//      Desc := Reg.ReadString(fn);
//      if not TfmIdxPackageRenameDlg.Execute(GetParentForm(TControl(_Sender)), fn, Desc) then
//        Exit;
//      Reg.WriteString(fn, Desc);
//    finally
//      Reg.CloseKey;
//    end;
//  finally
//    Reg.Free;
//  end;
//end;

procedure TInstallPackagesEnhancer.EnhancePackageChecklistForm(_Form: TCustomForm);
var
  BtnParent: TPanel;
  FilenamePanel: TPanel;
  AddBtn: TButton;
begin
  if not TryGetPackageInstallControlsOnPackageChecklist(_Form, BtnParent, AddBtn, FPkgListBox, FilenamePanel, FPkgNameLabel) then
    Exit;
  EnhanceForm(_Form, BtnParent, AddBtn, FilenamePanel);
end;

function TInstallPackagesEnhancer.TryGetPackageInstallControlsOnPackageChecklist(_Form: TCustomForm;
  out _BtnPanel: TPanel; out _AddBtn: TButton; out _ChkLst: TCheckListBox;
  out _NamePnl: TPanel; out _NameLabel: TLabel): Boolean;
var
  ctrl: TControl;
  wctrl: TWinControl;
begin
  Result := False;

  if _Form.ControlCount < 4 then
    Exit;

  ctrl := _Form.Controls[3];
  if ctrl.ClassName <> 'TGroupBox' then
    Exit;
  wctrl := TGroupBox(ctrl);

  if wctrl.ControlCount < 1 then
    Exit;

  ctrl := wctrl.Controls[0];
  if (ctrl.ClassName <> 'TFramePackageCheckList') or (ctrl.Name <> 'FramePackageCheckList1') then
    Exit;
  wctrl := TWinControl(ctrl);

  if wctrl.ControlCount < 1 then
    Exit;
  ctrl := wctrl.Controls[0];
  if (ctrl.ClassName <> 'TPanel') and (ctrl.Name <> 'Panel2') then
    Exit;
  wctrl := TWinControl(ctrl);
  _BtnPanel := TPanel(wctrl);

  if wctrl.ControlCount < 6 then
    Exit;

  ctrl := wctrl.Controls[1];
  if (ctrl.ClassName <> 'TCheckListBox') or (ctrl.Name <> 'DesignPackageList') then
    Exit;
  _ChkLst := TCheckListBox(ctrl);

  ctrl := wctrl.Controls[2];
  if (ctrl.ClassName <> 'TButton') or (ctrl.Name <> 'AddButton') then
    Exit;
  _AddBtn := TButton(ctrl);

  ctrl := wctrl.Controls[0];
  if (ctrl.ClassName <> 'TPanel') or (ctrl.Name <> 'Panel1') then
    Exit;

  _NamePnl := TPanel(ctrl);
  if _NamePnl.ControlCount < 1 then
    Exit;
  ctrl := _NamePnl.Controls[0];
  if (ctrl.ClassName <> 'TLabel') or (ctrl.Name <> 'LabelPackageFile') then
    Exit;
  _NameLabel := TLabel(ctrl);

  Result := True;
end;
// when called via Component -> Install Packages and no project is loaded:
// DlgPackageCheckList: TDlgPackageCheckList
// -> [3] GroupBox1: TGroupBox
//    -> [0] FramePackageCheckList1: TFramePackageCheckList
//       -> [0] Panel2: TPanel
//          -> [0] Panel1: TPanel
//             -> [0] LabelPackageFile: TLabel
//          -> [1] DesignPackageList: TCheckListBox
//          -> [2] AddButton: TButton
//          -> [3] RemoveButton: TButton
//          -> [4] ButtonComponents: TButton
//          -> [5] EditButton: TButton

end.
