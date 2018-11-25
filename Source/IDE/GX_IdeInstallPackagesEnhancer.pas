unit GX_IdeInstallPackagesEnhancer;

{$I GX_CondDefine.inc}

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
  Windows,
  Classes,
  Forms,
  ExtCtrls,
  Controls,
  StdCtrls,
  CheckLst,
  Dialogs,
  ShellApi,
  GX_OtaUtils,
  GX_IdeFormEnhancer,
  GX_GenericUtils,
  GX_IdeDialogEnhancer,
  GX_dzClassUtils;

type
  TInstallPackagesEnhancer = class(TIdeDialogEnhancer)
  private
//    FPkgListBox: TCheckListBox;
    FPkgNameLabel: TLabel;
//    procedure b_RenameClick(_Sender: TObject);
    procedure b_SelectClick(_Sender: TObject);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); overload; override;
  public
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

function TInstallPackagesEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;

  function IsPackageChecklistForm(_Form: TCustomForm): Boolean;
  const
    DIALOG_CLASS = 'TDlgPackageCheckList';
    DIALOG_NAME = 'DlgPackageCheckList';
  begin
    Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
  end;

  function IsProjectOptionsForm(_Form: TCustomForm): Boolean;
  const
  {$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
    DIALOG_CLASS = 'TProjectOptionsDialog';
    DIALOG_NAME = 'ProjectOptionsDialog';
  {$ELSE}
    DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
    DIALOG_NAME = 'DelphiProjectOptionsDialog';
  {$ENDIF}
  begin
    Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
  end;

begin
  Result := IsProjectOptionsForm(_Form) or
    IsPackageChecklistForm(_Form);
end;

procedure TInstallPackagesEnhancer.EnhanceForm(_Form: TForm);
resourcestring
  rcSelectAModule = 'Open an explorer window with this package selected.';
const
  ExplorerButtonName = 'GXPackageExplorerButton';
var
  SelectBtn: TButton;
  cmp: TComponent;
  NamePnl: TPanel;
begin
// Unfortunately renaming packages (see b_RenameClick method) does not work.
// So we don't add the Rename button.

  if TComponent_FindComponent(_Form, ExplorerButtonName, True, cmp, TButton) then
    Exit; // the button already exists

  if not TComponent_FindComponent(_Form, 'LabelPackageFile', True, TComponent(FPkgNameLabel), TLabel) then
    Exit; // label not found, so we can't add the button

  if not (FPkgNameLabel.Parent is TPanel) then
    Exit; // the label should be on a panel

  NamePnl := TPanel(FPkgNameLabel.Parent);

  SelectBtn := TButton.Create(NamePnl);
  SelectBtn.Name := ExplorerButtonName;
  SelectBtn.Parent := NamePnl;
  SelectBtn.Width := NamePnl.ClientHeight;
  SelectBtn.Caption := '...';
  SelectBtn.Align := alRight;
  SelectBtn.OnClick := b_SelectClick;
  SelectBtn.Hint := rcSelectAModule;
  SelectBtn.ShowHint := True;
end;

procedure TInstallPackagesEnhancer.b_SelectClick(_Sender: TObject);
var
  fn: string;
  Wow64fn: string;
begin
  fn := FPkgNameLabel.Caption;
  if fn = '' then
    Exit;

  Wow64fn := StringReplace(fn, '\system32\', '\SysWOW64\', [rfIgnoreCase]);
  if FileExists(Wow64fn) then
    fn := Wow64fn;

  // open an explorer window and select the package file there
  ShellExecute(0, nil, 'explorer.exe', PChar('/select,' + fn), nil, SW_SHOWNORMAL)
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

end.
