unit GX_PeInfoPrint;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  GX_BaseForm;

type
  TPeInfoTabs = (pitMsDos, pitPeHeader, pitPeOptHeader, pitImports, pitExports, pitVersionInfo, pitPackageInfo);

  TPeInfoTabSet = set of TPeInfoTabs;

type
  Tf_PeInfoPrint = class(TfmBaseForm)
    chk_MsDosHeader: TCheckBox;
    chk_PeHeader: TCheckBox;
    chk_PeOptional: TCheckBox;
    chk_Exports: TCheckBox;
    chk_Imports: TCheckBox;
    chk_VersionInfo: TCheckBox;
    chk_PackageInfo: TCheckBox;
    b_CheckAll: TButton;
    b_CheckNone: TButton;
    b_Print: TButton;
    b_Cancel: TButton;
    procedure b_CheckAllClick(Sender: TObject);
    procedure b_CheckNoneClick(Sender: TObject);
  private
    procedure SetData(_Tabs: TPeInfoTabSet);
    procedure GetData(out _Tabs: TPeInfoTabSet);
  public
    class function Execute(_Owner: TComponent; var _Tabs: TPeInfoTabSet): Boolean;
  end;

implementation

uses
  GX_dzVclUtils;

{$R *.dfm}

{ Tf_PeInfoPrint }

class function Tf_PeInfoPrint.Execute(_Owner: TComponent; var _Tabs: TPeInfoTabSet): Boolean;
var
  frm: Tf_PeInfoPrint;
  r: TRect;
begin
  frm := Tf_PeInfoPrint.Create(_Owner);
  try
    r := frm.BoundsRect;
    TMonitor_MakeFullyVisible(frm.Monitor, r);
    frm.BoundsRect := r;
    frm.SetData(_Tabs);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Tabs);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_PeInfoPrint.b_CheckAllClick(Sender: TObject);
begin
  chk_MsDosHeader.Checked := True;
  chk_PeHeader.Checked := True;
  chk_PeOptional.Checked := True;
  chk_Imports.Checked := True;
  chk_Exports.Checked := True;
  chk_VersionInfo.Checked := True;
  chk_PackageInfo.Checked := True;
end;

procedure Tf_PeInfoPrint.b_CheckNoneClick(Sender: TObject);
begin
  chk_MsDosHeader.Checked := False;
  chk_PeHeader.Checked := False;
  chk_PeOptional.Checked := False;
  chk_Imports.Checked := False;
  chk_Exports.Checked := False;
  chk_VersionInfo.Checked := False;
  chk_PackageInfo.Checked := False;
end;

procedure Tf_PeInfoPrint.GetData(out _Tabs: TPeInfoTabSet);
begin
  _Tabs := [];
  if chk_MsDosHeader.Checked then
    Include(_Tabs, pitMsDos);
  if chk_PeHeader.Checked then
    Include(_Tabs, pitPeHeader);
  if chk_PeOptional.Checked then
    Include(_Tabs, pitPeOptHeader);
  if chk_Imports.Checked then
    Include(_Tabs, pitImports);
  if chk_Exports.Checked then
    Include(_Tabs, pitExports);
  if chk_VersionInfo.Checked then
    Include(_Tabs, pitVersionInfo);
  if chk_PackageInfo.Checked then
    Include(_Tabs, pitPackageInfo);
end;

procedure Tf_PeInfoPrint.SetData(_Tabs: TPeInfoTabSet);
begin
  chk_MsDosHeader.Checked := pitMsDos in _Tabs;
  chk_PeHeader.Checked := pitPeHeader in _Tabs;
  chk_PeOptional.Checked := pitPeOptHeader in _Tabs;
  chk_Imports.Checked := pitImports in _Tabs;
  chk_Exports.Checked := pitExports in _Tabs;
  chk_VersionInfo.Checked := pitVersionInfo in _Tabs;
  chk_PackageInfo.Checked := pitPackageInfo in _Tabs;
end;

end.
