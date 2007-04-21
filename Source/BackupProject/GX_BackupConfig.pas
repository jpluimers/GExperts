unit GX_BackupConfig;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls;

type
  TfmBackupConfig = class(TForm)
    gbxOptions: TGroupBox;
    cbBackupInc: TCheckBox;
    cbIncludeDir: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblDirectives: TLabel;
    rgDefaultScope: TRadioGroup;
    gbBackupTarget: TGroupBox;
    rbBackupAskForFile: TRadioButton;
    rbBackupToDirectory: TRadioButton;
    lblBackupDir: TLabel;
    edBackupDir: TEdit;
    btnBackupDir: TButton;
    cbSearchOnLibraryPath: TCheckBox;
    btHelp: TButton;
    procedure btnBackupDirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbGenericBackupTargetClick(Sender: TObject);
    procedure cbBackupIncClick(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
  public
    procedure SetBackupTargetDirectoryEnabled(const Enable: Boolean);
  end;

implementation

{$R *.dfm}

uses
  GX_GenericUtils, GX_GxUtils;

procedure TfmBackupConfig.btnBackupDirClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edBackupDir.Text;
  if GetDirectory(Temp) then
    edBackupDir.Text := Temp;
end;

procedure TfmBackupConfig.FormShow(Sender: TObject);
begin
  cbBackupIncClick(nil);
end;

procedure TfmBackupConfig.SetBackupTargetDirectoryEnabled(const Enable: Boolean);
begin
  lblBackupDir.Enabled := Enable;
  edBackupDir.Enabled := Enable;
  btnBackupDir.Enabled := Enable;
end;

procedure TfmBackupConfig.rbGenericBackupTargetClick(Sender: TObject);
begin
  SetBackupTargetDirectoryEnabled(rbBackupToDirectory.Checked);
end;

procedure TfmBackupConfig.cbBackupIncClick(Sender: TObject);
begin
  cbSearchOnLibraryPath.Enabled := cbBackupInc.Checked;
end;

procedure TfmBackupConfig.btHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 10);
end;

end.

