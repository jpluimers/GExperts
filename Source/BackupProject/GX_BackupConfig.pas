unit GX_BackupConfig;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, GX_BaseForm;

type
  TfmBackupConfig = class(TfmBaseForm)
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
    gbDropDirs: TGroupBox;
    cbAddRecursively: TCheckBox;
    cbIgnoreHistoryDir: TCheckBox;
    cbIgnoreScmDirs: TCheckBox;
    cbIgnoreBackupFiles: TCheckBox;
    btnDefault: TButton;
    procedure btnBackupDirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbGenericBackupTargetClick(Sender: TObject);
    procedure cbBackupIncClick(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
    procedure cbAddRecursivelyClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
  private
    procedure edBackupDirOnDropFiles(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
    procedure SetBackupTargetDirectoryEnabled(const Enable: Boolean);
  end;

implementation

{$R *.dfm}

uses
  GX_GenericUtils, GX_GxUtils, GX_dzVclUtils, GX_MacroParser;

constructor TfmBackupConfig.Create(_Owner: TComponent);
begin
  inherited;
  TWinControl_ActivateDropFiles(edBackupDir, edBackupDirOnDropFiles);
  TEdit_ActivateAutoComplete(edBackupDir, [acsFileSystem], [actSuggest]);
end;


procedure TfmBackupConfig.edBackupDirOnDropFiles(_Sender: TObject; _Files: TStrings);
begin
  edBackupDir.Text := _Files[0];
end;

procedure TfmBackupConfig.btnBackupDirClick(Sender: TObject);
resourcestring
  SSaveDialogCaption = 'File to save to';
var
  Temp: string;
begin
  Temp := edBackupDir.Text;
  Temp := ReplaceStrings(Temp, True);
  if ShowSaveDialog(SSaveDialogCaption, 'zip', Temp) then
    edBackupDir.Text := Temp;
end;

procedure TfmBackupConfig.btnDefaultClick(Sender: TObject);
begin
  edBackupDir.Text := AddSlash('%PROJECTDIR%') + '%PROJECTNAME%';
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

procedure TfmBackupConfig.cbAddRecursivelyClick(Sender: TObject);
var
  b: Boolean;
begin
  b := cbAddRecursively.Checked;
  cbIgnoreHistoryDir.Enabled := b;
  cbIgnoreScmDirs.Enabled := b;
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

