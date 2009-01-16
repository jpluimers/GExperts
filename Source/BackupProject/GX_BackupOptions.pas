unit GX_BackupOptions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, GX_BaseForm;

type
  TfmBackupOptions = class(TfmBaseForm)
    gbBackupOptions: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbPassword: TCheckBox;
    lPassword: TLabel;
    edPassword: TEdit;
    rgScope: TRadioGroup;
    cbSearchLibraryPath: TCheckBox;
    procedure cbPasswordClick(Sender: TObject);
  end;

implementation

uses Graphics, GX_GenericUtils;

{$R *.dfm}

procedure TfmBackupOptions.cbPasswordClick(Sender: TObject);
begin
  edPassword.Enabled := cbPassword.Checked;
  if edPassword.Enabled then
  begin
    edPassword.Color := clWindow;
    TryFocusControl(edPassword);
  end
  else
    edPassword.Color := clBtnface;
  edPassword.Refresh;
end;

end.
