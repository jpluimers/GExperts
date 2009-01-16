unit GX_FavFileProp;

interface

uses
  Classes, Controls, Forms, ExtCtrls, StdCtrls, ComCtrls, GX_BaseForm;

type
  TfmFavFileProp = class(TfmBaseForm)
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    lblFile: TLabel;
    lblName: TLabel;
    lblDescription: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    edtName: TEdit;
    edtDescription: TEdit;
    imgFileIcon: TImage;
    lblIcon: TLabel;
    lblExecuteType: TLabel;
    cbxExecuteType: TComboBox;
    edtFilename: TEdit;
    sbnFile: TButton;
    lblExecuteUsing: TLabel;
    edtExecuteUsing: TEdit;
    sbnExecute: TButton;
    procedure sbnFileClick(Sender: TObject);
    procedure edtFilenameExit(Sender: TObject);
    procedure sbnExecuteClick(Sender: TObject);
    procedure cbxExecuteTypeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFavoriteFilesForm: TForm;
  public
    property FavoriteFilesForm: TForm write FFavoriteFilesForm;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Graphics, Dialogs, GX_FavFiles, GX_FavUtil;

procedure TfmFavFileProp.sbnFileClick(Sender: TObject);
var
  TheForm: TfmFavFiles;
begin
  TheForm := (FFavoriteFilesForm as TfmFavFiles);
  TheForm.SetFilter;

  if FileExists(edtFileName.Text) then
    TheForm.dlgGetFiles.FileName := edtFileName.Text;

  if TheForm.dlgGetFiles.Execute then
  begin
    edtFilename.Text := TheForm.dlgGetFiles.FileName;
    TheForm.AssignIconImage(imgFileIcon, edtFileName.Text);
  end;
end;

procedure TfmFavFileProp.edtFilenameExit(Sender: TObject);
begin
  (FFavoriteFilesForm as TfmFavFiles).AssignIconImage(imgFileIcon, edtFileName.Text);
end;

procedure TfmFavFileProp.sbnExecuteClick(Sender: TObject);
var
  TheForm: TfmFavFiles;
begin
  TheForm := FFavoriteFilesForm as TfmFavFiles;

  TheForm.dlgGetFiles.FilterIndex := 7;
  if FileExists(edtExecuteUsing.Text) then
    TheForm.dlgGetFiles.FileName := edtExecuteUsing.Text;
  if TheForm.dlgGetFiles.Execute then
    edtExecuteUsing.Text := TheForm.dlgGetFiles.FileName;
end;

procedure TfmFavFileProp.cbxExecuteTypeClick(Sender: TObject);
var
  ItemEnabled: Boolean;
begin
  ItemEnabled := (cbxExecuteType.ItemIndex = 2);

  sbnExecute.Enabled := ItemEnabled;
  edtExecuteUsing.Enabled := ItemEnabled;
  if ItemEnabled then
    edtExecuteUsing.Color := clWindow
  else
    edtExecuteUsing.Color := clBtnFace;
end;

procedure TfmFavFileProp.FormActivate(Sender: TObject);
begin
  cbxExecuteTypeClick(cbxExecuteType);
end;

procedure TfmFavFileProp.FormCreate(Sender: TObject);
var
  ExecType: TExecType;
begin
  cbxExecuteType.Items.Clear;
  for ExecType := Low(TExecType) to High(TExecType) do
    cbxExecuteType.Items.AddObject(ExecTypeNames[ExecType], TObject(Ord(ExecType)));
end;

end.

