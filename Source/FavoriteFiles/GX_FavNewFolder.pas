unit GX_FavNewFolder;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, GX_BaseForm;

type
  TfmFavNewFolder = class(TfmBaseForm)
    gbxNewFolder: TGroupBox;
    lblFolderName: TLabel;
    edtFolderName: TEdit;
    lblFolderType: TLabel;
    cbxFolderType: TComboBox;
    btnCancel: TButton;
    btnOK: TButton;
    procedure edtFolderNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFolderTypeMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
    FFavoriteFilesForm: TForm;
  public
    property FavoriteFilesForm: TForm write FFavoriteFilesForm;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Graphics, GX_FavUtil, GX_FavFiles;

procedure TfmFavNewFolder.edtFolderNameChange(Sender: TObject);
begin
  btnOK.Enabled := (Length(edtFolderName.Text) > 0);
end;

procedure TfmFavNewFolder.FormCreate(Sender: TObject);
var
  i: TFolderType;
begin
  for i := Low(TFolderType) to High(TFolderType) do
    cbxFolderType.Items.AddObject(FolderNames[i], Pointer(i));
  cbxFolderType.ItemIndex := 0;
end;

procedure TfmFavNewFolder.cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  try
    with cbxFolderType.Canvas do
    begin
      if odSelected in State then
        Brush.Color := clHighlight
      else
        Brush.Color := clWindow;
      FillRect(Rect);
      (FFavoriteFilesForm as TfmFavFiles).ilFolders.Draw(cbxFolderType.Canvas,
        Rect.Left+3, Rect.Top+1, Index*2);
      TextOut(Rect.Left+22, Rect.Top+3, cbxFolderType.Items[Index]);
    end;
  except
    on E: Exception do
    begin
      // ignore
    end;
  end;
end;

procedure TfmFavNewFolder.cbxFolderTypeMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := 18;
end;

end.
