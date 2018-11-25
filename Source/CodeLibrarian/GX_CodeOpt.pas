unit GX_CodeOpt;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, GX_BaseForm;

type
  TfmCodeOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    pgeCodeOpt: TPageControl;
    tabPaths: TTabSheet;
    tabLayout: TTabSheet;
    tabFonts: TTabSheet;
    lblStoragePath: TLabel;
    edPath: TEdit;
    sbBrowse: TButton;
    rbSide: TRadioButton;
    pnlSideSide: TPanel;
    shpLeft: TShape;
    shpRight: TShape;
    rbTop: TRadioButton;
    pnlTopBottom: TPanel;
    shpTop: TShape;
    shpBottom: TShape;
    lblTreeView: TLabel;
    lblEditor: TLabel;
    fcTreeview: TComboBox;
    fcEditor: TComboBox;
    lblSize: TLabel;
    udTreeview: TUpDown;
    udEditor: TUpDown;
    eTreeview: TEdit;
    eEditor: TEdit;
    procedure sbBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure eNumericKeyPress(Sender: TObject; var Key: Char);
  private
    procedure edPathOnFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_GenericUtils, GX_dzVclUtils;

procedure TfmCodeOptions.sbBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edPath.Text;
  if GetDirectory(Temp) then
    edPath.Text := Temp;
end;

procedure TfmCodeOptions.FormCreate(Sender: TObject);
begin
  fcTreeview.Items.Assign(Screen.Fonts);
  fcEditor.Items.Assign(Screen.Fonts);
end;

constructor TfmCodeOptions.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetConstraints(Self, [ccMinWidth, ccMinHeight, ccMaxHeight]);
  TWinControl_ActivateDropFiles(edPath, edPathOnFilesDropped);
  TEdit_ActivateAutoComplete(edPath, [acsFileSystem], [actSuggest]);
end;

procedure TfmCodeOptions.eNumericKeyPress(Sender: TObject; var Key: Char);
begin
  if not IsCharNumeric(Key) or IsCharTab(Key) then
    Key := #0;
end;

procedure TfmCodeOptions.edPathOnFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  edPath.Text := _Files[0];
end;

end.
