unit GX_CodeOpt;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls;

type
  TfmCodeOptions = class(TForm)
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
  end;

implementation

{$R *.dfm}

uses
  GX_GenericUtils;

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

procedure TfmCodeOptions.eNumericKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', #8]) then
    Key := #0;
end;

end.
