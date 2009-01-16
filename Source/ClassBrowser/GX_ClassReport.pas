unit GX_ClassReport;

interface

uses
  StdCtrls, Controls, Classes, Forms, ComCtrls, GX_BaseForm;

type
  TfmClassReport = class(TfmBaseForm)
    gbxPageSettings: TGroupBox;
    lblFont: TLabel;
    lblFontSize: TLabel;
    lblBoxSize: TLabel;
    lblInCharacters: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblBoxSpacing: TLabel;
    lblInPixels: TLabel;
    cbxFont: TComboBox;
    spnFontSize: TEdit;
    spnBoxSize: TEdit;
    spnBoxSpacing: TEdit;
    udBoxSize: TUpDown;
    udBoxSpacing: TUpDown;
    udFontSize: TUpDown;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfmClassReport.FormCreate(Sender: TObject);
begin
  cbxFont.Items.Assign(Screen.Fonts);
end;

end.
