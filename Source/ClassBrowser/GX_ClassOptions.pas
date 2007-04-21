unit GX_ClassOptions;

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, Forms;

type
  TfmClassOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    pcClassOptions: TPageControl;
    tshGeneric: TTabSheet;
    tshFilters: TTabSheet;
    gbxFonts: TGroupBox;
    lblTreeViewFont: TLabel;
    lblListViewFont: TLabel;
    gbxFilters: TGroupBox;
    cbConstants: TCheckBox;
    cbMethods: TCheckBox;
    cbTypes: TCheckBox;
    cbVariables: TCheckBox;
    cbProperties: TCheckBox;
    cbPrivate: TCheckBox;
    cbProtected: TCheckBox;
    cbPublic: TCheckBox;
    cbPublished: TCheckBox;
    gbxDiagram: TGroupBox;
    cbTop: TCheckBox;
    lblEditorFont: TLabel;
    cbTreeView: TComboBox;
    cbListView: TComboBox;
    cbEditor: TComboBox;
    cbStayInPackage: TCheckBox;
    gbxSearch: TGroupBox;
    cbParseRecursing: TCheckBox;
    cbAutoHide: TCheckBox;
    sTreeView: TEdit;
    sListView: TEdit;
    sEditor: TEdit;
    udTree: TUpDown;
    udList: TUpDown;
    udEditor: TUpDown;
    procedure FormCreate(Sender: TObject);
  end;


implementation

{$R *.dfm}

procedure TfmClassOptions.FormCreate(Sender: TObject);
begin
  cbTreeView.Items.Assign(Screen.Fonts);
  cbListView.Items.Assign(Screen.Fonts);
  cbEditor.Items.Assign(Screen.Fonts);
end;

end.
