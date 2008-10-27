unit GX_ClassOptions;

interface

uses
  Classes, StdCtrls, Controls, ComCtrls, Forms, ExtCtrls;

type
  TfmClassOptions = class(TForm)
    pnlButtons: TPanel;
    pnlContent: TPanel;
    pnlButtonsRight: TPanel;
    pcClassOptions: TPageControl;
    tshGeneric: TTabSheet;
    gbxFonts: TGroupBox;
    lblTreeViewFont: TLabel;
    lblListViewFont: TLabel;
    lblEditorFont: TLabel;
    cbTreeView: TComboBox;
    cbListView: TComboBox;
    cbEditor: TComboBox;
    sTreeView: TEdit;
    sListView: TEdit;
    sEditor: TEdit;
    udTree: TUpDown;
    udList: TUpDown;
    udEditor: TUpDown;
    cbAutoHide: TCheckBox;
    tshFilters: TTabSheet;
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
    cbStayInPackage: TCheckBox;
    gbxSearch: TGroupBox;
    cbParseRecursing: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
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
