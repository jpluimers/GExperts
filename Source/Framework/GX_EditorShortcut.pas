unit GX_EditorShortcut;

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, Forms, GX_BaseForm;

type
  TfmEditorShortcut = class(TfmBaseForm)
    gbxShortCut: TGroupBox;
    lblShortCut: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    hkyShortCut: THotKey;
  end;

implementation

{$R *.dfm}

end.

