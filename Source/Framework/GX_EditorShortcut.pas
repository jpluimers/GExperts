unit GX_EditorShortcut;

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, Forms, GXHotKey;

type
  TfmEditorShortcut = class(TForm)
    gbxShortCut: TGroupBox;
    lblShortCut: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
  public
    hkyShortCut: TGXHotKey;
  end;

implementation

{$R *.dfm}

procedure TfmEditorShortcut.FormCreate(Sender: TObject);
begin
  hkyShortCut := TGXHotKey.Create(Self);
  hkyShortCut.Parent := gbxShortCut;
  hkyShortCut.SetBounds(lblShortCut.Left, lblShortCut.Top + lblShortCut.Height + 8, gbxShortCut.ClientWidth - (lblShortCut.Left * 2), 22);
  lblShortCut.FocusControl := hkyShortCut;
end;

end.

