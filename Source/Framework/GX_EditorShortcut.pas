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
    btnDefault: TButton;
    procedure btnDefaultClick(Sender: TObject);
  public
    FDefault: TShortCut;
    class function Execute(Owner: TWinControl; const Expert: string; var ShortCut: TShortCut;
      const Default: TShortCut): Boolean;
  end;

implementation

{$R *.dfm}

uses
  Menus;

{ TfmEditorShortcut }

class function TfmEditorShortcut.Execute(Owner: TWinControl; const Expert: string;
  var ShortCut: TShortCut; const Default: TShortCut): Boolean;
var
  fm: TfmEditorShortcut;
begin
  fm := TfmEditorShortcut.Create(Owner);
  try
    fm.FDefault := Default;
    fm.gbxShortCut.Caption := Expert;
    fm.hkyShortCut.HotKey := ShortCut;
    fm.btnDefault.Caption := ShortCutToText(Default);
    Result := (fm.ShowModal = mrOk);
    if Result then
      ShortCut := fm.hkyShortCut.HotKey;
  finally
    fm.Free;
  end;
end;

procedure TfmEditorShortcut.btnDefaultClick(Sender: TObject);
begin
  hkyShortCut.HotKey := FDefault;
end;

end.

