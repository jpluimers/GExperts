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
  SysUtils, Menus, GX_dzVclUtils;

{ TfmEditorShortcut }

class function TfmEditorShortcut.Execute(Owner: TWinControl; const Expert: string;
  var ShortCut: TShortCut; const Default: TShortCut): Boolean;
var
  Dlg: TfmEditorShortcut;
begin
  Dlg := TfmEditorShortcut.Create(Owner);
  try
    Dlg.FDefault := Default;
    Dlg.gbxShortCut.Caption := Expert;
    THotkey_SetHotkey(Dlg.hkyShortCut, ShortCut);
    Dlg.btnDefault.Caption := ShortCutToText(Default);
    Result := (Dlg.ShowModal = mrOk);
    if Result then
      ShortCut := THotkey_GetHotkey(Dlg.hkyShortCut);
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmEditorShortcut.btnDefaultClick(Sender: TObject);
begin
  hkyShortCut.HotKey := FDefault;
end;

end.

