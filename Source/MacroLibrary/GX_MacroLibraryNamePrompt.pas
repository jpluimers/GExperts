unit GX_MacroLibraryNamePrompt;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TfmMacroLibraryNamePrompt = class(TForm)
    lblMacroName: TLabel;
    edtMacroName: TEdit;
    chkDoNotShowAgain: TCheckBox;
    b_OK: TButton;
    b_Cancel: TButton;
    lblMacroDesc: TLabel;
    mmoMacroDescription: TMemo;
  private
  public
    class function Execute(_Owner: TComponent; var _MacroName, _MacroDesc: string;
      var _PromptForName: boolean): boolean;
  end;

implementation

{$R *.dfm}

{ TTfmMacroLibraryNamePrompt }

class function TfmMacroLibraryNamePrompt.Execute(_Owner: TComponent;
  var _MacroName, _MacroDesc: string; var _PromptForName: boolean): boolean;
var
  frm: TfmMacroLibraryNamePrompt;
begin
  frm := TfmMacroLibraryNamePrompt.Create(_Owner);
  try
    frm.edtMacroName.Text := _MacroName;
    frm.mmoMacroDescription.Lines.Text := _MacroDesc;
    frm.chkDoNotShowAgain.Checked := False;
    Result := frm.ShowModal = mrOk;
    if Result then begin
      _MacroName := frm.edtMacroName.Text;
      _MacroDesc := frm.mmoMacroDescription.Lines.Text;
    end;
    // the checkbox is always evaluated
    _PromptForName := not frm.chkDoNotShowAgain.Checked;
  finally
    frm.Free;
  end;
end;

end.

