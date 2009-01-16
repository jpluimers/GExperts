unit GX_MacroLibraryNamePrompt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GX_BaseForm;

type
  TfmMacroLibraryNamePrompt = class(TfmBaseForm)
    lblMacroName: TLabel;
    edtMacroName: TEdit;
    chkDoNotShowAgain: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblMacroDesc: TLabel;
    mmoMacroDescription: TMemo;
  public
    class function Execute(AOwner: TComponent; var AMacroName, AMacroDesc: string;
      var APromptForName: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TTfmMacroLibraryNamePrompt }

class function TfmMacroLibraryNamePrompt.Execute(AOwner: TComponent;
  var AMacroName, AMacroDesc: string; var APromptForName: Boolean): Boolean;
var
  Form: TfmMacroLibraryNamePrompt;
begin
  Form := TfmMacroLibraryNamePrompt.Create(AOwner);
  try
    Form.edtMacroName.Text := AMacroName;
    Form.mmoMacroDescription.Lines.Text := AMacroDesc;
    Form.chkDoNotShowAgain.Checked := False;
    Result := (Form.ShowModal = mrOk);
    if Result then begin
      AMacroName := Form.edtMacroName.Text;
      AMacroDesc := Form.mmoMacroDescription.Lines.Text;
    end;
    // The checkbox is always evaluated
    APromptForName := not Form.chkDoNotShowAgain.Checked;
  finally
    FreeAndNil(Form);
  end;
end;

end.

