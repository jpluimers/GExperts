unit GX_MacroLibraryConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmGxMacroLibraryConfig = class(TForm)
    chk_AutoPrompt: TCheckBox;
    b_Ok: TButton;
    b_Cancel: TButton;
  private
  public
    class function Execute(var APromptForName: boolean): boolean;
  end;

implementation

{$R *.dfm}

{ TfmGxMacroLibraryConfig }

class function TfmGxMacroLibraryConfig.Execute(var APromptForName: boolean): boolean;
var
  frm: TfmGxMacroLibraryConfig;
begin
  frm := TfmGxMacroLibraryConfig.Create(nil);
  try
    frm.chk_AutoPrompt.Checked := APromptForName;
    Result := mrOk = frm.ShowModal;
    if Result then
      APromptForName := frm.chk_AutoPrompt.Checked;
  finally
    frm.Free;
  end;
end;

end.
