unit GX_eChangeCase;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, Forms;

type
  TfmChangeCase = class(TForm)
    grpCaseSelection: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

uses SysUtils, GX_EditorExpert, GX_GenericUtils, GX_eSelectionEditorExpert;

{$R *.dfm}

type
  TChangeCaseExpert = class(TSelectionEditorExpert)
  private
    FLastSelection: Integer;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
    function GetNoSelectionMode: TNoSelectionMode; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

{ TChangeCaseExpert }

constructor TChangeCaseExpert.Create;
begin
  inherited;
  ShortCut := scShift + scAlt + Ord('C');
  FLastSelection := 0;
end;

function TChangeCaseExpert.GetDisplayName: string;
resourcestring
  SChangeCaseName = 'Change Case';
begin
  Result := SChangeCaseName;
end;

procedure TChangeCaseExpert.GetHelpString(List: TStrings);
resourcestring
  SChangeCaseHelp = '  This expert changes the character case for the selected block of ' +
    'text in the editor.  To use it, select any number of lines in the IDE code editor and ' +
    'activate this expert.';
begin
  List.Text := SChangeCaseHelp;
end;

class function TChangeCaseExpert.GetName: string;
begin
  Result := 'ChangeCase';
end;

function TChangeCaseExpert.GetNoSelectionMode: TNoSelectionMode;
begin
  Result := nsmSelectIdent;
end;

function TChangeCaseExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TChangeCaseExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  Index: Integer;
  Dialog: TfmChangeCase;
begin
  Result := False;
  if Lines.Count > 0 then
  begin
    Dialog := TfmChangeCase.Create(nil);
    try
      if (FLastSelection > -1) and (FLastSelection < Dialog.grpCaseSelection.Items.Count) then
        Dialog.grpCaseSelection.ItemIndex := FLastSelection;
      if Dialog.ShowModal = mrOk then
      begin
        Index := Dialog.grpCaseSelection.ItemIndex;

        case Index of
          0: Lines.Text := AnsiLowerCase(Lines.Text);
          1: Lines.Text := AnsiUpperCase(Lines.Text);
          2: Lines.Text := TitleCase(Lines.Text);
          3: Lines.Text := SentenceCase(Lines.Text);
          4: Lines.Text := ToggleCase(Lines.Text);
        else
          Exit;
        end;
        FLastSelection := Index;
        Result := True;
      end;
    finally
      FreeAndNil(Dialog);
    end;
  end;
end;

initialization
  RegisterEditorExpert(TChangeCaseExpert);

end.

