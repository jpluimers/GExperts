unit GX_eSelectIdentifier;

interface

implementation

uses Classes, GX_EditorExpert, GX_OtaUtils;

type
  TSelectIdentifierExpert = class(TEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
    procedure Execute(Sender: TObject); override;
  end;

{ TSelectIdentifierExpert }

constructor TSelectIdentifierExpert.Create;
begin
  inherited;
end;

procedure TSelectIdentifierExpert.Execute(Sender: TObject);
begin
  GxOtaSelectCurrentIdent(GxOtaGetCurrentSourceEditor);
end;

function TSelectIdentifierExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('I');
end;

function TSelectIdentifierExpert.GetDisplayName: string;
resourcestring
  SSelectIdentName = 'Select Identifier';
begin
  Result := SSelectIdentName;
end;

procedure TSelectIdentifierExpert.GetHelpString(List: TStrings);
resourcestring
  SSelectIdentHelp = '  This expert selects the identifier under the edit cursor.';
begin
  List.Text := SSelectIdentHelp;
end;

class function TSelectIdentifierExpert.GetName: string;
begin
  Result := 'SelectIdent';
end;

function TSelectIdentifierExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterEditorExpert(TSelectIdentifierExpert);

end.

