unit GX_eAddToCaptitalization;

interface

uses
  SysUtils,
  Classes,
  GX_EditorExpert;

type
  TAddToCapitalizationExpert = class(TEditorExpert)
  public
    // Internal name of expert for expert identification.
    class function GetName: string; override;
    function GetHelpString: string; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

uses
  ToolsAPI,
  GX_OtaUtils,
  GX_eCodeFormatter;

{ TAddToCapitalization }

function TAddToCapitalizationExpert.GetDisplayName: string;
resourcestring
  SAddToCapitalizationName = 'Add To Formatter Capitalization';
begin
  Result := SAddToCapitalizationName;
end;

function TAddToCapitalizationExpert.GetHelpString: string;
resourcestring
  SAddToCapitalizationHelp = '  This expert adds the current word to the capitalization list '
    + 'of the code formatter. It will overwrite existing entries.';
begin
  Result := SAddToCapitalizationHelp;
end;

class function TAddToCapitalizationExpert.GetName: string;
begin
  Result := 'AddToCapitalization';
end;

function TAddToCapitalizationExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TAddToCapitalizationExpert.Execute(Sender: TObject);
var
  Identifier: string;
  Position: TOTAEditPos;
begin
  if not assigned(gblCodeFormatter) then
    Exit;
  if not GxOtaGetCurrentIdentData(Identifier, Position) then
    Exit;
  gblCodeFormatter.AddToCapitalization(Identifier);
end;

initialization
  RegisterEditorExpert(TAddToCapitalizationExpert);
end.
