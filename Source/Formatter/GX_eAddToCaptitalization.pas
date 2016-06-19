unit GX_eAddToCaptitalization;

interface

uses
  SysUtils,
  Classes,
  GX_EditorExpert;

type
  TAddToCapitalizationExpert = class(TEditorExpert)
  public
    function GetHelpString: string; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
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
    + 'of the code formatter.';
begin
  Result := SAddToCapitalizationHelp;
end;

procedure TAddToCapitalizationExpert.Execute(Sender: TObject);
var
  Identifier: string;
  Position: TOTAEditPos;
begin
  if not assigned(gblCodeFormatter) then
    exit;
  if not GxOtaGetCurrentIdentData(Identifier, Position) then
    exit;
  gblCodeFormatter.AddToCapitalization(Identifier);
end;

initialization
  RegisterEditorExpert(TAddToCapitalizationExpert);
end.
