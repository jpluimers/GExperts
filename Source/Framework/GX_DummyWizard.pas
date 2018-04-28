unit GX_DummyWizard;

interface

uses
  ToolsAPI;

type
  TDummyWizard = class(TNotifierObject, IOTAWizard)
  protected
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  GX_About;

{ TDummyWizard }

function TDummyWizard.GetIDString: string;
begin
  Result := 'GExperts.DummyWizard'; // Do not localize.
end;

function TDummyWizard.GetName: string;
begin
  Result := GetIDString;
end;

function TDummyWizard.GetState: TWizardState;
begin
  Result := [];
end;

constructor TDummyWizard.Create;
begin
  inherited Create;
  gblAboutFormClass.AddToAboutDialog;
end;

destructor TDummyWizard.Destroy;
begin
  gblAboutFormClass.RemoveFromAboutDialog;
  inherited;
end;

procedure TDummyWizard.Execute;
begin
  // do nothing
end;

end.
