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
  end;

implementation

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

procedure TDummyWizard.Execute;
begin
  // do nothing
end;

end.
