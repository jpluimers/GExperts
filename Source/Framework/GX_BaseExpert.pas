unit GX_BaseExpert;

interface

type
  TGX_BaseExpert = class(TObject)
  public
    // Internal name of expert for expert identification.
    class function GetName: string; virtual;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this is a different entry than
    // the action caption (GetActionCaption)
    function GetDisplayName: string; virtual;
    procedure Execute(Sender: TObject); virtual; abstract;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils;

{ TGX_BaseExpert }

function TGX_BaseExpert.GetDisplayName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a human-readable name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

class function TGX_BaseExpert.GetName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a Name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

end.
