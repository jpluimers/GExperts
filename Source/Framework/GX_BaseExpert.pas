unit GX_BaseExpert;

interface

uses
  Classes, Graphics;

type
  TGX_BaseExpert = class(TObject)
  protected
    function GetShortCut: TShortCut; virtual; abstract;
    procedure SetShortCut(Value: TShortCut); virtual; abstract;
    function GetBitmap: Graphics.TBitmap; virtual; abstract;
  public
    // Internal name of expert for expert identification.
    class function GetName: string; virtual;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this is a different entry than
    // the action caption (GetActionCaption)
    function GetDisplayName: string; virtual;
    procedure Execute(Sender: TObject); virtual; abstract;
    function GetDefaultShortCut: TShortCut; virtual;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils;

{ TGX_BaseExpert }

function TGX_BaseExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
end;

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
