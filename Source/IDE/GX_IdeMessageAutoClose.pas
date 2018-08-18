unit GX_IdeMessageAutoClose;
{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxMessageAutoClose = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Windows,
  Messages,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  Graphics,
  GX_OtaUtils,
  GX_dzClassUtils,
  GX_IdeFormEnhancer,
  GX_IdeDialogEnhancer,
  GX_GExperts;

type
  TWinControlHack = class(TWinControl)
  end;

type
  TMessageAutoClose = class(TIdeDialogEnhancer)
  private
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
  end;

var
  TheMessageAutoClose: TMessageAutoClose = nil;

type
  TMessageAutoCloseComponent = class(TComponent)
  private
    FHintCount: TLabel;
    FWarningCount: TLabel;
    FErrorCount: TLabel;
  public
    constructor Create(_Owner: TComponent; _HintCount, _WarningCount, _ErrorCount: TLabel); reintroduce;
    procedure BeforeDestruction; override;
  end;

{ TMessageAutoCloseComponent }

constructor TMessageAutoCloseComponent.Create(_Owner: TComponent;
  _HintCount, _WarningCount, _ErrorCount: TLabel);
begin
  inherited Create(_Owner);
  FHintCount := _HintCount;
  FWarningCount := _WarningCount;
  FErrorCount := _ErrorCount;
end;

procedure TMessageAutoCloseComponent.BeforeDestruction;
begin
  inherited;
  if FErrorCount.Caption <> '0' then
    Exit; //==>
  if FWarningCount.Caption <> '0' then
    Exit; //==>
  if FHintCount.Caption <> '0' then
    Exit; //==>
  GExpertsInst.TimedCloseMessageView;
end;

{ TGxMessageAutoClose }

class function TGxMessageAutoClose.GetEnabled: Boolean;
begin
  Result := Assigned(TheMessageAutoClose);
end;

class procedure TGxMessageAutoClose.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheMessageAutoClose) then
      TheMessageAutoClose := TMessageAutoClose.Create
  end else
    FreeAndNil(TheMessageAutoClose);
end;

{ TMessageAutoClose }

function TMessageAutoClose.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  DIALOG_CLASS = 'TProgressForm';
  DIALOG_NAME = 'ProgressForm';
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

procedure TMessageAutoClose.EnhanceForm(_Form: TForm);
const
  GxMessageAutoCloseComponent = 'GxMessageAutoCloseComponent';
var
  cmp: TComponent;
  HintCount: TLabel;
  WarningCount: TLabel;
  ErrorCount: TLabel;
begin
  if TComponent_FindComponent(_Form, GxMessageAutoCloseComponent, True, cmp) then
    Exit;

  if not TComponent_FindComponent(_Form, 'HintCount', False, TComponent(HintCount), TLabel) then
    Exit;
  if not TComponent_FindComponent(_Form, 'WarningCount', False, TComponent(WarningCount), TLabel) then
    Exit;
  if not TComponent_FindComponent(_Form, 'ErrorCount', False, TComponent(ErrorCount), TLabel) then
    Exit;

  cmp := TMessageAutoCloseComponent.Create(_Form, HintCount, WarningCount, ErrorCount);
  cmp.Name := GxMessageAutoCloseComponent;
end;

end.
