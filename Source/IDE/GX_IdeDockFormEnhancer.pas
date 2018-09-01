unit GX_IdeDockFormEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxIdeDockFormEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Forms,
  StrUtils,
  GX_GenericUtils,
  GX_IdeDialogEnhancer;

type
  TIdeDockFormEnhancer = class(TIdeDialogEnhancer)
  private
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
  end;

var
  TheJoinDockFormEnhancer: TIdeDockFormEnhancer = nil;

{ TGxIdeDockFormEnhancer }

class function TGxIdeDockFormEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheJoinDockFormEnhancer);
end;

class procedure TGxIdeDockFormEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheJoinDockFormEnhancer) then
      TheJoinDockFormEnhancer := TIdeDockFormEnhancer.Create
  end else
    FreeAndNil(TheJoinDockFormEnhancer);
end;

{ TIdeDockFormEnhancer }

function TIdeDockFormEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
const
  DIALOG_CLASS1 = 'TJoinDockForm';
  DIALOG_NAME1 = 'JoinDockForm';
  DIALOG_CLASS2 = 'TTabDockHostForm';
  DIALOG_NAME2 = 'TabDockHostForm';
  DIALOG_CLASS3 = 'TTabDockHostForm';
  DIALOG_NAME3 = 'DockSite';

begin
  Result := (_Form.ClassName = DIALOG_CLASS1) and StartsText(DIALOG_NAME1, _Form.Name)
    or (_Form.ClassName = DIALOG_CLASS2) and StartsText(DIALOG_NAME2, _Form.Name)
    or (_Form.ClassName = DIALOG_CLASS3) and StartsText(DIALOG_NAME3, _Form.Name);
end;

procedure TIdeDockFormEnhancer.EnhanceForm(_Form: TForm);
begin
  _Form.BorderStyle := bsSizeable;
  _Form.BorderIcons := _form.BorderIcons + [biMinimize];
end;

end.

