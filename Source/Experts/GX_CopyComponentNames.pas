unit GX_CopyComponentNames;

interface

uses GX_Experts, ActnList;

type
  TCopyComponentNamesExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

implementation

uses
  SysUtils, Classes, Clipbrd, ToolsAPI, GX_OtaUtils;

{ TCopyComponentNamesExpert }

procedure TCopyComponentNamesExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaFormEditorHasSelectedComponent;
end;

procedure TCopyComponentNamesExpert.Click(Sender: TObject);
var
  CurrentModule: IOTAModule;
  FormEditor: IOTAFormEditor;
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  Names: TStringList;
  ComponentName: WideString;
  i: Integer;
begin
  CurrentModule := GxOtaGetCurrentModule;
  FormEditor := nil;
  if Assigned(CurrentModule) then
    FormEditor := GxOtaGetFormEditorFromModule(CurrentModule);
  if not Assigned(FormEditor) then
    Exit;
  SelCount := FormEditor.GetSelCount;
  Names := TStringList.Create;  
  try
    for i := 0 to SelCount - 1 do
    begin
      CurrentComponent := FormEditor.GetSelComponent(i);
      ComponentName := GxOtaGetComponentName(CurrentComponent);
      if ComponentName <> '' then
        Names.Add(ComponentName);
    end;
    Clipboard.AsText := Trim(Names.Text);
  finally
    FreeAndNil(Names);
  end
end;

constructor TCopyComponentNamesExpert.Create;
begin
  inherited Create;
  //ShortCut := scCtrl + scShift + Ord('N');
end;

function TCopyComponentNamesExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Copy Component &Names';
begin
  Result := SMenuCaption;
end;

class function TCopyComponentNamesExpert.GetName: string;
begin
  Result := 'CopyComponentNames';
end;

function TCopyComponentNamesExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TCopyComponentNamesExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

initialization
  RegisterGX_Expert(TCopyComponentNamesExpert);
  
end.
