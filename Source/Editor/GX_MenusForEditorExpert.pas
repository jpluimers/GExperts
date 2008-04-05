unit GX_MenusForEditorExpert;

interface

uses
  Menus, ActnList, GX_Experts;

type
  TGxMenusForEditorExperts = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasSubmenuItems: Boolean; override;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); override;
    procedure Click(Sender: TObject); override;
  end;

implementation

uses
  SysUtils, Windows, Controls,
  GX_GExperts, GX_OtaUtils, GX_ActionBroker,
  GX_EditorExpert, GX_EditorExpertManager;

var
  InternalPopupMenu: TPopupMenu;

function GetInternalPopupMenu: TPopupMenu;
const
  MenuName = 'GxMenusForEditorExpertsInternalPopupMenu';
begin
  if not Assigned(InternalPopupMenu) then
  begin
    InternalPopupMenu := NewPopupMenu(nil, MenuName, paCenter, False, []);
    InternalPopupMenu.Images := GxOtaGetIdeImageList;
  end;

  Result := InternalPopupMenu;
end;

procedure ReleaseInternalPopupMenu;
begin
  FreeAndNil(InternalPopupMenu);
end;

{ TGxMenusForEditorExperts }

procedure TGxMenusForEditorExperts.Click(Sender: TObject);
begin
  // The submenu items perform all actions
end;

procedure TGxMenusForEditorExperts.CreateSubMenuItems(MenuItem: TMenuItem);
var
  i: Integer;
  AGExpertsInstance: TGExperts;
  AEditorExpertManager: TGxEditorExpertManager;
  AEditorExpert: TEditorExpert;

  ExpertMenuEntry: TMenuItem;
begin
  inherited;
  Assert(Assigned(MenuItem));
  MenuItem.Clear;

  AGExpertsInstance := GExpertsInst;
  Assert(Assigned(AGExpertsInstance));

  AEditorExpertManager := AGExpertsInstance.EditorExpertManager;
  // If editor experts are not enabled, then the editor
  // expert manager is not present; exit if this is the case.
  if not Assigned(AEditorExpertManager) then
    Exit;

  for i := 0 to AEditorExpertManager.EditorExpertCount-1 do
  begin
    AEditorExpert := AEditorExpertManager.EditorExpertList[i];
    Assert(Assigned(AEditorExpert));

    ExpertMenuEntry := TMenuItem.Create(MenuItem);
    ExpertMenuEntry.Action := GxActionBroker.FindAction(AEditorExpert.GetActionName);

    MenuItem.Add(ExpertMenuEntry);
  end;
end;

function TGxMenusForEditorExperts.GetActionCaption: string;
resourcestring
  SCaption = 'Editor Experts';
begin
  Result := SCaption;
end;

function TGxMenusForEditorExperts.GetDisplayName: string;
resourcestring
  SDisplayName = 'Editor Experts';
begin
  Result := SDisplayName;
end;

class function TGxMenusForEditorExperts.GetName: string;
begin
  Result := 'EditorExpertsMenu';
end;

function TGxMenusForEditorExperts.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGxMenusForEditorExperts.HasSubmenuItems: Boolean;
begin
  Result := True;
end;

procedure TGxMenusForEditorExperts.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := Assigned(GExpertsInst.EditorExpertManager)
    and (GExpertsInst.EditorExpertManager.EditorExpertCount > 0);
  Action.Visible := Action.Enabled;
end;

initialization
  RegisterGX_Expert(TGxMenusForEditorExperts);

finalization
  ReleaseInternalPopupMenu;

end.
