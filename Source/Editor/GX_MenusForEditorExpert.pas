unit GX_MenusForEditorExpert;

interface

uses
  Menus, ActnList, GX_Experts;

type
  TGxMenusForEditorExperts = class(TGX_Expert)
  private
    procedure PopulatePopupMenu(const PopupMenu: TPopupMenu);
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    function SupportsSubmenu: Boolean;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasSubmenuItems: Boolean; override;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); override;
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  SysUtils, Windows, Controls,
  GX_GExperts, GX_OtaUtils, GX_ActionBroker, GX_IdeUtils,
  GX_EditorExpert, GX_EditorExpertManager, ComCtrls;

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

procedure TGxMenusForEditorExperts.Execute(Sender: TObject);
var
  MousePosition: TPoint;
  APopupMenu: TPopupMenu;
  IsToolButton: Boolean;
begin
  IsToolButton := Assigned(Sender) and (Sender is TCustomAction) and ((Sender as TCustomAction).ActionComponent is TToolButton);
  if SupportsSubmenu and (not IsToolButton) and Assigned(Sender) then
  begin
    // The submenu items perform all actions
  end
  else begin
    MousePosition := Mouse.CursorPos;
    APopupMenu := GetInternalPopupMenu;
    Assert(Assigned(APopupMenu));
    PopulatePopupMenu(APopupMenu);
    APopupMenu.Popup(MousePosition.x, MousePosition.y);
  end;
end;

// Note: Partially duplicated below
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

    if AEditorExpert.Active then
    begin
      ExpertMenuEntry := TMenuItem.Create(MenuItem);
      ExpertMenuEntry.Action := GxActionBroker.FindAction(AEditorExpert.GetActionName);

      MenuItem.Add(ExpertMenuEntry);
    end;
  end;
end;

procedure TGxMenusForEditorExperts.PopulatePopupMenu(const PopupMenu: TPopupMenu);

  procedure ClearMenuItems(AMenu: TMenu);
  begin
    Assert(Assigned(AMenu));
    AMenu.Items.Clear;
  end;

var
  i: Integer;
  AGExpertsInstance: TGExperts;
  AEditorExpertManager: TGxEditorExpertManager;
  AEditorExpert: TEditorExpert;

  ExpertMenuEntry: TMenuItem;
begin
  Assert(Assigned(PopupMenu));
  ClearMenuItems(PopupMenu);

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

    if AEditorExpert.Active then
    begin
      ExpertMenuEntry := TMenuItem.Create(PopupMenu);
      ExpertMenuEntry.Action := GxActionBroker.FindAction(AEditorExpert.GetActionName);

      PopupMenu.Items.Add(ExpertMenuEntry);
    end;
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
  Result := SupportsSubmenu;
end;

function TGxMenusForEditorExperts.SupportsSubmenu: Boolean;
begin
  // The Delphi 7- IDEs seem to clear out the submenu item shortcuts
  Result := RunningDelphi8OrGreater;
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
