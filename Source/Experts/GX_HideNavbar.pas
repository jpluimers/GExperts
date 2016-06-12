unit GX_HideNavbar;

{
================================================================================
IDE Expert to hide/disable the Editor Navigation Toolbar (Castalia)
from the Delphi 10 Seattle IDE.
Contributed by: Achim Kalwa <delphi(at)achim-kalwa.de>
Integrated into GExperts by Thomas Mueller (http://blog.dummzeuch.de)
================================================================================
}

{$I GX_CondDefine.inc}

interface

uses
  ToolsAPI,
  Classes,
  DockForm;

type
  IHideNavigationToolbarExpert = interface ['{BC189A61-9313-4ABE-8AB3-2B80B3709DF5}']
    procedure SetVisible(_Value: Boolean);
  end;

function CreateHideNavigationToolbarExpert: IHideNavigationToolbarExpert;

implementation

uses
  SysUtils,
  Controls,
  GX_OtaUtils,
  GX_NTAEditServiceNotifier;

{$IFDEF GX_VER300_up}
// The navigation toolbar exists only in Delphi 10 (for now)
type
  ///<summary>
  /// We implement INTAEditServicesNotifier only to get a notification when the EditViewActivated
  /// method is called. This in turn calls the OnEditorViewActivated event. </summary>
  TEditServiceNotifier = class(TGxNTAEditServiceNotifier, INTAEditServicesNotifier)
  private
    type
      TOnEditorViewActivatedEvent = procedure(_Sender: TObject; _EditView: IOTAEditView) of object;
    var
      FOnEditorViewActivated: TOnEditorViewActivatedEvent;
  protected // INTAEditServicesNotifier
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); override;
  public
    constructor Create(_OnEditorViewActivated: TOnEditorViewActivatedEvent);
  end;
{$ENDIF GX_VER300_up}

type
  THideNavigationToolbarExpert = class(TInterfacedObject, IHideNavigationToolbarExpert)
{$IFDEF GX_VER300_up}
  private
    FNotifierIdx: Integer;
    FIsNavbarVisible: Boolean;
    function TryFindComponentByName(const ParentComponent: TComponent; const _Name: string;
      out _Comp: TComponent): Boolean;
    function TrySetNavbarVisible(_EditView: IOTAEditView): Boolean;
    procedure EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
{$ENDIF GX_VER300_up}
  private
    procedure SetVisible(_Value: Boolean);
{$IFDEF GX_VER300_up}
  public
    constructor Create;
    destructor Destroy; override;
{$ENDIF GX_VER300_up}
  end;

function CreateHideNavigationToolbarExpert: IHideNavigationToolbarExpert;
begin
  Result := THideNavigationToolbarExpert.Create;
end;

{ THideNavigationToolbarExpert }

procedure THideNavigationToolbarExpert.SetVisible(_Value: Boolean);
{$IFDEF GX_VER300_up}
var
  EditView: IOTAEditView;
{$ENDIF GX_VER300_up}
begin
{$IFDEF GX_VER300_up}
  if not Assigned(BorlandIDEServices) then
    exit;

  FIsNavbarVisible := _Value;
  if GxOtaTryGetTopMostEditView(EditView) then begin
    TrySetNavbarVisible(EditView)
  end;
{$ENDIF GX_VER300_up}
end;

{$IFDEF GX_VER300_up}

constructor THideNavigationToolbarExpert.Create;
begin
  inherited;
  if Assigned(BorlandIDEServices) then begin
    FNotifierIdx := (BorlandIDEServices as IOTAEditorServices).AddNotifier(
      TEditServiceNotifier.Create(EditorViewActivated));
  end;
end;

destructor THideNavigationToolbarExpert.Destroy;
begin
  if FNotifierIdx <> 0 then
    (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);

  inherited Destroy;
end;

procedure THideNavigationToolbarExpert.EditorViewActivated(_Sender: TObject;
  _EditView: IOTAEditView);
begin
  TrySetNavbarVisible(_EditView)
end;

function THideNavigationToolbarExpert.TryFindComponentByName(const ParentComponent: TComponent;
  const _Name: string; out _Comp: TComponent): Boolean;
var
  i: Integer;
  Cmp: TComponent;
begin
  Result := False;
  if not Assigned(ParentComponent) then
    Exit;

  for i := 0 to ParentComponent.ComponentCount - 1 do begin
    Cmp := ParentComponent.Components[i];

    if SameText(cmp.Name, _Name) then begin
      _Comp := Cmp;
      Result := True;
      Exit;
    end else
      Result := TryFindComponentByName(Cmp, _Name, _Comp); // Recursion!
  end;
end;

function THideNavigationToolbarExpert.TrySetNavbarVisible(_EditView: IOTAEditView): Boolean;
var
  C: TComponent;
  EditWindow: INTAEditWindow;
  Ctrl: TWinControl;
begin
  Result := False;
  if not Assigned(_EditView) then
    Exit;

  EditWindow := _EditView.GetEditWindow;
  if not Assigned(EditWindow) then
    Exit;

  ctrl := EditWindow.Form;
  if not Assigned(Ctrl) then
    Exit;

  if TryFindComponentByName(Ctrl, 'TEditorNavigationToolbar', c) then begin
    TWinControl(C).Visible := FIsNavbarVisible;
    TWinControl(C).Enabled := FIsNavbarVisible;
    Result := True;
  end;
end;

{ TEditServiceNotifier }

constructor TEditServiceNotifier.Create(_OnEditorViewActivated: TOnEditorViewActivatedEvent);
begin
  FOnEditorViewActivated := _OnEditorViewActivated;
  inherited Create;
end;

procedure TEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  if Assigned(FOnEditorViewActivated) then
    FOnEditorViewActivated(Self, EditView);
end;

{$ENDIF GX_VER300_up}

end.

