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
    procedure SetVisible(_Value: boolean);
  end;

function CreateHideNavigationToolbarExpert: IHideNavigationToolbarExpert;

implementation

uses
  SysUtils,
  Controls,
  GX_OtaUtils;

{$IFDEF GX_VER300_up}
// The navigation toolbar exists only in Delphi 10 (for now)
type

  ///<summary>
  /// We implement INTAEditServicesNotifier only to get a notification when the EditViewActivated
  /// method is called. This in turn calls the OnEditorViewActivated event. </summary>
  TEditServiceNotifier = class(TNotifierObject, INTAEditServicesNotifier)
  private
    type
      TOnEditorViewActivatedEvent = procedure(_Sender: TObject; _EditView: IOTAEditView) of object;
    var
      FOnEditorViewActivated: TOnEditorViewActivatedEvent;
  private // INTAEditServicesNotifier
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  public
    constructor Create(_OnEditorViewActivated: TOnEditorViewActivatedEvent);
  end;
{$ENDIF GX_VER300_up}

type
  THideNavigationToolbarExpert = class(TInterfacedObject, IHideNavigationToolbarExpert)
{$IFDEF GX_VER300_up}
  private
    FNotifierIdx: integer;
    FIsNavbarVisible: boolean;
    function TryFindComponentByName(const ParentComponent: TComponent; const _Name: string;
      out _Comp: TComponent): boolean;
    function TrySetNavbarVisible(_EditView: IOTAEditView): boolean;
    procedure EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
{$ENDIF GX_VER300_up}
  private
    procedure SetVisible(_Value: boolean);
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

procedure THideNavigationToolbarExpert.SetVisible(_Value: boolean);
{$IFDEF GX_VER300_up}
var
  EditView: IOTAEditView;
{$ENDIF GX_VER300_up}
begin
{$IFDEF GX_VER300_up}
  FIsNavbarVisible := _Value;
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then begin
    TrySetNavbarVisible(EditView)
  end;
{$ENDIF GX_VER300_up}
end;

{$IFDEF GX_VER300_up}

constructor THideNavigationToolbarExpert.Create;
begin
  inherited;
  FNotifierIdx := (BorlandIDEServices as IOTAEditorServices).AddNotifier(
    TEditServiceNotifier.Create(EditorViewActivated));
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
  const _Name: string; out _Comp: TComponent): boolean;
var
  i: Integer;
  cmp: TComponent;
begin
  Result := false;
  if not Assigned(ParentComponent) then
    Exit;

  for i := 0 to ParentComponent.ComponentCount - 1 do begin
    cmp := ParentComponent.Components[i];

    if SameText(cmp.Name, _Name) then begin
      _Comp := cmp;
      Result := true;
      exit;
    end else
      Result := TryFindComponentByName(cmp, _Name, _Comp); // Recursion!
  end;
end;

function THideNavigationToolbarExpert.TrySetNavbarVisible(_EditView: IOTAEditView): boolean;
var
  C: TComponent;
  EditWindow: INTAEditWindow;
  ctrl: TWinControl;
begin
  Result := False;
  if not Assigned(_EditView) then
    exit;

  EditWindow := _EditView.GetEditWindow;
  if not Assigned(EditWindow) then
    exit;

  ctrl := EditWindow.Form;
  if not Assigned(ctrl) then
    exit;

  if TryFindComponentByName(ctrl, 'TEditorNavigationToolbar', c) then begin
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

procedure TEditServiceNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure TEditServiceNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure TEditServiceNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure TEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  if Assigned(FOnEditorViewActivated) then
    FOnEditorViewActivated(Self, EditView);
end;

procedure TEditServiceNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
//
end;

procedure TEditServiceNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
//
end;

procedure TEditServiceNotifier.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
//
end;

procedure TEditServiceNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
//
end;

procedure TEditServiceNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
//
end;

{$ENDIF GX_VER300_up}

end.

