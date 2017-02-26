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

{$UNDEF GX_HIDE_NAVBAR_NECESSARY}
{$IFDEF GX_VER300_up} // RAD Studio 10 Seattle (24; BDS 17)
// The navigation toolbar exists only in Delphi 10 and later
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
// In 10.1 it can be turned off in Tools->Options->Editor Options->Display
{$DEFINE GX_HIDE_NAVBAR_NECESSARY}
{$ENDIF}
{$ENDIF}

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
  ExtCtrls,
  GX_OtaUtils,
  GX_DbugIntf,
  GX_NTAEditServiceNotifier;

{$IFDEF GX_HIDE_NAVBAR_NECESSARY}
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
{$ENDIF GX_HIDE_NAVBAR_NECESSARY}

type
  THideNavigationToolbarExpert = class(TInterfacedObject, IHideNavigationToolbarExpert)
{$IFDEF GX_HIDE_NAVBAR_NECESSARY}
  private
    FNotifierIdx: Integer;
    FIsNavbarVisible: Boolean;
    function TryFindComponentByName(const ParentComponent: TComponent; const _Name: string;
      out _Comp: TComponent): Boolean;
    function TrySetNavbarVisible(_EditView: IOTAEditView): Boolean;
    procedure EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
{$ENDIF GX_HIDE_NAVBAR_NECESSARY}
  private
    procedure SetVisible(_Value: Boolean);
{$IFDEF GX_HIDE_NAVBAR_NECESSARY}
  public
    constructor Create;
    destructor Destroy; override;
{$ENDIF GX_HIDE_NAVBAR_NECESSARY}
  end;

function CreateHideNavigationToolbarExpert: IHideNavigationToolbarExpert;
begin
  Result := THideNavigationToolbarExpert.Create;
end;

{ THideNavigationToolbarExpert }

procedure THideNavigationToolbarExpert.SetVisible(_Value: Boolean);
{$IFDEF GX_HIDE_NAVBAR_NECESSARY}
var
  EditView: IOTAEditView;
{$ENDIF GX_HIDE_NAVBAR_NECESSARY}
begin
{$IFDEF GX_HIDE_NAVBAR_NECESSARY}
  if not Assigned(BorlandIDEServices) then
    Exit;

  FIsNavbarVisible := _Value;
  if GxOtaTryGetTopMostEditView(EditView) then begin
    TrySetNavbarVisible(EditView)
  end;
{$ENDIF GX_HIDE_NAVBAR_NECESSARY}
end;

{$IFDEF GX_HIDE_NAVBAR_NECESSARY}

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
{$IFOPT D+}SendDebug('THideNavigationToolbarExpert.Destroy');{$ENDIF}
  if FNotifierIdx <> 0 then begin
    {$IFOPT D+}SendDebugFmt('FNotifierIdx: %d', [FNotifierIdx]);{$ENDIF}
    if Assigned(BorlandIDEServices) then begin
      {$IFOPT D+}SendDebug('BorlandIDEServices is assigned');{$ENDIF}
      {$IFOPT D+}SendDebug('Calling RemoveNotifyer');{$ENDIF}
      (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);
      {$IFOPT D+}SendDebug('Returned from RemoveNotifyer');{$ENDIF}
    end else begin
      {$IFOPT D+}SendDebug('BorlandIDEServices is NOT assigned');{$ENDIF}
    end;
  end;

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
  cmp: TComponent;
begin
  Result := False;
  if not Assigned(ParentComponent) then
    Exit;

  for i := 0 to ParentComponent.ComponentCount - 1 do begin
    cmp := ParentComponent.Components[i];

    if SameText(cmp.Name, _Name) then begin
      _Comp := cmp;
      Result := True;
      Exit;
    end else
      Result := TryFindComponentByName(cmp, _Name, _Comp); // Recursion!
  end;
end;

function THideNavigationToolbarExpert.TrySetNavbarVisible(_EditView: IOTAEditView): Boolean;
const
  GX_HideNavbarPanel = 'GX_HideNavbarPanel';
var
  C: TComponent;
  EditWindow: INTAEditWindow;
  Ctrl: TWinControl;
  ParentCtrl: TWinControl;
  pnl: TPanel;
begin
  Result := False;
  if not Assigned(_EditView) then
    Exit;

  EditWindow := _EditView.GetEditWindow;
  if not Assigned(EditWindow) then
    Exit;

  Ctrl := EditWindow.Form;
  if not Assigned(Ctrl) then
    Exit;

  if TryFindComponentByName(Ctrl, 'TEditorNavigationToolbar', C) then begin
    Ctrl := TWinControl(C);
    ParentCtrl := Ctrl.Parent;
    if Assigned(ParentCtrl) and (ParentCtrl is TPanel) and (ParentCtrl.Name = GX_HideNavbarPanel) then
      pnl := TPanel(ParentCtrl)
    else begin
      pnl := TPanel.Create(ParentCtrl);
      pnl.Parent := ParentCtrl;
      pnl.Name := GX_HideNavbarPanel;
      pnl.Align := alTop;
      pnl.BevelOuter := bvNone;
      pnl.Height := ctrl.Height;
      Ctrl.Parent := pnl;
    end;
    pnl.Visible := FIsNavbarVisible;
    pnl.Enabled := FIsNavbarVisible;
    if FIsNavbarVisible then
      Ctrl.Visible := True;
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

{$ENDIF GX_HIDE_NAVBAR_NECESSARY}

end.

