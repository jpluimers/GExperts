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

{$ifdef GX_VER300_up}
// The navigation toolbar exists only in Delphi 10 (for now)

type
  THideNavigationToolbar = class(TNotifierObject, INTAEditServicesNotifier)
  private
    function FindComponentByName(const ParentComponent: TComponent; const aName: string): TComponent;
  public
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;
{$endif GX_VER300_up}

function RegisterHideNavbarWizard: integer;
procedure UnregisterHideNavbarWizard(_Index: integer);

implementation

uses
  SysUtils,
  Controls;

{$ifdef GX_VER300_up}

{ THideNavigationToolbar }

function THideNavigationToolbar.FindComponentByName(const ParentComponent: TComponent; const aName: string): TComponent;
var
  i: Integer;
  aComponent: TComponent;
begin
  Result := nil;
  if not Assigned(ParentComponent) then
    Exit;

  for i := 0 to ParentComponent.ComponentCount - 1 do begin
    aComponent := ParentComponent.Components[i];

    if SameText(aComponent.Name, aName) then begin
      Result := aComponent; // Found it!
      Break;
    end else
      Result := FindComponentByName(aComponent, aName); // Rekursion!
  end;
end { FindComponentByName };

procedure THideNavigationToolbar.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure THideNavigationToolbar.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure THideNavigationToolbar.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
//
end;

procedure THideNavigationToolbar.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
var
  C: TComponent;
  aEditWindow: INTAEditWindow;
  aControl: TWinControl;

begin
  aEditWindow := nil;
  if Assigned(EditView) then begin
    aEditWindow := EditView.GetEditWindow;
  end;

  if Assigned(aEditWindow) then begin
    aControl := aEditWindow.Form;
    if Assigned(aControl) then begin
      C := FindComponentByName(aControl, 'TEditorNavigationToolbar');

      if (C is TWinControl) and (TWinControl(C).Visible) then begin
        TWinControl(C).Visible := False;
        TWinControl(C).Enabled := False;
      end;
    end;
  end; // if Assigned(aEditWindow)
end { EditorViewActivated };

procedure THideNavigationToolbar.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
//
end;

procedure THideNavigationToolbar.WindowActivated(const EditWindow: INTAEditWindow);
begin
//
end;

procedure THideNavigationToolbar.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
//
end;

procedure THideNavigationToolbar.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
//
end;

procedure THideNavigationToolbar.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
//
end;
{$endif GX_VER300_up}

function RegisterHideNavbarWizard: integer;
begin
{$ifdef GX_VER300_up}
  Result := (BorlandIDEServices as IOTAEditorServices).AddNotifier(THideNavigationToolbar.Create)
{$else}
  Result := 0;
{$endif GX_VER300_up}
end;

procedure UnregisterHideNavbarWizard(_Index: integer);
begin
{$ifdef GX_VER300_up}
  if _Index > 0 then
    (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(_Index);
{$endif GX_VER300_up}
end;

end.

