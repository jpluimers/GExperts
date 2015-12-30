unit GX_NTAEditServiceNotifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ToolsAPI, DockForm;

{$IFDEF GX_VER160_up}

type
  ///<summary>
  /// This is a dummy implementation of INTAEditServicesNotifier that does nothing.
  /// Descendants override some methods to get the notifications they are interested in. </summary>
  TGxNTAEditServiceNotifier = class(TNotifierObject, INTAEditServicesNotifier)
  protected // INTAEditServicesNotifier
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean); virtual;
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation); virtual;
    procedure WindowActivated(const EditWindow: INTAEditWindow); virtual;
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean); virtual;
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
  end;
{$ENDIF}

implementation

{$IFDEF GX_VER160_up}

{ TGxNTAEditServiceNotifier }

procedure TGxNTAEditServiceNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
  // do nothing
end;

procedure TGxNTAEditServiceNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
  // do nothing
end;
{$ENDIF}

end.
