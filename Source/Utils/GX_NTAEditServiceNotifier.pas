unit GX_NTAEditServiceNotifier;

interface

uses
  ToolsAPI, System.Classes, DockForm;

type
  ///<summary>
  /// We implement INTAEditServicesNotifier only to get a notification when the EditViewActivated
  /// method is called. This in turn calls the OnEditorViewActivated event. </summary>
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

implementation

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

end.
