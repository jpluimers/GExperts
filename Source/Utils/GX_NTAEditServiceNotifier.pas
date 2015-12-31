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
  TGxNTAEditServiceNotifier = class(TNotifierObject)
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
{$IFOPT D+}
  protected // IOTANotifier
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
{$ENDIF}
  end;
{$ENDIF}

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows;


{$IFDEF GX_VER160_up}

{ TGxNTAEditServiceNotifier }

{$IFOPT D+}
procedure TGxNTAEditServiceNotifier.AfterSave;
begin
  SendDebug('AfterSave');
end;

procedure TGxNTAEditServiceNotifier.BeforeSave;
begin
  SendDebug('BeforeSave');
end;

procedure TGxNTAEditServiceNotifier.Destroyed;
begin
  SendDebug('Destroyed');
end;

procedure TGxNTAEditServiceNotifier.Modified;
begin
  SendDebug('Modified');
end;
{$ENDIF}


procedure TGxNTAEditServiceNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
{$IFOPT D+}
  SendDebugFmt('DockFormRefresh(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
{$IFOPT D+}
  SendDebugFmt('DockFormUpdated(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
{$IFOPT D+}
  SendDebugFmt('DockFormVisibleChanged(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
{$IFOPT D+}
  SendDebugFmt('EditorViewActivated(%s)', [EditWindow.Form.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
{$IFOPT D+}
  SendDebugFmt('EditorViewModified(%s)', [EditWindow.Form.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
{$IFOPT D+}
  SendDebugFmt('WindowActivated(%s)', [EditWindow.Form.Caption]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
{$IFOPT D+}
  SendDebugFmt('WindowActivated(%s, %d, %d)', [EditWindow.Form.Caption, Command, Param]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
{$IFOPT D+}
  SendDebugFmt('WindowNotification(%s, %d)', [EditWindow.Form.Caption, Ord(Operation)]);
{$ENDIF}
end;

procedure TGxNTAEditServiceNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
{$IFOPT D+}
  SendDebugFmt('WindowShow(%s)', [EditWindow.Form.Caption]);
{$ENDIF}
end;
{$ENDIF}

end.
