unit GX_OTAEditorNotifier;

interface

uses
  Classes, ToolsAPI;

type
  IGxOtaEditorNotifier = interface(IOTAEditorNotifier) ['{D925E276-6494-4A5C-A989-2017DA55C80F}']
    procedure Attach(_Editor: IOTASourceEditor);
    procedure Detach;
  end;

  ///<summary>
  /// Implements the IOTAEditorNotifier interface without actually declaring that it does so.
  /// All methods are empty, descendants can simply override those methods that they are
  /// interested in. </summary>
  TGxOTAEditorNotifier = class(TNotifierObject)
  protected
    FEditor: IOTASourceEditor;
    FNotifierIndex: Integer;
  protected
    procedure ViewActivated(const View: IOTAEditView); virtual;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation); virtual;
  protected
    procedure Attach(_Editor: IOTASourceEditor);
    procedure Detach;
  end;

implementation

uses
  GX_OtaUtils;

{ TGxEditorNotifier }

procedure TGxOTAEditorNotifier.Attach(_Editor: IOTASourceEditor);
begin
  FEditor := _Editor;
  FNotifierIndex := FEditor.AddNotifier(Self as IOTAEditorNotifier);
end;

procedure TGxOTAEditorNotifier.Detach;
begin
  if Assigned(FEditor) then begin
    if FNotifierIndex <> InvalidNotifierIndex then
      FEditor.RemoveNotifier(FNotifierIndex);
    FEditor := nil;
  end;
end;

procedure TGxOTAEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  // do nothing
end;

procedure TGxOTAEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  // do nothing
end;

end.
