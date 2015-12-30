unit GX_OTANotifier;

interface

uses
  ToolsAPI;

type
  ///<summary>
  /// Implements the IOTANotifier interface without actually declaring that it does so.
  /// All methods are empty, descendants can simply override those methods that they are
  /// interested in. </summary>
  TGxOtaNotifier = class(TInterfacedObject)
  protected
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
  end;

implementation

{ TGxOtaNotifier }

procedure TGxOtaNotifier.AfterSave;
begin
  // do nothing
end;

procedure TGxOtaNotifier.BeforeSave;
begin
  // do nothing
end;

procedure TGxOtaNotifier.Destroyed;
begin
  // do nothing
end;

procedure TGxOtaNotifier.Modified;
begin
  // do nothing
end;

end.
