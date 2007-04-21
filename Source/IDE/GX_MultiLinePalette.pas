unit GX_MultiLinePalette;

{$I GX_CondDefine.inc}

interface

uses
  Classes;

type
  TMultiLineTabManager = class(TComponent)
  private
    FOldCPResizeHandler: TNotifyEvent;
    procedure CPResizeHandler(Sender: TObject);
  private
    FTabRaggedRight: Boolean;
    FTabScrollOpposite: Boolean;
    procedure SetTabRaggedRight(const Value: Boolean);
    procedure SetTabScrollOpposite(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TabRaggedRight: Boolean read FTabRaggedRight write SetTabRaggedRight;
    property TabScrollOpposite: Boolean read FTabScrollOpposite write SetTabScrollOpposite;
  end;


implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Controls, ComCtrls,
  GX_GenericUtils, GX_IdeUtils;

procedure TMultiLineTabManager.CPResizeHandler(Sender: TObject);
var
  AHeight: Integer;
  SenderControl: TTabControl;
begin
  {$IFOPT D+} SendDebug('Component palette resized: adjusting multiline settings'); {$ENDIF}
  SenderControl := Sender as TTabControl;

  AHeight := SenderControl.Height -
             (SenderControl.DisplayRect.Bottom - SenderControl.DisplayRect.Top) + 29;

  SenderControl.Constraints.MinHeight := AHeight;
  SenderControl.Parent.Constraints.MaxHeight := AHeight;
end;

procedure TMultiLineTabManager.SetTabRaggedRight(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if Value = FTabRaggedRight then
    Exit;

  FTabRaggedRight := Value;

  TabControl := GetComponentPaletteTabControl;
  if TabControl = nil then
    Exit;

  TabControl.RaggedRight := FTabRaggedRight;
end;

procedure TMultiLineTabManager.SetTabScrollOpposite(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if Value = FTabScrollOpposite then
    Exit;

  FTabScrollOpposite := Value;

  TabControl := GetComponentPaletteTabControl;
  if TabControl = nil then
    Exit;

  if FTabScrollOpposite then
  TabControl.ScrollOpposite := FTabScrollOpposite;
end;

constructor TMultiLineTabManager.Create(AOwner: TComponent);
var
  Tab: TTabControl;
begin
  inherited Create(AOwner);

  Tab := GetComponentPaletteTabControl;

  if (AOwner = nil) or (Tab = nil) then
    Exit;

  FOldCPResizeHandler := Tab.OnResize;
  Tab.OnResize := CPResizeHandler;

  Tab.MultiLine := True;
end;

destructor TMultiLineTabManager.Destroy;

  procedure RemoveIDETabHook;
  var
    TabControl: TTabControl;
  begin
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    with TabControl do
    begin
      MultiLine := False;
      OnResize := FOldCPResizeHandler;
      {$IFOPT D+} SendDebug('Restored old component palette resize handler'); {$ENDIF}
    end;
  end;

begin
  try
    {$IFOPT D+} SendDebug('Removing IDE component palette hooks'); {$ENDIF}
    RemoveIDETabHook;
  except
    on E: Exception do
    begin
      GxLogException(E, 'MultiLine component palette removal');
      {$IFOPT D+} SendDebug('MultiLine component palette removal exception: ' + E.Message) {$ENDIF};
      // Swallow exception
    end;
  end;
  inherited Destroy;
  {$IFOPT D+} SendDebug('Removed IDE component palette hooks'); {$ENDIF}
end;

end.
