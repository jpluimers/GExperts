unit GX_IdeObjectInspectorEnhancer;

{$I GX_CondDefine.inc}

{$UNDEF GX_EnableOIEnhancer}
{$IFDEF GX_VER210_up} // RAD Studio 2010 (15; BDS 7)
{$IFNDEF GX_VER300_up} // RAD Studio 10 Seattle (24; BDS 17)
{$DEFINE GX_EnableOIEnhancer}
{$ENDIF}
{$ENDIF}

interface

uses
  ToolsAPI,
  DesignIntf,
  Classes,
  Forms;

type
  TGxIdeObjectInspectorEnhancer = class
  public
    class function GetHideHotCmds: Boolean; // static;
    class procedure SetHideHotCmds(const Value: Boolean); // static;
    class function GetHideDescriptionPane: Boolean; // static;
    class procedure SetHideDescriptionPane(const Value: Boolean); //static;
  end;

implementation

{$IFDEF GX_EnableOIEnhancer}

uses
  ExtCtrls,
  Controls,
  SysUtils;

type
  TDesignNotification = class(TInterfacedObject, IDesignNotification)
  private
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  private
    FDescriptionPaneVisible: Boolean;
    FHotCommandsVisible: Boolean;
    procedure SetItemsVisible;
    procedure SetDescriptionPaneVisible(const Value: Boolean);
    procedure SetHotCommandsVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property DescriptionPaneVisible: Boolean read FDescriptionPaneVisible write SetDescriptionPaneVisible;
    property HotCommandsVisible: Boolean read FHotCommandsVisible write SetHotCommandsVisible;
  end;

constructor TDesignNotification.Create;
begin
  inherited Create;
end;

procedure TDesignNotification.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin

end;

procedure TDesignNotification.SetItemsVisible;
const
  GXHidePanelName = 'GXHidePanel';

  function CreateHidePanel(_Form: TForm; _Parent: TWinControl): TPanel;
  begin
    Result := TPanel.Create(_Form);
    Result.Name := GXHidePanelName;
    Result.Visible := False;
    Result.Parent := _Parent;
  end;

  procedure HideControl(_Form: TForm; var _HidePanel: TPanel; _Ctrl: TControl; _Visible: Boolean);
  begin
    if Assigned(_Ctrl) then begin
      if not _Visible then begin
        if not Assigned(_HidePanel) then
          _HidePanel := CreateHidePanel(_Form, _Ctrl.Parent);
        _Ctrl.Parent := _HidePanel;
      end else begin
        if _Ctrl.Parent = _HidePanel then begin
          _Ctrl.Parent := _HidePanel.Parent;
        end;
      end;
    end;
  end;

  procedure HideItems(_Form: TForm);
  const
    HotCmdsName = 'HotCommands';
    HotSplitName = 'CommandSplitter';
    DescPaneName = 'DescriptionPane';
    DescSplitName = 'DescriptSplitter';
  var
    cmp: TComponent;
    CmpName: string;
    DescPane: TControl;
    HotCmds: TControl;
    HidePanel: TPanel;
    i: Integer;
    HotSplitter: TSplitter;
    DescSplitter: TSplitter;
  begin
    HidePanel := nil;
    HotCmds := nil;
    DescPane := nil;
    HotSplitter := nil;
    DescSplitter := nil;
    for i := 0 to _Form.ComponentCount - 1 do begin
      cmp := _Form.Components[i];
      CmpName := cmp.Name;
      if cmp is TPanel then begin
        if SameText(CmpName, GXHidePanelName) then begin
          HidePanel := TPanel(cmp);
        end;
      end else if cmp is TSplitter then begin
        if SameText(CmpName, HotSplitName) then begin
          HotSplitter := TSplitter(cmp);
        end else if SameText(CmpName, DescSplitName) then begin
          DescSplitter := TSplitter(cmp);
        end;
      end else if cmp is TWinControl then begin
        if SameText(CmpName, HotCmdsName) then begin
          HotCmds := TWinControl(cmp)
        end else if SameText(CmpName, DescPaneName) then begin
          DescPane := TWinControl(cmp)
        end;
      end;
    end;

    // Always remove them all first and add them from the bottom up, so the
    // order of the panes and the splitters is correct.
    HideControl(_Form, HidePanel, DescPane, False);
    HideControl(_Form, HidePanel, DescSplitter, False);
    HideControl(_Form, HidePanel, HotCmds, False);
    HideControl(_Form, HidePanel, HotSplitter, False);

    HideControl(_Form, HidePanel, DescPane, FDescriptionPaneVisible);
    HideControl(_Form, HidePanel, DescSplitter, FDescriptionPaneVisible);
    HideControl(_Form, HidePanel, HotCmds, FHotCommandsVisible);
    HideControl(_Form, HidePanel, HotSplitter, FDescriptionPaneVisible);
  end;

var
  Form: TForm;
  FormIdx: Integer;
begin
  for FormIdx := 0 to Screen.FormCount - 1 do begin
    Form := Screen.Forms[FormIdx];
    if CompareText(Form.ClassName, 'TPropertyInspector') = 0 then begin
      HideItems(Form);
      Break;
    end;
  end;
end;

procedure TDesignNotification.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin

end;

var
  DestroyCount: Integer;

destructor TDesignNotification.Destroy;
begin
  Inc(DestroyCount);
  inherited;
end;

procedure TDesignNotification.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin

end;

procedure TDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin

end;

procedure TDesignNotification.ItemsModified(const ADesigner: IDesigner);
begin

end;

procedure TDesignNotification.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
var
  C: TComponent;
begin
  //  This can get called with ADesigner = Nil
  if ADesigner = nil then
    Exit;
  C := ADesigner.Root;
  if C <> nil then begin
    SetItemsVisible;
  end
end;

procedure TDesignNotification.SetDescriptionPaneVisible(const Value: Boolean);
begin
  FDescriptionPaneVisible := Value;
  SetItemsVisible;
end;

procedure TDesignNotification.SetHotCommandsVisible(const Value: Boolean);
begin
  FHotCommandsVisible := Value;
  SetItemsVisible;
end;

var
  DesignNotification: TDesignNotification = nil;

{$ENDIF}

{ TGxIdeObjectInspectorEnhancer }

class function TGxIdeObjectInspectorEnhancer.GetHideDescriptionPane: Boolean;
begin
{$IFDEF GX_EnableOIEnhancer}
  Result := Assigned(DesignNotification) and not DesignNotification.DescriptionPaneVisible;
{$ELSE}
  Result := False;
{$ENDIF}
end;

class function TGxIdeObjectInspectorEnhancer.GetHideHotCmds: Boolean;
begin
{$IFDEF GX_EnableOIEnhancer}
  Result := Assigned(DesignNotification) and not DesignNotification.HotCommandsVisible;
{$ELSE}
  Result := False;
{$ENDIF}
end;

class procedure TGxIdeObjectInspectorEnhancer.SetHideDescriptionPane(const Value: Boolean);
begin
{$IFDEF GX_EnableOIEnhancer}
  if Value then begin
    if not Assigned(DesignNotification) then begin
      DesignNotification := TDesignNotification.Create;
      RegisterDesignNotification(DesignNotification);
    end;
    DesignNotification.DescriptionPaneVisible := False;
  end else begin
    if Assigned(DesignNotification) then begin
      DesignNotification.DescriptionPaneVisible := True;
    end;
  end;
{$ENDIF}
end;

class procedure TGxIdeObjectInspectorEnhancer.SetHideHotCmds(const Value: Boolean);
begin
{$IFDEF GX_EnableOIEnhancer}
  if Value then begin
    if not Assigned(DesignNotification) then begin
      DesignNotification := TDesignNotification.Create;
      RegisterDesignNotification(DesignNotification);
    end;
    DesignNotification.HotCommandsVisible := False;
  end else begin
    if Assigned(DesignNotification) then begin
      DesignNotification.HotCommandsVisible := True;
    end;
  end;
{$ENDIF}
end;

end.

