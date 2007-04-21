unit GX_Toolbar;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ComCtrls, ExtCtrls, Controls;

const // Do not localize.
  EditorToolBarRegKey = 'EditorToolBar';
  SeparatorMenuItemString = '-';

type
  TGXToolBar = class(TToolBar)
  private
    // MultiLine editor tab support
    FTabCtrlHeight: SmallInt;
    FTabCtrlPanel: TPanel;
    FTabPanel: TPanel;
    FTabControl: TTabControl;
    FBToolBar: TToolBar;
    FCodePanel: TPanel;
    FOldMouseUp: TMouseEvent;

    procedure AddMiddleButtonClose;
    procedure RemoveMiddleButtonClose;
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    // OnClick handler for the toolbar configuration menu item
    procedure ConfigureToolBarButtonsClick(Sender: TObject);
    procedure CreateToolBarButtons;
    procedure FreeToolBarButtons;

    procedure ApplyEditorTabControlStyles;
    procedure ClearEditorTabControlStyles;
    procedure MultiLineTabResize(Sender: TObject);
    procedure GetEditorComponents;
    procedure AddMultiLineTabs;
    procedure RemoveMultiLineTabs;
    procedure CreateToolBarConfigurationMenu;
  protected
    function GetName: TComponentName;
    procedure SetName(const Value: TComponentName); override;
    procedure UpdateToolBarEdges;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RecreateToolBarButtons;
    procedure SetupToolBarButtons;
    procedure SetEditorControls;

    property Name: TComponentName read GetName write SetName;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Menus, ToolWin, ActnList, Graphics, ToolsAPI,
  GX_OtaUtils, GX_EditorEnhancements, GX_GxUtils, GX_IdeUtils, GX_GenericUtils;

const
  SGxEditorDockToolBar = 'GxEditorDockToolBar';

  // MultiLine editor tab support: Component name constants
  ToolBarName = 'ToolBar1';
  TabControlPanelName = 'TabControlPanel';
  TabControlName = 'TabControl';
  CodePanelName = 'CodePanel';
  TabPanelName = 'TabPanel';

var
  GxUnspecifiedImageIndex: Integer = -1;

procedure RegisterUnspecifiedBitmapWithIde;
const
  UnspecifiedName = 'Unspecified';
var
  Bitmap: TBitmap;
  NTAServices: INTAServices;
begin
  if IsStandAlone then
    Exit;
  if GxUnspecifiedImageIndex <> -1 then
    Exit;
  Bitmap := nil;
  GxLoadBitmapForExpert(UnspecifiedName, Bitmap);
  if Bitmap = nil then
    Exit;
  NTAServices := BorlandIDEServices as INTAServices;
  Assert(Assigned(NTAServices));
  GxUnspecifiedImageIndex := NTAServices.AddMasked(Bitmap, Bitmap.TransparentColor, UnspecifiedName);
  FreeAndNil(Bitmap);
end;

{ TGXToolBar }

constructor TGXToolBar.Create(AOwner: TComponent);
begin
  {$IFOPT D+} SendDebug(Format('Creating GXToolBar for %p', [Pointer(AOwner)])); {$ENDIF}
  inherited;
  RegisterUnspecifiedBitmapWithIde;

  SetToolBarGradient(Self);
  Name := SGxEditorDockToolBar;
  DockSite := False;
  Flat := True;
  AutoSize := True;
  Wrapable := False;
  Visible := False;

  BorderWidth := 0;
  BevelOuter := bvNone;
  BevelInner := bvNone;

  Align := alTop;
  UpdateToolBarEdges;
  SetupToolBarButtons;
  ShowHint := True;

  GetEditorComponents;
  // Set Wrapable here to prevent wrapping on startup
  if FBToolBar <> nil then
    FBToolBar.Wrapable := False;
end;

destructor TGXToolBar.Destroy;
begin
  {$IFOPT D+} SendDebug(Format('ToolBar: Destroying GXToolBar from window %p', [Pointer(Owner)])); {$ENDIF}

  {$IFOPT D+} SendDebug('ToolBar: Clearing editor tab control styles'); {$ENDIF}
  ClearEditorTabControlStyles;

  {$IFOPT D+} SendDebug('ToolBar: Calling inherited destructor'); {$ENDIF}
  inherited;
  {$IFOPT D+} SendDebug('ToolBar: Destroyed'); {$ENDIF}
end;

procedure TGXToolBar.CreateToolBarConfigurationMenu;
resourcestring
  SConfigureToolBar = 'Configure Toolbar...';
var
  APopupMenu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  APopupMenu := TPopupMenu.Create(Self);

  MenuItem := TMenuItem.Create(APopupMenu);
  APopupMenu.Items.Add(MenuItem);
  MenuItem.Caption := SConfigureToolBar;
  MenuItem.OnClick := ConfigureToolBarButtonsClick;

  PopupMenu := APopupMenu;
end;

procedure TGXToolBar.ConfigureToolBarButtonsClick(Sender: TObject);
begin
  EditorEnhancements.ShowToolBarConfigurationDialog;
end;

procedure TGXToolBar.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  Caption := '';
end;

function TGXToolBar.GetName: TComponentName;
begin
  Result := inherited Name;
end;

procedure TGXToolBar.SetEditorControls;
begin
  {$IFOPT D+} SendDebug('ToolBar: SetEditorControls'); {$ENDIF}

  // All these items need to be called from this
  // method, since they potentially cover multiple
  // editor windows.
  ApplyEditorTabControlStyles;
end;

// Get the components of the editor that need resizing.
// Call this every time the components are needed.
procedure TGXToolBar.GetEditorComponents;
var
  TabComponent: TComponent;
begin
  if not Assigned(Owner) then
    Exit;
  {$IFOPT D+}SendDebug('Getting Editor Components'); {$ENDIF}
  if not Assigned(FBToolBar) then
     FBToolBar := Owner.FindComponent(ToolBarName) as TToolBar;
  if not Assigned(FTabControl) then begin
     TabComponent := Owner.FindComponent(TabControlName);
     if TabComponent is TTabControl then
       FTabControl := TabComponent as TTabControl;
  end;
  if not Assigned(FTabCtrlPanel) then
     FTabCtrlPanel := Owner.FindComponent(TabControlPanelName) as TPanel;
  if Assigned(FTabCtrlPanel) then
  begin
    // Set TabCtrlPanel.Height only on first call to get orginal height
    // of TabCtrlPanel.  This value may not be available in .Create
    if FTabCtrlHeight = 0 then
      FTabCtrlHeight := FTabCtrlPanel.Height;
    if not (FTabCtrlPanel.Align = alTop) then
      FTabCtrlPanel.Align := alTop;
  end;
  if not Assigned(FTabPanel) then
    FTabPanel := Owner.FindComponent(TabPanelName) as TPanel;
  if not Assigned(FCodePanel) then
     FCodePanel := Owner.FindComponent(CodePanelName) as TPanel;
  if Assigned(FCodePanel) then
     if not (FCodePanel.Align = alClient) then
       FCodePanel.Align := alClient;
end;

procedure TGXToolBar.RemoveMultiLineTabs;
begin
  {$IFOPT D+} SendDebug('Removing multiline editor tabs'); {$ENDIF}
  GetEditorComponents;
  if Assigned(FTabControl) and Assigned(FBToolBar) then
  begin
    FTabControl.OnResize := nil;
    FTabControl.MultiLine := False;
    MultiLineTabResize(FTabControl);
  end;
end;

procedure TGXToolBar.AddMultiLineTabs;
begin
  {$IFOPT D+}SendDebug('Adding multiline editor tabs'); {$ENDIF}
  GetEditorComponents;
  if Assigned(FTabControl) and Assigned(FBToolBar) then
  begin
    FTabControl.MultiLine := True;
    FTabControl.OnResize  := MultiLineTabResize;
    MultiLineTabResize(FTabControl);
  end;
end;

procedure TGXToolBar.MultiLineTabResize(Sender: TObject);
var
  Adjust: Integer;
begin
  {$IFOPT D+}SendDebug('Resizing editor: Adjusting multiline editor tabs'); {$ENDIF}
  GetEditorComponents;
  if Assigned(FTabCtrlPanel) and Assigned(FTabControl) and Assigned(FBToolBar) and Assigned(FTabPanel) then
  begin
    if FTabControl.Style = tsTabs then
      Adjust := 6  // Adjustment for normal style tabs
    else
      Adjust := 3; // Adjustment for button style tabs

    // If called while the window is closing, TabControl.RowCount will cause AVs
    FTabCtrlPanel.Height := (FTabCtrlHeight * FTabControl.RowCount) - (Adjust * (FTabControl.RowCount - 1));
    FTabCtrlPanel.Top := 0;
    FTabCtrlPanel.Left := 0;

    FTabPanel.Height := FTabCtrlPanel.Height;
    FTabPanel.Top := 0;
    FTabPanel.Left := 0;

    FTabControl.Height := FTabCtrlPanel.Height;
    FTabControl.Top := 0;
    FTabControl.Left := 0;

    FBToolBar.Wrapable := False;
  end;
end;

procedure TGXToolBar.ApplyEditorTabControlStyles;
var
  LocalEditorEnhancements: TEditorEnhancements;
begin
  if IsStandAlone then
    Exit;
  {$IFOPT D+} SendDebug('Applying editor tab control settings'); {$ENDIF}
  LocalEditorEnhancements := EditorEnhancements;

  GetEditorComponents;
  if Assigned(FTabControl) then
  begin
    //Turn MultiLine tabs on and off based on EditorEnhancements MultiLine
    FTabControl.MultiLine := LocalEditorEnhancements.MultiLine;

    if LocalEditorEnhancements.MiddleButtonClose then
      AddMiddleButtonClose
    else
      RemoveMiddleButtonClose;

    if FTabControl.MultiLine then
      AddMultiLineTabs
    else
      RemoveMultiLineTabs;

    FTabControl.HotTrack := LocalEditorEnhancements.HotTrack;

    if LocalEditorEnhancements.Buttons then
    begin
      if LocalEditorEnhancements.ButtonsFlat then
        FTabControl.Style := tsFlatButtons
      else
        FTabControl.Style := tsButtons;
    end
    else
      FTabControl.Style := tsTabs;
  end;
  {$IFOPT D+} SendDebug('Done applying editor tab control settings'); {$ENDIF}
end;

procedure TGXToolBar.ClearEditorTabControlStyles;
begin
  GetEditorComponents;
  if Assigned(FTabControl) then
  begin
    // Do not call RemoveMultiLineTabs here.  It will cause AVs
    // when the window closes due to calls to MultiLineTabResize.
    FTabControl.OnResize := nil;
    FTabControl.MultiLine := False;
    FTabControl.HotTrack := False;
    FTabControl.Style := tsTabs;
  end;
end;

procedure TGXToolBar.FreeToolBarButtons;
begin
  while ComponentCount > 0 do
    Components[0].Free;
end;

procedure TGXToolBar.CreateToolBarButtons;
var
  ToolButton: TToolButton;
  IdeAction: TContainedAction;
  ActionName: string;
  Actions: TStrings;
  i: Integer;
begin
  Actions := EditorEnhancements.ToolBarActionsList;
  Assert(Assigned(Actions));

  for i := Actions.Count-1 downto 0 do
  begin
    ActionName := Actions[i];
    if Length(ActionName) = 0 then
      Continue;

    if ActionName = SeparatorMenuItemString then
    begin
      ToolButton := TToolButton.Create(Self);
      ToolButton.Style := tbsSeparator;
      ToolButton.Visible := Align in [alTop, alBottom];
      IdeAction := nil; // Separators do not have an attached action
    end
    else
    begin
      // Determine whether the action exists within the realm of the current IDE.
      // If it doesn't exist, ignore it.
      // We probably want to consider adding the tool button nevertheless, but
      // with a warning or text description that the action is missing?
      IdeAction := GxOtaGetIdeActionByName(ActionName);
      if Assigned(IdeAction) then
      begin
        ToolButton := TToolButton.Create(Self);
        {$IFDEF GX_VER160_up}
        if (IdeAction is TControlAction) then
          ToolButton.Style := tbsDropDown;
        {$ENDIF}
      end
      else
        ToolButton := nil;
    end;

    if Assigned(ToolButton) then
    begin
      ToolButton.Wrap := Align in [alLeft, alRight];
      ToolButton.Action := IdeAction;
      if Assigned(IdeAction) and (ToolButton.Caption = '') then
        ToolButton.Caption := IdeAction.Name;
      if ToolButton.Hint = '' then
        ToolButton.Hint := StripHotkey(ToolButton.Caption);
      ToolButton.Parent := Self;
      ToolButton.AutoSize := True;
      // It would be better to just show the caption, but I don't know how...
      if IdeAction is TCustomAction then
        if TCustomAction(IdeAction).ImageIndex = -1 then
          ToolButton.ImageIndex := GxUnspecifiedImageIndex;
    end;
  end;
  CreateToolBarConfigurationMenu;
end;

procedure TGXToolBar.RecreateToolBarButtons;
begin
  Visible := False;
  FreeToolBarButtons;
  CreateToolBarButtons;
  SetupToolBarButtons;
  UpdateToolBarEdges;
end;

procedure TGXToolBar.SetupToolBarButtons;
var
  i: Integer;
  Button: TToolButton;
begin
  for i := 0 to ButtonCount - 1 do
  begin
    Button := Buttons[i];
    if Align in [alLeft, alRight] then
    begin
      Button.Wrap := True;
      if Button.Style = tbsSeparator then
        Button.Visible := False;
    end
    else
    begin
      Button.Wrap := False;
      if Button.Style = tbsSeparator then
        Button.Visible := True;
    end;
  end;
end;

procedure TGXToolBar.UpdateToolBarEdges;
const
  AlignToEdgeMap: array[alTop..alRight] of TEdgeBorder =
    (ebBottom, ebTop, ebRight, ebLeft);
begin
  Assert(Align in [alBottom, alTop, alRight, alLeft]);
  EdgeBorders := [AlignToEdgeMap[Self.Align]];
end;

procedure TGXToolBar.AddMiddleButtonClose;
var
  TabMouseUp: TMouseEvent;
  SelfMouseUp: TMouseEvent;
begin
  if Assigned(FTabControl) then
  begin
    TabMouseUp := FTabControl.OnMouseUp;
    SelfMouseUp := OnMouseUp;
    if (@TabMouseUp <> @SelfMouseUp) then
    begin
      FOldMouseUp := FTabControl.OnMouseUp;
      FTabControl.OnMouseUp := OnMouseUp;
    end;
  end;
end;

procedure TGXToolBar.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldIndex, NewIndex:Integer;
  CanChange: Boolean;
begin
  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);

  if (Button = mbMiddle) then
  begin
    CanChange := True;
    OldIndex := FTabControl.TabIndex;
    NewIndex := FTabControl.IndexOfTabAt(X, Y);

    if NewIndex >= 0 then
    begin
      FTabControl.TabIndex := FTabControl.IndexOfTabAt(X, Y);
      if RunningDelphi7OrGreater then
        FTabControl.OnChanging(FTabControl, CanChange);
      if CanChange then
      begin
        FTabControl.OnChange(FTabControl);
        GxOtaCloseCurrentEditorTab;
      end else
        FTabControl.TabIndex := OldIndex;
    end;
  end;
end;

procedure TGXToolBar.RemoveMiddleButtonClose;
begin
   if Assigned(FOldMouseUp) then
     FTabControl.OnMouseUp := FOldMouseUp;
   FOldMouseUp := nil;
end;

end.



