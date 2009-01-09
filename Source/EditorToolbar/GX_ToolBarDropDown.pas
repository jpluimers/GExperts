unit GX_ToolBarDropDown;

interface

{$I GX_CondDefine.inc}

uses
  Windows,
  Classes, Controls, Menus,
  GX_OtaUtils;

type
  IPopupInterface = interface(IUnknown)
    ['{8D9E1714-D0F2-11D3-A943-625A4B000000}']
    // Show popup menu below passed control aligned to
    // the control's left bottom corner.
    procedure ShowPopupBelowControl(AControl: TControl);
    // Show popup menu at position APoint.
    procedure PopupAt(const APoint: TPoint);
  end;

  procedure InitializeGXToolBarDropDowns;

  { TODO 4 -oAnyone -cFeature: Equip the actions with
    an IPopupMenuAction interface. The (editor) toolbar then
    may query the action instance for the interface. If
    the interface is supported, assign
    IPopupMenuAction.PopupMenu to TToolButton.DropDownMenu }
(* Currently unused
type
  IPopupMenuAction = interface(IUnknown)
    ['{AEEA0DC9-275F-47C6-9BD6-ABBD2D67CE43}']
    function PopupMenu: TPopupMenu;
  end;
*)

type
  // Base class for reference-counted, self-destroying
  // popup menus.
  TRefCountedPopupMenu = class(TPopupMenu, IPopupInterface)
  protected
    // Added, fresh IUnknown reference counting
    // The VCL by default provides reference-counting
    // that is only usable with VCLComObject.
    FRefCount: Integer;
    FPopupPoint: TPoint;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddNoneItemIfEmpty;
    procedure FillMenuWithContent; virtual; abstract;
    function CalculateColumnCount: Cardinal;
  public
    // IPopupInterface
    procedure ShowPopupBelowControl(AControl: TControl); virtual;
    procedure PopupAt(const APoint: TPoint); virtual;
  end;

type
  TBasePopupListing = class(TRefCountedPopupMenu)
  protected
    FUnitList: TList;
    procedure GotoFileClick(Sender: TObject); virtual;
    procedure FillMenuWithContent; override;
    procedure PopulateUnitList; virtual;
    // Abstract method which is called with UnitInfo
    // unit information data.
    // The implementation looks at UnitInfo and manipulates
    // AList based on the data found in UnitInfo.
    // Note that Data must always remain associated with UnitInfo;
    // it must be stored in AList.
    { TODO 4 -oAnyone -cCleanup: This is a horrible callback interface.
      Its main problem is that there are two lots of information and the
      awkward treatment of the opaque "Data" pointer. }
    procedure ProcessUnitInfo(AList: TStrings; UnitInfo: TUnitInfo; Data: Pointer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // IPopupInterface
    procedure ShowPopupBelowControl(AControl: TControl); override;
  end;

  { TODO 4 -oAnyone -cFeature: Add hierarchical building of menus following
      a directory structure. Model following the implementation as proposed
      by "wfiskfr at yahoo.co.uk" as found in
        http://groups.yahoo.com/group/GExpertsDiscuss/message/687
      Be sure to use MinimizeName to keep the width of menu items under control. }
  TUnitPopupListing = class(TBasePopupListing)
  protected
    procedure ProcessUnitInfo(AList: TStrings; UnitInfo: TUnitInfo; Data: Pointer); override;
  end;

  TFormPopupListing = class(TBasePopupListing)
  protected
    procedure ProcessUnitInfo(AList: TStrings; UnitInfo: TUnitInfo; Data: Pointer); override;
  end;

  TUsesPopupListing = class(TBasePopupListing)
  protected
    procedure GotoFileClick(Sender: TObject); override;
    procedure PopulateUnitList; override;
    procedure ProcessUnitInfo(AList: TStrings; UnitInfo: TUnitInfo; Data: Pointer); override;
  end;

  TComponentPopupListing = class(TRefCountedPopupMenu)
  private
    procedure FocusInspectorClick(Sender: TObject);
    procedure AddComponentsToPopup(CompInfo: TCompInfo);
  protected
    procedure FillMenuWithContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPositionPopupListing = class(TRefCountedPopupMenu, IPopupInterface)
  private
    procedure GotoPosition(Sender: TObject);
  protected
    procedure FillMenuWithContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils,
  {$IFDEF LINUX} WinUtils, {$ENDIF}
  Graphics, StdCtrls, Forms, Dialogs,
  GX_Actions, GX_ActionBroker,
  GX_GxUtils, GX_GenericUtils, GX_IdeUtils,
  GX_EditReader, GX_UsesManager, GX_UnitPositions, Math;

const
  MinMenuLines = 5;

{ TRefCountedPopupMenu }

function TRefCountedPopupMenu._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TRefCountedPopupMenu._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TBasePopupListing }

{$IFOPT D+}
var
  InternalPopupListingCount: Integer;
{$ENDIF D+}

procedure TRefCountedPopupMenu.AddNoneItemIfEmpty;
resourcestring
  SNone = 'None';
var
  TempMenu: TMenuItem;
begin
  if Self.Items.Count < 1 then
  begin
    TempMenu := TMenuItem.Create(Self);
    TempMenu.Enabled := False;
    TempMenu.Caption := SNone;
    Self.Items.Add(TempMenu);
  end;
end;

constructor TBasePopupListing.Create(AOwner: TComponent);
begin
  {$IFOPT D+}
    SendDebug('Creating popup list');
    Inc(InternalPopupListingCount);
  {$ENDIF D+}

  inherited Create(AOwner);

  FUnitList := TList.Create;
end;

destructor TBasePopupListing.Destroy;
begin
  {$IFOPT D+} SendDebug('Clearing unit list'); {$ENDIF}
  ClearUnitInfoList(FUnitList);
  FreeAndNil(FUnitList);

  inherited Destroy;

  {$IFOPT D+}
    Dec(InternalPopupListingCount);
    SendDebug(Format('Destroying popup list - remaining: %d', [InternalPopupListingCount]));
  {$ENDIF D+}
end;

procedure TBasePopupListing.FillMenuWithContent;
var
  Menu: TMenuItem;
  i: Integer;
  UnitInfo: TUnitInfo;
  List: TStringList;
begin
  PopulateUnitList;

  List := TStringList.Create;
  try
    for i := 0 to FUnitList.Count - 1 do
    begin
      UnitInfo := TObject(FUnitList[i]) as TUnitInfo;
      ProcessUnitInfo(List, UnitInfo, Pointer(i));
    end;

    List.Sorted := True;

    for i := 0 to List.Count - 1 do
    begin
      Menu := TMenuItem.Create(nil);
      Menu.Tag := Integer(List.Objects[i]);
      Menu.Caption := List.Strings[i];

      if (i > 0) and (Cardinal(i) mod CalculateColumnCount = 0) then
        Menu.Break := mbBarBreak;

      Menu.OnClick := GotoFileClick;

      Self.Items.Add(Menu);
    end;
    AddNoneItemIfEmpty;
  finally
    FreeAndNil(List);
  end;
end;

procedure TBasePopupListing.GotoFileClick(Sender: TObject);
var
  EditRead: TEditReader;
  UnitInfo: TUnitInfo;
  SendingMenu: TMenuItem;
  SendingMenuCaption: string;
begin
  SendingMenu := Sender as TMenuItem;

  // Compensate for AutoHotKeys in D5+
  SendingMenuCaption := StringReplace(SendingMenu.Caption, '&', '', [rfReplaceAll]);

  UnitInfo := TObject(FUnitList[SendingMenu.Tag]) as TUnitInfo;

  {$IFOPT D+}SendDebug('Opening file: ' + UnitInfo.FileName);{$ENDIF}
  GxOtaOpenFile(UnitInfo.FileName);

  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData.
  EditRead := TEditReader.Create(UnitInfo.FileName);
  try
    if SendingMenuCaption = UnitInfo.FormName then
      EditRead.ShowForm
    else
      EditRead.ShowSource;
  finally
    FreeAndNil(EditRead);
  end;
end;

procedure TBasePopupListing.PopulateUnitList;
begin
  ClearUnitInfoList(FUnitList);
  GxOtaFillUnitInfoListForCurrentProject(FUnitList);
end;

procedure TBasePopupListing.ShowPopupBelowControl(AControl: TControl);
var
  MenuPopupPosition: TPoint;
  MaxMenuTop: Integer;
begin
  Assert(Assigned(AControl));
  Assert(Assigned(AControl.Parent));

  MenuPopupPosition.X := AControl.Left;
  MenuPopupPosition.Y := AControl.Top + AControl.Height + 1;

  MaxMenuTop := Screen.Height - (GetStandardMenuItemHeight * MinMenuLines);
  if AControl.Parent.ClientToScreen(MenuPopupPosition).Y > MaxMenuTop then
    MenuPopupPosition.Y := AControl.Parent.ScreenToClient(Point(0, MaxMenuTop)).Y;
  MenuPopupPosition := AControl.Parent.ClientToScreen(MenuPopupPosition);

  PopupAt(MenuPopupPosition);
end;

procedure TRefCountedPopupMenu.PopupAt(const APoint: TPoint);
var
  Cursor: IInterface;
begin
  Cursor := TempHourglassCursor;
  Application.ProcessMessages;
  FPopupPoint := APoint;

  {$IFOPT D+}SendDebug('Filling menu with content');{$ENDIF}
  FillMenuWithContent;

  {$IFOPT D+}SendDebug('Popping up the menu');{$ENDIF}
  Self.Popup(APoint.X, APoint.Y);
end;

procedure TRefCountedPopupMenu.ShowPopupBelowControl(AControl: TControl);
var
  MenuPopupPosition: TPoint;
begin
  Assert(Assigned(AControl));
  Assert(Assigned(AControl.Parent));

  MenuPopupPosition.X := AControl.Left;
  MenuPopupPosition.Y := AControl.Top + AControl.Height + 1;
  MenuPopupPosition := AControl.Parent.ClientToScreen(MenuPopupPosition);

  PopupAt(MenuPopupPosition);
end;


function TRefCountedPopupMenu.CalculateColumnCount: Cardinal;
begin
  Result := (Screen.Height - FPopupPoint.Y - 10) div GetStandardMenuItemHeight;

  if Result < 1 then
    Result := 1;
end;

{ TUnitPopupListing }

procedure TUnitPopupListing.ProcessUnitInfo(AList: TStrings;
  UnitInfo: TUnitInfo; Data: Pointer);
begin
  Assert(Assigned(UnitInfo));
  Assert(Assigned(AList));
  if IsBdsSourceFile(UnitInfo.FileName) then
    if AList.IndexOf(UnitInfo.SourceName) = -1 then
      AList.AddObject(UnitInfo.SourceName, Data);
end;

{ TFormPopupListing }

procedure TFormPopupListing.ProcessUnitInfo(AList: TStrings;
  UnitInfo: TUnitInfo; Data: Pointer);
var
  TrimmedFormName: string;
begin
  Assert(Assigned(UnitInfo));
  Assert(Assigned(AList));

  TrimmedFormName := Trim(UnitInfo.FormName);
  if TrimmedFormName <> '' then
    AList.AddObject(TrimmedFormName, Data);
end;

{ TUsesPopupListing }

procedure TUsesPopupListing.GotoFileClick(Sender: TObject);
resourcestring
  SUnableToLocateFile = 'Unable to locate ';
var
  UnitInfo: TUnitInfo;
  SendingMenu: TMenuItem;
begin
  SendingMenu := Sender as TMenuItem;

  UnitInfo := TObject(FUnitList[SendingMenu.Tag]) as TUnitInfo;
  if not GxOtaOpenFileFromPath(UnitInfo.FileName) then
    ShowMessage(SUnableToLocateFile + UnitInfo.FileName);
end;

procedure TUsesPopupListing.PopulateUnitList;
var
  Aliases: TStringList;

  function ApplyAlias(const UnitName: string): string;
  var
    i: Integer;
  begin
    Result := UnitName;
    i := Aliases.IndexOfName(Result);
    if i <> -1 then
      Result := Aliases.Values[Result];
  end;

var
  UsesManager: TUsesManager;
  UnitInfo: TUnitInfo;
  i: Integer;
  UsesItem: TUsesItem;
begin
  AssertIsDprOrPasOrInc(GxOtaGetCurrentSourceFile);
  ClearUnitInfoList(FUnitList);
  Aliases := TStringList.Create;
  try
    GxOtaGetUnitAliases(Aliases);
    {$IFOPT D+}SendDebug('Populating empty unit list');{$ENDIF}
    UsesManager := TUsesManager.Create(GxOtaGetCurrentSourceEditor);
    try
      {$IFOPT D+}SendDebug('Iterating through interface uses.  Count = ' + IntToStr(UsesManager.InterfaceUses.Count));{$ENDIF}
      for i := 0 to UsesManager.InterfaceUses.Count - 1 do
      begin
        UnitInfo := TUnitInfo.Create;
        FUnitList.Add(UnitInfo);
        UsesItem := UsesManager.InterfaceUses.Items[i];
        UnitInfo.FileName := ApplyAlias(UsesItem.Name) + '.pas';
        UnitInfo.FormName := '';
        UnitInfo.SourceName := UsesItem.Name;
      end;
      {$IFOPT D+}SendDebug('Iterating through implementation uses.  Count = ' + IntToStr(UsesManager.ImplementationUses.Count));{$ENDIF}
      for i := 0 to UsesManager.ImplementationUses.Count - 1 do
      begin
        UnitInfo := TUnitInfo.Create;
        FUnitList.Add(UnitInfo);
        UsesItem := UsesManager.ImplementationUses.Items[i];
        UnitInfo.FileName := ApplyAlias(UsesItem.Name) + '.pas';
        UnitInfo.FormName := '';
        UnitInfo.SourceName := UsesItem.Name;
      end;
    finally
      FreeAndNil(UsesManager);
    end;
    {$IFOPT D+}SendDebug('Unit list count = ' + IntToStr(FUnitList.Count));{$ENDIF}
  finally
    FreeAndNil(Aliases);
  end;
end;

procedure TUsesPopupListing.ProcessUnitInfo(AList: TStrings; UnitInfo: TUnitInfo; Data: Pointer);
var
  TrimmedUsesName: string;
begin
  Assert(Assigned(UnitInfo));
  Assert(Assigned(AList));

  TrimmedUsesName := Trim(UnitInfo.SourceName);
  if TrimmedUsesName <> '' then
    if AList.IndexOf(TrimmedUsesName) = -1 then
      AList.AddObject(TrimmedUsesName, Data);
end;

{ TPositionPopupListing }

constructor TPositionPopupListing.Create(AOwner: TComponent);
begin
  {$IFOPT D+}
    SendDebug('Creating popup list');
    Inc(InternalPopupListingCount);
  {$ENDIF D+}

  inherited Create(AOwner);
end;

destructor TPositionPopupListing.Destroy;
begin
  {$IFOPT D+}
    Dec(InternalPopupListingCount);
    SendDebug(Format('Destroying popup list - remaining: %d', [InternalPopupListingCount]));
  {$ENDIF D+}

  inherited Destroy;
end;

procedure TPositionPopupListing.GotoPosition(Sender: TObject);
begin
  Assert(Sender is TMenuItem);
  GxOtaGotoPosition(TMenuItem(Sender).Tag);
  GxOtaMakeSourceVisible(GxOtaGetCurrentSourceFile);
end;

procedure TPositionPopupListing.FillMenuWithContent;
var
  UnitPositions: TUnitPositions;
  MenuItem: TMenuItem;
  i: Integer;
  UnitPosition: TUnitPosition;
begin
  AssertIsDprOrPasOrInc(GxOtaGetCurrentSourceFile);
  UnitPositions := TUnitPositions.Create(GxOtaGetCurrentSourceEditor);
  try
    for i := 0 to UnitPositions.Count - 1 do
    begin
      UnitPosition := UnitPositions.Positions[i];
      if UnitPosition.Position <> -1 then
      begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := UnitPosition.Name;
        MenuItem.Tag     := UnitPosition.Position;
        MenuItem.OnClick := GotoPosition;
        Self.Items.Add(MenuItem);
      end;
    end;
  finally
    FreeAndNil(UnitPositions);
  end;
  AddNoneItemIfEmpty;
end;

{ TComponentPopupListing }

constructor TComponentPopupListing.Create(AOwner: TComponent);
begin
  {$IFOPT D+}
    SendDebug('Creating popup list');
    Inc(InternalPopupListingCount);
  {$ENDIF D+}

  inherited Create(AOwner);
end;

destructor TComponentPopupListing.Destroy;
begin
  {$IFOPT D+}
    Dec(InternalPopupListingCount);
    SendDebug(Format('Destroying popup list - remaining: %d', [InternalPopupListingCount]));
  {$ENDIF D+}

  inherited Destroy;
end;

procedure TComponentPopupListing.FocusInspectorClick(Sender: TObject);
const
  ObjectInspectorFormName = 'PropertyInspector'; // Do not localize.
var
  AppBuilderForm: TCustomForm;
  ObjectInspector: TCustomForm;
begin
  // Compensate for AutoHotKeys.  '&' is not valid in Name anyway
  GxOtaSelectComponentOnCurrentForm(StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]));

  AppBuilderForm := GetIdeMainForm;
  if AppBuilderForm <> nil then
  begin
    ObjectInspector := AppBuilderForm.FindComponent(ObjectInspectorFormName) as TCustomForm;
    if ObjectInspector <> nil then
    begin
      ObjectInspector.Show;
      ObjectInspector.SetFocus;
    end
    else
    begin
      {$IFOPT D+} SendDebug('TComponentPopupListing.FocusInspectorClick: Object inspector not found'); {$ENDIF}
    end;
  end;
end;

function CompareObjects(PointerItem1, PointerItem2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TCompInfo(PointerItem1).CompType, TCompInfo(PointerItem2).CompType);
  if Result = 0 then
  begin
    // The types are equal, so we compare the names.
    Result := AnsiCompareStr(TCompInfo(PointerItem1).CompName, TCompInfo(PointerItem2).CompName);
  end;
end;

procedure TComponentPopupListing.AddComponentsToPopup(CompInfo: TCompInfo);
var
  NumberColumns: Integer;
  SingleMenuBarHeight: Integer;
  i: Integer;
  TempMenu1: TMenuItem;
  TempMenu2: TMenuItem;
begin
  SingleMenuBarHeight := GetSystemMetrics(SM_CYMENU);
  NumberColumns := (Screen.Height - 10) div SingleMenuBarHeight;
  NumberColumns := Max(NumberColumns, 1);

  for i := 0 to Self.Items.Count - 1 do
  begin
    if SameText(CompInfo.CompType, Self.Items[i].Caption) then
    begin
      TempMenu1 := TMenuItem.Create(Self);
      TempMenu1.Caption := CompInfo.CompName;
      TempMenu1.OnClick := FocusInspectorClick;

      if (Self.Items[i].Count > 0) and
         (Self.Items[i].Count mod NumberColumns = 0) then
      begin
        TempMenu1.Break := mbBarBreak;
      end;

      Self.Items[i].Add(TempMenu1);

      Exit;
    end;
  end;

  TempMenu2 := TMenuItem.Create(Self);
  TempMenu2.Caption := CompInfo.CompType;
  Self.Items.Add(TempMenu2);

  TempMenu1 := TMenuItem.Create(Self);
  TempMenu1.Caption := CompInfo.Compname;
  TempMenu1.OnClick := FocusInspectorClick;

  TempMenu2.Add(TempMenu1);
end;

procedure TComponentPopupListing.FillMenuWithContent;
var
  i: Integer;
  List: TList;
  CompInfo: TCompInfo;
begin
  List := TList.Create;
  try
    GxOtaFillComponentInfoList(List);
    try
      List.Sort(CompareObjects);

      for i := 0 to List.Count - 1 do
      begin
        CompInfo := TObject(List.Items[i]) as TCompInfo;
        AddComponentsToPopup(CompInfo);
      end;

      AddNoneItemIfEmpty;
    finally
      ClearCompInfoList(List);
    end;
  finally
    FreeAndNil(List);
  end;
end;

// ***************************************************

type
  TGxToolBarDropDownActions = class(TObject)
  private
    FCurrentlyCreatedPopupMenu: IPopupInterface;

    FUnitDropDownActionBitmap: Graphics.TBitmap;
    IUnitDropDownAction: IGxAction;

    FFormDropDownActionBitmap: Graphics.TBitmap;
    IFormDropDownAction: IGxAction;

    FComponentDropDownActionBitmap: Graphics.TBitmap;
    IComponentDropDownAction: IGxAction;

    FUsesDropDownActionBitmap: Graphics.TBitmap;
    IUsesDropDownAction: IGxAction;

    FPositionDropDownActionBitmap: Graphics.TBitmap;
    IPositionDropDownAction: IGxAction;

    procedure ShowDropDown;

    procedure LoadActionBitmaps;
    procedure FreeActionBitmaps;

    procedure CreateUnitDropDownMenu(Sender: TObject);
    procedure CreateFormDropDownMenu(Sender: TObject);
    procedure CreateComponentDropDownMenu(Sender: TObject);
    procedure CreateUsesDropDownMenu(Sender: TObject);
    procedure CreatePositionDropDownMenu(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TGxToolBarDropDownActions }

constructor TGxToolBarDropDownActions.Create;
resourcestring
  SGxUnitDropDown = 'Unit List';
  SGxFormDropDown = 'Form List';
  SGxComponentDropDown = 'Component List';
  SGxUsesDropDown = 'Uses List';
  SGxPositionDropDown = 'Unit Positions';
begin
  inherited Create;

  LoadActionBitmaps;

  IUnitDropDownAction := GxActionBroker.RequestAction('GxUnitDropDown', FUnitDropDownActionBitmap);
  IUnitDropDownAction.Caption := SGxUnitDropDown;
  IUnitDropDownAction.OnExecute := CreateUnitDropDownMenu;

  IFormDropDownAction := GxActionBroker.RequestAction('GxFormDropDown', FFormDropDownActionBitmap);
  IFormDropDownAction.Caption := SGxFormDropDown;
  IFormDropDownAction.OnExecute := CreateFormDropDownMenu;

  IComponentDropDownAction := GxActionBroker.RequestAction('GxComponentDropDown', FComponentDropDownActionBitmap);
  IComponentDropDownAction.Caption := SGxComponentDropDown;
  IComponentDropDownAction.OnExecute := CreateComponentDropDownMenu;

  IUsesDropDownAction := GxActionBroker.RequestAction('GxUsesDropDown', FUsesDropDownActionBitmap);
  IUsesDropDownAction.Caption := SGxUsesDropDown;
  IUsesDropDownAction.OnExecute := CreateUsesDropDownMenu;

  IPositionDropDownAction := GxActionBroker.RequestAction('GxPositionDropDown', FPositionDropDownActionBitmap);
  IPositionDropDownAction.Caption := SGxPositionDropDown;
  IPositionDropDownAction.OnExecute := CreatePositionDropDownMenu;
end;

procedure TGxToolBarDropDownActions.CreateComponentDropDownMenu(Sender: TObject);
begin
  FCurrentlyCreatedPopupMenu := TComponentPopupListing.Create(nil);

  ShowDropDown;
end;

procedure TGxToolBarDropDownActions.CreateFormDropDownMenu(Sender: TObject);
begin
  FCurrentlyCreatedPopupMenu := TFormPopupListing.Create(nil);

  ShowDropDown;
end;

procedure TGxToolBarDropDownActions.CreatePositionDropDownMenu(Sender: TObject);
begin
  FCurrentlyCreatedPopupMenu := TPositionPopupListing.Create(nil);

  ShowDropDown;
end;

procedure TGxToolBarDropDownActions.CreateUnitDropDownMenu(Sender: TObject);
begin
  FCurrentlyCreatedPopupMenu := TUnitPopupListing.Create(nil);

  ShowDropDown;
end;

procedure TGxToolBarDropDownActions.CreateUsesDropDownMenu(Sender: TObject);
begin
  FCurrentlyCreatedPopupMenu := TUsesPopupListing.Create(nil);

  ShowDropDown;
end;

destructor TGxToolBarDropDownActions.Destroy;
begin
  // Somewhat redundant cleanup.
  FCurrentlyCreatedPopupMenu := nil;

  IUnitDropDownAction := nil;
  IFormDropDownAction := nil;
  IComponentDropDownAction := nil;
  IUsesDropDownAction := nil;
  IPositionDropDownAction := nil;

  FreeActionBitmaps;

  inherited Destroy;
end;

procedure TGxToolBarDropDownActions.FreeActionBitmaps;
begin
  FreeAndNil(FUnitDropDownActionBitmap);
  FreeAndNil(FFormDropDownActionBitmap);
  FreeAndNil(FComponentDropDownActionBitmap);
  FreeAndNil(FUsesDropDownActionBitmap);
  FreeAndNil(FPositionDropDownActionBitmap);
end;

procedure TGxToolBarDropDownActions.LoadActionBitmaps;
begin
  Assert(not Assigned(FUnitDropDownActionBitmap));
  Assert(not Assigned(FFormDropDownActionBitmap));
  Assert(not Assigned(FComponentDropDownActionBitmap));
  Assert(not Assigned(FUsesDropDownActionBitmap));
  Assert(not Assigned(FPositionDropDownActionBitmap));

  GxLoadBitmapForExpert('UnitDropDown', FUnitDropDownActionBitmap);
  GxLoadBitmapForExpert('FormDropDown', FFormDropDownActionBitmap);
  GxLoadBitmapForExpert('ComponentDropDown', FComponentDropDownActionBitmap);
  GxLoadBitmapForExpert('UsesDropDown', FUsesDropDownActionBitmap);
  GxLoadBitmapForExpert('PositionDropDown', FPositionDropDownActionBitmap);
end;

procedure TGxToolBarDropDownActions.ShowDropDown;
var
  MousePosition: TPoint;
  ControlUnderCursor: TWinControl;
begin
  Assert(Assigned(FCurrentlyCreatedPopupMenu));

  MousePosition := Mouse.CursorPos;

  ControlUnderCursor := FindVclWindow(MousePosition);
  if Assigned(ControlUnderCursor) then
  begin
    if ControlUnderCursor is TButtonControl then
      FCurrentlyCreatedPopupMenu.ShowPopupBelowControl(ControlUnderCursor)
    else
      FCurrentlyCreatedPopupMenu.PopupAt(MousePosition);
  end
  else
    FCurrentlyCreatedPopupMenu.PopupAt(MousePosition);
end;

var
  PrivateToolBarDropDowns: TGxToolBarDropDownActions;

procedure InitializeGXToolBarDropDowns;
begin
  PrivateToolBarDropDowns := TGxToolBarDropDownActions.Create;
end;

initialization

finalization
  FreeAndNil(PrivateToolBarDropDowns);
  {$IFOPT D+} Assert(InternalPopupListingCount = 0, 'Reference-counting leak for toolbar drop-down menus'); {$ENDIF}

end.
