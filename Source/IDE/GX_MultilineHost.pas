unit GX_MultilineHost;

// Original Author: Stefan Hoffmeister <Stefan.Hoffmeister@econos.de>

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls;

type
  TGxMultiLineTabDockHostsManager = class(TObject)
  private
    FSqueezingCode: Pointer;
    FDockableFormClass: TClass;
    FNotificationVmtIndex: Integer;
  protected
    procedure DisableHooking;
    procedure EnableHooking;
    function DetermineNotificationVmtIndex: Integer;
    function GetTabDockHostFormClass: TClass;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  DefaultToMultiLine: Boolean = False;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  {$IFDEF LINUX} WinUtils, {$ENDIF LINUX}
  SysUtils, Windows, Forms, Menus, ComCtrls, ImgList,
  GX_VerDepConst, GX_ClassHacks;

type
  TNotificationSignature = procedure(Self: TObject; AComponent: TComponent; Operation: TOperation); register;

var
  ReplacedNotification: TNotificationSignature;

const
  AddedMultiLineMenuItemName = 'mnuGxMultiLineTab';

type
  TGxSqueezedInClass = class(TComponent)
  // We cannot have any data members, since the class may never be instantiated...
  // Therefore, "Self" in the context of this class isn't exactly
  // what might be expected.
  private
    procedure InsertMultiLineControls(TabDockHost: TWinControl);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure PopupMenuPopup(Sender: TObject);
    procedure MultiLineClick(Sender: TObject);
  end;


procedure TGxSqueezedInClass.InsertMultiLineControls(TabDockHost: TWinControl);
resourcestring
  SMultiLine = 'MultiLine';
var
  MenuItem: TMenuItem;
  FoundPopupMenu: TPopupMenu;
  FoundPageControl: TComponent;
  APageControl: TPageControl;
  i: Integer;
begin
  for i := 0 to TabDockHost.ComponentCount-1 do
  begin
    if TabDockHost.Components[i] is TPopupMenu then
    begin
      FoundPopupMenu := TPopupMenu(TabDockHost.Components[i]);
      if FoundPopupMenu.FindComponent(AddedMultiLineMenuItemName) <> nil then
        Break;

      FoundPopupMenu.OnPopup := PopupMenuPopup;

      // Add a break line
      MenuItem := TMenuItem.Create(FoundPopupMenu);
      MenuItem.Caption := '-';

      FoundPopupMenu.Items.Insert(0, MenuItem);

      // Our multi-line entry
      MenuItem := TMenuItem.Create(FoundPopupMenu);
      MenuItem.Name := AddedMultiLineMenuItemName;
      MenuItem.Checked := DefaultToMultiLine;
      MenuItem.Caption := SMultiLine;
      MenuItem.OnClick := MultiLineClick;

      FoundPopupMenu.Items.Insert(0, MenuItem);

      // Done looping
      Break;
    end;
  end;

  for i := 0 to TabDockHost.ComponentCount-1 do
  begin
    FoundPageControl := TabDockHost.Components[i];
    if FoundPageControl is TPageControl then
    begin
      APageControl := TPageControl(FoundPageControl);
      if DefaultToMultiLine then
        APageControl.MultiLine := True;
    end;
  end;
end;

procedure TGxSqueezedInClass.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Warning: Handle "Self" with care...
  Assert(@ReplacedNotification <> nil);
  ReplacedNotification(Self, AComponent, Operation); // Essentially call "inherited;"

  Assert(TComponent(Self) is TWinControl);
  InsertMultiLineControls(TWinControl(Self));
end;

procedure TGxSqueezedInClass.MultiLineClick(Sender: TObject);
var
  i: Integer;
  IsMultiLine: Boolean;
  PageControl: TComponent;
begin
  // Warning: Handle "Self" with care...
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    IsMultiLine := Checked;
  end;

  for i := 0 to ComponentCount-1 do
  begin
    PageControl := Components[i];
    if PageControl is TPageControl then
    begin
      TPageControl(PageControl).MultiLine := IsMultiLine;
    end;
  end;
end;

procedure TGxSqueezedInClass.PopupMenuPopup(Sender: TObject);
var
  TabDockForm: TCustomForm;
  FoundPageControl: TPageControl;
  MenuItem: TMenuItem;
  i: Integer;
begin
  // Warning: Handle "Self" with care...
  TabDockForm := (Sender as TPopupMenu).Owner as TCustomForm;
  for i := 0 to TabDockForm.ComponentCount-1 do
  begin
    if TabDockForm.Components[i] is TPageControl then
    begin
      FoundPageControl := TPageControl(TabDockForm.Components[i]);
      MenuItem := (Sender as TPopupMenu).FindComponent(AddedMultiLineMenuItemName) as TMenuItem;
      Assert(MenuItem <> nil);
      MenuItem.Enabled := not (FoundPageControl.TabPosition in [tpLeft, tpRight]);
      MenuItem.Checked := FoundPageControl.MultiLine;
    end;
  end;
end;

{ TGxMultiLineTabDockHostsManager }

constructor TGxMultiLineTabDockHostsManager.Create;
(*
var
  i: Integer;
  TabDockHostForm: TComponent;
*)
begin
  inherited Create;

  FSqueezingCode := @TGxSqueezedInClass.Notification;

  FDockableFormClass := GetTabDockHostFormClass;
  FNotificationVmtIndex := DetermineNotificationVmtIndex;

  EnableHooking;
(*
  // Scan for tab dock hosts that are already present
  // and add ourselves to those forms that are already
  // present.
  // The order is important - first we must enable
  // hooking, since this is tested (implicitly) in
  // the squeezing class
  for i := 0 to Screen.FormCount-1 do
  begin
    if SameText(Screen.Forms[i].ClassName, TTabDockHostFormName) then
    begin
      TabDockHostForm := Screen.Forms[i];
      with TGxSqueezedInClass.Create(TabDockHostForm) do
        InsertMultiLineControls(TabDockHostForm);
    end;
  end;
*)
end;

destructor TGxMultiLineTabDockHostsManager.Destroy;
begin
  DisableHooking;
  
  FDockableFormClass := nil;
  FNotificationVmtIndex := -1;
  FSqueezingCode := nil;

  inherited Destroy;
end;

function GetVirtualMethodPointer(AClass: TClass; const Index: Cardinal): Pointer;
type
  PPointer = ^Pointer;
begin
  Result := PPointer(Cardinal(AClass) + Index * SizeOf(Pointer))^;
end;

procedure SetVirtualMethodPointer(AClass: TClass; const Index: Cardinal; const Method: Pointer);
var
  PatchAddress: Pointer;
  DummyProtection: DWORD;
  OldProtection: DWORD;
begin
  PatchAddress := Pointer(Cardinal(AClass) + Index * SizeOf(Pointer));

  // Set memory access rights so that we can write into this page
  if not VirtualProtect(PatchAddress, SizeOf(Pointer), PAGE_READWRITE, @OldProtection) then
    RaiseLastOsError;

  try
    // Write method into VMT
    Pointer(PatchAddress^) := Method;

  finally
    // Restore memory access rights
    if not VirtualProtect(PatchAddress, SizeOf(Pointer), OldProtection, @DummyProtection) then
      RaiseLastOsError;
  end;

  // Make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

procedure TGxMultiLineTabDockHostsManager.DisableHooking;
begin
  if FDockableFormClass = nil then
    Exit;

  Assert(@ReplacedNotification <> nil);

  if FNotificationVmtIndex < 0 then
    Exit;

  Assert(FSqueezingCode = GetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex));
  SetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex, @ReplacedNotification);
  @ReplacedNotification := nil;
end;

procedure TGxMultiLineTabDockHostsManager.EnableHooking;
begin
  if FDockableFormClass = nil then
  begin
    {$IFOPT D+} SendDebugError('Unable to locate IDE tab dock host form class'); {$ENDIF}
    Exit;
  end;

  Assert(@ReplacedNotification = nil);

  if FNotificationVmtIndex < 0 then
    Exit;

  @ReplacedNotification := GetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex);
  SetVirtualMethodPointer(FDockableFormClass, FNotificationVmtIndex, FSqueezingCode);
end;

function TGxMultiLineTabDockHostsManager.GetTabDockHostFormClass: TClass;
begin
  Result := GetClassReference(TTabDockHostFormName, TTabDockHostFormClassContainer);
end;

function TGxMultiLineTabDockHostsManager.DetermineNotificationVmtIndex: Integer;
type
  PPointer = ^Pointer;
const
  MaxIndex = 500;
var
  VmtScanner: PPointer;
  Index: Integer;
begin
  Result := -1;

  if FDockableFormClass = nil then
    Exit;

  // We need to "find" the index of the Notification virtual function;
  // theoretically hard-coding the index would work, too, but that
  // breaks as soon as the VCL changes somewhere deep down.
  // Accomplish this by finding the index in our squeezing class
  // (TGxSqueezedInClass) and simply use that index for our target
  // class - it is guaranteed to be the same index by virtue of
  // virtual method tables (and not compiler version dependent).
  VmtScanner := PPointer(TGxSqueezedInClass);
  Index := -1;

  // MaxIndex is just a sanity checker in order to
  // scan at most MaxIndex VMT entries.
  while Index < MaxIndex do
  begin
    Inc(Index);
    if VmtScanner^ = FSqueezingCode then
      Break;
    Inc(VmtScanner);
  end;

  if Index = MaxIndex then
    Exit;

  Result := Index;
end;

end.

