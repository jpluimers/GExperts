unit GX_ActionBroker;

{ All editor toolbar buttons will get their actions from this broker.
  This broker also provides transparent access to the Open
  Tools API's ActionList.

  Note: Except for adding a method or two to facilitate notification of
        dynamic menu item enabling and disabling, these interfaces
        should be stable and cover all use cases. }

interface

{$I GX_CondDefine.inc}

uses
  Classes, ActnList, Graphics,
  {$IFDEF GX_VER240_up}
  System.Actions,
  {$ENDIF GX_VER240_up}
  GX_Actions;

type
  IGxActionBroker = interface(IUnknown)
    ['{EC7632E1-D1A3-11D3-A944-DA3A65000000}']
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;

    // Finds an action in the list of available actions
    // Includes all actions registered / requested via
    // GExperts and the actions contained in the IDE's
    // action list.
    // Note that by finding an IDE action and querying
    // for its category name, it is possible to group
    // a GExperts action into the same category as a
    // given IDE action.
    function FindAction(const ActionName: string): TContainedAction;
    // Register an existing action with the action broker.
    // The action will be added to the IDE's action list,
    // making it available to every interested party.
    // Note that the registered action needs to implement
    // the IGxAction interface.
    function RegisterAction(Action: TContainedAction): IGxAction;
    // Request and register a newly created action interface
    // instance that represents an activity that does not live in
    // the GExperts menu.
    // After requesting, in particular Caption and OnExecute
    // need to be set to make the Action "useful"
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    // Request and register an action interface instance
    // that represents an activity that lives in the GExperts menu.
    // After requesting, in particular Caption and OnExecute
    // need to be set to make the Action "useful"
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    // Fill Categories with the all of the action categories
    procedure GetCategories(Categories: TStrings);

    // Access to all actions available to the broker; this
    // includes GExperts's own actions as well as the IDE's
    // actions. Do not hold on to an action instance for
    // any prolonged period of time.
    property Actions[Index: Integer]: TContainedAction read GetActions;
    property ActionCount: Integer read GetActionCount;
  end;


// Get an instance to the GExperts action broker.
function GxActionBroker: IGxActionBroker;

const  // Do not localize.
  GxGenericActionQualifier = 'Tools';
  GExpertsActionCategory = 'GExperts';

resourcestring
  SNoButtonCategory = '(None)';
  SAllButtonsCategory = '(All)';

implementation

uses
  Windows, SysUtils, Forms, Menus, Rescaler,
  ToolsAPI,
  GX_GxUtils, GX_IdeUtils, GX_OtaUtils, GX_KbdShortCutBroker,
  GX_GenericClasses, GX_GenericUtils, Contnrs;

type
  // Special action that implements menu actions.
  TGxMenuAction = class(TGxCustomAction, IUnknown, IGxAction, IGxMenuAction)
  private
    FAssociatedMenuItem: TMenuItem;
    procedure SetShortCut(Value: TShortCut); {$ifdef GX_VER240_up} override; {$endif}
  protected
    function GetAssociatedMenuItem: TMenuItem;
  end;

type
  TGxToolsAction = class(TGxCustomAction, IUnknown, IGxAction)
  private
    procedure SetShortCut(Value: TShortCut); {$ifdef GX_VER240_up} override; {$endif}
  end;

type
  TGxActionBroker = class(TSingletonInterfacedObject, IGxActionBroker)
  private
    FGxActionList: TObjectList;
    procedure ClearGxActionList;
    function GetIdeActionList: TCustomActionList;
    function GetActionOwner: TCustomForm;
    procedure RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
  protected
    // IGxActionBroker
    function GetActions(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    function FindAction(const Name: string): TContainedAction;
    function RegisterAction(Action: TContainedAction): IGxAction;
    function RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
    function RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
    procedure GetCategories(Categories: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PrivateGxActionBroker: TGxActionBroker;

function GxActionBroker: IGxActionBroker;
begin
  if not Assigned(PrivateGxActionBroker) then
    PrivateGxActionBroker := TGxActionBroker.Create;

  Result := PrivateGxActionBroker;
end;

procedure FreeGxActionBroker;
begin
  FreeAndNil(PrivateGxActionBroker);
end;

{ TGxActionBroker }

procedure TGxActionBroker.ClearGxActionList;
var
  i: Integer;
begin
  if Assigned(FGxActionList) then
  begin
    for i := 0 to FGxActionList.Count-1 do
      FGxActionList[i].Free;

    FGxActionList.Clear;
  end;
end;

constructor TGxActionBroker.Create;
begin
  inherited Create;

  FGxActionList := TObjectList.Create(False);
end;

destructor TGxActionBroker.Destroy;
begin
  ClearGxActionList;
  FreeAndNil(FGxActionList);

  inherited Destroy;
end;

function TGxActionBroker.FindAction(const Name: string): TContainedAction;
var
  TempAction: TContainedAction;
  ActionList: TCustomActionList;
  i: Integer;
begin
  Result := nil;

  ActionList := GetIdeActionList;
  Assert(Assigned(ActionList));

  for i := 0 to ActionList.ActionCount-1 do
  begin
    TempAction := ActionList.Actions[i];
    Assert(Assigned(TempAction));

    if SameText(TempAction.Name, Name) then
    begin
      Result := TempAction;
      Break;
    end;
  end;
end;

function TGxActionBroker.GetActionCount: Integer;
begin
  Result := GetIdeActionList.ActionCount;
end;

function TGxActionBroker.GetActionOwner: TCustomForm;
begin
  Result := GetIdeMainForm;
end;

function TGxActionBroker.GetActions(Index: Integer): TContainedAction;
begin
  Result := GetIdeActionList.Actions[Index];
end;

function TGxActionBroker.GetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;

  Assert(Assigned(Result));
end;

function TGxActionBroker.RegisterAction(Action: TContainedAction): IGxAction;
begin
  Result := nil;
  Assert(False, 'Unimplemented function: TGxActionBroker.RegisterAction');
end;

function CreateScaledBitmap(Bitmap: Graphics.TBitmap): Graphics.TBitmap;
const
  RequiredWidth = 16;
  RequiredHeight = 16;
var
  R: TRect;
  TempBitmap: Graphics.TBitmap;
begin
  Result := Graphics.TBitmap.Create;
  if (Bitmap.Width = RequiredWidth) and (Bitmap.Height = RequiredHeight) then
    Result.Assign(Bitmap)
  else
  begin
    // TempBitmap stores a copy of the bitmap but with a transparent color
    // of clBtnFace.  This prevents the rescaling of the image edges
    // from being discolored by the (usually odd) transparent color.
    TempBitmap := Graphics.TBitmap.Create;
    try
      TempBitmap.Height := Bitmap.Height;
      TempBitmap.Width := Bitmap.Width;
      TempBitmap.Canvas.Brush.Color := clBtnFace;
      TempBitmap.Transparent := True;
      R.TopLeft := Point(0, 0);
      R.BottomRight := Point(Bitmap.Height+1, Bitmap.Width+1);
      TempBitmap.Canvas.FillRect(R);
      Bitmap.Transparent := True;
      TempBitmap.Canvas.Draw(0, 0, Bitmap);
      Result.Width := RequiredWidth;
      Result.Height := RequiredHeight;
      Result.Transparent := True;
      if not Rescaler.Rescale(TempBitmap, Result, False) then
        Result.Canvas.StretchDraw(Rect(0, 0, Result.Width, Result.Height), TempBitmap);
    finally
      FreeAndNil(TempBitmap);
    end;
  end;
end;

procedure TGxActionBroker.RegisterActionWithIde(const AAction: TCustomAction; Bitmap: Graphics.TBitmap);
const
  GxBitmapSuffix = 'GxImage'; // Do not localize.
var
  NTAServices: INTAServices;
  ReadyBitmap: Graphics.TBitmap;

  BitmapName: string;
begin
  if IsStandAlone then
    Exit;

  Assert(Assigned(AAction));
  AAction.ActionList := GetIdeActionList;

  if Assigned(Bitmap) then
  begin
    NTAServices := BorlandIDEServices as INTAServices;
    Assert(Assigned(NTAServices));

    ReadyBitmap := CreateScaledBitmap(Bitmap);
    try
      BitmapName := AAction.Name + GxBitmapSuffix;
      {$IFDEF GX_VER170_up}
      ReadyBitmap.Transparent := False; // Prevent invisible enabled icons in XE6 (disabled ones might still be invisible/ghosted)
      {$ENDIF}
      if ReadyBitmap.Transparent then
        AAction.ImageIndex := NTAServices.AddMasked(ReadyBitmap, ReadyBitmap.TransparentColor, BitmapName)
      else
        AAction.ImageIndex := NTAServices.AddMasked(ReadyBitmap, GXTransparencyColor, BitmapName);
    finally
      FreeAndNil(ReadyBitmap);
    end;
  end;
end;

function TGxActionBroker.RequestAction(const ActionName: string; Bitmap: Graphics.TBitmap = nil): IGxAction;
var
  GxToolsAction: TGxToolsAction;
begin
  Assert(Length(ActionName) > 0);
  Assert(IsValidIdent(ActionName));

  GxToolsAction := TGxToolsAction.Create(GetActionOwner);
  GxToolsAction.Category := GExpertsActionCategory;
  GxToolsAction.Name := GExpertsActionCategory + GxGenericActionQualifier + ActionName;

  RegisterActionWithIde(GxToolsAction, Bitmap);

  Result := GxToolsAction;
end;

function TGxActionBroker.RequestMenuAction(const ActionName: string; Bitmap: Graphics.TBitmap): IGxMenuAction;
const
  MenuItemAppendix = '_MenuItem'; // Do not localize.
var
  GxMenuAction: TGxMenuAction;
begin
  Assert(Length(ActionName) > 0);

  GxMenuAction := TGxMenuAction.Create(GetActionOwner);
  GxMenuAction.Category := GExpertsActionCategory;
  GxMenuAction.Name := GExpertsActionCategory + ActionName;

  GxMenuAction.FAssociatedMenuItem := TMenuItem.Create(GxMenuAction);
  GxMenuAction.FAssociatedMenuItem.Name := GxMenuAction.Name + MenuItemAppendix;
  GxMenuAction.FAssociatedMenuItem.Action := GxMenuAction;

  RegisterActionWithIde(GxMenuAction, Bitmap);

  Result := GxMenuAction;
end;

procedure TGxActionBroker.GetCategories(Categories: TStrings);
var
  i: Integer;
  Category: string;
begin
  Assert(Assigned(Categories));
  Categories.Clear;
  for i := 0 to GxActionBroker.ActionCount - 1 do
  begin
    Category := GxActionBroker.Actions[i].Category;
    if Trim(Category) = '' then
      Category := SNoButtonCategory;
    EnsureStringInList(Categories, Category);
  end;
end;

{ TGxMenuAction }

function TGxMenuAction.GetAssociatedMenuItem: TMenuItem;
begin
  Result := FAssociatedMenuItem;
end;

procedure TGxMenuAction.SetShortCut(Value: TShortCut);
begin
  if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then
    IdeShortCut := nil;  // Unregisters the shortcut with the IDE

  if Assigned(FAssociatedMenuItem) and not Assigned(IdeShortCut) then
  begin
    IdeShortCut := GxKeyboardShortCutBroker.RequestMenuShortCut(OnExecute, FAssociatedMenuItem);

    Assert(Assigned(IdeShortCut));
    IdeShortCut.ShortCut := Value;
  end;

  inherited;
end;

{ TGxToolsAction }

procedure TGxToolsAction.SetShortCut(Value: TShortCut);
begin
  // Not necessary under Delphi 5/6 since the callbacks never happen anyway
  if RunningDelphi7OrGreater then
  begin
    if Assigned(IdeShortCut) and (IdeShortCut.ShortCut <> Value) then
      IdeShortCut := nil;  // Unregisters the shortcut with the IDE

    if not Assigned(IdeShortCut) then
    begin
      IdeShortCut := GxKeyboardShortCutBroker.RequestOneKeyShortCut(OnExecute, Value);
      Assert(Assigned(IdeShortCut));
    end;
  end;

  inherited SetShortCut(Value);
end;

initialization

finalization

  FreeGxActionBroker;

end.

