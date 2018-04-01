unit GX_Actions;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ActnList,
  Menus, ImgList,
  UITypes,
  GX_KbdShortCutBroker;

type
  { This interface is a rather typical action - except
    that it takes care of
      a) freeing + unregistering due to reference-counting
      b) keyboard shortcut assignment which is special
         due to the IDE's interference automatically. }
  IGxAction = interface(IUnknown)
    ['{4B163781-D710-11D3-A95A-5AE3EA000000}']
    function GetCaption: string;
    function GetCategory: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetImageIndex: TImageIndex;
    function GetOnExecute: TNotifyEvent;
    function GetOnUpdate: TNotifyEvent;
    function GetShortCut: TShortCut;
    function GetVisible: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetOnExecute(Value: TNotifyEvent);
    procedure SetOnUpdate(Value: TNotifyEvent);
    procedure SetShortCut(Value: TShortCut);
    procedure SetVisible(Value: Boolean);
    function GetAction: TCustomAction;

    property Caption: string read GetCaption write SetCaption;
    property Category: string read GetCategory write SetCategory;
    property Checked: Boolean read GetChecked write SetChecked;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Hint: string read GetHint write SetHint;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
    property OnExecute: TNotifyEvent read GetOnExecute write SetOnExecute;
    property OnUpdate: TNotifyEvent read GetOnUpdate write SetOnUpdate;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IGxMenuAction = interface(IGxAction)
    ['{FD68EC31-D723-11D3-A95A-5AE3EA000000}']
    function GetAssociatedMenuItem: TMenuItem;

    property AssociatedMenuItem: TMenuItem read GetAssociatedMenuItem;
  end;

type
  { Base implementor for all actions implemented and registered
    by GExperts.
    Features life-time management through reference-counting. }
  TGxCustomAction = class(TCustomAction, IGxAction)
  private
    FIdeShortCut: IGxKeyboardShortCut;
  protected
    property IdeShortCut: IGxKeyboardShortCut read FIdeShortCut write FIdeShortCut;
  protected
    // IUnknown overrides
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    // IGxAction
    function GetCaption: string;
    function GetCategory: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetImageIndex: TImageIndex;
    function GetOnExecute: TNotifyEvent;
    function GetOnUpdate: TNotifyEvent;
    function GetShortCut: TShortCut;
    function GetVisible: Boolean;
    procedure SetCaption(const Value: string); {$ifdef GX_VER240_up} override; {$endif}
    procedure SetCategory(const Value: string);
    procedure SetChecked(Value: Boolean); {$ifdef GX_VER240_up} override; {$endif}
    procedure SetEnabled(Value: Boolean);{$ifdef GX_VER240_up} override; {$endif}
    procedure SetHint(const Value: string); {$ifdef GX_VER240_up} override; {$endif}
    procedure SetImageIndex(Value: TImageIndex); {$ifdef GX_VER240_up} override; {$endif}
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetOnUpdate(Value: TNotifyEvent);
    procedure SetShortCut(Value: TShortCut); {$ifdef GX_VER240_up} override; {$endif}
    procedure SetVisible(Value: Boolean); {$ifdef GX_VER240_up} override; {$endif}
  public
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    function GetAction: TCustomAction;
  end;

implementation

uses
  Windows;

{ TGxCustomAction }

function TGxCustomAction.GetAction: TCustomAction;
begin
  Result := Self; 
end;

function TGxCustomAction.GetCaption: string;
begin
  Result := inherited Caption;
end;

function TGxCustomAction.GetCategory: string;
begin
  Result := inherited Category;
end;

function TGxCustomAction.GetChecked: Boolean;
begin
  Result := inherited Checked;
end;

function TGxCustomAction.GetEnabled: Boolean;
begin
  Result := inherited Enabled;
end;

function TGxCustomAction.GetHint: string;
begin
  Result := inherited Hint;
end;

function TGxCustomAction.GetImageIndex: TImageIndex;
begin
  Result := inherited ImageIndex;
end;

function TGxCustomAction.GetOnExecute: TNotifyEvent;
begin
  Result := inherited OnExecute;
end;

function TGxCustomAction.GetOnUpdate: TNotifyEvent;
begin
  Result := inherited OnUpdate;
end;

function TGxCustomAction.GetShortCut: TShortCut;
begin
  Result := inherited ShortCut;
end;

function TGxCustomAction.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TGxCustomAction.SetCaption(const Value: string);
begin
{$ifdef GX_VER240_up}
  inherited SetCaption(Value);
{$else}
  inherited Caption := Value;
{$endif}

  if Hint = '' then
    Hint := StripHotkey(Value);
end;

procedure TGxCustomAction.SetCategory(const Value: string);
begin
  TCustomAction(Self).Category := Value;
end;

procedure TGxCustomAction.SetChecked(Value: Boolean);
begin
{$ifdef GX_VER240_up}
  inherited SetChecked(Value);
{$else}
  inherited Checked := Value;
{$endif}
end;

procedure TGxCustomAction.SetEnabled(Value: Boolean);
begin
{$ifdef GX_VER240_up}
  inherited SetEnabled(Value);
{$else}
  inherited Enabled := Value;
{$endif}
end;

procedure TGxCustomAction.SetHint(const Value: string);
begin
{$ifdef GX_VER240_up}
  inherited SetHint(Value);
{$else}
  inherited Hint := Value;
{$endif}
end;

procedure TGxCustomAction.SetImageIndex(Value: TImageIndex);
begin
{$ifdef GX_VER240_up}
  inherited SetImageIndex(Value);
{$else}
  inherited ImageIndex := Value;
{$endif}
end;

procedure TGxCustomAction.SetOnExecute(Value: TNotifyEvent);
begin
  inherited SetOnExecute(Value);
end;

procedure TGxCustomAction.SetOnUpdate(Value: TNotifyEvent);
begin
  inherited OnUpdate := Value;
end;

procedure TGxCustomAction.SetShortCut(Value: TShortCut);
begin
{$ifdef GX_VER240_up}
  inherited SetShortCut(Value);
{$else}
  inherited ShortCut := Value;
{$endif}
end;

procedure TGxCustomAction.SetVisible(Value: Boolean);
begin
{$ifdef GX_VER240_up}
  inherited SetVisible(Value);
{$else}
  inherited Visible := Value;
{$endif}
end;

function TGxCustomAction._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGxCustomAction._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) { and (Owner = nil) } then
    Destroy;
end;

end.

