unit GXHotKey;

{$IFDEF LINUX}
{$DEFINE COMBOSTYLE}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, StdCtrls, Messages;

type
{$IFDEF COMBOSTYLE}
  TGXHotKey = class(TCombobox)
  private
    FHotKey: TShortCut;
    FInvalidKeys: THKInvalidKeys;
    FModifiers: THKModifiers;
    function GetHotKey: TShortCut;
    procedure SetHotKey(Value: TShortCut);
  protected
    procedure Loaded;override;
  public
    constructor create(AOwner: TComponent); override;
    procedure RefreshList;
  published
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write FInvalidKeys default [hcNone, hcShift];
    property Modifiers: THKModifiers read FModifiers write FModifiers default [hkAlt];
    property HotKey: TShortCut read GetHotKey write SetHotKey default $0041; { Alt - A }
  end;
{$ELSE}
  TGXHotKey = class(THotKey)
  public
    procedure RefreshList;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFDEF COMBOSTYLE}
uses Menus;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('GExperts', [TGXHotKey]);
end;

{$IFDEF COMBOSTYLE}

var
  PrivateShortCutList: TStringList = nil;

procedure GetShortCutList(ShortCuts: TStrings; IncludeBlank: Boolean);
var
  i: TShortCut;
  ShortCutText: string;
begin
  Assert(Assigned(ShortCuts));
  if not Assigned(PrivateShortCutList) then
  begin
    PrivateShortCutList := TStringList.Create;
    PrivateShortCutList.Duplicates := dupIgnore;
    PrivateShortCutList.Sorted := True;
    if IncludeBlank then
      PrivateShortCutList.Add('');
    for i := Low(TShortCut) to High(TShortCut) do
    begin
      ShortCutText := ShortCutToText(i);
      if ShortCutText <> '' then
        PrivateShortCutList.AddObject(ShortCutText, TObject(i));
    end;
  end;
  ShortCuts.Assign(PrivateShortCutList)
end;

constructor TGXHotKey.create(AOwner: TComponent);
begin
  inherited;
  FInvalidKeys := [hcNone, hcShift];
  FModifiers := [hkAlt];
  FHotKey := $0041;
end;

procedure TGXHotKey.Loaded;
begin
  inherited;
  Style := csDropDownList;
  if not (csDesigning in ComponentState) then
    RefreshList;
end;

function TGXHotKey.GetHotKey: TShortCut;
begin
  if csDesigning in ComponentState then
    Result := FHotKey
  else
    Result := TextToShortCut(Text);
end;

procedure TGXHotKey.SetHotKey(Value: TShortCut);
begin
  if csDesigning in ComponentState then
    FHotKey := Value
  else
  begin
    if FHotKey <> Value then
    begin
      FHotKey := Value;
      ItemIndex := Items.IndexOf(ShortCutToText(FHotKey));
    end;
  end;
end;

procedure TGXHotKey.RefreshList;
begin
  GetShortCutList(Items, True);
  ItemIndex := Items.IndexOf(ShortCutToText(FHotKey));
end;

initialization

finalization
  FreeAndNil(PrivateShortCutList);

{$ELSE}

procedure TGXHotKey.RefreshList;
begin
  // Nothing under Windows
end;

{$ENDIF}

end.

