unit GXCheckListbox;

interface

uses
  Windows, Messages, Classes, Controls, StdCtrls, Graphics;

type
  TGXCheckListbox = class(TListBox)
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetItemChecked(Index: Integer): Boolean;
    procedure SetItemChecked(Index: Integer; Value: Boolean);
    procedure ToggleItemChecked(Index: Integer);
  end;

procedure Register;

implementation

const
  CheckLeftGap = 1;
  CheckTopGap = 2;
  CheckSize = 13;
  CheckTextGap = 3;

procedure Register;
begin
  RegisterComponents('GExperts', [TGXCheckListbox]);
end;

constructor TGXCheckListbox.Create(AOwner: TComponent);
begin
  inherited;
  ItemHeight := 17; // Use the font height here?
  Style := lbOwnerDrawFixed;
end;

procedure TGXCheckListbox.DrawItem (Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  CheckPos : TRect;
begin
  Canvas.FillRect(Rect);
  CheckPos.Top := Rect.Top + CheckTopGap;
  CheckPos.Left := Rect.Left + CheckLeftGap;
  CheckPos.Bottom := CheckPos.Top + CheckSize;
  CheckPos.Right := CheckPos.Left + CheckSize;

  if GetItemChecked(Index) then
    DrawFrameControl(Canvas.Handle, CheckPos, DFC_BUTTON, DFCS_BUTTONCHECK + DFCS_Checked)
  else
    DrawFrameControl(Canvas.Handle, CheckPos, DFC_BUTTON, DFCS_BUTTONCHECK);

  Canvas.TextOut(CheckPos.Right + CheckTextGap, CheckPos.Top, Items[Index]);
end;

function TGXCheckListbox.GetItemChecked(Index: Integer): Boolean;
begin
  Result := Boolean(Items.Objects[Index]);
end;

procedure TGXCheckListbox.KeyPress(var Key: Char);
begin
  inherited;
  if (ItemIndex > -1) then
    if Key = #32 then
      ToggleItemChecked(ItemIndex);
end;

procedure TGXCheckListbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickIndex: Integer;
begin
  inherited;
  if (X > CheckLeftGap) and (X < CheckLeftGap + CheckSize) then
  begin
    ClickIndex := ItemAtPos(Point(X, Y), True);
    if ClickIndex > -1 then
      ToggleItemChecked(ClickIndex);
  end;
end;

procedure TGXCheckListbox.SetItemChecked(Index: Integer; Value: Boolean);
begin
  Items.Objects[Index] := TObject(Value);
  Repaint;
end;

procedure TGXCheckListbox.ToggleItemChecked(Index: Integer);
begin
  SetItemChecked(Index, not GetItemChecked(Index));
end;

end.

