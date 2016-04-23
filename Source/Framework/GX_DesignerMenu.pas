unit GX_DesignerMenu;

{$I GX_CondDefine.inc}

interface

uses
  Classes, DesignIntf, DesignMenus;

type
  TGxDesignerMenu = class(TBaseSelectionEditor, ISelectionEditor)
    Indexes: array of Integer;

    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections);
    function GetVerb(Index: Integer): string;
    function GetVerbCount: Integer;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem);
    procedure RequiresUnits(Proc: TGetStrProc);
  end;

implementation

uses GX_GExperts, GX_Experts;

{ TGxSelectionEditor }

procedure TGxDesignerMenu.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  GExpertsInst.ExpertList[Indexes[Index]].Execute(Self);
end;

function TGxDesignerMenu.GetVerb(Index: Integer): string;
begin
  Result := GExpertsInst.ExpertList[Indexes[Index]].GetActionCaption;
end;

function TGxDesignerMenu.GetVerbCount: Integer;
var
  i: Integer;
  Expert: TGX_Expert;
begin
  Result := 0;
  SetLength(Indexes, GExpertsInst.ExpertCount);

  for i := 0 to GExpertsInst.ExpertCount - 1 do
  begin
    Expert := GExpertsInst.ExpertList[i];
    if Expert.Active and Expert.HasDesignerMenuItem then
    begin
      Indexes[Result] := i;
      Inc(Result);
    end;
  end;
end;

procedure TGxDesignerMenu.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  GExpertsInst.ExpertList[Indexes[Index]].DoUpdateAction;
  AItem.Visible := GExpertsInst.ExpertList[Indexes[Index]].GetActionEnabled;
end;

procedure TGxDesignerMenu.RequiresUnits(Proc: TGetStrProc);
begin // FI:W519
  // Nothing
end;

initialization
  RegisterSelectionEditor(TComponent, TGxDesignerMenu);

end.
