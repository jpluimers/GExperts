unit OwnerList;

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  OwnerList: list class that owns the objects in the list.
  Use TOwnerList instead of TList when the list contains the
  sole references to the objects. Delete an object from the list,
  and the owner list automatically frees the object. Free the list,
  and it frees every object in the list.
}

interface

uses Classes;

{ List class that owns its items. }
type
  TOwnerList = class
  private
    fList: TList;
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; Item: TObject);
    function GetCount: Integer;
  protected
    property Items[Index: Integer]: TObject read GetItem write SetItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(Item: TObject): Integer; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    function IndexOf(Item: TObject): Integer; virtual;
    function Remove(Item: TObject): Integer;
    property Count: Integer read GetCount;
  end;

implementation

{ This class is similar to TList, except it owns the items in the
  list. Deleting an item deletes the object. Clearing the list deletes
  all the objects. }
constructor TOwnerList.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TOwnerList.Destroy;
begin
  if fList <> nil then
    Clear;
  fList.Free;
  inherited Destroy;
end;

function TOwnerList.GetItem(Index: Integer): TObject;
begin
  Result := fList[Index]
end;

procedure TOwnerList.SetItem(Index: Integer; Item: TObject);
var
  OldItem: TObject;
begin
  OldItem := fList[Index];
  if OldItem <> Item then
  begin
    fList[Index] := nil;
    OldItem.Free;
  end;
  fList[Index] := Item;
end;

function TOwnerList.GetCount: Integer;
begin
  Result := fList.Count
end;

function TOwnerList.Add(Item: TObject): Integer;
begin
  Result := fList.Add(Item)
end;

{ Delete items starting at the end of the list because that is
  much faster than starting at the beginning of the list. }
procedure TOwnerList.Clear;
var
  I: Integer;
begin
  for I := fList.Count-1 downto 0 do
    Delete(I);
end;

procedure TOwnerList.Delete(Index: Integer);
var
  Item: TObject;
begin
  Item := fList[Index];
  fList.Delete(Index);
  Item.Free;
end;

function TOwnerList.IndexOf(Item: TObject): Integer;
begin
  Result := fList.IndexOf(Item);
end;

function TOwnerList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

end.
