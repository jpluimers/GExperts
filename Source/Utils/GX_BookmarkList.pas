unit GX_BookmarkList;

interface

uses
  SysUtils,
  Classes;

type
  TBookmark = class
  private
    FLine: Integer;
    FNumber: Integer;
    FCharIdx: Integer;
    FModule: string;
  public
    constructor Create(_Number, _Line, _CharIdx: Integer; const _Module: string); overload;
    constructor Create(_bm: TBookmark); overload;
    function IsSame(_bm: TBookmark): Boolean;
    property Number: Integer read FNumber;
    property Line: Integer read FLine;
    property CharIdx: Integer read FCharIdx;
    property Module: string read FModule;
  end;

type
  TBookmarkList = class
  private
    function GetCount: Integer;
    function GetItems(_Idx: Integer): TBookmark;
    function CompareBookmarks(_Item: Pointer; _Idx: Integer): Integer;
  protected
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(_Number, _Line, _CharIdx: Integer; const _Module: string = ''): Boolean;
    procedure Clear;
    property Items[_Idx: Integer]: TBookmark read GetItems; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  GX_dzQuicksort;

{ TBookmark }

constructor TBookmark.Create(_Number, _Line, _CharIdx: Integer; const _Module: string);
begin
  inherited Create;
  FNumber := _Number;
  FLine := _Line;
  FCharIdx := _CharIdx;
  FModule := _Module;
end;

constructor TBookmark.Create(_bm: TBookmark);
begin
  Create(_bm.Number, _bm.Line, _bm.CharIdx, _bm.Module);
end;

function TBookmark.IsSame(_bm: TBookmark): Boolean;
begin
  Result := (Line = _bm.Line)
    and (CharIdx = _bm.CharIdx)
    and (Number = _bm.Number)
    and (Module = _bm.Module);
end;

{ TBookmarkList }

constructor TBookmarkList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TBookmarkList.Destroy;
var
  i: Integer;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do
      TBookmark(FList[i]).Free;
    FList.Free;
    FList := nil;
  end;
  inherited;
end;

function TBookmarkList.CompareBookmarks(_Item: Pointer; _Idx: Integer): Integer;
var
  Item1: TBookmark;
  Item2: TBookmark;
begin
  Item1 := _Item;
  Item2 := Items[_Idx];

  Result := CompareText(Item1.FModule, Item2.FModule);
  if Result = 0 then
    Result := Item1.FNumber - Item2.FNumber;
end;

function TBookmarkList.Add(_Number, _Line, _CharIdx: Integer; const _Module: string): Boolean;
var
  bm: TBookmark;
  idx: Integer;
begin
  bm := TBookmark.Create(_Number, _Line, _CharIdx, _Module);
  if BinarySearch(0, FList.Count - 1, idx, bm, CompareBookmarks) then begin
    bm.Free;
    Result := False;
  end else begin
    FList.Insert(idx, bm);
    Result := True;
  end;
end;

function TBookmarkList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBookmarkList.GetItems(_Idx: Integer): TBookmark;
begin
  Result := FList[_Idx];
end;

procedure TBookmarkList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TBookmark(FList[i]).Free;
  FList.Clear;
end;

end.
