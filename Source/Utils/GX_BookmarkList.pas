unit GX_BookmarkList;

interface

uses
  Classes;

type
  TBookmark = class
  private
    FLine: Integer;
    FNumber: Integer;
    FCharIdx: integer;
    FModule: string;
  public
    constructor Create(_Number, _Line, _CharIdx: Integer; const _Module: string);
    property Number: Integer read FNumber;
    property Line: Integer read FLine;
    property CharIdx: integer read FCharIdx;
    property Module: string read FModule;
  end;

type
  TBookmarkList = class
  private
    function GetCount: Integer;
    function GetItems(_Idx: Integer): TBookmark;
  protected
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(_Number, _Line, _CharIdx: Integer; const _Module: string = '');
    procedure Clear;
    property Items[_Idx: Integer]: TBookmark read GetItems; default;
    property Count: Integer read GetCount;
  end;

implementation

{ TBookmark }

constructor TBookmark.Create(_Number, _Line, _CharIdx: Integer; const _Module: string);
begin
  inherited Create;
  FNumber := _Number;
  FLine := _Line;
  FCharIdx := _CharIdx;
  FModule := '';
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

procedure TBookmarkList.Add(_Number, _Line, _CharIdx: Integer; const _Module: string);
begin
  FList.Add(TBookmark.Create(_Number, _Line, _CharIdx, _Module))
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
