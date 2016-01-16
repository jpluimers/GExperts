unit GX_CodeFormatterBookmarks;

// stores the bookmarks while the source file is being reformatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)

interface

uses
  SysUtils,
  Classes;

type
  TBookmark = class
  private
    fLine: Integer;
    fNumber: Integer;
  public
    constructor Create(_Number, _Line: Integer);
    property Number: Integer read fNumber;
    property Line: Integer read fLine;
  end;

  TBookmarks = class
  private
    function GetCount: Integer;
    function GetItems(_Idx: Integer): TBookmark;
  protected
    fList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(_Number, _Line: Integer);
    procedure Clear;
    property Items[_Idx: Integer]: TBookmark read GetItems; default;
    property Count: Integer read GetCount;
  end;

  TBookmarkHandler = class
  private
    FBookmarks: TBookmarks;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveBookmarks;
    procedure RestoreBookmarks;
  end;

implementation

uses
  ToolsApi;

{ TBookmark }

constructor TBookmark.Create(_Number, _Line: Integer);
begin
  inherited Create;
  fNumber := _Number;
  fLine := _Line;
end;

{ TBookmarks }

constructor TBookmarks.Create;
begin
  inherited;
  fList := TList.Create;
end;

destructor TBookmarks.Destroy;
var
  i: Integer;
begin
  if Assigned(fList) then
    begin
      for i := 0 to fList.Count - 1 do
        TBookmark(fList[i]).Free;
      fList.Free;
      fList := nil;
    end;
  inherited;
end;

procedure TBookmarks.Add(_Number, _Line: Integer);
begin
  fList.Add(TBookmark.Create(_Number, _Line))
end;

function TBookmarks.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TBookmarks.GetItems(_Idx: Integer): TBookmark;
begin
  Result := fList[_Idx];
end;

procedure TBookmarks.Clear;
var
  i: Integer;
begin
  for i := 0 to fList.Count - 1 do
    TBookmark(fList[i]).Free;
  fList.Clear;
end;

{ TBookmarkHandler }

constructor TBookmarkHandler.Create;
begin
  inherited Create;
  FBookmarks := TBookmarks.Create;
end;

destructor TBookmarkHandler.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

procedure TBookmarkHandler.SaveBookmarks;
var
  EditorService: IOTAEditorServices;
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  BmPos: TOTACharPos;
begin
  FBookmarks.Clear;
  if BorlandIDEServices.QueryInterface(IOTAEditorServices, EditorService) <> S_OK then
    Exit;
  SourceEditor := EditorService.GetTopBuffer;
  if not Assigned(SourceEditor) then
    Exit;
  EditView := SourceEditor.GetEditView(0);
  if not Assigned(EditView) then
    Exit;
  for i := 0 to 19 do
    begin
      BmPos := EditView.BookmarkPos[i];
      if BmPos.Line <> 0 then
        FBookmarks.Add(i, BmPos.Line);
    end;
end;

procedure TBookmarkHandler.RestoreBookmarks;
var
  EditorService: IOTAEditorServices;
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  SaveCursorPos: TOTAEditPos;
  BmEditPos: TOTAEditPos;
  i: Integer;
begin
  if not BorlandIDEServices.QueryInterface(IOTAEditorServices, EditorService) = S_OK then
    Exit;
  SourceEditor := EditorService.GetTopBuffer;
  if not Assigned(SourceEditor) then
    Exit;
  EditView := SourceEditor.GetEditView(0);
  if not Assigned(EditView) then
    Exit;
  SaveCursorPos := EditView.GetCursorPos;
  try
    for i := 0 to FBookmarks.Count - 1 do
      begin
        BmEditPos.Line := FBookmarks[i].Line;
        BmEditPos.Col := 1;
        EditView.SetCursorPos(BmEditPos);
        EditView.BookmarkToggle(FBookmarks[i].Number);
      end;
  finally
    EditView.SetCursorPos(SaveCursorPos);
  end;
  EditView.Paint;
  SourceEditor.show;
  FBookmarks.Clear;
end;

end.
