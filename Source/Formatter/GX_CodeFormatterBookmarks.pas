// stores the bookmarks while the source file is being reformatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterBookmarks;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  ToolsApi,
  GX_BookmarkList;

type
  TBookmarkHandler = class
  private
    FBookmarks: TBookmarkList;
  protected
    function GetEditView(var _SourceEditor: IOTASourceEditor; var _EditView: IOTAEditView): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RestoreItems;
    procedure SaveItems;
  end;

implementation

uses
  GX_OtaUtils;

{ TBookmarkHandler }

constructor TBookmarkHandler.Create;
begin
  inherited Create;
  FBookmarks := TBookmarkList.Create;
end;

destructor TBookmarkHandler.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

function TBookmarkHandler.GetEditView(var _SourceEditor: IOTASourceEditor; var _EditView: IOTAEditView): Boolean;
begin
  Result := False;
  if not GxOtaTryGetCurrentSourceEditor(_SourceEditor) then
    Exit;
  _EditView := _SourceEditor.GetEditView(0);
  Result := Assigned(_EditView);
end;

procedure TBookmarkHandler.SaveItems;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  BmPos: TOTACharPos;
begin
  FBookmarks.Clear;
  if not GetEditView(SourceEditor, EditView) then
    Exit;
  for i := 0 to 19 do begin
    BmPos := EditView.BookmarkPos[i];
    if BmPos.Line <> 0 then
      FBookmarks.Add(i, BmPos.Line, BmPos.CharIndex);
  end;
end;

procedure TBookmarkHandler.RestoreItems;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  SaveCursorPos: TOTAEditPos;
  BmEditPos: TOTAEditPos;
  i: Integer;
begin
  if not GetEditView(SourceEditor, EditView) then
    Exit;
  SaveCursorPos := EditView.GetCursorPos;
  try
    for i := 0 to FBookmarks.Count - 1 do begin
      BmEditPos.Line := FBookmarks[i].Line;
      BmEditPos.Col := FBookmarks[i].CharIdx;
      EditView.SetCursorPos(BmEditPos);
      EditView.BookmarkToggle(FBookmarks[i].Number);
    end;
  finally
    EditView.SetCursorPos(SaveCursorPos);
  end;
  EditView.Paint;
  SourceEditor.Show;
  FBookmarks.Clear;
end;

end.
