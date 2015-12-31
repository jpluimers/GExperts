{***************************************************************
 * Unit Name: GX_Bookmarks
 * Authors  : Thomas Mueller http://blog.dummzeuch.de
 ****************************************************************}

unit GX_Bookmarks;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ToolsAPI,
  ExtCtrls,
  Menus,
  GX_Experts,
  GX_BaseForm,
  GX_IdeDock,
  GX_BookmarkList;

type
  TfmGxBookmarksForm = class(TfmIdeDockForm)
    lv_Bookmarks: TListView;
    tim_Update: TTimer;
    pm_Bookmarks: TPopupMenu;
    mi_Delete: TMenuItem;
    mi_Add: TMenuItem;
    mi_Edit: TMenuItem;
    procedure lv_BookmarksDblClick(Sender: TObject);
    procedure tim_UpdateTimer(Sender: TObject);
    procedure mi_DeleteClick(Sender: TObject);
    procedure mi_AddClick(Sender: TObject);
    procedure mi_EditClick(Sender: TObject);
  private
    function GetEditView(var _SourceEditor: IOTASourceEditor;
      var _EditView: IOTAEditView): Boolean;
    procedure Init;
    procedure SetBookmark(const _ModuleName: string; _LineNo: Integer; _BmIdx: Integer = -1);
    procedure DeleteBookmark(const _ModuleName: string; _BmIdx: Integer);
    procedure AddBookmarks(const _ModuleName: string; _EditView: IOTAEditView; _Bookmarks: TBookmarkList);
    function HasChanged(_Bookmarks: TBookmarkList; _Items: TListItems): Boolean;
  public
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Registry,
  GX_GExperts,
  GX_ConfigurationInfo,
  GX_OtaUtils,
  GX_GenericUtils,
  GX_NTAEditServiceNotifier,
  GX_dzVclUtils,
  GX_EditBookmark;

type
  ///<summary>
  /// We implement INTAEditServicesNotifier only to get a notification when the EditViewActivated
  /// method is called. This in turn calls the OnEditorViewActivated event. </summary>
  // todo -otwm -cCleanup: Merge this code with the duplicate in GX_HideNavbar
  TEditServiceNotifier = class(TGxNTAEditServiceNotifier, INTAEditServicesNotifier)
  private
    type
      TOnEditorViewActivatedEvent = procedure(_Sender: TObject; _EditView: IOTAEditView) of object;
    var
      FOnEditorViewActivated: TOnEditorViewActivatedEvent;
  protected // INTAEditServicesNotifier
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); override;
  public
    constructor Create(_OnEditorViewActivated: TOnEditorViewActivatedEvent);
  end;

type
  TGxBookmarksExpert = class(TGX_Expert)
  private
    fmGxBookmarksForm: TfmGxBookmarksForm;
    FNotifierIdx: Integer;
    procedure EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Click(Sender: TObject); override;
  end;

{ TGxBookmarksExpert }

constructor TGxBookmarksExpert.Create;
begin
  inherited Create;

  fmGxBookmarksForm := TfmGxBookmarksForm.Create(nil);
  IdeDockManager.RegisterDockableForm(TfmGxBookmarksForm, fmGxBookmarksForm, 'fmGxBookmarksForm');

  if Assigned(BorlandIDEServices) then begin
    FNotifierIdx := (BorlandIDEServices as IOTAEditorServices).AddNotifier(
      TEditServiceNotifier.Create(EditorViewActivated));
  end;
end;

destructor TGxBookmarksExpert.Destroy;
begin
  if FNotifierIdx <> 0 then
    (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);

  if Assigned(fmGxBookmarksForm) then begin
    IdeDockManager.UnRegisterDockableForm(fmGxBookmarksForm, 'fmGxBookmarksForm');
    fmGxBookmarksForm.Free;
  end;

  inherited Destroy;
end;

procedure TGxBookmarksExpert.Click(Sender: TObject);
begin
  fmGxBookmarksForm.Init;
  IdeDockManager.ShowForm(fmGxBookmarksForm);
  EnsureFormVisible(fmGxBookmarksForm);
end;

procedure TGxBookmarksExpert.EditorViewActivated(_Sender: TObject; _EditView: IOTAEditView);
begin
  if Assigned(fmGxBookmarksForm) then
    fmGxBookmarksForm.Init;
end;

{ TfmGxBookmarksForm }

destructor TfmGxBookmarksForm.Destroy;
begin
  TListView_ClearWithObjects(lv_Bookmarks);
  inherited;
end;

function TfmGxBookmarksForm.GetEditView(var _SourceEditor: IOTASourceEditor;
  var _EditView: IOTAEditView): Boolean;
begin
  Result := False;
  _SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(_SourceEditor) then
    Exit;
  _EditView := _SourceEditor.GetEditView(0);
  Result := Assigned(_EditView);
end;

procedure TfmGxBookmarksForm.AddBookmarks(const _ModuleName: string; _EditView: IOTAEditView;
  _Bookmarks: TBookmarkList);
var
  i: Integer;
  BmPos: TOTACharPos;
begin
  for i := 0 to 19 do begin
    BmPos := _EditView.BookmarkPos[i];
    if BmPos.Line <> 0 then begin
      _Bookmarks.Add(i, BmPos.Line, BmPos.CharIndex, _ModuleName);
    end;
  end;
end;

function TfmGxBookmarksForm.HasChanged(_Bookmarks: TBookmarkList; _Items: TListItems): Boolean;
var
  i: Integer;
  bm1: TBookmark;
  bm2: Pointer;
begin
  Result := True;
  if _Bookmarks.Count <> _Items.Count then
    Exit;

  for i := 0 to _Bookmarks.Count - 1 do begin
    bm1 := _Bookmarks[i];
    bm2 := _Items[i].Data;
    if not bm1.IsSame(bm2) then
      Exit;
  end;

  Result := False;
end;

procedure TfmGxBookmarksForm.Init;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  bm: TBookmark;
  li: TListItem;
  Items: TListItems;
  Bookmarks: TBookmarkList;
begin
  tim_Update.Enabled := False;
  try
    if not GetEditView(SourceEditor, EditView) then
      Exit;
    Bookmarks := TBookmarkList.Create;
    try
      AddBookmarks(SourceEditor.Filename, EditView, Bookmarks);

      Items := lv_Bookmarks.Items;
      if HasChanged(Bookmarks, Items) then begin

        Items.BeginUpdate;
        try
          TListItems_ClearWithObjects(Items);
          for i := 0 to Bookmarks.Count - 1 do begin
            bm := Bookmarks[i];
            li := Items.Add;
            li.Data := TBookmark.Create(bm);
            li.Caption := IntToStr(bm.Number);
            li.SubItems.Add(ExtractFileName(bm.Module));
            li.SubItems.Add(IntToStr(bm.Line));
          end;
          TListView_Resize(lv_Bookmarks);
        finally
          Items.EndUpdate;
        end;
      end;
    finally
      FreeAndNil(Bookmarks);
    end;
  finally
    tim_Update.Enabled := True;
  end;
end;

procedure TfmGxBookmarksForm.tim_UpdateTimer(Sender: TObject);
begin
  Init;
end;

procedure TfmGxBookmarksForm.lv_BookmarksDblClick(Sender: TObject);
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
var
  li: TListItem;
  bmi: TBookmark;
  fn: string;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
begin
  if not TListView_TryGetSelected(lv_Bookmarks, li) then
    Exit;
  bmi := li.Data;
  fn := bmi.Module;

  if not GxOtaMakeSourceVisible(fn) then
    raise Exception.CreateFmt(SCouldNotOpenFile, [fn]);

  Module := GxOtaGetModule(fn);
  if not Assigned(Module) then
    Exit;

  SourceEditor := GxOtaGetSourceEditorFromModule(Module, fn);
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;

  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;

  EditView.BookmarkGoto(bmi.Number);
  EditView.MoveViewToCursor;
  GxOtaFocusCurrentIDEEditControl;
  EditView.Paint;
end;

function TGxBookmarksExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Bookmarks';
begin
  Result := SMenuCaption;
end;

class function TGxBookmarksExpert.GetName: string;
begin
  Result := 'BookmarksExpert';
end;

function TGxBookmarksExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGxBookmarksExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

{ TEditServiceNotifier }

constructor TEditServiceNotifier.Create(_OnEditorViewActivated: TOnEditorViewActivatedEvent);
begin
  inherited Create;
  FOnEditorViewActivated := _OnEditorViewActivated;
end;

procedure TEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  if Assigned(FOnEditorViewActivated) then
    FOnEditorViewActivated(Self, EditView);
end;

function TryGetEditView(const _fn: string; out _EditView: IOTAEditView): Boolean;
var
  SourceEditor: IOTASourceEditor;
begin
  SourceEditor := GxOtaGetSourceEditor(_fn);
  Result := Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0);
  if Result then
    _EditView := SourceEditor.EditViews[0];
end;

procedure TfmGxBookmarksForm.DeleteBookmark(const _ModuleName: string; _BmIdx: Integer);
var
  EditView: IOTAEditView;
  SaveCursorPos: TOTAEditPos;
  BmEditPos: TOTAEditPos;
begin
  if not TryGetEditView(_ModuleName, EditView) then
    Exit;

  if EditView.BookmarkPos[_BmIdx].Line <> 0 then begin
    SaveCursorPos := EditView.GetCursorPos;
    try
      BmEditPos.Line := EditView.BookmarkPos[_BmIdx].Line;
      BmEditPos.Col := EditView.BookmarkPos[_BmIdx].CharIndex;
      EditView.SetCursorPos(BmEditPos);
      EditView.BookmarkToggle(_BmIdx);
    finally
      EditView.SetCursorPos(SaveCursorPos);
    end;
  end;
end;

procedure TfmGxBookmarksForm.SetBookmark(const _ModuleName: string; _LineNo: Integer; _BmIdx: Integer = -1);
var
  EditView: IOTAEditView;
  i: Integer;
  SaveCursorPos: TOTAEditPos;
  BmEditPos: TOTAEditPos;
begin
  if not TryGetEditView(_ModuleName, EditView) then
    Exit;

  if _BmIdx = -1 then begin
    // no bookmark index was given, find the first one that's free
    for i := 0 to 19 do begin
      if EditView.BookmarkPos[i].Line = 0 then begin
        _BmIdx := i;
        break;
      end;
    end;
    if _BmIdx = -1 then
      Exit; // no free bookmark index found
  end;

  SaveCursorPos := EditView.GetCursorPos;
  try
    BmEditPos.Line := _LineNo;
    BmEditPos.Col := 1;
    EditView.SetCursorPos(BmEditPos);
    EditView.BookmarkRecord(_BmIdx);
  finally
    EditView.SetCursorPos(SaveCursorPos);
  end;
end;

procedure TfmGxBookmarksForm.mi_AddClick(Sender: TObject);
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  ModuleName: string;
  LineNo: Integer;
  BmIndex: Integer;
begin
  if not GetEditView(SourceEditor, EditView) then
    Exit;

  try
    ModuleName := SourceEditor.Filename;
    LineNo := EditView.CursorPos.Line;
    if not TfmEditBookmarks.Execute(Self, ModuleName, LineNo, BmIndex) then
      Exit;

    SetBookmark(ModuleName, LineNo, BmIndex);
  finally
    Init;
  end;
end;

procedure TfmGxBookmarksForm.mi_DeleteClick(Sender: TObject);
var
  li: TListItem;
  bmi: TBookmark;
begin
  if not TListView_TryGetSelected(lv_Bookmarks, li) then
    Exit;
  bmi := li.Data;
  DeleteBookmark(bmi.Module, bmi.Number);
end;

procedure TfmGxBookmarksForm.mi_EditClick(Sender: TObject);
var
  li: TListItem;
  bmi: TBookmark;
  ModuleName: string;
  LineNo: Integer;
begin
  if not TListView_TryGetSelected(lv_Bookmarks, li) then
    Exit;

  bmi := li.Data;
  try
    ModuleName := bmi.Module;
    LineNo := bmi.Line;
    if not TfmEditBookmarks.Execute(Self, ModuleName, LineNo) then
      Exit;

    SetBookmark(ModuleName, LineNo, bmi.Number);
  finally
    Init;
  end;
end;

initialization
  RegisterGX_Expert(TGxBookmarksExpert);
end.
