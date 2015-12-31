{***************************************************************
 * Unit Name: GX_Bookmarks
 * Authors  : Thomas Mueller http://blog.dummzeuch.de
 ****************************************************************}

unit GX_Bookmarks;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ToolsAPI, ExtCtrls,
  GX_BaseForm, GX_IdeDock;

type
  TfmGxBookmarksForm = class(TfmIdeDockForm)
    lv_Bookmarks: TListView;
    tim_Update: TTimer;
    procedure lv_BookmarksDblClick(Sender: TObject);
    procedure tim_UpdateTimer(Sender: TObject);
  private
    function GetEditView(var _SourceEditor: IOTASourceEditor;
      var _EditView: IOTAEditView): boolean;
    procedure Init;
  public
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, Menus, GX_GExperts, GX_ConfigurationInfo, GX_OtaUtils,
  GX_GenericUtils, GX_NTAEditServiceNotifier, GX_dzVclUtils;

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

type
  TBookmarkInfo = class
  private
    FBmIndex: integer;
    FModuleName: string;
    FLineNo: integer;
  public
    constructor Create(_BmIndex: integer; const _ModuleName: string; _LineNo: integer);
  end;

{ TfmGxBookmarksForm }

destructor TfmGxBookmarksForm.Destroy;
begin
  TListView_ClearWithObjects(lv_Bookmarks);
  inherited;
end;

function TfmGxBookmarksForm.GetEditView(var _SourceEditor: IOTASourceEditor; var _EditView: IOTAEditView): boolean;
begin
  Result := False;
  _SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(_SourceEditor) then
    Exit;
  _EditView := _SourceEditor.GetEditView(0);
  Result := Assigned(_EditView);
end;

procedure TfmGxBookmarksForm.Init;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  BmPos: TOTACharPos;
  li: TListItem;
  Items: TListItems;
begin
  Items := lv_Bookmarks.Items;
  Items.BeginUpdate;
  try
    TListItems_ClearWithObjects(Items);
    if not GetEditView(SourceEditor, EditView) then
      Exit;
    for i := 0 to 19 do begin
      BmPos := EditView.BookmarkPos[i];
      if BmPos.Line <> 0 then begin
        li := Items.Add;
        li.Data := TBookmarkInfo.Create(i, SourceEditor.FileName, BmPos.Line);
        li.Caption := IntToStr(i);
        li.SubItems.Add(ExtractFilename(SourceEditor.FileName));
        li.SubItems.Add(IntToStr(BmPos.Line));
      end;
    end;
    TListView_Resize(lv_Bookmarks);
  finally
    Items.EndUpdate;
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
  bmi: TBookmarkInfo;
  fn: string;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
begin
  if not TListView_TryGetSelected(lv_Bookmarks, li) then
    Exit;
  bmi := li.Data;
  fn := bmi.FModuleName;

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

  EditView.BookmarkGoto(bmi.FBmIndex);
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

{ TBookmarkInfo }

constructor TBookmarkInfo.Create(_BmIndex: integer; const _ModuleName: string; _LineNo: integer);
begin
  inherited Create;
  FBmIndex:= _BmIndex;
  FModuleName:=_ModuleName;
  FLineNo := _LineNo;
end;

initialization
  RegisterGX_Expert(TGxBookmarksExpert);
end.
