{***************************************************************
 * Unit Name: GX_Bookmarks
 * Authors  : Thomas Mueller http://blog.dummzeuch.de
 ****************************************************************}

unit GX_Bookmarks;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, GX_BaseForm,
  GX_IdeDock, Vcl.ComCtrls, ToolsAPI;

type
  TfmGxBookmarksForm = class(TfmIdeDockForm)
    lv_Bookmarks: TListView;
  private
    function GetEditView(var _SourceEditor: IOTASourceEditor;
      var _EditView: IOTAEditView): boolean;
    procedure Init;
  public
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, Menus, GX_GExperts, GX_ConfigurationInfo, GX_OtaUtils,
  GX_GenericUtils, GX_OTAEditorNotifier;

type
  TGxBookmarksExpert = class;

  TGxBookmarksNotifier = class(TGxOTAEditorNotifier, IGxOTAEditorNotifier, IOTAEditorNotifier, IOTANotifier)
  private
    FClient: TGxBookmarksExpert;
  private
    procedure ViewActivated(const View: IOTAEditView); override;
  public
    constructor Create(const _Client: TGxBookmarksExpert; _Editor: IOTASourceEditor);
    destructor Destroy; override;
  end;

  TGxBookmarksExpert = class(TGX_Expert)
  private
    fmGxBookmarksForm: TfmGxBookmarksForm;
    FSomeData: string;
    FNotifier: IGxOtaEditorNotifier;
  protected
    procedure SetActive(New: Boolean); override;
    procedure AddNotifier;
    procedure RemoveNotifier;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Configure; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure Click(Sender: TObject); override;
  end;

{ TGxBookmarksExpert }

procedure TGxBookmarksExpert.Click(Sender: TObject);
begin
  fmGxBookmarksForm.Init;
  IdeDockManager.ShowForm(fmGxBookmarksForm);
  EnsureFormVisible(fmGxBookmarksForm);
end;

procedure TGxBookmarksExpert.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button!';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

constructor TGxBookmarksExpert.Create;
begin
  inherited Create;

  FNotifier := nil;

  fmGxBookmarksForm := TfmGxBookmarksForm.Create(nil);
  IdeDockManager.RegisterDockableForm(TfmGxBookmarksForm, fmGxBookmarksForm, 'fmGxBookmarksForm');
end;

{ TfmGxBookmarksForm }

procedure TfmGxBookmarksForm.Init;
var
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
  i: Integer;
  BmPos: TOTACharPos;
  li: TListItem;
begin
  lv_Bookmarks.Clear;
  if not GetEditView(SourceEditor, EditView) then
    Exit;
  for i := 0 to 19 do begin
    BmPos := EditView.BookmarkPos[i];
    if BmPos.Line <> 0 then begin
      li := lv_Bookmarks.Items.Add;
      li.Caption :=     SourceEditor.FileName;
      li.SubItems.Add(IntToStr(BmPos.Line));
    end;
  end;
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

destructor TGxBookmarksExpert.Destroy;
begin
  if Assigned(fmGxBookmarksForm) then begin
    IdeDockManager.UnRegisterDockableForm(fmGxBookmarksForm, 'fmGxBookmarksForm');
    fmGxBookmarksForm.Free;
  end;

  inherited Destroy;
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
  Result := True;
end;

function TGxBookmarksExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxBookmarksExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FSomeData := Settings.ReadString(ConfigurationKey, 'Bookmarks', FSomeData);
end;

procedure TGxBookmarksExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  Settings.WriteString(ConfigurationKey, 'Bookmarks', FSomeData);
end;

procedure TGxBookmarksExpert.AddNotifier;
var
  Editor: IOTASourceEditor;
begin
  if not Assigned(FNotifier) then begin
    Editor := GxOtaGetCurrentSourceEditor;
    if Assigned(Editor) then begin
      FNotifier := TGxBookmarksNotifier.Create(Self, Editor);
    end;
  end;
end;

procedure TGxBookmarksExpert.RemoveNotifier;
begin
  if Assigned(FNotifier) then begin
    FNotifier.Detach;
    FNotifier := nil;
  end;
end;

procedure TGxBookmarksExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      AddNotifier
    else
      RemoveNotifier;
  end;
end;

{ TGxBookmarksNotifier }

constructor TGxBookmarksNotifier.Create(const _Client: TGxBookmarksExpert;
  _Editor: IOTASourceEditor);
begin
  inherited Create;
  FClient := _Client;
  Attach(_Editor);
end;

destructor TGxBookmarksNotifier.Destroy;
begin
  Detach;
  inherited;
  FClient := nil;
end;

procedure TGxBookmarksNotifier.ViewActivated(const View: IOTAEditView);
begin
  inherited;

end;

initialization
  RegisterGX_Expert(TGxBookmarksExpert);
end.
