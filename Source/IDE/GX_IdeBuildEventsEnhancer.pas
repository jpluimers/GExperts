unit GX_IdeBuildEventsEnhancer;

{$I GX_CondDefine.inc}

{$UNDEF GX_EnableBuildEventsEnhancer}
{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
{$DEFINE GX_EnableBuildEventsEnhancer}
{$ENDIF}

interface

uses
  SysUtils,
  Classes;

type
  TGxIdeBuildEventsEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

{$IFDEF GX_EnableBuildEventsEnhancer}

{$UNDEF HAS_OLD_STYLE_PAGE}
{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
{$IFNDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
{$DEFINE HAS_OLD_STYLE_PAGE}
{$ENDIF}{$ENDIF}

uses
  Controls,
  Menus,
  StdCtrls,
  Forms,
  Dialogs,
  Types,
  ComCtrls,
  ExtCtrls,
  GX_IdeProjectOptionsEnhancer,
  GX_dzVclUtils,
  GX_IdeFormEnhancer,
  GX_IdeFavoritesList,
  GX_ConfigurationInfo,
  GX_IdeBuildEventFavoriteEdit,
  GX_dzClassUtils,
  GX_IdeDialogEnhancer;

type
  TFavHandler = class;

  TBuildEventsEnhancer = class(TIdeDialogEnhancer)
  private
    FFavorites: TStringList;

    procedure FavoritesPmConfigure(_Owner: TFavHandler);
    procedure LoadSettings;
    procedure SaveSettings;
    function ConfigurationKey: string;
    procedure EditEntry(_Sender: TWinControl; var _Name, _Value: string; var _OK: Boolean);
{$IFDEF HAS_OLD_STYLE_PAGE}
    procedure InitBuildEvent(_Form: TForm; const _BtnName, _MemoName: string);
    procedure InitProjectOptions(_Form: TForm);
{$ENDIF HAS_OLD_STYLE_PAGE}
    function IsBuildEventsForm(_Form: TCustomForm): Boolean;
    procedure InitFavoritesMenu(_Owner: TFavHandler; _pm: TPopupMenu);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFavHandler = class(TComponent)
  private
    FEnhancer: TBuildEventsEnhancer;
    FMemo: TMemo;
    FFavBtn: TButton;
    FFavoritesPm: TPopupMenu;
    procedure FavoritesBtnClick(_Sender: TObject);
    procedure HandleFavoriteClick(_Sender: TObject);
    procedure ConfigureClick(_Sender: TObject);
  public
    constructor Create(_Owner: TComponent; _Enhancer: TBuildEventsEnhancer; const _Name: string;
      _Memo: TMemo; _EditBtn: TButton); reintroduce;
  end;

{ TFavHandler }

constructor TFavHandler.Create(_Owner: TComponent; _Enhancer: TBuildEventsEnhancer; const _Name: string;
  _Memo: TMemo; _EditBtn: TButton);
begin
  inherited Create(_Owner);
  Name := _Name;
  FEnhancer := _Enhancer;
  FMemo := _Memo;
  FFavBtn := TButton.Create(Self);
  FFavBtn.Parent := _EditBtn.Parent;
  FFavBtn.Name := '';
  FFavBtn.Top := _EditBtn.Top + _EditBtn.Height + 8;
  FFavBtn.Left := _EditBtn.Left;
  FFavBtn.Anchors := _EditBtn.Anchors;
  FFavBtn.Caption := '&Favorites';
  FFavBtn.OnClick := FavoritesBtnClick;

  FFavoritesPm := TPopupMenu.Create(Self);
end;

procedure TFavHandler.FavoritesBtnClick(_Sender: TObject);
var
  pnt: TPoint;
begin
  FEnhancer.InitFavoritesMenu(Self, FFavoritesPm);
  pnt := FFavBtn.ClientToScreen(Point(0, FFavBtn.Height));
  FFavoritesPm.Popup(pnt.X, pnt.Y);
end;

procedure TFavHandler.ConfigureClick(_Sender: TObject);
begin
  FEnhancer.FavoritesPmConfigure(Self);
end;

procedure TFavHandler.HandleFavoriteClick(_Sender: TObject);
var
  mi: TMenuItem;
  FavName: string;
  s: string;
begin
  mi := _Sender as TMenuItem;
  FavName := FEnhancer.FFavorites.Names[mi.Tag - 1];
  s := FEnhancer.FFavorites.Values[FavName];
  FMemo.Lines.Add(s);
end;

var
  TheBuildEventsEnhancer: TBuildEventsEnhancer = nil;

{ TBuildEventsEnhancer }

constructor TBuildEventsEnhancer.Create;
begin
  FFavorites := TStringList.Create;
  LoadSettings;
  inherited Create;
end;

destructor TBuildEventsEnhancer.Destroy;
begin
  FreeAndNil(FFavorites);
  inherited;
end;

function TBuildEventsEnhancer.ConfigurationKey: string;
begin
  Result := 'IDEEnhancements';
end;

procedure TBuildEventsEnhancer.LoadSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    ExpSettings.ReadStrings('BuildEventsFavorites', FFavorites);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TBuildEventsEnhancer.SaveSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  // do not localize any of the below items
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    ExpSettings.WriteStrings('BuildEventsFavorites', FFavorites);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
end;

function TBuildEventsEnhancer.IsBuildEventsForm(_Form: TCustomForm): Boolean;
const
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
  DIALOG_CLASS = 'TBuildEventPropEditor';
  DIALOG_NAME = 'BuildEventPropEditor';
{$ELSE}
  DIALOG_CLASS = 'TBuildEventEditor';
  DIALOG_NAME = 'BuildEventEditor';
{$ENDIF}
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

function TBuildEventsEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  Result := IsBuildEventsForm(_Form)
{$IFDEF HAS_OLD_STYLE_PAGE}
  or IsProjectOptionsForm(_Form);
{$ENDIF}
end;

{$IFDEF HAS_OLD_STYLE_PAGE}

{$UNDEF INCREASE_MEMO_HEIGHT}
{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
// In later versions, the memo is much larger already, but in Delphi 2007 it can only show
// two lines. Make it at least three lines.
{$DEFINE INCREASE_MEMO_HEIGHT}
{$ENDIF}{$ENDIF}

procedure TBuildEventsEnhancer.InitBuildEvent(_Form: TForm; const _BtnName, _MemoName: string);
var
  Cmp: TComponent;
  Button: TButton;
  Memo: TMemo;
  Handler: TFavHandler;
begin
  if TComponent_FindComponent(_Form, 'GX' + _BtnName, True, Cmp) then
    Exit;
  if not TComponent_FindComponent(_Form, _BtnName, True, Cmp) or not (Cmp is TButton) then
    Exit;
  Button := TButton(Cmp);
  if not TComponent_FindComponent(_Form, _MemoName, True, Cmp) or not (Cmp is TMemo) then
    Exit;
  Memo := TMemo(Cmp);
{$IFDEF INCREASE_MEMO_HEIGHT}
  Memo.Height := Memo.Height + 12;
  Memo.Anchors := Memo.Anchors + [akBottom];
  Memo.Parent.Anchors := Memo.Parent.Anchors + [akBottom];
{$ENDIF}

  Handler := TFavHandler.Create(Button.Parent, Self, 'GX' + _BtnName, Memo, Button);
  Handler.FFavBtn.TabOrder := Button.TabOrder + 1;
end;

procedure TBuildEventsEnhancer.InitProjectOptions(_Form: TForm);
var
  Cmp: TComponent;
{$IFDEF INCREASE_MEMO_HEIGHT}
  pPreBuild: TPanel;
  pPostBuild: TPanel;
  pPreLink: TPanel;
{$ENDIF INCREASE_MEMO_HEIGHT}
begin
  if TComponent_FindComponent(_Form, 'GX' + 'bEditPreBuild', True, Cmp) then begin
    // do it only once
    Exit;
  end;
  InitBuildEvent(_Form, 'bEditPreBuild', 'mPreBuildCommands');
  InitBuildEvent(_Form, 'bEditPostBuild', 'mPostBuildCommands');
  InitBuildEvent(_Form, 'bEditPreLink', 'mPreLinkCommands');
{$IFDEF INCREASE_MEMO_HEIGHT}
  if TComponent_FindComponent(_Form, 'pPreLink', True, Cmp) and (Cmp is TPanel)
    and not TPanel(Cmp).Visible then begin
    pPreLink := TPanel(Cmp);
    if TComponent_FindComponent(_Form, 'pPreBuild', True, Cmp) and (Cmp is TPanel) then begin
      pPreBuild := TPanel(Cmp);
      if TComponent_FindComponent(_Form, 'pPostBuild', True, Cmp) and (Cmp is TPanel) then begin
        pPreBuild.Height := pPreBuild.Height + pPreLink.Height div 2;
        pPostBuild := TPanel(Cmp);
        pPostBuild.Top := pPreBuild.Top + pPreBuild.Height + 8;
        pPostBuild.Height := pPostBuild.Height + pPreLink.Height div 2;
      end;
    end;
  end;
{$ENDIF INCREASE_MEMO_HEIGHT}
end;
{$ENDIF HAS_OLD_STYLE_PAGE}

procedure TBuildEventsEnhancer.EnhanceForm(_Form: TForm);
var
  frm: TForm;
  Cmp: TComponent;
  OkButton: TButton;
  CancelButton: TButton;
  Handler: TFavHandler;
begin
  if IsBuildEventsForm(_Form) then begin
    frm := _Form as TForm;
    if TComponent_FindComponent(frm, 'GX' + 'OkButton', True, Cmp) then
      Exit;
    if not TComponent_FindComponent(frm, 'OkButton', True, Cmp) or not (Cmp is TButton) then
      Exit;
    OkButton := TButton(Cmp);
    if not TComponent_FindComponent(frm, 'CancelButton', True, Cmp) or not (Cmp is TButton) then
      Exit;
    CancelButton := TButton(Cmp);

    if not TComponent_FindComponent(frm, 'CommandsMemo', True, Cmp) or not (Cmp is TMemo) then
      Exit;
    Handler := TFavHandler.Create(OkButton.Parent, Self, 'GX' + 'OkButton', TMemo(Cmp), OkButton);
    Handler.FFavBtn.Top := OkButton.Top;
    Handler.FFavBtn.Left := OkButton.Left - (CancelButton.Left - OkButton.Left);
    Handler.FFavBtn.TabOrder := OkButton.TabOrder;
    Exit;
  end;

{$IFDEF HAS_OLD_STYLE_PAGE}
  if IsProjectOptionsForm(_Form) then begin
    frm := _Form as TForm;
    InitProjectOptions(frm);
  end;
{$ENDIF HAS_OLD_STYLE_PAGE}
end;

procedure TBuildEventsEnhancer.InitFavoritesMenu(_Owner: TFavHandler; _pm: TPopupMenu);
var
  i: Integer;
  mi: TMenuItem;
  FavName: string;
begin
  _pm.Items.Clear;
  for i := 0 to FFavorites.Count - 1 do begin
    FavName := FFavorites.Names[i];
    mi := TPopupMenu_AppendMenuItem(_pm, FavName, _Owner.HandleFavoriteClick);
    mi.Tag := i + 1;
  end;
  TPopupMenu_AppendMenuItem(_pm, 'Configure ...', _Owner.ConfigureClick);
end;

procedure TBuildEventsEnhancer.FavoritesPmConfigure(_Owner: TFavHandler);
resourcestring
  SFavSearchPaths = 'Favorite Build Events';
begin
  Tf_GxIdeFavoritesList.Execute(GetParentForm(_Owner.FMemo), SFavSearchPaths, EditEntry, FFavorites);
  SaveSettings;
end;

procedure TBuildEventsEnhancer.EditEntry(_Sender: TWinControl;
  var _Name, _Value: string; var _OK: Boolean);
begin
  _OK := Tf_IdeBuildEventFavoriteEdit.Execute(_Sender, _Name, _Value)
end;

{$ENDIF GX_EnableBuildEventsEnhancer}

{ TGxIdeBuildEventsEnhancer }

class function TGxIdeBuildEventsEnhancer.GetEnabled: Boolean;
begin
{$IFDEF GX_EnableBuildEventsEnhancer}
  Result := Assigned(TheBuildEventsEnhancer);
{$ELSE}
  Result := False;
{$ENDIF}
end;

class procedure TGxIdeBuildEventsEnhancer.SetEnabled(const Value: Boolean);
begin
{$IFDEF GX_EnableBuildEventsEnhancer}
  if Value then begin
    if not Assigned(TheBuildEventsEnhancer) then
      TheBuildEventsEnhancer := TBuildEventsEnhancer.Create
  end else
    FreeAndNil(TheBuildEventsEnhancer);
{$ENDIF}
end;

initialization
finalization
  TGxIdeBuildEventsEnhancer.SetEnabled(False);
end.
