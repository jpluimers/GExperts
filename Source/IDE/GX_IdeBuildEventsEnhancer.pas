unit GX_IdeBuildEventsEnhancer;

{$I GX_CondDefine.inc}

{$UNDEF GX_EnableBuildEventsEnhancer}
{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
{$DEFINE GX_EnableBuildEventsEnhancer}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Forms;

type
  TGxIdeBuildEventsEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

{$IFDEF GX_EnableBuildEventsEnhancer}

uses
  Controls,
  Menus,
  Dialogs,
  Types,
  GX_dzVclUtils,
  GX_IdeFormEnhancer,
  GX_IdeFavoritesList,
  GX_ConfigurationInfo,
  GX_IdeBuildEventFavoriteEdit;

type
  TBuildEventsEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    FForm: TForm;
    FFavoritesPm: TPopupMenu;
    FFavoritesBtn: TButton;
    FFavorites: TStringList;
    FCommandsMemo: TMemo;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsBuildEventsForm(_Form: TCustomForm): Boolean;
    procedure FavoritesBtnClick(_Sender: TObject);
    procedure FavoritesPmConfigureClick(_Sender: TObject);
    procedure InitFavoritesMenu;
    procedure FavoritesPmHandleFavoriteClick(_Sender: TObject);
    procedure LoadSettings;
    procedure SaveSettings;
    function ConfigurationKey: string;
    procedure EditEntry(_Sender: TWinControl; var _Name, _Value: string; var _OK: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheBuildEventsEnhancer: TBuildEventsEnhancer = nil;

{ TBuildEventsEnhancer }

constructor TBuildEventsEnhancer.Create;
begin
  inherited Create;
  FFavorites := TStringList.Create;
  LoadSettings;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TBuildEventsEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
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
{$IFDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
  DIALOG_CLASS = 'TBuildEventPropEditor';
  DIALOG_NAME = 'BuildEventPropEditor';
{$ELSE}
  DIALOG_CLASS = 'TBuildEventEditor';
  DIALOG_NAME = 'BuildEventEditor';
{$ENDIF}
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
end;

procedure TBuildEventsEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
const
  B_FAVORITES = 'GxBuildEventEnhancerFavoritesButton';
var
  frm: TForm;
  Cmp: TComponent;
  OkButton: TButton;
  CancelButton: TButton;
begin
  if not IsBuildEventsForm(_Form) then begin
    Exit;
  end;

  frm := TForm(_Form);

  if Assigned(frm.FindComponent(B_FAVORITES)) then
    Exit;

  Cmp := frm.FindComponent('OkButton');
  if not Assigned(Cmp) or not (Cmp is TButton) then
    Exit;
  OkButton := TButton(Cmp);

  Cmp := frm.FindComponent('CancelButton');
  if not Assigned(Cmp) or not (Cmp is TButton) then
    Exit;
  CancelButton := TButton(Cmp);

  Cmp := frm.FindComponent('CommandsMemo');
  if not Assigned(Cmp) or not (Cmp is TMemo) then
    Exit;
  FCommandsMemo := TMemo(Cmp);

  FForm := frm;

  FFavoritesBtn := TButton.Create(frm);
  FFavoritesBtn.Parent := OkButton.Parent;
  FFavoritesBtn.Top := OkButton.Top;
  FFavoritesBtn.Left := OkButton.Left - (CancelButton.Left - OkButton.Left);
  FFavoritesBtn.Caption := '&Favourites';
  FFavoritesBtn.OnClick := FavoritesBtnClick;
  FFavoritesBtn.TabOrder := OkButton.TabOrder;

  FFavoritesPm := TPopupMenu.Create(_Form);
  InitFavoritesMenu;
end;

procedure TBuildEventsEnhancer.InitFavoritesMenu;
var
  i: Integer;
  mi: TMenuItem;
  FavName: string;
begin
  FFavoritesPm.Items.Clear;
  for i := 0 to FFavorites.Count - 1 do begin
    FavName := FFavorites.Names[i];
    mi := TPopupMenu_AppendMenuItem(FFavoritesPm, FavName, FavoritesPmHandleFavoriteClick);
    mi.Tag := i + 1;
  end;
  TPopupMenu_AppendMenuItem(FFavoritesPm, 'Configure ...', FavoritesPmConfigureClick);
end;

procedure TBuildEventsEnhancer.FavoritesBtnClick(_Sender: TObject);
var
  pnt: TPoint;
begin
  pnt := FFavoritesBtn.ClientToScreen(Point(0, FFavoritesBtn.Height));
  FFavoritesPm.Popup(pnt.X, pnt.Y);
end;

procedure TBuildEventsEnhancer.FavoritesPmConfigureClick(_Sender: TObject);
resourcestring
  SFavSearchPaths = 'Favorite Build Events';
begin
  Tf_GxIdeFavoritesList.Execute(FForm, SFavSearchPaths, EditEntry, FFavorites);
  SaveSettings;
  InitFavoritesMenu;
end;

procedure TBuildEventsEnhancer.EditEntry(_Sender: TWinControl;
  var _Name, _Value: string; var _OK: Boolean);
begin
  _OK := Tf_IdeBuildEventFavoriteEdit.Execute(_Sender, _Name, _Value)
end;

procedure TBuildEventsEnhancer.FavoritesPmHandleFavoriteClick(_Sender: TObject);
var
  mi: TMenuItem;
  FavName: string;
  s: string;
begin
  mi := _Sender as TMenuItem;
  FavName := FFavorites.Names[mi.Tag - 1];
  s := FFavorites.Values[FavName];
  FCommandsMemo.Lines.Add(s);
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

end.
