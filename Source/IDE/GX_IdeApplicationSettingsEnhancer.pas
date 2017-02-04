unit GX_IdeApplicationSettingsEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Forms;

type
  TGxIdeApplicationSettingsEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  Controls,
  Menus,
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
  GX_VerDepConst;

type
  TIdeAppSettingsEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    FSuffixEdit: TEdit;

    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    procedure InitProjectOptions(_Form: TForm);
    procedure btnSetSuffixClick(_Sender: TObject);
    function IsProjectOptionsForm(_Form: TCustomForm): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheAppSettingsEnhancer: TIdeAppSettingsEnhancer = nil;

{ TIdeAppSettingsEnhancer }

constructor TIdeAppSettingsEnhancer.Create;
begin
  inherited Create;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TIdeAppSettingsEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
  inherited;
end;

procedure TIdeAppSettingsEnhancer.InitProjectOptions(_Form: TForm);
var
  Cmp: TComponent;
  btn: TButton;
  s: string;
begin
  if TComponent_FindComponent(_Form, 'GXSOSuffixBtn', True, Cmp) then
    Exit;
  if not TComponent_FindComponent(_Form, 'ecSOSuffix', True, Cmp) or not (Cmp is TEdit) then
    Exit;
  FSuffixEdit := TEdit(Cmp);
  btn := TButton.Create(_Form);
  btn.Parent := FSuffixEdit.Parent;
  btn.Name := '';
  btn.Top := FSuffixEdit.Top;
  btn.Height := FSuffixEdit.Height;
  s := MajorVersionNumberChar + '0';
  btn.Width := _Form.Canvas.TextWidth(s) + 8;
  FSuffixEdit.Width := FSuffixEdit.Width - btn.Width;
  btn.Left := FSuffixEdit.Left + FSuffixEdit.Width;
  btn.Anchors := [akTop, akRight];
  btn.Caption := s;
  btn.OnClick := btnSetSuffixClick;
  btn.TabOrder := FSuffixEdit.TabOrder + 1;
end;

procedure TIdeAppSettingsEnhancer.btnSetSuffixClick(_Sender: TObject);
begin
  FSuffixEdit.Text := (_Sender as TButton).Caption;
end;

function TIdeAppSettingsEnhancer.IsProjectOptionsForm(_Form: TCustomForm): Boolean;
begin
  Result := (_Form.ClassName = 'TDelphiProjectOptionsDialog') and (_Form.Name = 'DelphiProjectOptionsDialog')
    or (_Form.ClassName = 'TProjectOptionsDialog') and (_Form.Name = 'ProjectOptionsDialog');
end;

procedure TIdeAppSettingsEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
var
  frm: TForm;
begin
  if IsProjectOptionsForm(_Form) then begin
    frm := _Form as TForm;
    InitProjectOptions(frm);
  end;
end;

{ TGxIdeApplicationSettingsEnhancer }

class function TGxIdeApplicationSettingsEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheAppSettingsEnhancer);
end;

class procedure TGxIdeApplicationSettingsEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheAppSettingsEnhancer) then
      TheAppSettingsEnhancer := TIdeAppSettingsEnhancer.Create
  end else
    FreeAndNil(TheAppSettingsEnhancer);
end;

initialization
finalization
  TGxIdeApplicationSettingsEnhancer.SetEnabled(False);
end.
