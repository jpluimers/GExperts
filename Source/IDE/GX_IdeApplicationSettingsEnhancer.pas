unit GX_IdeApplicationSettingsEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

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
  GX_VerDepConst,
  GX_IdeDialogEnhancer;

type
  TIdeAppSettingsEnhancer = class(TIdeDialogEnhancer)
  private
    FSuffixEdit: TEdit;
    procedure btnSetSuffixClick(_Sender: TObject);
  protected
    procedure EnhanceForm(_Form: TForm); override;
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
  public
  end;

var
  TheAppSettingsEnhancer: TIdeAppSettingsEnhancer = nil;

{ TIdeAppSettingsEnhancer }

procedure TIdeAppSettingsEnhancer.EnhanceForm(_Form: TForm);
const
  SuffixBtnName = 'GXSOSuffixBtn';
var
  Cmp: TComponent;
  btn: TButton;
  s: string;
begin
  if TComponent_FindComponent(_Form, SuffixBtnName, True, Cmp) then
    Exit;
  if not TComponent_FindComponent(_Form, 'ecSOSuffix', True, Cmp) or not (Cmp is TEdit) then
    Exit;
  FSuffixEdit := TEdit(Cmp);
  btn := TButton.Create(_Form);
  btn.Parent := FSuffixEdit.Parent;
  btn.Name := SuffixBtnName;
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

function TIdeAppSettingsEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
const
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  DIALOG_CLASS = 'TProjectOptionsDialog';
  DIALOG_NAME = 'ProjectOptionsDialog');
{$ELSE}
DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ENDIF}
begin
  Result := (_Form.ClassName = DIALOG_CLASS) and (_Form.Name = DIALOG_NAME);
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
