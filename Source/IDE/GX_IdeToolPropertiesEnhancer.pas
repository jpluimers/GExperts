unit GX_IdeToolPropertiesEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

type
  TGxIdeToolPropertiesEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  Controls,
  StdCtrls,
  Forms,
  Messages,
  GX_IdeFormEnhancer,
  GX_dzVclUtils,
  GX_IdeDialogEnhancer;

type
  TToolPropertiesEnhancer = class(TIdeDialogEnhancer)
  private
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
  end;

var
  TheToolPropertiesEnhancer: TToolPropertiesEnhancer = nil;

{ TGxIdeToolPropertiesEnhancer }

class function TGxIdeToolPropertiesEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheToolPropertiesEnhancer);
end;

class procedure TGxIdeToolPropertiesEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheToolPropertiesEnhancer) then
      TheToolPropertiesEnhancer := TToolPropertiesEnhancer.Create
  end else
    FreeAndNil(TheToolPropertiesEnhancer);
end;

{ TToolPropertiesEnhancer }

procedure TToolPropertiesEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
  ed: TEdit;
begin
  frm := Screen.ActiveCustomForm;
  if not IsDesiredForm(frm) then
    Exit;
  ed := _Sender as TEdit;
  ed.Text := _Files[0];
  if ed.Name = 'edProgram' then
    if TryFindComponent(frm, 'edWorkingDir', TComponent(ed), TEdit) then
      ed.Text := ExtractFileDir(_Files[0]);
end;

function TToolPropertiesEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  Result := _Form.ClassNameIs('TTransEditDlg')
    and SameText(_Form.Name, 'TransEditDlg');
end;

procedure TToolPropertiesEnhancer.EnhanceForm(_Form: TForm);
var
  ed: TEdit;
begin
// Drop files only works in Delphi 6 and 7 while autocomplete works in all versions.
// The "new" IDE apparently does something to TEdits that prevent them to receive WM_DROPFILES
// messages.
// I tried to use this for re-registering the drop files handler but it did not help:
//  FControlChangedHandle := TIDEFormEnhancements.RegisterControlChangeCallback(HandleControlChanged);

  if TryFindComponent(_Form, 'edProgram', TComponent(ed), TEdit) then begin
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    TEdit_ActivateAutoComplete(ed, [acsFileSystem], [actSuggest]);
  end;
  if TryFindComponent(_Form, 'edWorkingDir', TComponent(ed), TEdit) then begin
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    TEdit_ActivateAutoComplete(ed, [acsFileSystem], [actSuggest]);
  end;
  if TryFindComponent(_Form, 'edParameters', TComponent(ed), TEdit) then begin
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    TEdit_ActivateAutoComplete(ed, [acsFileSystem], [actSuggest]);
  end;
end;

//procedure TToolPropertiesEnhancer.HandleControlChanged(_Sender: TObject; _Form: TCustomForm; _Control: TWinControl);
//var
//  i: Integer;
//begin
//  if not IsToolPropertiesForm(_Form) then begin
//    FIsAutocompleteEnabled := False;
//    Exit;
//  end;
//  if not (_Control is TEdit) then
//    Exit;
//  for i := _Control.ComponentCount-1 downto 0 do begin
//    if _Control.Components[i].ClassNameIs('TDropFilesActivator') then begin
//      _Control.Components[i].Free;
//      TWinControl_ActivateDropFiles2(_Control, HandleFilesDropped);
//    end;
//  end;
//end;

initialization
finalization
  TGxIdeToolPropertiesEnhancer.SetEnabled(False);
end.
