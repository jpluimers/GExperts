unit GX_IdeToolPropertiesEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Forms;

type
  TGxIdeToolPropertiesEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  GX_IdeFormEnhancer,
  GX_dzVclUtils, Controls, Messages;

type
  TToolPropertiesEnhancer = class
  private
    FCallbackHandle: TFormChangeHandle;
    FIsAutocompleteEnabled: Boolean;
//    FControlChangedHandle: TControlChangeHandle;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsToolPropertiesForm(_Form: TCustomForm): Boolean;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    function TryFindEdit(_Form: TCustomForm; const _Name: string; out _ed: TEdit): boolean;
//    procedure HandleControlChanged(_Sender: TObject; _Form: TCustomForm; _Control: TWinControl);
  public
    constructor Create;
    destructor Destroy; override;
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

constructor TToolPropertiesEnhancer.Create;
begin
  inherited Create;
  FCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged);
end;

destructor TToolPropertiesEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FCallbackHandle);
  inherited;
end;

procedure TToolPropertiesEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
  ed: TEdit;
begin
  frm := Screen.ActiveCustomForm;
  if not IsToolPropertiesForm(frm) then
    Exit;
  ed := _Sender as TEdit;
  ed.Text := _Files[0];
  if ed.Name = 'edProgram' then
    if TryFindEdit(frm, 'edWorkingDir', ed) then
      ed.Text := ExtractFileDir(_Files[0]);
end;

function TToolPropertiesEnhancer.IsToolPropertiesForm(_Form: TCustomForm): Boolean;
begin
  Result := False;
  if not Assigned(_Form) then
    Exit;
  if not _Form.ClassNameIs('TTransEditDlg')
    or not SameText(_Form.Name, 'TransEditDlg') then
    Exit;
  Result := True;
end;

function TToolPropertiesEnhancer.TryFindEdit(_Form: TCustomForm; const _Name: string; out _ed: TEdit): boolean;
begin
  _ed := TEdit(_Form.FindComponent(_Name));
  Result := Assigned(_ed);
end;

procedure TToolPropertiesEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
var
  ed: TEdit;
begin
  if not IsToolPropertiesForm(_Form) then begin
//    TIDEFormEnhancements.UnregisterControlChangeCallback(FControlChangedHandle);
//    FControlChangedHandle := nil;
    FIsAutocompleteEnabled := False;
    Exit;
  end;

// Drop files only works in Delphi 6 and 7 while autocomplete works in all versions.
// The "new" IDE apparently does something to TEdits that prevent them to receive WM_DROPFILES
// messages.
// I tried to use this for re-registering the drop files handler but it did not help:
//  FControlChangedHandle := TIDEFormEnhancements.RegisterControlChangeCallback(HandleControlChanged);

  if TryFindEdit(_Form, 'edProgram', ed) then begin
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    TEdit_ActivateAutoComplete(ed, [acsFileSystem], [actSuggest]);
  end;
  if TryFindEdit(_Form, 'edWorkingDir', ed) then begin
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    TEdit_ActivateAutoComplete(ed, [acsFileSystem], [actSuggest]);
  end;
  if TryFindEdit(_Form, 'edParameters', ed) then begin
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

