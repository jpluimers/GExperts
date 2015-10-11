unit GX_IdeSearchPathEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Forms;

type
  TGxIdeSearchPathEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  GX_IdeFormEnhancer,
  GX_dzVclUtils;

type
  TSearchPathEnhancer = class
  private
    FCallbackHandle: TFormChangeHandle;
    FIsAutocompleteEnabled: Boolean;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsSearchPathForm(_Form: TCustomForm): Boolean;
    function TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheSearchPathEnhancer: TSearchPathEnhancer = nil;

{ TGxIdeSearchPathEnhancer }

class function TGxIdeSearchPathEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheSearchPathEnhancer);
end;

class procedure TGxIdeSearchPathEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then
    TheSearchPathEnhancer := TSearchPathEnhancer.Create
  else
    FreeAndNil(TheSearchPathEnhancer);
end;

{ TSearchPathEnhancer }

constructor TSearchPathEnhancer.Create;
begin
  inherited Create;
  FCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TSearchPathEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FCallbackHandle);
  inherited;
end;

procedure TSearchPathEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
  ed: TEdit;
begin
  frm := Screen.ActiveCustomForm;
  if not IsSearchPathForm(frm) then
    Exit;
  if not TryGetElementEdit(frm, ed) then
    Exit;

  ed.Text := _Files[0];
end;

function TSearchPathEnhancer.TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
var
  cmp: TComponent;
begin
  Result := False;
  cmp := _Form.FindComponent('ElementEdit');
  if not Assigned(cmp) then
    Exit;
  if not (cmp is TEdit) then
    Exit;

  _ed := cmp as TEdit;
  Result := True;
end;

{$IFDEF GX_VER220_up}
// Delphi XE and up
const
  SearchPathDialogClass = 'TInheritedListEditDlg';
  SearchPathDialogName = 'InheritedListEditDlg';
  SearchPathDialogCaption = 'Search Path';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ELSE GX_VER220_up}
{$IFDEF GX_VER200_up}
// Delphi 2009 and up
const
  SearchPathDialogClass = 'TInheritedListEditDlg';
  SearchPathDialogName = 'InheritedListEditDlg';
  SearchPathDialogCaption = 'Search path';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ELSE GX_VER200_up}
// Delphi 2007 and earlier
const
  SearchPathDialogClass = 'TOrderedListEditDlg';
  SearchPathDialogName = 'OrderedListEditDlg';
  SearchPathDialogCaption = 'Directories';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER220_up}

function TSearchPathEnhancer.IsSearchPathForm(_Form: TCustomForm): Boolean;
begin
  Result := False;
  if not Assigned(_Form) then
    Exit;
  if not SameText(_Form.ClassName, SearchPathDialogClass)
    or not SameText(_Form.Name, SearchPathDialogName) then
    Exit;
  if not SameText(_Form.Caption, SearchPathDialogCaption)
    and not SameText(_Form.Caption, SearchPathDialogCaptionFR)
    and not SameText(_Form.Caption, SearchPathDialogCaptionDE) then
    Exit;
  Result := True;
end;

procedure TSearchPathEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
var
  ed: TEdit;
  cmp: TComponent;
begin
  if not IsSearchPathForm(_Form) or not TryGetElementEdit(_Form, ed) then begin
    FIsAutocompleteEnabled := False;
    Exit;
  end;

  if not FIsAutocompleteEnabled then begin
    TEdit_SetAutocomplete(ed, [acsFileSystem], [actSuggest]);
    FIsAutocompleteEnabled := True;
    TWinControl_ActivateDropFiles(ed, HandleFilesDropped);
    cmp := _Form.FindComponent('CreationList');
    if cmp is TListBox then begin
      TWinControl_ActivateDropFiles(TListBox(cmp), HandleFilesDropped);
      cmp := _Form.FindComponent('InvalidPathLbl');
      if cmp is TLabel then
        TLabel(cmp).Caption := TLabel(cmp).Caption + ' Drag and drop is enabled.';
    end;
  end;
end;

initialization
finalization
  TGxIdeSearchPathEnhancer.SetEnabled(False);
end.
