unit GX_IdeProjectOptionsEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils;

type
  TGxIdeProjectOptionsEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const Value: Boolean); //static;
  end;

implementation

uses
  Classes,
  Messages,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  ActnList,
  Menus,
  GX_IdeFormEnhancer,
  GX_dzVclUtils;

type
  TComboboxDropHandler = class(TComponent)
  private
    // In Delphi 8-2007 the "Comboboxes" do not descend from TCustomComboBox. The only ancestor
    // that is a standard control is TWinControl, everything else is only available in the
    // IDE.
    FCmb: TWinControl;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    class function GenerateName(_cmb: TWinControl): string;
  public
    constructor Create(_Owner: TComponent); override;
    class function IsAlreadyHooked(_cmb: TWinControl): Boolean;
  end;

  { TComboboxDropHandler }

class function TComboboxDropHandler.GenerateName(_cmb: TWinControl): string;
begin
  Result := _cmb.Name + '_FilesDropHandler';
end;

class function TComboboxDropHandler.IsAlreadyHooked(_cmb: TWinControl): Boolean;
begin
  Result := _cmb.FindComponent(GenerateName(_cmb)) <> nil;
end;

constructor TComboboxDropHandler.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  FCmb := _Owner as TWinControl;
  Name := GenerateName(FCmb);
  TWinControl_ActivateDropFiles(FCmb, HandleFilesDropped);
end;

procedure TComboboxDropHandler.HandleFilesDropped(_Sender: TObject; _Files:
  TStrings);
// Since in Delphi 8-2007 FCmb is not a TCustomCombobBox, we cannot just typecast it
// but must send it the appropriate messages ourself. Just to complicate matters,
// we must pass the filename as a WideString.
type
{$IFDEF GX_VER160_up}
  PThisChar = PWideString;
  ThisString = WideString;
{$ELSE}
  // Delphi 6/7
  PThisChar = PChar;
  ThisString = string;
{$ENDIF}

var
  s: ThisString;
begin
  s := _Files[0];
  FCmb.Perform(WM_SETTEXT, 0, Integer(PThisChar(s)));
  FCmb.Perform(CM_TEXTCHANGED, 0, 0);
end;

type
  TProjectOptionsEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    FControlCallbackHandle: TControlChangeHandle;
    FOkBtn: TButton;
    FConfigCombo: TCustomCombo;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsProjectOptionsForm(_Form: TCustomForm): Boolean;
    function TryFindHistoryComboBox(_SettingsControl: TWinControl; _GrpBoxIdx: Integer;
      const _Name: string; out _cmb: TWinControl): Boolean;
    function TryGetSettingsControl(_Form: TCustomForm; out _SettingsControl: TWinControl): Boolean;
    procedure ShiftCtrlO(_Sender: TObject);
    procedure ShiftCtrlT(_Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheProjectOptionsEnhancer: TProjectOptionsEnhancer = nil;

  { TGxIdeProjectOptionsEnhancer }

class function TGxIdeProjectOptionsEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheProjectOptionsEnhancer);
end;

class procedure TGxIdeProjectOptionsEnhancer.SetEnabled(const Value: Boolean);
begin
  if Value then begin
    if not Assigned(TheProjectOptionsEnhancer) then
      TheProjectOptionsEnhancer := TProjectOptionsEnhancer.Create
  end else
    FreeAndNil(TheProjectOptionsEnhancer);
end;

{ TProjectOptionsEnhancer }

constructor TProjectOptionsEnhancer.Create;
begin
  inherited Create;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TProjectOptionsEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
  inherited;
end;

function CheckControl(_Parent: TWinControl; _Idx: Integer; const _ClassName: string;
  out _AsWinCtrl: TWinControl): Boolean;
var
  Ctrl: TControl;
  i: Integer;
begin
  Result := False;
  if _Idx = -1 then begin
    for i := 0 to _Parent.ControlCount - 1 do begin
      Ctrl := _Parent.Controls[i];
      if Ctrl.ClassName = _ClassName then begin
        _AsWinCtrl := TWinControl(Ctrl);
        Result := True;
      end;
    end;
  end else begin
    if _Parent.ControlCount <= _Idx then
      Exit;
    Ctrl := _Parent.Controls[_Idx];
    if Ctrl.ClassName <> _ClassName then
      Exit;
    _AsWinCtrl := TWinControl(Ctrl);
    Result := True;
  end;
end;

function TProjectOptionsEnhancer.TryGetSettingsControl(_Form: TCustomForm;
  out _SettingsControl: TWinControl): Boolean;
var
  wctrl: TWinControl;
begin
  Result := False;
{$IFDEF GX_VER180_up} // Delphi 2006 (BDS 3)
  if not CheckControl(_Form, 3, 'TPanel', wctrl) then
    Exit;
  if not CheckControl(wctrl, 1, 'TPropertySheetControl', wctrl) then
    Exit;
  if not CheckControl(wctrl, -1, 'TDebuggerLocalPage', wctrl) then
    Exit;
  _SettingsControl := wctrl;
  // in Delphi 10 there is an additional TPanel that contains the group boxes
  if CheckControl(wctrl, 2, 'TPanel', wctrl) then
    _SettingsControl := wctrl;
  Result := True;
{$ELSE}{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
  if not CheckControl(_Form, 2, 'TPanel', wctrl) then
    Exit;
  if not CheckControl(wctrl, 1, 'TPropertySheetControl', wctrl) then
    Exit;
  if not CheckControl(wctrl, -1, 'TDebuggerLocalPage', wctrl) then
    Exit;
  _SettingsControl := wctrl;
  // in Delphi 10 there is an additional TPanel that contains the group boxes
  if CheckControl(wctrl, 2, 'TPanel', wctrl) then
    _SettingsControl := wctrl;
  Result := True;
{$ELSE}{$IFDEF GX_VER140_up} // Delphi 6
  if not CheckControl(_Form, 1, 'TPageControl', wctrl) then
    Exit;
  if not CheckControl(wctrl, 0, 'TTabSheet', wctrl) then
    Exit;
  _SettingsControl := wctrl;
  Result := True;
{$ENDIF}{$ENDIF}{$ENDIF}
end;

function TProjectOptionsEnhancer.TryFindHistoryComboBox(_SettingsControl: TWinControl;
  _GrpBoxIdx: Integer; const _Name: string; out _cmb: TWinControl): Boolean;
var
  wctrl: TWinControl;
begin
  Result := False;

  if not CheckControl(_SettingsControl, _GrpBoxIdx, 'TGroupBox', wctrl) then
    Exit;
  if not CheckControl(wctrl, 0, 'THistoryPropComboBox', wctrl) or (wctrl.Name <>
    _Name) then
    Exit;
  _cmb := wctrl;
  Result := True;
end;

type
  TInputComboNamesArr = array[0..3] of string;
  TInputComboIndexArr = array[0..3] of Integer;
const
  // The names of the controls haven't changed, but their type
  // and the index of the containing GroupBoxes, so we need to specify them by
  // Delphi version, reduced a bit because it didn't change every time
  INPUT_COMBO_NAMES: TInputComboNamesArr = (
    'HostAppInput', 'ParamInput', 'CWDInput', 'SourcePathInput');
  INPUT_COMBO_INDEXES: TInputComboIndexArr = (
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
    0, 1, 2, 3
{$ELSE}{$IFDEF GX_VER220_up} // RAD Studio XE 1 (16; BDS 8)
    1, 2, 3, 5
{$ELSE}{$IFDEF GX_VER210_up} // RAD Studio 2010 (15; BDS 7)
    0, 1, 2, 4
{$ELSE}{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
    0, 1, 2, 5
{$ELSE}{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
    0, 1, 2, -1
{$ELSE}{$IFDEF GX_VER160_up} // Delphi 8 (BDS 1)
    - 1, -1, -1, -1 // I have no idea, probably the same as Delphi 2005
{$ELSE}{$IFDEF GX_VER150_up} // Delphi 7
    0, 1, 6, -1
{$ELSE}{$IFDEF GX_VER140_up} // Delphi 6
    0, 1, -1, -1
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
    );

{$IFDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
  RUN_PARAMS_DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER220_up} // RAD Studio XE 1 (16; BDS 8)
  RUN_PARAMS_DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER210_up} // RAD Studio 2010 (15; BDS 7)
  RUN_PARAMS_DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  RUN_PARAMS_DIALOG_CLASS = 'TDelphiProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'DelphiProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
  RUN_PARAMS_DIALOG_CLASS = 'TProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'ProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER160_up} // Delphi 8 (BDS 1)
  RUN_PARAMS_DIALOG_CLASS = 'TProjectOptionsDialog';
  RUN_PARAMS_DIALOG_NAME = 'ProjectOptionsDialog';
{$ELSE}{$IFDEF GX_VER150_up} // Delphi 7
  RUN_PARAMS_DIALOG_CLASS = 'TRunParamsDlg';
  RUN_PARAMS_DIALOG_NAME = 'RunParamsDlg';
{$ELSE}{$IFDEF GX_VER140_up} // Delphi 6
  RUN_PARAMS_DIALOG_CLASS = 'TRunParamsDlg';
  RUN_PARAMS_DIALOG_NAME = 'RunParamsDlg';
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

procedure TProjectOptionsEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);

  function TryHookCombo(_SettingsPanel: TWinControl; _Index: Integer; const
    _Name: string): Boolean;
  var
    cmb: TWinControl;
  begin
    Result := False;
    if _Index < 0 then
      Exit;
    if TryFindHistoryComboBox(_SettingsPanel, _Index, _Name, cmb) then begin
      if not TComboboxDropHandler.IsAlreadyHooked(cmb) then begin
        TComboboxDropHandler.Create(cmb);
        Result := True;
      end;
    end;
  end;

const
  GX_ACTION_LIST = 'GXProjectOptionsActionList'; // do no localize!
var
  i: Integer;
  SettingsControl: TWinControl;
  al: TActionList;
  lbl: TLabel;
begin
  if not IsProjectOptionsForm(_Form) then begin
    if Assigned(FControlCallbackHandle) then begin
      TIDEFormEnhancements.UnregisterControlChangeCallback(FControlCallbackHandle);
      FControlCallbackHandle := nil;
    end;
    Exit;
  end;

  if _Form.FindComponent(GX_ACTION_LIST) = nil then begin
    al := TActionList.Create(_Form);
    al.Name := GX_ACTION_LIST;

    FOkBtn := _Form.FindComponent('OKButton') as TButton;
    if Assigned(FOkBtn) then begin
      TActionlist_Append(al, 'OK', ShiftCtrlO, ShortCut(Ord('O'), [ssShift, ssCtrl]));
      FOkBtn.Caption := 'OK (s+c+O)';
    end;
    FConfigCombo := _Form.FindComponent('cbConfig') as TCustomCombo;
    if Assigned(FConfigCombo) then begin
      TActionlist_Append(al, 'Target', ShiftCtrlT, ShortCut(Ord('T'), [ssShift, ssCtrl]));
      lbl := _Form.FindComponent('Label1') as TLabel; // sic!
      if Assigned(lbl) then begin
        lbl.AutoSize := False;
        lbl.Top := FConfigCombo.Top;
        lbl.Height := lbl.Height * 2;
        lbl.Width := FConfigCombo.Left - lbl.Left;
        lbl.WordWrap := True;
        lbl.Caption := '&Target: (s+c+T)';
      end;
    end;
  end;

  if not TryGetSettingsControl(_Form, SettingsControl) then
    Exit;

  for i := 0 to 3 do
    TryHookCombo(SettingsControl, INPUT_COMBO_INDEXES[i], INPUT_COMBO_NAMES[i]);
end;

procedure TProjectOptionsEnhancer.ShiftCtrlO(_Sender: TObject);
begin
  FOkBtn.Click;
end;

procedure TProjectOptionsEnhancer.ShiftCtrlT(_Sender: TObject);
begin
  TWinControl_SetFocus(FConfigCombo);
end;

function TProjectOptionsEnhancer.IsProjectOptionsForm(_Form: TCustomForm):
  Boolean;
begin
  Result := (_Form.ClassName = RUN_PARAMS_DIALOG_CLASS) and (_Form.Name =
    RUN_PARAMS_DIALOG_NAME);
end;

initialization
finalization
  TGxIdeProjectOptionsEnhancer.SetEnabled(False);
end.
