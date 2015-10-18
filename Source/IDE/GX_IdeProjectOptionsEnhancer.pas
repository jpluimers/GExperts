unit GX_IdeProjectOptionsEnhancer;

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
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  GX_IdeFormEnhancer,
  GX_dzVclUtils;

type
  TComboboxDropHandler = class(TComponent)
  private
    FCmb: TCustomComboBox;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    class function GenerateName(_cmb: TCustomComboBox): string;
  public
    constructor Create(_Owner: TComponent); override;
    class function IsAlreadyHooked(_cmb: TCustomComboBox): boolean;
  end;

  { TComboboxDropHandler }

class function TComboboxDropHandler.GenerateName(_cmb: TCustomComboBox): string;
begin
  Result := _cmb.Name + '_FilesDropHandler';
end;

class function TComboboxDropHandler.IsAlreadyHooked(_cmb: TCustomComboBox): boolean;
begin
  Result := _cmb.FindComponent(GenerateName(_cmb)) <> nil;
end;

constructor TComboboxDropHandler.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  FCmb := _Owner as TCustomComboBox;
  Name := GenerateName(FCmb);
  TWinControl_ActivateDropFiles(FCmb, HandleFilesDropped);
end;

procedure TComboboxDropHandler.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  TCombobox(FCmb).Text := _Files[0];
end;

type
  TProjectOptionsEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;
    FControlCallbackHandle: TControlChangeHandle;
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsProjectOptionsForm(_Form: TCustomForm): Boolean;
//    procedure HandleControlChanged(_Sender: TObject; _Form: TCustomForm; _Control: TWinControl);
    function TryFindHistoryComboBox(_SettingsPanel: TPanel; _GrpBoxIdx: integer;
      const _Name: string; out _cmb: TCustomCombobox): boolean;
    function TryGetSettingsPanel(_Form: TCustomForm; out _pnl: TPanel): boolean;
//    function TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
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
  if Value then
    TheProjectOptionsEnhancer := TProjectOptionsEnhancer.Create
  else
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

//procedure TProjectOptionsEnhancer.HandleControlChanged(_Sender: TObject; _Form: TCustomForm;
//  _Control: TWinControl);
//var
//  cmp: TComponent;
//begin
//  cmp := _Form.FindComponent('HostAppInput');
//  if Assigned(cmp) and (cmp.ClassName = 'THistoryPropComboBox') then begin
//    TComboboxDropHandler.Create(cmp);
//    TIDEFormEnhancements.UnregisterControlChangeCallback(FControlCallbackHandle);
//    FControlCallbackHandle := nil;
//  end;
//end;

function TProjectOptionsEnhancer.TryGetSettingsPanel(_Form: TCustomForm; out _pnl: TPanel): boolean;
var
  Ctrl: TControl;
  wctrl: TWinControl;
begin
  Result := False;

  if _Form.ControlCount < 4 then
    Exit;
  Ctrl := _Form.Controls[3];
  if Ctrl.ClassName <> 'TPanel' then
    Exit;
  wctrl := TWinControl(Ctrl);

  if wctrl.ControlCount < 2 then
    Exit;
  Ctrl := wctrl.Controls[1];
  if Ctrl.ClassName <> 'TPropertySheetControl' then
    Exit;
  wctrl := TWinControl(Ctrl);

  if wctrl.ControlCount < 1 then
    Exit;
  Ctrl := wctrl.Controls[0];
  if Ctrl.ClassName <> 'TDebuggerLocalPage' then
    Exit;
  wctrl := TWinControl(Ctrl);

  if wctrl.ControlCount < 3 then
    Exit;
  Ctrl := wctrl.Controls[2];
  if Ctrl.ClassName <> 'TPanel' then
    Exit;
  _pnl := TPanel(Ctrl);
  Result := true;
end;

function TProjectOptionsEnhancer.TryFindHistoryComboBox(_SettingsPanel: TPanel; _GrpBoxIdx: integer;
  const _Name: string; out _cmb: TCustomCombobox): boolean;
var
  Ctrl: TControl;
  wctrl: TWinControl;
begin
  Result := False;

  if _SettingsPanel.ControlCount < _GrpBoxIdx + 1 then
    Exit;
  Ctrl := _SettingsPanel.Controls[_GrpBoxIdx];
  if Ctrl.ClassName <> 'TGroupBox' then
    Exit;
  wctrl := TWinControl(Ctrl);

  if wctrl.ControlCount < 1 then
    Exit;
  Ctrl := wctrl.Controls[0];
  if (Ctrl.ClassName <> 'THistoryPropComboBox') or (Ctrl.Name <> _Name) then
    Exit;
  _cmb := Ctrl as TCustomComboBox;
  Result := true;
end;

procedure TProjectOptionsEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);

  function TryHookCombo(_SettingsPanel: TPanel; _Index: integer; const _Name: string): boolean;
  var
    cmb: TCustomComboBox;
  begin
    Result := False;
    if TryFindHistoryComboBox(_SettingsPanel, _Index, _Name, cmb) then begin
      if not TComboboxDropHandler.IsAlreadyHooked(cmb) then begin
        TComboboxDropHandler.Create(cmb);
        Result := True;
      end;
    end;
  end;

var
  SettingsPanel: TPanel;
begin
  if not IsProjectOptionsForm(_Form) then begin
    if Assigned(FControlCallbackHandle) then begin
      TIDEFormEnhancements.UnregisterControlChangeCallback(FControlCallbackHandle);
      FControlCallbackHandle := nil;
    end;
    Exit;
  end;

//  FControlCallbackHandle := TIDEFormEnhancements.RegisterControlChangeCallback(HandleControlChanged);
  if not TryGetSettingsPanel(_Form, SettingsPanel) then
    exit;

  TryHookCombo(SettingsPanel, 0, 'HostAppInput');
  TryHookCombo(SettingsPanel, 2, 'CWDInput');
  TryHookCombo(SettingsPanel, 3, 'SourcePathInput');
end;

function TProjectOptionsEnhancer.IsProjectOptionsForm(_Form: TCustomForm): Boolean;
begin
  Result := (_Form.ClassName = 'TDelphiProjectOptionsDialog') and (_Form.Name = 'DelphiProjectOptionsDialog');
end;

initialization
finalization
  TGxIdeProjectOptionsEnhancer.SetEnabled(False);
end.

