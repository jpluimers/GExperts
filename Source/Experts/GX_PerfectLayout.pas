unit GX_PerfectLayout;

{$I GX_CondDefine.inc}

interface

uses Forms, StdCtrls, Controls, ExtCtrls, Classes;

type
  TfmPerfectLayout = class(TForm)
    gbxLayout: TGroupBox;
    pnlLayout1: TPanel;
    shpMain1: TShape;
    shpOI1: TShape;
    shpEditor1: TShape;
    shpWatch1: TShape;
    lblWatch1: TLabel;
    lblMain1: TLabel;
    lblOI1: TLabel;
    lblEditor1: TLabel;
    pnlLayout2: TPanel;
    shpMain2: TShape;
    shpOI2: TShape;
    shpEditor2: TShape;
    shpWatch2: TShape;
    lblWatch2: TLabel;
    lblMain2: TLabel;
    lblOI2: TLabel;
    lblEditor2: TLabel;
    rbnLayout1: TRadioButton;
    rbnLayout2: TRadioButton;
    rbnCustom: TRadioButton;
    btnCustom: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure btnCustomClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses SysUtils, Windows, Menus, Dialogs,
  GX_Experts, GX_ConfigurationInfo, GX_GenericUtils,
  GX_IdeUtils, GX_GxUtils;

type
  TLayoutType = (ltLayout1, ltLayout2, ltCustom);

  TLayoutExpert = class(TGX_Expert)
  private
    FLayoutType: TLayoutType;
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    class function FormsConfigurationKey: string;
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    function IsDefaultActive: Boolean; override;
    property LayoutType: TLayoutType read FLayoutType write FLayoutType;
  end;

function IsValidMoveableForm(const Form: TCustomForm; const AllowParented: Boolean): Boolean;
begin
  Result := False;

  if (Form = nil) then
    Exit;

  // The OI can be repositioned when parented to a docking host in D4+
  if (not AllowParented) and (Form.Parent <> nil) then
    Exit;

  // Don't save the state of the two GExperts configuration dialogs
  if (Form.Name = 'fmConfiguration') or (Form.Name = 'fmPerfectLayout') then
    Exit;

  // Don't save project forms or invisible forms
  if (Form.Designer <> nil) or (not Form.Visible) then
    Exit;

  Result := True;
end;

procedure TfmPerfectLayout.btnCustomClick(Sender: TObject);

  procedure SaveWindow(const Form: TForm; const Settings: TGExpertsSettings);
  begin
    Assert(Assigned(Form));
    Assert(Assigned(Settings));

    // Don't save the state of the two GExperts configuration dialogs
    if (Form.Name = 'fmConfiguration') or (Form.Name = 'fmPerfectLayout') then
      Exit;
    // Don't save project forms or invisible forms
    if (Form.Designer <> nil) or (not Form.Visible) then
      Exit;
    Settings.WriteInteger(Form.Name, 'Left', Form.Left);
    Settings.WriteInteger(Form.Name, 'Top', Form.Top);
    Settings.WriteInteger(Form.Name, 'Height', Form.Height);
    Settings.WriteInteger(Form.Name, 'Width', Form.Width);
  end;

var
  Settings: TGExpertsSettings;
  i: Integer;
  AForm: TForm;
  Key: string;
begin
  // Do not localize any of the items below
  Settings := TGExpertsSettings.Create;
  try
    Settings.DeleteKey(ConfigInfo.GExpertsIdeRootRegistryKey, TLayoutExpert.FormsConfigurationKey);
  finally
    FreeAndNil(Settings);
  end;

  Key := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + TLayoutExpert.FormsConfigurationKey;
  Settings := TGExpertsSettings.Create(Key);
  try
    for i := 0 to Screen.FormCount - 1 do
    begin
      AForm := Screen.Forms[i];
      if IsValidMoveableForm(AForm, False) then
        SaveWindow(AForm, Settings);
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmPerfectLayout.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 25);
end;

{ TPerfectLayoutExpert }

constructor TLayoutExpert.Create;
begin
  inherited Create;
  ShortCut := Menus.ShortCut(Word('L'), [ssCtrl, ssShift]);
end;

function TLayoutExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Perfect La&yout';
begin
  Result := SMenuCaption;
end;

class function TLayoutExpert.GetName: string;
begin
  Result := 'PerfectLayout';  // Do not localize.
end;

procedure TLayoutExpert.Click(Sender: TObject);

  procedure LoadCustomLayout;

    procedure LoadWindow(const Form: TForm; const Settings: TGExpertsSettings);
    begin
      Assert(Assigned(Form));
      Assert(Assigned(Settings));

      Form.WindowState := wsNormal;
      // Do not localize any of the below items.
      Form.Left := Settings.ReadInteger(Form.Name, 'Left', Form.Left);
      Form.Top := Settings.ReadInteger(Form.Name, 'Top', Form.Top);
      Form.Height := Settings.ReadInteger(Form.Name, 'Height', Form.Height);
      Form.Width := Settings.ReadInteger(Form.Name, 'Width', Form.Width);
    end;

  var
    Settings: TGExpertsSettings;
    i: Integer;
    AForm: TForm;
    Key: string;
  begin
    Key := AddSlash(ConfigInfo.GExpertsIdeRootRegistryKey) + FormsConfigurationKey;
    Settings := TGExpertsSettings.Create(Key);
    try
      for i := 0 to Screen.FormCount - 1 do
      begin
        AForm := Screen.Forms[i];
        if IsValidMoveableForm(AForm, False) then
          LoadWindow(AForm, Settings);
      end;
    finally
      FreeAndNil(Settings);
    end;
  end;

  function FindForm(const Name: string; AllowParented: Boolean): TForm;
  var
    App: TCustomForm;
  begin
    Result := Application.FindComponent(Name) as TForm;
    if Result = nil then
    begin
      App := GetIdeMainForm;
      if App <> nil then
        Result := App.FindComponent(Name) as TForm;
    end;
    if not IsValidMoveableForm(Result, AllowParented) then
      Result := nil;
  end;


resourcestring
  SCouldNotFindAppBuilder = 'Could not find IDE AppBuilder window.';
var
  OI, Watch, Editor: TCustomForm;
  App: TCustomForm;
  Left, Width, Top, Bottom: Integer;
  R: TRect;
begin
  if LayoutType = ltCustom then
  begin
    LoadCustomLayout;
    Exit;
  end;

  App := GetIdeMainForm;
  R := GetScreenWorkArea(App);
  if App = nil then
  begin
    MessageDlg(SCouldNotFindAppBuilder, mtError, [mbOK], 0);
    Exit;
  end;
  App.WindowState := wsNormal;
  App.SetBounds(R.Left, R.Top, R.Right-R.Left, App.Height);
  Top := R.Top + App.Height;
  Bottom := R.Bottom;
  Left := R.Left;
  Width := R.Right - R.Left;
  Watch := FindForm('WatchWindow', False);  // Do not localize.
  if Watch <> nil then
  begin
    if Watch.Visible and Watch.Floating then
    begin
      Watch.WindowState := wsNormal;
      Watch.SetBounds(R.Left, R.Bottom - Watch.Height, R.Right - R.Left, Watch.Height);
      Bottom := Watch.Top;
    end;
  end;
  OI := FindForm('PropertyInspector', True); // Do not localize.
  if OI <> nil then
  begin
    // In case the OI is docked:
    if GetParentForm(OI) <> nil then
      OI := GetParentForm(OI);
    if OI <> App then begin // If the OI isn't docked into the main window
      OI.Top := Top;
      OI.Height := Bottom - Top;
      if LayoutType = ltLayout1 then
      begin
        OI.Left := R.Left;
        Left := OI.Left + OI.Width;
      end
      else
      begin
        OI.Left := R.Right - R.Left - OI.Width;
        Left := R.Left;
      end;
      Width := R.Right - R.Left - OI.Width;
    end;
  end;
  Editor := FindForm('EditWindow_0', False); // Do not localize.
  if Editor <> nil then
  begin
    Editor.WindowState := wsNormal;
    Editor.SetBounds(Left, Top, Width, Bottom - Top);
  end;
end;

procedure TLayoutExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the below items
  FLayoutType := TLayoutType(Settings.ReadEnumerated(ConfigurationKey, 'Layout', TypeInfo(TLayoutType), Ord(ltLayout1)));
end;

procedure TLayoutExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the below items
  Settings.WriteEnumerated(ConfigurationKey, 'Layout', TypeInfo(TLayoutType), Ord(FLayoutType));
end;

procedure TLayoutExpert.Configure;
var
  Dlg: TfmPerfectLayout;
begin
  Dlg := TfmPerfectLayout.Create(nil);
  try
    Dlg.rbnLayout1.Checked := (FLayoutType = ltLayout1);
    Dlg.rbnLayout2.Checked := (FLayoutType = ltLayout2);
    Dlg.rbnCustom.Checked := (FLayoutType = ltCustom);
    if Dlg.ShowModal = mrOk then
    begin
      if Dlg.rbnLayout1.Checked then
        FLayoutType := ltLayout1
      else
      if Dlg.rbnLayout2.Checked then
        FLayoutType := ltLayout2
      else
      if Dlg.rbnCustom.Checked then
        FLayoutType := ltCustom;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TLayoutExpert.IsDefaultActive: Boolean;
begin
  Result := False; // Delphi 5+ provide more powerful saved desktops
end;

class function TLayoutExpert.FormsConfigurationKey: string;
begin
  Result := AddSlash(ConfigurationKey) + 'Forms';
end;

initialization
  RegisterGX_Expert(TLayoutExpert);

end.

