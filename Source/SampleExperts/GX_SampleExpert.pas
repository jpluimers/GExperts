{***************************************************************
 * Unit Name: GX_SampleExpert
 * Purpose  : Template for creation of new GExperts experts
 *          : Customize this when creating new experts
 *          : Add this unit to the GExperts .dpr and compile
 * Authors  : Stefan Hoffmeister, Scott Mattes, Erik Berry
 ****************************************************************}

unit GX_SampleExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_Experts, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TfmGxSampleExpertForm = class(TForm)
    btnOK: TButton;
    lblNote: TLabel;
    lblData: TLabel;
    edtData: TEdit;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, Menus, GX_GExperts, GX_ConfigurationInfo;

type
  TGxSampleExpert = class(TGX_Expert)
  private
    FSomeData: string;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Configure; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure Click(Sender: TObject); override;
  end;

{ TGxSampleExpert }

//*********************************************************
//    Name: TGxSampleExpert.Click
// Purpose: The action taken when the menu item is clicked
//*********************************************************
procedure TGxSampleExpert.Click(Sender: TObject);
begin
  with TfmGxSampleExpertForm.Create(nil) do
  try
    edtData.Text := FSomeData;
    if ShowModal = mrOk then
    begin
      FSomeData := edtData.Text;
      SaveSettings;
    end;
  finally
    Free;
  end;
end;

//*********************************************************
//    Name: TGxSampleExpert.Configure
// Purpose: Action taken when user clicks the Configure
//          button on the Experts tab of menu item GExperts/
//          GExperts Configuration...
//*********************************************************
procedure TGxSampleExpert.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button!';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

constructor TGxSampleExpert.Create;
begin
  inherited Create;

  // Assign a default value to your data
  FSomeData := 'Sample Data';

  // If desired, assign a default menu item shortcut
  ShortCut := Menus.ShortCut(Word('Z'), [ssCtrl, ssShift, ssAlt]);

  // Saved settings are loaded automatically for you by the ancestor
  // via the virtual LoadSettings method
end;

destructor TGxSampleExpert.Destroy;
begin
  // Free any created objects here
  inherited Destroy;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetActionCaption
// Purpose: Returns the string displayed on the GExperts
//          menu item
//*********************************************************
function TGxSampleExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Sample E&xpert...';
begin
  Result := SMenuCaption;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetName
// Purpose: Used to determine the unique keyword used to
// save the active state and shortcut into the registry
//*********************************************************
class function TGxSampleExpert.GetName: string;
begin
  Result := 'SampleExpert';
end;

//*********************************************************
//    Name: TGxSampleExpert.HasConfigOptions
// Purpose: This expert should have a configure button in
// 	the configuration dialog
//*********************************************************
function TGxSampleExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

//*********************************************************
//    Name: TGxSampleExpert.HasMenuItem
// Purpose: This expert has a visible menu item
//          in the GExperts top level menu
//*********************************************************
function TGxSampleExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

//*********************************************************
//    Name: TGxSampleExpert.LoadSettings
// Purpose: Gets the expert settings from the registry
//*********************************************************
procedure TGxSampleExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FSomeData := Settings.ReadString(ConfigurationKey, 'TestSetting', FSomeData);
end;

//*********************************************************
//    Name: TGxSampleExpert.SaveSettings
// Purpose: Saves the expert settings to the registry
// You must call this manually in the destructor
//*********************************************************
procedure TGxSampleExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  Settings.WriteString(ConfigurationKey, 'TestSetting', FSomeData);
end;

//********************************************************************
//    Name: TGxSampleExpert.SetActive
// Purpose: Called to clean up the expert when it is disabled at runtime
// or destroyed on shutdown.  Free any forms and objects here.
// The ancestor removes the menu item for you.
//********************************************************************
procedure TGxSampleExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Initialize anything necessary here (generally empty for modal experts)
    else
    begin
      // If we had a non-modal form, it would be destroyed here
      //FreeAndNil(FSampleForm);
    end;
  end;
end;

//*********************************************************
// Purpose: Lets GExperts know about this new expert
//*********************************************************
initialization
  RegisterGX_Expert(TGxSampleExpert);
end.

