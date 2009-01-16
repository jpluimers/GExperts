{***********************************************************************
 * Unit Name: GX_eSampleEditorExpert
 * Purpose  : To show one way of coding an Editor Expert.
 *            This expert puts up a modal window and does nothing else
 * Author   : Scott Mattes <smattes@erols.com>
 ***********************************************************************}

unit GX_eSampleEditorExpert;

{$I GX_CondDefine.inc}

interface
    
uses
  GX_Experts, GX_EditorExpert, GX_ConfigurationInfo,
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, GX_BaseForm;

type
  TGxSampleEditorExpert = class(TEditorExpert)
  private
    FData: string;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  end;

type
  TfmSampleEditorExpertConfig = class(TfmBaseForm)
    btnCancel: TButton;
    btnOK: TButton;
    lblData: TLabel;
    edtData: TEdit;
    lblNote: TLabel;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Menus, Registry,
  ToolsAPI, GX_GenericUtils,
  GX_GExperts;

{ TGxSampleEditorExpert }

//*********************************************************
//    Name: TGxSampleEditorExpert.Configure
// Purpose: Shows the config form and if the user selects
//          OK then the settings are saved
//    Note: If you have no config items, delete this method
//          (and its declaration from the interface section)
//          and the declaration of the form in the interface
//          section, and remove the and set FHasConfigOptions
//          to False in the Create method
//*********************************************************
procedure TGxSampleEditorExpert.Configure;
begin
  with TfmSampleEditorExpertConfig.Create(nil) do
  try
    edtData.Text := FData;
    if ShowModal = mrOk then
    begin
      FData := edtData.Text;
      SaveSettings;
    end;
  finally
    Free;
  end;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.Create
// Purpose: Sets up basic information about your editor expert
//   Notes: The value of ShortCut is touchy, use the ShortCut
//          button on the Editor Experts tab of menu item
//          GExperts/GExperts Configuration... on an existing
//          editor expert to see if you can use a specific
//          combination for your expert.
//*********************************************************
constructor TGxSampleEditorExpert.Create;
begin
  inherited Create;

  // The default shortcut key to activate your editor expert.
  ShortCut := scCtrl + scAlt + Ord('?');

  // Set default values for any data
  FData := 'editor data';

  // LoadSettings is called automatically for the expert upon creation.
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.Execute
// Purpose: Called when your hot-key is pressed, this is
//          where you code what the expert should do
//*********************************************************
procedure TGxSampleEditorExpert.Execute(Sender: TObject);
resourcestring
  SNotice = 'This is where your editor expert would do something useful.';
begin
  ShowMessage(SNotice);
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.DisplayName
// Purpose: The expert name that appears in Editor Experts box on the
//          Editor tab of menu item GExperts/GExperts Configuration...
//          Experts tab on menu item GExperts/GExperts Configuration...
//*********************************************************
function TGxSampleEditorExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Sample Editor Expert';
begin
  Result := SDisplayName;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.GetHelpString
// Purpose: To provide your text on what this editor expert
//          does to the expert description box on the Editor
//          Experts tab on menu item GExperts/GExperts Configuration...
//*********************************************************
procedure TGxSampleEditorExpert.GetHelpString(List: TStrings);
resourcestring
  SSampleEditorExpertHelp =
    'This is the text that will appear in the explanation box on the ' +
    'Editor tab of menu item GExperts/GExperts Configuration...' + sLineBreak +
    sLineBreak +
    'You can allow the text to wrap automatically, or you can force your ' +
    'own line breaks, or both.';
begin
  List.Text := SSampleEditorExpertHelp;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.GetName
// Purpose: Each editor expert needs to provide a name
//          that represents this editor expert.
//          An empty file name means that there is no
//          iconic representation.
//*********************************************************
class function TGxSampleEditorExpert.GetName: string;
const
  SName = 'SampleEditorExpert';
begin
  Result := SName;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.HasConfigOptions
// Purpose: Let the world know whether this expert has
//          configuration options.
//*********************************************************
function TGxSampleEditorExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TGxSampleEditorExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);

  FData := Settings.ReadString(ConfigurationKey, 'TestData', FData);
end;

procedure TGxSampleEditorExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);

  Settings.WriteString(ConfigurationKey, 'TestData', FData);
end;

//*******************************************************************
// Purpose: Tells GExperts about the existance of this editor expert
//*******************************************************************
initialization
  RegisterEditorExpert(TGxSampleEditorExpert);
end.
