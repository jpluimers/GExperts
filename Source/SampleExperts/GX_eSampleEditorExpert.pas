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
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  GX_Experts, GX_EditorExpert, GX_ConfigurationInfo, GX_BaseForm;

type
  ///<summary>
  /// Use this sample editor expert as a starting point for your own expert.
  /// Do not forget to rename the unit and the class(es).
  /// Many of the methods are optional and you can omit them if the default
  /// behaviour is suitable for your expert. </summary>
  TGxSampleEditorExpert = class(TEditorExpert)
  private
    FData: string;
  public
    // optional, defaults to ClassName
    class function GetName: string; override;
    constructor Create; override;
    // optional, defauls to true
    function CanHaveShortCut: boolean; override;
    // optional if HasConfigOptions returns false
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    // optional, defaults to GetName which in turn defaults to the class name
    function GetBitmapFileName: string; override;
    // optional, defaults to no shortcut
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // optional, default to true
    function HasConfigOptions: Boolean; override;
    // Overrride to load any configuration settings
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    // Overrride to save any configuration settings
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
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
//    Name: TGxSampleEditorExpert.CanHaveShortCut
// Purpose: Determines whether this expert can have a
//          hotkey assigned to it
//    Note: If it returns false, no hotkey configuration
//          control is shown in the configuratoin dialog.
//          If your expert can have a hotkey, you can
//          simply delete this function, since the
//          inherited funtion already returns true.
//*********************************************************
function TGxSampleEditorExpert.CanHaveShortCut: boolean;
begin
  Result := True;
end;

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
//*********************************************************
constructor TGxSampleEditorExpert.Create;
begin
  inherited Create;

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
//    Name: TGxSampleEditorExpert.GetBitmapFileName
// Purpose: Return the file name of an icon associated with
//          the expert. Do not specify a path.
//          Defaults to the expert's class name.
//    Note: This bitmap must be included in the
//          GXIcons.rc file which in turn can be created
//          from all .bmp files located in the Images
//          directory by calling the _CreateGXIconsRc.bat
//          script located in that directory.
//          It is possible to return an empty string. This
//          signals that no icon file is available.
//          You can remove this function from your expert
//          and simply provide the bitmap as
//          <TYourExpert>.bmp
//*********************************************************
function TGxSampleEditorExpert.GetBitmapFileName: string;
begin
  Result := inherited GetBitmapFileName;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.GetDefaultShortCut
// Purpose: The default shortcut to call your expert.
//   Notes: It is perfectly fine not to assign a default
//          shortcut and let the expert be called via
//          the menu only. The user can always assign
//          a shortcut to it in the configuration dialog.
//          Available shortcuts have become a very rare
//          resource in the Delphi IDE.
//          The value of ShortCut is touchy, use the ShortCut
//          button on the Editor Experts tab of menu item
//          GExperts/GExperts Configuration... on an existing
//          editor expert to see if you can use a specific
//          combination for your expert.
//*********************************************************
function TGxSampleEditorExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Ord('?'), [ssCtrl, ssAlt]);
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
//          does to the expert description hint that is shown
//          when the user puts the mouse over the expert's icon
//          in the configuration dialog.
//*********************************************************
function TGxSampleEditorExpert.GetHelpString: string;
resourcestring
  SSampleEditorExpertHelp =
    'This is the text that will appear in the explanation hint on the ' +
    'Editor tab of menu item GExperts/GExperts Configuration...' + sLineBreak +
    sLineBreak +
    'You can allow the text to wrap automatically, or you can force your ' +
    'own line breaks, or both.';
begin
  Result := SSampleEditorExpertHelp;
end;

//*********************************************************
//    Name: TGxSampleEditorExpert.GetName
// Purpose: Each editor expert needs to provide a name
//          that represents this editor expert.
//    Note: This string will be used to construct an action
//          name and therefore must be a valid identifier.
//          The inherited implementation returns the
//          expert's class name. This is usually fine
//          as long as it is unique within GExperts.
//          Feel free to omit this method from your expert.
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

procedure TGxSampleEditorExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);

  FData := Settings.ReadString('TestData', FData);
end;

procedure TGxSampleEditorExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);

  Settings.WriteString('TestData', FData);
end;

//*******************************************************************
// Purpose: Tells GExperts about the existance of this editor expert
//*******************************************************************
initialization
  RegisterEditorExpert(TGxSampleEditorExpert);
end.
