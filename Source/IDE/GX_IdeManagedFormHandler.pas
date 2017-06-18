unit GX_IdeManagedFormHandler;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  ExtCtrls,
  Controls,
  GX_IdeManagedForm;

type
  TStandardFormChanges = (sfcFixForm, sfcMakeSizable, sfcStoreSize, sfcStorePosition,
    sfcStoreSplitter, sfcResizePicDialogs, sfcDropDownCount, sfcEnhanceForm);
  TStandardFormChangesSet = set of TStandardFormChanges;

type
  TManagedFormHandler = class
  protected
    FFormClassName: string;
    FFormCaption: string;
    FSupportedChanges: TStandardFormChangesSet;
    FEnabledChanges: TStandardFormChangesSet;
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; virtual;
  public
    constructor Create(const _FormClassName, _FormCaption: string; _Supported: TStandardFormChangesSet);
    ///<summary>
    /// @returns A description for displaying in the configuration grid.
    ///          Usually it is the managed form's caption. </summary>
    function Description: string; virtual;
    ///<summary>
    /// @returns The class name of the form this class manages. </summary>
    function FormClassname: string; virtual;
    ///<summary>
    /// @returns A string describing the fixes (if any) applied to the managed form.
    ///          May be multi line. </summary>
    function FormFixes: string; virtual;
    ///<summary>
    /// @returns A string describing the enhancements (in addition to the default position and
    ///          sizing enhancements) this class applis to the managed form. May be multi line. </summary>
    function FormEnhancements: string; virtual;
    ///<summary>
    /// @returns a set of TStandardFormChanges supported by this class </summary>
    function SupportedChanges: TStandardFormChangesSet; virtual;
    ///<summary>
    /// @returns true, if this object wants to manage the given form. </summary>
    function IsDesiredForm(_Form: TCustomForm): Boolean; virtual;
    ///<summary>
    /// @returns true, if the given Form has already been handled </summary>
    function HasBeenHandled(_Form: TCustomForm): Boolean;
    ///<summary>
    /// Called when the form for which IsDesiredForm returned true becomes the current form.
    /// Note: It's possible that the form has already been changed, so check this first! </summary>
    procedure Execute(_Form: TCustomForm);
    ///<summary>
    /// The subset of SupportedChanges that are enabled </summary>
    property EnabledChanges: TStandardFormChangesSet read FEnabledChanges write FEnabledChanges;
  end;

type
  TManagedFormHandlerFactory = class
  public
    ///<summary>
    /// register a TManagedFormHandler </summary>
    class procedure RegisterHandler(_Handler: TManagedFormHandler);
    ///<summary>
    /// @returns the registered handler count </summary>
    class function Count: Integer;
    ///<summary>
    /// @returns the Idxth registered handler </sumary>
    class function GetHandler(_Idx: Integer): TManagedFormHandler;
    ///<summary>
    /// Adds all TManagedFormHandlers for the given Form to the list </summary>
    class procedure GetHandlers(_Form: TCustomForm; _List: TList);
  end;

implementation

uses
  GX_dzClassUtils;

var
  gblFormHandlers: TList = nil;

function GetFormHandlers: TList;
begin
  if not Assigned(gblFormHandlers) then
    gblFormHandlers := TList.Create;
  Result := gblFormHandlers;
end;

{ TManagedFormHandlerFactory }

class function TManagedFormHandlerFactory.Count: Integer;
begin
  Result := GetFormHandlers.Count;
end;

class function TManagedFormHandlerFactory.GetHandler(_Idx: Integer): TManagedFormHandler;
begin
  Result := GetFormHandlers.Items[_Idx];
end;

class procedure TManagedFormHandlerFactory.GetHandlers(_Form: TCustomForm; _List: TList);
var
  Handler: TManagedFormHandler;
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Handler := GetHandler(i);
    if Handler.IsDesiredForm(_Form) then
      _List.Add(Handler);
  end;
end;

class procedure TManagedFormHandlerFactory.RegisterHandler(_Handler: TManagedFormHandler);
begin
  GetFormHandlers.Add(_Handler);
end;

{ TManagedFormHandler }

constructor TManagedFormHandler.Create(const _FormClassName, _FormCaption: string;
  _Supported: TStandardFormChangesSet);
begin
  FSupportedChanges := _Supported;
  inherited Create;
  FFormClassName := _FormClassName;
  FFormCaption := _FormCaption;
  EnabledChanges := SupportedChanges - [sfcStorePosition];
end;

function TManagedFormHandler.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedForm.Create(_Form);
end;

function TManagedFormHandler.Description: string;
begin
  Result := FFormCaption;
end;

procedure TManagedFormHandler.Execute(_Form: TCustomForm);
var
  frm: TManagedForm;
  Changes: TFormChanges;
begin
  if HasBeenHandled(_Form) then
    Exit;

  frm := CreateManagedForm(_Form);
  Changes.MakeResizable := (sfcMakeSizable in FEnabledChanges);
  Changes.RememberSize := (sfcStoreSize in FEnabledChanges);
  Changes.RememberWidth := (sfcStoreSize in FEnabledChanges);
  Changes.RememberPosition := (sfcStorePosition in FEnabledChanges);
  Changes.RememberSplitterPosition := (sfcStoreSplitter in FEnabledChanges);
  Changes.ResizePictureDialogs := (sfcResizePicDialogs in FEnabledChanges);
  if sfcDropDownCount in FEnabledChanges then
    Changes.ComboDropDownCount := 15
  else
    Changes.ComboDropDownCount := 0;
  frm.Init(Changes);
end;

function TManagedFormHandler.FormClassname: string;
begin
  Result := FFormClassName;
end;

function TManagedFormHandler.FormEnhancements: string;
begin
  Result := '';
end;

function TManagedFormHandler.FormFixes: string;
begin
  Result := '';
end;

function TManagedFormHandler.HasBeenHandled(_Form: TCustomForm): Boolean;
var
  cmp: TComponent;
begin
  Result := TComponent_FindComponent(_Form, TManagedForm.GenerateName(_Form.Name), False, cmp, TManagedForm);
end;

function TManagedFormHandler.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  Result := SameText(_Form.ClassName, FFormClassName);
end;

function TManagedFormHandler.SupportedChanges: TStandardFormChangesSet;
begin
  Result := FSupportedChanges;
end;

type
  TManagedFormHandlerSrchDialog = class(TManagedFormHandler)
  protected
{.$IFNDEF GX_VER150_up} // Delphi 7
    // only for Delphi 6 do we need to make the form components sizeable
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
{.$ENDIF}
  end;

type
  TManagedFormHandlerRplcDialog = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  end;

type
  TManagedFormGotoDialog = class(TManagedFormHandler)
  public
    function FormEnhancements: string; override;
  end;

type
  TManagedFormHandlerDefaultEnvironmentDialog = class(TManagedFormHandler)
    // Tools -> Options (Delphi 2007)
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  end;

type
  TManagedFormHandlerSearchPath = class(TManagedFormHandler)
  public
    function FormEnhancements: string; override;
  end;

type
  TManagedFormHandlerImageListEditor = class(TManagedFormHandler)
{.$IFNDEF GX_VER150_up} // Delphi 7
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
{.$ENDIF}
  end;

type
  TManagedFormHandlerPictureEditDlg = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  end;

type
  TManagedFormHandlerProjectOptionsDialog = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  end;

type
  TManagedFormBuildEventPropEditor = class(TManagedFormHandler)
  public
    function FormEnhancements: string; override;
  end;

type
  TManagedFormHandlerPasEnvironmentDialog = class(TManagedFormHandler)
{.$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
{.$ENDIF}
  end;

type
  TManagedFormHandlerConnEditForm = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  public
    function FormFixes: string; override;
  end;

type
  TManagedFormHandlerPakComponentsDlg = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  end;

type
  TManagedFormFixFormPositioningOnly = class(TManagedFormHandler)
  public
    function FormFixes: string; override;
  end;

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
type
  TManagedFormHandlerAboutBox = class(TManagedFormHandler)
  protected
    function CreateManagedForm(_Form: TCustomForm): TManagedForm; override;
  public
    function FormFixes: string; override;
    // remove scroll bar
  end;
{$ENDIF}
{$ENDIF}

type
  TManagedFormHandlerEditorProperties = class(TManagedFormHandler)
    // todo: make the controls sizeable
  end;

{ TManagedFormHandlerSrchDialog }

{.$IFNDEF GX_VER150_up} // Delphi 7
// only for Delphi 6 do we need to make the form sizeable

function TManagedFormHandlerSrchDialog.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  // do not call inherited, we need a special TManagedFormDescendant
  Result := TManagedFormSrchDialog.Create(_Form);
end;
{.$ENDIF}

{ TManagedFormFixFormPositioningOnly }

function TManagedFormFixFormPositioningOnly.FormFixes: string;
begin
  Result := 'Fix form positioning in multi monitor setup.';
end;

{ TManagedFormConnEditForm }

function TManagedFormHandlerConnEditForm.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormConnEditForm.Create(_Form);
end;

function TManagedFormHandlerConnEditForm.FormFixes: string;
begin
  Result := 'Fix positioning of Build Connection String form in multi monitor setup.';
end;

{ TManagedFormHandlerSearchPath }

function TManagedFormHandlerSearchPath.FormEnhancements: string;
begin
  Result := 'Drag and drop for directories'#13#10
    + 'Autocomplete for directories'#13#10
    + 'Memo for direct editing'#13#10
    + 'Make absolute / relative buttons'#13#10
    + 'Keyboard shortcuts for move up/down'#13#10
    + 'Favorite directories';
end;

{ TManagedFormBuildEventPropEditor }

function TManagedFormBuildEventPropEditor.FormEnhancements: string;
begin
  Result := 'Add favorites.';
end;

{ TManagedFormGotoDialog }

function TManagedFormGotoDialog.FormEnhancements: string;
begin
  Result := 'List of frequently used source positions';
end;

procedure Reg(_ManagedFormhandler: TManagedFormHandler);
begin
  TManagedFormHandlerFactory.RegisterHandler(_ManagedFormhandler);
end;

procedure InitializeFormHandlers;
begin
  // ** File menu **

{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
  // File -> New -> Other
  // fix for RSP-13229: File -> New -> Other opens on different monitor
  // which occurs in Delphi 2009/2010/XE-XE8/10
  Reg(TManagedFormFixFormPositioningOnly.Create(
    'TGalleryBrowseDlg', 'New Items (Gallery)', [sfcFixForm]));
{$ENDIF}
{$ENDIF}

{$IFDEF GX_VER210_up} // RAD Studio 2010 (15; BDS 7)
  // Delphi 2010/XE-XE8/10-10.2:
  // File -> Reopen -> Properties
  Reg(TManagedFormHandler.Create(
    'TReopenMenuPropertiesDialog', 'Reopen Menu Properties',
    [sfcStorePosition]));
{$ENDIF}

  // ** Search menu **

  // Delphi 6/7/?/2007/?: Search -> Find
  Reg(TManagedFormHandlerSrchDialog.Create(
    'TSrchDialog', 'Find Text',
    [
{$IFNDEF GX_VER150_up} // Delphi 7
    sfcMakeSizable, // already resizeable in Delphi 7 and later
{$ENDIF}
    sfcStoreSize, sfcStorePosition, sfcDropDownCount]));

  // Delphi 6/7/?/2007/?: Search -> Replace
  Reg(TManagedFormHandlerRplcDialog.Create(
    'TRplcDialog', 'Replace Text',
    [
{$IFNDEF GX_VER150_up} // Delphi 7
    sfcMakeSizable, // already resizeable in Delphi 7 and later
{$ENDIF}
    sfcStoreSize, sfcStorePosition, sfcDropDownCount]));

  Reg(TManagedFormGotoDialog.Create(
    'TGotoLineDialog', 'Go to Line Number', [sfcEnhanceForm]));

  // ** Project menu **

  // Project -> Options *and* Components -> Install Packages
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  // Delphi 6/7/2005/2006: Project -> Options *and* Components -> Install Packages
  Reg(TManagedFormHandlerProjectOptionsDialog.Create(
    'TProjectOptionsDialog', 'Project Options',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));
{$ELSE}
  // Delphi 2007/2009/2010: Project -> Options *and* Run -> Parameters *and* Components -> Install Packages
  // for whatever reason, Borland didn't make that dialog sizeable in Delphi 2007 (it is in Delphi 2009 and later)
  // changing the dropdown count seems to result in intermittent "invalid pointer operation" exceptions in Delphi 10.1
  Reg(TManagedFormHandler.Create(
    'TDelphiProjectOptionsDialog', 'Project Options', [
{$IFNDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
    sfcMakeSizable,
{$ENDIF}
    sfcStoreSize, sfcStorePosition]));
{$ENDIF}

{$IFDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  Reg(TManagedFormBuildEventPropEditor.Create(
{$IFDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
    'TBuildEventPropEditor',
{$ELSE}
    'TBuildEventEditor',
{$ENDIF}
    'Build Event Editor', [sfcEnhanceForm]));
{$ENDIF}

{$IFDEF GX_VER200_up} // RAD Studio 2009 (14; BDS 6)
{$IFNDEF GX_VER310_up} // RAD Studio 10.1 Berlin (25; BDS 18)
  // RSP-13230: on dual monitor Project -> Resources and images gets shown on primary monitor
  // which occur in Delphi 2009/2010/XE-XE8/10
  Reg(TManagedFormFixFormPositioningOnly.Create(
    'TProjectResourcesDlg', 'Resources for <project>', [sfcFixForm]));
{$ENDIF}
{$ENDIF}

  // Projects -> Options -> Conditional Defines, Search Path, Unit Aliases, Unit Scope Names
  // ?: Search Path editor dialog
  Reg(TManagedFormHandlerSearchPath.Create(
    'TInheritedListEditDlg', 'Search Path dialog', [sfcStoreSize, sfcStorePosition, sfcDropDownCount, sfcEnhanceForm]));

  // ** Run menu **

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
  // Delphi 2005/2006/2007/? Run -> Load Process
  Reg(TManagedFormHandler.Create(
    'TLoadProcessDialog', 'Load Process',
    [
{$IFNDEF GX_VER230_up} // RAD Studio XE 2 (17; BDS 9)
    // make sizeable in Delphi 2005/2006/2007/2009/2010
    sfcMakeSizable,
{$ENDIF}
    sfcStoreSize, sfcStorePosition, sfcStoreSplitter, sfcDropDownCount]));
{$ENDIF}

  // ** Component menu **

  // Delphi 6/7/2005-2010/XE-XE8/10/10.1/10.2: Component -> Install Packages -> Components
  Reg(TManagedFormHandlerPakComponentsDlg.Create(
    'TPakComponentsDlg', 'Components (in Package)',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition]));

  // ** Tools menu **

{$IFNDEF GX_VER160_up} // Delphi 8 (BDS 1)
  // Delphi 6/7: Tools -> Editor Options
  Reg(TManagedFormHandlerEditorProperties.Create(
    'TPasEditorPropertyDialog', 'Editor Properties',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));
{$ENDIF}

{$IFDEF GX_BCB}
  // C++ Builder? Others?
  Reg(TManagedFormHandler.Create(
    'TCppProjOptsDlg', 'TCppProjOptsDlg', [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));
{$ENDIF}

{$IFDEF GX_VER160_up} // Delphi 8 (BDS 1)
{$IFNDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
  // Delphi 8?
  Reg(TManagedFormHandler.Create(
    'TDotNetOptionForm', '.Net Options',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));
{$ENDIF}
{$ENDIF}

{$IFNDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
  // Delphi 6/7: Tools -> Environment Options
  Reg(TManagedFormHandlerPasEnvironmentDialog.Create(
    'TPasEnvironmentDialog', 'Environment Options', [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));
{$ELSE}
  // Delphi 2005/2006/2007/?: Tools -> Options
  // changing the dropdown count seems to result in intermittent "invalid pointer operation" exceptions in Delphi 10.1
  Reg(TManagedFormHandlerDefaultEnvironmentDialog.Create(
    'TDefaultEnvironmentDialog', 'Options',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcStoreSplitter]));
{$ENDIF}

  // Delphi 6/7/2005-2007:
  // Project -> Options -> Directories/Conditionals -> Search Path, Debug Source Path, Cond. Defines, Unit Aliases, Namespace Prefixes
  // (from Delphi 2009 on this is a TInheritedListEditDlg)
  // Delphi 6/7/2005-2010/XE-XE4/?:
  // Tools -> (Environment) Options -> Library Path, Debug Library Path, Browsing Path
  Reg(TManagedFormHandlerSearchPath.Create(
    'TOrderedListEditDlg', 'Directories Dialog', [sfcStoreSize, sfcStorePosition, sfcEnhanceForm]));

  // ** Property Editors **

  // Delphi 6/7/2005-2007/?: TImageList -> ImageList Editor
  Reg(TManagedFormHandlerImageListEditor.Create(
    'TImageListEditor', 'ImageList Editor',
    [
{$IFNDEF GX_VER150_up} // Delphi 7
    // Delphi 6:
    sfcMakeSizable,
{$ENDIF}
    sfcStoreSize, sfcStorePosition, sfcResizePicDialogs, sfcDropDownCount]));

  // Delphi 6/7/2005-2010/XE-XE8/10-10.2: TImage -> Picture -> ...-Button
  Reg(TManagedFormHandlerPictureEditDlg.Create(
    'TPictureEditorDlg', 'Picture Editor',
    [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcResizePicDialogs]));

  // Delphi 6/7/?/2007/? TActionlist -> Action List Editor
  Reg(TManagedFormHandler.Create(
    'TActionListDesigner', 'Action List Editor', [sfcStoreSize, sfcStorePosition]));

  //Delphi 6/7/? TTable / TQuery etc. -> Fields Editor
  Reg(TManagedFormHandler.Create(
    'TFieldsEditor', 'Fields Editor', [sfcStoreSize, sfcStorePosition]));

  // Delphi 6/7
  // 2005 .. 10.2 already stores the size, but not the position
  Reg(TManagedFormHandler.Create(
    'TDBGridColumnsEditor', 'DBGrid Columns Editor', [
{$IFNDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
    sfcStoreSize,
{$ENDIF}
    sfcStorePosition
      ]));

  // I don't remember where I found this form.
  // Connection String Editor in newer versions?
  // It's declared in the DBExpress package, but I could no longer find a way to open it in the IDE
  Reg(TManagedFormHandler.Create(
    'TDriverSettingsForm', 'Driver Settings', [sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));

  // Delphi 6/7/2005-2010/XE-XE8/10-10.2: Ado Connection String
  Reg(TManagedFormHandlerConnEditForm.Create(
    'TConnEditForm', 'Connection String Editor',
    [sfcFixForm, sfcMakeSizable, sfcStoreSize, sfcStorePosition, sfcDropDownCount]));

  // Delphi 6/7/?: TMainMenu / TPopupMenu -> Menu Designer
  Reg(TManagedFormHandler.Create(
    'TMenuBuilder', 'Menu Designer', [sfcStoreSize, sfcStorePosition]));

  // ** Help menu **

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)
  Reg(TManagedFormHandlerAboutBox.Create(
    'TAboutBox', 'About Box', []));
{$ENDIF}
{$ENDIF}

  // ** not in menu **

{$IFDEF not_defined_symbol}
  // Ctrl+B added by the Buffer List enhancement module listed in
  // Tools -> Options -> Editor Options -> Key Mappings -> Enhancement Modules
  // source code is also available, at least in Delphi 6/7 and 2005 in the Demos folder
  // of the installation disk.
  Reg(TManagedFormHandler.Create(
    'TBufferListFrm', 'Buffer List', [sfcStoreSize, sfcStorePosition]));
{$ENDIF}

  // todo:
  // Delphi 6: TDebuggerOptDialog Tools -> Debugger Options

  // Delphi 6: TPalettePropertyDialog Component -> Configure Palette

  // Delphi 6: Project -> Import Type Library
end;

procedure FinalizeFormHandlers;
var
  i: Integer;
begin
  if Assigned(gblFormHandlers) then
    for i := 0 to gblFormHandlers.Count - 1 do begin
      TObject(gblFormHandlers[i]).Free;
    end;
  FreeAndNil(gblFormHandlers);
end;

{ TManagedFormHandlerRplcDialog }

function TManagedFormHandlerRplcDialog.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormRplcDialog.Create(_Form);
end;

{ TManagedFormHandlerPictureEditDlg }

function TManagedFormHandlerPictureEditDlg.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormPictureEditDlg.Create(_Form);
end;

{ TManagedFormHandlerImageListEditor }

{.$IFNDEF GX_VER150_up} // Delphi 7

function TManagedFormHandlerImageListEditor.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormImageListEditor.Create(_Form);
end;
{.$ENDIF}

{ TManagedFormHandlerDefaultEnvironmentDialog }

function TManagedFormHandlerDefaultEnvironmentDialog.CreateManagedForm(
  _Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormDefaultEnvironmentDialog.Create(_Form);
end;

{ TManagedFormHandlerPakComponentsDlg }

function TManagedFormHandlerPakComponentsDlg.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormPakComponentsDlg.Create(_Form);
end;

{ TManagedFormHandlerPasEnvironmentDialog }

function TManagedFormHandlerPasEnvironmentDialog.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormPasEnvironmentDialog.Create(_Form);
end;

{ TManagedFormHandlerProjectOptionsDialog }

function TManagedFormHandlerProjectOptionsDialog.CreateManagedForm(
  _Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormProjectOptionsDialog.Create(_Form);
end;

{ TManagedFormHandlerAboutBox }

{$IFDEF GX_VER170_up} // Delphi 9/2005 (BDS 2)
{$IFNDEF GX_VER185_up} // Delphi 2007 (11; BDS 4)

function TManagedFormHandlerAboutBox.CreateManagedForm(_Form: TCustomForm): TManagedForm;
begin
  Result := TManagedFormAboutBox.Create(_Form);
end;

function TManagedFormHandlerAboutBox.FormFixes: string;
begin
  Result := 'Remove horizontal scroll bar from installed products list.';
end;
{$ENDIF}
{$ENDIF}

initialization
  InitializeFormHandlers;
finalization
  FinalizeFormHandlers;
end.
