unit GX_IdeUtils;

{ A number of utility functions that work directly on the
  IDE using mechanism provided by the VCL.

  None of the functions in this unit use the "official"
  way through the Open Tools API, as the Open Tools API
  does not deliver whatever is needed here.

  See GX_OtaUtils.pas for routines that make use of the
  officially exposed functionality. }

interface

{$I GX_CondDefine.inc}

uses
  Graphics, Forms, Menus, ComCtrls, Controls, Classes, StdCtrls;

const
  EditorFormClassName = 'TEditWindow';
  EditorControlName = 'Editor';
  EditorControlClassName = 'TEditControl';

// Returns a reference to the main form of the IDE (TAppBuilder)
function GetIdeMainForm: TCustomForm;
// Returns the height of a IDE main menu item
function GetMainMenuItemHeight: Integer;
// Returns the height of a standard OS menu item
function GetStandardMenuItemHeight: Integer;
// Returns a reference to the IDE's component palette tab control.
// May return nil.
function GetComponentPaletteTabControl: TTabControl;
// Returns a reference to the Object Inspector form.
// May return nil.
function GetObjectInspectorForm: TCustomForm;
function GetComponentPalettePopupMenu: TPopupMenu;
// Returns True of AForm is an editor form in the IDE,
// False otherwise. AForm may be nil.
function IsIdeEditorForm(AForm: TCustomForm): Boolean;
function IsEditControl(Control: TControl): Boolean;
// Get the actual TEditControl embedded in the given IDE editor form
function GetIDEEditControl(Form: TCustomForm): TWinControl;
function GetIDEFormNamed(const FormName: string): TCustomForm;
// We can cause internal editor kernel AVs if we change the editor text
// while the replace confirmation dialog is up, so we detect that case here
function IsReplaceConfirmDialogOnScreen: Boolean;

// Return the IDE's root directory (the installation directory).
// Returns an empty string if the information could not be retrieved.
function GetIdeRootDirectory: string;

///<summary>
/// Reads the current Desktop from the registry </summary>
function GetIdeDesktopName: string;

///<summary>
/// Sets the IDE desktop by changing it in the Destkop toolbar </summary>
procedure SetIdeDesktop(const _Desktop: string);

///<summary>
/// Returns a reference to the IDE desktop selection combobox's Items.
/// Use with care!!!! </summary>
function TryGetIdeDesktops(out _Items: TStrings): Boolean;

function TryGetDesktopCombo(out _cmb: TCombobox): Boolean;


// Return the IDE's version identifier, such as ENT, CSS, PRO, STD,
// or the empty string if unknown
function GetIdeEdition: string;

// Get the IDE's editor background color
function GetIdeEditorBackgroundColor: TColor;
function GetIdeEnvironmentOverrides(Overrides: TStrings): Boolean;

// Set the PopupMode property in Delphi 8+ to get the z-order/layering right
procedure SetNonModalFormPopupMode(Form: TCustomForm);
procedure SetModalFormPopupMode(Form: TCustomForm);

function GetIDEVersionID: string;

function RunningWindows: Boolean;

function RunningDelphi8: Boolean;
function RunningDelphi8OrGreater: Boolean;
function RunningDelphi7OrLess: Boolean;
function RunningDelphi7OrGreater: Boolean;
function RunningDelphi2005: Boolean;
function RunningBDS2006OrLess: Boolean;
function RunningBDS2006: Boolean;
function RunningBDS2006OrGreater: Boolean;
function RunningDelphi2007OrLess: Boolean;
function RunningDelphi2007: Boolean;
function RunningDelphi2007OrGreater: Boolean;
function RunningRS2009: Boolean;
function RunningRS2009OrGreater: Boolean;
function RunningRS2010OrGreater: Boolean;
function RunningRSXE: Boolean;
function RunningRSXEOrGreater: Boolean;

function RunningCPPBuilder: Boolean;
function IDEHasWelcomePage: Boolean;
function FileIsWelcomePage(const FileName: string): Boolean;
function IDEEditorEncodingIsUTF8: Boolean;

function IsThemingEnabled: Boolean;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Windows, Registry, StrUtils,
  GX_GenericUtils, GX_OtaUtils, GX_GxUtils, GX_dzCompilerAndRtlVersions;

function GetIdeMainForm: TCustomForm;
begin
  Assert(Assigned(Application));
  Result := Application.FindComponent('AppBuilder') as TCustomForm;

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find AppBuilder!');
  {$ENDIF}
end;

function GetMainMenuItemHeight: Integer;
{$IFDEF GX_VER150_up}
var
  MainForm: TCustomForm;
  Component: TComponent;
begin
  Result := 23;
  MainForm := GetIdeMainForm;
  Component := nil;
  if MainForm <> nil then
    Component := MainForm.FindComponent('MenuBar');
  if (Component is TControl) then
    Result := TControl(Component).ClientHeight; // This is approximate?
{$ELSE}
begin
  Result := GetSystemMetrics(SM_CYMENU);
{$ENDIF}
end;

function GetStandardMenuItemHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
  if Result < 1 then
    Result := 20; // Guess instead of raising an error?  This should never happen.
end;

function GetComponentPaletteTabControl: TTabControl;
var
  MainForm: TCustomForm;
begin
  Result := nil;

  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := MainForm.FindComponent('TabControl') as TTabControl;

  {$IFOPT D+}
    if (Result = nil) and (ComponentPaletteAvailable) then
      SendDebugError('Unable to find component palette TTabControl!');
  {$ENDIF}
end;

function GetObjectInspectorForm: TCustomForm;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := (MainForm.FindComponent('PropertyInspector') as TCustomForm);
  if Result = nil then
    Result := GetIDEFormNamed('PropertyInspector');

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find object inspector!');
  {$ENDIF}
end;

function GetComponentPalettePopupMenu: TPopupMenu;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := (MainForm.FindComponent('PaletteMenu') as TPopupMenu);

  {$IFOPT D+}
    if Result = nil then
      SendDebugError('Unable to find PaletteMenu!');
  {$ENDIF}
end;

function IsIdeEditorForm(AForm: TCustomForm): Boolean;
begin
  Result := (AForm <> nil) and
            (StartsStr('EditWindow_', AForm.Name)) and
            (AForm.ClassName = EditorFormClassName) and
            (not (csDesigning in AForm.ComponentState));
end;

function IsEditControl(Control: TControl): Boolean;
begin
  Result := False;
  if Assigned(Control) then
    Result := Control.ClassName = EditorControlClassName;
end;

function GetIDEEditControl(Form: TCustomForm): TWinControl;
var
  Component: TComponent;
begin
  Assert(Assigned(Form));
  Result :=  nil;
  Component := (Form.FindComponent(EditorControlName) as TWinControl);
  if Assigned(Component) then
    if Component is TWinControl then
      Result := Component as TWinControl;
end;

function GetIDEFormNamed(const FormName: string): TCustomForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Screen.CustomFormCount - 1 do
  begin
    if Screen.CustomForms[i].Name = FormName then
    begin
      Result := Screen.CustomForms[i];
      Break;
    end;
  end;
end;

// We can cause internal editor kernel AVs if we change the editor text
// while the replace confirmation dialog is up, so we detect that case here
function IsReplaceConfirmDialogOnScreen: Boolean;
const
  ConfirmDialogClassName = 'TMessageForm';
var
  AForm: TForm;
  i: Integer;
begin
  Result := False;

  Assert(Assigned(Screen));
  // We search in reverse order, since what we're looking for
  // should usually be last
  for i := Screen.FormCount - 1 downto 0 do
  begin
    AForm := Screen.Forms[i];

    Assert(Assigned(AForm));
    if SameText(AForm.ClassName, ConfirmDialogClassName) then
    begin
      // Make sure it lives in the main VCL module (package or not)
      if FindClassHInstance(AForm.ClassType) = VclInstance then
      begin
        // Now some weak heuristics (don't localize the component names).
        if Assigned(AForm.FindComponent('Yes')) and
           Assigned(AForm.FindComponent('No')) and
           Assigned(AForm.FindComponent('Cancel')) and
           Assigned(AForm.FindComponent('All')) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

function GetIdeDesktopName: string;
var
  reg: TRegistry;
begin
  Result := '';

  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey+'\Session') then
      begin
        Result := Reg.ReadString('DesktopName');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
      GxLogException(E, 'Error in GetIdeDesktopName');
  end;
end;

type
  TComboBoxHack = class(TComboBox)
  end;

function TryGetDesktopCombo(out _cmb: TCombobox): Boolean;
var
  AppBuilder: TForm;
begin
  Result := False;
  AppBuilder := TForm(Application.FindComponent('AppBuilder'));
  if not Assigned(AppBuilder) then
    Exit;
  _cmb := TComboBox(AppBuilder.FindComponent('cbDesktop'));
  Result := Assigned(_cmb);
end;

procedure SetIdeDesktop(const _Desktop: string);
var
  cbDesktop: TComboBox;
begin
  if not TryGetDesktopCombo(cbDesktop) then
    Exit; //==>

  cbDesktop.Text := _Desktop;
  TComboBoxHack(cbDesktop).Click;
end;

function TryGetIdeDesktops(out _Items: TStrings): Boolean;
var
  cbDesktop: TComboBox;
begin
  Result := TryGetDesktopCombo(cbDesktop);
  if Result then
    _Items := cbDesktop.Items;
end;

function GetIdeRootDirectory: string;
const
  BinDirPostfix = PathDelim + 'Bin';
begin
  Result := '';

  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey) then
      begin
        Result := ReadString('RootDir');
        CloseKey;
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
      GxLogException(E, 'Error in GetIdeRootDirectory');
  end;

  if not DirectoryExists(Result) then
  begin
    Result := RemoveSlash(ExtractFilePath(Application.ExeName));
    if SameText(RightStr(Result, Length(BinDirPostfix)), BinDirPostfix) then
      Result := DeleteRight(Result, Length(BinDirPostfix))
    else
      Result := '';
  end;
  Result := AddSlash(Result);
end;

function GetIdeEdition: string;
begin
  Result := '';

  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey) then
    begin
      Result := ReadString('Version');
      CloseKey;
    end;
  finally
    Free;
  end;
end;

function GetIdeEditorBackgroundColor: TColor;
const
  RegAddKey = '\Editor\Highlight\Whitespace'; // do not localize
var
  Reg: TRegistry;
  Value: Integer;
const
  IdePalette: array [0..15] of TColor = (clBlack, clMaroon, clGreen,
    clOlive, clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed,
    clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
begin
  Result := clWindow;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + RegAddKey, False) then
    begin
      if Reg.ValueExists('Default Background') then
      begin
        if Reg.GetDataType('Default Background') = rdString then
        begin
          if SameText(Reg.ReadString('Default Background'), 'False') then
          begin
            if Reg.ValueExists('Background Color') then
            begin
              if Reg.GetDataType('Background Color') = rdInteger then
              begin
                Value := Reg.ReadInteger('Background Color');
                if (Value > 15) or (Value < 0) then
                  Value := 15;
                Result := IdePalette[Value];
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
  {$IFOPT D+}SendDebug('IDE Background Color is: ' + ColorToString(Result) + '  '); {$ENDIF}
end;

function GetIdeEnvironmentOverrides(Overrides: TStrings): Boolean;
var
  Reg: TRegistry;
  Names: TStringList;
  i: Integer;
  Value: string;
begin
  Assert(Assigned(Overrides));
  Overrides.Clear;
  Result := True;
  Names := nil;
  Reg := TRegistry.Create;
  try
    Names := TStringList.Create;
    if Reg.OpenKey(GxOtaGetIdeBaseRegistryKey + '\Environment Variables', False) then
    begin
      Reg.GetValueNames(Names);
      for i := 0 to Names.Count - 1 do begin
        Value := Reg.ReadString(Names[i]);
        Overrides.Add(Names[i] + '=' + Value);
      end;
    end;
  finally
    FreeAndNil(Reg);
    FreeAndNil(Names);
  end;
end;

procedure SetNonModalFormPopupMode(Form: TCustomForm);
begin //FI:W519
  {$IFDEF GX_VER160_up}
  if Assigned(Form) then
    Form.PopupMode := pmExplicit;
  {$ENDIF GX_VER160_up}
end;

procedure SetModalFormPopupMode(Form: TCustomForm);
begin //FI:W519
  {$IFDEF GX_VER160_up}
  if Assigned(Form) then
    Form.PopupMode := pmAuto;
  {$ENDIF GX_VER160_up}
end;

function GetIDEVersionID: string;
var
  RegKey: string;
  LastSlashPos: Integer;
  Version: string;
begin
  RegKey := GxOtaGetIdeBaseRegistryKey;
  LastSlashPos := LastCharPos(RegKey, '\');
  Version := Copy(RegKey, LastSlashPos + 1, 999);
  Result := Version;
  if RunningDelphi8OrGreater then
    Result := 'BDS' + Version;
end;

function RunningWindows: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi8: Boolean;
begin
  {$IFDEF VER160}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi8OrGreater: Boolean;
begin
  {$IFDEF GX_VER160_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi7OrLess: Boolean;
begin
  Result := not RunningDelphi8OrGreater;
end;

function RunningDelphi7OrGreater: Boolean;
begin
  {$IFDEF GX_VER150_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2005: Boolean;
begin
  {$IFDEF VER170}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2007: Boolean;
begin
  {$IFDEF VER185}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningDelphi2007OrLess: Boolean;
begin
  {$IFDEF GX_VER200_up}
  Result := False;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function RunningDelphi2007OrGreater: Boolean;
begin
  {$IFDEF GX_VER185_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2009: Boolean;
begin
  {$IFDEF VER200}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRSXE: Boolean;
begin
  {$IFDEF VER220}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRSXEOrGreater: Boolean;
begin
  {$IFDEF GX_VER220_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2009OrGreater: Boolean;
begin
  {$IFDEF GX_VER200_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningRS2010OrGreater: Boolean;
begin
  {$IFDEF GX_VER210_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningBDS2006OrGreater: Boolean;
begin
  {$IFDEF GX_VER180_up}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function RunningBDS2006OrLess: Boolean;
begin
  Result := not RunningDelphi2007OrGreater;
end;

function RunningBDS2006: Boolean;
begin
  {$IFDEF VER180}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

// This is whether we are running C++Builder Version 6, not if the IDE has C++ support (like BDS2006+)
function RunningCPPBuilder: Boolean;
begin
  {$IFDEF BCB}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function IDEHasWelcomePage: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

function FileIsWelcomePage(const FileName: string): Boolean;
begin
  Result := IDEHasWelcomePage and StringInArray(FileName, ['default.htm', 'bds:/default.htm']);
end;

function IDEEditorEncodingIsUTF8: Boolean;
begin
  Result := RunningDelphi8OrGreater;
end;

function IsThemingEnabled: Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  if CompilerVersion < CompilerVersionDelphiX103 then
    Exit; //==>

  // if yes, check if theming is enabled
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(GxOtaGetIdeBaseRegistryKey + '\Theme') then begin
      try
        if reg.ValueExists('Enabled') and (reg.GetDataType('Enabled') = rdInteger) then
          Result := (reg.ReadInteger('Enabled') <> 0);
      finally
        reg.CloseKey;
      end;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

end.
