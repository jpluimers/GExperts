/// <summary>
/// This is an extract from u_dzVclUtils in dzlib http://blog.dummzeuch.de/dzlib/ </summary>
unit GX_dzVclUtils;

interface

{$I GX_CondDefine.inc}

{$IFDEF GX_VER200_up}
{$DEFINE SUPPORTS_UNICODE_STRING}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Classes,
  Windows,
  SysUtils,
  Controls,
  ComCtrls,
  CommCtrl,
  ActnList,
  StdCtrls,
  Menus,
  Forms,
  Grids;

type
  ///<summary> Ancestor to all exceptions raised in this unit. </summary>
  EdzVclUtils = class(Exception);

type
  TAutoCompleteSourceEnum = (acsFileSystem, acsUrlHistory, acsUrlMru);
  TAutoCompleteSourceEnumSet = set of TAutoCompleteSourceEnum;
type
  TAutoCompleteTypeEnum = (actSuggest, actAppend);
  TAutoCompleteTypeEnumSet = set of TAutoCompleteTypeEnum;

///<summary>
/// Enables autocompletion for an edit control using a call to SHAutoComplete
/// NOTE that this will only work until the control handle is recreated which the VCL does
///      quite often. See TEdit_AutoComplete which handles this problem.
/// @param ed is a TCustomEdit control for which autocompletion should be enabled
/// @param Source is a set of TAutoCompleteSourceEnum that determines from which source to
///               autocomplate, any combination of acsFileSystem, acsUrlHistory, acsUrlMru
///               is allowed.
/// @param Type is a set fo TAutoCompleteEnumSet that determines the type of autocompletion
///             actAppend means that the first match will be appended to the existing text and
///                       selected, so that typing anything will automatically overwrite it.
///             actSuggest means that a list of matches will be displayed as a dropdown list
///                        from which the user can then select using the arrow keys or the mouse.
///             is is possible to pass an empty set, in which case the registry setting.
///             (Unfortunately MSDN doesn't say where in the registry this setting is located.)
/// @returns true, if autocomplete could be activated </summary>
function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): Boolean;

///<summary>
/// Enables autocompletion for an edit control using a call to SHAutoComplete
/// @param ed is a TCustomEdit control for which autocompletion should be enabled
/// @param Source is a set of TAutoCompleteSourceEnum that determines from which source to
///               autocomplate, any combination of acsFileSystem, acsUrlHistory, acsUrlMru
///               is allowed.
/// @param Type is a set fo TAutoCompleteEnumSet that determines the type of autocompletion
///             actAppend means that the first match will be appended to the existing text and
///                       selected, so that typing anything will automatically overwrite it.
///             actSuggest means that a list of matches will be displayed as a dropdown list
///                        from which the user can then select using the arrow keys or the mouse.
///             is is possible to pass an empty set, in which case the registry setting.
///             (Unfortunately MSDN doesn't say where in the registry this setting is located.)
/// @returns the TAutoCompleteActivator instance created.
///          NOTE: You do not need to free this object! It will automatically be freed when the
///                TEdit is destroyed. </summary>
function TEdit_ActivateAutoComplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): TObject;

type
  ///<summary>
  /// Event used in TWinControl_ActivateDropFiles.
  /// @param Sender is the control onto which the files were dropped
  /// @param Files is a TStrings containg the full names of all files dropped.
  ///              Use Files.DelimitedText to get a semicolon separated list of all files. </summary>
  TOnFilesDropped = procedure(_Sender: TObject; _Files: TStrings) of object;

///<summary>
/// Enables dropping of files (folders) from explorer to the given WinControl
/// @param WinCtrl is a TWinControl for which dropping should be enabled
/// @param Callback is a TOnFilesDropped event that is called when files are dropped on the form.
/// @returns the TAutoCompleteActivator instance created.
///          NOTE: You do not need to free this object! It will automatically be freed when the
///                TEdit is destroyed. </summary>
function TWinControl_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;

///<summary> tries to focus the given control, returns false if that's not possible </summary>
function TWinControl_SetFocus(_Ctrl: TWinControl): Boolean;
///<summary>
/// @returns true, if the shift key is currently pressed </summary>
function IsShiftDown: Boolean;
function IsCtrlDown: Boolean;
function IsAltDown: Boolean;
function GetModifierKeyState: TShiftState;

///<summary>
/// This prevents the THotKey component to wrongly display Ctrl+Left as Ctrl+NUM4 </summary>
procedure THotkey_SetHotkey(_hk: THotKey; _ShortCut: TShortCut);
///<summary>
/// This is the reverse of the above, at least in theory, because the Delphi VCL does not
/// distinguish between Ctrl+Left and Ctrl+NUM4 </summary>
function THotkey_GetHotkey(_hk: THotKey): TShortCut;

///<summary>
/// Sets the control's Constraints.MinHeight und Constraints.MinWidth properties
/// to the control's Width and Height.
/// @param FixedHeight if true, the form's MaxHeight will also be set, so that the height
///                    cannot be changed </summary>
procedure TControl_SetMinConstraints(_Control: TControl; _FixedHeight: Boolean = False);

///<summary> sets the with of a ComboBox's dropdown  in pixels </summary>
procedure TComboBox_SetDropdownWidth(_cmb: TCustomComboBox; _Pixels: Integer);

///<summary> Selects the item if it is in the list and returns the new ItemIndex
///          @param cmb is the TCustomCombobox (descendant) to use
///          @param Item is the item to select
///          @param DefaultIdx is the ItemIndex to use if no item matches.
///          @returns the index of the newly selected item or -1 if it doesn't exist </summary>
function TComboBox_Select(_cmb: TCustomComboBox; const _Item: string; _DefaultIdx: Integer = -1;
  _AllowPartialMatch: Boolean = False): Integer;

///<summary>
/// Add a new item with Object = Pointer(Value) </summary>
procedure TComboBox_AddIntObject(_cmb: TCustomComboBox; const _Item: string; _Value: Integer);

///<summary> Frees all objects assigned to the combobox and then clears the combobox </summary>
procedure TComboBox_ClearWithObjects(_cmb: TCustomComboBox);

///<summary> Selects the entry in a combobox that has an object pointer matching Value
///          @param cmb is the TCustomCombobox (descendant) to select
///          @param Value is the desired object value
///          @returns true, if the value could be found, false otherwise </summary>
function TComboBox_SelectByObject(_cmb: TCustomComboBox; _Value: Pointer): Boolean; overload;
function TComboBox_SelectByObject(_cmb: TCustomComboBox; _Value: Integer): Boolean; overload;

///<summary> Gets the caption of the selected combobox item
///          @param cmb is the TCustomCombobox (descendant) to read from
///          @param Item is the selected item, only valid if the function returns true
///          @param FocusControl is a boolean which determines whether to focus the control
///                              if it does not contain a valid value, default = false
///          @returns true, if an item was selected </summary>
function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Item: string;
  _FocusControl: Boolean = False): Boolean; overload;

///<summary> Gets the index of the selected combobox item
///          @param cmb is the TCustomCombobox (descendant) to read from
///          @param Item is the selected item, only valid if the function returns true
///          @param FocusControl is a boolean which determines whether to focus the control
///                              if it does not contain a valid value, default = false
///          @returns true, if an item was selected </summary>
function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Idx: Integer;
  _FocusControl: Boolean = False): Boolean; overload;

///<summary> Gets the object pointer of the selected combobox item
///          @param cmb is the TCustomCombobox (descendant) to read from
///          @param Idx is the combobox's ItemIndex, only valid if the function returns true
///          @param Obj is the value of the object pointer of the selected item, only valid
///                     if the function returns true
///          @param FocusControl is a boolean which determines whether to focus the control
///                              if it does not contain a valid value, default = false
///          @returns true, if these out parameters are valid </summary>
function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Obj: Pointer; _FocusControl: Boolean = False): Boolean; overload;
function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Idx: Integer; out _Obj: Pointer; _FocusControl: Boolean = False): Boolean; overload;
function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _ObjAsInt: Integer; _FocusControl: Boolean = False): Boolean; overload;
function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Item: string; out _Obj: Pointer; _FocusControl: Boolean = False): Boolean; overload;
function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Item: string; out _ObjAsInt: Integer; _FocusControl: Boolean = False): Boolean; overload;

function TComboBox_GetSelectedObjectDef(_cmb: TCustomComboBox;
  _Default: Integer; _FocusControl: Boolean = False): Integer;

///<summary> Frees all objects assigned to the list and then clears the list </summary>
procedure TListbox_ClearWithObjects(_lst: TCustomListbox);

///<summary> Gets the caption of the selected listbox item
///          @param cmb is the TCustomListbox (descendant) to read from
///          @param Item is the selected item, only valid if the function returns true
///          @param FocusControl is a boolean which determines whether to focus the control
///                              if it does not contain a valid value, default = false
///          @returns true, if an item was selected </summary>
function TListBox_GetSelected(_lb: TCustomListbox; out _Item: string;
  _FocusControl: Boolean = False): Boolean;

///<summary> Gets the object pointer of the selected listbox item
///          @param lst is the TCustomListbox (descendant) to read from
///          @param Idx is the listbox's ItemIndex, only valid if the function returns true
///          @param Obj is the value of the object pointer of the selected item, only valid
///                     if the function returns true
///          @returns true, if out parameters are valid </summary>
function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Idx: Integer; out _Obj: Pointer): Boolean; overload;
///<summary> Gets the object pointer of the selected listbox item
///          @param lst is the TCustomListbox (descendant) to read from
///          @param Obj is the value of the object pointer of the selected item, only valid
///                     if the function returns true
///          @returns true, if out parameters are valid </summary>
function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Obj: Pointer): Boolean; overload;

type
  TListViewResizeOptions = (lvrCaptions, lvrContent);
  TLIstViewResizeOptionSet = set of TListViewResizeOptions;
///<summary>
/// Resize all columns of a TListView in vsReport ViewStyle
/// @param lc is the TListColumn to resize
/// @param Options is a set of tListViewResizeOptions
///                lvrCaptions means resize so the captions fit
///                lvrContent menas resize so the contents fit
///                both can be combined. </summary>
procedure TListView_ResizeColumn(_lc: TListColumn; _Options: TLIstViewResizeOptionSet);
///<summary>
/// Resize all columns of a TListView in vsReport ViewStyle
/// @param lc is the TListColumn to resize
/// @param Options is a set of tListViewResizeOptions
///                lvrCaptions means resize so the captions fit
///                lvrContent menas resize so the contents fit
///                both can be combined. </summary>
procedure TListView_Resize(_lv: TListView; _Options: TLIstViewResizeOptionSet = [lvrCaptions, lvrContent]);

///<summary>
/// Gets the index of the selected item of the ListView
/// @param lv is the ListView to work on
/// @param Idx is the index of the selected item, only valid if Result = true
/// @returns true, if an item was selected, false otherwise </summary>
function TListView_TryGetSelected(_lv: TListView; out _Idx: Integer): Boolean; overload;
///<summary>
/// Gets the selected item of the ListView
/// @param lv is the ListView to work on
/// @param li is the selected item, only valid if Result = true
/// @returns true, if an item was selected, false otherwise </summary>
function TListView_TryGetSelected(_lv: TListView; out _li: TListItem): Boolean; overload;

///<summary> free all lv.Items[n].Data objects and then clear the items </summary>
procedure TListView_ClearWithObjects(_lv: TListView);
///<summary> free all li[n].Data objects and then clear the items </summary>
procedure TListItems_ClearWithObjects(_li: TListItems);

///<summary>
/// Append a new action to the given action list, assign Caption
/// and optionally Shortcut and OnExecute event.
/// @returns the new action </summary>
function TActionlist_Append(_al: TActionList; const _Caption: string = ''): TAction; overload;
function TActionlist_Append(_al: TActionList; const _Caption: string; _ShortCut: TShortCut): TAction; overload;
function TActionlist_Append(_al: TActionList; const _Caption: string; _OnExecute: TNotifyEvent): TAction; overload;
function TActionlist_Append(_al: TActionList; const _Caption: string; _OnExecute: TNotifyEvent; _ShortCut: TShortCut): TAction; overload;

///<summary>
/// Appends a new menu item with the given Caption to the popup menu and returns it </summary>
function TPopupMenu_AppendMenuItem(_pm: TPopupMenu; const _Caption: string; _OnClick: TNotifyEvent): TMenuItem; overload;

function TPopupMenu_FindMenuItem(_pm: TPopupMenu; const _Name: string; out _miFound: TMenuItem): Boolean;

function TMainMenu_FindMenuItem(_mnu: TMainMenu; const _Name: string; out _miFound: TMenuItem): Boolean;

function TMenuItem_FindMenuItem(_mi: TMenuItem; const _Name: string; out _miFound: TMenuItem): Boolean;

{$IFNDEF GX_VER200_up}
//Delphi 2009 introduced TCustomButton as the common Ancestor of TButton and TBitBtn.
type
  TCustomButton = TButton;
{$ENDIF}

///<summary>
/// Adds a drowdown menu to a button which automatically gets opened when the user clicks on
/// the button. The button's OnClick event gets changed by this. </summary>
procedure TButton_AddDropdownMenu(_btn: TCustomButton; _pm: TPopupMenu);

type
  TRectLTWH = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;
procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _Left, _Top, _Width, _Height: Integer); overload;
procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _a: TRect); overload;
procedure TRectLTWH_AssignTLRB(var _LTWH: TRectLTWH; _Left, _Top, _Right, _Bottom: Integer);

///<summary>
/// Move the rectangle (usually representing a form) so it is fully visible on the given monitor.</summary>
procedure TMonitor_MakeFullyVisible(_MonitorRect: TRect; var _Left, _Top, _Width, _Height: Integer); overload;
procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Left, _Top, _Width, _Height: Integer); overload;
procedure TMonitor_MakeFullyVisible(_MonitorRect: TRect; var _Rect: TRect; out _Width, _Height: Integer); overload;
procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRect; out _Width, _Height: Integer); overload;
procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRect); overload;
procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRectLTWH); overload;

type
  ///<summary> used in ResizeStringGrid and ResizeDbGrid to specify additional options
  ///  <ul>
  ///    <li>roUseGridWidth -> make the columns take up the whole grid width</li>
  ///    <li>roIgnoreHeader -> do not use the column header to calculate the column
  ///                          width</li>
  ///    <li>roUseLastRows -> use the last 10 rows to calculate the minimum width, not
  ///                         first 10
  ///    <li>roUseAllRows -> use all Grid rows to calculate the minimum width, not
  ///                        just the first 10</li>
  ///  </ul> </summary>
  TResizeOptions = (roUseGridWidth, roIgnoreHeader, roUseLastRows, roUseAllRows, roReduceMinWidth);
  TResizeOptionSet = set of TResizeOptions;

///<summary> Resizes the columns of a TCustomGrid to fit their contents
///          @param Grid is the TCustomGrid to work on
///          @param Options is a TResizeOptionSet specifying additional options,
///                         defaults to an empty set. </summary>
procedure TGrid_Resize(_Grid: TCustomGrid); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; _RowOffset: Integer); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of Integer); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of Integer; _RowOffset: Integer); overload;

implementation

uses
  Consts,
  Messages,
{$IFDEF SUPPORTS_UNICODE_STRING}
  AnsiStrings,
{$ENDIF SUPPORTS_UNICODE_STRING}
  ShellApi,
  Types,
  FileCtrl,
  StrUtils,
  GX_GenericUtils;

type
  TWindowProcHook = class(TComponent)
  private
    FCtrl: TWinControl;
    FOldWindowProc: TWndMethod;
  protected
    procedure WmNcCreate; virtual;
    procedure WmNcDestroy; virtual;
    procedure NewWindowProc(var _Msg: TMessage); virtual;
  public
    constructor Create(_WinControl: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

{ TWindowProcHook }

constructor TWindowProcHook.Create(_WinControl: TWinControl);
begin
  inherited Create(_WinControl);
  FCtrl := _WinControl;

  FOldWindowProc := FCtrl.WindowProc;
  FCtrl.WindowProc := NewWindowProc;
end;

destructor TWindowProcHook.Destroy;
begin
  if Assigned(FCtrl) and Assigned(FOldWindowProc) then begin
    FCtrl.WindowProc := FOldWindowProc;
  end;
  inherited;
end;

procedure TWindowProcHook.WmNcCreate;
begin
  // do nothing
end;

procedure TWindowProcHook.WmNcDestroy;
begin
  // do nothing
end;

procedure TWindowProcHook.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = WM_NCCREATE then
    WmNcCreate
  else if _Msg.Msg = WM_NCDESTROY then
    WmNcDestroy;
  // call original WindowProc method to handle all other messages
  FOldWindowProc(_Msg);
end;

type
  TDropFilesActivator = class(TWindowProcHook)
  private
    FCallback: TOnFilesDropped;
    procedure WmDropFiles(var _Msg: TMessage);
    procedure doCallback(_st: TStrings);
  protected
    procedure WmNcCreate; override;
    procedure WmNcDestroy; override;
    procedure NewWindowProc(var _Msg: TMessage); override;
  public
    constructor Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
    destructor Destroy; override;
  end;

{ TDropFilesActivator }

constructor TDropFilesActivator.Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
begin
  inherited Create(_WinControl);
  FCallback := _Callback;
  DragAcceptFiles(FCtrl.Handle, True);
end;

destructor TDropFilesActivator.Destroy;
begin
  if Assigned(FCtrl) and (FCtrl.HandleAllocated) then
    DragAcceptFiles(FCtrl.Handle, False);
  inherited;
end;

procedure TDropFilesActivator.doCallback(_st: TStrings);
begin
  if Assigned(FCallback) then
    FCallback(FCtrl, _st);
end;

function CheckAdmin(out _IsAdmin: Boolean): Boolean; forward;

// This allows dropping files to an elevated program from applications that don't run elevated
// e.g. from normal Windows Explorer windows.

procedure AllowDropFilesForAdmin(_Handle: HWND);
const
  MSGFLT_ALLOW = 1;
  WM_COPYGLOBALDATA = 73;
type
  TChangeWindowMessageFilterEx = function(Handle: HWND; Msg: Cardinal; dwFlag: Word; _PassNilHere: Pointer): BOOL; stdcall;
var
  ChangeWindowMessageFilterEx: TChangeWindowMessageFilterEx;
  User32Handle: THandle;
  Ptr: TFarProc;
  WinHandle: HWND;
begin
  User32Handle := SafeLoadLibrary('user32.dll');
  if User32Handle > 0 then begin
    Ptr := GetProcAddress(User32Handle, 'ChangeWindowMessageFilterEx');
    if Assigned(Ptr) then begin
      @ChangeWindowMessageFilterEx := Ptr;
      WinHandle := _Handle;
      // We don't care whether it worked. If it didn't, we can't do a thing about it.
      ChangeWindowMessageFilterEx(WinHandle, WM_DROPFILES, MSGFLT_ALLOW, nil);
      ChangeWindowMessageFilterEx(WinHandle, WM_COPYGLOBALDATA, MSGFLT_ALLOW, nil);
      ChangeWindowMessageFilterEx(WinHandle, WM_COPYDATA, MSGFLT_ALLOW, nil);
    end;
    FreeLibrary(User32Handle);
  end;
end;

procedure TDropFilesActivator.WmNcCreate;
var
  IsAdmin: Boolean;
begin
  inherited;
  DragAcceptFiles(FCtrl.Handle, True);
  if CheckAdmin(IsAdmin) and IsAdmin then
    AllowDropFilesForAdmin(FCtrl.Handle);
end;

procedure TDropFilesActivator.WmNcDestroy;
begin
  inherited;
  DragAcceptFiles(FCtrl.Handle, False);
end;

procedure TDropFilesActivator.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = WM_DROPFILES then
    WmDropFiles(_Msg);

  inherited NewWindowProc(_Msg);
end;

procedure TDropFilesActivator.WmDropFiles(var _Msg: TMessage);
var
  arr: array[0..255] of Char;
  fn: string;
  i: Integer;
  sl: TStringList;
  cnt: Cardinal;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    cnt := DragQueryFile(_Msg.wParam, $FFFFFFFF, nil, 255);
    for i := 0 to cnt - 1 do begin
      DragQueryFile(_Msg.wParam, i, PChar(@arr), SizeOf(arr));
      fn := PChar(@arr);
      sl.Add(fn);
    end;
    DragFinish(_Msg.wParam);
    doCallback(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TForm_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;
begin
  Result := TWinControl_ActivateDropFiles(_WinCtrl, _Callback);
end;

function TWinControl_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;
begin
  Result := TDropFilesActivator.Create(_WinCtrl, _Callback);
end;

const
  // constants and descriptions from MSDN
  // http://msdn.microsoft.com/en-us/library/windows/desktop/bb759862(v=vs.85).aspx

  // Ignore the registry default and force the AutoAppend feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;

  // Ignore the registry value and force the AutoAppend feature on. The completed string will be
  // displayed in the edit box with the added characters highlighted.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;

  // Ignore the registry default and force the AutoSuggest feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;

  // Ignore the registry value and force the AutoSuggest feature on.
  // A selection of possible completed strings will be displayed as a
  // drop-down list, below the edit box. This flag must be used in
  // combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;

  // The default setting, equivalent to
  // SHACF_FILESYSTEM | SHACF_URLALL.
  // SHACF_DEFAULT cannot be combined with any other flags.
  SHACF_DEFAULT = $00000000;

  // Include the file system only.
  SHACF_FILESYS_ONLY = $00000010;

  // Include the file system and directories, UNC servers, and UNC server shares.
  SHACF_FILESYS_DIRS = $00000020;

  // Include the file system and the rest of the Shell (Desktop, Computer, and Control Panel, for example).
  SHACF_FILESYSTEM = $00000001;

  // Include the URLs in the user's History list.
  SHACF_URLHISTORY = $00000002;

  // Include the URLs in the user's Recently Used list.
  SHACF_URLMRU = $00000004;

  // Include the URLs in the users History and Recently Used lists. Equivalent to
  // SHACF_URLHISTORY | SHACF_URLMRU.
  SHACF_URLALL = SHACF_URLHISTORY or SHACF_URLMRU;

  // Allow the user to select from the autosuggest list by pressing the TAB key.
  // If this flag is not set, pressing the TAB key will shift focus to the next
  // control and close the autosuggest list.
  // If SHACF_USETAB is set, pressing the TAB key will select the first item
  // in the list. Pressing TAB again will select the next item in the list,
  // and so on. When the user reaches the end of the list, the next TAB key
  // press will cycle the focus back to the edit control.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL*
  // flags
  SHACF_USETAB = $00000008;

  SHACF_VIRTUAL_NAMESPACE = $00000040;

function SHAutoComplete(hwndEdit: HWND; dwFlags: DWORD): HResult; stdcall; external 'Shlwapi.dll';

function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): Boolean;
var
  Options: DWORD;
  Res: HResult;
begin
  Options := 0;
  if acsFileSystem in _Source then
    Options := Options or SHACF_FILESYSTEM;
  if acsUrlHistory in _Source then
    Options := Options or SHACF_URLHISTORY;
  if acsUrlMru in _Source then
    Options := Options or SHACF_URLMRU;
  if actSuggest in _Type then
    Options := Options or SHACF_AUTOSUGGEST_FORCE_ON;
  if actAppend in _Type then
    Options := Options or SHACF_AUTOAPPEND_FORCE_ON;

  Res := SHAutoComplete(_ed.Handle, Options);
  Result := (Res = S_OK);
end;

type
  TAutoCompleteActivator = class(TWindowProcHook)
  private
    FSource: TAutoCompleteSourceEnumSet;
    FType: TAutoCompleteTypeEnumSet;
  protected
    procedure WmNcCreate; override;
    procedure SetAutoComplete;
    procedure NewWindowProc(var _Msg: TMessage); override;
  public
    constructor Create(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
      _Type: TAutoCompleteTypeEnumSet = []);
  end;

{ TAutoCompleteActivator }

constructor TAutoCompleteActivator.Create(_ed: TCustomEdit;
  _Source: TAutoCompleteSourceEnumSet = [acsFileSystem]; _Type: TAutoCompleteTypeEnumSet = []);
begin
  inherited Create(_ed);
  FSource := _Source;
  FType := _Type;
  SetAutoComplete;
end;

procedure TAutoCompleteActivator.WmNcCreate;
begin
  inherited;
  SetAutoComplete;
end;

procedure TAutoCompleteActivator.SetAutoComplete;
begin
  TEdit_SetAutocomplete(FCtrl as TCustomEdit, FSource, FType);
end;

// This and the calling function IsAutoSuggstionDropdownVisible are taken from mghie's answer on
// http://stackoverflow.com/a/9228954/49925

function EnumThreadWindowsProc(AWnd: HWND; AParam: LParam): BOOL; stdcall;
var
  WndClassName: string;
  FoundAndVisiblePtr: PInteger;
begin
  SetLength(WndClassName, 1024);
  GetClassName(AWnd, PChar(WndClassName), Length(WndClassName));
  WndClassName := PChar(WndClassName);
  if WndClassName = 'Auto-Suggest Dropdown' then begin // do not translate
    FoundAndVisiblePtr := PInteger(AParam);
    FoundAndVisiblePtr^ := Ord(IsWindowVisible(AWnd));
    Result := False;
  end else
    Result := True;
end;

function IsAutoSuggestDropdownVisible: Boolean;
var
  FoundAndVisible: Integer;
begin
  FoundAndVisible := 0;
  EnumThreadWindows(GetCurrentThreadId, @EnumThreadWindowsProc,
    LParam(@FoundAndVisible));
  Result := FoundAndVisible > 0;
end;

procedure TAutoCompleteActivator.NewWindowProc(var _Msg: TMessage);
begin
  if (_Msg.Msg = CM_WANTSPECIALKEY) then begin
    if (_Msg.wParam = VK_RETURN) or (_Msg.wParam = VK_ESCAPE) then begin
      if IsAutoSuggestDropdownVisible then begin
        _Msg.Result := 1;
        Exit; //==>
      end;
    end;
  end;
  inherited NewWindowProc(_Msg);
end;

function TEdit_ActivateAutoComplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = []): TObject;
begin
  Result := TAutoCompleteActivator.Create(_ed, _Source, _Type);
end;

function TWinControl_SetFocus(_Ctrl: TWinControl): Boolean;
var
  ParentForm: TCustomForm;
begin
  Result := False;
  try
    ParentForm := GetParentForm(_Ctrl);
    if Assigned(ParentForm) then begin
      ParentForm.ActiveControl := _Ctrl;
      if ParentForm.Visible and ParentForm.Enabled and _Ctrl.CanFocus then
        _Ctrl.SetFocus;
      Result := True;
    end;
  except
    on e: EInvalidOperation do begin
      // ignore any EInvalidOperation here
      // the VCL does not allow us to relly check if a control
      // can be focused so we need to handle the exception
    end;
  end;
end;

// This code is based on an idea from
// http://delphi-wmi-class-generator.googlecode.com/svn/trunk/units/ListView_Helper.pas

procedure TListView_ResizeColumn(_lc: TListColumn; _Options: TLIstViewResizeOptionSet);
var
  Width: Integer;
begin
  if _Options = [lvrCaptions, lvrContent] then begin
    _lc.Width := LVSCW_AUTOSIZE;
    Width := _lc.Width;
    _lc.Width := LVSCW_AUTOSIZE_USEHEADER;
    if Width > _lc.Width then
      _lc.Width := LVSCW_AUTOSIZE;
  end else if _Options = [lvrContent] then
    _lc.Width := LVSCW_AUTOSIZE
  else if _Options = [lvrCaptions] then
    _lc.Width := LVSCW_AUTOSIZE_USEHEADER;
end;

procedure TListView_Resize(_lv: TListView; _Options: TLIstViewResizeOptionSet = [lvrCaptions, lvrContent]);
var
  i: Integer;
begin
  for i := 0 to _lv.Columns.Count - 1 do
    TListView_ResizeColumn(_lv.Columns[i], _Options);
end;

function TActionlist_Append(_al: TActionList; const _Caption: string = ''): TAction;
begin
  Result := TAction.Create(_al);
  Result.Name := '';
  Result.ActionList := _al;
  Result.Caption := _Caption;
end;

function TActionlist_Append(_al: TActionList; const _Caption: string; _ShortCut: TShortCut): TAction;
begin
  Result := TActionlist_Append(_al, _Caption);
  Result.ShortCut := _ShortCut;
end;

function TActionlist_Append(_al: TActionList; const _Caption: string; _OnExecute: TNotifyEvent): TAction;
begin
  Result := TActionlist_Append(_al, _Caption);
  Result.OnExecute := _OnExecute;
end;

function TActionlist_Append(_al: TActionList; const _Caption: string; _OnExecute: TNotifyEvent; _ShortCut: TShortCut): TAction;
begin
  Result := TActionlist_Append(_al, _Caption, _ShortCut);
  Result.OnExecute := _OnExecute;
end;

function TPopupMenu_AppendMenuItem(_pm: TPopupMenu; const _Caption: string; _OnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(_pm);
  Result.Caption := _Caption;
  Result.OnClick := _OnClick;
  _pm.Items.Add(Result);
end;

function TPopupMenu_FindMenuItem(_pm: TPopupMenu; const _Name: string; out _miFound: TMenuItem): Boolean;
begin
  Result := TMenuItem_FindMenuItem(_pm.Items, _Name, _miFound);
end;

function TMainMenu_FindMenuItem(_mnu: TMainMenu; const _Name: string; out _miFound: TMenuItem): Boolean;
begin
  Result := TMenuItem_FindMenuItem(_mnu.Items, _Name, _miFound);
end;

function TMenuItem_FindMenuItem(_mi: TMenuItem; const _Name: string; out _miFound: TMenuItem): Boolean;
var
  i: Integer;
  mi: TMenuItem;
begin
  for i := 0 to _mi.Count - 1 do begin
    mi := _mi.Items[i];
    if mi.Name = _Name then begin
      _miFound := mi;
      Result := True;
      Exit; //==>
    end;
    if TMenuItem_FindMenuItem(mi, _Name, _miFound) then begin
      Result := True;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TListView_TryGetSelected(_lv: TListView; out _Idx: Integer): Boolean;
begin
  _Idx := _lv.ItemIndex;
  Result := (_Idx <> -1);
end;

function TListView_TryGetSelected(_lv: TListView; out _li: TListItem): Boolean;
var
  Idx: Integer;
begin
  Result := TListView_TryGetSelected(_lv, Idx);
  if Result then
    _li := _lv.Items[Idx];
end;

procedure TListItems_ClearWithObjects(_li: TListItems);
var
  i: Integer;
begin
  for i := 0 to _li.Count - 1 do begin
    TObject(_li[i].Data).Free;
    _li[i].Data := nil;
  end;
  _li.Clear;
end;

procedure TListView_ClearWithObjects(_lv: TListView);
begin
  TListItems_ClearWithObjects(_lv.Items);
end;

procedure TControl_SetMinConstraints(_Control: TControl; _FixedHeight: Boolean = False);
begin
  _Control.Constraints.MinHeight := _Control.Height;
  _Control.Constraints.MinWidth := _Control.Width;
  if _FixedHeight then
    _Control.Constraints.MaxHeight := _Control.Height;
end;

procedure TComboBox_SetDropdownWidth(_cmb: TCustomComboBox; _Pixels: Integer);
begin
  _cmb.HandleNeeded;
  _cmb.Perform(CB_SETDROPPEDWIDTH, _Pixels, 0);
end;

procedure TComboBox_AddIntObject(_cmb: TCustomComboBox; const _Item: string; _Value: Integer);
begin
  _cmb.Items.AddObject(_Item, Pointer(_Value));
end;

function TComboBox_Select(_cmb: TCustomComboBox; const _Item: string; _DefaultIdx: Integer = -1;
  _AllowPartialMatch: Boolean = False): Integer;
var
  i: Integer;
  sl: TStringList;
begin
  if _AllowPartialMatch then begin
    sl := TStringList.Create;
    try
      sl.Assign(_cmb.Items);
      Result := _DefaultIdx;
      for i := 0 to sl.Count - 1 do
        if StrBeginsWith(_Item, sl[i], False) then begin
          Result := i;
          Break;
        end;
    finally
      FreeAndNil(sl);
    end;
  end else begin
    Result := _cmb.Items.IndexOf(_Item);
    if Result = -1 then
      Result := _DefaultIdx;
  end;
  _cmb.ItemIndex := Result;
end;

function TComboBox_SelectByObject(_cmb: TCustomComboBox; _Value: Pointer): Boolean;
var
  i: Integer;
begin
  for i := 0 to _cmb.Items.Count - 1 do begin
    Result := (_cmb.Items.Objects[i] = _Value);
    if Result then begin
      _cmb.ItemIndex := i;
      Exit;
    end;
  end;
  Result := False;
end;

function TComboBox_SelectByObject(_cmb: TCustomComboBox; _Value: Integer): Boolean;
begin
  Result := TComboBox_SelectByObject(_cmb, Pointer(_Value));
end;

function TComboBox_GetObjectCaption(_cmb: TCustomComboBox; _Obj: Pointer; out _s: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to _cmb.Items.Count - 1 do begin
    Result := (_cmb.Items.Objects[i] = _Obj);
    if Result then begin
      _s := _cmb.Items[i];
      Exit;
    end;
  end;
  Result := False;
end;

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox; out _Obj: Pointer; _FocusControl: Boolean = False): Boolean;
var
  Idx: Integer;
begin
  Result := TComboBox_GetSelectedObject(_cmb, Idx, _Obj, _FocusControl);
end;

{$IFNDEF GX_VER160_up}
type
  NativeInt = Integer;
{$ENDIF ~GX_VER160_up}

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox; out _ObjAsInt: Integer; _FocusControl: Boolean = False): Boolean;
var
  Obj: Pointer;
begin
  Result := TComboBox_GetSelectedObject(_cmb, Obj, _FocusControl);
  if Result then
    _ObjAsInt := NativeInt(Obj);
end;

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox; out _Idx: Integer;
  out _Obj: Pointer; _FocusControl: Boolean = False): Boolean;
begin
  _Idx := _cmb.ItemIndex;
  Result := (_Idx <> -1);
  if Result then
    _Obj := _cmb.Items.Objects[_Idx]
  else if _FocusControl then
    _cmb.SetFocus;
end;

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Item: string; out _Obj: Pointer; _FocusControl: Boolean = False): Boolean;
var
  Idx: Integer;
begin
  Idx := _cmb.ItemIndex;
  Result := (Idx <> -1);
  if Result then begin
    _Item := _cmb.Items[Idx];
    _Obj := _cmb.Items.Objects[Idx];
  end else if _FocusControl then
    _cmb.SetFocus;
end;

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox;
  out _Item: string; out _ObjAsInt: Integer; _FocusControl: Boolean = False): Boolean;
var
  Obj: Pointer;
begin
  Result := TComboBox_GetSelectedObject(_cmb, _Item, Obj, _FocusControl);
  if Result then
    _ObjAsInt := NativeInt(Obj);
end;

function TComboBox_GetSelectedObjectDef(_cmb: TCustomComboBox; _Default: Integer;
  _FocusControl: Boolean = False): Integer;
begin
  if not TComboBox_GetSelectedObject(_cmb, Result, _FocusControl) then
    Result := _Default;
end;

function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Idx: Integer;
  _FocusControl: Boolean = False): Boolean;
begin
  _Idx := _cmb.ItemIndex;
  Result := (_Idx <> -1);
  if not Result then
    if _FocusControl then
      _cmb.SetFocus;
end;

function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Item: string;
  _FocusControl: Boolean = False): Boolean;
var
  Idx: Integer;
begin
  Result := TComboBox_GetSelected(_cmb, Idx, _FocusControl);
  if Result then
    _Item := _cmb.Items[Idx];
end;

/// <summary>
/// Frees all objects stored in the TStrings intance and returns the instance,
/// meant to be called like
/// @code( TStrings_FreeAllObjects(sl).Free; ) or
/// @code( TStrings_FreeAllObjects(sl).Clear; ) </summary>

function TStrings_FreeAllObjects(_Strings: TStrings): TStrings;
var
  i: Integer;
begin
  for i := 0 to _Strings.Count - 1 do begin
    _Strings.Objects[i].Free;
    _Strings.Objects[i] := nil;
  end;
  Result := _Strings;
end;

procedure TComboBox_ClearWithObjects(_cmb: TCustomComboBox);
begin
  TStrings_FreeAllObjects(_cmb.Items);
  _cmb.Clear;
end;

procedure TListbox_ClearWithObjects(_lst: TCustomListbox);
begin
  TStrings_FreeAllObjects(_lst.Items);
  _lst.Items.Clear;
end;

function TListBox_GetSelected(_lb: TCustomListbox; out _Item: string;
  _FocusControl: Boolean = False): Boolean;
var
  Idx: Integer;
begin
  Idx := _lb.ItemIndex;
  Result := Idx <> -1;
  if Result then
    _Item := _lb.Items[Idx]
  else if _FocusControl then
    _lb.SetFocus;
end;

function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Idx: Integer; out _Obj: Pointer): Boolean;
begin
  _Idx := _lst.ItemIndex;
  Result := _Idx <> -1;
  if Result then
    _Obj := _lst.Items.Objects[_Idx];
end;

function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Obj: Pointer): Boolean;
var
  Idx: Integer;
begin
  Idx := _lst.ItemIndex;
  Result := Idx <> -1;
  if Result then
    _Obj := _lst.Items.Objects[Idx];
end;

type
  TButtonPopupMenuLink = class(TComponent)
  private
    FBtn: TCustomButton;
    FMenu: TPopupMenu;
    FLastClose: DWORD;
  public
    constructor Create(_btn: TCustomButton; _pm: TPopupMenu); reintroduce;
    procedure doOnButtonClick(_Sender: TObject);
  end;

{ TButtonPopupMenuLink }

type
  TCustomButtonHack = class(TCustomButton)
  end;

constructor TButtonPopupMenuLink.Create(_btn: TCustomButton; _pm: TPopupMenu);
begin
  inherited Create(_btn);
  FBtn := _btn;
  FMenu := _pm;
  FMenu.PopupComponent := FBtn;
  TCustomButtonHack(FBtn).OnClick := Self.doOnButtonClick;
end;

procedure TButtonPopupMenuLink.doOnButtonClick(_Sender: TObject);
var
  Pt: TPoint;
begin
  if GetTickCount - FLastClose > 100 then begin
    Pt := FBtn.ClientToScreen(Point(0, FBtn.ClientHeight));
    FMenu.Popup(Pt.X, Pt.Y);
    { Note: PopupMenu.Popup does not return until the menu is closed }
    FLastClose := GetTickCount;
  end;
end;

procedure TButton_AddDropdownMenu(_btn: TCustomButton; _pm: TPopupMenu);
begin
  TButtonPopupMenuLink.Create(_btn, _pm);
end;

function IsShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function IsCtrlDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_CONTROL] and 128) <> 0);
end;

function IsAltDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[VK_MENU] and 128) <> 0);
end;

function GetModifierKeyState: TShiftState;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := [];
  if ((State[vk_Shift] and 128) <> 0) then
    Include(Result, ssShift);
  if ((State[VK_CONTROL] and 128) <> 0) then
    Include(Result, ssCtrl);
  if ((State[VK_MENU] and 128) <> 0) then
    Include(Result, ssAlt);
end;

// taken from:
// http://forum.delphi-treff.de/index.php/Thread/28987-IsAdmin-Funktion-von-Win2k-bis-Vista-7-UAC/?postID=209851#post209851

function CheckAdmin(out _IsAdmin: Boolean): Boolean;
type
  TIsUserAnAdminFunc = function(): BOOL; stdcall;
const
  ADVAPI32DLL = 'ADVAPI32.dll';
  SHELL32DLL = 'shell32.dll';
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_ENABLED = 4;
var
  CheckTokenMembership: function(TokenHandle: THandle; SidToCheck: PSID;
    out IsMember: Boolean): Boolean; stdcall;
  lib: Cardinal;
  Sid: PSID;
  IsUserAnAdminFunc: TIsUserAnAdminFunc;
begin
  Result := False;
  _IsAdmin := False;
  if (Win32MajorVersion = 5) and (Win32MinorVersion = 0) then begin
    // Windows 2000
    lib := GetModuleHandle(ADVAPI32DLL);
    if lib = 0 then
      LoadLibrary(ADVAPI32DLL);
    @CheckTokenMembership := GetProcAddress(lib, 'CheckTokenMembership');
    if Assigned(CheckTokenMembership) then begin
      Result := True;
      if AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, Sid) then begin
        _IsAdmin := CheckTokenMembership(THandle(nil), Sid, Result);
        FreeSid(Sid);
      end;
    end;
  end else if Win32MajorVersion >= 5 then begin
    // XP or above
    lib := LoadLibraryA(SHELL32DLL);
    try
      if lib <> 0 then begin
        @IsUserAnAdminFunc := GetProcAddress(lib, 'IsUserAnAdmin');
        Result := Assigned(@IsUserAnAdminFunc);
        if Result then
          _IsAdmin := IsUserAnAdminFunc();
      end;
    finally
      FreeLibrary(lib);
    end;
  end;
end;

type
  TExtKeyArr = array[0..13] of Word;
const
  EXT_KEYS: TExtKeyArr = (VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_INSERT, VK_DELETE,
    VK_NUMLOCK, VK_PAUSE, VK_PRINT,
    VK_DIVIDE
    );
  STD_KEYS: TExtKeyArr = (VK_NUMPAD4, VK_NUMPAD6, VK_NUMPAD8, VK_NUMPAD2,
    VK_NUMPAD9, VK_NUMPAD3, VK_NUMPAD7, VK_NUMPAD1, VK_NUMPAD0, VK_DECIMAL,
    VK_NUMLOCK, VK_PAUSE, VK_PRINT,
    Ord('/'));

procedure THotkey_SetHotkey(_hk: THotKey; _ShortCut: TShortCut);
var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(_ShortCut, Key, Shift);
  _hk.HotKey := _ShortCut;
  // If it is an "extended" key, we need to set the hkExt flag in Modifiers
  // Extended keys are
  // * left/right/up/down cursor keys
  // * PageUp/Down, Home/End, Delete/Insert keys
  // * Numlock, Break and Print keys
  // * Divide and Enter key (there is no special key code for Enter, so we have to ignore it
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_INSERT, VK_DELETE,
    VK_NUMLOCK, VK_PAUSE, VK_PRINT,
    VK_DIVIDE] then
    _hk.Modifiers := _hk.Modifiers + [hkExt]
  else
    _hk.Modifiers := _hk.Modifiers - [hkExt];
end;

function THotkey_GetHotkey(_hk: THotKey): TShortCut;
var
  Key: Word;
  Shift: TShiftState;
  i: Integer;
begin
  Result := _hk.HotKey;
  ShortCutToKey(Result, Key, Shift);

  if hkExt in _hk.Modifiers then begin
    for i := Low(STD_KEYS) to High(STD_KEYS) do begin
      if Key = STD_KEYS[i] then begin
        Key := EXT_KEYS[i];
        Break;
      end;
    end;
  end else begin
    for i := Low(EXT_KEYS) to High(EXT_KEYS) do begin
      if Key = EXT_KEYS[i] then begin
        Key := STD_KEYS[i];
        Break;
      end;
    end;
  end;
end;

procedure TMonitor_MakeFullyVisible(_MonitorRect: TRect; var _Left, _Top, _Width, _Height: Integer); overload;
begin
  if _Left + _Width > _MonitorRect.Right then
    _Left := _MonitorRect.Right - _Width;
  if _Left < _MonitorRect.Left then
    _Left := _MonitorRect.Left;
  if _Top + _Height > _MonitorRect.Bottom then
    _Top := _MonitorRect.Bottom - _Height;
  if _Top < _MonitorRect.Top then
    _Top := _MonitorRect.Top;
end;

procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Left, _Top, _Width, _Height: Integer); overload;
begin
  TMonitor_MakeFullyVisible(_Monitor.WorkareaRect, _Left, _Top, _Width, _Height);
end;

procedure TMonitor_MakeFullyVisible(_MonitorRect: TRect; var _Rect: TRect; out _Width, _Height: Integer); overload;
var
  Left: Integer;
  Top: Integer;
begin
  Left := _Rect.Left;
  Top := _Rect.Top;
  _Width := _Rect.Right - Left;
  _Height := _Rect.Bottom - Top;
  TMonitor_MakeFullyVisible(_MonitorRect, Left, Top, _Width, _Height);
  _Rect.Left := Left;
  _Rect.Top := Top;
  _Rect.Right := Left + _Width;
  _Rect.Bottom := Top + _Height;
end;

procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRect; out _Width, _Height: Integer); overload;
begin
  TMonitor_MakeFullyVisible(_Monitor.WorkareaRect, _Rect, _Width, _Height);
end;

procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRect); overload;
var
  Width: Integer;
  Height: Integer;
begin
  TMonitor_MakeFullyVisible(_Monitor, _Rect, Width, Height);
end;

procedure TMonitor_MakeFullyVisible(_Monitor: TMonitor; var _Rect: TRectLTWH); overload;
begin
  TMonitor_MakeFullyVisible(_Monitor.WorkareaRect, _Rect.Left, _Rect.Top, _Rect.Width, _Rect.Height);
end;

{ TRectLTWH }

procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _Left, _Top, _Width, _Height: Integer);
begin
  _LTWH.Left := _Left;
  _LTWH.Top := _Top;
  _LTWH.Width := _Width;
  _LTWH.Height := _Height;
end;

procedure TRectLTWH_Assign(var _LTWH: TRectLTWH; _a: TRect);
begin
  TRectLTWH_AssignTLRB(_LTWH, _a.Left, _a.Top, _a.Right, _a.Bottom);
end;

procedure TRectLTWH_AssignTLRB(var _LTWH: TRectLTWH; _Left, _Top, _Right, _Bottom: Integer);
begin
  TRectLTWH_Assign(_LTWH, _Left, _Top, _Right - _Left, _Bottom - _Top);
end;

type
  TGridHack = class(TCustomGrid);

procedure TGrid_Resize(_Grid: TCustomGrid);
begin
  TGrid_Resize(_Grid, [], [], -1);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet);
begin
  TGrid_Resize(_Grid, _Options, [], -1);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; _RowOffset: Integer);
begin
  TGrid_Resize(_Grid, _Options, [], _RowOffset);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of Integer);
begin
  TGrid_Resize(_Grid, _Options, _ConstantCols, -1);
end;

function ArrayContains(_Element: Integer; const _Arr: array of Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(_Arr) to High(_Arr) do begin
    Result := _Arr[i] = _Element;
    if Result then
      Exit;
  end;
end;

procedure HandleRow(_Grid: TGridHack; _Col, _Row: Integer; var _MinWidth: Integer);
var
  ColWidth: Integer;
  ColText: string;
begin
  ColText := _Grid.GetEditText(_Col, _Row);
  ColWidth := _Grid.Canvas.TextWidth(ColText);
  if ColWidth > _MinWidth then
    _MinWidth := ColWidth;
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of Integer; _RowOffset: Integer);
var
  Col, Row: Integer;
  Grid: TGridHack;
  MinWidth: Integer;
  MinCol: Integer;
  MaxCol: Integer;
  MaxRow: Integer;
  ColWidths: array of Integer;
  FirstRow: Integer;
  SumWidths: Integer;
  Additional: Integer;
begin
  Grid := TGridHack(_Grid);
  MaxCol := Grid.ColCount - 1;
  MinCol := 0;
  SetLength(ColWidths, MaxCol + 1);

  if _RowOffset = -1 then
    FirstRow := Grid.FixedRows
  else
    FirstRow := _RowOffset;

  MaxRow := FirstRow + 10;
  if (MaxRow >= Grid.RowCount) or (roUseAllRows in _Options) or (roUseLastRows in _Options) then
    MaxRow := Grid.RowCount - 1;
  if roUseLastRows in _Options then begin
    if MaxRow > FirstRow + 10 then
      FirstRow := MaxRow - 1
  end;
  SumWidths := MaxCol; // one spare pixel per column
  if goVertLine in Grid.Options then
    Inc(SumWidths, Grid.GridLineWidth);
  for Col := MinCol to MaxCol do begin
    if ArrayContains(Col, _ConstantCols) then
      MinWidth := Grid.ColWidths[Col]
    else begin
      MinWidth := 0;

      if not (roIgnoreHeader in _Options) then
        for Row := 0 to Grid.FixedRows - 1 do
          HandleRow(Grid, Col, Row, MinWidth);

      for Row := FirstRow to MaxRow do
        HandleRow(Grid, Col, Row, MinWidth);

      if goVertLine in Grid.Options then
        Inc(MinWidth, Grid.GridLineWidth);
      Inc(MinWidth, 4); // 2 Punkte rechts und links, wie in TStringGrid.DrawCell
      if MinWidth < Grid.DefaultColWidth then
        MinWidth := Grid.DefaultColWidth;
    end;
    ColWidths[Col] := MinWidth;
    Inc(SumWidths, MinWidth);
  end;
  if (SumWidths < Grid.ClientWidth) and (roUseGridWidth in _Options)
    and (Length(_ConstantCols) < MaxCol + 1) then begin
    Additional := (Grid.ClientWidth - SumWidths) div (MaxCol + 1 - Length(_ConstantCols));
    for Col := MinCol to MaxCol do begin
      if not ArrayContains(Col, _ConstantCols) then begin
        Inc(ColWidths[Col], Additional);
        Inc(SumWidths, Additional);
      end;
    end;
    if SumWidths < Grid.ClientWidth then begin
      Col := MaxCol;
      while ArrayContains(Col, _ConstantCols) do
        Dec(Col);
      Inc(ColWidths[Col], Grid.ClientWidth - SumWidths);
    end;
  end;
  for Col := MinCol to MaxCol do
    Grid.ColWidths[Col] := ColWidths[Col];
end;

end.
