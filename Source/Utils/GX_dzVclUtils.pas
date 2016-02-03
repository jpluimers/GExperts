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
  StdCtrls, Menus;

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

///<summary>
/// @returns true, if the shift key is currently pressed </summary>
function IsShiftDown: Boolean;
function IsCtrlDown: Boolean;
function IsAltDown: Boolean;
function GetModifierKeyState: TShiftState;

///<summary> Sets the control's Constraints.MinHeight und Constraints.MinWidth properties
///          to the control's Width and Height. </summary>
procedure TControl_SetMinConstraints(_Control: TControl);

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
function TActionlist_Append(_al: TActionList; _Caption: string = ''): TAction; overload;
function TActionlist_Append(_al: TActionList; _Caption: string; _Shortcut: TShortCut): TAction; overload;
function TActionlist_Append(_al: TActionList; _Caption: string; _OnExecute: TNotifyEvent): TAction; overload;
function TActionlist_Append(_al: TActionList; _Caption: string; _OnExecute: TNotifyEvent; _Shortcut: TShortCut): TAction; overload;

///<summary>
/// Appends a new menu item with the given Caption to the popup menu and returns it </summary>
function TPopupMenu_AppendMenuItem(_pm: TPopupMenu; const _Caption: string; _OnClick: TNotifyEvent): TMenuItem; overload;


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
  StrUtils;

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

procedure TDropFilesActivator.WmNcCreate;
begin
  inherited;
  DragAcceptFiles(FCtrl.Handle, True);
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

function SHAutoComplete(hwndEdit: HWnd; dwFlags: DWORD): HResult; stdcall; external 'Shlwapi.dll';

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

function EnumThreadWindowsProc(AWnd: HWnd; AParam: LParam): BOOL; stdcall;
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

function TActionlist_Append(_al: TActionList; _Caption: string = ''): TAction;
begin
  Result := TAction.Create(_al);
  Result.Name := '';
  Result.ActionList := _al;
  Result.Caption := _Caption;
end;

function TActionlist_Append(_al: TActionList; _Caption: string; _Shortcut: TShortCut): TAction;
begin
  Result := TActionlist_Append(_al, _Caption);
  Result.ShortCut := _Shortcut;
end;

function TActionlist_Append(_al: TActionList; _Caption: string; _OnExecute: TNotifyEvent): TAction;
begin
  Result := TActionlist_Append(_al, _Caption);
  Result.OnExecute := _OnExecute;
end;

function TActionlist_Append(_al: TActionList; _Caption: string; _OnExecute: TNotifyEvent; _Shortcut: TShortCut): TAction;
begin
  Result := TActionlist_Append(_al, _Caption, _Shortcut);
  Result.OnExecute := _OnExecute;
end;

function TPopupMenu_AppendMenuItem(_pm: TPopupMenu; const _Caption: string; _OnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(_pm);
  Result.Caption := _Caption;
  Result.OnClick := _OnClick;
  _pm.Items.Add(Result);
end;

function TListView_TryGetSelected(_lv: TListView; out _Idx: Integer): Boolean;
begin
  _Idx := _lv.ItemIndex;
  Result := (_Idx <> -1);
end;

function TListView_TryGetSelected(_lv: TListView; out _li: TListItem): Boolean;
var
  idx: Integer;
begin
  Result := TListView_TryGetSelected(_lv, idx);
  if Result then
    _li := _lv.Items[idx];
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

procedure TControl_SetMinConstraints(_Control: TControl);
begin
  _Control.Constraints.MinHeight := _Control.Height;
  _Control.Constraints.MinWidth := _Control.Width;
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

{$IFNDEF GX_VER170_up}

function StartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiStartsText(ASubText, AText);
end;
{$ENDIF}

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
        if StartsText(_Item, sl[i]) then begin
          Result := i;
          break;
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
  idx: Integer;
begin
  Result := TComboBox_GetSelectedObject(_cmb, idx, _Obj, _FocusControl);
end;

function TComboBox_GetSelectedObject(_cmb: TCustomComboBox; out _ObjAsInt: Integer; _FocusControl: Boolean = False): Boolean;
var
  Obj: Pointer;
begin
  Result := TComboBox_GetSelectedObject(_cmb, Obj, _FocusControl);
  if Result then
    _ObjAsInt := Integer(Obj);
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
  idx: Integer;
begin
  idx := _cmb.ItemIndex;
  Result := (idx <> -1);
  if Result then begin
    _Item := _cmb.Items[idx];
    _Obj := _cmb.Items.Objects[idx];
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
    _ObjAsInt := Integer(Obj);
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
  idx: Integer;
begin
  Result := TComboBox_GetSelected(_cmb, idx, _FocusControl);
  if Result then
    _Item := _cmb.Items[idx];
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
  idx: Integer;
begin
  idx := _lb.ItemIndex;
  Result := idx <> -1;
  if Result then
    _Item := _lb.Items[idx]
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
  idx: Integer;
begin
  idx := _lst.ItemIndex;
  Result := idx <> -1;
  if Result then
    _Obj := _lst.Items.Objects[idx];
end;

end.
