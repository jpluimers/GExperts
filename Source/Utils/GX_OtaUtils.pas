unit GX_OtaUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, ActnList, ImgList, Menus, ToolsAPI,
  GX_GenericUtils;

// Returns the TObject represented by an IOTAComponent, if possible
function GxOtaGetNativeObject(const AComponent: IOTAComponent): TObject;
// Returns the TPersistent represented by an IOTAComponent, if possible
function GxOtaGetNativePersistent(const AComponent: IOTAComponent): TPersistent;
// Returns the TComponent represented by an IOTAComponent, if possible
function GxOtaGetNativeComponent(const AComponent: IOTAComponent): TComponent;
// Returns if the component is inherited from a parent form
function GxOtaIsInheritedComponent(const AComponent: IOTAComponent): Boolean;
// Returns the name of the component
function GxOtaGetComponentName(const AComponent: IOTAComponent): WideString;
// Sets the name of the component
function GxOtaSetComponentName(const AComponent: IOTAComponent; const Name: WideString): Boolean;
// Returns the name of the component's parent if the component has a parent
function GxOtaGetComponentParentName(const AComponent: IOTAComponent): WideString;
// See if the two IOTAComponents are the same component
function GxOtaComponentsAreEqual(Comp1, Comp2: IOTAComponent): Boolean;

procedure GxOtaGetInstalledComponentList(Components: TStrings; ActiveOnly: Boolean);

function GxOtaGetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; PreferStrings: Boolean = False): WideString; overload;
function GxOtaSetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; const Value: WideString): Boolean; overload;
function GxOtaNormalizePropertyValue(Component: IOTAComponent; const PropName, Value: string): string;
function GxOtaGetPropertyIndexByName(const AComponent: IOTAComponent;
  const PropertyName: WideString): Integer;

function GxOtaEditorIsRecordingMacro: Boolean;
function GxOtaEditorIsPlayingMacro: Boolean;
function GxOtaEditorIsPausedMacro: Boolean;
procedure GxOtaSetEditorKeyTracingEnabled(Enabled: Boolean);
function GxOtaGetEditorKeyTracingEnabled: Boolean;

const
  // Invalid notifier index
  InvalidNotifierIndex = -1;
  dNFM = 'nfm';  // Not present in the Delphi 8/9/10 ToolsAPI.pas
  FileVersionOptionNames:     array [0..1] of string = ('FileVersion', 'Dateiversion');
  ProductNameOptionNames:     array [0..1] of string = ('ProductName', 'Produktname');
  InternalNameOptionNames:    array [0..1] of string = ('InternalName', 'Interner Name');
  FileDescriptionOptionNames: array [0..1] of string = ('FileDescription', 'Beschreibung');
  ProductVersionOptionNames:  array [0..1] of string = ('ProductVersion', 'Produktversion');

  CodeInsightKeyCodeCompletion   = #0;
  CodeInsightKeyParameterInsight = #1;
  CodeInsightKeyBrowse           = #2;
  CodeInsightKeySymbolHint       = #3;

  {$IFNDEF GX_VER160_up}
  sDefaultPersonality = 'Default.Personality';
  sDelphiPersonality = 'Delphi.Personality';
  sDelphiDotNetPersonality = 'DelphiDotNet.Personality';
  sCBuilderPersonality = 'CPlusPlusBuilder.Personality';
  sCSharpPersonality = 'CSharp.Personality';
  sVBPersonality = 'VB.Personality';
  sDesignPersonality = 'Design.Personality';
  {$ENDIF}

type
  TGxEditorReadStream = class(TStream)
  private
    FSize: Longint;
    FPosition: Longint;
    FEditReader: IOTAEditReader;
  protected
    function GetSize: Int64; {$IFDEF GX_VER150_up} override; {$ENDIF GX_VER150_up}
  public
    constructor Create(Editor: IOTASourceEditor);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Size: Int64 read GetSize;
  end;

// Save an edit reader to a stream
procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream);

// If UseSelection is True, get the selected text in the current edit view
// (if any) and return True or get all of the editor's text (if no selection)
// and return False
function GxOtaGetActiveEditorText(Stream: TStream; UseSelection: Boolean = True): Boolean;

// Returns the editor's active text as a string, similar to above
function GxOtaGetActiveEditorTextAsString(var Text: string; UseSelection: Boolean = True): Boolean;

// Get current identifier under cursor in the active edit buffer
// Scans current line from cursor position to left and right for identifier name.
function GxOtaGetCurrentIdent: string;

// Replace the source editor's text with the passed text
procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; Text: string);

// Returns current line from edit buffer
// StartOffset is the start offset of line (zero-based)
// ColumnNo is the current column number (zero-based, can be > then number of chars in line)
// LineNo is the current line number
function GxOtaGetCurrentLineData(var StartOffset, ColumnNo, LineNo: Integer): string;

// Get the identifier at the cursor position, similar to above.
// If an identifier is found then:
// - Ident is the identifier text (if one is under the cursor)
// - IdentOffset is the starting offset of the identifier in the character stream
// - StartPos is the position where the identifier starts (line/col, 1-based)
// - CurrentPos is the current cursor position (line/col)
// - AfterLen is the length of the part of the identifier that is after cursor
procedure GxOtaGetCurrentIdentEx(var Ident: string; var IdentOffset: Integer;
  var StartPos: TOTAEditPos; var CurrentPos: TOTAEditPos; var AfterLen: Integer); overload;
// Get the identifier and starting position only.  Return True on success.
function GxOtaGetCurrentIdentData(var Ident: string; var StartEditPos: TOTAEditPos): Boolean;

// Get all of the characters in the current line that are before the cursor
function GxOtaGetPreceedingCharactersInLine(View: IOTAEditView): string;

// Select the current identifier under the cursor
function GxOtaSelectCurrentIdent(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True): Boolean;
// Select the current line of text
procedure GxOtaSelectCurrentLine(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True);

// Insert a string into the current IOTASourceEditor
// This should be used when the Text is a single line
// It replaces the current selection (if any) and doesn't
// require multiple undo operations to undo the insert
procedure GxOtaInsertLineIntoEditor(const Text: string);

// Insert any text into the current IOTASourceEditor
// This should be used when the Text should not replace the
// current selection and when it can contain more than one
// line.  An undo might require multiple undo operations.
// This procedure ensures the cursor is still visible.
procedure GxOtaInsertTextIntoEditor(const Text: string);

// Get an edit writer for the current source editor (raise an exception if not availble)
// Use the current source editor if none is specified
function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;

// Insert a string into a file at a some character position
// Uses the current source editor if none is specified
procedure GxOtaInsertTextIntoEditorAtPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor = nil);

// Set the EditView's cursor position based on a character index or a
// TOTAEditPos.  Uses the topmost EditView if none is specified
procedure GxOtaGotoPosition(Position: Longint; EditView: IOTAEditView = nil;
  Middle: Boolean = True);
procedure GxOtaGotoEditPos(EditPos: TOTAEditPos; EditView: IOTAEditView = nil;
  Middle: Boolean = True);

// This function converts a character index into a TOTACharPos, since
// IOTAEditView.PosToCharPos doesn't work in D5/D6 (it always AVs)
function GxOtaGetCharPosFromPos(Position: Longint; EditView: IOTAEditView): TOTACharPos;

// Get the TOTAEditPos for the edit view (defaults to the topmost view)
function GxOtaGetCurrentEditPos(EditView: IOTAEditView = nil): TOTAEditPos;
// Get the position in the character array for the edit view (defaults to the topmost view)
function GxOtaGetCurrentEditBufferPos(EditView: IOTAEditView = nil): Integer;

// Delete Count characters starting from StartPos in some source editor
// Uses the current source editor if none is specified
procedure GxOtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);

// Replace an edit view's current selection with a string
procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor; ViewNum: Integer;
  const Text: string);

// Get the text of a source file or form whether it is open or not
// and assign it to Lines.Text
function GxOtaGetFileAsText(const FileName: string; Lines: TStrings; out WasBinary: Boolean): Boolean;

// Read the requested file's contents into a WideString.  Supports ANSI and UNICODE files.
function GxOtaFileToWideString(const FileName: WideString; Data: WideString; out WasBinary: Boolean): Boolean;

// Test whether the last attribute before the current one is part of another
// one passed in InElement - or something else.  This is only called if
// GetAttributeAtPos returns atWhiteSpace - it is not clear, then, whether
// the cursor is inside another element.  Currently, this code is used for
// atComment and atString, see below.
function GxOtaIsWhiteSpaceInElement(const InElement: Integer;
  EV: IOTAEditView; EditorPosition: TOTAEditPos): Boolean;
function GxOtaIsWhiteSpaceInComment(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
function GxOtaIsWhiteSpaceInString(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;

type
  TUnitInfo = class(TObject)
  private
    FFormName: string;
    FSourceName: string;
    FFileName: string;
  public
    property SourceName: string read FSourceName write FSourceName;
    property FormName: string read FFormName write FFormName;
    property FileName: string read FFileName write FFileName;
  end;

  TCompInfo = class(TObject)
  private
    FCompName: WideString;
    FCompType: WideString;
  public
    property CompName: WideString read FCompName write FCompName;
    property CompType: WideString read FCompType write FCompType;
  end;

procedure ClearCompInfoList(const List: TList);
procedure GxOtaFillComponentInfoList(const List: TList);

procedure ClearUnitInfoList(const List: TList);
procedure GxOtaFillUnitInfoListForCurrentProject(const List: TList);

//
//
//

procedure GxOtaSelectComponentOnCurrentForm(const ComponentName: string);

// Get a reference to the actual form instance being designed
function GxOtaGetCurrentDesignForm: TCustomForm;
// Refresh the object inspector's view into a designer
function GxOtaRefreshCurrentDesigner: Boolean;

// Write a simple (bold) title message to the IDE Messages window
procedure GxOtaWriteTitleMessage(const Title: string);
// Write a tool message to the IDE Messages window
procedure GxOtaWriteToolMessage(const FileName, MessageStr, PrefixStr: string;
  LineNumber, ColumnNumber: Integer);
// Clears all tool and title messages in the IDE Messages window
procedure GxOtaClearToolMessages;

// Ses if the IDE is curently debugging an application
function GxOtaCurrentlyDebugging: Boolean;

// Get an IDE services interface.  Succeeds or raises an exception.
function GxOtaGetIDEServices: IOTAServices;
function GxOtaGetDebuggerServices: IOTADebuggerServices;

// Determine if we are currently editing a VCL/CLX/NFM
function GxOtaGetActiveDesignerType: string;
function GxOtaActiveDesignerIsVCL: Boolean;
function GxOtaActiveDesignerIsCLX: Boolean;
function GxOtaActiveDesignerIsNFM: Boolean;

// Get the personality identifier string for the project, or blank for none
function GxOtaGetProjectPersonality(Project: IOTAProject): string;
function GxOtaGetCurrentProjectPersonality: string;

// See if a project is of a specific personality
function GxOtaProjectIsDotNet(Project: IOTAProject): Boolean;
function GxOtaProjectIsDelphiDotNet(Project: IOTAProject): Boolean;
function GxOtaProjectIsNativeCpp(Project: IOTAProject): Boolean;
function GxOtaProjectIsNativeDelphi(Project: IOTAProject): Boolean;
function GxOtaProjectIsEitherDelphi(Project: IOTAProject): Boolean;
function GxOtaProjectIsCSharp(Project: IOTAProject): Boolean;
// See if the current project is of a specific personality
function GxOtaCurrentProjectIsDotNet: Boolean;
function GxOtaCurrentProjectIsDelphiDotNet: Boolean;
function GxOtaCurrentProjectIsNativeCpp: Boolean;
function GxOtaCurrentProjectIsNativeDelphi: Boolean;
function GxOtaCurrentProjectIsEitherDelphi: Boolean;
function GxOtaCurrentProjectIsCSharp: Boolean;

// Check if the IDE supports a certain personality
function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
function GxOtaHaveCPPSupport: Boolean;
function GxOtaHaveDelphiSupport: Boolean;
function GxOtaHaveCSharpSupport: Boolean;

// Get the preferred hex number display prefix chanacters for our personality
function GxOtaGetHexPrefix: string;

// See if the user is editing a form
function GxOtaCurrentlyEditingForm: Boolean;

// See if the user is editing a source file
function GxOtaCurrentlyEditingSource: Boolean;

// Obtain and focus the IDE's internal edit control wrapper
function GxOtaGetCurrentIDEEditControl: TWinControl;
function GxOtaFocusCurrentIDEEditControl: Boolean;

// Returns reference to currently active project;
// returns nil if there is no (active) project.
function GxOtaGetCurrentProject: IOTAProject;

// See if there is an active project
function GxOtaHaveCurrentProject: Boolean;

// Returns file name of the project; returns
// an empty string if there is no (active) project.
// Use NormalizeBdsProj to get the dpr instead of the bdsproj/dproj for Delphi projects
function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean = False): string;

// Returns the name of the currently active project; returns
// an empty string if there is no (active) project.
// The name of a project is the project's file name without
// a file extension and without path information.
function GxOtaGetCurrentProjectName: string;

// Returns reference to the IDE's project group;
// returns Nil if there is no project group.
function GxOtaGetProjectGroup: IOTAProjectGroup;

// Returns the full path and file name of IDE's project group.
// Returns an empty string if there is no project group.
function GxOtaGetProjectGroupFileName: string;

// Get the list of files in the project
procedure GxOtaGetProjectFileNames(Project: IOTAProject; Files: TStrings);

// Returns the file name of the currently "active"
// module; if no module is active, an empty
// string is returned.
function GxOtaGetFileNameOfCurrentModule: string;

// Get the IOTAProjectOptions interface for the active project
function GxOtaGetActiveProjectOptions: IOTAProjectOptions;
// Get the value of a specific project option for the active project
function GxOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
// Get the value of a version info keys string
function GxOtaGetVersionInfoKeysOption(const Option: string; var Value: Variant): Boolean;
// Get the first value of a list of version info keys strings
function GxOtaGetVersionInfoKeysOptions(const Options: array of string; var Value: Variant): Boolean;
// Get all of the version info keys (do not free the string list)
function GxOtaGetVersionInfoKeysStrings(out Strings: TStrings): Boolean;
function GxOtaGetVersionInfoKeysString: string;

// Get the selected text in the top edit view, if any
function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean = True): string;

function GxOtaMoveEditCursorColumn(EditView: IOTAEditView; RelativePos: Integer): Boolean;

// Returns -1, 0, 1 depending on the position relationship between two edit positions
function GxOtaCompareEditPos(AEditPos1, AEditPos2: TOTAEditPos): Integer;

// Select a range of text in the source editor
procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; const Start, After: TOTACharPos); overload;
procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; Start, After: TOTAEditPos); overload;
function GxOtaGetSelection(const EditView: IOTAEditView;
  var BlockStart, BlockEnd: TOTAEditPos; var SelStart, SelLength: Integer): Boolean;

// Un-fold all regions or the nearest region in a file
procedure GxOtaUnfoldAllRegions(const View: IOTAEditView);
procedure GxOtaUnfoldNearestRegion(const View: IOTAEditView);

// Delete the selected text in the editor
function GxOtaDeleteSelection(const EditView: IOTAEditView = nil): Boolean;

// Close FileName; returns True on success, False otherwise.
function GxOtaCloseFile(const FileName: string): Boolean;
// Close the current edit view tab
procedure GxOtaCloseCurrentEditorTab;

// Open FileName in IDE; returns True on success, False otherwise.
function GxOtaOpenFile(const FileName: string): Boolean;
function GxOtaOpenFileOrForm(const FileName: string): Boolean;

// Returns True if FileName is an open file in the IDE;
// returns False otherwise.
// FileName must be a fully qualified file name, including
// the path name.  UseBase determines whether things like .dfm/.hpp map
// to their parent file type.  Otherwise, they will not be located.
function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;

// Determine is a file exists or the file's module is currently loaded in the IDE
function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;

// Make sure the given filename's source is visible.
// This swaps source/form views as necessary
function GxOtaMakeSourceVisible(const FileName: string): Boolean;
function GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
procedure GxOtaGoToFileLine(const FileName: string; Line: Integer);
procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);

// Transform dfm, hpp, etc. references to the base .pas/cpp file name
function GxOtaGetBaseModuleFileName(const FileName: string): string;

// Get the environment options interface.  Succeeds or raises an exception.
function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
// Return the IDE's environment string value that is
// associated with EnvironmentStringName
function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;
// Return all of the IDE's environment settings names
procedure GxOtaGetIdeEnvironmentStrings(Settings: TStrings);
// Return the IDE's global library path
function GxOtaGetIdeLibraryPath: string;
// Return the IDE's global browsing path
function GxOtaGetIdeBrowsingPath: string;
// Return the effective library path, with the project specific paths
// first and then the IDE's global library path.  The paths are
// optionally macro expanded and with non-existing paths removed.
procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);
// Retrieve a guess at all possible paths where files in the current project
// might be located by the compiler
procedure GxOtaGetAllPossiblePaths(Paths: TStrings);
// Locate a base file name on a list of paths
function GxOtaFindPathToFile(const FileName: string; Paths: TStrings = nil): string;
// Try to open a file located anywhere in GxOtaGetAllPossiblePaths
function GxOtaOpenFileFromPath(const FileName: string): Boolean;
// Obtain a list of alias mappings in OldUnit=NewUnit format
procedure GxOtaGetUnitAliases(Aliases: TStrings);

// Returns IDE's base registry key, for instance
//   Software\Borland\Delphi\4.0\
// The returned string is guaranteed to NOT have a
// backslash appended and it does NOT have a leading
// backslash either (Windows 2000 is allergic to that).
function GxOtaGetIdeBaseRegistryKey: string;

// Get a string describing the IDE such as 'Delphi' or 'C++Builder'
function GxOtaGetIDEProductIdentifier: string;

// Returns the IDE's image list.
function GxOtaGetIdeImageList: TCustomImageList;

// Return the IDE's main menu.
function GxOtaGetIdeMainMenu: TMainMenu;

// Return the IDE's main action list
function GxOtaGetIdeActionList: TCustomActionList;

// Find an action in the IDE's action list by name and return it.
function GxOtaGetIdeActionByName(const Name: string): TContainedAction;

// Return the top-most edit view/buffer (globally or for a given source editor)
// Returns nil if none exists.
function GxOtaGetTopMostEditView: IOTAEditView; overload;
function GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView; overload;
function GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
function GxOtaGetTopMostEditBufferFileName: string;

// Get the number of open modules
function GxOtaGetOpenModuleCount: Integer;

// Returns the current module; may return nil of there is no
// current module.
function GxOtaGetCurrentModule: IOTAModule;

// Returns the current IOTASourceEditor or nil if
// there is no current source editor.
function GxOtaGetCurrentSourceEditor: IOTASourceEditor;
procedure GxOtaShowCurrentSourceEditor;

// Raise an exception if the source editor is readonly
procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);

// Returns a module interface for a given filename
// May return nil if no such module is open.
function GxOtaGetModule(const FileName: string): IOTAModule;

// Returns a fully qualified name of the current file,
// which could either be a form or unit (.pas/.cpp/.dfm/.xfm etc.).
// Returns a blank string if no file is currently selected.
function GxOtaGetCurrentSourceFile: string;

// Returns the syntax highlighting enumeration constant that
// is applicable for the SourceEditor. Returns shNone if
// there is no syntax-highlighting (for instance, because it
// has been turned off in the IDE or because there is no
// current source editor).
function GxOtaGetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor = nil): TGXSyntaxHighlighter;

// Returns True if property with name PropertyName exists for the component;
// False otherwise.
// Property names are insensitive to case.
function GxOtaPropertyExists(const Component: IOTAComponent; const PropertyName: string): Boolean;

function GxOtaGetFormEditorForFileName(const FileName: string): IOTAFormEditor;
// Gets the form editor for the active module, or nil if none is present
function GxOtaGetCurrentFormEditor: IOTAFormEditor;
// Gets the number of selected components on the current form editor (if any)
function GxOtaGetCurrentFormEditorSelectionCount: Integer;
// Determine if the curent form editor has a selected component (not the root designer)
function GxOtaFormEditorHasSelectedComponent: Boolean;
// Returns a form editor for Module if it exists; nil otherwise.
function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
// Shows the form editor for a module.  Returns True if successful.
function GxOtaShowFormForModule(const Module: IOTAModule): Boolean;

// Returns the IOTASourceEditor interface for a module
// if there is a file that supports one; returns nil
// if there is no IOTASourceEditor
function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;

// Get the active IOTAEditor
function GxOtaGetCurrentEditor: IOTAEditor;

// Get an editor for the file indicated by FileName
// contained in the passed Module
function GxOtaGetEditorFromModule(Module: IOTAModule; const FileName: string): IOTAEditor;

// Get the Index-th IOTAEditor for the given module
// Works around a BCB 5 bug with simple units
function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;

// Obtain the IOTAEditActions interface for a given module
function GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
// Obtain the IOTAEditActions interface for the current module
function GxOtaGetEditActions: IOTAEditActions;

// Returns True if the only selected component on FormEditor is the "root"
// of all components, i.e. the data module or form on which everything
// is hosted.
// Returns False if there is more than one component selected
// or if the currently selected component is not the root.
function GxOtaSelectedComponentIsRoot(const FormEditor: IOTAFormEditor): Boolean;
//  Get the BiDiMode property for a VCL form
function GxOtaGetFormBiDiMode(Form: IOTAFormEditor): TBiDiMode;

// Returns True if FileName points to a valid expert DLL
// (valid wizard / expert entry point). False if not.
// Always returns True on Windows 9x, since Windows 9x does not
// support the DONT_RESOLVE_DLL_REFERENCES parameter for plain
// loading of the DLL as data.
function IsValidExpertDll(const FileName: string): Boolean;

function IsStandAlone: Boolean;

// Consolidate with GxOtaGotoPosition?
procedure GxOtaSetCurrentSourcePosition(Position: Integer);

function GxOtaGetKeyboardServices: IOTAKeyboardServices;

// Get the configures tab stops as a string for the current editor file
function GxOtaGetTabStops: string;

// Returns tab size (single value)
// Note: works only for configurations with single value in "Tab Stops" option
function GxOtaGetTabWidth: Integer;

// Expand the tabs in the lines into spaces using the current editor tab witdh
procedure GxOtaExpandTabsInList(ALines: TStrings);

// Return the tab-expanded whitespace at the beginning of the current selection
function GxOtaGetLeadingWhiteSpaceInSelection: string;

// Show a dialog enumerating all of the current project's modules and editors
procedure GxOtaShowProjectModuleInformation;

// Show a dialog enumerating all of the IDE's actions with details
procedure GxOtaShowIDEActions;

{$IFDEF GX_VER160_up}
function ConvertToIDEEditorString(const S: string): UTF8String;
{$ELSE}
function ConvertToIDEEditorString(const S: string): string;
{$ENDIF}

{$IFDEF GX_VER160_up}
function IDEEditorStringToString(const S: UTF8String): string;
{$ELSE}
function IDEEditorStringToString(const S: string): string;
{$ENDIF}

type
  TBaseIdeNotifier = class(TNotifierObject, IOTAIDENotifier)
  private
    FNotifierIndex: Integer;
  public
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean); virtual;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); virtual;
    procedure AfterCompile(Succeeded: Boolean); virtual;
    function AddNotifierToIDE: Boolean;
    function RemoveNotifierFromIDE: Boolean;
    destructor Destroy; override;
  end;

  TBaseDebuggerNotifier = class(TNotifierObject, IOTADebuggerNotifier)
  private
    FNotifierIndex: Integer;
  public
    function AddNotifierToIDE: Boolean;
    function RemoveNotifierFromIDE: Boolean;
    destructor Destroy; override;
    // IOTADebuggerNotifier
    procedure BreakpointAdded({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint); virtual;
    procedure BreakpointDeleted({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint); virtual;
    procedure ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); virtual;
    procedure ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess); virtual;
  end;

  EStandAloneUsage = class(Exception);

const
  NamePropertyName = 'Name';
  InvalidIntValue = -99999;

implementation

uses
  {$IFDEF LINUX} WinUtils, {$ENDIF}
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Variants, Windows, DesignIntf, TypInfo, 
  GX_EditReader, GX_IdeUtils, GX_VerDepConst, SetString;

procedure ClearUnitInfoList(const List: TList);
var
  i: Integer;
begin
  if Assigned(List) then
  begin
    for i := 0 to List.Count-1 do
      TObject(List[i]).Free;
    List.Clear;
  end;
end;

procedure GxOtaFillUnitInfoListForCurrentProject(const List: TList);
var
  i: Integer;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  UnitInfo: TUnitInfo;
begin
  Assert(Assigned(List));
  ClearUnitInfoList(List);

  CurrentProject := GxOtaGetCurrentProject;
  if not Assigned(CurrentProject) then
    Exit;

  for i := 0 to CurrentProject.GetModuleCount - 1 do
  begin
    ModuleInfo := CurrentProject.GetModule(i);
    Assert(Assigned(ModuleInfo));

    if ModuleInfo.FileName <> '' then
    begin
      UnitInfo := TUnitInfo.Create;
      List.Add(UnitInfo);

      UnitInfo.FileName := ModuleInfo.FileName;
      UnitInfo.FormName := ModuleInfo.FormName;
      UnitInfo.SourceName := ModuleInfo.Name;
    end;
  end;
end;

procedure ClearCompInfoList(const List: TList);
var
  i: Integer;
begin
  if Assigned(List) then
  begin
    for i := 0 to List.Count - 1 do
      TObject(List.Items[i]).Free;
  end;
end;

procedure GxOtaFillComponentInfoList(const List: TList);
var
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
  OwnedComponent: IOTAComponent;
  CompInfo: TCompInfo;
  i: Integer;
begin
  Assert(Assigned(List));

  FormEditor := GxOtaGetCurrentFormEditor;
  if Assigned(FormEditor) then
  begin
    RootComponent := FormEditor.GetRootComponent;
    Assert(Assigned(RootComponent), 'FormEditor has no root component');

    for i := 0 to RootComponent.GetComponentCount-1 do
    begin
      OwnedComponent := RootComponent.GetComponent(i);
      if Assigned(OwnedComponent) then
      begin
        CompInfo := TCompInfo.Create;
        CompInfo.CompName := GxOtaGetComponentName(OwnedComponent);
        CompInfo.CompType := OwnedComponent.GetComponentType;
        List.Add(CompInfo);
      end;
    end;
  end;
end;

function GxOtaGetFormEditorForFileName(const FileName: string): IOTAFormEditor;
var
  Module: IOTAModule;
begin
  Result := nil;
  Module := GxOtaGetModule(GxOtaGetBaseModuleFileName(FileName));
  if Assigned(Module) then
    Result := GxOtaGetFormEditorFromModule(Module);
end;

function GxOtaGetCurrentFormEditor: IOTAFormEditor;
var
  Module: IOTAModule;
begin
  Result := nil;
  Module := GxOtaGetCurrentModule;
  if Assigned(Module) then
    Result := GxOtaGetFormEditorFromModule(Module);
end;

function GxOtaGetCurrentFormEditorSelectionCount: Integer;
var
  FormEditor: IOTAFormEditor;
begin
  Result := 0;
  FormEditor := GxOtaGetCurrentFormEditor;
  if Assigned(FormEditor) then
    Result := FormEditor.GetSelCount;
end;

function GxOtaFormEditorHasSelectedComponent: Boolean;
var
  FormEditor: IOTAFormEditor;
  FileName: string;
begin
  Result := False;
  FileName := GxOtaGetCurrentSourceFile;
  if not IsForm(FileName) then
    Exit;

  FormEditor := GxOtaGetCurrentFormEditor;
  if Assigned(FormEditor) then
  begin
    Result := (FormEditor.GetSelCount > 0) and
      not GxOtaSelectedComponentIsRoot(FormEditor);
  end;
end;

function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
var
  i: Integer;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if Supports(Editor, IOTAFormEditor, FormEditor) then
    begin
      Assert(not Assigned(Result));
      Result := FormEditor;
      // In order to assert our assumptions that only one form
      // is ever associated with a module, do not call Break; here.
    end;
  end;
end;

function GxOtaShowFormForModule(const Module: IOTAModule): Boolean;
var
  FormEditor: IOTAFormEditor;
begin
  Result := False;
  Assert(Assigned(Module));
  FormEditor := GxOtaGetFormEditorFromModule(Module);
  if Assigned(FormEditor) then
  begin
    FormEditor.Show;
    Result := True;
  end;
end;

procedure GxOtaSelectComponentOnCurrentForm(const ComponentName: string);
var
  FormEditor: IOTAFormEditor;
  FoundComponent: IOTAComponent;
begin
  if Length(ComponentName) = 0 then
    Exit;

  FormEditor := GxOtaGetCurrentFormEditor;
  Assert(Assigned(FormEditor));

  FoundComponent := FormEditor.FindComponent(ComponentName);
  if not Assigned(FoundComponent) then
    Exit;

  FoundComponent.Select(False); // Select, but do not add to an already existing selection
  FormEditor.Show;
end;

function GxOtaGetCurrentDesignForm: TCustomForm;
var
  FormEditor: IOTAFormEditor;
  FormDesigner: IDesigner;
  Root: TComponent;
begin
  Result := nil;

  FormEditor := GxOtaGetCurrentFormEditor;
  if not Assigned(FormEditor) then
    Exit;

  FormDesigner := (FormEditor as INTAFormEditor).FormDesigner;
  if not Assigned(FormDesigner) then
    Exit;

  Root := FormDesigner.Root;
  if Root is TCustomForm then
    Result := TCustomForm(Root)
  else if Assigned(Root) then
  begin
    Root := Root.Owner;
    if Root is TCustomForm then
      Result := TCustomForm(Root)
  end;
end;

function GxOtaRefreshCurrentDesigner: Boolean;
var
  FormEditor: IOTAFormEditor;
  FormDesigner: IDesigner;
begin
  Result := False;

  FormEditor := GxOtaGetCurrentFormEditor;
  if not Assigned(FormEditor) then
    Exit;

  FormDesigner := (FormEditor as INTAFormEditor).FormDesigner;
  if Assigned(FormDesigner) then
  begin
    FormDesigner.Modified;
    Result := True;
  end;
end;

procedure GxOtaWriteTitleMessage(const Title: string);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddTitleMessage(Title);
end;

procedure GxOtaWriteToolMessage(const FileName, MessageStr, PrefixStr: string;
  LineNumber, ColumnNumber: Integer);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddToolMessage(FileName, MessageStr, PrefixStr,
    LineNumber, ColumnNumber);
end;

procedure GxOtaClearToolMessages;
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIdeServices as IOTAMessageServices;
  if Assigned(IMessageServices) then
    IMessageServices.ClearToolMessages;
end;

function GxOtaCurrentlyDebugging: Boolean;
var
  DebuggerServices: IOTADebuggerServices;
begin
  Result := False;
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
    Result := DebuggerServices.ProcessCount > 0;
end;

function GxOtaGetIDEServices: IOTAServices;
begin
  Result := BorlandIDEServices as IOTAServices;
  if not Assigned(Result) then
    raise Exception.Create('IOTAServices not implemented');
end;

function GxOtaGetDebuggerServices: IOTADebuggerServices;
begin
  Result := BorlandIDEServices as IOTADebuggerServices;
  if not Assigned(Result) then
    raise Exception.Create('IOTADebuggerServices not implemented');
end;

function GxOtaGetActiveDesignerType: string;
begin
  if RunningLinux then
    Result := dCLX
  else
    Result := GxOtaGetIDEServices.GetActiveDesignerType;
end;

function GxOtaActiveDesignerIsVCL: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dVCL);
end;

function GxOtaActiveDesignerIsCLX: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dCLX);
end;

function GxOtaActiveDesignerIsNFM: Boolean;
begin
  Result := (GxOtaGetActiveDesignerType = dNFM);
end;

function GxOtaGetProjectPersonality(Project: IOTAProject): string;
begin
  Result := '';
  if Assigned(Project) then
  begin
    {$IFDEF GX_VER160_up} //Delphi 8+
    Result := Project.Personality;
    {$ELSE} // Delphi 7 or earlier
    if RunningCPPBuilder then
      Result := sCBuilderPersonality
    else
      Result := sDelphiPersonality;
    {$ENDIF}
  end;
end;

function GxOtaGetCurrentProjectPersonality: string;
begin
  Result := GxOtaGetProjectPersonality(GxOtaGetCurrentProject);
end;

function GxOtaProjectIsDotNet(Project: IOTAProject): Boolean;
begin
  Result := StringInArray(GxOtaGetProjectPersonality(Project), [sDelphiDotNetPersonality, sCSharpPersonality, sVBPersonality]);
end;

function GxOtaProjectIsDelphiDotNet(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sDelphiDotNetPersonality);
end;

function GxOtaProjectIsNativeCpp(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sCBuilderPersonality);
end;

function GxOtaProjectIsNativeDelphi(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sDelphiPersonality);
end;

function GxOtaProjectIsEitherDelphi(Project: IOTAProject): Boolean;
begin
  Result := StringInArray(GxOtaGetProjectPersonality(Project), [sDelphiPersonality, sDelphiDotNetPersonality]);
end;

function GxOtaProjectIsCSharp(Project: IOTAProject): Boolean;
begin
  Result := SameText(GxOtaGetProjectPersonality(Project), sCSharpPersonality);
end;

function GxOtaCurrentProjectIsDotNet: Boolean;
begin
  Result := GxOtaProjectIsDotNet(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsDelphiDotNet: Boolean;
begin
  Result := GxOtaProjectIsDelphiDotNet(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsNativeCpp: Boolean;
begin
  Result := GxOtaProjectIsNativeCpp(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsNativeDelphi: Boolean;
begin
  Result := GxOtaProjectIsNativeDelphi(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsEitherDelphi: Boolean;
begin
  Result := GxOtaProjectIsEitherDelphi(GxOtaGetCurrentProject);
end;

function GxOtaCurrentProjectIsCSharp: Boolean;
begin
  Result := GxOtaProjectIsCSharp(GxOtaGetCurrentProject);
end;

{$IFDEF GX_VER160_up}
function GxOtaGetPersonalityServices: IOTAPersonalityServices;
begin
  Result := PersonalityServices;
  if not Assigned(Result) then
    raise Exception.Create('Personality services not available');
end;

function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
var
  Personalities: IOTAPersonalityServices;
  i: Integer;
begin
  Result := False;
  Personalities := GxOtaGetPersonalityServices;
  for i := 0 to Personalities.PersonalityCount - 1 do begin
    if SameText(Personalities.Personalities[i], Personality) then begin
      Result := True;
      Break;
    end;
  end;
end;
{$ELSE Delphi 7-}
function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
begin
  if RunningCPPBuilder then
    Result := StringInArray(Personality, [sDelphiPersonality, sCBuilderPersonality])
  else
    Result := StringInArray(Personality, [sDelphiPersonality]);
end;
{$ENDIF}

function GxOtaHaveCPPSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sCBuilderPersonality);
end;

function GxOtaHaveDelphiSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sDelphiPersonality) or
    GxOtaIdeSupportsPersonality(sDelphiDotNetPersonality)
end;

function GxOtaHaveCSharpSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sCSharpPersonality);
end;

function GxOtaGetHexPrefix: string;
begin
  if StringInArray(GxOtaGetCurrentProjectPersonality, [sCBuilderPersonality, sCSharpPersonality]) then
    Result := '0x'
  else
    Result := '$';
end;

function GxOtaCurrentlyEditingForm: Boolean;
var
  Editor: IOTAEditor;
begin
  Result := False;
  Editor := GxOtaGetCurrentEditor;
  if Assigned(Editor) then
    Result := Supports(Editor, IOTAFormEditor);
end;

function GxOtaCurrentlyEditingSource: Boolean;
begin
  Result := IsIdeEditorForm(Screen.ActiveCustomForm) or IsEditControl(Screen.ActiveControl);
end;

function GxOtaGetCurrentIDEEditControl: TWinControl;
var
  EditView: IOTAEditView;
  EditWindow: INTAEditWindow;
  EditForm: TCustomForm;
begin
  Result := nil;
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
  begin
    EditWindow := EditView.GetEditWindow;
    if Assigned(EditWindow) then
    begin
      EditForm := EditWindow.Form;
      if Assigned(EditForm) then
        Result := GetIDEEditControl(EditForm);
    end;
  end;
end;

function GxOtaFocusCurrentIDEEditControl: Boolean;
var
  EditControl: TWinControl;
begin
  Result := False;
  EditControl := GxOtaGetCurrentIDEEditControl;
  if EditControl.CanFocus then begin
    try
      EditControl.SetFocus;
      Result := True;
    except
      // Ignore exceptions and return False
    end;
  end;
end;

function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
begin
  Result := '';
  if Assigned(Project) then begin
    Result := Project.FileName;
    if NormalizeBdsProj and IsBdsprojOrDproj(Result) then begin
      if GxOtaProjectIsEitherDelphi(Project) then begin
        Result := ChangeFileExt(Result, '.dpr');
        if (not FileExists(Result)) and FileExists(ChangeFileExt(Result, '.dpk')) then
          Result := ChangeFileExt(Result, '.dpk');
      end;
    end;
  end;
end;

function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean): string;
begin
  Result := GxOtaGetProjectFileName(GxOtaGetCurrentProject, NormalizeBdsProj);
end;

function GxOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(BorlandIDEServices));

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  Result := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if Supports(IModule, IOTAProjectGroup, Result) then
      Break;
  end;
end;

function GxOtaGetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := nil;

  IProjectGroup := GxOtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
  begin
    Assert(Assigned(BorlandIDEServices));
    IModuleServices := BorlandIDEServices as IOTAModuleServices;
    Assert(Assigned(IModuleServices));

    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      if Supports(IModule, IOTAProject, Result) then
        Break;
    end;
  end;

  try
    // This raises exceptions in D5 with .bat projects active
    if Assigned(IProjectGroup) and (not Assigned(Result)) then
      Result := IProjectGroup.ActiveProject;
  except
    Result := nil;
  end;
end;

function GxOtaHaveCurrentProject: Boolean;
begin
  Result := GxOtaGetCurrentProject <> nil;
end;

function GxOtaGetCurrentProjectName: string;
var
  IProject: IOTAProject;
begin
  Result := '';

  IProject := GxOtaGetCurrentProject;
  if Assigned(IProject) then
  begin
    Result := ExtractFileName(IProject.FileName);
    Result := ChangeFileExt(Result, '');
  end;
end;

function GxOtaGetProjectGroupFileName: string;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := '';
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  IProjectGroup := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
      Break;
  end;
  // Delphi 5 does not return the file path when querying IOTAProjectGroup directly
  if IProjectGroup <> nil then
    Result := IModule.FileName;
end;

procedure GxOtaGetProjectFileNames(Project: IOTAProject; Files: TStrings);
var
  i: Integer;
begin
  Assert(Assigned(Files));
  Files.Clear;
  if not Assigned(Project) then
    Exit;
  for i := 0 to Project.GetModuleCount - 1 do
    if Trim(Project.GetModule(i).FileName) <> '' then
      Files.Add(Project.GetModule(i).FileName);
end;

function GxOtaGetFileNameOfCurrentModule: string;
var
  Module: IOTAModule;
begin
  Result := '';
  Module := GxOtaGetCurrentModule;
  if Assigned(Module) then
    Result := Module.FileName;
end;

function GxOtaGetActiveProjectOptions: IOTAProjectOptions;
var
  Project: IOTAProject;
begin
  Result := nil;
  Project := GxOtaGetCurrentProject;
  if Assigned(Project) then
    Result := Project.ProjectOptions;
end;

function GxOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
var
  ProjectOptions: IOTAProjectOptions;
begin
  Result := False;
  Value := '';
  ProjectOptions := GxOtaGetActiveProjectOptions;
  if Assigned(ProjectOptions) then
  begin
    Value := ProjectOptions.Values[Option];
    Result := True;
  end;
end;

function GxOtaGetVersionInfoKeysOption(const Option: string; var Value: Variant): Boolean;
var
  KeysStrings: TStrings;
  OptionValue: string;
begin
  Result := False;
  Value := Null;
  if GxOtaGetVersionInfoKeysStrings(KeysStrings) then
  begin
    OptionValue := KeysStrings.Values[Option];
    if OptionValue <> '' then
    begin
      Result := True;
      Value := OptionValue;
    end;
  end;
end;

function GxOtaGetVersionInfoKeysOptions(const Options: array of string; var Value: Variant): Boolean; overload;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Options) to High(Options) do
  begin
    Result := GxOtaGetVersionInfoKeysOption(Options[i], Value);
    if Result then
      Break;
  end;
end;

function GxOtaGetVersionInfoKeysStrings(out Strings: TStrings): Boolean;
var
  KeysValue: Variant;
  KeysInteger: Integer;
begin
  Result := False;
  Strings := nil;
  if GxOtaGetActiveProjectOption('Keys', KeysValue) then
  begin
    if (not VarIsNull(KeysValue)) and (VarType(KeysValue) = varInteger) then
    begin
      KeysInteger := KeysValue;
      if TObject(KeysInteger) is TStrings then
      begin
        Strings := TStrings(KeysInteger);
        Result := True;
      end;
    end;
  end;
end;

function GxOtaGetVersionInfoKeysString: string;
var
  Strings: TStrings;
begin
  Result := '';
  if GxOtaGetVersionInfoKeysStrings(Strings) then
    Result := Strings.Text;
end;

function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean): string;
var
  EditView: IOTAEditView;
  EditBlock: IOTAEditBlock;
begin
  Result := '';

  EditView := GxOtaGetTopMostEditView;
  if not Assigned(EditView) then
    Exit;

  EditBlock := EditView.Block;
  if Assigned(EditBlock) then begin
    Result := IDEEditorStringToString(EditBlock.Text);
    if not IncludeTrailingCRLF and (EditBlock.Style in [btNonInclusive]) then
      RemoveLastEOL(Result);
  end;
end;

function GxOtaMoveEditCursorColumn(EditView: IOTAEditView; RelativePos: Integer): Boolean;
var
  CursorPos: TOTAEditPos;
begin
  Assert(Assigned(EditView));
  Result := False;
  CursorPos := EditView.CursorPos;
  if CursorPos.Col + RelativePos > 0 then
  begin
    CursorPos.Col := CursorPos.Col + RelativePos;
    EditView.CursorPos := CursorPos;
    Result := True;
  end;
end;

function GxOtaGetCurrentLineData(var StartOffset, ColumnNo, LineNo: Integer): string;

  // Returns column, line number, and source of the active editor
  function GetCurrentSourceAndPos(var CursorPosition: Integer;
    var CursorLine: Integer): string;
  var
    EditView: IOTAEditView;
    EditPos: TOTAEditPos;
    CharPos: TOTACharPos;
  begin
    Result := '';
    CursorPosition := 0;

    EditView := GxOtaGetTopMostEditView;
    if Assigned(EditView) then
    begin
      GxOtaGetActiveEditorTextAsString(Result, False);
      EditPos := EditView.CursorPos;
      EditView.ConvertPos(True, EditPos, CharPos);
      CursorLine := CharPos.Line;
      CursorPosition := EditView.CharPosToPos(CharPos);
    end;
  end;

var
  TotalSrc: string;
  Index: Integer;
  SrcLen, LineLen: Integer;
  CursorPos: Integer;
begin
  TotalSrc := GetCurrentSourceAndPos(CursorPos, LineNo);

  // Find the start of the line
  Index := CursorPos;
  while Index > 0 do
  begin
    if EOLSizeAtPos(TotalSrc, Index) > 0 then
      Break;
    Dec(Index);
  end;

  if Index < 0 then
    Index := 0;
  StartOffset := Index;

  // Search for the end of the line
  SrcLen := Length(TotalSrc);
  Index := CursorPos + 1;
  while Index < SrcLen do
  begin
    if EOLSizeAtPos(TotalSrc, Index) > 0 then
      Break;
    Inc(Index);
  end;

  LineLen := (Index - 1) - StartOffset;
  Result := Copy(TotalSrc, StartOffset + 1, LineLen);

  // The column number is the difference between the start-of-line offset
  // and the current-cursor-position offset, so it is zero-based
  ColumnNo := CursorPos - StartOffset;
end;

// TODO 3 -oAnyone -cCleanup : This claims to return TOTAEditPos values but really returns TOTACharPos values
procedure GxOtaGetCurrentIdentEx(var Ident: string; var IdentOffset: Integer;
  var StartPos: TOTAEditPos; var CurrentPos: TOTAEditPos; var AfterLen: Integer);

  function IsEndOfWordChar(Char: Char): Boolean;
  begin
    if IsCharIdentifier(Char) then
      Result := False
    else
      Result := True;
  end;

var
  CurrentLine: string;
  IdentName, Tmp: string;
  IdentLen: Integer;
  HeadLen, WhiteCnt: Integer;
  CheckIdx: Integer;
  TailStartIdx: Integer;
  ColumnNo: Integer;
  StartOffset: Integer;
  LineNo: Integer;
  TailLen: Integer;
begin
  Ident := '';
  CurrentLine := GxOtaGetCurrentLineData(StartOffset, ColumnNo, LineNo);

  CurrentPos.Col := ColumnNo;
  CurrentPos.Line := LineNo;
  IdentOffset := -1;

  StartPos.Line := LineNo;
  StartPos.Col := 0;

  // The index of first character after the cursor
  TailStartIdx := ColumnNo + 1;

  // Search how many characters of Ident name are after cursor (TailLen)
  // ColumnNo is zero-based
  CheckIdx := TailStartIdx;
  while CheckIdx <= Length(CurrentLine) do
  begin
    if IsEndOfWordChar(CurrentLine[CheckIdx]) then
      Break;
    Inc(CheckIdx); // Move to next character
  end;

  // TailLen is the number of additional chars after the cursor pos
  TailLen := CheckIdx - TailStartIdx; // char after end - first char

  Tmp := Copy(CurrentLine, 1, ColumnNo + TailLen);

  // Search how many characters are for Ident name before cursor
  CheckIdx := TailStartIdx - 1; // character before cursor
  while CheckIdx > 0 do
  begin
    if IsEndOfWordChar(Tmp[CheckIdx]) then
      Break;
    Dec(CheckIdx); // Move to next character
  end;

  HeadLen := (TailStartIdx - 1) - CheckIdx;
  IdentOffset := StartOffset + CheckIdx;

  WhiteCnt := IdentOffset - StartOffset;
  // The number of characters before the identifier
  IdentLen := HeadLen + TailLen;

  IdentName := Copy(Tmp, WhiteCnt + 1, IdentLen);

  Ident := Trim(IdentName);

  if Ident = '' then // No identifier is present, but the cursor position can be returned
  begin
    if Length(CurrentLine) < ColumnNo then
    begin  // The line is too short to hold the current position
      IdentOffset := StartOffset;
      StartPos.Col := 0;
    end
    else
    begin // The line has enough characters
      IdentOffset := StartOffset + ColumnNo;
      StartPos.Col := ColumnNo;
    end;
  end
  else
    StartPos.Col := ColumnNo - HeadLen;

  Inc(StartPos.Col);
  AfterLen := TailLen;
end;

function GxOtaGetCurrentIdentData(var Ident: string; var StartEditPos: TOTAEditPos): Boolean;
var
  IdentOffset: Integer;
  StartPos: TOTACharPos;
  EStart: TOTAEditPos;
  ECurrent: TOTAEditPos;
  AfterLen: Integer;
  EditView: IOTAEditView;
begin
  // This claims to return TOTAEditPos, but actually returns TOTACharPos in disguise
  GxOtaGetCurrentIdentEx(Ident, IdentOffset, EStart, ECurrent, AfterLen);
  StartEditPos.Col := 0;
  StartEditPos.Line := 0;
  Result := Ident <> '';
  if Result then
  begin
    StartPos.CharIndex := EStart.Col - 1;
    StartPos.Line := EStart.Line;
    EditView := GxOtaGetTopMostEditView;
    Assert(Assigned(EditView));
    EditView.ConvertPos(False, StartEditPos, StartPos);
  end;
end;

function GxOtaGetCurrentIdent: string;
var
  IdentOffset: Integer;
  StartPos: TOTAEditPos;
  CurrentPos: TOTAEditPos;
  AfterLen: Integer;
begin
  GxOtaGetCurrentIdentEx(Result, IdentOffset, StartPos, CurrentPos, AfterLen);
end;

procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; Text: string);
var
  Writer: IOTAEditWriter;
begin
  Assert(Assigned(SourceEditor));
  Writer := SourceEditor.CreateUndoableWriter;
  if not Assigned(Writer) then
    raise Exception.Create('No edit writer');
  Writer.DeleteTo(MaxLongint);
  Writer.Insert(PAnsiChar(ConvertToIDEEditorString(Text)));
  Writer := nil;
end;

function GxOtaCompareEditPos(AEditPos1, AEditPos2: TOTAEditPos): Integer;
begin
  if (AEditPos1.Line > AEditPos2.Line) or
    ((AEditPos1.Line = AEditPos2.Line) and (AEditPos1.Col > AEditPos2.Col))
  then
    Result := 1
  else if (AEditPos1.Line = AEditPos2.Line) and (AEditPos1.Col = AEditPos2.Col) then
    Result := 0
  else
    Result := -1;
end;

procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; const Start, After: TOTACharPos);
begin
  Assert(Assigned(Editor));
  Editor.BlockVisible := False;
  try
    Editor.BlockType := btNonInclusive;
    Editor.BlockStart := Start;
    Editor.BlockAfter := After;
  finally
    Editor.BlockVisible := True;
  end;
end;

procedure GxOtaSelectBlock(const Editor: IOTASourceEditor; Start, After: TOTAEditPos); overload;
var
  EditView: IOTAEditView;
begin
  Assert(Assigned(Editor));
  EditView := GxOtaGetTopmostEditView(Editor);
  Assert(Assigned(EditView));

  EditView.CursorPos := Start;
  EditView.Block.BeginBlock;
  EditView.CursorPos := After;
  EditView.Block.EndBlock;
  EditView.Block.SetVisible(True);
  EditView.Paint;
end;

function GxOtaGetSelection(const EditView: IOTAEditView;
  var BlockStart, BlockEnd: TOTAEditPos; var SelStart, SelLength: Integer): Boolean;
var
  UseView: IOTAEditView;
  Block: IOTAEditBlock;
  CharPos: TOTACharPos;
begin
  Result := False;
  UseView := EditView;
  if not Assigned(UseView) then
    UseView := GxOtaGetTopmostEditView;
  if Assigned(UseView) then
  begin
    Block := UseView.Block;
    if Assigned(Block) then
    begin
      BlockStart.Col := Block.StartingColumn;
      BlockStart.Line := Block.StartingRow;
      BlockEnd.Col := Block.EndingColumn;
      BlockEnd.Line := Block.EndingRow;

      if GxOtaCompareEditPos(BlockStart, BlockEnd) < 0 then
        UseView.ConvertPos(True, BlockStart, CharPos)
      else
        UseView.ConvertPos(True, BlockEnd, CharPos);

      SelStart := UseView.CharPosToPos(CharPos);
      SelLength := Block.Size;

      Result := True;
    end;
  end;
end;

procedure GxOtaUnfoldAllRegions(const View: IOTAEditView);
{$IFDEF GX_VER160_up}
var
  ElideActions: IOTAElideActions;
begin
  Assert(Assigned(View));
  Assert(Assigned(View.Buffer));
  if IsBdsSourceFile(View.Buffer.FileName) then // SQL files, for example, can raise an exception below
    if Supports(View, IOTAElideActions, ElideActions) then
      ElideActions.UnElideAllBlocks;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure GxOtaUnfoldNearestRegion(const View: IOTAEditView);
{$IFDEF GX_VER160_up}
var
  ElideActions: IOTAElideActions;
begin
  Assert(Assigned(View));
  Assert(Assigned(View.Buffer));
  if IsBdsSourceFile(View.Buffer.FileName) then // SQL files, for example, can raise an exception below
    if Supports(View, IOTAElideActions, ElideActions) then
      ElideActions.UnElideNearestBlock;
end;
{$ELSE}
begin
end;
{$ENDIF}

function GxOtaDeleteSelection(const EditView: IOTAEditView): Boolean;
var
  UseView: IOTAEditView;
  Block: IOTAEditBlock;
begin
  Result := False;
  UseView := EditView;
  if not Assigned(UseView) then
    UseView := GxOtaGetTopmostEditView;
  if Assigned(UseView) then
  begin
    Block := UseView.Block;
    if Assigned(Block) then
    begin
      Block.Delete;
      Result := True;
    end;
  end;
end;

function GxOtaCloseFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  Result := ActionServices.CloseFile(FileName);
end;

procedure GxOtaCloseCurrentEditorTab;
var
  EditActions: IOTAEditActions;
begin
  EditActions := GxOtaGetEditActions;
  if Assigned(EditActions) then
    EditActions.ClosePage;
end;

function GxOtaOpenFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  Result := ActionServices.OpenFile(FileName);
end;

function GxOtaOpenFileOrForm(const FileName: string): Boolean;
begin
  if IsForm(FileName) then begin
    Result := GxOtaOpenFile(GxOtaGetBaseModuleFileName(FileName));
    GxOtaShowFormForModule(GxOtaGetCurrentModule);
  end
  else
    Result := GxOtaOpenFile(FileName);
end;

function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
  FileName: string;
begin
  Result := False;

  if IsStandAlone then
    Exit;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  FileName := AFileName;
  if UseBase then
    FileName := GxOtaGetBaseModuleFileName(FileName);

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for i := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := GxOtaGetFileEditorForModule(Module, i);
      Assert(Assigned(FileEditor));

      Result := SameFileName(FileName, FileEditor.FileName);
      if Result then
        Exit;
    end;
  end;
end;

function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := FileExists(AFileName) or GxOtaIsFileOpen(AFileName, UseBase);
end;

function GxOtaMakeSourceVisible(const FileName: string): Boolean;
var
  EditActions: IOTAEditActions;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  FileEditor: IOTAEditor;
  i: Integer;
  BaseFileName: string;
begin
  BaseFileName := GxOtaGetBaseModuleFileName(FileName);
  Module := GxOtaGetModule(BaseFileName);
  if Module = nil then
    Module := GxOtaGetModule(FileName);
  if Module = nil then
    Module := GxOtaGetModule(ChangeFileExt(FileName, '.dfm'));

  if Module <> nil then
  begin
    if IsForm(FileName) then
    begin
      if not GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module, BaseFileName);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
        begin
          FormEditor := GxOtaGetFormEditorFromModule(Module);
          FormEditor.Show;
          EditActions.SwapSourceFormView;
        end;
      end
    end
    else // We are focusing a regular text file, not a form
    begin
      if GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
          EditActions.SwapSourceFormView;
      end;
    end;
  end;

  // D5/BDS 2006 sometimes delay opening the file until messages are processed
  Application.ProcessMessages;

  if not (GxOtaIsFileOpen(BaseFileName) or GxOtaIsFileOpen(FileName)) then
    Result := GxOtaOpenFile(FileName)
  else
    Result := True;

  if Result then
  begin
    {$IFDEF VER160}
    // Delphi 8 can not open both the module and the form text, so stop here
    if IsForm(FileName) then
      Exit;
    {$ENDIF VER160}
    Module := GxOtaGetModule(BaseFileName);
    if Module = nil then
      Module := GxOtaGetModule(FileName);
    if Module <> nil then
    begin
      for i := 0 to Module.GetModuleFileCount-1 do
      begin
        FileEditor := Module.GetModuleFileEditor(i);
        Assert(Assigned(FileEditor));

        if SameFileName(FileEditor.FileName, FileName) then
        begin
          FileEditor.Show;
          Exit;
        end;
      end;
    end;
    Result := False;
  end;
end;

function GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
var
  Editor: IOTAEditor;
begin
  Result := False;
  if Module.GetModuleFileCount = 1 then
  begin
    Editor := GxOtaGetFileEditorForModule(Module, 0);
    Assert(Assigned(Editor));
    if IsForm(Editor.GetFileName) then
      Result := True;
  end;
end;

procedure GxOtaGoToFileLine(const FileName: string; Line: Integer);
var
  EditRead: TEditReader;
begin
  if not GxOtaIsFileOpen(FileName) then
    if not GxOtaOpenFile(FileName) then
      raise Exception.Create('Error opening ' + FileName);

  if Line < 0 then
    Exit;

  EditRead := TEditReader.Create(FileName);
  try
    EditRead.GotoLine(Line);
  finally
    FreeAndNil(EditRead);
  end;
end;

procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0; StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
var
  EditView: IOTAEditView;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  CurPos: TOTAEditPos;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  MatchLength: Integer;
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
begin
  // Force the source editor to show the right file (cpp, pas, dfm, xfm, etc.)
  if not GxOtaMakeSourceVisible(FileName) then
    raise Exception.CreateFmt(SCouldNotOpenFile, [FileName]);

  Module := GxOtaGetModule(FileName);
  if not Assigned(Module) then
    Exit;

  SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
  if not Assigned(SourceEditor) then
    Exit;

  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;

  SourceEditor.Show;
  EditView.GetEditWindow.Form.Update;
  EditView.Block.SetVisible(False);

  // Set the top line of the edit view
  CurPos.Col := 1;
  CurPos.Line := Line;
  if ShowInMiddle then
    CurPos.Line := CurPos.Line - (EditView.ViewSize.cy div 2);
  if CurPos.Line < 1 then
   CurPos.Line := 1;
  EditView.TopPos := CurPos;

  Application.ProcessMessages;

  GxOtaFocusCurrentIDEEditControl;

  // Position the cursor to the line and column of the match
  CharPos.CharIndex := StartColumn - 1;
  CharPos.Line := Line;
  EditView.ConvertPos(False, EditPos, CharPos);
  EditView.CursorPos := EditPos;
  // This is disabled since it causes crashes in D2007 jumping to matches in data modules with no opened project
  // inside EdScript.TOTAEditView.unElideNearestBlock
  //GxOtaUnfoldNearestRegion(EditView);
  EditView.CursorPos := EditPos;
  EditView.Block.BeginBlock;
  MatchLength := StopColumn - StartColumn + 1;
  // This calculation doesn't work when there are tabs inside the match text (rare)
  EditPos.Col := EditPos.Col + MatchLength;
  EditView.CursorPos := EditPos;
  EditView.Block.EndBlock;
  EditView.Block.SetVisible(True);
  EditView.Paint;
end;

function GxOtaGetBaseModuleFileName(const FileName: string): string;
var
  AltName: string;
begin
  Result := FileName;
  if IsForm(FileName) then
  begin
    if GxOtaHaveCPPSupport then
    begin
      AltName := ChangeFileExt(FileName, '.cpp');
      if GxOtaFileOrModuleExists(AltName) then
        Result := AltName;
    end;
    AltName := ChangeFileExt(FileName, '.pas');
    if GxOtaFileOrModuleExists(AltName) then
      Result := AltName;
  end;
end;

function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
begin
  Result := GxOtaGetIDEServices.GetEnvironmentOptions;
  Assert(Assigned(Result));
end;

function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;
begin
  Result := GxOtaGetEnvironmentOptions.Values[EnvironmentStringName];
end;

procedure GxOtaGetIdeEnvironmentStrings(Settings: TStrings);
var
  EnvOptions: IOTAEnvironmentOptions;
  i: Integer;
  Options: TOTAOptionNameArray;
begin
  EnvOptions := GxOtaGetEnvironmentOptions;

  Settings.Clear;
  Options := EnvOptions.GetOptionNames; // Broken in Delphi 2005
  for i := 0 to Length(Options) - 1 do
    Settings.Add(Options[i].Name);
end;

function GxOtaGetIdeLibraryPath: string;
begin
   // Do not localize.
  if RunningBDS2006OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
    Result := GxOtaGetIdeEnvironmentString('DotNetLibraryPath')
  else if RunningBDS2006OrGreater and GxOtaCurrentProjectIsNativeCpp then
    Result := GxOtaGetIdeEnvironmentString('CppSearchPath')
  else
    Result := GxOtaGetIdeEnvironmentString('LibraryPath');
end;

function GxOtaGetIdeBrowsingPath: string;
begin
   // Do not localize.
  if RunningBDS2006OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
    Result := GxOtaGetIdeEnvironmentString('DotNetBrowsingPath')
  else if RunningBDS2006OrGreater and GxOtaCurrentProjectIsNativeCpp then
    Result := GxOtaGetIdeEnvironmentString('CppBrowsingPath')
  else if (not RunningBDS2006OrGreater) or GxOtaCurrentProjectIsNativeDelphi then
    Result := GxOtaGetIdeEnvironmentString('BrowsingPath');
end;

procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject; DoProcessing: Boolean);
const
  IDEBaseMacros: array [0..2] of string = ('BDS', 'DELPHI', 'BCB');
var
  BasePath: string;
  Environment: TStringList;

  function ReplaceMacro(const Str, OldValue, NewValue: string): string;
  var
    ReplaceVal: string;
  begin
    ReplaceVal := '$(' + OldValue + ')';
    Result := StringReplace(Str, ReplaceVal, NewValue, [rfReplaceAll, rfIgnoreCase]);
  end;

  procedure AddPathItemToList(PathItem: string; const Prefix: string);
  var
    i: Integer;
    EnvName: string;
    EnvValue: string;
  begin
    if DoProcessing then
    begin
      // Expand the IDE base folder names $([DELPHI,BCB,BDS])
      for i := Low(IDEBaseMacros) to High(IDEBaseMacros) do
        PathItem := ReplaceMacro(PathItem, IDEBaseMacros[i], BasePath);

      // Expand any environment variable macros
      for i := 0 to Environment.Count - 1 do begin
        EnvName := Environment.Names[i];
        EnvValue := Environment.Values[Environment.Names[i]];
        if (Trim(EnvName) <> '') and (Trim(EnvValue) <> '') then
          PathItem := ReplaceMacro(PathItem, EnvName, EnvValue);
      end;

      if not IsPathAbsolute(PathItem) then
      begin
        if Prefix <> '' then
          if (Prefix[Length(Prefix)] <> PathDelim) and (PathItem[1] <> PathDelim) then
            PathItem := AddSlash(PathItem);
        PathItem := Prefix + PathItem;
      end;
      // Ensure the directory exists and has a trailing slash
      if not DirectoryExists(PathItem) then
        Exit;
      PathItem := AddSlash(PathItem);
    end;
    PathItem := AddSlash(PathItem);
    EnsureStringInList(Paths, PathItem);
  end;

  procedure ProcessIdePathString(PathString: string; const Prefix: string);
  var
    PathItem: string;
    SemicolonPos: Integer;
  begin
    SemicolonPos := Pos(';', PathString);  // Is this stored the same under Kylix?
    while SemicolonPos > 0 do
    begin
      PathItem := Trim(Copy(PathString, 1, SemicolonPos - 1));
      if PathItem <> '' then
        AddPathItemToList(PathItem, Prefix);

      Delete(PathString, 1, SemicolonPos);
      SemicolonPos := Pos(';', PathString);
    end;
    PathString := Trim(PathString);
    if PathString <> '' then
      AddPathItemToList(PathString, Prefix);
  end;

var
  IdePathString: string;
  ProjectOptions: IOTAProjectOptions;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  BasePath := RemoveSlash(GetIdeRootDirectory);
  Environment := TStringList.Create;
  try
    GetEnvironmentVariables(Environment);
    if Project = nil then
      Project := GxOtaGetCurrentProject;
    if Assigned(Project) then
    begin
      // Add the current project directory first
      ProcessIdePathString(ExtractFileDir(Project.FileName), ExtractFileDir(Project.FileName));
      // Then the project search path
      ProjectOptions := Project.GetProjectOptions;
      if Assigned(ProjectOptions) then
      begin
        IdePathString := ProjectOptions.Values['SrcDir'];
        ProcessIdePathString(IdePathString, ExtractFilePath(Project.FileName));
      end;
    end;
    // And finally the IDE global search path
    IdePathString := GxOtaGetIdeLibraryPath;
    ProcessIdePathString(IdePathString, GetCurrentDir);
  finally
    FreeAndNil(Environment);
  end;
end;

procedure GxOtaGetAllPossiblePaths(Paths: TStrings);

  function GetPath(const FileName: string): string;
  begin
    Result := RemoveSlash(ExtractFilePath(FileName));
  end;

  procedure AddAPath(const APath: string);
  begin
    if DirectoryExists(APath) then
      EnsureStringInList(Paths, APath);
  end;

  procedure AddVCLPaths;
  var
    BasePath: string;
  begin
    BasePath := AddSlash(GetIdeRootDirectory) + 'Source' + PathDelim;
    AddAPath(BasePath + 'RTL\Common');
    AddAPath(BasePath + 'RTL\Corba');
    AddAPath(BasePath + 'RTL\Sys');
    AddAPath(BasePath + 'RTL\Win');
    AddAPath(BasePath + 'VCL');
    AddAPath(BasePath + 'CLX');
    AddAPath(BasePath + 'Decision Cube');
    AddAPath(BasePath + 'Indy');
    AddAPath(BasePath + 'Internet');
    AddAPath(BasePath + 'IntraWeb');
    AddAPath(BasePath + 'Property Editors');
    AddAPath(BasePath + 'Samples');
    AddAPath(BasePath + 'Soap');
    AddAPath(BasePath + 'ToolsAPI');
    AddAPath(BasePath + 'WebMidas');
    AddAPath(BasePath + 'WebSnap');
    AddAPath(BasePath + 'Xml');
    AddAPath(BasePath + 'db');
    AddAPath(BasePath + 'dbwebcontrols');
    AddAPath(BasePath + 'IBX');
    AddAPath(BasePath + 'IndyNet');
    AddAPath(BasePath + 'rtl');
    AddAPath(BasePath + 'Win32\vcl');
    AddAPath(BasePath + 'Win32\rtl\common');
    AddAPath(BasePath + 'Win32\rtl\sys');
    AddAPath(BasePath + 'Win32\rtl\win');
    AddAPath(BasePath + 'Win32\Internet');
    AddAPath(BasePath + 'Win32\soap');
    AddAPath(BasePath + 'Win32\WebSnap');
    AddAPath(BasePath + 'Win32\xml');
    AddAPath(BasePath + 'dotNet\rtl');
    AddAPath(BasePath + 'dotNet\vcl');
    AddAPath(BasePath + 'dotNet\db');
    AddAPath(BasePath + 'dotNet\dbwebcontrols');
    AddAPath(BasePath + 'dotNet\IBX');
    AddAPath(BasePath + 'dotNet\IndyNet');
    AddAPath(BasePath + 'dotNet\IndyNet\Core');
    AddAPath(BasePath + 'dotNet\xml');
  end;

var
  UnitList: TList;
  UnitInfo: TUnitInfo;
  i: Integer;
begin
  Assert(Assigned(Paths));
  // Add library search paths
  GxOtaGetEffectiveLibraryPath(Paths);
  // Add current file path
  AddAPath(GetPath(GxOtaGetFileNameOfCurrentModule));
  // Add path of the project group
  AddAPath(GetPath(GxOtaGetProjectGroupFileName));
  // Add path of the current project
  AddAPath(GetPath(GxOtaGetCurrentProjectFileName));
  // Add path of the current source file (probably same as first one)
  AddAPath(GetPath(GxOtaGetCurrentSourceFile));
  // Add paths of all files included in the project
  UnitList := TList.Create;
  try
    GxOtaFillUnitInfoListForCurrentProject(UnitList);
    for i := 0 to UnitList.Count - 1 do
    begin
      UnitInfo := TUnitInfo(UnitList[i]);
      AddAPath(GetPath(UnitInfo.FileName));
    end;
  finally
    FreeAndNil(UnitList);
  end;
  // Add paths to VCL source (since we are smart)
  AddVCLPaths;
end;

function GxOtaFindPathToFile(const FileName: string; Paths: TStrings): string;

  function MakeFilename(const Path, FileName: string): string;
  begin
    if Path = '' then
      Result := FileName
    else if Path[Length(Path)] = PathDelim then
      Result := Path + FileName
    else
      Result := Path + PathDelim + FileName;
  end;

var
  PathList: TStringList;
  i: Integer;
  NewFileName: string;
begin
  Result := FileName;
  if not (IsPathAbsolute(Result) and FileExists(Result)) then
  begin
    PathList := TStringList.Create;
    try
      PathList.Sorted := True;
      if Assigned(Paths) then
        PathList.Assign(Paths)
      else
        GxOtaGetAllPossiblePaths(PathList);
      for i := 0 to PathList.Count - 1 do
      begin
        NewFileName := MakeFilename(PathList[i], FileName);
        if FileExists(NewFileName) then
        begin
          Result := NewFileName;
          Break;
        end;
      end;
    finally
      FreeAndNil(PathList);
    end;
  end;
end;

function GxOtaOpenFileFromPath(const FileName: string): Boolean;
var
  FoundFile: string;
begin
  Result := False;
  FoundFile := GxOtaFindPathToFile(FileName);
  if FileExists(FoundFile) then
    Result := GxOtaOpenFile(FoundFile);
end;

procedure GxOtaGetUnitAliases(Aliases: TStrings);
var
  AliasItem, AliasString: string;
  VarAliases: Variant;
  SemicolonPos: Integer;
  FoundAliases: Boolean;
begin
  Assert(Assigned(Aliases));
  Aliases.Clear;
  FoundAliases := GxOtaGetActiveProjectOption('UnitAliases', VarAliases);
  if FoundAliases then
  begin
    AliasString := VarAliases;
    SemicolonPos := Pos(';', AliasString);  // Is this stored the same under Kylix?
    while SemicolonPos > 0 do
    begin
      AliasItem := AnsiLowerCase(Trim(Copy(AliasString, 1, SemicolonPos - 1)));
      if (AliasItem <> '') then
        EnsureStringInList(Aliases, AliasItem);
      Delete(AliasString, 1, SemicolonPos);
      SemicolonPos := Pos(';', AliasString);
    end;
    AliasString := Trim(AliasString);
    if (AliasString <> '') then
      EnsureStringInList(Aliases, AliasString);
  end;
end;

function GxOtaGetIdeBaseRegistryKey: string;
begin
  if IsStandAlone then
    Result := 'Software\Borland\' + CompilerDefinedProductRegistryKey
  else
    Result := GxOtaGetIDEServices.GetBaseRegistryKey;

  if Length(Result) > 0 then
  begin
    // Windows 2000 is allergic to a leading backslash
    // in the registry key - NT4, for instance, is not.
    if Result[1] = '\' then
      Delete(Result, 1, 1);

    Assert(Result[Length(Result)] <> '\');
  end;
end;

// This should only be used for a display that does not care about the format, it can change regularly
function GxOtaGetIDEProductIdentifier: string;
begin
  if IsStandAlone then
  begin
    if RunningCppBuilder then
      Result := 'C++Builder'
    else if RunningLinux then
      Result := 'Kylix'
    else if RunningBDS2006 then
      Result := 'Borland Developer Studio'
    else
      Result := 'Delphi';  // Delphi 6/7/8/2005/2007.  Check Cogswell.
  end
  else
  begin
    Result := GxOtaGetIDEServices.GetProductIdentifier; // D2007: 'CodeGear Delphi for Microsoft Windows'
    Assert(NotEmpty(Result), 'GetProductIdentifier returns nothing');
  end;
end;

function GxOtaGetIdeImageList: TCustomImageList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ImageList;

  Assert(Assigned(Result));
end;

function GxOtaGetIdeMainMenu: TMainMenu;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices), 'No BorlandIDEServices found');
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices), 'No INTAServices found');
  Result := NTAServices.MainMenu;

  Assert(Assigned(Result), 'No IDE main menu found');
end;

function GxOtaGetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;
  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;
end;

function GxOtaGetIdeActionByName(const Name: string): TContainedAction;
var
  IdeActionList: TCustomActionList;
  InspectedAction: TContainedAction;
  i: Integer;
begin
  IdeActionList := GxOtaGetIdeActionList;
  Assert(Assigned(IdeActionList));
  Result := nil;
  i := IdeActionList.ActionCount;
  while i > 0 do
  begin
    Dec(i);

    InspectedAction := IdeActionList.Actions[i];
    Assert(Assigned(InspectedAction));

    if SameText(InspectedAction.Name, Name) then
    begin
      Result := InspectedAction;
      Break;
    end;
  end;
(*
  if not Assigned(Result) then
  begin
    MainForm := GetIdeMainForm;
    if Assigned(MainForm) then
    begin
      Action := MainForm.FindComponent(Name);
      if Action is TCustomAction then
        Result := Action as TCustomAction;
    end;
  end;
*)
end;

function GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView;
begin
  if SourceEditor = nil then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
    Result := SourceEditor.EditViews[0]
  else
    Result := nil;
end;

// Delphi 2005 Update 1 does not support IOTAEditorServices!
{$IFDEF VER170}
type
  IOTAEditorServices = IOTAEditorServices70;
{$ENDIF VER170}

function GxOtaGetEditorServices: IOTAEditorServices;
begin
  Result := (BorlandIDEServices as IOTAEditorServices);
  Assert(Assigned(Result));
end;

function GxOtaGetTopMostEditView: IOTAEditView;
begin
  Result := nil;
  // Bug: Delphi 5/6 crash when calling TopView with no files open
  if GxOtaGetOpenModuleCount = 0 then
    Exit;
  Result := GxOTAGetEditorServices.TopView;
end;

function GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
begin
  Result := GxOTAGetEditorServices.TopBuffer;
end;

function GxOtaGetTopMostEditBufferFileName: string;
var
  Buffer: IOTAEditBuffer;
begin
  Result := '';
  Buffer := GxOtaGetTopMostEditBuffer;
  if Assigned(Buffer) then
    Result := Buffer.FileName;
end;

function GxOtaGetOpenModuleCount: Integer;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.ModuleCount;
end;

function GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.CurrentModule;
end;

function GxOtaGetCurrentSourceEditor: IOTASourceEditor;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := nil;
  EditBuffer := GxOtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and (EditBuffer.FileName <> '') then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule, EditBuffer.FileName);
  if Result = nil then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule);
end;

procedure GxOtaShowCurrentSourceEditor;
var
  Editor: IOTASourceEditor;
begin
  Editor := GxOtaGetCurrentSourceEditor;
  if Assigned(Editor) then
    Editor.Show;
end;

procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);
begin
  Assert(Assigned(SourceEditor));
  if Supports(SourceEditor, IOTAEditBuffer) then
    if (SourceEditor as IOTAEditBuffer).IsReadOnly then
      raise Exception.CreateFmt('%s is read only', [ExtractFileName(SourceEditor.FileName)]);
end;

function GxOtaGetModule(const FileName: string): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.FindModule(FileName);
end;

function GxOtaGetCurrentSourceFile: string;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
begin
  Result := '';
  Module := GxOtaGetCurrentModule;
  if Module <> nil then
  begin
    Editor := Module.GetCurrentEditor;
    if Editor <> nil then
      Result := Editor.FileName
    else // C++Builder 6 returns nil for some old-style modules without DFMs
      Result := Module.FileName;
  end;
end;

function GxOtaGetCurrentSyntaxHighlighter(SourceEditor: IOTASourceEditor): TGXSyntaxHighlighter;
var
{$IFDEF GX_VER150_up}
  EditView: IOTAEditView;
  EditBuffer: IOTAEditBuffer;
  EditOptions: IOTAEditOptions;
{$ELSE not GX_VER150_up}
  FileName: string;
  Ext: string;
  Highlighter: TOTASyntaxHighlighter;
{$ENDIF not GX_VER150_up}
begin
  Result := gxpNone;
  if SourceEditor = nil then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    Exit;

// TODO 4 -cCleanup -oAnyone: Move mapping out of this routine, into separate lookup
{$IFDEF GX_VER150_up}
  // difficult case -> Get 'new style' IOTAEditOptions and determine style from that
  // in D7 and up, IOTASourceEditor.SetSyntaxHighlighter does not function any more.
  // so we have to use some heuristics from EditOptions to find out what
  // syntax highlighter is actually being used
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;
  EditBuffer := EditView.Buffer;
  if not Assigned(EditBuffer) then
    Exit;
  EditOptions := EditBuffer.EditOptions;
  if not Assigned(EditOptions) then
    Exit;
  if EditOptions.IDString = cDefEdPascal then
    Result := gxpPAS
  else if EditOptions.IDString = cDefEdC then
    Result := gxpCPP
  else if EditOptions.IDString = cDefEdCSharp then
    Result := gxpCS
  else if EditOptions.IDString = cDefEdHTML then
    Result := gxpHTML
  else if EditOptions.IDString = cDefEdXML then
    Result := gxpXML
  else if EditOptions.IDString = cDefEdSQL then
    Result := gxpSQL;
  // Ignore: cDefEdIDL, cDefEdVisualBasic, cDefEdJavaScript, cDefEdStyleSheet, cDefEdINI, cDefEdPHP
{$ELSE not GX_VER150_up}
  FileName := SourceEditor.FileName;
  Highlighter := SourceEditor.SetSyntaxHighlighter(ToolsAPI.shQuery);
  case Highlighter of
    shPascal: Result := gxpPas;
    shC: Result := gxpCPP;
    shSQL: Result := gxpSQL;
    else begin
      Ext := ExtractUpperFileExt(FileName);
      if StringInArray(Ext, ['XML', 'BDSGROUP', 'BDSPROJ', 'DPROJ']) then
        Result := gxpXML
      else if StringInArray(Ext, ['HTM', 'HTML', 'ASP']) then
        Result := gxpHTML
      else if StringInarray(Ext, ['CS']) then
        Result := gxpCS
    end;
  end
{$ENDIF GX_VER150_up}
end;

function GxOtaGetPropertyIndexByName(const AComponent: IOTAComponent; const PropertyName: WideString): Integer;
begin
  Assert(Assigned(AComponent));

  Result := AComponent.GetPropCount-1;
  while Result >= 0 do
  begin
    if SameText(PropertyName, AComponent.GetPropName(Result)) then
      Break;

    Dec(Result);
  end;
end;

function GxOtaEditorIsRecordingMacro: Boolean;
var
  CurRecord: IOTARecord;
begin
  Result := False;
  CurRecord := GxOtaGetKeyboardServices.CurrentRecord;
  if Assigned(CurRecord) then
    Result := CurRecord.IsRecording;
end;

function GxOtaEditorIsPlayingMacro: Boolean;
var
  CurPlayback: IOTARecord;
begin
  Result := False;
  CurPlayback := GxOtaGetKeyboardServices.CurrentPlayback;
  if Assigned(CurPlayback) then
    Result := CurPlayback.IsPlaying;
end;

function GxOtaEditorIsPausedMacro: Boolean;
var
  CurRecord: IOTARecord;
begin
  Result := False;
  CurRecord := GxOtaGetKeyboardServices.CurrentRecord;
  if Assigned(CurRecord) then
    Result := CurRecord.IsPaused;
end;

procedure GxOtaSetEditorKeyTracingEnabled(Enabled: Boolean);
begin
  (BorlandIDEServices as ToolsAPI.IOTAKeyboardDiagnostics).KeyTracing := Enabled;
end;

function GxOtaGetEditorKeyTracingEnabled: Boolean;
begin
  Result := (BorlandIDEServices as ToolsAPI.IOTAKeyboardDiagnostics).KeyTracing;
end;

type
  // TODO 3 -cCleanup -oAnyone: Remove this type, since it is confusing and is not .NET compatible
  TPropertyValBuffer = packed record
    case Integer of
      0: (RawBuffer: array[0..255] of Byte);
      1: (SString: ShortString);
      2: (Int: Integer);
      3: (Int64: Int64);
      4: (LString: PAnsiChar);
      5: (WString: PWChar);
      6: (VChar: Char);
      7: (VWChar: WideChar);
      8: (VFloat: Extended);
    end;

function GxOtaGetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; PreferStrings: Boolean): WideString;
var
  PropertyType: TTypeKind;
  VVariant: Variant;
  Buffer: TPropertyValBuffer;
  NativeObject: TObject;
begin
  Assert(Assigned(AComponent));
  Result := '';
  if Trim(PropertyName) = '' then
    Exit;
  PropertyType := AComponent.GetPropTypeByName(PropertyName);

  Buffer.LString := nil;
  Buffer.WString := nil;
  Buffer.Int64 := 0;
  if not AComponent.GetPropValueByName(PropertyName, Buffer) then
    Exit;

  case PropertyType of
    tkLString: Result := Buffer.LString;
    tkString:  Result := Buffer.SString;
    tkInteger: Result := IntToStr(Buffer.Int);
    tkInt64:   Result := IntToStr(Buffer.Int64);
    tkChar:    Result := Buffer.VChar;
    tkWChar:   Result := Buffer.VWChar;
    tkFloat:   Result := FloatToStr(Buffer.VFloat);
    tkWString: Result := Buffer.WString;
    tkSet:
      begin
        if PreferStrings and GxOtaActiveDesignerIsVCL then
        begin
          NativeObject := GxOtaGetNativeObject(AComponent);
          Assert(Assigned(NativeObject), 'No native object for property ' + PropertyName);
          Result := SetToString(FindTypeInfo(NativeObject, PropertyName), Buffer.Int);
        end
        else
          Result := IntToStr(Buffer.Int);
      end;
    tkEnumeration:
      begin
        if PreferStrings and GxOtaActiveDesignerIsVCL then
        begin
          NativeObject := GxOtaGetNativeObject(AComponent);
          Assert(Assigned(NativeObject));
          Result := GetEnumName(FindTypeInfo(NativeObject, PropertyName), Buffer.Int);
        end
        else
          Result := IntToStr(Buffer.Int);
      end;
    tkVariant:
      begin
        if not AComponent.GetPropValueByName(PropertyName, VVariant) then
          Exit;
        Result := VVariant;
      end;
    // Unsupported: tkUnknown, tkMethod, tkInterface, tkDynArray, tkClass, tkArray, tkRecord
    else raise Exception.Create('Unsupported property type ' + GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType)));
  end;
end;

function GxOtaNormalizePropertyValue(Component: IOTAComponent; const PropName, Value: string): string;
var
  PropType: TTypeKind;
  NativeObject: TObject;
begin
  Assert(Assigned(Component));
  Result := Value;
  PropType := Component.GetPropTypeByName(PropName);
  if PropType = tkEnumeration then
  begin
    NativeObject := GxOtaGetNativeObject(Component);
    if Assigned(NativeObject) then
      Result := IntToStr(GetEnumValueFromStr(NativeObject, PropName, Value))
    else
    begin // CLX, .net, etc?
      if PropType = tkEnumeration then begin
        if SameText(Value, 'True') then
          Result := '1'
        else if SameText(Value, 'False') then
          Result := '0';
      end;
    end;
  end;
end;

function GxOtaSetComponentPropertyAsString(const AComponent: IOTAComponent;
  const PropertyName: string; const Value: WideString): Boolean;
var
  PropertyType     : TTypeKind;
  PropertyTypeName : string;
  ComponentName    : WideString;
  NativeObject     : TObject;
  VInteger         : Integer;
  VInt64           : Int64;
  VVariant         : Variant;
  VAString         : AnsiString;
  VShortString     : string;
  VFloat           : Extended;
  VAChar           : AnsiChar;
  VWChar           : WChar;
  VBoolean         : Boolean;

  function PropertyDescription: string;
  begin
    Result := Format('%s.%s of type %s', [ComponentName, PropertyName, PropertyTypeName]);
  end;

begin
  Assert(Assigned(AComponent));
  Result := False;
  if Trim(PropertyName) = '' then
    Exit;
  ComponentName := GxOtaGetComponentName(AComponent);
  NativeObject := GxOtaGetNativeObject(AComponent);

  PropertyType := AComponent.GetPropTypeByName(PropertyName);
  PropertyTypeName := GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType));
  case PropertyType of
    tkWString: Result := AComponent.SetPropByName(PropertyName, Value);

    tkLString: begin
        VAString := Value;
        Result := AComponent.SetPropByName(PropertyName, VAString);
      end;

    tkString: begin
        VShortString := Value;
        Result := AComponent.SetPropByName(PropertyName, VShortString);
      end;

    tkInteger: begin
        VInteger := StrToInt(Value);
        Result := AComponent.SetPropByName(PropertyName, VInteger);
      end;

    tkInt64: begin
        VInt64 := StrToInt64(Value);
        Result := AComponent.SetPropByName(PropertyName, VInt64);
      end;

    tkVariant: begin
        VVariant := Value;
        Result := AComponent.SetPropByName(PropertyName, VVariant);
      end;

    tkFloat: begin
        VFloat :=  StrToFloat(Value);
        Result := AComponent.SetPropByName(PropertyName, VFloat);
      end;

    tkChar: begin
        if Length(Value) > 0 then begin
          VAChar := AnsiChar(Value[1]);
          Result := AComponent.SetPropByName(PropertyName, VAChar);
        end;
      end;

    tkWChar: begin
        if Length(Value) > 0 then begin
          VWChar := Value[1];
          Result := AComponent.SetPropByName(PropertyName, VWChar);
        end;
      end;

    // Note: Booleans are of type tkEnumeration
    tkEnumeration: begin
        if Assigned(NativeObject) then begin
          VInteger := GetEnumValueFromStr(NativeObject, PropertyName, Value);
          Result := AComponent.SetPropByName(PropertyName, VInteger);
        end
        else begin // VCL.NET error in BDS 2006: "Invalid property typeinfo type": http://qc.borland.com/wc/qcmain.aspx?d=42751
          if SameText(Value, 'TRUE') or SameText(Value, 'FALSE') then begin
            VBoolean := SameText(Value, 'TRUE');
            Result := AComponent.SetPropByName(PropertyName, VBoolean);
          end
          else begin
            VInteger := StrToIntDef(Value, -99998);
            if VInteger <> -99998 then
              Result := AComponent.SetPropByName(PropertyName, VInteger)
            else
              raise Exception.CreateFmt('Unsupported property value for %s: %s (try using the integer representing the enum value)', [PropertyDescription, Value]);
          end;
        end;
      end;

    // Support for sets is partial, because we don't support normalizing the values
    tkSet: begin
        if Assigned(NativeObject) then begin
          VInteger := ApplyValueToSetProperty(NativeObject, PropertyName, Value);
          SetOrdProp(NativeObject, PropertyName, VInteger);
        end
        else
          raise Exception.CreateFmt('No native object available to set component property %s', [PropertyDescription]);
      end;

    else // Unsupported: tkUnknown, tkMethod, tkInterface, tkDynArray, tkClass, tkArray, tkRecord
      raise Exception.CreateFmt('Unsupported property type for %s', [PropertyDescription]);
  end;
end;

function GxOtaGetNativeObject(const AComponent: IOTAComponent): TObject;
var
  CompHandle: TOTAHandle;
begin
  Result := nil;
  if (AComponent = nil) or ((GxOtaCurrentlyEditingForm) and (not GxOtaActiveDesignerIsVCL)) then
    Exit;
  CompHandle := AComponent.GetComponentHandle;
  if Assigned(CompHandle) then
    Result := TObject(CompHandle);
end;

function GxOtaGetNativePersistent(const AComponent: IOTAComponent): TPersistent;
var
  ComponentObject: TObject;
begin
  Result := nil;
  ComponentObject := GxOtaGetNativeObject(AComponent);
  if Assigned(ComponentObject) then
  begin
    if GxOtaActiveDesignerIsNFM then begin
      {$IFOPT D+}SendDebugError('Attempt to get a native persistent for a .NET object');{$ENDIF}
      Exit;
    end;
    if ComponentObject is TPersistent then
      Result := ComponentObject as TPersistent;
  end;
end;

function GxOtaGetNativeComponent(const AComponent: IOTAComponent): TComponent;
var
  ComponentObject: TObject;
begin
  Result := nil;
  ComponentObject := GxOtaGetNativeObject(AComponent);
  if Assigned(ComponentObject) then
  begin
    if GxOtaActiveDesignerIsNFM then begin
      {$IFOPT D+}SendDebugError('Attempt to get a native component for a .NET object');{$ENDIF}
      Exit;
    end;
    if ComponentObject is TComponent then
      Result := ComponentObject as TComponent;
  end;
end;

function GxOtaIsInheritedComponent(const AComponent: IOTAComponent): Boolean;
var
  NativeComponent: TComponent;
begin
  Result := False;
  NativeComponent :=  GxOtaGetNativeComponent(AComponent);
  if Assigned(NativeComponent) then
    Result := (csAncestor in NativeComponent.ComponentState);
end;

function GxOtaGetComponentName(const AComponent: IOTAComponent): WideString;
var
  Component: TComponent;
begin
  Assert(Assigned(AComponent));
  Result := GxOtaGetComponentPropertyAsString(AComponent, NamePropertyName);
  if IsEmpty(Result) then
  begin
    Component := GxOtaGetNativeComponent(AComponent);
    if Assigned(Component) then
      Result := Component.Name;
  end;
end;

function GxOtaSetComponentName(const AComponent: IOTAComponent; const Name: WideString): Boolean;
begin
  Assert(Assigned(AComponent));
  Result := GxOtaSetComponentPropertyAsString(AComponent, NamePropertyName, Name);
end;

function GxOtaGetComponentParentName(const AComponent: IOTAComponent): WideString;
resourcestring
  SNoParent = 'No Parent';
var
  {$IFNDEF GX_VER160_up}
  Parent: TComponent;
  NativeComponent: TComponent;
  {$ELSE GX_VER160_up}
  Parent: IOTAComponent;
  {$ENDIF GX_VER160_up}
begin
  Assert(Assigned(AComponent));
  Result := SNoParent;

  // IOTAComponent.GetParent is broken in Delphi 5/6/7
  {$IFNDEF GX_VER160_up}
  NativeComponent :=  GxOtaGetNativeComponent(AComponent);
  if Assigned(NativeComponent) then
  begin
    Parent := NativeComponent.GetParentComponent;
    if Assigned(Parent) then
      Result := Parent.Name;
  end;
  {$ELSE GX_VER160_up}
  Parent := AComponent.GetParent;
  if Assigned(Parent) then
    Result := GxOtaGetComponentName(Parent);
  {$ENDIF GX_VER160_up}
end;

function GxOtaComponentsAreEqual(Comp1, Comp2: IOTAComponent): Boolean;
begin
  // Should we compare handles directly in Delphi 5/6/7?
  Result := False;
  if (not Assigned(Comp1)) or (not Assigned(Comp2)) then
    Exit;
  // Check if the parents are equal?
  Result := SameText(Comp1.GetComponentType, Comp2.GetComponentType) and
    SameText(GxOtaGetComponentName(Comp1), GxOtaGetComponentName(Comp2)) and
    SameText(GxOtaGetComponentParentName(Comp1), GxOtaGetComponentParentName(Comp2));
end;

procedure GxOtaGetInstalledComponentList(Components: TStrings; ActiveOnly: Boolean);
var
  PackageServices: IOTAPackageServices;
  PackageCounter: Integer;
  ComponentCounter: Integer;
  InstalledComponentName: string;
begin
  PackageServices := BorlandIDEServices as IOTAPackageServices;
  Assert(Assigned(PackageServices));

  Components.BeginUpdate;
  try
    Components.Clear;
    for PackageCounter := 0 to PackageServices.PackageCount-1 do
    begin
      for ComponentCounter := 0 to PackageServices.GetComponentCount(PackageCounter)-1 do
      begin
        InstalledComponentName := PackageServices.ComponentNames[PackageCounter, ComponentCounter];
        // Filter out duplicate components and filter CLX/VCL components
        if (Components.IndexOf(InstalledComponentName) = -1) then
        begin
          if (not ActiveOnly) or (GetClass(InstalledComponentName) <> nil) then
            Components.Add(InstalledComponentName);
        end;
      end;
    end;
  finally
    Components.EndUpdate;
  end;
  if Components is TStringList then
    (Components as TStringList).Sort;
end;

function GxOtaPropertyExists(const Component: IOTAComponent; const PropertyName: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  Assert(Assigned(Component));
  for i := 0 to Component.GetPropCount-1 do
  begin
    Result := SameText(Component.GetPropName(i), PropertyName);
    if Result then
      Break;
  end;
end;

function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    IEditor := GxOtaGetFileEditorForModule(Module, i);

    if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
    begin
      if Assigned(ISourceEditor) then
      begin
        if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then
        begin
          Result := ISourceEditor;
          Break;
        end;
      end;
    end;
  end;
end;

function GxOtaGetCurrentEditor: IOTAEditor;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
  TopFile: string;
begin
  Result := nil;
  Module := GxOtaGetCurrentModule;
  if Module <> nil then
  begin
    Editor := Module.GetCurrentEditor;
    if Editor <> nil then
      TopFile := Editor.FileName
    else // C++Builder 6 returns nil for some old-style modules without DFMs
      TopFile := Module.FileName;
    Result := GxOtaGetEditorFromModule(Module, TopFile);
  end;
end;

function GxOtaGetEditorFromModule(Module: IOTAModule; const FileName: string): IOTAEditor;
var
  i: Integer;
  Editor: IOTAEditor;
begin
  Result := nil;
  if Module = nil then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if SameFileName(Editor.FileName, FileName) then
    begin
      Result := Editor;
      Break;
    end;
  end;
end;

function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

function GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
var
  i: Integer;
  EditView: IOTAEditView;
  SourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if Module = nil then
    Exit;
  SourceEditor := GxOtaGetSourceEditorFromModule(Module);
  if SourceEditor = nil then
    Exit;
  for i := 0 to SourceEditor.GetEditViewCount - 1 do
  begin
    EditView := SourceEditor.GetEditView(i);
    if Supports(EditView, IOTAEditActions, Result) then
      Exit;
  end;
  Result := nil;
end;

function GxOtaGetEditActions: IOTAEditActions;
begin
  Result := GxOtaGetEditActionsFromModule(GxOtaGetCurrentModule);
end;

function GxOtaSelectedComponentIsRoot(const FormEditor: IOTAFormEditor): Boolean;
var
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  RootComponent: IOTAComponent;
begin
  Result := False;
  Assert(Assigned(FormEditor));

  // Note: No components are selected when the form editor isn't in focus
  SelCount := FormEditor.GetSelCount;

  // If SelCount = 1, either a "real" component is selected or
  // the form itself is selected.  If <> 1, the form is not selected.
  if SelCount = 1 then
  begin
    CurrentComponent := FormEditor.GetSelComponent(0);
    RootComponent := FormEditor.GetRootComponent;
    // Delphi 5 sometimes returns nil for one of the above (when the form isn't fully created?)
    if (CurrentComponent = nil) or (RootComponent = nil) then
      Exit;

    Result := SameText(RootComponent.GetComponentType, CurrentComponent.GetComponentType);
  end
end;

function GxOtaGetFormBiDiMode(Form: IOTAFormEditor): TBiDiMode;
var
  FormComponent: IOTAComponent;
  NativeForm: TComponent;
begin
  Result := bdLeftToRight;
  if Assigned(Form) then
  begin
    FormComponent := Form.GetRootComponent;
    if Assigned(FormComponent) then
    begin
      NativeForm := GxOtaGetNativeComponent(FormComponent);
      if Assigned(NativeForm) and (NativeForm is TCustomForm) then
        Result := (NativeForm as TCustomForm).BiDiMode;
    end;
  end;
end;

{$UNDEF LoadAndCheckEntryPoint}
{$DEFINE LoadAndCheckEntryPoint}

function IsValidExpertDll(const FileName: string): Boolean;
{$IFDEF LoadAndCheckEntryPoint}
{$IFDEF MSWINDOWS}
var
  DllHandle: THandle;
{$ENDIF MSWINDOWS}
{$ENDIF LoadAndCheckEntryPoint}
begin
  Result := True;

{$IFDEF LoadAndCheckEntryPoint}
  {$IFDEF MSWINDOWS}
    // Check that the DLL *really* is a valid expert or wizard DLL
    // (supported on Windows NT only).
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      DllHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES {NT only!});
      if DllHandle <> 0 then
      begin
        try
          Result := (GetProcAddress(DllHandle, ExptIntfExpertEntryPoint) <> nil);
          if not Result then
            Result := (GetProcAddress(DllHandle, WizardEntryPoint) <> nil);
        finally
          FreeLibrary(DllHandle);
        end;
      end;
    end; {NT Check}
  {$ENDIF MSWINDOWS}
{$ENDIF LoadAndCheckEntryPoint}
end;

function IsStandAlone: Boolean;
begin
  Result := (BorlandIDEServices = nil);
end;

{ TGxEditorStream }

constructor TGxEditorReadStream.Create(Editor: IOTASourceEditor);
begin
  inherited Create;

  Assert(Assigned(Editor));
  FEditReader := Editor.CreateReader;
  Assert(Assigned(FEditReader));

  FPosition := 0;
  FSize := -1;
end;

destructor TGxEditorReadStream.Destroy;
begin
  FEditReader := nil;

  inherited Destroy;
end;

function TGxEditorReadStream.GetSize: Int64;
const
  BufSize = 16384;
var
  CurSize, BytesRead: Longint;
  Buf: array[0..BufSize] of AnsiChar;
begin
  Assert(FEditReader <> nil, 'fReader is nil in TEditReaderStream.GetSize');

  if FSize < 0 then
  begin
    // IOTAEditReader does not allow us to get the size directly
    CurSize := 0;
    repeat
      BytesRead := FEditReader.GetText(CurSize, Buf, BufSize);
      Inc(CurSize, BytesRead);
    until BytesRead < BufSize;
    FSize := CurSize;
  end;
  Result := FSize;
end;

function TGxEditorReadStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FEditReader.GetText(FPosition, @Buffer, Count);
end;

function TGxEditorReadStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent:   FPosition := FPosition + Offset;
    soFromEnd:       FPosition := Size + Offset;
  else
    raise Exception.CreateFmt('Invalid seek origin, %d', [Origin]);
  end;
  Result := FPosition;
end;

function TGxEditorReadStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('Cannot write to TGxEditorReadStream');
end;

procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
const
  // Leave typed constant as is - needed for streaming code.
  TerminatingNullChar: AnsiChar = #0;
  BufferSize = 1024 * 24;
var
  EditReaderPos: Integer;
  ReadDataSize: Integer;
  Buffer: array[0..BufferSize] of AnsiChar;
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

  EditReaderPos := 0;
  ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
  Inc(EditReaderPos, ReadDataSize);
  while ReadDataSize = BufferSize do
  begin
    Stream.Write(Buffer, ReadDataSize);
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
    Inc(EditReaderPos, ReadDataSize);
  end;
  Stream.Write(Buffer, ReadDataSize);
  Stream.Write(TerminatingNullChar, SizeOf(TerminatingNullChar));
end;

function GxOtaGetActiveEditorText(Stream: TStream; UseSelection: Boolean): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
  IEditReader: IOTAEditReader;
  BlockSelText: string;
begin
  Assert(Stream <> nil);

  Result := False;

  ISourceEditor := GxOtaGetCurrentSourceEditor;
  if ISourceEditor = nil then
    Exit;

  if ISourceEditor.EditViewCount > 0 then
  begin
    IEditView := GxOtaGetTopMostEditView(ISourceEditor);
    Assert(IEditView <> nil);

    if (IEditView.Block.Size > 0) and UseSelection then
    begin
      BlockSelText := IDEEditorStringToString(IEditView.Block.Text);
      Stream.WriteBuffer(PAnsiChar(ConvertToIDEEditorString(BlockSelText))^, Length(BlockSelText) + SizeOf(Byte(0)));
      Result := True;
    end
    else
    begin
      IEditReader := ISourceEditor.CreateReader;
      GxOtaSaveReaderToStream(IEditReader, Stream);
      Result := False;
    end;
  end;
end;

function GxOtaGetActiveEditorTextAsString(var Text: string; UseSelection: Boolean): Boolean;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    Result := GxOtaGetActiveEditorText(StringStream, UseSelection);
    Text := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

function ReadEditorTextToString(EditReader: IOTAEditReader; LineStartPos: Integer; LineLength: Integer): string;
var
  AString: AnsiString;
begin
  SetLength(AString, LineLength);
  EditReader.GetText(Max(LineStartPos, 0), PAnsiChar(AString), LineLength);
  Result := AString;
end;

function GxOtaGetPreceedingCharactersInLine(View: IOTAEditView): string;
var
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  Position: Longint;
  EditReader: IOTAEditReader;
  LineStartPos: Integer;
  Offset: Integer;
  LineLength: Integer;
begin
  Assert(Assigned(View));
  Result := '';

  EditPos := View.CursorPos;
  View.ConvertPos(True, EditPos, CharPos);
  Position := View.CharPosToPos(CharPos);
  if Position < 0 then
    Exit; // Happens when you type past the 1024 character limit

  EditReader := View.Buffer.CreateReader;
  // If there are embedded tabs in the line, our offsets are off and we must
  // be sure to not request nagative indexes or return more than one line
  LineStartPos := Position - (EditPos.Col - 1);
  Offset := 0;
  if LineStartPos < 0 then // This happens with tabs at the beginning of the file
    Offset := Abs(LineStartPos);
  LineLength := EditPos.Col - 1 - Offset;
  Assert(LineLength >= 0);
  if LineLength = 0 then
    Exit;
  Result := ReadEditorTextToString(EditReader, LineStartPos, LineLength);
  Result := LastLineOf(Result);
end;

function GxOtaSelectCurrentIdent(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean = True): Boolean;
var
  EStart: TOTAEditPos;
  EEnd: TOTAEditPos;
  Ident: string;
begin
  Result := GxOtaGetCurrentIdentData(Ident, EStart);
  if Result then
  begin
    EEnd.Line := EStart.Line;
    EEnd.Col := EStart.Col + Length(Ident);
    GxOtaSelectBlock(SourceEditor, EStart, EEnd);
  end;
end;

procedure GxOtaSelectCurrentLine(const SourceEditor: IOTASourceEditor; MakeVisible: Boolean);
var
  EditPos: TOTAEditPos;
  EditView: IOTAEditView;
begin
  Assert(Assigned(SourceEditor));
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if EditView = nil then
    Exit;
  EditPos.Line := EditView.Position.Row;
  EditPos.Col := 1;
  EditView.CursorPos := EditPos;
  EditView.Block.BeginBlock;
  EditView.Block.Style := btNonInclusive;
  if EditView.Position.MoveEOL then
    EditView.MoveViewToCursor;
  EditView.Block.EndBlock;
  if MakeVisible then
  begin
    EditView.Block.SetVisible(True);
    EditView.Paint;
  end;
end;

procedure GxOtaInsertLineIntoEditor(const Text: string);
var
  EditView: IOTAEditView;
resourcestring
  SUnableToWriteToEditor = 'Unable to write to source editor';
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView), SUnableToWriteToEditor);
  EditView.Position.InsertText(ConvertToIDEEditorString(Text));
  EditView.Paint;
end;

procedure GxOtaInsertTextIntoEditor(const Text: string);
var
  EditView: IOTAEditView;
  Position: Longint;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView), 'No edit view found');
  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True, EditPos, CharPos);
  Position := EditView.CharPosToPos(CharPos);
  GxOtaInsertTextIntoEditorAtPos(Text, Position);
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

function GxOtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
resourcestring
  SEditWriterNotAvail = 'Edit writer not available';
begin
  if not Assigned(SourceEditor) then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) then
  begin
    GxOtaAssertSourceEditorNotReadOnly(SourceEditor);
    Result := SourceEditor.CreateUndoableWriter;
  end;
  Assert(Assigned(Result), SEditWriterNotAvail);
end;

procedure GxOtaInsertTextIntoEditorAtPos(const Text: string; Position: Longint;
  SourceEditor: IOTASourceEditor);
var
  EditWriter: IOTAEditWriter;
begin
  if Text = '' then
    Exit;
  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(Position);
  EditWriter.Insert(PAnsiChar(ConvertToIDEEditorString(Text)));
end;

procedure GxOtaGotoPosition(Position: Longint; EditView: IOTAEditView; Middle: Boolean);
var
  CurPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  if not Assigned(EditView) then
    EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));

  CharPos := GxOtaGetCharPosFromPos(Position, EditView);
  CurPos.Col := CharPos.CharIndex + 1;
  CurPos.Line := CharPos.Line;
  GxOtaGotoEditPos(CurPos, EditView, Middle);
end;

procedure GxOtaGotoEditPos(EditPos: TOTAEditPos; EditView: IOTAEditView; Middle: Boolean);
var
  TopRow: TOTAEditPos;
begin
  if not Assigned(EditView) then
    EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));

  if EditPos.Line < 1 then
    EditPos.Line := 1;
  TopRow := EditPos;
  if Middle then
    TopRow.Line := TopRow.Line - (EditView.ViewSize.cy div 2) + 1;
  if TopRow.Line < 1 then
    TopRow.Line := 1;
  TopRow.Col := 1;
  EditView.TopPos := TopRow;

  EditView.CursorPos := EditPos;
  Application.ProcessMessages;
  EditView.Paint;
end;

function GxOtaGetCharPosFromPos(Position: Longint; EditView: IOTAEditView): TOTACharPos;
var
  EditWriter : IOTAEditWriter;
begin
  Assert(Assigned(EditView));
  EditWriter := EditView.Buffer.CreateUndoableWriter;
  Assert(Assigned(EditWriter));
  EditWriter.CopyTo(Position);
  Result := EditWriter.CurrentPos;
end;

function GxOtaGetCurrentEditPos(EditView: IOTAEditView): TOTAEditPos;
begin
  Result.Col := -1;
  Result.Line := -1;
  if EditView = nil then
    EditView := GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;
  Result := EditView.CursorPos;
end;

function GxOtaGetCurrentEditBufferPos(EditView: IOTAEditView = nil): Integer;
var
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  if EditView = nil then
    EditView := GxOtaGetTopMostEditView;
  EditPos := GxOtaGetCurrentEditPos(EditView);
  EditView.ConvertPos(True, EditPos, CharPos);
  Result := EditView.CharPosToPos(CharPos);
end;

procedure GxOtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);
var
  EditWriter: IOTAEditWriter;
begin
  EditWriter := GxOtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(StartPos);
  EditWriter.DeleteTo(StartPos + Count);
end;

{$DEFINE GX_WorkAroundFirstCharInLineSelectionBug}
// Adapted from Ray Lischner's ReplaceSelection, but ported to use ToolsAPI.pas
procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor; ViewNum: Integer; const Text: string);
resourcestring
  SBlockNotCompatible = 'Unsupported block selection type for a replace operation';
var
  View: IOTAEditView;
  Start, After: TOTACharPos;

  procedure ReplaceColumns(Start, After: TOTAEditPos; RplText: string);
  begin
    raise Exception.Create('Replacing of columnar selections is no longer supported');
  end;

  // Replace the text between Start and After, inclusive, with Text.
  // IsInclusive determines whether the last selected character is part
  // of the replaced block or not.
  procedure ReplaceInclusiveExclusive(const IsInclusive: Boolean);
  var
    Writer: IOTAEditWriter;
    StartPos: Integer;
    AfterPos: Integer;
    DeleteToPos: Integer;

    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
      FirstCharInLineDeleted: Boolean;
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
  begin
    if not IsInclusive then
    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
    begin
      FirstCharInLineDeleted := (After.CharIndex = 1);
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    end
    else
      FirstCharInLineDeleted := False;
    {$ELSE}
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}

    StartPos := View.CharPosToPos(Start);
    Assert(StartPos >= 0, 'StartPos < 0');
    AfterPos := View.CharPosToPos(After);
    Assert(AfterPos >= 0, 'AfterPos < 0');
    Writer := Editor.CreateUndoableWriter;
    try
      // Copy the initial part of the file, up to the selection
      Writer.CopyTo(StartPos);
      // Delete the block if there is one to delete
      DeleteToPos := AfterPos;

      if (After.CharIndex = 0) and (After.Line - Start.Line = 1) then
      begin
        Dec(DeleteToPos, Length(#13#10));
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
        begin
          Inc(DeleteToPos, Length(#13#10));
          Inc(DeleteToPos);
        end;
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
      end
      else
      begin
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
          Inc(DeleteToPos)
        else if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ELSE}
        if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
      end;
      if DeleteToPos > StartPos then
        Writer.DeleteTo(DeleteToPos);
      // Insert the replacement text
      Writer.Insert(PAnsiChar(ConvertToIDEEditorString(Text)));
      // Copy the rest of the file
      Writer.CopyTo(High(Longint));
    finally
      Writer := nil;
    end;
  end;

begin
  Assert(Assigned(Editor));
  Assert(Editor.EditViewCount > ViewNum, 'Editor.EditViewCount <= passed in ViewNum');
  GxOtaAssertSourceEditorNotReadOnly(Editor);

  View := Editor.GetEditView(ViewNum);
  Assert(Assigned(View), 'No edit view available');
  Assert(Assigned(View.Block), 'No block available');

  Start := Editor.BlockStart;
  After := Editor.BlockAfter;

  case Editor.BlockType of
    btInclusive:
      ReplaceInclusiveExclusive(True);
    btNonInclusive:
      ReplaceInclusiveExclusive(False);
    btColumn:
      ReplaceColumns(TOTAEditPos(Start), TOTAEditPos(After), Text);
    btLine:
      begin
        Start.CharIndex := 0; // Start of line
        After.CharIndex := 1023; // Max line length
        ReplaceInclusiveExclusive(True);
      end;
    else
      raise Exception.Create(SBlockNotCompatible);
  end;
end;

// Should we integrate form support into TEditReader instead of using this?
function GxOtaGetFileAsText(const FileName: string; Lines: TStrings; out WasBinary: Boolean): Boolean;
var
  FileReader: TEditReader;
  MemStream: TMemoryStream;
  FormStream: TMemoryStream;
  BaseFileName: string;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
  SourceEditor: IOTASourceEditor;
  Component: TComponent;
  EditStream: TGxEditorReadStream;
  //i: Integer;
  //IsUTF8: Boolean;
begin
  WasBinary := False;
  Assert(Assigned(Lines));
  Result := False;

  MemStream := TMemoryStream.Create;
  try
    if IsForm(FileName) then
    begin
      if IsStandAlone then
        LoadFormFileToStrings(FileName, Lines, WasBinary)
      else
      begin
        BaseFileName := GxOtaGetBaseModuleFileName(FileName);
        // Is the form currently open as text
        if GxOtaIsFileOpen(FileName) then
        begin
          Module := GxOtaGetModule(FileName);
          if not Assigned(Module) then
            Exit;
          SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
          if not Assigned(SourceEditor) then
            Exit;
          EditStream := TGxEditorReadStream.Create(SourceEditor);
          try
            MemStream.LoadFromStream(EditStream);
            MemStream.Position := 0;
          finally
            FreeAndNil(EditStream);
          end;
          Lines.LoadFromStream(MemStream);
        end
        else if GxOtaIsFileOpen(BaseFileName) then
        begin
          Module := GxOtaGetModule(BaseFileName);
          if not Assigned(Module) then
            Exit;
          FormEditor := GxOtaGetFormEditorFromModule(Module);
          if not Assigned(FormEditor) then
            Exit;
          RootComponent := FormEditor.GetRootComponent;
          if RootComponent <> nil then
          begin
            Component := GxOtaGetNativeComponent(RootComponent);
            if Component <> nil then
            begin
              FormStream :=  TMemoryStream.Create;
              try
                FormStream.WriteComponent(Component);
                FormStream.Position := 0;
                ObjectBinaryToText(FormStream, MemStream);
                MemStream.Position := 0;
              finally;
                FreeAndNil(FormStream);
              end;
              Lines.LoadFromStream(MemStream);
            end;
          end;
        end
        else
          LoadFormFileToStrings(FileName, Lines, WasBinary)
      end;
    end
    else
    begin
      if IsStandAlone then
        Lines.LoadFromFile(FileName)
      else
      begin
        BaseFileName := GxOtaGetBaseModuleFileName(FileName);
        Module := GxOtaGetModule(BaseFileName);
        if Assigned(Module) then
        begin
          SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
          // If there is no source editor for the file name, we can't load it
          // We might be trying to load a .pas with the .dfm open as text
          if (not (IsBdsgroup(FileName) or IsBdsprojOrDproj(FileName))) and (not Assigned(SourceEditor)) then
            Exit;
        end;
        FileReader := TEditReader.Create(FileName);
        try
          FileReader.SaveToStream(MemStream);
          //IsUTF8 := FileReader.IsUTF8;
          MemStream.Position := 0;
        finally
          FreeAndNil(FileReader);
        end;
        Lines.LoadFromStream(MemStream);
        //if IsUTF8 then
        //begin
        //  for i := 0 to Lines.Count - 1 do
        //    // We only support chracters representable in the user's ANSI encoding (not full UNICODE)
        //    Lines[i] := Utf8ToAnsi(Lines[i]);
        //end;
      end;
    end;
  finally
    FreeAndNil(MemStream);
  end;
  Result := True;
end;

function GxOtaFileToWideString(const FileName: WideString; Data: WideString; out WasBinary: Boolean): Boolean;
var
  FileReader: TEditReader;
  MemStream: TMemoryStream;
  FormStream: TMemoryStream;
  BaseFileName: WideString;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  RootComponent: IOTAComponent;
  SourceEditor: IOTASourceEditor;
  Component: TComponent;
  EditStream: TGxEditorReadStream;
  FormLines: TStringList;
begin
  WasBinary := False;
  Result := False;
  Exit; // NOT IMPLEMENTED YET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  MemStream := TMemoryStream.Create;
  try
    if IsForm(FileName) then
    begin
      if IsStandAlone then
      begin
        //LoadFormFileToWideString(FileName, Data, WasBinary);
        FormLines := TStringList.Create;
        try
          LoadFormFileToStrings(FileName, FormLines, WasBinary);
          Data := FormLines.Text;
        finally
          FreeAndNil(FormLines);
        end;
      end
      else
      begin
        BaseFileName := GxOtaGetBaseModuleFileName(FileName);
        // Is the form currently open as text
        if GxOtaIsFileOpen(FileName) then
        begin
          Module := GxOtaGetModule(FileName);
          if not Assigned(Module) then
            Exit;
          SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
          if not Assigned(SourceEditor) then
            Exit;
          EditStream := TGxEditorReadStream.Create(SourceEditor);
          try
            MemStream.LoadFromStream(EditStream);
            MemStream.Position := 0;
          finally
            FreeAndNil(EditStream);
          end;
          AnsiStreamToWideString(MemStream, Data);
        end
        else if GxOtaIsFileOpen(BaseFileName) then
        begin
          Module := GxOtaGetModule(BaseFileName);
          if not Assigned(Module) then
            Exit;
          FormEditor := GxOtaGetFormEditorFromModule(Module);
          if not Assigned(FormEditor) then
            Exit;
          RootComponent := FormEditor.GetRootComponent;
          if RootComponent <> nil then
          begin
            Component := GxOtaGetNativeComponent(RootComponent);
            if Component <> nil then
            begin
              FormStream :=  TMemoryStream.Create;
              try
                FormStream.WriteComponent(Component);
                FormStream.Position := 0;
                ObjectBinaryToText(FormStream, MemStream);
                MemStream.Position := 0;
              finally;
                FreeAndNil(FormStream);
              end;
              AnsiStreamToWideString(MemStream, Data);
            end;
          end;
        end
        else
          //LoadFormFileToStrings(FileName, Lines, WasBinary)
      end;
    end
    else
    begin
      if IsStandAlone then
        //Lines.LoadFromFile(FileName)
      else
      begin
        BaseFileName := GxOtaGetBaseModuleFileName(FileName);
        Module := GxOtaGetModule(BaseFileName);
        if Assigned(Module) then
        begin
          SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
          // If there is no source editor for the file name, we can't load it
          // We might be trying to load a .pas with the .dfm open as text
          if (not (IsBdsgroup(FileName) or IsBdsprojOrDproj(FileName))) and (not Assigned(SourceEditor)) then
            Exit;
        end;
        FileReader := TEditReader.Create(FileName);
        try
          FileReader.SaveToStream(MemStream);
          MemStream.Position := 0;
        finally
          FreeAndNil(FileReader);
        end;
        //Lines.LoadFromStream(MemStream);
      end;
    end;
  finally
    FreeAndNil(MemStream);
  end;
  Result := True;
end;

// Test whether the last attribute before the current one is part of another
// one passed in InElement - or something else.  This is only called if
// GetAttributeAtPos returns atWhiteSpace - it is not clear, then, whether
// the cursor is inside another element.  Currently, this code is used for
// atComment and atString, see below.
function GxOtaIsWhiteSpaceInElement(const InElement: Integer;
  EV: IOTAEditView; EditorPosition: TOTAEditPos): Boolean;
var
  Element: Integer;
  LineFlag: Integer;
begin
  Element := atWhiteSpace;

  // Strange enough, but this algorithm seems to be serving each situation,
  // even the case where we react the very beginning of the line (col = 0)?
  // The expected case would have been to climb the lines up and check further
  // from the end towards the beginning, but that somehow is not necessary??
  Dec(EditorPosition.Col);
  while EditorPosition.Col >= 0 do
  begin
    EV.GetAttributeAtPos(EditorPosition, False, Element, LineFlag);
    if Element <> atWhiteSpace then
      Break;
    Dec(EditorPosition.Col);
  end;

  Result := (Element = InElement);
end;

function GxOtaIsWhiteSpaceInComment(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
begin
  Result := GxOtaIsWhiteSpaceInElement(atComment, EditView, EditorPosition);
end;

function GxOtaIsWhiteSpaceInString(const EditView: IOTAEditView; const EditorPosition: TOTAEditPos): Boolean;
begin
  Result := GxOtaIsWhiteSpaceInElement(atString, EditView, EditorPosition);
end;

procedure GxOtaSetCurrentSourcePosition(Position: Integer);
var
  EditView: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
  begin
    CharPos := GxOtaGetCharPosFromPos(Position - 1, EditView);
    EditView.ConvertPos(False, EditPos, CharPos);
    EditView.CursorPos := EditPos;
    EditView.MoveViewToCursor;
    EditView.Paint;
  end;
end;

function GxOtaGetKeyboardServices: IOTAKeyboardServices;
begin
  if not Supports(BorlandIDEServices, IOTAKeyboardServices, Result) then
    raise Exception.Create('IDE keyboard services are not available');
end;

// Note: This returns the tab stops in reverse order (last to first)
function GxOtaGetTabStops: string;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := '';
  EditBuffer := GxOtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and Assigned(EditBuffer.BufferOptions) then
    Result := Trim(EditBuffer.BufferOptions.TabStops);
end;

function GxOtaGetTabWidth: Integer;
const
  DefTabSize = 4;
var
  TabStops: string;
  SpacePos: Integer;
begin
  TabStops := GxOtaGetTabStops;
  SpacePos := LastCharPos(TabStops, ' ');
  if SpacePos > 0 then
    Result := StrToIntDef(Copy(TabStops, SpacePos + 1, 3), DefTabSize) - 1
  else if TabStops <> '' then
    // D6-D2005 return the value entered + 1 with only one tab stop defined
    Result := StrToIntDef(TabStops, DefTabSize) - 1
  else
    Result := DefTabSize;
end;

procedure GxOtaExpandTabsInList(ALines: TStrings);
var
  i: Integer;
  TabSize: Integer;
begin
  TabSize := GxOtaGetTabWidth;
  for i := 0 to ALines.Count - 1 do
    ALines[i] := ExpandTabsInLine(ALines[i], TabSize);
end;

function GxOtaGetLeadingWhiteSpaceInSelection: string;
var
  SelText: string;
  Lines: TStringList;
  MinIndex, MinSpaceCount: Integer;
begin
  SelText := GxOtaGetCurrentSelection(False);

  Lines := TStringList.Create;
  try
    Lines.Text := SelText;

    GxOtaExpandTabsInList(Lines);
    FindMinIndent(Lines, MinIndex, MinSpaceCount);

    if MinIndex >= 0 then
      Result := Copy(Lines[MinIndex], 1, MinSpaceCount)
    else
      Result := '';
  finally
    Lines.Free;
  end;
end;

procedure GxOtaShowProjectModuleInformation;

  function GetEditorInterfaces(Editor: IOTAEditor): string;

    procedure AddEditor(Intf: TGuid; const Name: string);
    begin
      if Supports(Editor, Intf) then
      begin
        if Result = '' then
          Result := Name
        else
          Result := Result + ', ' + Name;
      end;
    end;

  begin
    Assert(Assigned(Editor));
    Result := '';
    AddEditor(IOTASourceEditor, 'IOTASourceEditor');
    AddEditor(IOTAFormEditor, 'IOTAFormEditor');
    {$IFDEF GX_VER160_up}
    AddEditor(IOTASourceEditor70, 'IOTASourceEditor70');
    {$ENDIF GX_VER160_up}
    {$IFDEF GX_VER170_up}
    AddEditor(IOTAEditorContent, 'IOTAEditorContent');
    {$ENDIF GX_VER170_up}
    AddEditor(IOTAProjectResource, 'IOTAProjectResource');
    AddEditor(IOTATypeLibEditor, 'IOTATypeLibEditor');
  end;

var
  Project: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  Module: IOTAModule;
  Editor: IOTAEditor;
  i: Integer;
  List: TStringList;

  procedure AddEditorDetails(AModule: IOTAModule);
  var
    j: Integer;
  begin
    for j := 0 to AModule.ModuleFileCount - 1 do
    begin
      Editor := AModule.ModuleFileEditors[j];
      Assert(Assigned(Editor));
      List.Add(Format('    Editor: %s: (Implements: %s)', [Editor.FileName, GetEditorInterfaces(Editor)]));
    end;
  end;

begin
  List := TStringList.Create;
  try
    Project := GxOtaGetCurrentProject;
    Assert(Assigned(Project), 'No project open');
    List.Add(Format('Project: %s (Personality: %s):', [Project.FileName, GxOtaGetCurrentProjectPersonality]));
    AddEditorDetails(Project);
    List.Add('');

    for i := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(i);
      Assert(Assigned(ModuleInfo));
      List.Add(Format('ModuleInfo: %s (Name: %s, FormName: %s, DesignClass: %s):', [ModuleInfo.FileName, ModuleInfo.Name, ModuleInfo.FormName, ModuleInfo.DesignClass]));
      Module := GxOtaGetModule(ModuleInfo.FileName);
      if not Assigned(Module) then
        List.Add('  <No IOTAModule>')
      else
      begin
        List.Add(Format('  Module: %s:', [Module.FileName]));
        AddEditorDetails(Module);
      end;
      List.Add('');
    end;
    MessageBox(0, PChar(List.Text), 'Project Modules and Editors', MB_ICONINFORMATION or MB_OK);
  finally
    FreeAndNil(List);
  end;
end;

procedure GxOtaShowIDEActions;
var
  Actions: TCustomActionList;
  Action: TContainedAction;
  Msg: string;
  ActionMsg: string;
  ActionCaption: string;
  ActionShortCut: TShortCut;
  i: Integer;
begin
  Actions := GxOtaGetIdeActionList;
  Assert(Assigned(Actions));
  Msg := '';
  for i := 0 to Actions.ActionCount - 1 do
  begin
    Action := Actions[i];
    ActionCaption := '';
    ActionShortCut := 0;
    if Action is TCustomAction then
    begin
      ActionCaption := (Action as TCustomAction).Caption;
      ActionShortCut := (Action as TCustomAction).ShortCut;
    end;
    ActionMsg := Format('%s: %s  Shortcut: %s (%d)  Category: %s  Class: %s', [Action.Name, ActionCaption, ShortCutToText(ActionShortCut), ActionShortCut, Action.Category, Action.ClassName]);
    if Msg = '' then
      Msg := ActionMsg
    else
      Msg := Msg + sLineBreak + ActionMsg;
  end;
  MessageBox(0, PChar(Msg), 'IDE Actions', MB_ICONINFORMATION or MB_OK)
end;

{$IFDEF GX_VER160_up}
function ConvertToIDEEditorString(const S: string): UTF8String;
begin
  Result := AnsiToUtf8(S);
end;
{$ELSE}
function ConvertToIDEEditorString(const S: string): string;
begin
  Result := S;
end;
{$ENDIF}

{$IFDEF GX_VER160_up}
function IDEEditorStringToString(const S: UTF8String): string;
begin
  Result := Utf8ToAnsi(S);
end;
{$ELSE}
function IDEEditorStringToString(const S: string): string;
begin
  Result := S;
end;
{$ENDIF}

{ TBaseIdeNotifier }

function TBaseIdeNotifier.AddNotifierToIDE: Boolean;
begin
  FNotifierIndex := GxOtaGetIDEServices.AddNotifier(Self);
  Result := (FNotifierIndex <> InvalidNotifierIndex);
end;

function TBaseIdeNotifier.RemoveNotifierFromIDE: Boolean;
var
  SavedIndex: Integer;
begin
  Result := False;
  if FNotifierIndex = InvalidNotifierIndex then
    Exit;
  SavedIndex := FNotifierIndex;
  FNotifierIndex := InvalidNotifierIndex;
  GxOtaGetIDEServices.RemoveNotifier(SavedIndex);
  Result := True;
end;

procedure TBaseIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
  // Nothing
end;

procedure TBaseIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
  // Nothing
end;

procedure TBaseIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // Nothing
end;

destructor TBaseIdeNotifier.Destroy;
begin
  if FNotifierIndex <> InvalidNotifierIndex then
    RemoveNotifierFromIDE;
  inherited Destroy;
end;

{ TBaseDebuggerNotifier }

function TBaseDebuggerNotifier.AddNotifierToIDE: Boolean;
begin
  FNotifierIndex := GxOtaGetDebuggerServices.AddNotifier(Self);
  Result := (FNotifierIndex <> InvalidNotifierIndex);
end;

function TBaseDebuggerNotifier.RemoveNotifierFromIDE: Boolean;
var
  SavedIndex: Integer;
begin
  Result := False;
  if FNotifierIndex = InvalidNotifierIndex then
    Exit;
  SavedIndex := FNotifierIndex;
  FNotifierIndex := InvalidNotifierIndex;
  GxOtaGetDebuggerServices.RemoveNotifier(SavedIndex);
  Result := True;
end;

destructor TBaseDebuggerNotifier.Destroy;
begin
  RemoveNotifierFromIDE;
  inherited;
end;

procedure TBaseDebuggerNotifier.BreakpointAdded({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.BreakpointDeleted({$IFDEF GX_VER170_up} const {$ENDIF} Breakpoint: IOTABreakpoint);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.ProcessCreated({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  // Nothing
end;

procedure TBaseDebuggerNotifier.ProcessDestroyed({$IFDEF GX_VER170_up} const {$ENDIF} Process: IOTAProcess);
begin
  // Nothing
end;

initialization

finalization
  // We need to clear all tool messages due to a bug in the IDE's
  // implementation of IOTACustomMessage:
  // Messages are apparently not cleared upon shutdown of the
  // IDE - and if we write our own IOTACustomMessage, the IDE
  // will try to access these. But since the DLL is gone -> AV.
  //GxOtaClearToolMessages;

end.
