unit GX_eChangeCase;

interface

implementation

uses
  SysUtils, Classes, Windows, Menus, Forms, Controls, ToolsAPI,
  GX_EditorExpert, GX_GenericUtils, GX_OtaUtils,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  GX_dzVclUtils;

type
  TChangeCase = (ccLowerCase, ccUpperCase, ccTitleCase, ccSentenceCase, ccToggleCase, ccNone);

type
  TChangeCaseExpert = class(TEditorExpert)
  private
    FLastSelection: Integer;
    FPopup: TPopupMenu;
    FCodeList: TStringList;
    FKBHook: HHOOK;
    function IsEditorActive(out ctl: TWinControl): Boolean;
    procedure miChangeToLowerCaseClicked(Sender: TObject);
    procedure miChangeToUpperCaseClicked(Sender: TObject);
    procedure miChangeToTitleCaseClicked(Sender: TObject);
    procedure miChangeToSentenceCaseClicked(Sender: TObject);
    procedure miToggleCaseClicked(Sender: TObject);
    procedure ReplaceSelection;
    function BlockSelectionToLineEndType(SourceEditor: IOTASourceEditor;
      const Selection: string): Integer;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    function GetDefaultShortCut: TShortCut; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

var
  EnterWasPressed: Boolean = False;

// This keyboard hook is necessary because I found no other way to detect if the
// user closed the popup menu by pressing the enter key or by some other means.
// When pressing enter, the default menu item is executed, otherwise nothing.
// So this hook checks whether the enter key is pressed and sets a global flag,
// which is then checked in TChangeCaseExpert.Execute.
function KeyboardHookProc(Code: Integer; WordParam: Word; LongParam: LongInt) : LongInt;  stdcall;
begin
  if code >= 0 then begin
    if WordParam = VK_RETURN then
      EnterWasPressed := True;
  end;
  Result := CallNextHookEx(0, Code, WordParam, LongParam);
end;

{ TChangeCaseExpert }

constructor TChangeCaseExpert.Create;
resourcestring
  SChangeToLowerCase = '&lowercase';
  SChangeToUpperCase = '&UPPER CASE';
  SChangeToTitleCase = '&Title Case';
  SChangeToSentenceCase = '&Sentence case';
  SToggleCase = 't&OGGLE cASE';
var
  mi: TMenuItem;
begin
  inherited;
  FLastSelection := 0;
  FPopup := TPopupMenu.Create(nil);
  mi := TPopupMenu_AppendMenuItem(FPopup, SChangeToLowerCase, miChangeToLowerCaseClicked);
  mi.Tag := Ord(ccLowerCase);
  mi := TPopupMenu_AppendMenuItem(FPopup, SChangeToUpperCase, miChangeToUpperCaseClicked);
  mi.Tag := Ord(ccUpperCase);
  mi := TPopupMenu_AppendMenuItem(FPopup, SChangeToTitleCase, miChangeToTitleCaseClicked);
  mi.Tag := Ord(ccTitleCase);
  mi := TPopupMenu_AppendMenuItem(FPopup, SChangeToSentenceCase, miChangeToSentenceCaseClicked);
  mi.Tag := Ord(ccSentenceCase);
  mi := TPopupMenu_AppendMenuItem(FPopup, SToggleCase, miToggleCaseClicked);
  mi.Tag := Ord(ccToggleCase);

  FCodeList := TStringList.Create;

  FKBHook := SetWindowsHookEx(WH_KEYBOARD, TFNHookProc(@KeyboardHookProc), HInstance, GetCurrentThreadId());
end;

destructor TChangeCaseExpert.Destroy;
begin
  UnHookWindowsHookEx(FKBHook);
  FreeAndNil(FCodeList);
  FreeAndNil(FPopup);
  inherited;
end;

function TChangeCaseExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scShift + scAlt + Ord('C');
end;

function TChangeCaseExpert.GetDisplayName: string;
resourcestring
  SChangeCaseName = 'Change Case';
begin
  Result := SChangeCaseName;
end;

function TChangeCaseExpert.GetHelpString: string;
resourcestring
  SChangeCaseHelp = '  This expert changes the character case for the selected block of ' +
    'text in the editor.  To use it, select any number of lines in the IDE code editor and ' +
    'activate this expert.';
begin
  Result := SChangeCaseHelp;
end;

class function TChangeCaseExpert.GetName: string;
begin
  Result := 'ChangeCase';
end;

// WARNING: This is a hack.
// GxOtaReplaceSelection has a problem with appending an
// unwanted CRLF for multiline selections to the line end
// The clean solution is just to call
//    GxOtaReplaceSelection(Editor, 0, CodeList.Text);
// and have the problem solved elsewhere, but this is hard...
// We don't remove the CRLF if the block ends in the first column,
// because then we delete a necessary CRLF.
// todo: This is copied from TSelectionEditorExpert, maybe this
// should go to TEditorExpert or to some intermediate class.
function TChangeCaseExpert.BlockSelectionToLineEndType(SourceEditor: IOTASourceEditor;
  const Selection: string): Integer;
var
  View: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  Result := 0;
  View := GxOtaGetTopMostEditView(SourceEditor);
  Assert(Assigned(View));
  CharPos.Line := View.Block.EndingRow;
  CharPos.CharIndex := View.Block.EndingColumn;
  View.ConvertPos(False, EditPos, CharPos);
  // TODO 3 -cIssue -oAnyone: Why is a 2 necessary for this column check (tested in Delphi 6)
  if (Length(Selection) > 1) and (EditPos.Col > 2) then
  begin
    if Copy(Selection, Length(Selection) - 1, 2) = #13#10 then
      Result := 2
    else if Selection[Length(Selection)] = #10 then
      Result := 1;
  end;
end;

procedure TChangeCaseExpert.ReplaceSelection;
var
  SourceEditor: IOTASourceEditor;
  LineEndType: Integer;
begin
  EnterWasPressed := False;
  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;
  if FCodeList.Count = 1 then begin
    {$IFOPT D+}SendDebugFmt('Replacing selected editor text with "%s"', [FCodeList.Text]);{$ENDIF}
    GxOtaReplaceSelection(SourceEditor, 0, TrimRight(FCodeList.Text));
  end else begin
    LineEndType := BlockSelectionToLineEndType(SourceEditor, FCodeList.Text);
    if LineEndType = 2 then
      GxOtaReplaceSelection(SourceEditor, 0, Copy(FCodeList.Text, 1, Length(FCodeList.Text) - 2))
    else if LineEndType = 1 then
      GxOtaReplaceSelection(SourceEditor, 0, Copy(FCodeList.Text, 1, Length(FCodeList.Text) - 1))
    else
      GxOtaReplaceSelection(SourceEditor, 0, FCodeList.Text);
  end;
end;

function TChangeCaseExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TChangeCaseExpert.IsEditorActive(out ctl: TWinControl): Boolean;
begin
  ctl := Screen.ActiveControl;
  Result := Assigned(ctl) and (ctl.Name = 'Editor') and ctl.ClassNameIs('TEditControl');
end;

procedure TChangeCaseExpert.miChangeToLowerCaseClicked(Sender: TObject);
begin
  FCodeList.Text := AnsiLowerCase(FCodeList.Text);
  ReplaceSelection;
  FLastSelection := (Sender as TMenuItem).MenuIndex;
end;

procedure TChangeCaseExpert.miChangeToSentenceCaseClicked(Sender: TObject);
begin
  FCodeList.Text := SentenceCase(FCodeList.Text);
  ReplaceSelection;
  FLastSelection := (Sender as TMenuItem).MenuIndex;
end;

procedure TChangeCaseExpert.miChangeToTitleCaseClicked(Sender: TObject);
begin
  FCodeList.Text := TitleCase(FCodeList.Text);
  ReplaceSelection;
  FLastSelection := (Sender as TMenuItem).MenuIndex;
end;

procedure TChangeCaseExpert.miChangeToUpperCaseClicked(Sender: TObject);
begin
  FCodeList.Text := AnsiUpperCase(FCodeList.Text);
  ReplaceSelection;
  FLastSelection := (Sender as TMenuItem).MenuIndex;
end;

procedure TChangeCaseExpert.miToggleCaseClicked(Sender: TObject);
begin
  FCodeList.Text := ToggleCase(FCodeList.Text);
  ReplaceSelection;
  FLastSelection := (Sender as TMenuItem).MenuIndex;
end;

procedure TChangeCaseExpert.Execute(Sender: TObject);
var
  ctl: TWinControl;
  pnt: TPoint;
  SourceEditor: IOTASourceEditor;
begin
  if not IsEditorActive(ctl) then
    Exit;
  if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
    Exit;
  FCodeList.Text := GxOtaGetCurrentSelection;
  if FCodeList.Count = 0 then begin
    GxOtaSelectCurrentIdent(SourceEditor, False);
    FCodeList.Text := GxOtaGetCurrentSelection;
  end;
  if FCodeList.Count = 0 then
    Exit;

  FPopup.Items[FLastSelection].Default := True;
  if not Windows.GetCaretPos(pnt) then
    pnt := Point(0, 0);
  pnt := ctl.ClientToScreen(pnt);
  EnterWasPressed := False;
  FPopup.Popup(pnt.X, pnt.Y);
  // This is a rather ugly hack to execute the default menu item when the user presses Enter:
  // We installed a keyboard hook that sets the global variable EnterWasPressed to true,
  // when (you guessed it:) the Enter key was pressed. Pressing Enter closes the popup
  // menu even if no item was selected. EnterWasPressed is set to false just before
  // showing the popup menu and also in ReplaceSelection which is called by all
  // menu item's OnClick handlers. So if EnterWasPressed is true, we know:
  // 1. Enter was pressed after the popup menu was shown
  // 2. No menu item was executed
  // so we execute the default item.
  // I said it was ugly, didn't I?
  // But it gets worse: In order to execute the Item's .OnClick method we have to call
  // Application.ProcessMessages before we can check for EnterWasPressed. If we don't do
  // that, selecting an entry with the arrow keys and Enter will result in duplicating
  // the selected text. I'm beginning to regret changing the selection from a dialog to
  // a popup menu.
  // If you have got any better idea, please feel free to change this code.
  // -- 2016-10-01 twm
  Application.ProcessMessages;
  if EnterWasPressed then begin
    IncCallCount;
    FPopup.Items[FLastSelection].Click;
  end;
end;

initialization
  RegisterEditorExpert(TChangeCaseExpert);
end.

