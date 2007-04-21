// Notifier for expanding macro templates after pressing space
unit GX_MacroExpandNotifier;

interface

uses
  GX_EditorChangeServices;

// Use a timer to delay expansion by a short amount, so the user can cancel
// the expansion by pressing some key other than space.
{$DEFINE EXPAND_WITH_TIMER}

const
  DefExpandChar = #32;

type
  TProcessMacroEvent = procedure (const ATemplateName, ATemplateCode: string; var Accepted: Boolean) of object;

  IMacroExpandNotifier = interface(IGxEditorNotification)
  ['{159DF04C-C199-475A-9857-DEA5A81E7517}']
    procedure Detach;
    procedure SetExpandDelay(Value: Integer);
  end;

function PrepareNewExpandMacroNotifier(AOnProcessMacro: TProcessMacroEvent): IMacroExpandNotifier;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Classes, SysUtils, ExtCtrls, Forms, ToolsAPI,
  GX_OtaUtils, GX_GenericUtils, GX_IdeUtils,
  GX_MacroTemplates;

type
  TMacroExpandNotifier = class(TNotifierObject, IGxEditorNotification, IMacroExpandNotifier)
  private
    FOnProcessMacro: TProcessMacroEvent;
    FNotifierIndex: Integer;
    FCheckTimer: TTimer;
    FExpandChar: Char;
    FExpandDelay: Integer;
    FEventLock: Integer;
    FQueuedTemplateName: string;
    procedure CheckExpand;
    procedure HandleCheckTimer(Sender: TObject);
    procedure StartCheckTimer;
    procedure StopCheckTimer;
    procedure CheckMacroCodePlacement(const SourceEditor: IOTASourceEditor);
    function DetermineReplacement(const Element: Integer;
      const SyntaxHighlighter: TGXSyntaxHighlighter): Boolean;
    procedure TryExpand(const ATemplateName, ATemplateCode: string);
    procedure LockEvents;
    procedure TryExpandLater(const ATemplateName: string);
    procedure UnlockEvents;
    procedure HandleNotification(const SourceEditor: IOTASourceEditor);
  protected
    // IGxEditorNotification
    procedure NewModuleOpened(const Module: IOTAModule);
    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor; Component: IOTAComponent;
      const OldName, NewName: string);
    function EditorKeyPressed(const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
    function GetIndex: Integer;
  protected
    procedure Attach;
  public
    constructor Create(AOnProcessMacro: TProcessMacroEvent);
    destructor Destroy; override;

    procedure Detach;
    procedure SetExpandDelay(Value: Integer);

    property ExpandChar: Char read FExpandChar;
    property ExpandDelay: Integer read FExpandDelay;
  end;

function PrepareNewExpandMacroNotifier(AOnProcessMacro: TProcessMacroEvent): IMacroExpandNotifier;
begin
  Result := TMacroExpandNotifier.Create(AOnProcessMacro) as IMacroExpandNotifier;
end;

{ TMacroExpandNotifier }

constructor TMacroExpandNotifier.Create(AOnProcessMacro: TProcessMacroEvent);
begin
  inherited Create;

  FOnProcessMacro := AOnProcessMacro;
  FNotifierIndex := -1;
  FExpandChar := DefExpandChar;
  FExpandDelay := DefExpandDelay;

  Attach;
end;

destructor TMacroExpandNotifier.Destroy;
begin
  Detach;
  inherited;
end;

procedure TMacroExpandNotifier.Attach;
begin
  Assert(FNotifierIndex = -1);
  FNotifierIndex := GxEditorChangeServices.AddNotifier(Self);
end;

procedure TMacroExpandNotifier.Detach;
begin
  {$IFDEF EXPAND_WITH_TIMER}
  StopCheckTimer;
  {$ENDIF EXPAND_WITH_TIMER}
  GxEditorChangeServices.RemoveNotifierIfNecessary(FNotifierIndex);
end;

procedure TMacroExpandNotifier.ComponentRenamed(const FormEditor: IOTAFormEditor;
  Component: IOTAComponent; const OldName, NewName: string);
begin // empty
end;

procedure TMacroExpandNotifier.FormEditorModified(const FormEditor: IOTAFormEditor);
begin // empty
end;

function TMacroExpandNotifier.GetIndex: Integer;
begin
  Result := FNotifierIndex;
end;

procedure TMacroExpandNotifier.NewModuleOpened(const Module: IOTAModule);
begin // empty
end;

function TMacroExpandNotifier.EditorKeyPressed(const SourceEditor: IOTASourceEditor;
  CharCode: Word; KeyData: Integer): Boolean;
var
  ShiftState: TShiftState;
begin
  {$IFOPT D+} SendDebug('Key pressed: '+IntToStr(CharCode)+'/'+Char(byte(CharCode))); {$ENDIF D+}

  ShiftState := KeyDataToShiftState(KeyData);

  if (ShiftState = []) and (CharCode = Ord(DefExpandChar)) then
  begin
    // Disabled until this hook is working properly (without crashing)
    //HandleNotification(SourceEditor);
  end;

  Result := False;
end;

procedure TMacroExpandNotifier.LockEvents;
begin
  Inc(FEventLock);
end;

procedure TMacroExpandNotifier.UnlockEvents;
begin
  if FEventLock > 0 then
    Dec(FEventLock);
end;

procedure TMacroExpandNotifier.SourceEditorModified(const SourceEditor: IOTASourceEditor);
begin
  {$IFOPT D+} SendDebug('SourceEditorModified'); {$ENDIF}
  HandleNotification(SourceEditor);
end;

procedure TMacroExpandNotifier.HandleNotification(const SourceEditor: IOTASourceEditor);
begin
  if FEventLock > 0  then
    Exit;

  {$IFDEF EXPAND_WITH_TIMER}
  StopCheckTimer;
  {$ENDIF EXPAND_WITH_TIMER}

  LockEvents;
  try
    if IsReplaceConfirmDialogOnScreen then
      Exit;

    // Only correct when the active form is an IDE TEditWindow.
    // This prevents some unwanted corrections when other experts,
    // the form designer, or the IDE make changes.
    if not GxOtaCurrentlyEditingSource then
      Exit;

    CheckMacroCodePlacement(SourceEditor);
  finally
    UnlockEvents;
  end;
end;

procedure TMacroExpandNotifier.CheckMacroCodePlacement(const SourceEditor: IOTASourceEditor);
var
  LineText: string;
  Element: Integer;
  LineFlag: Integer;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  SyntaxHighlighter: TGXSyntaxHighlighter;
  Index: Integer;
  TemplateName: string;
begin
  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;

  LineText := GxOtaGetPreceedingCharactersInLine(EditView);

  if Trim(LineText) = '' then
    Exit;

  if StrCharAt(LineText, Length(LineText)) = ExpandChar then
  begin
    CursorPos := EditView.CursorPos;
    EditView.GetAttributeAtPos(CursorPos, False, Element, LineFlag);

    if Element = atWhiteSpace then
    begin
      if GxOtaIsWhiteSpaceInComment(EditView, CursorPos) then
        Element := atComment
      else if GxOtaIsWhiteSpaceInString(EditView, CursorPos) then
        Element := atString;
    end;

    SyntaxHighlighter := GxOtaGetCurrentSyntaxHighlighter(SourceEditor);
    if not DetermineReplacement(Element, SyntaxHighlighter) then
      Exit;

    Index := Length(LineText) - 1;
    while (Index > 0) and (LineText[Index] in LocaleIdentifierChars) do
      Dec(Index);

    TemplateName := Copy(LineText, Index + 1, Length(LineText) - Index - 1);
    if TemplateName<>'' then
    {$IFDEF EXPAND_WITH_TIMER}
      TryExpandLater(TemplateName);
    {$ELSE !EXPAND_WITH_TIMER}
      TryExpand(TemplateName, TemplateName+ExpandChar);
    {$ENDIF !EXPAND_WITH_TIMER}
  end;
end;

function TMacroExpandNotifier.DetermineReplacement(const Element: Integer;
  const SyntaxHighlighter: TGXSyntaxHighlighter): Boolean;
const
  shsSource = [atWhiteSpace, atReservedWord, atIdentifier, atSymbol, atNumber,
    atFloat, atOctal, atHex, atCharacter, atIllegal, SyntaxOff];
begin
  Result := ((SyntaxHighlighter = gxpPAS) and (Element in shsSource))
    or ((SyntaxHighlighter = gxpCPP) and (Element in shsSource))
    or ((SyntaxHighlighter = gxpCPP) and (Element in [atPreproc]))
    or ((SyntaxHighlighter = gxpSQL) and (Element in shsSource))
    or (Element in [atAssembler]);  // No corrections are done in atString/atComment
end;

procedure TMacroExpandNotifier.StartCheckTimer;
begin
  if FCheckTimer = nil then
  begin
    FCheckTimer := TTimer.Create(nil);
    FCheckTimer.OnTimer := Self.HandleCheckTimer;
  end;

  FCheckTimer.Enabled := False;
  FCheckTimer.Interval := Self.ExpandDelay;
  FCheckTimer.Enabled := True;
end;

procedure TMacroExpandNotifier.StopCheckTimer;
begin
  if Assigned(FCheckTimer) then
  begin
    FCheckTimer.Enabled := False;
    FCheckTimer.OnTimer := nil;
    FreeAndNil(FCheckTimer);
  end;
end;

procedure TMacroExpandNotifier.HandleCheckTimer(Sender: TObject);
begin
  if Assigned(FCheckTimer) then
    FCheckTimer.Enabled := False;

  CheckExpand;
end;

procedure TMacroExpandNotifier.TryExpandLater(const ATemplateName: string);
begin
  FQueuedTemplateName := ATemplateName;
  StartCheckTimer;
end;

procedure TMacroExpandNotifier.TryExpand(const ATemplateName, ATemplateCode: string);
var
  Accepted: Boolean;
begin
  LockEvents;
  try
    if Assigned(FOnProcessMacro) then
      FOnProcessMacro(ATemplateName, ATemplateCode, Accepted);
  finally
    UnlockEvents;
  end;
end;

procedure TMacroExpandNotifier.CheckExpand;
var
  TemplateName: string;
begin
  if FQueuedTemplateName <> '' then
  begin
    TemplateName := FQueuedTemplateName;
    FQueuedTemplateName := '';
    TryExpand(TemplateName, TemplateName+ExpandChar);
  end;
end;

procedure TMacroExpandNotifier.SetExpandDelay(Value: Integer);
begin
  FExpandDelay := Value;
end;

end.
