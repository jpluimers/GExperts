unit GX_KibitzComp;

{$I GX_CondDefine.inc}

{$IFDEF GX_VER150_up} // Delphi 7+
  {$DEFINE GX_KIBITZ_OTA}
{$ENDIF GX_VER150_up}

interface

uses
  Classes, ToolsAPI;

procedure GetKibitzSymbols(const SourceEditor: IOTASourceEditor;
  const EditControl: TObject; const EditView: IOTAEditView;
  const XPos, YPos: Integer;
  const SourceString, TrailingCharacters: string; Strings: TStrings);

function KibitzEnabled: Boolean;
function KibitzOta: Boolean;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows, SysUtils, GX_OtaUtils, GX_IdeUtils, GX_GenericUtils;

type
  PObject = ^TObject;
  TSymbols = packed array[0..(MaxInt div SizeOf(Integer)) - 1] of Integer;
  PSymbols = ^TSymbols;
  TUnknowns = packed array [0..(MaxInt div SizeOf(Byte)) - 1] of Byte;
  PUnknowns = ^TUnknowns;

  // Verified for D6, should work for D7 now as well
  TKibitzResult = packed record
  {$IFDEF GX_VER150_up}
    KibitzDataArray: array [0..82] of Integer;
  {$ELSE}
    KibitzDataArray: array [0..81] of Integer;
  {$ENDIF}
    KibitzDataStr: string;
    KibitzReserveArray: array[0..255] of Integer;
  end;

{$IFNDEF GX_KIBITZ_OTA}
const
  {$IFDEF VER140}
    {$IFDEF LINUX}
      {$IFDEF GX_KYLIX1}
        CorIdeLibName = 'bplcoreide.so.6';
        DphIdeLibName = 'bpldelphide.so.6';
        dccLibName = 'libdcc.so.6';
        {$DEFINE LibNamesDefined}
      {$ENDIF GX_KYLIX1}
      {$IFDEF GX_KYLIX2}
        CorIdeLibName = 'bplcoreide.so.6';    // ???
        DphIdeLibName = 'bpldelphide.so.6';   // ???
        dccLibName = 'libdcc.so.6';           // ???
        {$DEFINE LibNamesDefined}
      {$ENDIF GX_KYLIX2}
      {$IFDEF GX_KYLIX3}
        CorIdeLibName = 'bplcoreide.so.6.9';
        DphIdeLibName = 'bpldelphide.so.6.9';
        dccLibName = 'libdcc.so.6.9';
        {$DEFINE LibNamesDefined}
      {$ENDIF GX_KYLIX3}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
    CorIdeLibName = 'coreide60.bpl';
    {$IFDEF BCB}
    DphIdeLibName = 'bcbide60.bpl';
    {$ELSE not BCB}
    DphIdeLibName = 'delphide60.bpl';
    {$ENDIF BCB}
    dccLibName = 'dcc60.dll';
    {$DEFINE LibNamesDefined}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER140}

  // dphideXX.bpl
  // Codcmplt.TCodeCompletionManger.GetKibitzInfo(XPos, YPos: Integer; var KibitzResult: TKibitzResult);
  GetKibitzInfoName = '@Codcmplt@TCodeCompletionManager@GetKibitzInfo$qqriir22Comtypes@TKibitzResult';
  // Codcmplt.CodeCompletionManager: TCodeCompletionManager;
  CodeCompletionManagerName = '@Codcmplt@CodeCompletionManager';
  // dccXX.dll
  // KibitzGetValidSymbols(var KibitzResult: TKibitzResult; Symbols: PSymbols; Unknowns: PUnknowns; SymbolCount: Integer): Integer; stdcall;
  KibitzGetValidSymbolsName = 'KibitzGetValidSymbols';
  // corideXX.bpl
  // Comdebug.CompGetSymbolText(Symbol: PSymbols; var S: string; Unknown: Word); stdcall;
  CompGetSymbolTextName = '@Comdebug@CompGetSymbolText$qqsp16Comtypes@TSymbolr17System@AnsiStringus';

type
  TGetKibitzInfoProc = procedure(Self: TObject; XPos, YPos: Integer; var KibitzResult: TKibitzResult); register;

  TKibitzGetValidSymbolsProc = function(var KibitzResult: TKibitzResult; Symbols: PSymbols;
                                         Unknowns: PUnknowns; SymbolCount: Integer): Integer; stdcall;

  TCompGetSymbolTextProc = procedure(Symbol: Integer {Comtypes::TSymbol*};
                                     var S: string; Unknown: Word); stdcall;

var
  GetKibitzInfo: TGetKibitzInfoProc;
  KibitzGetValidSymbols: TKibitzGetValidSymbolsProc;
  CompGetSymbolText: TCompGetSymbolTextProc;
  CodeCompletionManager: PObject;
{$ENDIF GX_KIBITZ_OTA}

var
  HaveInitialized: Boolean = False;
  PrivateKibitzEnabled: Boolean = False;

{$IFNDEF GX_KIBITZ_OTA}
var
  CorIdeModule: HModule;
  DphIdeModule: HModule;
  dccModule: HModule;
{$ENDIF GX_KIBITZ_OTA}

function Initialize: Boolean;
begin
  if HaveInitialized then
  begin
    Result := PrivateKibitzEnabled;
    Exit;
  end;
  Result := False;
  HaveInitialized := True;

  if IsStandAlone or RunningCPPBuilder then
    Exit;

{$IFDEF GX_KIBITZ_OTA}
  if (BorlandIDEServices = nil) then
  begin
    {$IFOPT D+}SendDebugError('BorlandIDEServices unavailable');{$ENDIF}
    Exit;
  end;

  if not Supports(BorlandIDEServices, IOTACodeInsightServices) then
  begin
    {$IFOPT D+}SendDebugError('BorlandIDEServices does not support IOTACodeInsightServices');{$ENDIF}
    Exit;
  end;

{$ELSE not GX_KIBITZ_OTA}

  DphIdeModule := LoadPackage(DphIdeLibName);
  if DphIdeModule = 0 then
  begin
    {$IFOPT D+}SendDebugError('Failed to load DphIdeModule');{$ENDIF}
    Exit;
  end;

  CodeCompletionManager := GetProcAddress(DphIdeModule, CodeCompletionManagerName);
  if not Assigned(CodeCompletionManager) then
  begin
    {$IFOPT D+}SendDebugError('Failed to load CodeCompletionManager from DphIdeModule');{$ENDIF}
    Exit;
  end;

  GetKibitzInfo := GetProcAddress(DphIdeModule, GetKibitzInfoName);
  if not Assigned(GetKibitzInfo) then
  begin
    {$IFOPT D+}SendDebugError('Failed to load GetKibitzInfo from DphIdeModule');{$ENDIF}
    Exit;
  end;

  dccModule := LoadLibrary(dccLibName);
  if dccModule = 0 then
  begin
    {$IFOPT D+}SendDebugError('Failed to load dccModule');{$ENDIF}
    Exit;
  end;

  KibitzGetValidSymbols := GetProcAddress(dccModule, KibitzGetValidSymbolsName);
  if not Assigned(KibitzGetValidSymbols) then
  begin
    {$IFOPT D+}SendDebugError('Failed to load KibitzGetValidSymbols from dccModule');{$ENDIF}
    Exit;
  end;

  CorIdeModule := LoadPackage(CorIdeLibName);
  if CorIdeModule = 0 then
  begin
    {$IFOPT D+}SendDebugError('Failed to load CorIdeModule');{$ENDIF}
    Exit;
  end;

  CompGetSymbolText := GetProcAddress(CorIdeModule, CompGetSymbolTextName);
  if not Assigned(CompGetSymbolText) then
  begin
    {$IFOPT D+}SendDebugError('Failed to load CompGetSymbolText');{$ENDIF}
    Exit;
  end;

{$ENDIF GX_KIBITZ_OTA}

  // If everything succeeded set KibitzEnabled to True.
  PrivateKibitzEnabled := True;
  Result := KibitzEnabled;
end;

function KibitzEnabled: Boolean;
begin
  Initialize;
  Result := PrivateKibitzEnabled;
end;

function KibitzOta: Boolean;
begin
  {$IFDEF GX_KIBITZ_OTA}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure Finalize;
begin
{$IFNDEF GX_KIBITZ_OTA}
  if CorIdeModule <> 0 then
  begin
    UnLoadPackage(CorIdeModule);
    CorIdeModule := 0;
  end;

  if dccModule <> 0 then
  begin
    FreeLibrary(dccModule);
    dccModule := 0;
  end;

  if DphIdeModule <> 0 then
  begin
    UnLoadPackage(DphIdeModule);
    DphIdeModule := 0;
  end;
{$ENDIF GX_KIBITZ_OTA}
end;

// XPos starts a 0; YPos at 1.
procedure GetKibitzSymbols(const SourceEditor: IOTASourceEditor;
  const EditControl: TObject; const EditView: IOTAEditView;
  const XPos, YPos: Integer;
  const SourceString, TrailingCharacters: string; Strings: TStrings);

{$IFDEF GX_KIBITZ_OTA}
var
  NewCharPos: TOTACharPos;
  NewCursorPos: TOTAEditPos;
  OldCursorPos: TOTAEditPos;
  ManagerIndex: Integer;
  CodeInsightServices: IOTACodeInsightServices;
  CodeInsightManager: IOTACodeInsightManager;

  procedure AddManagerSymbolsToList(CurrentCodeInsightManager: IOTACodeInsightManager);
  var
    Allow: Boolean;
    ElementAttribute: Integer;
    LineFlag: Integer;
    FirstChar: Char;
    CodeInsightType: TOTACodeInsightType;
    InvokeType: TOTAInvokeType;
    ValidChars: TSysCharSet;
    CodeInsightSymbolList: IOTACodeInsightSymbolList;
    SymbolCount: Integer;
    SymbolIndex: Integer;
    Symbol: string;
    DisplayParams: Boolean;
  begin
    if not CurrentCodeInsightManager.Enabled then
      Exit;
    if not CurrentCodeInsightManager.HandlesFile(SourceEditor.FileName) then
      Exit;

    CodeInsightServices.SetQueryContext(EditView, CodeInsightManager);
    try
      Allow := True;
      CurrentCodeInsightManager.AllowCodeInsight(Allow, #0);
      if not Allow then
        Exit; // otherwise you will get an AV in 'InvokeCodeCompletion'

      EditView.GetAttributeAtPos(EditView.CursorPos, False, ElementAttribute, LineFlag);

      {$IFNDEF GX_VER150_up}
      if TrailingCharacters = '' then
        FirstChar := #0
      else
        FirstChar := TrailingCharacters[1];
      {$ELSE}
      // Without this, Delphi 7 and 2005 pop up a dialog stating "Unable to invoke
      // Code Completion due to errors in source code" with unknown identifiers
      FirstChar := CodeInsightKeySymbolHint;
      {$ENDIF GX_VER150_up}

      CurrentCodeInsightManager.GetCodeInsightType(FirstChar, ElementAttribute,
        CodeInsightType, InvokeType);

(*
There is a bug here that you can reproduce as follows:
1. Drop a TButton on a form (Button1)
2. Double click on Button1
3. between the begin/end pair, enter this code:  'Buttn1.' (withoout quotes)
//exp: CurrentCodeInsightManager.InvokeCodeCompletion returns 'True'
//act: CurrentCodeInsightManager.InvokeCodeCompletion returns' False'
       and you get an error message indicating that the code does not compile:
       'Unable to invoke Code Completion due to errors in source code'
4. between the begin/end pair, enter this code:  'Buttn1 '
//exp: CurrentCodeInsightManager.InvokeCodeCompletion returns 'True'
//act: CurrentCodeInsightManager.InvokeCodeCompletion returns 'True'
       and 'Buttn1 ' gets replaced by 'Button1 '

I tried some tricks around this:

- replace any InvokeType=itTimer by InvokeType=itManual
  (does not resolve)
- ignore the "False" result from "InvokeCodeCompletion"
  (does not resolve - the resulting symbol list is zero (0) symbols long
- don't call "CurrentCodeInsightManager.GetCodeInsightType"
  (error message comes in a dialog, in stead of the messages pane)
- replace "FirstChar = '.'" with "FirstChar := ' '"
  (error message comes in a dialog, instead of the messages pane)

One solution might be to (temporarily) replace the '.' in the code editor
with a ' ', and at the end restore the '.', but this adds to the undo stack.
*)

      // Not used, but the IDE calls it in this order, and the calling order might be important.
      ValidChars := CurrentCodeInsightManager.EditorTokenValidChars(False);

      try
        Symbol := SourceString;
        if not CurrentCodeInsightManager.InvokeCodeCompletion(InvokeType, Symbol) then
          Exit;
        try
          CurrentCodeInsightManager.GetSymbolList(CodeInsightSymbolList);
          if (CodeInsightSymbolList = nil) then
            Exit;

          SymbolCount := CodeInsightSymbolList.Count;
          // Expand string list to the needed capacity so that it does not
          // have to be resized while adding symbols in the loop below.
          Strings.Capacity := (Strings.Capacity - Strings.Count) + SymbolCount;
          for SymbolIndex := 0 to SymbolCount - 1 do
            Strings.Add(CodeInsightSymbolList.GetSymbolText(SymbolIndex));
        finally
          DisplayParams := False;
          CurrentCodeInsightManager.Done(False, DisplayParams);
        end;
      except
        on E: Exception do
          {$IFOPT D+}SendDebugError('Exception: ' + E.Message);{$ENDIF}
      end;
    finally
      if RunningBDS2006OrGreater {and (StrContains('pascal', CodeInsightManager.Name))} then
        CodeInsightServices.SetQueryContext(nil, CodeInsightManager)
      else
        CodeInsightServices.SetQueryContext(nil, nil);  // This crashes BDS 2006 for some reason?
    end;
  end;

begin
  Initialize;
  // Exit if kibitzing is not enabled, or the IDE is debugging.
  if (not KibitzEnabled) or GxOtaCurrentlyDebugging then
    Exit;

  NewCharPos.CharIndex := XPos;
  NewCharPos.Line := YPos;
  EditView.ConvertPos(False, NewCursorPos, NewCharPos);

  OldCursorPos := EditView.CursorPos;
  try // finally restore cursor position
    CodeInsightServices := (BorlandIDEServices as IOTACodeInsightServices);

    // Due to a bug in OTA, you sometimes cannot request the
    // GetCurrentCodeInsightManager - it just returns 'nil'
    CodeInsightServices.GetCurrentCodeInsightManager(CodeInsightManager);

    if (CodeInsightManager = nil) then
    begin
      // So we ask all the managers to provide their list of symbols
      for ManagerIndex := 0 to CodeInsightServices.CodeInsightManagerCount - 1 do
      begin
        CodeInsightManager := CodeInsightServices.CodeInsightManager[ManagerIndex];
        AddManagerSymbolsToList(CodeInsightManager);
      end;
    end
    else
      AddManagerSymbolsToList(CodeInsightManager);
  finally
    EditView.CursorPos := OldCursorPos;
  end;

end;
{$ELSE}
var
  KibitzResult: TKibitzResult;
  SymbolCount: Integer;
  Unknowns: PUnknowns;
  Symbols: PSymbols;
  i: Integer;
  S: string;
  OldEditControl: TObject;
  MagicEditControlHolder: PObject;
begin
  Initialize;
  // Exit if kibitzing is not enabled, or the IDE is debugging.
  if (not KibitzEnabled) or GxOtaCurrentlyDebugging then
    Exit;

  // Save the old edit control (I don't know if it is necessary but it won't do harm).
  MagicEditControlHolder := PObject(PChar(CodeCompletionManager^) + 4);

  OldEditControl := MagicEditControlHolder^;
  Assert((OldEditControl = nil) or (OldEditControl.ClassName = 'TEditControl'));
  MagicEditControlHolder^ := EditControl;
  try
    // Get general kibitzinfo.
    GetKibitzInfo(CodeCompletionManager^, XPos, YPos, KibitzResult);
    case Byte(KibitzResult.KibitzDataArray[0]) of
      $0B,
      $08,
      $09: Exit;
    else
      // Get valid symbol count.
      SymbolCount := KibitzGetValidSymbols(KibitzResult, nil, nil, MaxInt);
      // Allocate memory for symbols.
      GetMem(Symbols, SymbolCount * 4);
      try
        GetMem(Unknowns, SymbolCount);
        try
          // Get symbols.
          KibitzGetValidSymbols(KibitzResult, Symbols, Unknowns, SymbolCount);
          // Expand string list to the needed capacity
          // so that it does not have to be resized
          // while adding symbols in the loop below.
          Strings.Capacity := (Strings.Capacity - Strings.Count) + SymbolCount;

          Strings.BeginUpdate;
          try
            for i := 0 to SymbolCount - 1 do
            begin
              // Get the name of the symbol.
              CompGetSymbolText(Symbols^[i], S, 2);
              // Add the retrieved string to the list.
              Strings.Add(S);
            end;
          finally
            Strings.EndUpdate;
          end;
        finally
          FreeMem(Unknowns);
        end;
      finally
        FreeMem(Symbols);
      end;
    end;

  finally
    // Restore the old edit control
    MagicEditControlHolder^ := OldEditControl;
  end;
end;
{$ENDIF GX_KIBITZ_OTA}

initialization

finalization
  Finalize;

end.
