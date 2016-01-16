// stores the breakpoints while the source code is being formatted
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterBreakpoints;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  ToolsApi;

type
  TSourceBreakpoint = class
  private
    FDoHandleExceptions: Boolean;
    FStackFramesToLog: Integer;
    FDoIgnoreExceptions: Boolean;
    FDoBreak: Boolean;
    FDisableGroup: string;
    FEvalExpression: string;
    FLogResult: Boolean;
    FEnableGroup: string;
    FLogMessage: string;
    FGroupName: string;
    FFileName: string;
    FEnabled: Boolean;
    FLineNumber: Integer;
    FExpression: string;
    FPassCount: Integer;
    FThreadCondition: string;
  public
    // IOTABreakpoint (Delphi >=2010)
    property ThreadCondition: string read FThreadCondition write FThreadCondition;
    // IOTABreakpoint120 (Delphi 2005)
    property StackFramesToLog: Integer read FStackFramesToLog write FStackFramesToLog;
    // IOTABreakpoint80
    property DoHandleExceptions: Boolean read FDoHandleExceptions write FDoHandleExceptions;
    property DoIgnoreExceptions: Boolean read FDoIgnoreExceptions write FDoIgnoreExceptions;
    // IOTABreakpoint50
    property GroupName: string read FGroupName write FGroupName;
    property DoBreak: Boolean read FDoBreak write FDoBreak;
    property LogMessage: string read FLogMessage write FLogMessage;
    property EvalExpression: string read FEvalExpression write FEvalExpression;
    property LogResult: Boolean read FLogResult write FLogResult;
    property EnableGroup: string read FEnableGroup write FEnableGroup;
    property DisableGroup: string read FDisableGroup write FDisableGroup;
    // IOTABreakpoint40
    property Enabled: Boolean read FEnabled write FEnabled;
    property Expression: string read FExpression write FExpression;
    property FileName: string read FFileName write FFileName;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property PassCount: Integer read FPassCount write FPassCount;
  end;

type
  TBreakpoints = class
  private
    FList: TList;
    function GetItems(_Idx: Integer): TSourceBreakpoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TSourceBreakpoint);
    function Count: Integer;
    procedure Clear;
    function Find(const _Filename: string; _LineNumber: Integer; out _Idx: Integer): Boolean;
    property Items[_Idx: Integer]: TSourceBreakpoint read GetItems; default;
  end;

type
  TBreakpointHandler = class
  private
    FBreakpoints: TBreakpoints;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure RestoreItems;
    procedure SaveItems;
    procedure Dump(const _Filename: string);
  end;

implementation

uses
  IniFiles, Math;

{ TBreakpoints }

constructor TBreakpoints.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TBreakpoints.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TSourceBreakpoint(FList[i]).Free;
  FreeAndNil(FList);
  inherited;
end;

function TBreakpoints.Find(const _Filename: string; _LineNumber: Integer;
  out _Idx: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    if SameText(Items[i].FileName, _Filename) and (Items[i].LineNumber = _LineNumber) then begin
      _Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TBreakpoints.GetItems(_Idx: Integer): TSourceBreakpoint;
begin
  Assert((_Idx > -1) and (_Idx < FList.Count));
  Result := FList[_Idx];
end;

procedure TBreakpoints.Add(Item: TSourceBreakpoint);
begin
  Assert(Assigned(FList));
  FList.Add(Item);
end;

function TBreakpoints.Count: Integer;
begin
  Assert(Assigned(FList));
  Result := FList.Count;
end;

procedure TBreakpoints.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TSourceBreakpoint(FList[i]).Free;
  FList.Clear;
end;

{ TBreakpointHandler }

constructor TBreakpointHandler.Create;
begin
  inherited;
  FBreakpoints := TBreakpoints.Create;
end;

destructor TBreakpointHandler.Destroy;
begin
  FreeAndNil(FBreakpoints);
  inherited;
end;

procedure TBreakpointHandler.RestoreItems;
var
  DebuggerServices: IOTADebuggerServices;
  i: Integer;
  SrcBrkPtInt: IOTABreakpoint;
  SrcBreakpoint: TSourceBreakpoint;
  Idx: Integer;
begin
  if BorlandIDEServices.QueryInterface(IOTADebuggerServices, DebuggerServices) <> S_OK then
    Exit;

  for i := 0 to DebuggerServices.SourceBkptCount - 1 do begin
    SrcBrkPtInt := DebuggerServices.SourceBkpts[i];
    if FBreakpoints.Find(SrcBrkPtInt.FileName, SrcBrkPtInt.LineNumber, Idx) then
      FBreakpoints.FList.Delete(Idx);
  end;
  for i := 0 to FBreakpoints.Count - 1 do begin
    SrcBreakpoint := FBreakpoints[i];
    try
      SrcBrkPtInt := DebuggerServices.NewSourceBreakpoint(
        SrcBreakpoint.FileName, SrcBreakpoint.LineNumber,
        DebuggerServices.CurrentProcess);
    except
      Break; // FIXME: Delphi 6/7 do not support recreation of source breakpoints at designtime (EListError: List index out of bounds (-1))
    end;
{$IFDEF GX_VER210_up}
    SrcBrkPtInt.ThreadCondition := SrcBreakpoint.ThreadCondition;
{$ENDIF}
{$IFDEF GX_VER170_up}
    SrcBrkPtInt.StackFramesToLog := SrcBreakpoint.StackFramesToLog;
{$ENDIF}
{$IFDEF GX_VER160_up}
    SrcBrkPtInt.DoHandleExceptions := SrcBreakpoint.DoHandleExceptions;
    SrcBrkPtInt.DoIgnoreExceptions := SrcBreakpoint.DoIgnoreExceptions;
{$ENDIF}
    SrcBrkPtInt.GroupName := SrcBreakpoint.GroupName;
    SrcBrkPtInt.DoBreak := SrcBreakpoint.DoBreak;
    SrcBrkPtInt.LogMessage := SrcBreakpoint.LogMessage;
    SrcBrkPtInt.EvalExpression := SrcBreakpoint.EvalExpression;
    SrcBrkPtInt.LogResult := SrcBreakpoint.LogResult;
    SrcBrkPtInt.EnableGroup := SrcBreakpoint.EnableGroup;
    SrcBrkPtInt.DisableGroup := SrcBreakpoint.DisableGroup;
    SrcBrkPtInt.Enabled := SrcBreakpoint.Enabled;
    SrcBrkPtInt.Expression := SrcBreakpoint.Expression;
    SrcBrkPtInt.FileName := SrcBreakpoint.FileName;
    SrcBrkPtInt.LineNumber := SrcBreakpoint.LineNumber;
    SrcBrkPtInt.PassCount := SrcBreakpoint.PassCount;
  end;
end;

procedure TBreakpointHandler.SaveItems;
var
  DebuggerServices: IOTADebuggerServices;
  i: Integer;
  SrcBrkPtInt: IOTABreakpoint;
  SrcBreakpoint: TSourceBreakpoint;
begin
  FBreakpoints.Clear;
  if BorlandIDEServices.QueryInterface(IOTADebuggerServices, DebuggerServices) <> S_OK then
    Exit;
  for i := 0 to DebuggerServices.SourceBkptCount - 1 do begin
    SrcBrkPtInt := DebuggerServices.SourceBkpts[i];
    SrcBreakpoint := TSourceBreakpoint.Create;

{$IFDEF GX_VER210_up}
    SrcBreakpoint.ThreadCondition := SrcBrkPtInt.ThreadCondition;
{$ENDIF}
{$IFDEF GX_VER170_up}
    SrcBreakpoint.StackFramesToLog := SrcBrkPtInt.StackFramesToLog;
{$ENDIF}
{$IFDEF GX_VER160_up}
    SrcBreakpoint.DoHandleExceptions := SrcBrkPtInt.DoHandleExceptions;
    SrcBreakpoint.DoIgnoreExceptions := SrcBrkPtInt.DoIgnoreExceptions;
{$ENDIF}
    SrcBreakpoint.GroupName := SrcBrkPtInt.GroupName;
    SrcBreakpoint.DoBreak := SrcBrkPtInt.DoBreak;
    SrcBreakpoint.LogMessage := SrcBrkPtInt.LogMessage;
    SrcBreakpoint.EvalExpression := SrcBrkPtInt.EvalExpression;
    SrcBreakpoint.LogResult := SrcBrkPtInt.LogResult;
    SrcBreakpoint.EnableGroup := SrcBrkPtInt.EnableGroup;
    SrcBreakpoint.DisableGroup := SrcBrkPtInt.DisableGroup;
    SrcBreakpoint.Enabled := SrcBrkPtInt.Enabled;
    SrcBreakpoint.Expression := SrcBrkPtInt.Expression;
    SrcBreakpoint.FileName := SrcBrkPtInt.FileName;
    SrcBreakpoint.LineNumber := SrcBrkPtInt.LineNumber;
    SrcBreakpoint.PassCount := SrcBrkPtInt.PassCount;
    FBreakpoints.Add(SrcBreakpoint);
  end;
//  Dump('d:\breakpoints.ini');
end;

procedure TBreakpointHandler.Dump(const _Filename: string);
var
  INI: TIniFile;
  i: Integer;
  SrcBreakpoint: TSourceBreakpoint;
begin
  INI := TIniFile.Create(_Filename);
  try
    for i := 0 to FBreakpoints.Count - 1 do begin
      SrcBreakpoint := FBreakPoints[i];
      INI.WriteString('Breakpoints', 'Breakpoint' + IntToStr(i),
        '''' + SrcBreakpoint.FFileName + '''' + ','
        + IntToStr(SrcBreakpoint.LineNumber) + ','
        + '''' + SrcBreakpoint.Expression + '''' + ','
        + IntToStr(SrcBreakpoint.PassCount) + ','
        + IntToStr(IfThen(SrcBreakpoint.Enabled, 1)) + ','
        + '''' + SrcBreakpoint.GroupName + '''' + ','
        + IntToStr(IfThen(SrcBreakpoint.DoBreak, 1)) + ','
        + IntToStr(IfThen(SrcBreakpoint.DoIgnoreExceptions, 1)) + ','
        + IntToStr(IfThen(SrcBreakpoint.DoHandleExceptions, 1)) + ','
        + '''' + SrcBreakpoint.LogMessage + '''' + ','
        + IntToStr(IfThen(SrcBreakpoint.LogResult, 1)) + ','
        + '''' + SrcBreakpoint.EvalExpression + '''' + ','
        + '''' + SrcBreakpoint.EnableGroup + '''' + ','
        + '''' + SrcBreakpoint.DisableGroup + '''' + ','
        + IntToStr(SrcBreakpoint.StackFramesToLog) + ','
        + '''' + SrcBreakpoint.ThreadCondition + ''''
        );
    end;
  finally
    FreeAndNil(INI);
  end;
end;

end.

