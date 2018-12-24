unit GX_StandAloneLoadDLL;

interface

uses
  Windows,
  SysUtils;

type
  EGxDllLoad = class(Exception)
  end;

  ELoadFailed = class(EGxDllLoad)
  end;

  EEntryPointMissing = class(EGxDllLoad)
  end;

type
  IGExpertsDll = interface
    function GetProcAddress(const _EntryPoint: AnsiString): Pointer;
    function DllName: string;
    function ModuleHandle: HINST;
  end;

function LoadGExpertsDll(const _DllName: string): IGExpertsDll;
function LoadAnyGExpertsDLL: IGExpertsDll;

implementation

uses
  Forms;

type
  TGExpertsDll = class(TInterfacedObject, IGExpertsDll)
  private
    FDllHandle: HMODULE;
    FDllName: string;
    function GetProcAddress(const _EntryPoint: AnsiString): Pointer;
    function DllName: string;
    function ModuleHandle: HMODULE;
  public
    constructor Create(const _DllName: string);
    destructor Destroy; override;
  end;

function LoadGExpertsDll(const _DllName: string): IGExpertsDll;
begin
  Result := TGExpertsDll.Create(_DllName);
end;

function TryLoad(const _DllName: string; out _Dll: IGExpertsDll): Boolean;
begin
  try
    _Dll := LoadGExpertsDll(_DllName);
    Result := True;
  except
    Result := False;
  end;
end;

function LoadAnyGExpertsDLL: IGExpertsDll;
begin
  if TryLoad('GExpertsRS103.dll', Result)
    or TryLoad('GExpertsRS102.dll', Result)
    or TryLoad('GExpertsRS101.dll', Result)
    or TryLoad('GExpertsRS10.dll', Result)
    or TryLoad('GExpertsRSXE8.dll', Result)
    or TryLoad('GExpertsRSXE7.dll', Result)
    or TryLoad('GExpertsRSXE6.dll', Result)
    or TryLoad('GExpertsRSXE5.dll', Result)
    or TryLoad('GExpertsRSXE4.dll', Result)
    or TryLoad('GExpertsRSXE3.dll', Result)
    or TryLoad('GExpertsRSXE2.dll', Result)
    or TryLoad('GExpertsRSXE1.dll', Result)
    or TryLoad('GExpertsRS2010.dll', Result)
    or TryLoad('GExpertsRS2009.dll', Result)
    or TryLoad('GExpertsDelphi2007.dll', Result)
    or TryLoad('GExpertsBDS2006.dll', Result)
    or TryLoad('GExpertsDelphi2005.dll', Result)
    or TryLoad('GExpertsD7.dll', Result)
    or TryLoad('GExpertsD6.dll', Result) then
    Exit; //==>
  raise Exception.Create('Failed to load any of the possible GExperts DLLs');
end;

{ TGExpertsDll }

function GxSafeLoadLibrary(const Filename: string): HMODULE;
var
  FPUControlWord: Word;
begin
  asm
    FNSTCW  FPUControlWord
  end;
  try
    Result := LoadLibrary(PChar(Filename));
  finally
    asm
      FNCLEX
      FLDCW FPUControlWord
    end;
  end;
end;

constructor TGExpertsDll.Create(const _DllName: string);
var
  Path: string;
begin
  inherited Create;
  Path := ExtractFilePath(Application.ExeName);
  Path := IncludeTrailingPathDelimiter(Path);
  FDllName := Path + _DllName;
  FDllHandle := GxSafeLoadLibrary(_DllName);
  if FDllHandle = 0 then
    raise ELoadFailed.CreateFmt('Could not load library %s.', [FDllName]);
end;

destructor TGExpertsDll.Destroy;
begin
  FreeLibrary(FDllHandle);
  inherited;
end;

function TGExpertsDll.DllName: string;
begin
  Result := FDllName;
end;

function TGExpertsDll.GetProcAddress(const _EntryPoint: AnsiString): Pointer;
begin
  Result := Windows.GetProcAddress(FDllHandle, PAnsiChar(_EntryPoint));
  if not Assigned(Result) then
    raise EEntryPointMissing.CreateFmt('Could not find entry point "%s" in dll %s', [_EntryPoint, FDllName]);
end;

function TGExpertsDll.ModuleHandle: HMODULE;
begin
  Result := FDllHandle;
end;

end.
