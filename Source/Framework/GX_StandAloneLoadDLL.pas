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
    function GetProcAddress(const _EntryPoint: AnsiString): pointer;
    function DllName: string;
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
    function GetProcAddress(const _EntryPoint: AnsiString): pointer;
    function DllName: string;
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
  if TryLoad('GExpertsRS101.dll', Result)
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
//    or TryLoad('GExpertsD8.dll', Result)
    or TryLoad('GExpertsD7.dll', Result)
    or TryLoad('GExpertsD6.dll', Result) then
    Exit; //==>
  raise Exception.Create('Failed to load any of the possible GExperts DLLs');
end;

{ TGExpertsDll }

constructor TGExpertsDll.Create(const _DllName: string);
var
  Path: string;
begin
  inherited Create;
  Path := ExtractFilePath(Application.ExeName);
  Path := IncludeTrailingPathDelimiter(Path);
  FDllName := Path + _DllName;
  FDllHandle := SysUtils.SafeLoadLibrary(_DllName);
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

function TGExpertsDll.GetProcAddress(const _EntryPoint: AnsiString): pointer;
begin
  Result := Windows.GetProcAddress(FDllHandle, PAnsiChar(_EntryPoint));
  if not Assigned(Result) then
    raise EEntryPointMissing.CreateFmt('Could not find entry point "%s" in dll %s', [_EntryPoint, FDllName]);
end;

end.
