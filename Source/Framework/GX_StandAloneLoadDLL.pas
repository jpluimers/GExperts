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
    function GetProcAddress(const _EntryPoint: string): pointer;
  end;

function LoadGExpertsDll(const _DllName: string): IGExpertsDll;

implementation

type
  TGExpertsDll = class(TInterfacedObject, IGExpertsDll)
  private
    FDllHandle: HMODULE;
    FDllName: string;
    function GetProcAddress(const _EntryPoint: string): pointer;
  public
    constructor Create(const _DllName: string);
    destructor Destroy; override;
  end;

function LoadGExpertsDll(const _DllName: string): IGExpertsDll;
begin
  Result := TGExpertsDll.Create(_DllName);
end;

{ TGExpertsDll }

constructor TGExpertsDll.Create(const _DllName: string);
begin
  inherited Create;
  FDllName := _DllName;
  FDllHandle := SysUtils.SafeLoadLibrary(_DllName);
  if FDllHandle = 0 then
    raise ELoadFailed.CreateFmt('Could not load library %s.', [FDllName]);
end;

destructor TGExpertsDll.Destroy;
begin
  FreeLibrary(FDllHandle);
  inherited;
end;

function TGExpertsDll.GetProcAddress(const _EntryPoint: string): pointer;
begin
  Result := Windows.GetProcAddress(FDllHandle, PAnsiChar(_EntryPoint));
  if not Assigned(Result) then
    raise EEntryPointMissing.CreateFmt('Could not find entry point "%s" in dll %s', [_EntryPoint, FDllName]);
end;

end.
