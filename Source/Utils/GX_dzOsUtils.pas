///<summary> This unit contains operating system dependent functions, at least some of them. </summary>
unit GX_dzOsUtils;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  EOsFunc = class(Exception);
  EOFNoFileinfo = class(EOsFunc);

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check
///          @param Major is a word returning the major version number
///          @param Minor is a word returning the minor version number
///          @param Revision is a word returning the revision number
///          @param Build is a word returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns words.
function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean; overload;

///<summary> Reads a file's version information and returns the four parts of the version
///          number.
///          @param Filename is a string with the name of the file to check
///          @param Major is an integer returning the major version number
///          @param Minor is an integer returning the minor version number
///          @param Revision is an integer returning the revision number
///          @param Build is an integer returning the build number
///          @returns True, if version information was found,
///                   False if the file does not contain any version information </summary>
///          @note: There is also an overloaded version that returns integers.
function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean; overload;

///<summary> @returns the filename of the current module </summary>
function GetModuleFilename: string; overload;
function GetModuleFilename(const _Module: Cardinal): string; overload;

///<summary>
/// @returns true, if the Ctrl key is currently pressed </summary>
function IsCtrlDown: Boolean;
///<summary>
/// @returns true, if the Shift key is currently pressed </summary>
function IsShiftDown: Boolean;

implementation

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

function GetModuleFilename(const _Module: Cardinal): string;
var
  Buffer: array[0..260] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFilename(_Module, Buffer, SizeOf(Buffer)))
end;

function GetModuleFilename: string;
begin
  Result := GetModuleFilename(HInstance);
end;

function GetFileBuildInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  Assert(_Filename <> '');
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwFileVersionMS shr 16;
        _Minor := dwFileVersionMS and $FFFF;
        _Revision := dwFileVersionLS shr 16;
        _Build := dwFileVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileProductInfo(_Filename: string;
  out _Major, _Minor, _Revision, _Build: Integer): Boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  Assert(_Filename <> '');
  VerInfoSize := GetFileVersionInfoSize(PChar(_Filename), Dummy);
  Result := (VerInfoSize <> 0);
  if Result then begin
    GetMem(VerInfo, VerInfoSize);
    try
      GetFileVersionInfo(PChar(_Filename), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do begin
        _Major := dwProductVersionMS shr 16;
        _Minor := dwProductVersionMS and $FFFF;
        _Revision := dwProductVersionLS shr 16;
        _Build := dwProductVersionLS and $FFFF;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function GetFileBuildInfo(const _Filename: string;
  out _Major, _Minor, _Revision, _Build: Word): Boolean;
var
  Major, Minor, Revision, Build: Integer;
begin
  Result := GetFileBuildInfo(_Filename, Major, Minor, Revision, Build);
  if Result then begin
    _Major := Major;
    _Minor := Minor;
    _Revision := Revision;
    _Build := Build;
  end;
end;

end.

