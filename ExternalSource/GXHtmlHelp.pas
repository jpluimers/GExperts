unit GXHtmlHelp;

interface

uses Windows;

function HtmlHelp(hWndCaller: HWND; pszFile: PChar; uCommand: UINT; dwData: DWORD): HWND;
function HtmlHelpA(hWndCaller: HWND; pszFile: PAnsiChar; uCommand: UINT; dwData: DWORD): HWND;
function HtmlHelpW(hWndCaller: HWND; pszFile: PWideChar; uCommand: UINT; dwData: DWORD): HWND;

const
  HH_DISPLAY_INDEX =  2;
  HH_HELP_CONTEXT  = $F;

implementation

uses SysUtils;

type
  THtmlHelpAProc = function (hWndCaller: HWND; pszFile: PAnsiChar; uCommand: UINT; dwData: DWORD): HWnd; stdcall;
  THtmlHelpWProc = function (hWndCaller: HWND; pszFile: PWideChar; uCommand: UINT; dwData: DWORD): HWnd; stdcall;

var
  HtmlHelpOcxModule: HModule;
  HtmlHelpAProc: THtmlHelpAProc;
  HtmlHelpWProc: THtmlHelpWPRoc;

function HtmlHelpSupported: Boolean;
begin
  if HtmlHelpOcxModule = 0 then
  begin
    HtmlHelpOcxModule := LoadLibrary('hhctrl.ocx');
    if HtmlHelpOcxModule <> 0 then
    begin
      @HtmlHelpAProc := GetProcAddress(HtmlHelpOcxModule, 'HtmlHelpA');
      @HtmlHelpWProc := GetProcAddress(HtmlHelpOcxModule, 'HtmlHelpW');
    end;
  end;
  if Assigned(HtmlHelpAProc) and Assigned(HtmlHelpWProc) then
    Result := True
  else
    Result := False;
end;

function HtmlHelp(hWndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT; dwData: DWORD): HWND;
begin
  Result := HtmlHelpA(hWndCaller, pszFile, uCommand, dwData);
  if Result = 0 then
    raise Exception.Create('Microsoft HTML Help not installed.  Please update to Internet Explorer 5 or later.');
end;

function HtmlHelpA(hWndCaller: HWND; pszFile: PAnsiChar; uCommand: UINT; dwData:DWORD): HWND;
begin
  if HtmlHelpSupported then
    Result := HtmlHelpAProc(hWndCaller, pszFile, uCommand, dwData)
  else
    Result := 0;
end;

function HtmlHelpW(hWndCaller: HWND; pszFile: PWideChar; uCommand: UINT; dwData:DWORD): HWND;
begin
  if HtmlHelpSupported then
    Result := HtmlHelpWProc(hWndCaller, pszFile, uCommand, dwData)
  else
    Result := 0;
end;

procedure UnloadHtmlHelpModule;
begin
  if HtmlHelpOcxModule <> 0 then
    FreeLibrary(HtmlHelpOcxModule);
end;

initialization
  HtmlHelpOcxModule := 0;

finalization
  UnloadHtmlHelpModule;

end.

