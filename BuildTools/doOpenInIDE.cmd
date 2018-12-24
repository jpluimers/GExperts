@rem Open project in Delphi IDE
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal
set buildtools=%~dp0%
if "%1" == "" goto detectver
echo %1
set delphiver=%1
goto continue
:detectver
call "%buildtools%doGetDelphiVer.cmd"
:continue
call "%buildtools%\doGetDelphiPath.cmd" %delphiver%
if %delphiver% LSS 8 goto delphi32
set delphiexe=bds.exe
goto :bds
:delphi32
set delphiexe=delphi32.exe
:bds
for %%a in (*.dpr) do set dprname=%%a
start "Delphi %delphiver% IDE" "%DelphiPath%\bin\%delphiexe%" %dprname%
endlocal
goto :eof
