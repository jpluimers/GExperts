@rem Open project in Delphi IDE
@echo off
setlocal
set buildtools=%~dp0%
call %buildtools%doGetDelphiVer.cmd
call %buildtools%\doGetDelphiPath.cmd %delphiver%
if %delphiver% LSS 8 goto delphi32
set delphiexe=bds.exe
goto :bds
:delphi32
set delphiexe=delphi32.exe
:bds
for %%a in (GExperts*.dpr) do set dprname=%%a
start "Delphi %delphiver% IDE" "%DelphiPath%\bin\%delphiexe%" %dprname%
endlocal
goto :eof
