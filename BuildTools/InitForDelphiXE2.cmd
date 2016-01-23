@rem this sets the environment variables DelphiVersion, DelphiPath and DelphiExe
setlocal
call %~dp0\doInitForDelphi.cmd %0
endlocal set DelphiVersion=%DelphiVersion% & set DelphiPath=%DelphiPath%
set DelphiExe=%DelphiPath%\bin\bds.exe
call "%DelphiPath%\bin\rsvars.bat"
