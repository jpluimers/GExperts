@rem this sets the environment variables DelphiVersion, DelphiPath and DelphiExe
setlocal
call %~dp0\doInitForDelphi.cmd %0
endlocal set DelphiVersion=%DelphiVersion% & set DelphiPath=%DelphiPath%
set DelphiExe=%DelphiPath%\bin\bds.exe
set OldPath=%PATH%
call "%DelphiPath%\bin\rsvars.bat"
rem Delphi 2007 sets the FrameworkDir incorrectly on 64 bit Windows, so we fix this here
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%OldPath%
