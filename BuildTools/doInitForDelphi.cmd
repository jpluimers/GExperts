@rem this sets the environment variables DelphiVersion, DelphiPath and DelphiExe
@rem Call with the InitForDelphiXxx.cmd script name as parameter!
setlocal
set CallingCmdName=%~n1
set DelphiVersion=%CallingCmdName:~13%
echo Initializing for Delphi %DelphiVersion%
call %~dp0\doGetDelphiPath.cmd %DelphiVersion%
endlocal set DelphiVersion=%DelphiVersion% & set DelphiPath=%DelphiPath%
