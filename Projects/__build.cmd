@echo off
call :FindInParents %0% buildtools
"%result%\prepbuild.exe" --ReadIni=GExperts --IncBuild --updateIni=GExperts
pause

if "%1"=="" goto :list
call :dobuild %1
goto :eof
:list

call :dobuild 6
call :dobuild 7
call :dobuild 2005
call :dobuild 2006
call :dobuild 2007
call :dobuild 2009
call :dobuild 2010
call :dobuild XE1
call :dobuild XE2
call :dobuild XE3
call :dobuild XE4
call :dobuild XE5
call :dobuild XE6
call :dobuild XE7
call :dobuild XE8
call :dobuild Xx10Seattle
call :dobuild Xx101Berlin
call :dobuild Xx102
call :dobuild Xx103

goto :eof

:dobuild
pushd Delphi%1
setlocal
call __Build_Project.cmd
endlocal
popd
goto :eof

:FindInParents
@rem search all parent directories for a subdir and return
@rem the full path to that directory in %result%
setlocal
set parentdir=%1%
set subdir=%2%
:loop
call :GetDir "%parentdir%"
set parentdir=%result%
if exist "%parentdir%\%subdir%" goto found
goto loop
:found
endlocal & set result=%parentdir%\%subdir%
goto :eof

:GetDir
rem extract path
setlocal
set result=%~dp1%
rem remove any quotes
set result=%result:"=%
rem add quotes
set result="%result%"
rem remove \ before the closing quote
set result=%result:\"="%
rem remove any quotes
set result=%result:"=%
endlocal & set result=%result%
goto :eof
