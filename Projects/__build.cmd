@echo off

@rem the build number is incremented in the _build.cmd script with is located in the main directory

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
