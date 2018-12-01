@rem Searches the parent directories for the buildtools and calls the doBuildProject.cmd there
@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
setlocal
call :FindInParents %0% buildtools
call "%result%\doBuildProject.cmd"
pause
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
