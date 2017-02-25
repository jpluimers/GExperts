@rem Searches the parent dirctories for the buildtools and calls the doOpenInIde.cmd there
@echo off
setlocal
call :FindInParents %0% buildtools
call %result%\doOpenInIde.cmd 2007
goto :eof

:FindInParents
@rem search all parent directories for a subdir and return
@rem the full path to that directory in %result%
setlocal
set parentdir=%1%
set subdir=%2%
:loop
call :GetDir %parentdir%
set parentdir=%result%
if exist %parentdir%\%subdir% goto found
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
