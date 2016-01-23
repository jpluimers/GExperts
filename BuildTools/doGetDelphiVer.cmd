@rem determine the Delphi version from the current directory name,
@rem assuming that it is of the form \Delphi%delphiver%\
@echo off
setlocal
call :GetLastDir %CD%
set thisdir=%result%
set prefix=%thisdir:~0,6%
if /I "%prefix%"=="Delphi" goto ok
echo %thisdir% does not start with "Delphi", cannot determine Delphi version.
pause
goto :eof
:ok
set delphiver=%thisdir:~6%
endlocal & set delphiver=%delphiver%
goto :eof

:GetLastDir
rem extract path
setlocal
set directory=%1%
rem extract "filename" (= last directory of path)
call :LastItem %directory%
endlocal & set result=%result%
goto :eof

:LastItem
set result=%~n1%
goto :eof
