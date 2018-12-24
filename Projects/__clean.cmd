@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
if "%1"=="" goto :list
call :doItem %1
goto :eof

:list
call :doItem 6
call :doItem 7
call :doItem 2005
call :doItem 2006
call :doItem 2007
call :doItem 2009
call :doItem 2010
call :doItem XE1
call :doItem XE2
call :doItem XE3
call :doItem XE4
call :doItem XE5
call :doItem XE6
call :doItem XE7
call :doItem XE8
call :doItem Xx10Seattle
call :doItem Xx101Berlin
goto :eof

:doItem
if not exist delphi%1 goto :eof
pushd delphi%1
call :doclean
popd
if not exist delphi%1\dcu\*.dcu goto :eof
del delphi%1\dcu\*.dcu
goto :eof

:doclean
call :delfile GXIcons.res
call :delfile *.~*
call :delfile *.local
call :delfile *.cfg
call :delfile *.identcache
call :delfile *.dsk
call :deltree __history
call :deltree ModelSupport
goto :eof

:delfile
if exist %1 del %1
goto :eof

:deltree
if not exist %1\* goto :eof
del /s /q %1\*
rd %1
goto :eof
