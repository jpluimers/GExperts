@echo off
buildtools\prepbuild.exe --incbuild --readini=projects\GExperts --updateini=projects\GExperts
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
call :doItem Xx102

goto :eof

:doItem
pushd projects
setlocal
call __build.cmd %1
endlocal
popd
goto :eof
