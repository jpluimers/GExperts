@echo off
set ZIPFile=GExperts-experimental-twm.zip
set ZIPEXE=buildtools\7z a -tzip

if exist %ZIPFile% del %ZIPFile%

rem * Add all files that don't change
pushd release
echo adding common files
..\%ZIPEXE% ..\%ZIPFile% *
popd

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
call :doItem XX10
call :doItem XX101

%ZIPEXE% %ZIPFile% install\Register-GExperts-*.cmd

pause

goto :eof

:doItem

setlocal

if %1==6     set GExpertsDLL=GExpertsD6
if %1==7     set GExpertsDLL=GExpertsD7
if %1==2005  set GExpertsDLL=GExpertsDelphi2005
if %1==2006  set GExpertsDLL=GExpertsBDS2006
if %1==2007  set GExpertsDLL=GExpertsDelphi2007
if %1==2009  set GExpertsDLL=GExpertsRS2009
if %1==2010  set GExpertsDLL=GExpertsRS2010
if %1==XE1   set GExpertsDLL=GExpertsRSXE1
if %1==XE2   set GExpertsDLL=GExpertsRSXE2
if %1==XE3   set GExpertsDLL=GExpertsRSXE3
if %1==XE4   set GExpertsDLL=GExpertsRSXE4
if %1==XE5   set GExpertsDLL=GExpertsRSXE5
if %1==XE6   set GExpertsDLL=GExpertsRSXE6
if %1==XE7   set GExpertsDLL=GExpertsRSXE7
if %1==XE8   set GExpertsDLL=GExpertsRSXE8
if %1==XX10 set GExpertsDLL=GExpertsRS10
if %1==XX101 set GExpertsDLL=GExpertsRS101

if "%GExpertsDLL%" == "" goto nodll

%ZIPEXE% %ZIPFile% DLLs\%GExpertsDLL%.dll

endlocal
goto :eof

:nodll
echo "%2" is not a valid Delphi version
pause
endlocal
goto :eof

goto :eof
