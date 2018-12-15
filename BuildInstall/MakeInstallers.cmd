@setlocal enableextensions
@if not defined gx_cmd_debug (echo off)
@endlocal
set iscc="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
set version=1.3.12
set instver=13C
call :getdate
set special=-experimental-twm-%dateYYYYMMDD%

rem call :makeinst RS103 RS10.3
rem pause
rem goto :eof

call :makeinst Delphi6 D6
call :makeinst Delphi7 D7
call :makeinst Delphi2005
call :makeinst BDS2006
call :makeinst Delphi2007
call :makeinst RS2009
call :makeinst RS2010
call :makeinst RSXE1
call :makeinst RSXE2
call :makeinst RSXE3
call :makeinst RSXE4
call :makeinst RSXE5
call :makeinst RSXE6
call :makeinst RSXE7
call :makeinst RSXE8
call :makeinst RS100 RS10
call :makeinst RS101 RS10.1
call :makeinst RS102 RS10.2
call :makeinst RS103 RS10.3
goto :eof

:makeinst
set Delphi=%1
set Prefix=%2
if "%prefix%" == "" set Prefix=%Delphi%
%iscc% GExperts.iss /d%Delphi% /dVersion=%version% /fGX%Prefix%-%instver%%special%
rem %iscc% GExperts.iss /dDelphi2010 /dVersion=%version% /fGXRS2010-%instver%-experimental-twm-%date%.exe
goto :eof

:getdate
rem taken from http://stackoverflow.com/a/16349176/49925
:: Check WMIC is available
WMIC.EXE Alias /? >NUL 2>&1 || GOTO s_error

:: Use WMIC to retrieve date and time
FOR /F "skip=1 tokens=1-6" %%G IN ('WMIC Path Win32_LocalTime Get Day^,Hour^,Minute^,Month^,Second^,Year /Format:table') DO (
   IF "%%~L"=="" goto s_done
      Set _yyyy=%%L
      Set _mm=00%%J
      Set _dd=00%%G
      Set _hour=00%%H
      SET _minute=00%%I
      SET _second=00%%K
)
:s_done

:: Pad digits with leading zeros
      Set _mm=%_mm:~-2%
      Set _dd=%_dd:~-2%
      Set _hour=%_hour:~-2%
      Set _minute=%_minute:~-2%
      Set _second=%_second:~-2%

Set dateYYYYMMDD=%_yyyy%-%_mm%-%_dd%
set timeHHMMSS=hour%_%_minute%_%_second%
Set logtimestamp=%dateYYYYMMDD%_%timeHHMMSS%
goto :eof

:s_error
echo WMIC is not available, cannot deterimine date and time
pause
