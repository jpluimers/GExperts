@echo off
set iscc="C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
set version=1.38
set date=2016-05-05
set instver=138
set special=-experimental-twm-%date%

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
pause
goto :eof

:makeinst
set Delphi=%1
set Prefix=%2
if "%prefix%" == "" set Prefix=%Delphi%
%iscc% GExperts.iss /d%Delphi% /dVersion=%version% /fGX%Prefix%-%instver%%special%
rem %iscc% GExperts.iss /dDelphi2010 /dVersion=%version% /fGXRS2010-%instver%-experimental-twm-%date%.exe
goto :eof


