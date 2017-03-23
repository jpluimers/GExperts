@rem set the DelphiPath variable for various Delphi versions

setlocal

set DelphiVersion=%1

rem Support for Windows 7/8, 64 Bit
set ProgFiles=%ProgramFiles(x86)%
if not "%ProgFiles%"=="" goto Win64Bit
set ProgFiles=%ProgramFiles%
:Win64Bit

set DelphiPath=

rem this is equivalent to a case/switch statement
rem call :Delphi%DelphiVersion% resolves into call :Delphi6 etc.
call :Delphi%DelphiVersion%
goto DelphiEndCase
:Delphi6
  call :ReadReg Borland\Delphi\6.0
  goto :eof
:Delphi7
  call :ReadReg Borland\Delphi\7.0
  goto :eof
:Delphi2005
  call :ReadReg Borland\BDS\3.0
  goto :eof
:Delphi2006
  call :ReadReg Borland\BDS\4.0
  goto :eof
:Delphi2007
  call :ReadReg Borland\BDS\5.0
  goto :eof
:Delphi2009
  call :ReadReg CodeGear\BDS\6.0
  goto :eof
:Delphi2010
  call :ReadReg CodeGear\BDS\7.0
  goto :eof
:DelphiXE
  call :ReadReg Embarcadero\BDS\8.0
  goto :eof
:DelphiXE2
  call :ReadReg Embarcadero\BDS\9.0
  goto :eof
:DelphiXE3
  call :ReadReg Embarcadero\BDS\10.0
  goto :eof
:DelphiXE4
  call :ReadReg Embarcadero\BDS\11.0
  goto :eof
:DelphiXE5
  call :ReadReg Embarcadero\BDS\12.0
  goto :eof
:DelphiXE6
  call :ReadReg Embarcadero\BDS\14.0
  goto :eof
:DelphiXE7
  call :ReadReg Embarcadero\BDS\15.0
  goto :eof
:DelphiXE8
  call :ReadReg Embarcadero\BDS\16.0
  goto :eof
:Delphi10Seattle
  call :ReadReg Embarcadero\BDS\17.0
:Delphi101Berlin
  call :ReadReg Embarcadero\BDS\18.0
:Delphi102
  call :ReadReg Embarcadero\BDS\18.0
:DelphiEndCase

echo DelphiPath: "%DelphiPath%"
if exist "%DelphiPath%" goto allok
echo *** Error: Directory "%DelphiPath%" does not exist. ***
pause
goto :eof

:allok
endlocal & set DelphiPath=%DelphiPath%
rem echo DelphiPath: "%DelphiPath%"
goto :eof

:ReadReg
rem read the registry entry
set DelphiPath=
FOR /F "usebackq skip=2 tokens=3,*" %%A IN (`REG QUERY HKCU\Software\%1 /v RootDir 2^>nul`) DO (
  set DelphiPath=%%A %%B
)
rem remove one trailing space which might have been added because %%B was empty
rem remove any quotes
set DelphiPath=%DelphiPath:"=%
rem add quotes
set DelphiPath="%DelphiPath%"
rem remove space before the closing quote
set DelphiPath=%DelphiPath: "="%
rem remove any quotes
set DelphiPath=%DelphiPath:"=%
goto :eof
