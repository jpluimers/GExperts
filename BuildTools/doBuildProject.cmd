@rem builds a project using dcc32
@echo off
setlocal
set buildtools=%~dp0%
if exist _prebuild.cmd call _prebuild.cmd
call %buildtools%doGetDelphiVer.cmd
call %buildtools%\doGetDelphiPath.cmd %delphiver%

if "%delphiver%" == "6" goto dcc32
if "%delphiver%" == "7" goto dcc32
if "%delphiver%" == "2005" goto dcc32
if "%delphiver%" == "2006" goto dcc32

@rem build is done with msbuild
if "%delphiver%" == "2007" goto is2007

@rem Delphi 2009+ works fine in 64 bit Windows
call "%DelphiPath%\bin\rsvars.bat"
@rem determine the .dproj file
for %%a in (GExperts*.dproj) do set dprname=%%a
@rem build it
msbuild /target:Build /p:config=Release %dprname%
goto :done

:is2007
@rem Delphi 2007 sets the FrameworkDir incorrectly on 64 bit Windows, so we fix this here
set OldPath=%PATH%
call "%DelphiPath%\bin\rsvars.bat"
SET FrameworkDir=%SystemRoot%\Microsoft.NET\Framework\
SET PATH=%FrameworkDir%%FrameworkVersion%;%FrameworkSDKDir%;%OldPath%
@rem determine the .dproj file
for %%a in (GExperts*.dproj) do set dprname=%%a
@rem build it
msbuild /target:Build /p:config=Release %dprname%
goto :done

:dcc32
@rem determine the .dpr file
for %%a in (GExperts*.dpr) do set dprname=%%a
@rem build is done with dcc32
"%DelphiPath%\bin\dcc32.exe" %dprname%

:done
echo done building %dprname%
endlocal
