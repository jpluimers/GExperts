@ECHO OFF
SET SRCDIR=D:\Projects\GExperts\Source\
SET SYNEDIT=D:\Projects\GExperts\ExternalSource\UniSynEdit
SET DELPHIDIR=D:\Borland\Delphi7\d8bin
SET SWITCHES=-B -DGX_AutomatedBuild -DSYNEDIT -LUdesignide -LUvcl -LUvclx -LUrtl -LUvcljpg -$D- -$L- -$Q+ -$R+ -$T+ -I..\Source\Framework -UD:\Borland\Delphi7\d8lib -U..\ExternalSource -U..\ExternalSource\Abbrevia -U%SYNEDIT%
CD %SRCDIR%

%DELPHIDIR%\dcc32.exe %SWITCHES% GExpertsD8.dpr
