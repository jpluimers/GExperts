@ECHO OFF
SET SRCDIR=D:\Projects\GExperts\Source\
SET SYNEDIT=D:\Borland\SynEdit\Source
SET DELPHIDIR=D:\Borland\Delphi7\d8bin
SET SWITCHES=-B -DGX_AutomatedBuild -DSYNEDIT -LUdesignide -LUvcl -LUvclx -LUrtl -$D- -$L- -$Q+ -$R+ -$T+ -I..\Source\Framework -UD:\Borland\Delphi7\d8lib -U..\ExternalSource -U..\ExternalSource\Abbrevia -U%SYNEDIT%
CD %SRCDIR%

%DELPHIDIR%\dcc32.exe %SWITCHES% GExpertsD8.dpr
