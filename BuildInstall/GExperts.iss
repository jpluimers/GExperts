; This install script requires the Inno Setup Quick Start Pack version 5.2.2 or
; greater with the included ISPP, available from: http://www.jrsoftware.org/

// TODO: Use ExecAsOriginalUser to register the DLL with the IDE for better Vista elevation support

#ifdef ISPPCC_INVOKED
; Command line compiler
  #ifndef Version
    #error Usage: "iscc.exe GExperts.iss /dDelphi7 /dVersion=1.35"
  #endif
#else
; IDE compiler
  #define RSXE2
  #define Version "1.35"
#endif

#define Product "GExperts"
#define VerRegKey "1.3"
#define RegCompany   "Borland"

#ifdef Delphi6
  #define IDEShortName "Delphi"
  #define IDELongName  "Delphi"
  #define IDEVer       "6"
  #define DLLSuffix    "D6"
  #define IDERegName   "Delphi"
  #define IDERegVer    "6"
#endif
#ifdef Delphi7
  #define IDEShortName "Delphi"
  #define IDELongName  "Delphi"
  #define IDEVer       "7"
  #define DLLSuffix    "D7"
  #define IDERegName   "Delphi"
  #define IDERegVer    "7"
#endif
#ifdef Delphi8
  #define IDEShortName "Delphi"
  #define IDELongName  "Delphi"
  #define IDEVer       "8"
  #define DLLSuffix    "D8"
  #define IDERegName   "BDS"
  #define IDERegVer    "2"
#endif
#ifdef Delphi2005
  #define IDEShortName "Delphi"
  #define IDELongName  "Delphi"
  #define IDEVer       "2005"
  #define DLLSuffix    "Delphi2005"
  #define IDERegName   "BDS"
  #define IDERegVer    "3"
#endif
#ifdef BDS2006
  #define IDEShortName "BDS"
  #define IDELongName  "BDS"
  #define IDEVer       "2006"
  #define DLLSuffix    "BDS2006"
  #define IDERegName   "BDS"
  #define IDERegVer    "4"
#endif
#ifdef Delphi2007
  #define IDEShortName "Delphi"
  #define IDELongName  "Delphi"
  #define IDEVer       "2007"
  #define DLLSuffix    "Delphi2007"
  #define IDERegName   "BDS"
  #define IDERegVer    "5"
#endif
#ifdef RS2009
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "2009"
  #define DLLSuffix    "RS2009"
  #define IDERegName   "BDS"
  #define IDERegVer    "6"
  #define RegCompany   "CodeGear"
#endif
#ifdef RS2010
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "2010"
  #define DLLSuffix    "RS2010"
  #define IDERegName   "BDS"
  #define IDERegVer    "7"
  #define RegCompany   "CodeGear"
#endif
#ifdef RSXE1
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE1"
  #define DLLSuffix    "RSXE1"
  #define IDERegName   "BDS"
  #define IDERegVer    "8"
  #define RegCompany   "Embarcadero"
#endif
#ifdef RSXE2
  #define IDEShortName "RADStudio"
  #define IDELongName  "RAD Studio"
  #define IDEVer       "XE2"
  #define DLLSuffix    "RSXE2"
  #define IDERegName   "BDS"
  #define IDERegVer    "9"
  #define RegCompany   "Embarcadero"
#endif


#ifndef IDEShortName
  #error Usage: "isppcc.exe GExperts.iss /dDelphi7 /dVersion=1.35"
#endif

#define FullName    Product +" for "+ IDELongName +" "+ IDEVer
#define BinaryDir   IDEShortName + IDEVer
#define DLLName     "GExperts" + DLLSuffix + ".dll"
#define AppIDValue  Product + IDEShortName + IDEVer
#define ThisYear    GetDateTimeString('yyyy', '', '');

[Setup]
AllowNoIcons=true
AppCopyright=Copyright 1996-{#ThisYear} by Erik Berry and the {#Product} Development Team
AppName={#Product}
AppVerName={#FullName}
AppID={#AppIDValue}
DefaultDirName={pf}\{#FullName}
DefaultGroupName={#FullName}
LicenseFile=..\Documentation\License.txt
InfoBeforeFile=..\Documentation\PreInstall.txt
AppPublisher={#Product} Development Team
AppPublisherURL=http://www.gexperts.org/
AppVersion={#Version}
AppMutex=GExperts.Addin.For.Borland.IDEs
UninstallDisplayIcon={app}\ExpertManager.exe
VersionInfoVersion={#Version}
VersionInfoDescription={#FullName} Setup
VersionInfoTextVersion={#Version}
SolidCompression=yes
;OutputBaseFilename=Setup{#DLLSuffix}

[Files]
Source: ..\Binaries\{#BinaryDir}\ExpertManager.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Binaries\{#BinaryDir}\GExpertsDebugWindow.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Binaries\{#BinaryDir}\GExpertsGrep.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Documentation\GExperts.chm; DestDir: {app}; Flags: ignoreversion
Source: ..\Source\{#DLLName}; DestDir: {app}; Flags: ignoreversion
Source: ..\ExternalSource\DbugIntf.pas; DestDir: {app}; Flags: ignoreversion
Source: ..\Documentation\Readme.txt; DestDir: {app}; Flags: isreadme ignoreversion

[Icons]
Name: {group}\Debug Window; Filename: {app}\GExpertsDebugWindow.exe
Name: {group}\Expert Manager; Filename: {app}\ExpertManager.exe
Name: {group}\GExperts Help; Filename: {app}\GExperts.chm
Name: {group}\GExperts Readme; Filename: {app}\Readme.txt
Name: {group}\Grep Search; Filename: {app}\GExpertsGrep.exe

[Registry]
Root: HKCU; Subkey: Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts; ValueType: STRING; ValueName: GExperts; ValueData: {app}\{#DLLName}; Flags: uninsdeletevalue; Check: IDEExecuted
Root: HKLM; Subkey: Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts; ValueType: STRING; ValueName: GExperts; ValueData: {app}\{#DLLName}; Flags: uninsdeletevalue uninsdeletekeyifempty
Root: HKCU; Subkey: Software\GExperts\Debug; ValueType: STRING; ValueName: FilePath; ValueData: {app}\GExpertsDebugWindow.exe

[Code]
procedure DeleteDirAndSettings(Dir: string);
begin
  if (Dir <> '') and DirExists(Dir) then
  begin
    Dir := AddBackslash(Dir);
    Deltree(Dir + '*.xml',      False, True, False);
    Deltree(Dir + '*.fs',       False, True, False);
    Deltree(Dir + '*.log',      False, True, False);
    Deltree(Dir + '*.debuglog', False, True, False);
    Deltree(Dir + '*.gex',      False, True, False);
    Deltree(Dir + 'Classes.{#DLLSuffix}', True, True, True); // Classes.D6, etc.
    // Delete this settings directory itself only if it is now empty
    Deltree(Dir,                True, False, False);
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  DataPath: string;
begin
  if CurUninstallStep = usPostUninstall then
  begin
    DataPath := '';
    // Delete the DLL reference even if it was copied from the HKLM tree
    RegDeleteValue(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\Experts', 'GExperts');
    case MsgBox('Do you want to delete all of your GExperts preferences and data files?',
                mbConfirmation, MB_YESNOCANCEL or MB_DEFBUTTON2) of
      IDYES:
        begin
          RegQueryStringValue(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\GExperts-{#VerRegKey}\Misc', 'ConfigPath', DataPath);
          DeleteDirAndSettings(DataPath);
          DeleteDirAndSettings(ExpandConstant('{app}'));
          RegDeleteKeyIncludingSubkeys(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0\GExperts-{#VerRegKey}');
        end;
      IDCANCEL:
        Abort;
    end;
  end;
end;

function IDEExecuted: Boolean;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, 'Software\{#RegCompany}\{#IDERegName}\{#IDERegVer}.0');
end;
