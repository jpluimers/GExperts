unit GX_VerDepConst;

{$I GX_CondDefine.inc}

interface

const
  // This will likely never change again in future IDE versions, so it is global
  ExptIntfExpertEntryPoint = 'INITEXPERT0017';
  // Text description of packages with which GExperts must be compiled to work
  RequiredPackageText = '  vcl, vclx, and designide';


// *****************************************************
//
//    Name of GExperts DLL
//
// *****************************************************
  {$UNDEF GExpertsDllNameDefined}
  {$IFDEF VER140}
    GExpertsDll = 'GExpertsD6.dll';
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER140}

  {$IFDEF VER150}
    GExpertsDll = 'GExpertsD7.dll';
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER150}

  {$IFDEF VER160}
    GExpertsDll = 'GExpertsD8.dll';
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER160}

  {$IFDEF VER170}
    GExpertsDll = 'GExpertsDelphi2005.dll';
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFDEF VER185}
    GExpertsDll = 'GExpertsDelphi2007.dll';
    {$ELSE}
    GExpertsDll = 'GExpertsBDS2006.dll';
    {$ENDIF}
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER180}

  {$IFDEF VER200}
    GExpertsDll = 'GExpertsRS2009.dll';
    {$DEFINE GExpertsDllNameDefined}
  {$ENDIF VER200}

  {$IFNDEF GExpertsDllNameDefined}
    GExpertsDllNameDefined not defined
  {$ENDIF GExpertsDllNameDefined}
  {$UNDEF GExpertsDllNameDefined}


// *****************************************************
//
//    Name of core IDE package for docking support
//
// *****************************************************
  {$UNDEF TTabDockHostFormClassContainerDefined}
  {$IFDEF VER140}
    TTabDockHostFormClassContainer = 'designide60.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER140}

  {$IFDEF VER150}
    TTabDockHostFormClassContainer = 'designide70.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER150}

  {$IFDEF VER160}
    TTabDockHostFormClassContainer = 'designide71.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER160}

  {$IFDEF VER170}
    TTabDockHostFormClassContainer = 'designide90.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER170}

  {$IFDEF VER180} // BDS 2006 and Delphi 2007
    TTabDockHostFormClassContainer = 'designide100.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER180}

  {$IFDEF VER200}
    TTabDockHostFormClassContainer = 'designide120.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER200}

  {$IFNDEF TTabDockHostFormClassContainerDefined}
    TTabDockHostFormClassContainer not defined
  {$ENDIF TTabDockHostFormClassContainerDefined}
  {$UNDEF TTabDockHostFormClassContainerDefined}


// *****************************************************
//
//    Name of tab dock host for multi-line tab docking support
//
// *****************************************************
TTabDockHostFormName = 'TTabDockHostForm';


// *****************************************************
//
//    Major version number of IDE, e.g.
//      Delphi 6.01 -> '6'
//
// *****************************************************
  {$UNDEF MajorVersionNumber}
  {$IFDEF VER140}
    MajorVersionNumberChar = '6';  // Delphi 6, C++Builder 6
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER140}

  {$IFDEF VER150}
    MajorVersionNumberChar = '7';  // Delphi 7, C++Builder 7
  {$ENDIF VER150}

  {$IFDEF VER160}
    MajorVersionNumberChar = '8';  // Delphi 8
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER160}

  {$IFDEF VER170}
    MajorVersionNumberChar = '9';  // Delphi 9/2005
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185}
    MajorVersionNumberChar = '10';  // BDS 2006
    {$ELSE}
    MajorVersionNumberChar = '11';  // Delphi 2007
    {$ENDIF}
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER180}

  {$IFDEF VER200}
    MajorVersionNumberChar = '12';  // RS 2009
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER200}

  {$IFNDEF MajorVersionNumber}
    MajorVersionNumberChar has not been defined
  {$ENDIF MajorVersionNumber}
  {$UNDEF MajorVersionNumber}


// *****************************************************
//
//    The "English" name of the IDE: "Delphi 2007", etc.
//
// *****************************************************
  {$UNDEF IDEEnglishNameDefined}
  {$IFDEF VER140}
    {$IFDEF BCB}
    IDEEnglishName = 'C++Builder 6';
    {$DEFINE IDEEnglishNameDefined}
    {$ELSE}
    IDEEnglishName = 'Delphi 6';
    {$DEFINE IDEEnglishNameDefined}
    {$ENDIF}
  {$ENDIF VER140}

  {$IFDEF VER150}
    IDEEnglishName = 'Delphi 7';
    {$DEFINE IDEEnglishNameDefined}
  {$ENDIF VER150}

  {$IFDEF VER160}
    IDEEnglishName = 'Delphi 8';
    {$DEFINE IDEEnglishNameDefined}
  {$ENDIF VER160}

  {$IFDEF VER170}
    IDEEnglishName = 'Delphi 2005';
    {$DEFINE IDEEnglishNameDefined}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185}
    IDEEnglishName = 'BDS 2006';
    {$DEFINE IDEEnglishNameDefined}
    {$ELSE}
    IDEEnglishName = 'Delphi 2007';
    {$DEFINE IDEEnglishNameDefined}
    {$ENDIF}
  {$ENDIF VER180}

  {$IFDEF VER200}
    IDEEnglishName = 'RAD Studio 2009';
    {$DEFINE IDEEnglishNameDefined}
  {$ENDIF VER200}

  {$IFNDEF IDEEnglishNameDefined}
    IDEEnglishName has not been defined
  {$ENDIF IDEEnglishNameDefined}
  {$UNDEF IDEEnglishNameDefined}


// *****************************************************
//
//    Base registry key for each IDE version, in case
//    the IDE isn't running but we still need it.
//    Do not localize these strings.
//
// *****************************************************
  {$IFDEF GX_VER200_up}
  CompanyRegPrefix = 'CodeGear\';
  {$ELSE}
  CompanyRegPrefix = 'Borland\';
  {$ENDIF}

  {$UNDEF IdeBaseKey}
  {$IFDEF VER140}
    {$IFNDEF BCB}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\6.0';
    {$ELSE BCB}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'C++Builder\6.0';
    {$ENDIF BCB}
    {$DEFINE IdeBaseKey}
  {$ENDIF VER140}

  {$IFDEF VER150}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\7.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER150}

  {$IFDEF VER160}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\2.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER160}

  {$IFDEF VER170}
    // Delphi 2005
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\3.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\4.0'; // BDS 2006
    {$ELSE}
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\5.0'; // Delphi 2007
    {$ENDIF}
    {$DEFINE IdeBaseKey}
  {$ENDIF VER180}

  {$IFDEF VER200}
    // Rad Studio 2009
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\6.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER200}

  {$IFNDEF IdeBaseKey}
    CompilerDefinedProductRegistryKey not defined
  {$ENDIF IdeBaseKey}
  {$UNDEF IdeBaseKey}


// *****************************************************
//
//    Storage folders for the class browser
//
// *****************************************************

  {$UNDEF ClassBrowserStorageDefined}
  {$IFDEF VER140}
    {$IFNDEF BCB}
    ClassBrowserStorageFolder = 'Storage6';
    {$ELSE BCB}
    ClassBrowserStorageFolder = 'Storage6.BCB';
    {$ENDIF BCB}
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER140}

  {$IFDEF VER150}
    ClassBrowserStorageFolder = 'Storage7';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER150}

  {$IFDEF VER160}
    ClassBrowserStorageFolder = 'Classes.D8';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER160}

  {$IFDEF VER170}
    ClassBrowserStorageFolder = 'Classes.Delphi2005';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185}
    ClassBrowserStorageFolder = 'Classes.BDS2006';
    {$ELSE}
    ClassBrowserStorageFolder = 'Classes.Delphi2007';
    {$ENDIF}
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER180}

  {$IFDEF VER200}
    ClassBrowserStorageFolder = 'Classes.RadStudio2009';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER200}

  {$IFNDEF ClassBrowserStorageDefined}
    Storage folder for class browser not defined
  {$ENDIF ClassBrowserStorageDefined}
  {$UNDEF ClassBrowserStorageDefined}


// Used to identify the running IDE
  {$IFDEF GX_BCB}
    IdeProductName = 'BCB';
  {$ELSE}
    IdeProductName = 'Delphi';
  {$ENDIF GX_BCB}

implementation

end.
