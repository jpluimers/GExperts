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
    {$IFDEF LINUX}
      {$IFDEF GX_KYLIX1}
        GExpertsDll = 'libGExpertsKylix1.so';
        {$DEFINE GExpertsDllNameDefined}
      {$ENDIF GX_KYLIX1}
      {$IFDEF GX_KYLIX2}
        GExpertsDll = 'libGExpertsKylix2.so';
        {$DEFINE GExpertsDllNameDefined}
      {$ENDIF GX_KYLIX2}
      {$IFDEF GX_KYLIX3}
        GExpertsDll = 'libGExpertsKylix3.so';
        {$DEFINE GExpertsDllNameDefined}
      {$ENDIF GX_KYLIX3}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      GExpertsDll = 'GExpertsD6.dll';
      {$DEFINE GExpertsDllNameDefined}
    {$ENDIF LINUX}
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
    GExpertsDll = 'GExpertsRS2008.dll';
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
    {$IFDEF LINUX}
      {$IFDEF GX_KYLIX1}
        TTabDockHostFormClassContainer = 'bpldesignide.so.6';
        {$DEFINE TTabDockHostFormClassContainerDefined}
      {$ENDIF GX_KYLIX1}
      {$IFDEF GX_KYLIX2}
        TTabDockHostFormClassContainer = 'bpldesignide.so.6';   // ???
        {$DEFINE TTabDockHostFormClassContainerDefined}
      {$ENDIF GX_KYLIX2}
      {$IFDEF GX_KYLIX3}
        TTabDockHostFormClassContainer = 'bpldesignide.so.6.9';
        {$DEFINE TTabDockHostFormClassContainerDefined}
      {$ENDIF GX_KYLIX3}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      TTabDockHostFormClassContainer = 'designide60.bpl';
      {$DEFINE TTabDockHostFormClassContainerDefined}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER140}

  {$IFDEF VER150}
    {$IFDEF LINUX}
      TTabDockHostFormClassContainer = 'bpldesignide.so.7';
      {$DEFINE TTabDockHostFormClassContainerDefined}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      TTabDockHostFormClassContainer = 'designide70.bpl';
      {$DEFINE TTabDockHostFormClassContainerDefined}
    {$ENDIF MSWINDOWS}
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
    {$IFDEF LINUX}
      {$IFDEF GX_KYLIX1}
        MajorVersionNumberChar = '1';  // Kylix 1
        {$DEFINE MajorVersionNumber}
      {$ENDIF GX_KYLIX1}
      {$IFDEF GX_KYLIX2}
        MajorVersionNumberChar = '2';  // Kylix 2
        {$DEFINE MajorVersionNumber}
      {$ENDIF GX_KYLIX2}
      {$IFDEF GX_KYLIX3}
        MajorVersionNumberChar = '3';  // Kylix 3
        {$DEFINE MajorVersionNumber}
      {$ENDIF GX_KYLIX3}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      MajorVersionNumberChar = '6';  // Delphi 6, C++Builder 6
      {$DEFINE MajorVersionNumber}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER140}

  {$IFDEF VER150}
    {$IFDEF LINUX}
      MajorVersionNumberChar = '4';  // Kylix 4??
      {$DEFINE MajorVersionNumber}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      MajorVersionNumberChar = '7';  // Delphi 7, C++Builder 7
      {$DEFINE MajorVersionNumber}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER150}

  {$IFDEF VER160}
    MajorVersionNumberChar = '8';  // Delphi 8
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER160}

  {$IFDEF VER170}
    MajorVersionNumberChar = '9';  // Delphi 9
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
    MajorVersionNumberChar = '12';  // RS 2008
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
    {$IFDEF LINUX}
      {$IFDEF GX_KYLIX1}
        IDEEnglishName = 'Kylix 1';
        {$DEFINE IDEEnglishNameDefined}
      {$ENDIF GX_KYLIX1}
      {$IFDEF GX_KYLIX2}
        IDEEnglishName = 'Kylix 2';
        {$DEFINE IDEEnglishNameDefined}
      {$ENDIF GX_KYLIX2}
      {$IFDEF GX_KYLIX3}
        IDEEnglishName = 'Kylix 3';
        {$DEFINE IDEEnglishNameDefined}
      {$ENDIF GX_KYLIX3}
    {$ENDIF LINUX}
    {$IFDEF MSWINDOWS}
      {$IFDEF BCB}
      IDEEnglishName = 'C++Builder 6';
      {$DEFINE IDEEnglishNameDefined}
      {$ELSE}
      IDEEnglishName = 'Delphi 6';
      {$DEFINE IDEEnglishNameDefined}
      {$ENDIF}
    {$ENDIF MSWINDOWS}
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
    IDEEnglishName = 'RAD Studio 2008';
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
  {$UNDEF IdeBaseKey}
  {$IFDEF VER140}
    {$IFDEF LINUX}
      CompilerDefinedProductRegistryKey = '--- void ---';
      {$DEFINE IdeBaseKey}
    {$ENDIF LINUX}

    {$IFDEF MSWINDOWS}
      {$IFNDEF BCB}
      CompilerDefinedProductRegistryKey = 'Delphi\6.0';
      {$ELSE BCB}
      CompilerDefinedProductRegistryKey = 'C++Builder\6.0';
      {$ENDIF BCB}
      {$DEFINE IdeBaseKey}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER140}

  {$IFDEF VER150}
    {$IFDEF LINUX}
      CompilerDefinedProductRegistryKey = '--- void ---';
      {$DEFINE IdeBaseKey}
    {$ENDIF LINUX}

    {$IFDEF MSWINDOWS}
      {$IFNDEF BCB}
      CompilerDefinedProductRegistryKey = 'Delphi\7.0';
      {$ELSE BCB}
      CompilerDefinedProductRegistryKey = 'C++Builder\7.0';
      {$ENDIF BCB}
      {$DEFINE IdeBaseKey}
    {$ENDIF MSWINDOWS}
  {$ENDIF VER150}

  {$IFDEF VER160}
    CompilerDefinedProductRegistryKey = 'BDS\2.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER160}

  {$IFDEF VER170}
    // Delphi 9
    CompilerDefinedProductRegistryKey = 'BDS\3.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER170}

  {$IFDEF VER180}
    {$IFNDEF VER185}
    CompilerDefinedProductRegistryKey = 'BDS\4.0'; // BDS 2006
    {$ELSE}
    CompilerDefinedProductRegistryKey = 'BDS\5.0'; // Delphi 2007
    {$ENDIF}
    {$DEFINE IdeBaseKey}
  {$ENDIF VER180}

  {$IFDEF VER200}
    // BDS 2007
    CompilerDefinedProductRegistryKey = 'BDS\6.0';
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
    {$IFNDEF BCB}
    ClassBrowserStorageFolder = 'Storage7';
    {$ELSE BCB}
    ClassBrowserStorageFolder = 'Storage7.BCB';
    {$ENDIF BCB}
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
    ClassBrowserStorageFolder = 'Classes.RS2008';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER200}

  {$IFNDEF ClassBrowserStorageDefined}
    Storage folder for class browser not defined
  {$ENDIF ClassBrowserStorageDefined}
  {$UNDEF ClassBrowserStorageDefined}


// Used to identify the running IDE
{$IFDEF LINUX}
  IdeProductName = 'Kylix';
{$ELSE}
  {$IFDEF GX_BCB}
    IdeProductName = 'BCB';
  {$ELSE}
    IdeProductName = 'Delphi';
  {$ENDIF GX_BCB}
{$ENDIF LINUX}

implementation

end.
