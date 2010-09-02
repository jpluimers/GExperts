unit GX_VerDepConst;

{$I GX_CondDefine.inc}

interface

const
  // This will likely never change again in future IDE versions, so it is global
  ExptIntfExpertEntryPoint = 'INITEXPERT0017';
  // Text description of packages with which GExperts must be compiled to work
  RequiredPackageText = '  vcl, vclx, and designide';
  TTabDockHostFormName = 'TTabDockHostForm';

  {$IFDEF GX_VER220_up} 
  CompanyRegPrefix = 'Embarcadero\';
  {$ELSE}
    {$IFDEF GX_VER200_up}
    CompanyRegPrefix = 'CodeGear\';
    {$ELSE}
    CompanyRegPrefix = 'Borland\';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF GX_BCB}
    IdeProductName = 'BCB';
  {$ELSE}
    IdeProductName = 'Delphi';
  {$ENDIF GX_BCB}


  // IDE Major Versions
  {$IFDEF VER140} // Delphi/BCB 6
    GExpertsDll = 'GExpertsD6.dll';
    TTabDockHostFormClassContainer = 'designide60.bpl';
    MajorVersionNumberChar = '6';
    {$IFDEF BCB}
    IDEEnglishName = 'C++Builder 6';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'C++Builder\6.0';
    ClassBrowserStorageFolder = 'Storage6.BCB';
    {$ELSE}
    IDEEnglishName = 'Delphi 6';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\6.0';
    ClassBrowserStorageFolder = 'Storage6';
    {$ENDIF}
  {$ENDIF VER140}

  {$IFDEF VER150} // Delphi/BCB 7
    GExpertsDll = 'GExpertsD7.dll';
    TTabDockHostFormClassContainer = 'designide70.bpl';
    MajorVersionNumberChar = '7';
    IDEEnglishName = 'Delphi 7';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'Delphi\7.0';
    ClassBrowserStorageFolder = 'Storage7';
  {$ENDIF VER150}

  {$IFDEF VER160} // Delphi.NET 8
    GExpertsDll = 'GExpertsD8.dll';
    TTabDockHostFormClassContainer = 'designide71.bpl';
    MajorVersionNumberChar = '8';
    IDEEnglishName = 'Delphi 8';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\2.0';
    ClassBrowserStorageFolder = 'Classes.D8';
  {$ENDIF VER160}

  {$IFDEF VER170} // Delphi 2005
    GExpertsDll = 'GExpertsDelphi2005.dll';
    TTabDockHostFormClassContainer = 'designide90.bpl';
    MajorVersionNumberChar = '9';
    IDEEnglishName = 'Delphi 2005';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\3.0';
    ClassBrowserStorageFolder = 'Classes.Delphi2005';
  {$ENDIF VER170}

  {$IFDEF VER180}
    TTabDockHostFormClassContainer = 'designide100.bpl';
    {$IFNDEF VER185} // BDS 2006
    GExpertsDll = 'GExpertsBDS2006.dll';
    MajorVersionNumberChar = '10';
    IDEEnglishName = 'BDS 2006';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\4.0';
    ClassBrowserStorageFolder = 'Classes.BDS2006';
    {$ELSE}         // Delphi/RAD Studio 2007
    MajorVersionNumberChar = '11';
    GExpertsDll = 'GExpertsDelphi2007.dll';
    IDEEnglishName = 'Delphi 2007';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\5.0';
    ClassBrowserStorageFolder = 'Classes.Delphi2007';
    {$ENDIF}
  {$ENDIF VER180}

  {$IFDEF VER200} // Delphi/RAD Studio 2009
    GExpertsDll = 'GExpertsRS2009.dll';
    TTabDockHostFormClassContainer = 'designide120.bpl';
    MajorVersionNumberChar = '12';
    IDEEnglishName = 'RAD Studio 2009';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\6.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio2009';
  {$ENDIF VER200}

  {$IFDEF VER210} // Delphi/RAD Studio 2010
    GExpertsDll = 'GExpertsRS2010.dll';
    TTabDockHostFormClassContainer = 'designide140.bpl';
    MajorVersionNumberChar = '14';
    IDEEnglishName = 'RAD Studio 2010';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\7.0';
    ClassBrowserStorageFolder = 'Classes.RADStudio2010';
  {$ENDIF VER210}
  
  {$IFDEF VER220} // Delphi/RAD Studio XE 1
    GExpertsDll = 'GExpertsRSXE1.dll';
    TTabDockHostFormClassContainer = 'designide150.bpl';
    MajorVersionNumberChar = '15';
    IDEEnglishName = 'RAD Studio XE';
    CompilerDefinedProductRegistryKey = CompanyRegPrefix + 'BDS\8.0';
    ClassBrowserStorageFolder = 'Classes.RADStudioXE1';
  {$ENDIF VER220}
  
implementation

end.
