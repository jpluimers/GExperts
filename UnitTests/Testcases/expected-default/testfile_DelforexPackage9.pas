package DelforexPackage9;

{$R *.res}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
{$DEBUGINFO ON}
{$EXTENDEDSYNTAX ON}
{$IMPORTEDDATA ON}
{$IOCHECKS ON}
{$LOCALSYMBOLS ON}
{$LONGSTRINGS ON}
{$OPENSTRINGS ON}
{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$REFERENCEINFO ON}
{$SAFEDIVIDE OFF}
{$STACKFRAMES OFF}
{$TYPEDADDRESS OFF}
{$VARSTRINGCHECKS ON}
{$WRITEABLECONST ON}
{$MINENUMSIZE 1}
{$IMAGEBASE $400000}
{$DESCRIPTION 'Delphi Code Formatter Expert'}
{$DESIGNONLY}
{$IMPLICITBUILD OFF}

requires
  vcl,
  designide,
  rtl;

contains
  u_DeforExpert in 'u_DeforExpert.pas',
  GX_CodeFormatterDone in 'GX_CodeFormatterDone.pas' {fmCodeFormatterDone},
  GX_CodeFormatterEditCapitalization in 'GX_CodeFormatterEditCapitalization.pas'
    {fmCodeFormatterEditCapitalization},
  GX_CodeFormatterDefaultSettings in 'GX_CodeFormatterDefaultSettings.pas',
  u_DelforRegistry in 'u_DelforRegistry.pas',
  w_DelForAboutDelFor in 'w_DelForAboutDelFor.pas' {f_AboutDelFor},
  w_DelforAbout in 'w_DelforAbout.pas' {f_About},
  GX_CodeFormatterBookmarks in 'GX_CodeFormatterBookmarks.pas',
  GX_CollectionLikeLists in '..\common\GX_CollectionLikeLists.pas',
  u_DelForTypesEx in 'u_DelForTypesEx.pas',
  GX_CodeFormatterBreakpoints in 'GX_CodeFormatterBreakpoints.pas',
  GX_CodeFormatterTypes in '..\common\GX_CodeFormatterTypes.pas',
  GX_CodeFormatterTokens in '..\engine\GX_CodeFormatterTokens.pas',
  GX_CodeFormatterFormatter in '..\engine\GX_CodeFormatterFormatter.pas',
  GX_CodeFormatterParser in '..\engine\GX_CodeFormatterParser.pas',
  GX_CodeFormatterStack in '..\engine\GX_CodeFormatterStack.pas',
  GX_CodeFormatterSettings in '..\engine\GX_CodeFormatterSettings.pas',
  GX_CodeFormatterEngine in '..\engine\GX_CodeFormatterEngine.pas',
  GX_CodeFormatterConfig in 'GX_CodeFormatterConfig.pas';

end.
