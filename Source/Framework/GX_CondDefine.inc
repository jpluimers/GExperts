{ GX_CondDefine.inc }

// Third-Party Product Defines
// ------------------------------------------------------------------------

// Define SYNEDIT below if you have a copy of the Open Source
// SynEdit component version 2 or greater.  This component is
// required for the Source Export expert, and can optionally be
// used in several others.
// Download from: http://synedit.sourceforge.net/
// or via CVS here: http://sourceforge.net/cvs/?group_id=3221

// Define GX_DEBUGLOG below to enable exception and SendDebug logging
// to <ModuleName>.debuglog. This requires the JCL 1.99+ available at:
// http://sourceforge.net/projects/jcl and a detailed map file
// (see the linker options in the IDE).  Use sparingly, since this
// option will slow down the IDE

{$IFNDEF GX_AutomatedBuild}
  // Turn things on we want compiled into GExperts
  {.$DEFINE SYNEDIT}
  {.$DEFINE GX_DEBUGLOG}
{$ENDIF GX_AutomatedBuild}


// Miscellaneous GExperts Defines (do not change the items below!)
// ---------------------------------------------------------------

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IFDEF BCB}
    {$DEFINE GX_BCB}
  {$ELSE}
    {$DEFINE GX_Delphi}
  {$ENDIF}
  
  {$IF CompilerVersion >= 14}
    {$DEFINE GX_VER140_up} // Delphi 6
    {$IFDEF LINUX}
      {$DEFINE GX_KYLIX}
      {$IF RTLVersion = 14.2}
        {$DEFINE GX_KYLIX2} // Kylix 2
      {$IFEND}
      {$IF RTLVersion = 14.5}
        {$DEFINE GX_KYLIX3} // Kylix 3
      {$IFEND}
    {$ENDIF}
    {$IF CompilerVersion >= 15}
      {$DEFINE GX_VER150_up}  // Delphi 7
      {$IF CompilerVersion >= 16}
        {$DEFINE GX_VER160_up}  // Delphi 8
        {$IF CompilerVersion >= 17}
          {$DEFINE GX_VER170_up}  // Delphi 9/2005
          {$IF CompilerVersion >= 18}
            {$DEFINE GX_VER180_up}  // BDS 2006
            {$IFDEF VER185}
              {$DEFINE GX_VER185_up}  // Delphi 2007
            {$ENDIF}
            {$IF CompilerVersion >= 19}
              {$DEFINE GX_VER190_up}  // Rad Studio 2008?
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ELSE not CONDITIONALEXPRESSIONS}
  Sorry, but this version of GExperts does not support the IDE
  you are using. Please visit the GExperts web site at
  http://www.gexperts.org/ to obtain more information about
  support for your IDE.
{$ENDIF}


// Turn on IDE docking support
{$DEFINE EnableIdeDockingSupport}

// All IDEs support this feature to some degree (though Delphi 8+ does not support multiline editor tabs)
{$DEFINE GX_EditorEnhancements}

// JCL stack tracing and SynEdit only work under Windows
{$IFNDEF MSWINDOWS}
  {$UNDEF GX_DEBUGLOG}
  {$UNDEF SYNEDIT}
{$ENDIF not MSWINDOWS}

// Define GX_ENHANCED_EDITOR if SynEdit is available
{$IFDEF SYNEDIT}
  {$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF SYNEDIT}

{$IFOPT B+}
  GExperts will not work well with "Complete Boolean Evaluation" on.
  Please turn it off in the Project Options dialog.
{$ENDIF}

{$IFOPT C-}
  GExperts does not work well with assertions turned off.
  Please turn them on in the Project Options dialog.
{$ENDIF}

