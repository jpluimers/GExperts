GExperts 1.33 Beta 1
Open Source Programming Tools for Delphi and C++Builder

Source code, the FAQ, and the latest news are available at:
  http://www.gexperts.org/

Please send bug reports and comments using the feedback wizard
available in the GExperts about box.  You may send other GExperts
related queries to the project leader at:
  Erik Berry <eberry@gexperts.org> or <eb@techie.com>


INTRODUCTION
----------------------
GExperts is a set of tools built to increase the productivity of
Delphi and C++Builder programmers by adding several features to the
IDE.  GExperts is developed as Open Source software and is contributed
as freeware to the development community.  Users are encouraged to
download the source code and submit bug fixes and patches as well as
new features for inclusion in the GExperts distribution.


THE EXPERTS
----------------------
GExperts contains numerous experts.  See the online help
(GExperts.chm) for information on the usage of the experts.


KNOWN LIMITATIONS/BUGS
-----------------------------------------------
- The following features are not supported under Delphi 8/2005/2006/2007:
  UNICODE format text files, inner classes, class helpers, and generics in
  the Class Browser, and some of the old editor tab enhancements, such as
  multiline editor tabs.  Note that the IDE converts some high ANSI and MBCS
  characters to UTF-8 UNICODE when loaded in the code editor, so you may
  experience problems searching for or viewing those characters.  Those IDEs
  also no longer have a tabbed component palette, so those options are no
  longer supported, as well.
- The compiler replacement option of the Code Proofreader is only partially
  working under Delphi 7-2005.  It does not correct when an identifier is
  terminated by pressing a symbol key such as a period or open parenthesis,
  but works fine when the symbol is terminated by a space (IDE limitation).
- You should turn off the "Ignore Comments" Grep Search option to perform a
  search and replace on the results (GExperts limitation).
- Due to either native Open Tools API limitations or bugs, the following
  items can not be supported under Delphi 8/2005/2006/2007/2009:
  Delphi 8 Only:
   - Rename Components (IDE bug setting the Name property)
   - Jumping to a form search match from Grep Results when the source is open
     (IDE bug)
  Delphi 2005 Only:
   - The Project Option Sets environment options (IDE bug)
   - Set Tab Order: Selecting components in the desired tab order (IDE bug)
  Delphi 8, 2005, 2006, and 2007:
   - Any interaction at all with the WinForms/.NET form designer/components
     (no WinForms support for IOTAFormEditor/IOTAComponent)
   - Replace Components for VCL.NET (No direct access to components)
   - Components to Code for VCL.NET (No direct access to components)
  Delphi 8, 2005, 2006, 2007, and 2009:
   - It is no longer possible for addins to override some built-in IDE
     shortcuts.  You may need to configure your GExperts shortcuts (Prev/Next
     Identifier, Procedure List, etc.) to not conflict with your selected
     keymapping.  Also, the Macro Library can not always intercept the
     Shift+Ctrl+R keystroke to automatically grab keyboard macros, so you may
     need to create macros using the recording functions in the toolbar.


INSTALLATION
----------------------
  GExperts is distributed as a self-installing executable that should
automatically install itself into the IDE.  If you are installing under
Vista as a non-administrator, you may need to manually register the GExperts
DLL with the IDE after installation.  This is most easily done using the
Expert Manager tool in the GExperts start menu group.  If you prefer, you
can also manually register the DLL with the IDE using the Windows registry
editor (RegEdit.exe).  Create the a key key similar to the following (the
version number appearing before "\Experts\" is IDE dependent):
HKEY_CURRENT_USER\Software\CodeGear\BDS\6.0\Experts\   (Rad Studio 2009)
HKEY_CURRENT_USER\Software\Borland\BDS\5.0\Experts\    (Rad Studio 2007)
HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Experts\ (Delphi 7)

Then add a new string value that points to your GExperts DLL (X is the IDE
version such as 6, 7, 8, 2005, 2006, 2007, 2009, etc.):
GExperts=C:\Program Files\GExperts\GExpertsRSX.dll
GExperts=C:\Program Files\GExperts\GExpertsDelphiX.dll
GExperts=C:\Program Files\GExperts\GExpertsBDSX.dll or
GExperts=C:\Program Files\GExperts\GExpertsDX.dll or

  Before installing GExperts, it is strongly recommended that you install
the latest updates and patches for your IDE available from:
  http://www.codegear.com/


UNINSTALLATION
----------------------
Uninstallation of GExperts can be done from the Control Panel's
Add/Remove Programs or Programs [and Features] tool under Vista.


CHANGE LOG
----------------------
VERSION 1.33 Beta (September 25, 2008)
- General: Added beta support for RAD Studio 2009.  Some of the tools such
  as the grep search and replace, source code parser (class browser,
  procedure list, etc.) have only very limited unicode support.  The Source
  Export, Code Librarian, and basic editor experts should have full unicode support.
  Shutdown optimizations, removal of all known memory leaks, and other minor tweaks.
- Grep Search: There is a new regular expression engine that supports a large
  subset of the Perl regular expression syntax, etc.  Grep can now search
  files in ANSI, UTF-8, and UTF-16 formats in Delphi 2009.  Note: Grep replace
  does not fully support unciode yet.
- Code Proofreader: Should be slightly more accurate with fewer false positive
  corrections made to your code.
- Editor Experts: The main menu contains a submenu item for each editor
  expert now in Delphi 8 or later.
- Favorite Files: Supports preview of jpg, png, and gif files if the VCL
  supports it in your IDE version.


VERSION 1.32 (September 11, 2007)
- General: Added support for RAD Studio 2007
  GExperts data files are saved under the user's Application Data\GExperts
  directory by default for new installs, to better support Windows Vista user
  restrictions.  To change this location manually, go to GExperts,
  Configuration, File Locations.
- Grep: Allow searching of the active form's text while the editor is active
- To Do List: Allow scanning .inc files in searched directories for to do items


VERSION 1.31 (April 7, 2007)
- General: Added support for Delphi 2007
  Fixed a possible shutdown exception when unloading GExperts
  Help switched to HTML Help format (.chm) for better Vista support
  Improve the font rendering on several forms
- IDE Enhancements: Added a new option to allow resizing and enhancing some
  IDE dialogs (for BDS 2006 or later).  See the help file for full details.


VERSION 1.30 (October 26, 2006)
 - General: Added support for Delphi 2006.  Better syntax highlighting
   support for C#, HTML, and XML (Erik).  Better ClearType support in
   several forms.
 - Open File: New tool to quickly search for and open files in the project,
   search path, recent files, favorite files, etc.  The file groups are
   configurable and the tool can override the IDE's internal open
   project/form/unit dialogs. (Paul Gardner, Erik)
 - Replace Components: Comprehensive new system to map properties between
   component classes and map constants to properties for replacement
   components.  Import/export of mapping sets via XML.  Interactive and
   file-based logging of replacement results.  Smarter preservation of
   properties such as TStrings, TWideStrings, TFields, component
   references, etc. (Oasis Digital Solutions, Piotr Likus, Erik).
 - Rename Components: Allows interactively renaming components after
   they are placed on the form (Leonel Togniolli).  Added ability to edit
   a configurable list of component properties (Caption, Text, etc.) in
   addition to the Name for each component class and supply default values
   for most new component properties. (Thomas Mueller, Erik).
 - Macro Templates: Optional expansion of templates when pressing space.
   Ability to embed the selection intelligently inside a construct such as
   try/finally using the %SELECTION% macro.  New macros such as
   %BEGINPARAMLIST%, %PARAMNAME%, %PARAMTYPE%, etc. to format individual
   parameters one per line.  New default templates for XML method
   documentation, begin/end completion, for/while loops, etc.
   (Piotr Likus, Erik)
 - Macro Library: Allows record and playback of saved editor keystroke
   macros (John Hansen, Erik)
 - Find Component Reference: Allows quickly jumping from a component to the
   first usage in your source, or from a component name in the source to
   the component on the form. (Kristofer Skaug, Leonel Togniolli, Erik)
 - Set Component Properties: New tool to set properties (Boolean, string,
   set, enum, ordinal, etc.) to some value when the a project is compiled.
   Useful to deactivate datasets/connections, etc. (Robert Wachtel,
   Gami, Erik)
 - Editor Experts: New alignment tool to horizontally align variable
   declarations, comments, assignments, etc. (Stefan Pettersson, Erik)
 - Editor Enhancements: Re-enabled for Delphi 8/2005, including the editor
   toolbar, etc (Rick Hollerich, Erik).  Middle mouse button close support
   for the editor tabs in Delphi 6/7 (Gami).  Note that the various editor
   tab options can not be supported in the newer IDEs.
 - Procedure List: Option to view the selected procedure's source code
   (Paul Gardner, Erik).  Optionally search in the class name. (Primoz)
   Support for scanning Delphi .inc files and upgraded C++ support.
 - Code Proofreader: Added basic C# support (Erik)
 - Component Grid: Allow editing the Hint property for components (Erik)
 - IDE Enhancements: Changing the multiline component tabs setting in
   Delphi 6/7 no longer requires a restart (Erik)


VERSION 1.22 (January, 2005)
 - General: Work around new Delphi 2005 Update 1 bug causing various
   "Interface not supported" errors for Grep Search, Message Dialog, etc.
 - Procedure List: Fix parsing of non-compiling C++ code to avoid infinite loops
 - Rename Components: Works for both string/widestring name properties
 - Code Proofreader: Prevent Delphi 7/2005 from popping up a dialog stating
   "Unable to invoke Code Completion due to errors in source code" with unknown
   identifiers present.
 - Clipboard History: Speed up save/load of clipboard history items with a
   stored maximum of 100, fix copying/deleting of individual saved items.


VERSION 1.21 (November, 2004)
 - General: Added basic support for Delphi 2005
 - Macro Templates: Improve saving macros with trailing CRLF characters
 - Editor Experts: Improve handling of UNIX-style line endings
	 Fix parsing of empty strings and strings with embedded quotes
 - Backup Project: Fix finding files to backup on the IDE library path
 - Components to Code: Support exporting whole forms at once
 - Component Grid: Add support for variant Tag properties
 - Grep Search: Fix searches of root directories of disk drives


VERSION 1.20 (August, 2004)
 - General: Several of the tools now save their data to XML files using
   OmniXML (http://omnixml.com/), update to SynEdit 1.2+ for syntax
   highlighting, new shared toolbar/button bitmaps throughout GExperts,
   Components to Code and Copy Component Names are available from the
   form designer menu, lots of internal changes to improve code clarity
   and safety.
 - Delphi 8: Preliminary Delphi 8 support is included.  It is largely
   untested and has several limitations as documented in the "KNOWN
   LIMITATIONS/BUGS" section.  This includes basic support for some Delphi
   8 syntax elements such as operator, static, strict, dotted class names,
   nested classes, and .nfm form files.
 - Macro Templates: New editor expert that allows you to define an
   unlimited number of templates to insert into the editor, similar to
   the IDE's code templates, but with support for lots of replaceable
   macros, like %PROJECTNAME% (Piotr Likus, Erik).
 - Rename Components - Define component renaming rules that execute when
   components are added to a form.  The rules can be completely automatic,
   or allow the user to override the new name (Achim Kalwa, Erik).
 - Copy Component Names - New tool to copy the form designer's selected
   component names to the clipboard (Alexander Golovko)
 - Clipboard History: Saves the clipboard text items to an XML file.
 - Grep Search: New configuration option to make the default search string
   the identifier under the edit cursor, allow skipping comments in
   Delphi/C++ source files (Benjamin Fournier), multi-file search and
   replace inside the grep results (Jim Campbell, Erik)
 - Grep Results: New view option to hide the toolbar, show path names
   relative to the search root, new options dialog (Peter Nagel, Erik)
 - Class Browser: Allows searching by a partial class name
 - Code Librarian: Always sort folders before code snippets.  Stores the
   code snippets using a custom structured storage library written
   by Primož Gabrijelcic to remove the database dependencies (Erik).
 - Backup Project: Include h/bpr/dof/todo files when backing up a project,
   C++ support to add files using "#pragma backup FileSpec" (Benjamin
   Fournier), support quoted names and relative paths when using #backup.
   Abbrevia is now the library we use to create .zip files.
 - Macros: Many new substitution macros: %VERFILEVERSION%, %VERPRODUCTVERSION%,
   %VERMAJOR%, %VERMINOR%, %VERRELEASE%, %VERBUILD%, %SELECTION%, %CLIPBOARD%,
   %USER%, %BEFORE%, %INTERFACE%, %IDENT%, %INPUTVAR%, etc.
   See the help file for more details.
 - Project Option Sets: New tabbed GUI that is easier to use when docked
   and conversion to an XML storage format (John Hansen)
 - Editor Experts: Removed procedure/unit header experts, since the
   macro templates editor expert provides all of their functionality,
   New editor expert to reverse all assignment statements and for loops in
   a selected block of code (help from Per-Eric Larsson, Ulrich Gerhardt),
   Support for matching delimiters in C++ code and matching from ending to
   beginning delimiters (Benjamin Fournier), Added Delphi delimiter matching
   support for with, while, then, do, if, else, for, class, record, array,
   interface, implementation, uses, private, protected, public, and published
   (Erik), New editor expert to change the selection case (Thomas Due, Erik),
   New editor expert to select the current identifier.
 - Uses Clause Manager: New editor expert to add and remove items from
   uses clauses.  It allows incremental search to add a unit from the
   library path, project, etc. (Erik, Per-Eric Larsson, and Eyal Post)
 - Procedure List: C++ Nested procedure/namespace/.h support and other
   C++ parsing improvements (John Hansen, Erik)
 - Components to Code: Added C++ support (Benjamin Fournier)
 - To Do List: Added C++ support (Benjamin Fournier)
 - Code Proofreader: Basic support for compiler-assisted correction under
   Delphi 7+.  It does not work when a symbol is terminated with a '.'.
   Saves the configuration data to CodeProofreader.xml instead of a
   TClientDataSet .cds file (Ulrich Gerhardt, Erik).
 - IDE Enhancements: Under Delphi 7+, the popup menu of tab names is now
   activated by a button to the right of the palette to work around the
   lack of menu columns/scrolling in the IDE context menu (Achim Kalwa).
   The GExperts main menu items now automatically arrange themselves in a
   submenu on low resolution screens to ensure all menu items are available.
 - Replace Components: New option to not use with statements when
   generating Delphi code (Benjamin Fournier)
 - Project Dependencies: Custom dependency filtering and the ability to
   export the dependency list (John Hansen, Erik)
 - Message Dialog: Added support for MessageBox dialogs, New options to
   embed the dialog call in a case statement, quote or not quote the
   message string, and copy the code to the clipboard (Paul Gardner, Erik)
 - Set Tab Order: Added the ability to estimate the best tab order for
   children of the selected parent component by comparing the Top/Left
   properties of the children. Changed the tab order display to be a
   drag and drop tree rather than a list. (Alessandro Storniolo)
 - Favorite Files: The data is now saved to an XML file called
   FavoriteFiles.xml (help from Ulrich Gerhardt).
 - Help File: Updated for 1.2 (Mike Gallaher, Erik)


VERSION 1.12 (August, 2002)
 - General: Added support for Delphi 7 and C++Builder 6.01/6.02
 - Remove RLE compressed bitmaps to satisfy buggy video drivers
 - Procedure List: Fix infinite loop parsing some C++ source files
 - Backup Project: Remove .bpg extension from %PROJECTGROUPNAME% macro
 - Grep: Never search the same file twice when searching project groups
   Highlighting a matched search term deals better with leading tabs
   Deal better with corrupt DFMs, and accidental grep of binary files
   Search the project file when searching all files in the project
 - Editor Experts: Header experts use C++ comments under BCB by default
 - Clean Directories: Added check/uncheck all and invert to popup menus
 - Editor Toolbar: Don't allow Close and Close All toolbar buttons,
   since they aren't stable
 - Replace Components: Support recreating TTabSheet components
 - Code Proofreader: Fixed an odd AV with lots of help from Eyal Post
 - Project Dependencies: Allow aborting a dependency scan


VERSION 1.11 (February 19, 2002)
 - General: Fixed problems creating menu items in French IDE editions
   Added support for Delphi 6.02 and C++Builder 6 (largely untested)
 - Clipboard History: Options were not always saved
 - Grep Search: Treat '_' and numeric characters as part of a word
   when searching in "Whole word" matching mode
 - Code Proofreader: Allow the same dictionary word to be added to
   more than one correction language
 - PE Information: Detect more binary/subsystem/CPU types and report
   more detailed image/DLL characteristics
 - Priority Booster: Removed this expert since it wasn't working/useful


VERSION 1.1 (January 23, 2002)
 - General: Conversion to use the new Open Tools API (ToolsAPI.pas)
   Underlying expert architecture completely redesigned (Stefan)
   Removed support for Delphi 3, 4 and C++Builder 3, 4
   Added Delphi 6 and very limited Kylix support
   Added support for CLX/XFM forms in many experts
   Main configuration dialog is resizable
   All of the database experts use Client DataSets instead of the BDE
   under Delphi 6 Professional/Enterprise and Delphi 5 Enterprise
   Lots of minor UI changes, features, and bug fixes....
 - To Do List: Support for scanning an entire project group and multiple
   (semicolon separated) directories
 - Grep Search: Support for scanning an entire project group and multiple
   (semicolon separated) directories
 - Grep Results: Support for showing the context around matched lines
   Jumping to a match selects the match in the editor (Rick Hollerich)
   Results can be exported to the clipboard or a file (Rick Hollerich)
   Multiple matches on a single line are consolidated (Rick Hollerich)
   New option to always expand matches when searching completes
 - Message Dialog: Optionally generates a supporting if statement
   Automatically adds [Q]Dialogs to the uses clause if it is not already
   present (from Krzysztof Jez)
 - Debug Window: UI cleanup, added "Hide Toolbar", "Stay on Top", "Pause",
   and remote clear capability (see DbugIntf.pas)
 - Code Librarian: Snippets can be stored in a TClientDataSet
   You must set the read-only attribute on the CodeDB.cds file to safely
   share it over a network
 - Code Proofreader: Configuration dialog is resizable
   Dictionary and Auto Correct entries can be stored in a TClientDataSet
   and can contain replacement/dictionary strings up to 75 characters
 - IDE Enhancements: The top-level GExperts menu item can optionally
   be placed in the Tools menu (Michael Beck)
   The Object Inspector can be configured to show font names in the
   drop-down list using each font's typeface
 - Editor Toolbar: More comprehensive set of possible toolbar buttons
   Toolbar bitmaps match those used by the IDE (Stefan)
   Toolbar buttons automatically enable/disable as necessary
   New button to show/open units used by the current unit (John Hansen)
   New button to jump to specific positions in a unit (John Hansen)
 - Editor Experts: New editor expert to sort the selected lines
   New editor experts to jump to the previous/next occurrence of the
   current identifier (from Max Vlasov)
 - Procedure List: Procedure extraction is about 4 times faster
   Added support for parsing C++ files (Ales Kahanek)
 - Clean Directories: The unit output and executable output directories
   can be included in the default list of cleanable directories
 - Backup Project: Searches both the IDE and project library paths
   looking for included files.  Project parsing progress is reported.
 - Project Dependencies: Can open any unit found on the library path.
 - Feedback Wizard: The GExperts about box can bring up a wizard that
   makes it easy to send "useful" bug reports and other feedback.
   Please do not send feedback directly to the developers via email
   any longer. (Stig Jørgensen)
 - Help File: Updated to reflect the latest changes (James Roberts).


VERSION 1.01 (May 19, 2001)
 - General: All known bugs are fixed
 - Clean Directories: Recursive deletion was not working under Delphi 3
 - Clipboard History: Coexists with other clipboard history applications
 - Code Librarian: Fixed a possible AV when using "Contract All"
 - Replace Components: Works for data modules now
 - Grep Search: Disabled the "Ignore comments" option until it is fixed
 - PE Information: Allow clipboard copying of the "Exports" tab
 - Project Option Sets: Fixed option filtering/sorting problems


VERSION 1.00 (February, 2001)
 - General: All of the experts except the Code Proofreader can be
   dynamically enabled and disabled from the configuration dialog.
   Modified docked window menus to only activate when the docked form
   contains the currently focused control.  GExperts menu items can
   now be placed on the IDE's toolbar. Menu shortcuts are no longer
   lost when loading a project group.
 - Editor Experts: Fixed a possible range check error in the
   delimiter experts when the cursor was near the top of a unit.
   Comment and uncomment experts work around a common IDE AV. Procedure
   header has two new macros: %RESULT% and %ARGUMENTS%.
 - Perfect Layout: Improved the results when the Object Inspector
   is docked into the edit window.  Loads data from the right registry
   key (Thomas Mueller).
 - Grep Results: Fixed some minor problems opening and setting focus
   to DFM files when jumping to a grep match.  It still isn't perfect.
   Fixed a possible memory leak.
 - Replace Components: Works with modules outside the current project.
 - Editor Toolbar: Improved sizing and positioning with respect to
   docked windows.
 - Code Proofreader: The '|' character can be used in AutoCorrect
   entries to determine the cursor position after a correction.
 - Code Librarian: Double-clicking a code snippet node copies the text
   into the code editor and closes the Code Librarian.
 - To Do List: A cosmetic problem left the to do entry's displayed line
   number off by one.  Now supports jumping to one of multiple to do
   entries with the exact same comment text.
 - Class Browser: Works better with method resolution clauses (method
   redirections), multiple interface inheritance, non-standard calling
   conventions, multiple variable declarations on a single line,
   property overrides, non-standard line endings, and is generally more
   accurate when jumping to or viewing the code for class members.
 - Favorite Files: Projects, project groups, and packages will by default
   try to close the current project before opening themselves.
   Splitter position is saved/restored correctly.  The modified flag is
   set after editing the file list.
 - Project Option Sets: Fixed an issue preventing setting environment
   options that were comma delimited strings.
 - Backup Project: Reports files it is unable to find or open.
 - Help File: Updated to reflect the latest changes.


FUTURE ENHANCEMENTS?
----------------------
- [Unassigned] Persistent editor bookmarks
- [Unassigned] Integrate file auto-save code
- [Unassigned] Add compile date to version information and sync
  FileVersion with ProductVersion
- [Unassigned] Add GX_MessageBox warnings to the Pascal only experts
  when used under C++Builder
- [Unassigned] Save/Restore of editor macros
- [Unassigned] Replace SortGrid with a TListView and make the various
  TListViews sortable
- [Unassigned] Integrate a full regular expression parser as a
  compile-time option to the grep search
- [Unassigned] Allow adding some common editor and object inspector
  actions to the editor toolbar?
- [Unassigned] Add support for hiding the toolbar on all dockable experts
- [Unassigned] Modify To Do expert so that it can display Borland style
  to do items. The basic parsing code exists, but needs to be surfaced
  in the user interface.
- [Unassigned] Add word wrapped printing to the To Do List, etc.


MISCELLANEOUS NOTES
----------------------
- If a DFM is opened as text from the Grep Results (for example), you
  may experience an IDE bug where it forgets the value of the Text DFM
  flag on the form and converts it to a binary format.
- The Code Proofreader can only perform corrections when the editor caret
  is directly after the word to correct following a change.  As a result,
  pressing enter will not trigger corrections.
- Changing the configured GExperts top-level menu item location requires
  an IDE restart before the new setting is active.
- The editor toolbar does not support separators when aligned left/right.
- If a DFM is opened as text, a subsequent Grep Search can not scan the
  DFM's associated source files.  Delphi does not provide access to the
  source file buffer.  Under C++Builder you might also see duplicated
  matches for the DFM file when it is being viewed as text.
- Perfect Layout was designed for Delphi 3, and as such will probably
  never support docking or undocking of windows in later version IDEs.
  Please use the desktop saving feature of your IDE instead.
- Nothing in GExperts is aware of or takes into account conditional
  defines, so code that is defined out will still be parsed by GExperts.
- Source Export only supports background colors when exporting to HTML.


THANKS
----------------------
Vincent Parrett donated a copy of FinalBuilder 5
http://www.atozedsoftware.com/

AutomatedQA donated copies of AQTime and TestComplete
http://www.automatedqa.com/

Alexander Halser donated a copy of Help and Manual 3
http://www.helpandmanual.com/


GEXPERTS LICENSE
----------------------

GExperts is copyright 1996-2008 by GExperts, Inc, Erik Berry, and several
other authors who have submitted their code for inclusion. This license
agreement only covers code written by GExperts, Inc. and Erik Berry. You
should contact the other authors concerning their respective copyrights
and conditions.

The rules governing the use of GExperts and the GExperts source code are
derived from the official Open Source Definition, available at
http://www.opensource.org/. The conditions and limitations are as follows:

Usage of GExperts binary distributions is permitted for all developers.
You may not use the GExperts source code to develop proprietary or
commercial products including plugins or libraries for those products.
You may use the GExperts source code in an Open Source project, under
the terms listed below.

You may not use the GExperts source code to create and distribute custom
versions of GExperts under the "GExperts" name.  If you do modify and
distribute custom versions of GExperts, the binary distribution must be
named differently and clearly marked so users can tell they are not using
the official GExperts distribution.  A visible and unmodified version of
this license must appear in any modified distribution of GExperts.

Custom distributions of GExperts must include all of the custom changes
as a patch file that can be applied to the original source code.  This
restriction is in place to protect the integrity of the original author's
source code.  No support for modified versions of GExperts will be
provided by the original authors or on the GExperts mailing lists.

All works derived from GExperts must be distributed under a license
compatible with this license and the official Open Source Definition,
which can be obtained from http://www.opensource.org/.

Please note that GExperts, Inc. and the other contributing authors hereby
state that this package is provided "as is" and without any express or
implied warranties, including, but not without limitation, the implied
warranties of merchantability and fitness for a particular purpose. In
other words, we accept no liability for any damage that may result from
using GExperts or programs that use the GExperts source code.

If you have license questions, please email Erik Berry at eberry@gexperts.org.

EOF
