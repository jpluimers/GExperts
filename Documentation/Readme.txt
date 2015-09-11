GExperts 1.38
Open Source Programming Tools for Delphi and C++Builder

Source code, the FAQ, and the latest news are available at:
  http://www.gexperts.org/

Please send all bug reports and suggestions using the feedback wizard
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
- The following features are not supported under Delphi 8 - RAD Studio 10:
  Inner classes, class helpers, anonymous methods, and generics in the
  Class Browser, and some of the old editor tab enhancements, such as
  multiline editor tabs.  Note that the IDE converts some high ANSI and MBCS
  characters to UTF-8 UNICODE when loaded in the code editor, so you may
  experience problems with those files (especially in Delphi 2007 and earlier).
- The compiler replacement option of the Code Proofreader is only partially
  working under Delphi 7 - RAD Studio 10.  It does not correct when an identifier is
  terminated by pressing a symbol key such as a period or open parenthesis,
  but works fine when the symbol is terminated by a space (IDE limitation).
- Due to either native Open Tools API limitations or bugs, the following
  items can not be supported:
  Delphi 8 Only:
   - Rename Components (IDE bug setting the Name property)
   - Jumping to a form search match from Grep Results when the source is open
     (IDE bug)
  Delphi 2005 Only:
   - The Project Option Sets environment options (IDE bug)
   - Set Tab Order: Selecting components in the desired tab order (IDE bug)
  Delphi 8 - 2007:
   - Any interaction at all with the WinForms/.NET form designer/components
     (no WinForms support for IOTAFormEditor/IOTAComponent)
   - Replace Components for VCL.NET (No direct access to components)
   - Components to Code for VCL.NET (No direct access to components)
  Delphi 8 - RAD Studio 10:
   - It is no longer possible for addins to override some built-in IDE
     shortcuts.  You may need to configure your GExperts shortcuts (Prev/Next
     Identifier, Procedure List, etc.) to not conflict with your selected
     keymapping.  Also, the Macro Library can not always intercept the
     Shift+Ctrl+R keystroke to automatically grab keyboard macros, so you may
     need to create macros using the recording functions in the toolbar.
  Delphi 2009 - 2010 Update 1:
   - Record and playback in the Marco Library does not work.  This is an IDE
     bug handling editor macro streams (QC 78289).


INSTALLATION
----------------------
GExperts is distributed as a self-installing executable that should
automatically install itself into the IDE.  If you are installing under
Vista as a non-administrator, you may need to manually register the GExperts
DLL with the IDE after installation.  This is most easily done using the
Expert Manager tool in the GExperts start menu group.  If you prefer, you
can also manually register the DLL with the IDE using the Windows registry
editor (RegEdit.exe).  Create a key similar to the following (the
version number appearing before "\Experts\" is IDE dependent):
HKEY_CURRENT_USER\Software\Embarcadero\BDS\17.0\Experts\ (RAD Studio 10)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\16.0\Experts\ (RAD Studio XE8)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\12.0\Experts\ (RAD Studio XE5)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\9.0\Experts\  (RAD Studio XE2)
HKEY_CURRENT_USER\Software\CodeGear\BDS\6.0\Experts\     (RAD Studio 2009)
HKEY_CURRENT_USER\Software\Borland\BDS\5.0\Experts\      (RAD Studio 2007)
HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Experts\   (Delphi 7)

Then add a new string value that points to your GExperts DLL (X is the IDE
version such as 6, 7, 8, 2005, 2006, 2007, 2009, 2010, XE1, XE8, 10, etc.):
GExperts=C:\Program Files\GExperts\GExpertsRSX.dll
GExperts=C:\Program Files\GExperts\GExpertsDelphiX.dll
GExperts=C:\Program Files\GExperts\GExpertsBDSX.dll or
GExperts=C:\Program Files\GExperts\GExpertsDX.dll or

Before installing GExperts, it is strongly recommended that you install
the latest updates and patches for your IDE available from:
  http://www.embarcadero.com/


UNINSTALLATION
----------------------
Uninstallation of GExperts can be done from the Control Panel's
Add/Remove Programs or Programs [and Features] tool.


CHANGE LOG
----------------------
VERSION 1.38 (September 2015)
- General: Added support for RAD Studio 10 Seattle.
- General: Improved unicode support for several GExperts tools (Thomas Mueller).
- General: Various minor bug fixes.


VERSION 1.37 (September 2012 - May 2015)
- General: Added support for RAD Studio XE3, XE4, XE5, XE6, XE7, and XE8
  (with help from Jeroen Pluimers).  Various other minor bug fixes.
- Grep: Add Ignore Delphi comments feature (via Paul Gardner).  Significantly
  speed up searches by updating the search status details less often.  Add
  ability to auto-hide the Grep Results window when jumping to a match.
- Components to Code: Preview the generated code as you change the options.
- Message Dialog: Support leading spaces better for inserted code.
  Force focus to editor after code insertion.
- Set Component Properties: Close opened but not modified files to save
  resources.  Force show modified files, to show changes without saving.
  Ignore some problematic TWebBrower properties causing read errors.


VERSION 1.36 (December, 2011)
- General: Add official support for RAD Studio XE2, Allow filtering
  expert list in configuration dialog (Ulrich Gerhardt), Update SynEdit
  (syntax-highlighting text editor) version
- Replace Components: Ignore Touch/TTouchManager properties, since they
  crash 2010/XE when copied to a new component, add more error checking.
- Backup Project: Update Abbrevia component version used to generate .zip files
- Set Component Properties: Allow setting properties on forms and data modules
- Grep: Ignore symbolic link directories during recursive searches
- Uses Clause Manager: Add shortcut to open selected file.  Close dialog
  after opening a file.
- Message Dialog: Changed default shortcut to Shift+Ctrl+D to not conflict
  with source formatter in recent IDE releases.


VERSION 1.35 (June, 2011)
- General: Fix problems with the bug reporting tool and about box disappearing.
- Grep: Search and replace supports regular expression subgroubs (Egon Elbre).
- Favorite Files: Support relative filenames in saved files lists, support
  multiple groups of favorite files.  These changes may be useful for
  project-specific favorite files lists. (Ulrich Gerhardt, Erik)
- Clipboard History: Show combined text for multi-selected history entries.


VERSION 1.34 (September, 2010)
- General: Added support for RAD Studio XE.  Minor bugfixes and updates to
  the help files. (Erik)
- Set FocusControl: Add tool to assign focus control of labels by selecting
  the related label and wincontrol or a group of wincontrols, and the tool
  tries to guess the label to associate with each control.  (Daniel
  Maltarollo and Erik)
- Grep: Allow excluding any number of directories from a search.
  Always center the match line in the Grep Result context pane.
- Components to Code: Optionally generate code to free all created
  components (Peter Dzomlija).
- Hide/Show Non-Visual Components: Added support for TFrame designers (Erik).


VERSION 1.33 (May 31, 2009)
- General: Added support for RAD Studio 2009/2010.  Note that some of the tools
  that require our source code parser (Procedure List, Class Browser, To Do
  List, Delimiter Editor Experts, etc.) do not have full unicode support.
  Other tools such as the Grep Search/Replace, Source Export, Code Librarian,
  etc. should support UNICODE files just fine.  Also included are some shutdown
  optimizations, removal of all known memory leaks, and other minor tweaks.
  All GExperts forms use the OS default font face/size.  You can also override
  the UI font in the General tab of the settings.
- Grep Search: There is a new regular expression engine that supports a large
  subset of the Perl regular expression syntax.  Grep can now search and
  replace files in ANSI, UTF-8, and UTF-16 formats in Delphi 2009/2010.  The
  stay on top feature was removed when running inside the IDE, since it could
  hide modal dialogs.  Allow searching within previously matched files
  (John Hansen).
- Hide/Show Non-Visual Components: This new tool allows temporarily hiding
  non-visual components on a form, so you can visualize the form as it will
  appear at runtime.
- Editor Experts: The main menu contains a submenu item for each editor
  expert now in Delphi 8 or later.
- Favorite Files: Supports preview of jpg, png, and gif files if the VCL
  supports it in your IDE version, supports select all for files and move
  up/down for folders.
- Code Proofreader: Should be slightly more accurate with fewer false positive
  corrections made to your code.
- To Do List: Display the category and owner for to do items, indicate items
  marked DONE, support more priorities (Pestasoft, Erik).
- Code Librarian: Support C# and XML syntax highlighting.
- Help File: Updated to reflect the current release. (John Hansen, Erik)


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


FUTURE ENHANCEMENTS?
----------------------
- [Unassigned] Persistent editor bookmarks
- [Unassigned] Integrate file auto-save code
- [Unassigned] Add compile date to version information and sync
  FileVersion with ProductVersion
- [Unassigned] Add GX_MessageBox warnings to the Pascal only experts
  when used under C++Builder
- [Unassigned] Replace SortGrid with a TListView and make the various
  TListViews sortable
- [Unassigned] Allow adding some common editor and object inspector
  actions to the editor toolbar?
- [Unassigned] Add support for hiding the toolbar on all dockable experts
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


GEXPERTS LICENSE
----------------------

GExperts is copyright 1996-2015 by GExperts, Inc, Erik Berry, and several
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
