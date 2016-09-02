{ $Id: GX_FormatterUnitTests.dpr,v 1.3 2005/12/09 16:20:35 twm Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.3 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developer of the Original Code are Serge Beaumont,
 * Jeff Moore and Chris Houghten.
 * Portions created The Initial Developers are Copyright (C) 2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Chris Houghten <choughte@users.sourceforge.net>
 * Serge Beaumont <beaumose@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *)

{$IFDEF LINUX}
{$DEFINE DUNIT_CLX}
{$ENDIF}
program GX_FormatterUnitTests;

uses
  TestFramework,
  GUITestRunner,
  DelForExTestOldNew in 'DelForExTestOldNew.pas',
  DelForExTestFiles in 'DelForExTestFiles.pas',
  GX_CodeFormatterDefaultSettings in '..\..\Source\Formatter\GX_CodeFormatterDefaultSettings.pas',
  GX_CodeFormatterTokenList in '..\..\Source\Formatter\engine\GX_CodeFormatterTokenList.pas',
  GX_CodeFormatterTypes in '..\..\Source\Formatter\engine\GX_CodeFormatterTypes.pas',
  GX_CodeFormatterEngine in '..\..\Source\Formatter\engine\GX_CodeFormatterEngine.pas',
  GX_CodeFormatterTokens in '..\..\Source\Formatter\engine\GX_CodeFormatterTokens.pas',
  GX_CodeFormatterSettings in '..\..\Source\Formatter\engine\GX_CodeFormatterSettings.pas',
  GX_CodeFormatterFormatter in '..\..\Source\Formatter\engine\GX_CodeFormatterFormatter.pas',
  GX_CodeFormatterStack in '..\..\Source\Formatter\engine\GX_CodeFormatterStack.pas',
  GX_CodeFormatterParser in '..\..\Source\Formatter\engine\GX_CodeFormatterParser.pas',
  GX_CodeFormatterConfigHandler in '..\..\Source\Formatter\GX_CodeFormatterConfigHandler.pas',
  GX_CodeFormatterUnicode in '..\..\Source\Formatter\engine\GX_CodeFormatterUnicode.pas',
  GX_GenericUtils in '..\..\Source\Utils\GX_GenericUtils.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  TGUITestRunner.runRegisteredTests;
end.

