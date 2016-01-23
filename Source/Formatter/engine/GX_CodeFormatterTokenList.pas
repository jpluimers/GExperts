// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterTokenList;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_CodeFormatterTokens;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TPascalToken;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: List for storing TPascalToken items }
  TPascalTokenList = class(_DZ_OBJECT_LIST_TEMPLATE_)

  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

end.

