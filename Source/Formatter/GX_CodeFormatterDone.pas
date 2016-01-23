// displayed by the code formatter after formatting a unit
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterDone;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
{$IFDEF GX_VER140_up}
  Variants,
{$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TfmCodeFormatterDone = class(TForm)
    Label1: TLabel;
    chk_DontShowAgain: TCheckBox;
    Label2: TLabel;
    b_Ok: TButton;
  private
  public
  end;

implementation

{$R *.dfm}

end.

