unit GXFmxUtils;

{$I GX_CondDefine.inc}

interface

type
  TGxAlign = (galNone, galTop, galBottom, galLeft, galRight, galClient, galCustom);

function TryIntToGxAlign(_IntValue: Integer; out _ga: TGxAlign): Boolean;
function GxAlignToInt(_ga: TGxAlign): Integer;

implementation

uses
{$IFDEF GX_VER230_up}
  Fmx.Types,
{$ENDIF}
  GX_Otautils;

function TryIntToGxAlign(_IntValue: Integer; out _ga: TGxAlign): Boolean;
{$IFDEF GX_VER230_up}
var
  al: TAlignLayout;
{$ENDIF}
begin
{$IFDEF GX_VER230_up}
  if GxOtaActiveDesignerIsFMX then begin
    al := TAlignLayout(_IntValue);
    Result := True;
    case al of
      TAlignLayout.None: _ga := galNone;
      TAlignLayout.Top: _ga := galTop;
      TAlignLayout.Left: _ga := galLeft;
      TAlignLayout.Right: _ga := galRight;
      TAlignLayout.Bottom: _ga := galBottom;
      TAlignLayout.Client: _ga := galClient;
//      TAlignLayout.MostTop: ; // _ga := galTop;
//      TAlignLayout.MostBottom: ; // _ga := galBottom;
//      TAlignLayout.MostLeft: ; // _ga := galLeft;
//      TAlignLayout.MostRight: ; // _ga := galRight;
    else
//      TAlignLayout.Contents: ;
//      TAlignLayout.Center: ;
//      TAlignLayout.VertCenter: ;
//      TAlignLayout.HorzCenter: ;
//      TAlignLayout.Horizontal: ;
//      TAlignLayout.Vertical: ;
//      TAlignLayout.Scale: ;
//      TAlignLayout.Fit: ;
//      TAlignLayout.FitLeft: ;
//      TAlignLayout.FitRight: ;
      Result := False;
    end;
  end else
{$ENDIF}begin
    _ga := TGxAlign(_IntValue);
    Result := True;
  end;
end;

function GxAlignToInt(_ga: TGxAlign): Integer;
begin
{$IFDEF GX_VER230_up}
  if GxOtaActiveDesignerIsFMX then begin
    case _ga of
      galNone: Result := Ord(TAlignLayout.None);
      galTop: Result := Ord(TAlignLayout.Top);
      galBottom: Result := Ord(TAlignLayout.Bottom);
      galLeft: Result := Ord(TAlignLayout.Left);
      galRight: Result := Ord(TAlignLayout.Right);
      galClient: Result := Ord(TAlignLayout.Client);
      galCustom: Result := Ord(TAlignLayout.None);
    else
      Result := Ord(TAlignLayout.None);
    end;
  end else
{$ENDIF}begin
    Result := Ord(_ga);
  end;
end;

end.

