unit GX_dzClassUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

function TStrings_ValueFromIndex(_st: TStrings; _Idx: integer): string;

implementation

function TStrings_ValueFromIndex(_st: TStrings; _Idx: integer): string;
var
  Name: string;
begin
  Name := _st.Names[_Idx];
  Result := _st.Values[Name];
end;

end. 
