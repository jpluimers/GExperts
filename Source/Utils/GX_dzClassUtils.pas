/// <summary>
/// This is an extract from u_dzClassUtils in dzlib http://blog.dummzeuch.de/dzlib/ </summary>
unit GX_dzClassUtils;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils;

///<summary>
/// Assigns st to sl and sorts it.
/// sl.Objects contains the index into st+1
///            so st[Integer(ls.Objects[i])] = sl[i] </summary>
procedure TStrings_GetAsSortedList(_st: TStrings; _sl: TStringList; _Duplicates: TDuplicates = dupAccept);

///<summary>
/// assign the current index to the Objects property and then sort the list </summary>
procedure TStringList_MakeIndex(_sl: TStringList);

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);

function TStrings_ValueFromIndex(_st: TStrings; _Idx: integer): string;

implementation

procedure TStrings_GetAsSortedList(_st: TStrings; _sl: TStringList; _Duplicates: TDuplicates = dupAccept);
begin
  Assert(Assigned(_st));
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  _sl.Assign(_st);
  TStringList_MakeIndex(_sl);
end;

function TStrings_ValueFromIndex(_st: TStrings; _Idx: integer): string;
var
  Name: string;
begin
  Assert(Assigned(_st));

  Name := _st.Names[_Idx];
  Result := _st.Values[Name];
end;

procedure TStringList_MakeIndex(_sl: TStringList);
var
  i: integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := POinter(i + 1);
  _sl.Sorted := true;
end;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);
var
  i: integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := POinter(i + 1);
  _sl.Sorted := true;
end;

end.

