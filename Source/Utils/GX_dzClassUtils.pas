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

function TStrings_ValueFromIndex(_st: TStrings; _Idx: Integer): string;

///<summary>
/// Like TComponent.FindComponent but optionally search recursively because in the IDE
/// sometimes not all controls are owned by the form (e.g. in the Project Options dialog).
/// This will do a depth first search.
/// Name can be an empty string, which will return the first component with an empty name.</sumamry>
function TComponent_FindComponent(_Owner: TComponent; const _Name: string; _Recursive: Boolean;
  out _Found: TComponent): Boolean;

implementation

function TComponent_FindComponent(_Owner: TComponent; const _Name: string; _Recursive: Boolean;
  out _Found: TComponent): Boolean;
var
  i: Integer;
  comp: TComponent;
begin
  if (_Owner = nil) then begin
    Result := False;
    Exit;
  end;
  Result := True;
  for i := 0 to _Owner.ComponentCount - 1 do begin
    comp := _Owner.Components[i];
    if SameText(comp.Name, _Name) then begin
      _Found := comp;
      Exit;
    end;
    if _Recursive then begin
      if TComponent_FindComponent(comp, _Name, _Recursive, _Found) then
        Exit;
    end;
  end;
  Result := False;
end;

procedure TStrings_GetAsSortedList(_st: TStrings; _sl: TStringList; _Duplicates: TDuplicates = dupAccept);
begin
  Assert(Assigned(_st));
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  _sl.Assign(_st);
  TStringList_MakeIndex(_sl);
end;

function TStrings_ValueFromIndex(_st: TStrings; _Idx: Integer): string;
var
  Name: string;
begin
  Assert(Assigned(_st));

  Name := _st.Names[_Idx];
  Result := _st.Values[Name];
end;

procedure TStringList_MakeIndex(_sl: TStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := POinter(i + 1);
  _sl.Sorted := True;
end;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := POinter(i + 1);
  _sl.Sorted := True;
end;

end.
