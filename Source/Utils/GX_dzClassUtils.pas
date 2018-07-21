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

procedure TStrings_FreeWithObjects(_List: TStrings);

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
  out _Found: TComponent; _CmpClass: TComponentClass = nil): Boolean;

///<summary>
/// Checks both, the code and data pointer of a Method and returns true, if both are equal </summary>
function IsSameMethod(_Method1, _Method2: TNotifyEvent): Boolean;

procedure TList_FreeWithObjects(_List: TList);

implementation

procedure TList_FreeWithObjects(_List: TList);
var
  i: Integer;
begin
  if Assigned(_List) then begin
    for i := 0 to _List.Count - 1 do
      TObject(_List[i]).Free;
    FreeAndNil(_List);
  end;
end;

procedure TStrings_FreeWithObjects(_List: TStrings);
var
  i: Integer;
begin
  if Assigned(_List) then
    for i := 0 to _List.Count - 1 do begin
      _List.Objects[i].Free;
      _List.Objects[i] := nil;
    end;
  _List.Free;
end;

function IsSameMethod(_Method1, _Method2: TNotifyEvent): Boolean;
begin
  Result := (TMethod(_Method1).Code = TMethod(_Method2).Code)
    and (TMethod(_Method1).Data = TMethod(_Method2).Data);
end;

function TComponent_FindComponent(_Owner: TComponent; const _Name: string; _Recursive: Boolean;
  out _Found: TComponent; _CmpClass: TComponentClass = nil): Boolean;
var
  i: Integer;
  comp: TComponent;
begin
  Result := False;
  if (_Owner = nil) then
    Exit;

  for i := 0 to _Owner.ComponentCount - 1 do begin
    comp := _Owner.Components[i];
    if SameText(comp.Name, _Name) then begin
      Result := not Assigned(_CmpClass) or (comp is _CmpClass);
      if Result then begin
        _Found := comp;
        Exit;
      end;
    end;
    if _Recursive then begin
      Result := TComponent_FindComponent(comp, _Name, _Recursive, _Found, _CmpClass);
      if Result then
        Exit;
    end;
  end;
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
    _sl.Objects[i] := Pointer(i + 1);
  _sl.Sorted := True;
end;

procedure TGXUnicodeStringList_MakeIndex(_sl: TGXUnicodeStringList);
var
  i: Integer;
begin
  Assert(Assigned(_sl));

  _sl.Sorted := False;
  for i := 0 to _sl.Count - 1 do
    _sl.Objects[i] := Pointer(i + 1);
  _sl.Sorted := True;
end;

end.
