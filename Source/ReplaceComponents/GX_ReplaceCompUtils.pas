// Original Author: Piotr Likus
// Replace Components Utilities
unit GX_ReplaceCompUtils;

{$I GX_CondDefine.inc}

interface

uses
  Classes, TypInfo;

type
  TPropInfoList = class(TList)
  private
    function GetItem(Index: Integer): PPropInfo;
    procedure SetItem(Index: Integer; const Value: PPropInfo);
  protected
    FOwner: TObject;
  public
    function FindProperty(const APropName: string): Integer; virtual;
    property Owner: TObject read FOwner write FOwner;
    property Items[Index: Integer]: PPropInfo read GetItem write SetItem; default;
  end;

function GetPropertyList(ClassInfo: PTypeInfo): TPropInfoList;
procedure GetPropertyNames(const AClassName: string; Items: TStrings);
function ClassLevel(SearchForClass, TestClass: TClass): Integer;
function IsStoredProperty(Instance: TObject; APropInfo: PPropInfo): Boolean;
function IsDefaultPropertyValue(Instance: TObject; APropInfo: PPropInfo): Boolean;

function FlatLine(const AText: string): string;
function EscapeStr(const AStr: string; AEscapeChar: Char): string;
function CSVAddItem(const ALine, AItem: string; ASepChar: Char = ','; AQuoteChar: Char = '"'): string;
function ExtractToken(var VStr: string; const Delimiter: string): string;

implementation

uses
  Variants, GX_GenericUtils, SysUtils;
  
function FlatLine(const AText: string): string;
begin
  Result := CompressWhiteSpace(AText);
end;

function EscapeStr(const AStr: string; AEscapeChar: Char): string;
var
  i: Integer;
begin
  Result := AStr;
  i := Length(AStr);
  while i >= 1 do
  begin
    if AStr[i] = AEscapeChar then
      Insert(AEscapeChar, Result, i);
    Dec(i);
  end;
end;

function CSVAddItem(const ALine, AItem: string; ASepChar: Char; AQuoteChar: Char): string;
var
  sItem: string;
begin
  if (Pos(ASepChar, AItem)>0) or (Pos(AQuoteChar, AItem)>0) then
  begin
    sItem := EscapeStr(AItem, AQuoteChar);
    sItem := AQuoteChar+sItem+AQuoteChar;
  end
  else // no quotes
    sItem := AItem;

  if ALine = '' then
    Result := sItem
  else
    Result := ALine+ASepChar+sItem;
end;

function ExtractToken(var VStr: string; const Delimiter: string): string;
var
  Idx: Integer;
begin
  Idx := Pos(Delimiter, VStr);
  if Idx > 0 then
  begin
    Result := Copy(VStr, 1, Idx-1);
    VStr := Copy(VStr, Idx + Length(Delimiter),
      Length(VStr)-Idx-Length(Delimiter)+1);
  end
  else
  begin
    Result := VStr;
    VStr := '';
  end;
end;

function GetPropertyList(ClassInfo : PTypeInfo): TPropInfoList;
var
  i, PropCount: Integer;
  PropList: PPropList;
begin
  Assert(ClassInfo<>nil);

  Result := TPropInfoList.Create;
  try
    PropCount := GetTypeData(ClassInfo)^.PropCount;
    if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(Pointer));
      try
        GetPropInfos(ClassInfo, PropList);
        Result.Count := PropCount;
        for i := 0 to PropCount - 1 do
          Result.Items[i] := PropList[i];
      finally
        FreeMem(PropList);
      end;
    end;
  except
    FreeAndNil(Result);
  end;
end;

procedure GetPropertyNames(const AClassName: string; Items: TStrings);
var
  ClassPtr: TPersistentClass;
  PropertyList: TPropInfoList;
  i: Integer;
begin
  Items.Clear;
  ClassPtr := GetClass(AClassName);
  if ClassPtr = nil then Exit;

  PropertyList := GetPropertyList(ClassPtr.ClassInfo);

  if PropertyList = nil then
    Exit;

  try
    for i := 0 to PropertyList.Count-1 do
      Items.Add(string(PropertyList.Items[i].Name));
  finally
    FreeAndNil(PropertyList);
  end;
end;

function ClassLevel(SearchForClass, TestClass: TClass): Integer;
var
  ClassRef: TClass;
begin
  if (SearchForClass = nil) or (TestClass = nil) then
  begin
    Result := -1;
    Exit;
  end;

  Result := 0;
  ClassRef := SearchForClass;
  while (ClassRef <> nil) and (not ClassRef.ClassNameIs(TestClass.ClassName)) do
  begin
    Inc(Result);
    ClassRef := ClassRef.ClassParent;
  end;

  if ClassRef = nil then
    Result := -1;
end;

function IsStoredProperty(Instance: TObject; APropInfo: PPropInfo): Boolean;
begin
  Result := not IsDefaultPropertyValue(Instance, APropInfo);
  if Result then
    Result := TypInfo.IsStoredProp(Instance, APropInfo);
end;

// Based on D7 Classes.IsDefaultPropertyValue
function IsDefaultPropertyValue(Instance: TObject; APropInfo: PPropInfo): Boolean;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Default := PPropInfo(APropInfo)^.Default;
    Value := GetOrdProp(Instance, APropInfo);
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, APropInfo);
    Result := (Value = 0);
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, APropInfo);
    Result := (Value = '');
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, APropInfo);
    Result := VarIsClear(Value);
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, APropInfo);
    Result := (Value = 0);
  end;

  function IsDefaultObjectProp: Boolean;
  var
    Obj: TObject;
  begin
    Obj := GetObjectProp(Instance, APropInfo);
    Result := (Obj = nil);
  end;

  function IsDefaultMethodProp: Boolean;
  var
    Met: TMethod;
  begin
    Met := GetMethodProp(Instance, APropInfo);
    Result := (Met.Code = nil);
  end;

  function IsDefaultInterfaceProp: Boolean;
  var
    Intf: IInterface;
  begin
    Intf := GetInterfaceProp(Instance, APropInfo);
    Result := (Intf = nil);
  end;

var
  PropType: PTypeInfo;
begin
  Result := True;
  if APropInfo^.GetProc = nil then
    Exit;

  PropType := PPropInfo(APropInfo)^.PropType^;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet:
      Result := IsDefaultOrdProp;
    tkFloat:
      Result := IsDefaultFloatProp;
    tkString, tkLString, tkWString {$IFDEF GX_VER200_up}, tkUString {$ENDIF}:
      Result := IsDefaultStrProp;
    tkVariant:
      Result := IsDefaultVariantProp;
    tkInt64:
      Result := IsDefaultInt64Prop;
    tkClass:
      Result := IsDefaultObjectProp;
    tkMethod:
      Result := IsDefaultMethodProp;
    tkInterface:
      Result := IsDefaultInterfaceProp;
  end;
end;

{ TPropInfoList }

function TPropInfoList.FindProperty(const APropName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if string(Items[i].Name) = APropName then
    begin
      Result := i;
      Break;
    end;
end;

function TPropInfoList.GetItem(Index: Integer): PPropInfo;
begin
  Result := PPropInfo(inherited Items[Index]);
end;

procedure TPropInfoList.SetItem(Index: Integer; const Value: PPropInfo);
begin
  inherited Items[Index] := Value;
end;

end.

