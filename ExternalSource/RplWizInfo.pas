unit RplWizInfo;

{$I GX_CondDefine.inc}

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  Component Replace Wizard.

  Temporarily store component and property information
  for doing search & replace of components. Replacing
  a component means deleting the old one and inserting the
  new one. This might involve deleting all the children
  of the old component, which might be a lot. Hence the need
  for a comprehensive way to store an arbitrary tree of components.

  A TCompInfo object represents a single component. It can have
  children, which are stored in a TCompList, which is just a list
  of TCompInfo objects. Each TCompInfo object also has a list of
  properties, which is a TPropList object. A property list contains
  a list of TPropInfo objects. Each TPropInfo object represents
  a single property. The property value might be a string, Variant,
  etc., so the TPropInfo object must be able to handle anything.

  Each object knows how to create itself from an IComponent interface,
  to capture an existing component's state. When replacing a component,
  TCompInfo creates a TCompInfo object for the component, its properties
  and its child components. Then it deletes the component interface,
  and creates a new one, with the new type. It then asks all the properties
  to apply themselves to the new component. Only those with matching
  names and types apply themselves. The others are silently ignored.
  (This avoids nagging the user about minor problems, e.g., converting
  a TBitBtn to a TButton, but introduces the possibility that the user might
  make a gross error, e.g., replacing a TBitBtn with a TBlobField. There is
  no undo, so this is potentially a problem, but it is better to assume
  the user is intelligent than to assume the user is an idiot.) TCompInfo
  then recreates all the child objects.

  To replace components, descend the component hierarchy once. Any
  component that is to be replaced, replace it and recreates its children.
  When recreating children, any old child that was to be replace is replaced.
  Thus, the recursive descent bifurcates the first time it encounters
  a component to be replaced. The Search routine descends the tree looking
  for a component to replace. The Replace routine does the replacement.
}

// This is defined to use "hacked" version of TComponent - to access it's
// private fields. It is used to re-link all components connected to the
// old instance of the replaced component.
// Optional. Use only if some components are not re-linked during replace.
{.$DEFINE Use_HackedComponent}

interface

uses
  SysUtils, TypInfo, Classes, OwnerList, Controls, ToolsAPI,
  GX_ReplaceCompData;

const
{ Default Height / Width for non-visible components.
  Can be any value >0, required to correctly specify position on replace.
  (20 is safe also for visible components) }
  NonVisibleDefSize = 20;

{ Represent a component as a type name and a list of
  properties. A property has a type and a value. }
type
  TFriendComponent = class(TComponent);

  TCompInfo = class;
  TCompInfoProc = procedure(ACompInfo: TCompInfo; AData: Pointer; var Stop: Boolean) of object;

{ TCompRepController
  Class that controls processing.
  Exception handling, log maintenance, mapping preparation.
}
  TCompRepController = class
  protected
    FDestClassName: string;
    FSourceClassName: string;
    FRoot: TCompInfo;
  public
    procedure HandleException(E: Exception; const Context: string); virtual; abstract;
    procedure LogMsg(const AMsg: string); virtual; abstract;

    procedure SignalBegin; virtual; abstract;
    procedure SignalEnd; virtual; abstract;

    procedure SignalFileBegin(const AFileName: string); virtual; abstract;
    procedure SignalFileEnd(const AFileName: string); virtual; abstract;
    procedure SignalStackBegin(const AName: string); virtual; abstract;
    procedure SignalStackEnd(const AName: string); virtual; abstract;
    procedure SignalObjectBegin(const AName: string; AObject: TObject); virtual; abstract;
    procedure SignalObjectEnd(const AName: string; AObject: TObject); virtual; abstract;

    procedure PrepareMappingForProp(const AClassName, APropName: string;
      Mappings: TCompRepMapList); virtual; abstract;
    procedure PrepareConstMappingForProp(const AClassName, APropName: string;
      Mappings: TCompRepMapList); virtual; abstract;

    function IsLogValuesForced: Boolean; virtual; abstract;

    property SourceClassName: string read FSourceClassName write FSourceClassName;
    property DestClassName: string read FDestClassName write FDestClassName;
    property Root: TCompInfo read FRoot write FRoot;
  end;

  TPropertyValue = record
    { Delphi does not let you store a Variant or a string in a variant
      record, so use a character array that is big enough to hold
      a Variant. Delphi stores a string as a Pointer, so use Pointer
      as the type. }
    case TTypeKind of
      tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
                (IntegerValue: Longint;);
      tkInt64: (Int64Value: Int64;);
      tkFloat:  (FloatValue: Extended;);
      tkMethod: (MethodValue: TMethod;);
      tkClass, tkString, tkLString, tkWString {$IFDEF GX_VER200_up}, tkUString {$ENDIF}: (PtrValue: Pointer;);
      tkVariant: (VariantValue: array[1..SizeOf(Variant)] of Byte);
  end;

  TPropInfo = class
  private
    FPropType: TTypeKind;
    FPropValue: TPropertyValue;
    FPropName: string;
    FController: TCompRepController;
  public
    constructor Create(Controller: TCompRepController; Index: Integer;
      CompIntf: IOTAComponent);
    procedure SetComponent(CompIntf: IOTAComponent);
    function GetIntValue(Default: Integer): Integer;
    function GetStrValue(const Default: string): string;
    property Name: string read FPropName;
    property PropType: TTypeKind read FPropType;
    property PropValue: TPropertyValue read FPropValue;
  end;

  TPropList = class(TOwnerList)
  private
    FStream: TMemoryStream;
    FLeft: Integer;
    FTop: Integer;
    FController: TCompRepController;
    function GetPropInfo(Index: Integer): TPropInfo;
    procedure SetComponentPosition(ADestComponent: IOTAComponent);
    procedure SetComponentDefProps(CompIntf: IOTAComponent);
    procedure AssignPropsToCompByLoop(CompIntf: IOTAComponent);
    procedure AssignPropsToCompByMapping(OldCompIntf, NewCompIntf: IOTAComponent);
    procedure CopyComponentProps(OldCompIntf, NewCompIntf: IOTAComponent);
    procedure ExecutePropAssignWithMapping(OldCompIntf: IOTAComponent;
      NewCompIntf: IOTAComponent;
      SourcePropInfo: TPropInfo; DestPropList: TPropList; Mappings: TCompRepMapList);
    procedure ExecutePropAssign(OldCompIntf, NewCompIntf: IOTAComponent;
      SourcePropInfo: TPropInfo; DestPropList: TPropList;
      const SourceExpression, DestExpression: string);
    procedure FindPropInfo(CompIntf: IOTAComponent; PropList: TPropList;
      const PropExpression: string; var VPropInfo: PPropInfo; var Obj: TObject);
    procedure CopyPropValue(SourceObject: TObject; SourceInfo: PPropInfo;
      DestObject: TObject; DestInfo: PPropInfo);
    procedure FindPropInfoEx(CompIntf: IOTAComponent; PropList: TPropList;
      const PropExpression: string; var VPropInfo: PPropInfo;
      var Obj: TObject; var VParentPropInfo: PPropInfo;
      var ParentObj: TObject);
    function CheckCompNameAssign(SourceObject: TObject;
      SourceInfo: PPropInfo; const DestExpression: string;
      DestObject: TObject; DestInfo: PPropInfo; DestParentObject: TObject;
      DestParentInfo: PPropInfo; NewCompIntf: IOTAComponent): Boolean;
    function FindComponentByOwner(OwnerComp: TComponent;
      const CompName: string): TComponent;
    procedure ExecutePropAssignWithConstList(NewCompIntf: IOTAComponent;
      DestPropInfo: TPropInfo; DestPropList: TPropList;
      Mappings: TCompRepMapList);
    procedure AssignPropValue(const ConstExpression: string;
      DestObject: TObject; DestInfo: PPropInfo);
    procedure ExecutePropAssignWithConst(NewCompIntf: IOTAComponent;
      DestPropInfo: TPropInfo; DestPropList: TPropList;
      const ConstExpression, DestExpression: string);
    procedure LogPropValue(CompIntf: IOTAComponent; PropInfo: TPropInfo;
      const APropExpression: string; LogOnlyIfDef: Boolean);
    function CanRead(Instance: TObject; APropInfo: PPropInfo): Boolean;
    function CanWrite(Instance: TObject; APropInfo: PPropInfo): Boolean;
  protected
    function GetIntegerPropByName(const PropName: string; DefaultVal: Integer = -1): Longint;
    property Stream: TMemoryStream read FStream;
  public
    // We deliberately hide the base virtual method
    constructor Create(Controller: TCompRepController;
      CompIntf: IOTAComponent); reintroduce;
    destructor Destroy; override;
    function IndexOfProp(const APropName: string): Integer;
    procedure SetComponent(CompIntf: IOTAComponent);
    function GetName: string;
    function GetLeft: Longint;
    function GetTop: Longint;
    function GetWidth: Longint;
    function GetHeight: Longint;
    property Prop[Index: Integer]: TPropInfo read GetPropInfo; default;
  end;

  TCompList = class(TOwnerList)
  private
    FController: TCompRepController;
    procedure EnumChildren(Param: Pointer; CompIntf: IOTAComponent; var Result: Boolean);
    function GetCompInfo(Index: Integer): TCompInfo;
    procedure ForEachChild(AProc: TCompInfoProc; AData: Pointer; var Stop: Boolean);
  protected
    function FindComponent(CompIntf: IOTAComponent): TCompInfo;
    procedure GetMatchingComponents(List: TStrings; const OldType: string);
    function Replace(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings; const NewType: string): Integer;
    function Search(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings; const NewType: string): Integer;
    procedure CommitReplace(OwnerLevel: Integer);
  public
    // We deliberately hide the base virtual method
    constructor Create(Controller: TCompRepController; CompIntf: IOTAComponent); reintroduce;
    property Children[Index: Integer]: TCompInfo read GetCompInfo; default;
  end;

  TCompInfo = class
  private
    FTypeName: string;
    FProperties: TPropList;
    FChildren: TCompList;
    FInterface: IOTAComponent;
    FController: TCompRepController;
    FNewInterface: IOTAComponent;
    FOldName: string;
    procedure SetInterface(NewIntf: IOTAComponent);
    procedure UpdateComponentParent(AParent: TCompInfo; AComponent: IOTAComponent);
    procedure UpdateComponentProperties(ADestComponent: IOTAComponent);
    procedure SetNewInterface(const Value: IOTAComponent);
    procedure CommitReplace(OwnerLevel: Integer);
    procedure RelinkComponents(OldCompInterface, NewCompInterface: IOTAComponent);
    procedure UpdateName;
    procedure AddReplacedChild(ACompInfo: TCompInfo; AData: Pointer; var Stop: Boolean);
    procedure RelinkByComponents(PropOwner: TComponent; OldObject, NewObject: TObject; IgnoredList: TList);
    procedure RelinkByControls(PropOwner: TControl; OldObject, NewObject: TObject; IgnoredList: TList);
    procedure RelinkByNotification(PropOwner: TObject; OldObject, NewObject: TObject; IgnoredList: TList);
    function GetAsNativeObject: TObject;
    procedure ReplaceLinks(PropOwner: TObject; OldObject, NewObject: TObject);
    function GetLatestInterface: IOTAComponent;
  protected
    function FindComponent(CompIntf: IOTAComponent): TCompInfo;
    function Search(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings; const NewType: string): Integer;
    function Replace(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings; const NewType: string): Integer;
    procedure CreateComponent(Parent: TCompInfo; FormEditor: IOTAFormEditor; const NewType: string);
    procedure GetMatchingComponents(List: TStrings; const OldType: string);
    procedure ForEachChild(AProc: TCompInfoProc; AData: Pointer; var Stop: Boolean);
  public
    constructor Create(Controller: TCompRepController; CompIntf: IOTAComponent);
    destructor Destroy; override;
    function GetName: string;
    property TypeName: string read FTypeName write FTypeName;
    property Children: TCompList read FChildren;
    property Properties: TPropList read FProperties;
    property ComponentInterface: IOTAComponent read FInterface write SetInterface;
    property NewComponentInterface: IOTAComponent read FNewInterface write SetNewInterface;
    property LatestComponentInterface: IOTAComponent read GetLatestInterface;
  end;

  TFormInfo = class
  private
    FCompInfo: TCompInfo;
    FInterface: IOTAFormEditor;
    FController: TCompRepController;
  protected
    function FindComponent(CompIntf: IOTAComponent): TCompInfo;
  public
    constructor Create(Controller: TCompRepController; FormEditor: IOTAFormEditor);
    destructor Destroy; override;
    procedure GetMatchingComponents(List: TStrings; const OldType: string);
    procedure GetSelectedComponents(List: TStrings; const OldType: string);
    function ReplaceComponents(List: TStrings; const NewType: string): Integer;
    property Component: TCompInfo read FCompInfo;
    property FormInterface: IOTAFormEditor read FInterface;
  end;

implementation

uses
  GX_OtaUtils, ComCtrls, Windows, Variants,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  {$IFDEF GX_VER170_up} WideStrings, {$ENDIF}
  GX_GenericUtils, GX_ReplaceCompUtils;

const
  BufferSize = 8192;

resourcestring
  sCannotCreate = 'Cannot create a new %s';

type
  EGetPropException = class(Exception);

{$IFDEF Use_HackedComponent}

  THackedComponent = class(TPersistent, IInterface, IInterfaceComponentReference)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
    FFreeNotifies: TList;
  end;

{$ENDIF}
  
{ TPropInfo }
constructor TPropInfo.Create(Controller: TCompRepController; Index: Integer;
  CompIntf: IOTAComponent);
resourcestring
  SReadErr =
  'Error while reading property %s from component %s';
begin
  inherited Create;
  FController := Controller;
  FPropName := CompIntf.GetPropName(Index);
  FPropType := CompIntf.GetPropType(Index);
  try
    CompIntf.GetPropValue(Index, FPropValue);
  // TWebBrowser raises exceptions reading properties like Resizable/StatusText
  except
    on E: Exception do begin
      FController.HandleException(E,
        Format(SReadErr, [CompIntf.GetPropName(Index), CompIntf.GetComponentType]));
    end;
  end
end;

{ Set the property value in the component, but only if it has a property
  of the same name and type. }
procedure TPropInfo.SetComponent(CompIntf: IOTAComponent);
resourcestring
  SSetError = 'Component error setting %s.%s: %s';
var
  NewType: TTypeKind;
  Comp: TPersistent;
  PropInfo: PPropInfo;
begin
  Comp := GxOtaGetNativePersistent(CompIntf);
  Assert(Assigned(Comp));
  PropInfo := GetPropInfo(Comp, Name);
  // If the property is read only, we should not try to set it
  if Assigned(PropInfo) and (not Assigned(PropInfo.SetProc)) then
    Exit;
  NewType := CompIntf.GetPropTypeByName(Name);
  if NewType = PropType then
  try
    CompIntf.SetPropByName(Name, FPropValue);
  except
    on E: Exception do
      raise Exception.CreateFmt(SSetError, [Comp.ClassName, Name, E.Message]);
  end;
end;

function TPropInfo.GetIntValue(Default: Integer): Integer;
begin
  if PropType in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
    Result := PropValue.IntegerValue
  else
    Result := Default
end;

function TPropInfo.GetStrValue(const Default: string): string;
begin
  if PropType in [tkString, tkLString] then
    Result := string(AnsiString(PropValue.PtrValue))
  else if PropType in [tkWString] then
    Result := WideString(PropValue.PtrValue)
  {$IFDEF GX_VER200_up}  
  else if PropType in [tkUString] then
    Result := string(PropValue.PtrValue)
  {$ENDIF}
  else
    Result := Default
end;

type
  TExposePersistent = class(TPersistent)
  public
    procedure DefineProperties(Filer: TFiler); override;
  end;

procedure TExposePersistent.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

{ TPropList }

constructor TPropList.Create(Controller: TCompRepController; CompIntf: IOTAComponent);
var
  i: Integer;
  Comp: TExposePersistent;
  Writer: TWriter;
  PropInfo: PPropInfo;
begin
  inherited Create;
  Comp := TExposePersistent(GxOtaGetNativePersistent(CompIntf));
  Assert(Assigned(Comp));
  FController := Controller;
  FLeft := -1;
  FTop := -1;
  for i := 0 to CompIntf.GetPropCount-1 do begin
    PropInfo := TypInfo.GetPropInfo(Comp, CompIntf.GetPropName(i));
    // If the property is write only, we should not try to get the value
    if Assigned(PropInfo) and (Assigned(PropInfo.GetProc)) then
    try
      // These two properties result in "Not implemented" type errors in several IDE releases (recently tested in XE4)
      if (CompIntf.GetComponentType = 'TWebBrowser') and (StringInArray(string(PropInfo.Name), ['StatusText', 'Resizable'])) then
        Continue;
      Add(TPropInfo.Create(FController, i, CompIntf));
    except on E: EGetPropException do
      // Ignore EGetPropException, since it is non-fatal
    end;
  end;
  FStream := TMemoryStream.Create;
  Writer := TWriter.Create(Stream, BufferSize);
  try
    Comp.DefineProperties(Writer);
  finally
    Writer.Free;
  end;
  // Manual support for Top/Left for non-visual components
  if ((Comp.InheritsFrom(TComponent)) and (not Comp.InheritsFrom(TControl))) then
  begin
    FLeft := LongRec(TComponent(Comp).DesignInfo).Lo;
    FTop := LongRec(TComponent(Comp).DesignInfo).Hi;
  end;
end;

destructor TPropList.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TPropList.GetPropInfo(Index: Integer): TPropInfo;
begin
  Result := TObject(Items[Index]) as TPropInfo;
end;

procedure TPropList.SetComponent(CompIntf: IOTAComponent);
begin
  Assert(Assigned(CompIntf));
  AssignPropsToCompByLoop(CompIntf);
  SetComponentDefProps(CompIntf);
end;

procedure TPropList.CopyComponentProps(OldCompIntf, NewCompIntf: IOTAComponent);
begin
  Assert(Assigned(OldCompIntf) and Assigned(NewCompIntf));
  AssignPropsToCompByMapping(OldCompIntf, NewCompIntf);
  SetComponentDefProps(NewCompIntf);
end;

procedure TPropList.AssignPropsToCompByLoop(CompIntf: IOTAComponent);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if not SameText(Prop[i].Name, 'Name') then
      Prop[i].SetComponent(CompIntf);
end;

// For each property: 1) find mappings, 2) execute assign
procedure TPropList.AssignPropsToCompByMapping(OldCompIntf, NewCompIntf: IOTAComponent);
var
  i: Integer;
  Mappings: TCompRepMapList;
  NewCompClassName, CompClassName: string;
  NewPropList: TPropList;
begin
  CompClassName := OldCompIntf.GetComponentType;
  NewCompClassName := NewCompIntf.GetComponentType;

  Mappings := TCompRepMapList.Create;
  try
    NewPropList := TPropList.Create(FController, NewCompIntf);
    try
      // Apply defined copy property mappings
      for i := 0 to Count-1 do
        if not SameText(Prop[i].Name, 'Name') then
        begin
          Mappings.Clear;
          FController.PrepareMappingForProp(CompClassName, Prop[i].Name, Mappings);
          ExecutePropAssignWithMapping(OldCompIntf, NewCompIntf, Prop[i], NewPropList, Mappings);
        end;

      // Apply defined constant mappings
      for i := 0 to NewPropList.Count-1 do
        if not SameText(NewPropList[i].Name, 'Name') then
        begin
          Mappings.Clear;
          FController.PrepareConstMappingForProp(NewCompClassName, NewPropList[i].Name, Mappings);
          ExecutePropAssignWithConstList(NewCompIntf, NewPropList[i], NewPropList, Mappings);
        end;
    finally
      NewPropList.Free;
    end;
  finally
    Mappings.Free;
  end;
end;

{
    - if direct mapping found then
        if Disabled - skip direct assignment
        else assign values
      else (mapping not found)
        assign values by the same name
    - execute all non-direct mappings if not disabled
}
procedure TPropList.ExecutePropAssignWithMapping(OldCompIntf: IOTAComponent; NewCompIntf: IOTAComponent;
  SourcePropInfo: TPropInfo; DestPropList: TPropList; Mappings: TCompRepMapList);
var
  DirectFound: Boolean;
  Idx, i: Integer;
  LogValuesForced: Boolean;
begin
  DirectFound := False;
  LogValuesForced := FController.IsLogValuesForced;

  for i := 0 to Mappings.Count-1 do
  begin
    if SameText(Mappings[i].SourcePropName, SourcePropInfo.Name) then
      DirectFound := True;

    if LogValuesForced or Mappings[i].LogValuesEnabled then
      LogPropValue(OldCompIntf, SourcePropInfo, Mappings[i].SourcePropName,
        Mappings[i].LogOnlyDefValuesEnabled and not LogValuesForced);

    if not Mappings[i].Disabled then
      ExecutePropAssign(OldCompIntf, NewCompIntf, SourcePropInfo, DestPropList,
        Mappings[i].SourcePropName, Mappings[i].DestPropName);
  end;

  if not DirectFound then
  begin
    if LogValuesForced then
      LogPropValue(OldCompIntf, SourcePropInfo, SourcePropInfo.Name, False);

    Idx := DestPropList.IndexOfProp(SourcePropInfo.Name);
    if Idx >= 0 then
      ExecutePropAssign(OldCompIntf, NewCompIntf, SourcePropInfo, DestPropList,
        SourcePropInfo.Name, SourcePropInfo.Name);
  end;
end;

procedure TPropList.ExecutePropAssignWithConstList(NewCompIntf: IOTAComponent;
  DestPropInfo: TPropInfo; DestPropList: TPropList; Mappings: TCompRepMapList);
var
  i: Integer;
begin
  for i := 0 to Mappings.Count-1 do
  begin
    if (not Mappings[i].UseConstValue) or Mappings[i].Disabled then
      Continue;

    ExecutePropAssignWithConst(NewCompIntf, DestPropInfo, DestPropList,
      Mappings[i].ConstValue, Mappings[i].DestPropName);
  end;
end;

function TPropList.CanRead(Instance: TObject; APropInfo: PPropInfo): Boolean;
begin
  Result := Assigned(APropInfo.GetProc);
end;

function TPropList.CanWrite(Instance: TObject; APropInfo: PPropInfo): Boolean;
begin
  Result := Assigned(APropInfo.SetProc);
  if (not Result) and (APropInfo^.PropType^.Kind in [tkClass]) then
    Result := (ClassLevel(TStrings, GetObjectPropClass(Instance, string(APropInfo.Name))) >= 0);
end;

procedure TPropList.ExecutePropAssign(OldCompIntf: IOTAComponent; NewCompIntf: IOTAComponent;
  SourcePropInfo: TPropInfo; DestPropList: TPropList; const SourceExpression, DestExpression: string);
resourcestring
  SSetError = 'Component error setting %s.%s';  
var
  SourceInfo, DestInfo, DestParentInfo: PPropInfo;
  SourceObj, DestObj, DestParentObj: TObject;
begin
  FindPropInfo(OldCompIntf, Self, SourceExpression, SourceInfo, SourceObj);
  FindPropInfoEx(NewCompIntf, DestPropList, DestExpression, DestInfo, DestObj,
    DestParentInfo, DestParentObj);

  if CheckCompNameAssign(SourceObj, SourceInfo,
    DestExpression, DestObj, DestInfo,
    DestParentObj, DestParentInfo, NewCompIntf)
  then
    Exit;

  if (SourceInfo = nil) or (DestInfo = nil) or
     (not CanRead(SourceObj, SourceInfo)) or
     (not CanWrite(DestObj, DestInfo))
  then
  begin
    {$IFOPT D+} SendDebugFmt('Property not assigned: %s.%s -> %s', [GetName, SourceExpression, DestExpression]); {$ENDIF}
    Exit;
  end;

  {$IFOPT D+} SendDebugFmt('Trying to assign: %s.%s -> %s', [GetName, SourceExpression, DestExpression]); {$ENDIF}
  try
    CopyPropValue(SourceObj, SourceInfo, DestObj, DestInfo);
  except
    on E: Exception do begin
      FController.HandleException(E,
        Format(SSetError,
          [DestObj.ClassName, DestInfo.Name]));
    end;
  end;
end;

procedure TPropList.LogPropValue(CompIntf: IOTAComponent;
  PropInfo: TPropInfo; const APropExpression: string;
  LogOnlyIfDef: Boolean);
resourcestring
  SReadCtx = 'Retrieving property value: %s.%s.%s';
  SSaveCtx = 'Saving property value: %s.%s.%s to log';
  SUnknownExpr = 'Unknown property expression: %s.%s';
  SPropValue = 'Property value: %s.%s.%s = %s';
var
  ValueObj: TObject;
  ValuePropInfo: PPropInfo;
  Context: string;
  PropValue: string;
begin
  try
    Context := Format(SReadCtx,
          [CompIntf.GetComponentType, GetName, APropExpression]);

    FindPropInfo(CompIntf, Self, APropExpression, ValuePropInfo, ValueObj);
    if (ValuePropInfo=nil) or (ValueObj=nil) then
      raise Exception.Create(Format(SUnknownExpr,
        [CompIntf.GetComponentType, APropExpression]));

    Context := Format(SSaveCtx,
      [CompIntf.GetComponentType, GetName, APropExpression]);

    if (ValuePropInfo^.GetProc<>nil) and
       ((not LogOnlyIfDef) or (IsDefaultPropertyValue(ValueObj, ValuePropInfo)))
    then
    begin
      if ValuePropInfo^.PropType^.Kind in [tkClass, tkMethod, tkInterface] then
      begin
        if IsDefaultPropertyValue(ValueObj, ValuePropInfo) then
          PropValue := '[ default / nil ]'
        else
          PropValue := '[ assigned ]';
      end
      else
      begin
        if not IsDefaultPropertyValue(ValueObj, ValuePropInfo) then
          PropValue := '[ changed ]'
        else
          PropValue := '';
        PropValue := VarToStr(GetPropValue(ValueObj, string(ValuePropInfo.Name))) + ' '+
          PropValue;
      end;
      FController.LogMsg(Format(SPropValue,
        [CompIntf.GetComponentType, GetName, ValuePropInfo.Name, PropValue]));
    end;

  except
    on E: Exception do
      FController.HandleException(E, Context);
  end;
end;

procedure TPropList.ExecutePropAssignWithConst(NewCompIntf: IOTAComponent;
  DestPropInfo: TPropInfo; DestPropList: TPropList;
  const ConstExpression, DestExpression: string);
resourcestring
  SSetError = 'Component error setting %s.%s';  
var
  DestInfo: PPropInfo;
  DestObj: TObject;
begin
  FindPropInfo(NewCompIntf, DestPropList, DestExpression, DestInfo, DestObj);

  if (DestInfo = nil) or (not Assigned(DestInfo.SetProc))
  then
  begin
    {$IFOPT D+} SendDebug(Format('Property not assigned: %s.%s -> %s', [
      GetName, ConstExpression, DestExpression])); {$ENDIF}
    Exit;
  end;

  {$IFOPT D+} SendDebug(Format('Trying to assign: %s.%s -> %s', [
      GetName, ConstExpression, DestExpression])); {$ENDIF}

  try
    AssignPropValue(ConstExpression, DestObj, DestInfo);
  except
    on E: Exception do begin
      FController.HandleException(E,
        Format(SSetError,
          [DestObj.ClassName, DestInfo.Name]));
    end;
  end;
end;

function TPropList.CheckCompNameAssign(SourceObject: TObject; SourceInfo: PPropInfo;
  const DestExpression: string; DestObject: TObject; DestInfo: PPropInfo;
  DestParentObject: TObject; DestParentInfo: PPropInfo;
  NewCompIntf: IOTAComponent): Boolean;
resourcestring
  SCompNF = 'Component not found: "%s"';
  SCompNameAssignCtx = 'Checking component name assign %s.%s';
var
  DestPropOwner: TComponent;
  NewDestComponent: TComponent;
  PropTail, PropName, ParentPropName: string;
  CompName: string;
  DestClass: string;
  DestName: string;
begin
  Result := False;
  try
    DestPropOwner := GxOtaGetNativeComponent(NewCompIntf);
    if (DestParentObject = nil) or (DestParentInfo = nil) then
      Exit;

    if not (SourceInfo.PropType^.Kind in [tkString, tkLString {$IFDEF GX_VER200_up}, tkUString{$ENDIF}]) then
      Exit;

    if ClassLevel(GetObjectPropClass(DestParentObject, string(DestParentInfo.Name)), TComponent) < 0 then
      Exit;

    PropTail := ReverseString(DestExpression);
    PropName := ReverseString(ExtractToken(PropTail, '.'));
    ParentPropName := ReverseString(ExtractToken(PropTail, '.'));

    if (not SameText(PropName, 'Name')) or
       (not SameText(ParentPropName, string(DestParentInfo.Name)))
    then
      Exit;

    CompName := GetStrProp(SourceObject, string(SourceInfo.Name));

    if Trim(CompName)<>'' then
    begin
      NewDestComponent := FindComponentByOwner(DestPropOwner, CompName);
      if NewDestComponent = nil then
        raise Exception.Create(Format(SCompNF, [CompName]));
    end
    else
      NewDestComponent := nil;

    SetObjectProp(DestParentObject, string(DestParentInfo.Name), NewDestComponent);
    Result := True;

  except
    on E: Exception do begin
      DestClass := '';
      DestName := '';
      if Assigned(DestObject) then
        DestClass := DestObject.ClassName;
      if Assigned(DestInfo) then
        DestName := string(DestInfo.Name);

      FController.HandleException(E, Format(SCompNameAssignCtx, [DestClass, DestName]));
    end;
  end;
end;

function TPropList.FindComponentByOwner(OwnerComp: TComponent; const CompName: string): TComponent;
begin
  if OwnerComp<>nil then
  begin
    Result := OwnerComp.FindComponent(CompName);
    if Result = nil then
      Result := FindComponentByOwner(OwnerComp.Owner, CompName);
  end
  else
    Result := nil;
end;

procedure TPropList.CopyPropValue(SourceObject: TObject; SourceInfo: PPropInfo;
  DestObject: TObject; DestInfo: PPropInfo);
resourcestring
  SRODest = 'Invalid destination - read-only: %s.%s';
var
  SourceObj: TObject;
  DestObj: TObject;
  SourceType, DestType: TTypeKind;
begin
  SourceType := SourceInfo.PropType^.Kind;
  DestType := DestInfo.PropType^.Kind;
  // Same property types
  if SourceType = DestType then
  begin
    case SourceType of
      tkClass:
      begin
        SourceObj := GetObjectProp(SourceObject, string(SourceInfo.Name));
        DestObj := GetObjectProp(DestObject, string(DestInfo.Name));

        if DestInfo.SetProc = nil then
        begin // Setter is nil - only TStrings is handled here
          if (SourceObj is TStrings) and (DestObj is TStrings) then // Both TStrings
            (DestObj as TStrings).Text := (SourceObj as TStrings).Text
          else // Impossible case (no setter & not TStrings)
            raise Exception.Create(Format(SRODest, [DestObject.ClassName, DestInfo.Name]));
        end
        else // Setter is available
        begin
          {$IFDEF GX_VER170_up} // Tested as working in BDS 2006
          if (SourceObj is TStrings) and (DestObj is TWideStrings) then // TStrings -> TWideStrings
            (DestObj as TWideStrings).Text := (SourceObj as TStrings).Text
          else if (SourceObj is TWideStrings) and (DestObj is TStrings) then // TWideStrings -> TStrings
            (DestObj as TStrings).Text := (SourceObj as TWideStrings).Text
          else
          {$ENDIF GX_VER170_up}
          begin
            // TTouchManager assignments cause the new component to be corrupt when the old component is deleted in 2010/XE
            if (SourceInfo.Name = 'Touch') and (SourceInfo.PropType^.Name = 'TTouchManager') then
              Exit;
            SetObjectProp(DestObject, string(DestInfo.Name), GetObjectProp(SourceObject, string(SourceInfo.Name)));
          end;
        end;
      end; // tkClass
      tkMethod:
        SetMethodProp(DestObject, string(DestInfo.Name), GetMethodProp(SourceObject, string(SourceInfo.Name)));
      else
        SetPropValue(DestObject, string(DestInfo.Name), GetPropValue(SourceObject, string(SourceInfo.Name)));
    end // case
  end // Same type
  else // Different property types
  begin
    // Different types but both can be a variant
    if (SourceType <> tkClass) and (DestType <> tkClass) then
      SetPropValue(DestObject, string(DestInfo.Name), GetPropValue(SourceObject, string(SourceInfo.Name)))
    else // Class on one side only
    begin
      // Test for TStrings on source side
      if (SourceType = tkClass) then
      begin
        SourceObj := GetObjectProp(SourceObject, string(SourceInfo.Name));
        if SourceObj is TStrings then
          SetPropValue(DestObject, string(DestInfo.Name), (SourceObj as TStrings).Text)
        {$IFDEF GX_VER170_up}
        else if SourceObj is TWideStrings then
          SetPropValue(DestObject, string(DestInfo.Name), (SourceObj as TWideStrings).Text)
        {$ENDIF GX_VER170_up}
      end
      // Test for T[Wide]Strings on dest side
      else if DestType = tkClass then
      begin
        DestObj := GetObjectProp(DestObject, string(DestInfo.Name));
        if DestObj is TStrings then
          (DestObj as TStrings).Text := GetPropValue(SourceObject, string(SourceInfo.Name))
        {$IFDEF GX_VER170_up}
        else if DestObj is TWideStrings then
          (DestObj as TWideStrings).Text := GetPropValue(SourceObject, string(SourceInfo.Name))
        {$ENDIF GX_VER170_up}
      end;
    end;
  end;
end;

procedure TPropList.AssignPropValue(const ConstExpression: string;
  DestObject: TObject; DestInfo: PPropInfo);
resourcestring
  SAssignFailedForDestClass = 'Assigning const failed - wrong destination class: %s.%s';
var
  SList: TStrings;
begin
  if not (DestInfo.PropType^.Kind in [tkClass])
  then
    SetPropValue(DestObject, string(DestInfo.Name), ConstExpression)
  else // Class on dest side
  begin
    // Test for TStrings on dest side
    if (DestInfo.PropType^.Kind in [tkClass]) and
       (ClassLevel(TStrings, GetObjectPropClass(DestObject, string(DestInfo.Name))) >= 0)
    then
    begin
      SList := GetObjectProp(DestObject, string(DestInfo.Name)) as TStrings;
      if Assigned(SList) then
        SList.Text := ConstExpression;
    end
    else
      raise Exception.Create(Format(SAssignFailedForDestClass, [DestObject.ClassName, DestInfo.Name]));
  end;
end;

procedure TPropList.FindPropInfo(CompIntf: IOTAComponent; PropList: TPropList;
  const PropExpression: string; var VPropInfo: PPropInfo; var Obj: TObject);
var
  TempPropInfo: PPropInfo;
  TempObj: TObject;
begin
  FindPropInfoEx(CompIntf, PropList, PropExpression, VPropInfo, Obj,
    TempPropInfo, TempObj);
end;

procedure TPropList.FindPropInfoEx(CompIntf: IOTAComponent; PropList: TPropList;
  const PropExpression: string; var VPropInfo: PPropInfo; var Obj: TObject;
  var VParentPropInfo: PPropInfo; var ParentObj: TObject);
var
  PropName, PropTail: string;
  PersObject: TPersistent;
  RawObject: TObject;
begin
  PropTail := PropExpression;
  PropName := ExtractToken(PropTail, '.');
  PersObject := GxOtaGetNativePersistent(CompIntf);
  VParentPropInfo := nil;
  ParentObj := nil;
  Obj := nil;

  VPropInfo := TypInfo.GetPropInfo(PersObject, PropName);

  while (VPropInfo <> nil) and (PropTail <> '') do
  begin
    // save parent
    VParentPropInfo := VPropInfo;
    ParentObj := PersObject;

    if VPropInfo.PropType^.Kind <> tkClass then
    begin
      VPropInfo := nil;
      Break;
    end;

    if PersObject<>nil then
      RawObject := GetObjectProp(PersObject, VPropInfo)
    else
      RawObject := nil;

    if not (RawObject is TPersistent) then
    begin
      VPropInfo := nil;
      Break;
    end;

    // assign new output values
    PropName := ExtractToken(PropTail, '.');
    VPropInfo := TypInfo.GetPropInfo(
      GetObjectPropClass(PersObject, string(VPropInfo.Name)), PropName);
    PersObject := RawObject as TPersistent;
  end;

  if VPropInfo<>nil then
    Obj := PersObject
  else
    Obj := nil;

end;

procedure TPropList.SetComponentDefProps(CompIntf: IOTAComponent);
resourcestring
  SReadErr =
  'Reading properties using DefineProperties for %s';
var
  Reader: TReader;
  Comp: TExposePersistent;
begin
  try
    Comp := TExposePersistent(GxOtaGetNativePersistent(CompIntf));
    Stream.Position := 0;
    Reader := TReader.Create(Stream, BufferSize);
    try
      Comp.DefineProperties(Reader);
    finally
      Reader.Free;
    end;
  except
    on E: Exception do begin
      FController.HandleException(E,
        Format(SReadErr, [CompIntf.GetComponentType]));
    end;
  end
end;

// Manually set Top/Left for non-visual TComponents
procedure TPropList.SetComponentPosition(ADestComponent: IOTAComponent);
var
  NewComp: TComponent;
begin
  if FLeft <> -1 then
  begin
    NewComp := GxOtaGetNativeComponent(ADestComponent);
    if Assigned(NewComp) then
      NewComp.DesignInfo := MakeLong(FLeft, FTop);
  end;
end;

function TPropList.IndexOfProp(const APropName: string): Integer;
begin
  Result := Count-1;
  while Result >= 0 do
    if SameText(Self[Result].Name, APropName) then
      Break
    else
      Dec(Result);
end;

{ Lookup a property by name, and if it is an Integer, then
  return the Integer value. Otherwise, return default value. }
function TPropList.GetIntegerPropByName(const PropName: string; DefaultVal: Integer): Longint;
var
  Idx: Integer;
begin
  Result := DefaultVal;  
  Idx := IndexOfProp(PropName);
  if Idx >= 0 then
    Result := Prop[Idx].GetIntValue(DefaultVal);
end;

function TPropList.GetLeft: Longint;
begin
  Result := GetIntegerPropByName('Left', FLeft)
end;

function TPropList.GetTop: Longint;
begin
  Result := GetIntegerPropByName('Top', FTop)
end;

function TPropList.GetWidth: Longint;
begin
  Result := GetIntegerPropByName('Width', NonVisibleDefSize)
end;

function TPropList.GetHeight: Longint;
begin
  Result := GetIntegerPropByName('Height', NonVisibleDefSize)
end;

{ Get the value of the Name property. }
function TPropList.GetName: string;
var
  Idx: Integer;
begin
  Result := '';

  Idx := IndexOfProp('Name');
  if Idx >= 0 then
    Result := Prop[Idx].GetStrValue('');
end;


{ TCompList }

procedure TCompList.EnumChildren(Param: Pointer; CompIntf: IOTAComponent; var Result: Boolean);
begin
  Self.Add(TCompInfo.Create(FController, CompIntf));
  Result := True;
end;

constructor TCompList.Create(Controller: TCompRepController; CompIntf: IOTAComponent);
begin
  inherited Create;
  FController := Controller;
  CompIntf.GetChildren(nil, EnumChildren);
end;

function TCompList.GetCompInfo(Index: Integer): TCompInfo;
begin
  Result := TObject(Items[Index]) as TCompInfo;
end;

{ Look for a component represented by CompIntf. Return nil for not found. }
function TCompList.FindComponent(CompIntf: IOTAComponent): TCompInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    Result := Children[i].FindComponent(CompIntf);
    if Result <> nil then
      Exit;
  end;
end;

procedure TCompList.ForEachChild(AProc: TCompInfoProc; AData: Pointer; var Stop: Boolean);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    AProc(Children[i], AData, Stop);
    if Stop then Break;

    Children[i].ForEachChild(AProc, AData, Stop);
  end;
end;

function TCompList.Replace(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings;
  const NewType: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    Inc(Result, Children[i].Replace(Parent, FormEditor, List, NewType));
end;

function TCompList.Search(Parent: TCompInfo; FormEditor: IOTAFormEditor; List: TStrings; const NewType: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    Inc(Result, Children[i].Search(Parent, FormEditor, List, NewType));
end;

{ Add to List all components whose type is OldType. }
procedure TCompList.GetMatchingComponents(List: TStrings; const OldType: string);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Children[i].GetMatchingComponents(List, OldType);
end;

procedure TCompList.CommitReplace(OwnerLevel: Integer);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Children[i].CommitReplace(OwnerLevel);
end;

{ TCompInfo }
constructor TCompInfo.Create(Controller: TCompRepController; CompIntf: IOTAComponent);
begin
  inherited Create;
  FController := Controller;
  FChildren := TCompList.Create(FController, CompIntf);
  FProperties := TPropList.Create(FController, CompIntf);
  FTypeName := CompIntf.GetComponentType;

  FInterface := CompIntf;
end;

destructor TCompInfo.Destroy;
begin
  Children.Free;
  Properties.Free;

  inherited Destroy;
end;

{ Change the component interface reference. Take care now to free
  the old interface until it is safe to do so. }
procedure TCompInfo.SetInterface(NewIntf: IOTAComponent);
begin
  FInterface := NewIntf;
end;

{ Create a new component of type NewType, duplicating the old component's
  properties. }
procedure TCompInfo.CreateComponent(Parent: TCompInfo; FormEditor: IOTAFormEditor; const NewType: string);
resourcestring
  SUpdParent = 'Updating Parent';
var
  NewComponent: IOTAComponent;
  OldGroup: TPersistentClass;
begin
  if GxOtaActiveDesignerIsVCL then
    OldGroup := ActivateClassGroup(Controls.TControl)
  else
    OldGroup := nil; // Not necessary?: ActivateClassGroup(QControls.TControl);
  try

    FOldName := GetName;

    Assert(Assigned(Parent), 'Parent not assigned');
    Assert(Assigned(Parent.LatestComponentInterface), 'Parent.LatestComponentInterface not assigned');
    with Properties do
    try
      {$IFOPT D+} SendDebug(Format('Creating with pos: (Left=%d, Top=%d', [GetLeft, GetTop])); {$ENDIF}
      // If this starts crashing try IDesigner.CreateComponent
      NewComponent := FormEditor.CreateComponent(Parent.LatestComponentInterface,
        NewType, GetLeft, GetTop, GetWidth, GetHeight);

    except on E: Exception do
      raise Exception.Create('Error calling IOTAFormEditor.CreateComponent: ' + E.Message);
    end;
  finally
    if OldGroup <> nil then
      ActivateClassGroup(OldGroup);
  end;

  if NewComponent = nil then
    raise Exception.CreateFmt(sCannotCreate, [NewType]);

  NewComponentInterface := NewComponent;
  UpdateComponentProperties(NewComponent);

  try // Update prent
    UpdateComponentParent(Parent, NewComponent);
  except
    on E: Exception do
      FController.HandleException(E, SUpdParent);
  end;
end;

procedure TCompInfo.UpdateComponentParent(AParent: TCompInfo; AComponent: IOTAComponent);
var
  OldObject, NewObject: TObject;
  OldParentObject, NewParentObject: TObject;
begin
  OldObject := GxOtaGetNativePersistent(ComponentInterface);
  OldParentObject := GxOtaGetNativePersistent(AParent.ComponentInterface);

  NewObject := GxOtaGetNativePersistent(AComponent);
  NewParentObject := GxOtaGetNativePersistent(AParent.LatestComponentInterface);

  if (OldObject is TComponent)
     and (TFriendComponent(OldObject).GetParentComponent = OldParentObject)
     and (NewObject is TComponent) and (NewParentObject is TComponent)
     and (TFriendComponent(NewObject).GetParentComponent = nil)
  then
    TFriendComponent(NewObject).SetParentComponent(TComponent(NewParentObject));

  if (NewObject is TTabSheet) and (NewParentObject is TPageControl) then
    TTabSheet(NewObject).PageControl := TPageControl(NewParentObject);

  if (OldObject is TWinControl) and (OldParentObject is TWinControl) and
    (TWinControl(OldObject).Parent = OldParentObject) and
    (NewObject is TWinControl) and (NewParentObject is TWinControl)
  then
    TWinControl(NewObject).Parent := TWinControl(NewParentObject);
end;

procedure TCompInfo.UpdateComponentProperties(ADestComponent: IOTAComponent);
begin
  Properties.CopyComponentProps(ComponentInterface, ADestComponent);
  Properties.SetComponentPosition(ADestComponent);
end;

procedure TCompInfo.UpdateName;
var
  Comp: TComponent;
begin
  Comp := GxOtaGetNativeComponent(ComponentInterface);
  if Assigned(Comp) then
    Comp.Name := FOldName;
end;

{ Return the component's name, that is, the value of its Name property. }
function TCompInfo.GetName: string;
begin
  Result := Properties.GetName;
end;

{ Search for the component whose interface is CompIntf. Return nil
  for not found. To search for an interface, compare component handles,
  which are unique among all existing components. }
function TCompInfo.FindComponent(CompIntf: IOTAComponent): TCompInfo;
begin
  if GxOtaComponentsAreEqual(CompIntf, ComponentInterface) then
    Result := Self
  else
    Result := Children.FindComponent(CompIntf);
end;

{ Find all components whose type is OldType; add the names of the
  matching components to List. }
procedure TCompInfo.GetMatchingComponents(List: TStrings; const OldType: string);
begin
  Children.GetMatchingComponents(List, OldType);
  if SameText(TypeName, OldType) then
    List.Add(GetName);
end;

{ Create a component and its children. If the component is named in List,
  use NewType for its type. }
function TCompInfo.Replace(Parent: TCompInfo; FormEditor: IOTAFormEditor;
  List: TStrings; const NewType: string): Integer;
var
  Index: Integer;
  Count: Integer;
  StackName: string;
begin
  StackName := GetName;
  FController.SignalStackBegin(StackName);
  try
    Index := List.IndexOf(StackName);
    if Index < 0 then
    begin
      { Not in list, so use old type name. }
      CreateComponent(Parent, FormEditor, TypeName);
      Count := 0;
    end
    else
    begin
      { Create component with new type name. }
      List.Delete(Index);
      CreateComponent(Parent, FormEditor, NewType);
      Count := 1;
    end;

    { Recursively recreate child components. }
    Result := Children.Replace(Self, FormEditor, List, NewType) + Count;
  finally
    FController.SignalStackEnd(StackName);
  end;
end;

{ If this component's name is in List, delete it and recreate the component
  using type, NewType, and recursively recreate its children. If any children
  are in List, recreate them with the NewType. If this component is not in
  the list, search its children. }
function TCompInfo.Search(Parent: TCompInfo; FormEditor: IOTAFormEditor;
  List: TStrings; const NewType: string): Integer;

  function GetStringListItem(List: TStrings; Index: Integer): string;
  begin
    if (Index < List.Count) and (Index > -1) then
      Result := List[Index]
    else
      Result := '';
  end;

resourcestring
  AncestorError = 'There are known bugs in the IDE that cause crashes trying '+
    'to replace components on inherited froms.  The component %s can not be '+
    'replaced, since it is on an inherited form.  Operation aborted.';
  SObjectProc = 'Object processing';
  SReplaceCompd = 'Component replaced successfully: %s - > %s:%s';
var
  Index: Integer;
  CompObject: TObject;
  Component: TComponent;
  ListName, StackName: string;
  OldClassName, NewClassName: string;
begin
  Result := 0;
  StackName := GetName;
  OldClassName := '';
  NewClassName := '';
  FController.SignalStackBegin(StackName);
  try
    Index := List.IndexOf(StackName);
    if Index < 0 then
      { Not in List, so search the children. }
      Result := Children.Search(Self, FormEditor, List, NewType)
    else
    begin
      { Found a component to replace. Delete the old one, create a new one,
        and recreate all of its children. }
      CompObject := nil;
      ListName := GetStringListItem(List, Index);
      try // finally object end
        try // except object processing
          try // except object begin
            List.Delete(Index);
            CompObject := GxOtaGetNativeObject(ComponentInterface);

            if Assigned(CompObject) then
              OldClassName := CompObject.ClassName;

            if Assigned(CompObject) and (CompObject is TComponent) then begin
              Component := CompObject as TComponent;
              if csAncestor in Component.ComponentState then
                raise Exception.CreateFmt(AncestorError, [Component.Name]);
            end;
            FController.SignalObjectBegin(ListName, CompObject);

          except
            on E: Exception do
            begin
              FController.SignalObjectBegin(ListName, CompObject);
              raise;
            end;
          end;

          // Create a new one & assign properties
          CreateComponent(Parent, FormEditor, NewType);

          // Recreate child objects
          Result := Children.Replace(Self, FormEditor, List, NewType) + 1;

        finally
          FController.SignalObjectEnd(ListName, CompObject);
        end;

        CompObject := GxOtaGetNativeObject(NewComponentInterface);
        try
          if Assigned(CompObject) then
            NewClassName := CompObject.ClassName;

          FController.SignalObjectBegin(ListName, CompObject);
          CommitReplace(0);
          FController.LogMsg(Format(SReplaceCompd, [OldClassName, NewClassName, ListName]));
        finally
          FController.SignalObjectEnd(ListName, CompObject);
        end;

        except
          on E: Exception do
            FController.HandleException(E, SObjectProc);
        end;
    end
  finally
    FController.SignalStackEnd(StackName);
  end;
end;

procedure TCompInfo.CommitReplace(OwnerLevel: Integer);
resourcestring
  SObjectDel = 'Object deletion';
  SObjectDelFailed = 'Object deletion failed';
  SObjectIntfReplace = 'Interface replacing';
  SChildrenIntfReplace = 'Children interface replacing';
  SCompRenaming = 'Component renaming';
begin
  FController.Root.RelinkComponents(ComponentInterface, NewComponentInterface);

  if OwnerLevel = 0 then
  try // delete
    // delete old one
    if Assigned(ComponentInterface) then
    begin
      if not ComponentInterface.Delete then
        raise Exception.Create(SObjectDelFailed);
    end;

  except
    on E: Exception do
      FController.HandleException(E, SObjectDel);
  end;

  try
    ComponentInterface := NewComponentInterface;
    NewComponentInterface := nil;
  except
    on E: Exception do
      FController.HandleException(E, SObjectIntfReplace);
  end;

  try
    Children.CommitReplace(OwnerLevel+1);
  except
    on E: Exception do
      FController.HandleException(E, SChildrenIntfReplace);
  end;

  try
    UpdateName;
  except
    on E: Exception do
      FController.HandleException(E, SCompRenaming);
  end;
end;

procedure TCompInfo.RelinkComponents(OldCompInterface: IOTAComponent;
  NewCompInterface: IOTAComponent);
resourcestring
  SRelinkErrControls = 'Component error while relinking using Controls';
  SRelinkErrComps = 'Component error while relinking using Components';
  SRelinkErrNotif = 'Component error while relinking using Notification';
var
  OldObject, NewObject: TObject;
  Stop: Boolean;
  ChildList: TList;
  NativeObject: TObject;
begin
  OldObject := GxOtaGetNativeObject(OldCompInterface);
  NewObject := GxOtaGetNativeObject(NewCompInterface);
  ChildList := TList.Create;
  try
    Stop := False;
    AddReplacedChild(Self, ChildList, Stop);
    ForEachChild(AddReplacedChild, ChildList, Stop);

    NativeObject := GetAsNativeObject;

    try
      if NativeObject is TComponent then
        RelinkByComponents(TComponent(NativeObject), OldObject, NewObject, ChildList);
    except
      on E: Exception do
        FController.HandleException(E, SRelinkErrComps);
    end;

    try
      if NativeObject is TWinControl then
        RelinkByControls(TWinControl(NativeObject), OldObject, NewObject, ChildList);
    except
      on E: Exception do
        FController.HandleException(E, SRelinkErrControls);
    end;

    try
      if NativeObject is TComponent then
        RelinkByNotification(NativeObject, OldObject, NewObject, ChildList);
    except
      on E: Exception do
        FController.HandleException(E, SRelinkErrNotif);
    end;

  finally
    ChildList.Free;
  end;
end;

function TCompInfo.GetAsNativeObject: TObject;
begin
  if Assigned(NewComponentInterface) then
    Result := GxOtaGetNativeObject(NewComponentInterface)
  else
    Result := GxOtaGetNativeObject(ComponentInterface);
end;

procedure TCompInfo.ReplaceLinks(PropOwner: TObject;
  OldObject, NewObject: TObject);
var
  PropNames: TStringList;
  i: Integer;
  ObjPtr: TObject;
begin
  if PropOwner = nil then
    Exit;
    
  PropNames := TStringList.Create;
  try
    GetPropertyNames(PropOwner.ClassName, PropNames);

    for i := 0 to PropNames.Count-1 do
    begin
      if IsPublishedProp(PropOwner, PropNames[i]) and PropIsType(PropOwner, PropNames[i], tkClass) then
      begin
        ObjPtr := GetObjectProp(PropOwner, PropNames[i]);
        if ObjPtr = OldObject then
          SetObjectProp(PropOwner, PropNames[i], NewObject);
      end;
    end;

  finally
    PropNames.Free;
  end;
end;

procedure TCompInfo.RelinkByComponents(PropOwner: TComponent;
  OldObject, NewObject: TObject; IgnoredList: TList);
var
  i: Integer;
begin
  if IgnoredList.IndexOf(PropOwner)<0 then
    ReplaceLinks(PropOwner, OldObject, NewObject);

  for i := 0 to PropOwner.ComponentCount-1 do
    RelinkByComponents(PropOwner.Components[i], OldObject, NewObject, IgnoredList);
end;

procedure TCompInfo.RelinkByControls(PropOwner: TControl;
  OldObject, NewObject: TObject; IgnoredList: TList);
var
  WinOwner: TWinControl;
  i: Integer;
begin
  if IgnoredList.IndexOf(PropOwner)<0 then
    ReplaceLinks(PropOwner, OldObject, NewObject);

  if PropOwner is TWinControl then
  begin
    WinOwner := TWinControl(PropOwner);
    for i := 0 to WinOwner.ControlCount-1 do
      RelinkByControls(WinOwner.Controls[i], OldObject, NewObject, IgnoredList);
  end;
end;

procedure TCompInfo.RelinkByNotification(PropOwner: TObject;
  OldObject, NewObject: TObject; IgnoredList: TList);
var
  FreeNotifies: TList;
  i: Integer;
begin
  FreeNotifies := nil;
{$IFDEF Use_HackedComponent}
  if PropOwner is TComponent then
    FreeNotifies := THackedComponent(PropOwner).FreeNotifies;
{$ENDIF Use_HackedComponent}

  if IgnoredList.IndexOf(PropOwner)<0 then
    ReplaceLinks(PropOwner, OldObject, NewObject);

  if Assigned(FreeNotifies) then
    for i := 0 to FreeNotifies.Count-1 do
      RelinkByNotification(FreeNotifies[i], OldObject, NewObject, IgnoredList);
end;

procedure TCompInfo.AddReplacedChild(ACompInfo: TCompInfo; AData: Pointer; var Stop: Boolean);
var
  OldObject: TObject;
begin
  OldObject := GxOtaGetNativeObject(ACompInfo.ComponentInterface);

  if Assigned(ACompInfo.NewComponentInterface) and Assigned(OldObject) then
    (TList(AData)).Add(OldObject);
end;

procedure TCompInfo.ForEachChild(AProc: TCompInfoProc; AData: Pointer; var Stop: Boolean);
begin
  FChildren.ForEachChild(AProc, AData, Stop);
end;

procedure TCompInfo.SetNewInterface(const Value: IOTAComponent);
begin
  FNewInterface := Value;
end;

function TCompInfo.GetLatestInterface: IOTAComponent;
begin
  if Assigned(FNewInterface) then
    Result := FNewInterface
  else
    Result := FInterface;  
end;

{ TFormInfo }

constructor TFormInfo.Create(Controller: TCompRepController; FormEditor: IOTAFormEditor);
var
  CompIntf: IOTAComponent;
begin
  inherited Create;

  FController := Controller;
  FInterface := FormEditor;
  CompIntf := FormInterface.GetRootComponent;

  FCompInfo := TCompInfo.Create(FController, CompIntf);
  FController.Root := FCompInfo;
end;

destructor TFormInfo.Destroy;
begin
  FCompInfo.Free;

  inherited Destroy;
end;

{ Look up the component represented by CompIntf. }
function TFormInfo.FindComponent(CompIntf: IOTAComponent): TCompInfo;
begin
  Result := Component.FindComponent(CompIntf);
end;

{ Fill List with the names of all components whose type is OldType. }
procedure TFormInfo.GetMatchingComponents(List: TStrings; const OldType: string);
begin
  Component.GetMatchingComponents(List, OldType);
end;

{ Examine all selected components, and add to the list any components
  whose type is OldType. Also search the children of selected components. }
procedure TFormInfo.GetSelectedComponents(List: TStrings; const OldType: string);
var
  i: Integer;
  Info: TCompInfo;
  CompIntf: IOTAComponent;
begin
  for i := 0 to FormInterface.GetSelCount-1 do
  begin
    CompIntf := FormInterface.GetSelComponent(i);
    Info := FindComponent(CompIntf);
    Info.GetMatchingComponents(List, OldType);
  end;
end;

{ Replace all the components whose names are in List. Replace them
  with new components of type NewType. Return the number of
  replacements performed. }
function TFormInfo.ReplaceComponents(List: TStrings; const NewType: string): Integer;
begin
  { Start by selecting the form, so when parent = nil, the new components
    will be children of the form, not the current selection. }
  Component.ComponentInterface.Select(False);
  Result := Component.Search(nil, FormInterface, List, NewType);
end;

end.

