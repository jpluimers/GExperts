// Original Author: Piotr Likus
// Replace Components data storage objects
unit GX_ReplaceCompData;

interface

uses
  Classes, SysUtils, Contnrs,
  OmniXML,
  Gx_GenericClasses;

const
  // Default values for mapping flags
  CompRepDefDisabled = False;
  CompRepDefBiDirEnabled = True;
  CompRepDefUseConstValue = False;
  CompRepDefLogValuesEnabled = False;
  CompRepDefLogOnlyDefValuesEnabled = False;

type
  TCompRepMapList = class;
  TCompRepMapItem = class;

  TCompRepMapGroupItem = class
  private
    FName: string;
    FItems: TCompRepMapList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const SourceClassName, DestClassName: string;
      const SourcePropName, DestPropName: string;
      DisabledFlag, BiDirFlag: Boolean;
      UseConstValue: Boolean; ConstValue: string;
      LogValuesEnabled: Boolean;
      LogOnlyDefValuesEnabled: Boolean): TCompRepMapItem;

    procedure AddItem(Item: TCompRepMapItem);
    procedure ExtractItem(Item: TCompRepMapItem);

    property Name: string read FName write FName;
    property Items: TCompRepMapList read FItems;
  end;

  TCompRepMapGroupList = class(TGxObjectDictionary)
  private
    function GetItem(Index: Integer): TCompRepMapGroupItem;
  public
    function Add(const MapGroupName: string): TCompRepMapGroupItem;
    property Items[Index: Integer]: TCompRepMapGroupItem read GetItem; default;
  end;

  TCompRepMapItem = class
  private
    FBiDirEnabled: Boolean;
    FDisabled: Boolean;
    FDestClassName: string;
    FDestPropName: string;
    FSourceClassName: string;
    FSourcePropName: string;
    FGroup: TCompRepMapGroupItem;
    FUseConstValue: Boolean;
    FLogOnlyDefValuesEnabled: Boolean;
    FLogValuesEnabled: Boolean;
    FConstValue: string;
    function GetGroupName: string;
    procedure SetGroup(const Value: TCompRepMapGroupItem);
    function GetDestText: string;
    function GetSourceText: string;
    function GetIndex: Integer;
    function GetDestCorePropName: string;
    function GetSourceCorePropName: string;
  public
    constructor Create;
    procedure Reverse;

    procedure AssignMapping(ASource: TCompRepMapItem); virtual;
    function ExtractCorePropName(const AName: string): string;

    property Group: TCompRepMapGroupItem read FGroup write SetGroup;
    property GroupName: string read GetGroupName;

    property Disabled: Boolean read FDisabled write FDisabled;
    property BiDirEnabled: Boolean read FBiDirEnabled write FBiDirEnabled;

    property SourceClassName: string read FSourceClassName write FSourceClassName;
    property SourcePropName: string read FSourcePropName write FSourcePropName;
    property DestClassName: string read FDestClassName write FDestClassName;
    property DestPropName: string read FDestPropName write FDestPropName;

    property UseConstValue: Boolean read FUseConstValue write FUseConstValue;
    property ConstValue: string read FConstValue write FConstValue;

    property LogValuesEnabled: Boolean read FLogValuesEnabled write FLogValuesEnabled;
    property LogOnlyDefValuesEnabled: Boolean read FLogOnlyDefValuesEnabled write FLogOnlyDefValuesEnabled;

    property SourceText: string read GetSourceText;
    property SourceCorePropName: string read GetSourceCorePropName;
    property DestText: string read GetDestText;
    property DestCorePropName: string read GetDestCorePropName;

    property Index: Integer read GetIndex;
  end;

  TCompRepMapList = class(TObjectList)
  private
    function GetItem(Index: Integer): TCompRepMapItem;
  public
    property Items[Index: Integer]: TCompRepMapItem read GetItem; default;
  end;

  TReplaceCompData = class
  private
    FRootConfigurationKey: string;
    function GetXmlFileName: string;
    procedure LoadError(const E: Exception);
    procedure LoadMapGroup(const Doc: IXMLDocument; const MapGroupNode: IXMLNode);
    function GetNodeProperty(const ANode: IXMLNode; const APropName: string; ADefaultValue: string = ''): string;
    function AddMapGroup(const MapGroupName: string): TCompRepMapGroupItem;
    procedure LoadMapItem(const MapItemNode: IXMLNode; MapGroup: TCompRepMapGroupItem);
    procedure SaveMapGroup(const Doc: IXMLDocument; const MapGroupNode: IXMLNode; MapGroupItem: TCompRepMapGroupItem);
    function GetNodeAttrib(const ANode: IXMLNode; const APropName: string; ADefaultValue: string = ''): string;
    function GetNodeBoolProperty(const ANode: IXMLNode; const APropName: string; ADefaultValue: Boolean): Boolean;
    procedure LoadDefaultPropertyMaps;
  protected
    FMapGroupList: TCompRepMapGroupList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveData;
    procedure ReloadData;
    procedure AppendFrom(const FileName: string);
    procedure SaveTo(const FileName: string; GroupNames: TStringList = nil);
    function PrepareMapGroup(const MapGroupName: string): TCompRepMapGroupItem;
    property MapGroupList: TCompRepMapGroupList read FMapGroupList;
    property RootConfigurationKey: string read FRootConfigurationKey write FRootConfigurationKey;
  end;

  TCompRepEvent = class
  public
    FileName: string;
    SourceClassName: string;
    DestClassName: string;
    StackText: string;
    ObjectSearchName: string;
    ComponentName: string;
    ObjectClass: string;
    When: TDateTime;
    Text: string;
    ErrorClass: string;
    Context: string;
    function IsError: Boolean;
    function ObjectText: string;
    function FlatText: string;
    function FormatEventAsText(Template: string): string;
    function EventType: string;
  end;

  TCompRepEventList = class(TObjectList)
  private
    function GetItem(Index: Integer): TCompRepEvent;
    procedure SaveLogEvent(const Doc: IXMLDocument; const ItemNode: IXMLNode; Event: TCompRepEvent);
  public
    procedure SaveAsXML(const AFileName: string; SelectedList: TList);
    property Items[Index: Integer]: TCompRepEvent read GetItem; default;
  end;

  TDefaultMap = record
    SourceClass: string;
    DestClass: string;
    SourceProp: string;
    DestProp: string;
    Disabled: Boolean;
    BiDi: Boolean;
    UseConst: Boolean;
    ConstValue: string;
  end;

const
  DefaultMaps: packed array[0..5] of TDefaultMap = (
    (
      SourceClass: 'TEdit';
      DestClass: 'TMemo';
      SourceProp: 'Text';
      DestProp: 'Lines';
      Disabled: False;
      BiDi: True;
      UseConst: False;
      ConstValue: '';
    ),
    (
      SourceClass: 'TStringGrid';
      DestClass: 'TListView';
      SourceProp: '';
      DestProp: 'ViewStyle';
      Disabled: False;
      BiDi: False;
      UseConst: True;
      ConstValue: 'vsReport';
    ),
    (
      SourceClass: 'TGroupBox';
      DestClass: 'TPanel';
      SourceProp: '';
      DestProp: 'Caption';
      Disabled: False;
      BiDi: False;
      UseConst: True;
      ConstValue: '';
    ),
    (
      SourceClass: 'TDBText';
      DestClass: 'TDBEdit';
      SourceProp: '';
      DestProp: 'BorderStyle';
      Disabled: False;
      BiDi: False;
      UseConst: True;
      ConstValue: 'bsNone';
    ),
    (
      SourceClass: 'TDBText';
      DestClass: 'TDBEdit';
      SourceProp: '';
      DestProp: 'ReadOnly';
      Disabled: False;
      BiDi: False;
      UseConst: True;
      ConstValue: 'True';
    ),
    (
      SourceClass: 'TDBText';
      DestClass: 'TDBEdit';
      SourceProp: '';
      DestProp: 'TabStop';
      Disabled: False;
      BiDi: False;
      UseConst: True;
      ConstValue: 'False';
    )
  );

procedure AddNodeProperty(const Doc: IXMLDocument; const Node: IXMLNode; const PropName, Value: string);

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  GX_GenericUtils, GX_ConfigurationInfo, GX_XmlUtils, GX_ReplaceCompUtils;

const // Do not localize any of the strings in this const section:
  cXmlNodeRoot = 'ReplaceCompMapping';
  cXmlNodeMapGroup = 'MapGroup';
  cXmlNodeMapItem = 'MapItem';
  cXmlNodeMapGrpName = 'Name';

  cXmlNodeMapItemSrcClassName = 'SourceClassName';
  cXmlNodeMapItemSrcPropName = 'SourcePropName';
  cXmlNodeMapItemDestClassName = 'DestClassName';
  cXmlNodeMapItemDestPropName = 'DestPropName';
  cXmlNodeMapItemDisabled = 'Disabled';
  cXmlNodeMapItemBiDirEnabled = 'BiDirEnabled';
  cXmlNodeMapItemUseConstValue = 'UseConstValue';
  cXmlNodeMapItemConstValue = 'ConstValue';
  cXmlNodeMapItemLogValuesEnabled = 'LogValuesEnabled';
  cXmlNodeMapItemLogOnlyDefValuesEnabled = 'LogOnlyDefValuesEnabled';

  cXmlPropValueBool: array[Boolean] of string = ('false', 'true');

  cXmlNodeLogRoot = 'LogEventList';
  cXmlNodeLogEvent = 'LogEvent';

  cXmlNodeEventWhen = 'When';
  cXmlNodeEventType = 'EventType';
  cXmlNodeEventText = 'Text';

  cXmlNodeEventErrorClass = 'ErrorClass';

  cXmlNodeEventContext = 'Context';
  cXmlNodeEventObjSearchName = 'ObjectSearchName';
  cXmlNodeEventObjClassName = 'ObjectClassName';
  cXmlNodeEventComponentName = 'ComponentName';

  cXmlNodeEventChildStack = 'ChildStack';

  cXmlNodeEventFileName = 'FileName';
  cXmlNodeEventSourceClassName = 'SourceClassName';
  cXmlNodeEventDestClassName = 'DestClassName';

type
  EReplaceCompError = class(Exception);

procedure AddNodeProperty(const Doc: IXMLDocument; const Node: IXMLNode; const PropName, Value: string);
var
  ChildNode: IXMLNode;
begin
  ChildNode := Doc.CreateElement(PropName);
  Node.AppendChild(ChildNode);
  ChildNode.Text := Value;
end;

{ TCompRepMapGroupItem }

constructor TCompRepMapGroupItem.Create;
begin
  inherited Create;
  FItems := TCompRepMapList.Create;
end;

destructor TCompRepMapGroupItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TCompRepMapGroupItem.AddItem(Item: TCompRepMapItem);
begin
  if FItems.IndexOf(Item)<0 then
    FItems.Add(Item);
end;

procedure TCompRepMapGroupItem.ExtractItem(Item: TCompRepMapItem);
begin
  FItems.Extract(Item);
end;

function TCompRepMapGroupItem.Add(const SourceClassName, DestClassName,
  SourcePropName, DestPropName: string; DisabledFlag, BiDirFlag: Boolean;
  UseConstValue: Boolean; ConstValue: string; LogValuesEnabled: Boolean;
  LogOnlyDefValuesEnabled: Boolean): TCompRepMapItem;
begin
  Result := TCompRepMapItem.Create;
  Result.SourceClassName := SourceClassName;
  Result.DestClassName := DestClassName;
  Result.SourcePropName := SourcePropName;
  Result.DestPropName := DestPropName;
  Result.Disabled := DisabledFlag;
  Result.BiDirEnabled := BiDirFlag;
  Result.UseConstValue := UseConstValue;
  Result.ConstValue := ConstValue;
  Result.LogValuesEnabled := LogValuesEnabled;
  Result.LogOnlyDefValuesEnabled := LogOnlyDefValuesEnabled;
  FItems.Add(Result);
  Result.Group := Self;
end;

{ TCompRepMapList }

function TCompRepMapList.GetItem(Index: Integer): TCompRepMapItem;
begin
  Result := (inherited Items[Index]) as TCompRepMapItem;
end;

{ TCompRepMapGroupList }

function TCompRepMapGroupList.Add(const MapGroupName: string): TCompRepMapGroupItem;
begin
  Result := TCompRepMapGroupItem.Create;
  Result.Name := MapGroupName;
  AddWithCode(MapGroupName, Result);
end;

function TCompRepMapGroupList.GetItem(
  Index: Integer): TCompRepMapGroupItem;
begin
  Result := (inherited Items[Index]) as TCompRepMapGroupItem;
end;

{ TReplaceCompData }

constructor TReplaceCompData.Create;
begin
  inherited Create;
  FMapGroupList := TCompRepMapGroupList.Create;
end;

destructor TReplaceCompData.Destroy;
begin
  FreeAndNil(FMapGroupList);
  inherited;
end;

function TReplaceCompData.GetXmlFileName: string;
begin
  Result := AddSlash(ConfigInfo.ConfigPath) + 'ReplaceComponents.xml'
end;

procedure TReplaceCompData.LoadError(const E: Exception);
resourcestring
  SLoadError = 'Error while loading %s' + sLineBreak;
begin
  GxLogAndShowException(E, Format(SLoadError, [GetXmlFileName]));
end;

procedure TReplaceCompData.AppendFrom(const FileName: string);
var
  Doc: IXMLDocument;
  MapGroupNodes: IXMLNodeList;
  MapGroupNode: IXMLNode;
  i: Integer;
begin
  Doc := CreateXMLDoc;
  Doc.Load(FileName);

  MapGroupNodes := Doc.DocumentElement.SelectNodes(cXmlNodeMapGroup);
  {$IFOPT D+} SendDebug(Format('ReplaceComp - %d groups found', [MapGroupNodes.Length])); {$ENDIF}

  for i := 0 to MapGroupNodes.Length - 1 do
  begin
    MapGroupNode := MapGroupNodes.Item[i];
    try
      LoadMapGroup(Doc, MapGroupNode);
    except
      on e: EReplaceCompError do
        LoadError(e);
      on e: EConvertError do
        LoadError(e);
    end;
  end;
end;

procedure TReplaceCompData.ReloadData;
var
  FileName: string;
begin
  FileName := GetXmlFileName;

  if not FileExists(FileName) then
  begin
    {$IFOPT D+} SendDebug('ReplaceComp map file not found: ' + FileName); {$ENDIF}
    LoadDefaultPropertyMaps;
  end
  else
  begin
    {$IFOPT D+} SendDebug('ReplaceComp map file found: ' + FileName); {$ENDIF}
    FMapGroupList.Clear;
    AppendFrom(FileName);
  end;
end;

function TReplaceCompData.GetNodeProperty(const ANode: IXMLNode;
  const APropName: string; ADefaultValue: string): string;
var
  ChildNode: IXMLNode;
begin
  ChildNode := ANode.SelectSingleNode(APropName);
  if Assigned(ChildNode) then
    Result := ChildNode.Text
  else
    Result := ADefaultValue;
end;

function TReplaceCompData.GetNodeBoolProperty(const ANode: IXMLNode;
  const APropName: string; ADefaultValue: Boolean): Boolean;
var
  StrValue: string;  
begin
  StrValue := GetNodeProperty(ANode, APropName,
    cXmlPropValueBool[ADefaultValue]);
  Result := (StrValue = cXmlPropValueBool[True]);
end;


function TReplaceCompData.GetNodeAttrib(const ANode: IXMLNode;
  const APropName: string; ADefaultValue: string): string;
var
  ChildNode: IXMLNode;
begin
  ChildNode := ANode.Attributes.GetNamedItem(APropName);
  if Assigned(ChildNode) then
    Result := ChildNode.Text
  else
    Result := ADefaultValue;
end;

function TReplaceCompData.AddMapGroup(
  const MapGroupName: string): TCompRepMapGroupItem;
begin
  Result := FMapGroupList.Add(MapGroupName);
end;

function TReplaceCompData.PrepareMapGroup(const MapGroupName: string): TCompRepMapGroupItem;
begin
  Result := (FMapGroupList.FindObject(MapGroupName) as TCompRepMapGroupItem);
  if Result = nil then
    Result := AddMapGroup(MapGroupName);
end;

procedure TReplaceCompData.LoadMapItem(const MapItemNode: IXMLNode; MapGroup: TCompRepMapGroupItem);
var
  SourceClassName, DestClassName, SourcePropName, DestPropName: string;
  DisabledFlag: Boolean;
  BiDirFlag: Boolean;
  UseConstValue, LogValuesEnabled, LogOnlyDefValuesEnabled: Boolean;
  ConstValue: string;
begin
  try
    Assert(MapItemNode.NodeType = ELEMENT_NODE);
    Assert(MapItemNode.NodeName = cXmlNodeMapItem);
    Assert(MapGroup <> nil);

    SourceClassName := GetNodeProperty(MapItemNode, cXmlNodeMapItemSrcClassName);
    DestClassName := GetNodeProperty(MapItemNode, cXmlNodeMapItemDestClassName);
    SourcePropName := GetNodeProperty(MapItemNode, cXmlNodeMapItemSrcPropName);
    DestPropName := GetNodeProperty(MapItemNode, cXmlNodeMapItemDestPropName);

    DisabledFlag := GetNodeBoolProperty(MapItemNode, cXmlNodeMapItemDisabled, CompRepDefDisabled);
    BiDirFlag := GetNodeBoolProperty(MapItemNode, cXmlNodeMapItemBiDirEnabled, CompRepDefBiDirEnabled);
    UseConstValue := GetNodeBoolProperty(MapItemNode, cXmlNodeMapItemUseConstValue, CompRepDefUseConstValue);
    ConstValue := GetNodeProperty(MapItemNode, cXmlNodeMapItemConstValue);
    LogValuesEnabled := GetNodeBoolProperty(MapItemNode, cXmlNodeMapItemLogValuesEnabled, CompRepDefLogValuesEnabled);
    LogOnlyDefValuesEnabled := GetNodeBoolProperty(MapItemNode, cXmlNodeMapItemLogOnlyDefValuesEnabled, CompRepDefLogOnlyDefValuesEnabled);

    MapGroup.Add(SourceClassName, DestClassName, SourcePropName, DestPropName,
      DisabledFlag, BiDirFlag, UseConstValue, ConstValue, LogValuesEnabled,
      LogOnlyDefValuesEnabled);
  except
    on e: EReplaceCompError do
      LoadError(e);
    on e: EConvertError do
      LoadError(e);
  end;
end;

procedure TReplaceCompData.LoadMapGroup(const Doc: IXMLDocument; const MapGroupNode: IXMLNode);
var
  MapItemNodes: IXMLNodeList;
  MapItemNode: IXMLNode;
  i: Integer;
  MapGroup: TCompRepMapGroupItem;
  GroupName: string;
begin
  GroupName := GetNodeAttrib(MapGroupNode, cXmlNodeMapGrpName);
  {$IFOPT D+} SendDebug('Map Group name = '+GroupName); {$ENDIF}

  MapGroup := PrepareMapGroup(GroupName);
  {$IFOPT D+} SendDebug('Map Assigned Group name = '+MapGroup.Name); {$ENDIF}

  MapItemNodes := MapGroupNode.SelectNodes(cXmlNodeMapItem);
  for i := 0 to MapItemNodes.Length - 1 do
  begin
    MapItemNode := MapItemNodes.Item[i];
    LoadMapItem(MapItemNode, MapGroup);
  end;
end;

procedure TReplaceCompData.SaveMapGroup(const Doc: IXMLDocument;
  const MapGroupNode: IXMLNode; MapGroupItem: TCompRepMapGroupItem);
var
  i: Integer;
  MapItem: TCompRepMapItem;
  MapItemNode: IXMLElement;
begin
  for i := 0 to MapGroupItem.Items.Count - 1 do
  begin
    MapItem := MapGroupItem.Items[i];

    MapItemNode := Doc.CreateElement(cXmlNodeMapItem);
    MapGroupNode.AppendChild(MapItemNode);

    AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemSrcClassName, MapItem.SourceClassName);
    AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemDestClassName, MapItem.DestClassName);
    AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemSrcPropName, MapItem.SourcePropName);
    AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemDestPropName, MapItem.DestPropName);

    if MapItem.Disabled <> CompRepDefDisabled then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemDisabled, cXmlPropValueBool[MapItem.Disabled]);
    if MapItem.BiDirEnabled <> CompRepDefBiDirEnabled then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemBiDirEnabled, cXmlPropValueBool[MapItem.BiDirEnabled]);
    if MapItem.UseConstValue <> CompRepDefUseConstValue then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemUseConstValue, cXmlPropValueBool[MapItem.UseConstValue]);
    if MapItem.ConstValue <> '' then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemConstValue, MapItem.ConstValue);
    if MapItem.LogValuesEnabled <> CompRepDefLogValuesEnabled then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemLogValuesEnabled, cXmlPropValueBool[MapItem.LogValuesEnabled]);
    if MapItem.LogOnlyDefValuesEnabled <> CompRepDefLogOnlyDefValuesEnabled then
      AddNodeProperty(Doc, MapItemNode, cXmlNodeMapItemLogOnlyDefValuesEnabled, cXmlPropValueBool[MapItem.LogOnlyDefValuesEnabled]);
  end;
end;

procedure TReplaceCompData.SaveTo(const FileName: string; GroupNames: TStringList);
var
  Doc: IXMLDocument;
  RootNode, MapGroupNode: IXMLElement;
  i: Integer;
begin
  Doc := CreateXMLDoc;
  AddXMLHeader(Doc);
  RootNode := Doc.CreateElement(cXmlNodeRoot);
  Doc.DocumentElement := RootNode;

  for i := 0 to FMapGroupList.Count-1 do
  begin
    if Assigned(GroupNames) and (GroupNames.IndexOf(FMapGroupList[i].Name) < 0) then
      Continue;
    MapGroupNode := Doc.CreateElement(cXmlNodeMapGroup);
    MapGroupNode.SetAttribute(cXmlNodeMapGrpName, FMapGroupList[i].Name);
    RootNode.AppendChild(MapGroupNode);
    SaveMapGroup(Doc, MapGroupNode, FMapGroupList[i]);
  end;
  Doc.Save(FileName, ofIndent);
end;

procedure TReplaceCompData.SaveData;
begin
  SaveTo(GetXmlFileName);
end;

procedure TReplaceCompData.LoadDefaultPropertyMaps;
const
  DefaultGroupName = 'Borland VCL';
var
  i: Integer;
  MapGroup: TCompRepMapGroupItem;
  DM: TDefaultMap;
begin
  MapGroup := FMapGroupList.Add(DefaultGroupName);
  for i := Low(DefaultMaps) to High(DefaultMaps) do
  begin
    DM := DefaultMaps[i];
    MapGroup.Add(DM.SourceClass, DM.DestClass, DM.SourceProp, DM.DestProp,
      DM.Disabled, DM.BiDi, DM.UseConst, DM.ConstValue, False, False);
  end;
end;

{ TCompRepMapItem }

function TCompRepMapItem.GetGroupName: string;
begin
  if Assigned(FGroup) then
    Result := FGroup.Name
  else
    Result := '';
end;

function TCompRepMapItem.GetDestText: string;
begin
  if Trim(FDestPropName)='' then
    Result := FDestClassName
  else
    Result := FDestClassName + '.' + FDestPropName;
end;

function TCompRepMapItem.GetSourceText: string;
resourcestring
  SConst = 'const';
begin
  if Trim(FSourcePropName)='' then
  begin
    if UseConstValue then
      Result := FSourceClassName + ' / ' + SConst + ' ' + FConstValue
    else
      Result := FSourceClassName;
  end
  else
    Result := FSourceClassName + '.' + FSourcePropName;
end;

procedure TCompRepMapItem.SetGroup(const Value: TCompRepMapGroupItem);
begin
  if FGroup <> Value then
  begin
    if Assigned(FGroup) then
      FGroup.ExtractItem(Self);
    FGroup := Value;
    if Assigned(FGroup) then
      FGroup.AddItem(Self);
  end;
end;

function TCompRepMapItem.GetIndex: Integer;
begin
  if Assigned(FGroup) then
    Result := FGroup.Items.IndexOf(Self)
  else
    Result := -1;
end;

constructor TCompRepMapItem.Create;
begin
  inherited Create;
  FBiDirEnabled := CompRepDefBiDirEnabled;
  FDisabled := CompRepDefDisabled;
  FUseConstValue := CompRepDefUseConstValue;
  FLogValuesEnabled := CompRepDefLogValuesEnabled;
  FLogOnlyDefValuesEnabled := CompRepDefLogOnlyDefValuesEnabled;
end;

procedure TCompRepMapItem.Reverse;

  procedure ExchangeStrings(var S1, S2: string);
  var
    TmpStr: string;
  begin
    TmpStr := S1;
    S1 := S2;
    S2 := TmpStr;
  end;

begin
  ExchangeStrings(FSourceClassName, FDestClassName);
  ExchangeStrings(FSourcePropName, FDestPropName);
end;

function TCompRepMapItem.ExtractCorePropName(const AName: string): string;
var
  TmpName: string;
begin
  TmpName := AName;
  Result := ExtractToken(TmpName, '.');
end;

function TCompRepMapItem.GetSourceCorePropName: string;
begin
  Result := ExtractCorePropName(SourcePropName);
end;

function TCompRepMapItem.GetDestCorePropName: string;
begin
  Result := ExtractCorePropName(DestPropName);
end;

procedure TCompRepMapItem.AssignMapping(ASource: TCompRepMapItem);
begin
  Self.SourceClassName := ASource.SourceClassName;
  Self.SourcePropName := ASource.SourcePropName;
  Self.DestClassName := ASource.DestClassName;
  Self.DestPropName := ASource.DestPropName;
  Self.BiDirEnabled := ASource.BiDirEnabled;
  Self.Disabled := ASource.Disabled;
  Self.UseConstValue := ASource.UseConstValue;
  Self.ConstValue := ASource.ConstValue;
  Self.LogValuesEnabled := ASource.LogValuesEnabled;
  Self.LogOnlyDefValuesEnabled := ASource.LogOnlyDefValuesEnabled;
end;

{ TCompRepEventList }

function TCompRepEventList.GetItem(Index: Integer): TCompRepEvent;
begin
  Result := (inherited Items[Index]) as TCompRepEvent;
end;

procedure TCompRepEventList.SaveAsXML(const AFileName: string; SelectedList: TList);
var
  Doc: IXMLDocument;
  RootNode, ItemNode: IXMLElement;
  i: Integer;
begin
  Doc := CreateXMLDoc;
  AddXMLHeader(Doc);
  RootNode := Doc.CreateElement(cXmlNodeLogRoot);
  Doc.DocumentElement := RootNode;

  for i := 0 to Count-1 do
  begin
    if Assigned(SelectedList) and (SelectedList.IndexOf(Self[i]) < 0) then
      Continue;

    ItemNode := Doc.CreateElement(cXmlNodeLogEvent);
    RootNode.AppendChild(ItemNode);
    SaveLogEvent(Doc, ItemNode, Items[i]);
  end;

  Doc.Save(AFileName, ofIndent);
end;

procedure TCompRepEventList.SaveLogEvent(const Doc: IXMLDocument;
  const ItemNode: IXMLNode; Event: TCompRepEvent);
begin
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventWhen, DateTimeToStr(Event.When));
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventType, Event.EventType);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventText, Event.Text);

  if Event.IsError then
    AddNodeProperty(Doc, ItemNode, cXmlNodeEventErrorClass, Event.ErrorClass);

  AddNodeProperty(Doc, ItemNode, cXmlNodeEventContext, Event.Context);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventObjSearchName, Event.ObjectSearchName);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventObjClassName, Event.ObjectClass);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventComponentName, Event.ComponentName);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventChildStack, Event.StackText);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventFileName, Event.FileName);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventSourceClassName, Event.SourceClassName);
  AddNodeProperty(Doc, ItemNode, cXmlNodeEventDestClassName, Event.DestClassName);
end;

{ TCompRepEvent }

function TCompRepEvent.FlatText: string;
begin
  Result := FlatLine(Text);
end;

function TCompRepEvent.IsError: Boolean;
begin
  Result := (ErrorClass <> '');
end;

function TCompRepEvent.ObjectText: string;
resourcestring
  SNone = 'none';
begin
  if ObjectSearchName <> '' then
    Result := ObjectClass + ':' + ObjectSearchName
  else
    Result := SNone;
end;

function TCompRepEvent.FormatEventAsText(Template: string): string;
resourcestring
  SErrorLayout = 'Error: %ErrorClass%'+#10;
var
  ErrorPart: string;
begin
  Result := Template;
  Result := StringReplace(Result, '%When%', DateTimeToStr(Self.When), [rfReplaceAll, rfIgnoreCase]);

  if Self.IsError then
    ErrorPart := SErrorLayout
  else
    ErrorPart := '';

  Result := StringReplace(Result, '%EventType%', EventType, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%Text%', Self.Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%FlatText%', Self.FlatText, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%FileName%', Self.FileName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%SourceClassName%', Self.SourceClassName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%DestClassName%', Self.DestClassName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ObjectSearchName%', Self.ObjectSearchName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ObjectClass%', Self.ObjectClass, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ComponentName%', Self.ComponentName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ChildStack%', Self.StackText, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ErrorPart%', ErrorPart, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%ErrorClass%', Self.ErrorClass, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%Context%', Self.Context, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%FlatContext%', FlatLine(Self.Context), [rfReplaceAll, rfIgnoreCase]);
end;

function TCompRepEvent.EventType: string;
begin
  if IsError then
    Result := 'Error'
  else
    Result := 'Info';
end;

end.

