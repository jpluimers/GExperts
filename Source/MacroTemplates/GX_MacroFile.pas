// Save/load macro templates from an XML file
// Original Author: Piotr Likus

unit GX_MacroFile;

interface

uses
  Classes;

type
  TTemplateInsertPos = (tipCursorPos, tipUnitStart, tipLineStart, tipLineEnd);

const
  DefaultInsertPos = tipCursorPos;
  EmptyShortCut = 0;

type
  TTemplateList = class;

  // Stores all information about single macro template
  TMacroObject = class
  private
    FText: string; // code of macro
    FDesc: string; // description
    FPubUnits: TStringList; // list of public units
    FPrivUnits: TStringList; // list of private units
    FInsertPos: TTemplateInsertPos; // position of insert
    FShortCut: TShortCut;
    FName: string;
    procedure SetPrivUnits(const Value: TStringList);
    procedure SetPubUnits(const Value: TStringList);
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    procedure Assign(ASource: TMacroObject);
    property Name: string read FName write FName;
    property Desc: string read FDesc write FDesc;
    property Text: string read FText write FText;
    property PrivUnits: TStringList read FPrivUnits write SetPrivUnits;
    property PubUnits: TStringList read FPubUnits write SetPubUnits;
    property InsertPos: TTemplateInsertPos read FInsertPos write FInsertPos default DefaultInsertPos;
    property ShortCut: TShortCut read FShortCut write FShortCut default EmptyShortCut;
  end;

  TTemplateList = class(TList)
  protected
    function GetTemplate(Index: Integer): TMacroObject;
  public
    procedure Clear; override;
    destructor Destroy; override;

    function CreateTemplate(const AName: string): TMacroObject;
    procedure Remove(ATemplate: TMacroObject);

    property Templates[Index: Integer]: TMacroObject read GetTemplate; default;
  end;

  // Saves/loads templates from an XML file
  TMacroFile = class
  private
    FTemplateList: TTemplateList;
    FFileName: string;
    function GetMacroItem(Index: Integer): TMacroObject;
    function GetMacroCount: Integer;
  public
    constructor Create(const AFileName: string = '');
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromFile;
    procedure SaveToFile(const AFilename: string);

    function AddMacro(AMacroObject: TMacroObject): TMacroObject;
    procedure RemoveMacro(AMacroObject: TMacroObject);
    function IndexOf(const AObjectName: string): Integer;

    property MacroCount: Integer read GetMacroCount;
    property MacroItems[Index: Integer]: TMacroObject read GetMacroItem; default;
    property FileName: string read FFileName write FFileName;
  end;

  function InsertPosToText(Pos: TTemplateInsertPos): string;

implementation

uses
  SysUtils, OmniXML, GX_XmlUtils, GX_GenericUtils;

const
  // XML storage tag tag names
  TagPrivUnits = 'PrivUnits'; // property name for Private units
  TagPubUnits  = 'PubUnits';  // property name for Public units
  TagDesc      = 'Desc';      // property name for Description
  TagName      = 'Name';      // property name for Name
  TagInsertPos = 'InsertPos'; // property name for Insert position
  TagShortCut  = 'Shortcut';  // property name for ShortCut
  TagTemplates = 'Templates'; // list of templates
  TagTemplate  = 'Template';  // single template

{ TMacroObject }

procedure TMacroObject.Assign(ASource: TMacroObject);
begin
  FName := ASource.Name;
  FDesc := ASource.Desc;
  FText := ASource.Text;
  FPrivUnits.Assign(ASource.PrivUnits);
  FPubUnits.Assign(ASource.PubUnits);
  FInsertPos := ASource.InsertPos;
  FShortCut := ASource.ShortCut;
end;

constructor TMacroObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FPrivUnits := TStringList.Create;
  FPubUnits := TStringList.Create;
  FInsertPos := tipCursorPos;
end;

destructor TMacroObject.Destroy;
begin
  FreeAndNil(FPrivUnits);
  FreeAndNil(FPubUnits);
  inherited;
end;

procedure TMacroObject.SetPrivUnits(const Value: TStringList);
begin
  FPrivUnits.Assign(Value);
end;

procedure TMacroObject.SetPubUnits(const Value: TStringList);
begin
  FPubUnits.Assign(Value);
end;

{ TMacroFile }

constructor TMacroFile.Create(const AFileName: string);
begin
  inherited Create;
  FTemplateList := TTemplateList.Create;
  FFileName := AFileName;
end;

destructor TMacroFile.Destroy;
begin
  FreeAndNil(FTemplateList);
  inherited;
end;

function TMacroFile.AddMacro(AMacroObject: TMacroObject): TMacroObject;
begin
  Result := FTemplateList.CreateTemplate(AMacroObject.Name);
  Result.Assign(AMacroObject);
end;

procedure TMacroFile.Clear;
begin
  inherited;
  FTemplateList.Clear;
end;

function TMacroFile.GetMacroItem(Index: Integer): TMacroObject;
begin
  Result := TMacroObject(FTemplateList[Index]);
end;

function TMacroFile.IndexOf(const AObjectName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FTemplateList.Count - 1 do
    if AnsiCompareText(MacroItems[i].Name, AObjectName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;

procedure TMacroFile.RemoveMacro(AMacroObject: TMacroObject);
begin
  FTemplateList.Remove(AMacroObject);
end;

procedure TMacroFile.LoadFromFile;

  function GetOptionalProp(AMap: IXMLNamedNodeMap; APropName: string;
    ADefaultValue: string = ''): string;
  var
    PropAttr: IXMLNode;
  begin
    PropAttr := AMap.GetNamedItem(APropName);
    if Assigned(PropAttr) then
      Result := PropAttr.NodeValue
    else
      Result := ADefaultValue;
  end;

var
  Doc: IXMLDocument;
  i: Integer;
  MacroObject: TMacroObject;
  Nodes: IXMLNodeList;
  TmplName, TmplDesc, TmplText, TmplPrivUnits, TmplPubUnits, TmplInsertPos: string;
  TmplShortCut: string;
  PropMap: IXMLNamedNodeMap;
  PropNode: IXMLNode;
  Macro: IXMLNode;
begin
  FTemplateList.Clear;
  if not FileExists(FFileName) then
    Exit;

  Doc := CreateXMLDoc;
  Doc.Load(FileName);
  Nodes := Doc.DocumentElement.SelectNodes(TagTemplate);
  for i := 0 to Nodes.Length - 1 do
  begin
    Macro := Nodes.Item[i];
    PropMap := Macro.Attributes;

    PropNode := PropMap.GetNamedItem(TagName);
    if not Assigned(PropNode) then
      Exit;

    TmplName := PropNode.NodeValue;

    PropNode := PropMap.GetNamedItem(TagDesc);
    if not Assigned(PropNode) then
      Exit;

    TmplDesc := PropNode.NodeValue;

    TmplText := GetCDataSectionTextOrNodeText(Macro);

    TmplPrivUnits := GetOptionalProp(PropMap, TagPrivUnits);
    TmplPubUnits := GetOptionalProp(PropMap, TagPubUnits);
    TmplInsertPos := GetOptionalProp(PropMap, TagInsertPos, IntToStr(Ord(DefaultInsertPos)));
    TmplShortCut := GetOptionalProp(PropMap, TagShortCut, IntToStr(EmptyShortCut));

    MacroObject := FTemplateList.CreateTemplate(TmplName);
    MacroObject.Desc := TmplDesc;
    MacroObject.Text := TmplText;
    MacroObject.PrivUnits.CommaText := TmplPrivUnits;
    MacroObject.PubUnits.CommaText := TmplPubUnits;
    MacroObject.InsertPos := TTemplateInsertPos(StrToIntDef(TmplInsertPos, Ord(DefaultInsertPos)));
    MacroObject.ShortCut := StrToIntDef(TmplShortCut, EmptyShortCut);
  end;
end;

procedure TMacroFile.SaveToFile(const AFilename: string);
var
  Doc: IXMLDocument;
  Root: IXMLElement;
  i: Integer;
  MacroObject: TMacroObject;
  MacroItem: IXMLElement;
  TextNode: IXMLCDATASection;
begin
  Doc := CreateXMLDoc;
  AddXMLHeader(Doc);
  Root := Doc.CreateElement(TagTemplates);
  Doc.AppendChild(Root);
  for i := 0 to FTemplateList.Count - 1 do
  begin
    MacroItem := Doc.CreateElement(TagTemplate);

    MacroObject := FTemplateList[i];
    MacroItem.SetAttribute(TagName, MacroObject.Name);
    MacroItem.SetAttribute(TagDesc, MacroObject.Desc);
    if MacroObject.PrivUnits.Count > 0 then
      MacroItem.SetAttribute(TagPrivUnits, MacroObject.PrivUnits.CommaText);
    if MacroObject.PubUnits.Count > 0 then
      MacroItem.SetAttribute(TagPubUnits, MacroObject.PubUnits.CommaText);
    if MacroObject.InsertPos <> DefaultInsertPos then
      MacroItem.SetAttribute(TagInsertPos, IntToStr(Ord(MacroObject.InsertPos)));
    if MacroObject.ShortCut <> EmptyShortCut then
      MacroItem.SetAttribute(TagShortCut, IntToStr(MacroObject.ShortCut));

    TextNode := Doc.CreateCDATASection(EscapeCDataText(MacroObject.Text));
    MacroItem.AppendChild(TextNode);

    Root.AppendChild(MacroItem);
  end;
  if PrepareDirectoryForWriting(ExtractFileDir(AFilename)) then
    Doc.Save(AFileName, ofFlat);
end;

function TMacroFile.GetMacroCount: Integer;
begin
  Result := FTemplateList.Count;
end;

{ TTemplateList }

destructor TTemplateList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTemplateList.Clear;
var
  Obj: TObject;
begin
  while Count > 0 do
  begin
    Obj := Self[Count - 1];
    Delete(Count - 1);
    FreeAndNil(Obj);
  end;
end;

function TTemplateList.GetTemplate(Index: Integer): TMacroObject;
begin
  Result := TMacroObject(inherited Items[Index]);
end;

function TTemplateList.CreateTemplate(const AName: string): TMacroObject;
begin
  Result := TMacroObject.Create(AName);
  Add(Result);
end;

procedure TTemplateList.Remove(ATemplate: TMacroObject);
begin
  inherited Remove(ATemplate);
  FreeAndNil(ATemplate);
end;

function InsertPosToText(Pos: TTemplateInsertPos): string;
begin
  Result := 'Unknown';
  case Pos of
    tipCursorPos: Result := 'Cursor';
    tipUnitStart: Result := 'File';
    tipLineStart: Result := 'Line';
    tipLineEnd:   Result := 'LineEnd';
  end;
end;

end.

