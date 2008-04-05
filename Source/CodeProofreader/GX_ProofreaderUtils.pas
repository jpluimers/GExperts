unit GX_ProofreaderUtils;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Contnrs, GX_GenericClasses;

type
  TGXWhereReplace = (wrAnywhere, wrWordBegin, wrWordEnd, wrWholeWord);
  TReplacementSource = (rtPasSrc, rtCPPSrc, rtPreproc, rtSQLSrc, rtAssembler, rtString, rtComment, rtCSSrc);

  TCorrectionKind = (ckAutoCorrection, ckWord);

  TCorrectionItem = class(TObject)
  private
    FCorrectionKind: TCorrectionKind;
    FSourceLanguage: TReplacementSource;
    FOriginalText: string;
    FInfoString: string;
    FTime: TDateTime;
  public
    property CorrectionKind: TCorrectionKind read FCorrectionKind write FCorrectionKind;
    property SourceLanguage: TReplacementSource read FSourceLanguage write FSourceLanguage;
    property OriginalText: string read FOriginalText write FOriginalText;
    property InfoString: string read FInfoString write FInfoString;
    property Time: TDateTime read FTime write FTime;
  end;

  TCorrectionHistory = class(TObject)
  private
    FItems: TObjectList;
    FMaxHistory: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCorrectionItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCorrectionItem read GetItem; default;
    procedure Add(Item: TCorrectionItem);
  end;

  TReplacementItem = class(TObject)
  private
    FTyped: string;
    FWhere: TGXWhereReplace;
    FReplace: string;
  public
    constructor Create(const TypedString: string; ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string);

    property Typed: string read FTyped;
    property Where: TGXWhereReplace read FWhere;
    property Replace: string read FReplace;
  end;

  TReplacementStringList = class(TStringList)
  public
    destructor Destroy; override;
    function Add(const TypedString: string; ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string): Integer; reintroduce;
    procedure Put(Index: Integer; const TypedString: string; ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string); reintroduce;
    procedure Delete(Index: Integer); override;
    function Find(const TypedString: string): Integer; reintroduce;
  end;

const
  // These strings are used as mappings from the enum values used internally
  // to English meanings for display:
  GXWhereReplaceStrings: array[TGXWhereReplace] of string = (
    'Anywhere',
    'At the begining of a word',
    'At the end of a word',
    'Whole word only');

  GXWhereReplaceXmlStrings: array[TGXWhereReplace] of string = (
    'Anywhere',
    'WordBeginning',
    'WordEnd',
    'WholeWord');

  ReplacementSourceText: array[TReplacementSource] of string = (
    'Pascal source code',
    'C++ source code',
    'C pre-processor',
    'SQL source code',
    'Inline assembler',
    'Strings',
    'Comments',
    'C# Source');

  ReplacementSourceXmlStrings: array[TReplacementSource] of string = (
    'Pascal',
    'C++',
    'CPreProcessor',
    'SQL',
    'Assembler',
    'Strings',
    'Comments',
    'C#');

function XmlStringToReplacementSource(const LanguageName: string): TReplacementSource;
function XmlStringToWhereReplace(const WhereReplaceString: string): TGXWhereReplace;

implementation

uses
  SysUtils, GX_GenericUtils;

function XmlStringToReplacementSource(const LanguageName: string): TReplacementSource;
var
  ReplacementSource: TReplacementSource;
begin
  for ReplacementSource := Low(TReplacementSource) to High(TReplacementSource) do
    if ReplacementSourceXmlStrings[ReplacementSource] = LanguageName then
    begin
      Result := ReplacementSource;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown Language "%s"', [LanguageName]);
end;

function XmlStringToWhereReplace(const WhereReplaceString: string): TGXWhereReplace;
var
  WhereReplace: TGXWhereReplace;
begin
  for WhereReplace := Low(TGXWhereReplace) to High(TGXWhereReplace) do
    if GXWhereReplaceXmlStrings[WhereReplace] = WhereReplaceString then
    begin
      Result := WhereReplace;
      Exit;
    end;
  raise EConvertError.CreateFmt('Unknown WhereReplace "%s"', [WhereReplaceString]);
end;

{ TCorrectionHistory }

procedure TCorrectionHistory.Add(Item: TCorrectionItem);
begin
  if Item = nil then
    Exit;

  Assert(Assigned(FItems));

  if Count = FMaxHistory then
  begin
    Assert(Count > 0);
    FItems[Count - 1].Free;
    FItems.Delete(Count - 1);
  end;
  FItems.Insert(0, Item);
end;

constructor TCorrectionHistory.Create;
const
  OwnsObject = True;
begin
  inherited Create;

  FItems := TObjectList.Create(not OwnsObject);
  FMaxHistory := 25;
end;

destructor TCorrectionHistory.Destroy;
var
  i: Integer;
begin
  if FItems <> nil then
  begin
    for i := 0 to FItems.Count - 1 do
      FItems[i].Free;

    FreeAndNil(FItems);
  end;

  inherited Destroy;
end;

function TCorrectionHistory.GetCount: Integer;
begin
  Assert(Assigned(FItems));
  Result := FItems.Count;
end;

function TCorrectionHistory.GetItem(Index: Integer): TCorrectionItem;
begin
  Assert(Assigned(FItems));

  Result := nil;
  if (Index >= 0) and (Index < FItems.Count) then
    Result := FItems[Index] as TCorrectionItem;
end;

{ TReplacementItem }

constructor TReplacementItem.Create(const TypedString: string; ReplaceWhere: TGXWhereReplace;
  const ReplaceWithString: string);
begin
  inherited Create;

  FTyped := TypedString;
  FWhere := ReplaceWhere;
  FReplace := ReplaceWithString;
end;

{ TReplacementStringList }

destructor TReplacementStringList.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Delete(i);
  inherited Destroy;
end;

function TReplacementStringList.Add(const TypedString: string;
  ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string): Integer;
var
  Item: TReplacementItem;
begin
  Item := TReplacementItem.Create(TypedString, ReplaceWhere, ReplaceWithString);
  Result := AddObject(AnsiUpperCase(TypedString), Item);
end;

procedure TReplacementStringList.Put(Index: Integer; const TypedString: string;
  ReplaceWhere: TGXWhereReplace; const ReplaceWithString: string);
var
  Item: TReplacementItem;
begin
  inherited Put(Index, AnsiUpperCase(TypedString));
  Item := Objects[Index] as TReplacementItem;
  Assert(Assigned(Item));
  Item.FTyped := TypedString;
  Item.FWhere := ReplaceWhere;
  Item.FReplace := ReplaceWithString;
end;

procedure TReplacementStringList.Delete(Index: Integer);
var
  Item: TReplacementItem;
begin
  // Overriden to avoid memory leaks from TReplacementItem.
  // TODO 3 -cCleanup -oAnyone: Does this prevent all leaks?
  Item := Objects[Index] as TReplacementItem;
  Assert(Assigned(Item));
  Item.Free;
  inherited Delete(Index);
end;

function TReplacementStringList.Find(const TypedString: string): Integer;
var
  i: Integer;
  TestString: string;
begin
  Result := -1;
  if TypedString = '' then
    Exit;

  TestString := AnsiUpperCase(TypedString);
  for i := Count - 1 downto 0 do
  begin
    if StrEndsWith(Strings[i], TestString) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

end.

