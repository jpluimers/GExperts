unit GX_UsesManager;

// Add to, remove from, and query the uses clauses of Object Pascal units
// Orginal Author: Krzysztof Jez - krzysztofj@bms.com.pl
// Modified by Erik Berry to remove external dependencies and cleanup
// These routines do not support C++ source

interface

uses Classes, Contnrs, ToolsAPI, mPasLex;

type
  TUsesItem = class(TObject)
    Name: string;
    BeginPos: Longint;
    EndPos: Longint; // Position at the end of the unit name
    CommaBeforePos: Longint; // Position of ',' before unit name
    CommaAfterPos: Longint;  // Position of ',' after unit name
    SpaceAfter: Boolean;
  end;

  TUsesList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TUsesItem;
    procedure SetItem(AIndex: Integer; const Value: TUsesItem);
  public
    function Add: TUsesItem;
    function IndexOf(const AUnitName: string): Integer;
    property Items[AIndex: Integer]: TUsesItem read GetItem write SetItem;
  end;

  TUsesStatus = (usNonExisting, usInterface, usImplementation, usInsideUnit);

  TUsesManager = class(TObject)
  private
    FParser: TmwPasLex;
    FFileContent: string;
    FInterfUses: TUsesList;
    FImplemUses: TUsesList;
    FImplPosition: Integer;  // Position of the last char of the "implementation" keyword
    FIntfPosition: Integer;  // Position of the last char of the "interface" keyword
    FBegOfIntfUses: Integer; // Position of first char of interface "uses" keyword
    FEndOfIntfUses: Integer; // Position of the semicolon which ends interface uses clause
    FBegOfImplUses: Integer; // Position of first char of implementation "uses" keyword
    FEndOfImplUses: Integer; // Position of the semicolon which ends implementation uses clause
    FFileName: string;
    procedure BuildUsesList;
    function GetCurrentUnitName: string;
    function UsesLineWouldBeTooLong(InsertPos, InsertLength: Integer): Boolean;
    procedure InternalInit;
  public
    constructor Create(const SourceEditor: IOTASourceEditor); overload;
    constructor Create(const FileName: string); overload;
    destructor Destroy; override;
    function GetUsesStatus(const AUnitName: string): TUsesStatus;
    function AddToImpSection(const AUnitName: string): Boolean;
    function AddToIntSection(const AUnitName: string): Boolean;
    function InternalAddToUsesSection(const AUnitName: string; ToInterface: Boolean): Boolean;
    function IsPositionBeforeImplementation(Pos: Integer): Boolean;
    procedure AddUnits(AUnits: TStrings; AToImplementation: Boolean = True);
    procedure InternalRemoveFromUses(InInterface: Boolean; const AUnitName: string);
    procedure RemoveFromImplUses(const AUnitName: string);
    procedure RemoveImplementationUses;
    procedure RemoveFromIntfUses(const AUnitName: string);
    procedure RemoveInterfaceUses;
    property ImplementationUses: TUsesList read FImplemUses;
    property InterfaceUses: TUsesList read FInterfUses;
  end;

// These act on the current source editor
function UseUnitInImplementation(const AUnitName: string): Boolean;
function UseUnitInInterface(const AUnitName: string): Boolean;
function GetUsesStatus(const AUnitName: string): TUsesStatus;
procedure GetImplementationUnits(Units: TStrings);
procedure GetInterfaceUnits(Units: TStrings);
procedure RemoveUnitFromImplementation(const AUnitName: string);
procedure RemoveUnitFromInterface(const AUnitName: string);


implementation

uses SysUtils, GX_OtaUtils, GX_GenericUtils;

function UseUnitInImplementation(const AUnitName: string): Boolean;
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    Result := AddToImpSection(AUnitName);
  finally
    Free;
  end;
end;

function UseUnitInInterface(const AUnitName: string): Boolean;
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    Result := AddToIntSection(AUnitName);
  finally
    Free;
  end;
end;

function GetUsesStatus(const AUnitName: string): TUsesStatus;
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    Result := GetUsesStatus(AUnitName);
  finally
    Free;
  end;
end;

procedure InternalGetUnits(Units: TStrings; FromIntf: Boolean);
var
  i: Integer;
  UsesList: TUsesList;
begin
  Assert(Assigned(Units));
  Units.Clear;
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    if FromIntf then
      UsesList := FInterfUses
    else
      UsesList := FImplemUses;
    for i := 0 to UsesList.Count - 1 do
      Units.Add(UsesList.Items[i].Name);
  finally
    Free;
  end;
end;

procedure GetImplementationUnits(Units: TStrings);
begin
  InternalGetUnits(Units, False);
end;

procedure GetInterfaceUnits(Units: TStrings);
begin
  InternalGetUnits(Units, True);
end;

procedure RemoveUnitFromImplementation(const AUnitName: string);
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    RemoveFromImplUses(AUnitName);
  finally
    Free;
  end;
end;

procedure RemoveUnitFromInterface(const AUnitName: string);
begin
  with TUsesManager.Create(GxOtaGetCurrentSourceEditor) do
  try
    RemoveFromIntfUses(AUnitName);
  finally
    Free;
  end;
end;

constructor TUsesManager.Create(const SourceEditor: IOTASourceEditor);
begin
  inherited Create;

  Assert(Assigned(SourceEditor));
  FFileName := SourceEditor.FileName;
  FFileContent := GxOtaReadEditorTextToString(SourceEditor.CreateReader);
  InternalInit;
end;

constructor TUsesManager.Create(const FileName: string);
var
  sl: TStringList;
begin
  inherited Create;

  FFileName := FileName;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FFileName);
    FFileContent := sl.Text;
  finally
    FreeAndNil(sl);
  end;
  InternalInit;
end;

procedure TUsesManager.InternalInit;
begin
  FInterfUses := TUsesList.Create;
  FImplemUses := TUsesList.Create;

  FParser := TmwPasLex.Create;
  FParser.Origin := @FFileContent[1];
  BuildUsesList;
end;

destructor TUsesManager.Destroy;
begin
  FreeAndNil(FParser);
  FreeAndNil(FInterfUses);
  FreeAndNil(FImplemUses);

  inherited Destroy;
end;

function TUsesManager.AddToImpSection(const AUnitName: string): Boolean;
begin
  Result := InternalAddToUsesSection(AUnitName, False);
end;

function TUsesManager.AddToIntSection(const AUnitName: string): Boolean;
begin
  Result := InternalAddToUsesSection(AUnitName, True);
end;

// Add a unit to a uses clause (may require removing it from the other clause)
function TUsesManager.InternalAddToUsesSection(const AUnitName: string; ToInterface: Boolean): Boolean;
var
  InsertPosition: Integer;
  LastUses: TUsesItem;
  InsertString: string;
  Status: TUsesStatus;
  UsesItems: TUsesList;
  UsesPos: Integer;
begin
  Result := False;
  Status := GetUsesStatus(AUnitName);
  if Status = usInsideUnit then
    Exit;

  if ToInterface then
  begin
    if Status = usImplementation then
      RemoveFromImplUses(AUnitName)
    else if Status = usInterface then
      Exit;
    UsesPos := FIntfPosition;
    UsesItems := FInterfUses;
  end
  else begin // Add to implementation
    if Status in [usInterface, usImplementation] then
      Exit;
    UsesPos := FImplPosition;
    UsesItems := FImplemUses;
  end;
  if UsesPos = 0 then
    Exit;

  // If a uses item exists
  if UsesItems.Count > 0 then
  begin
    // Retrieve the position after the last uses item
    LastUses := UsesItems.Items[UsesItems.Count - 1];
    InsertPosition := LastUses.EndPos;
    InsertString := ', ' + AUnitName;
    if UsesLineWouldBeTooLong(InsertPosition, Length(InsertString)) then
      InsertString := ',' + sLineBreak + '  ' + AUnitName;
    // Insert the new unit name into the uses clause
    GxOtaInsertTextIntoEditorAtPos(InsertString, InsertPosition);
  end
  else // The uses clause does not exist
  begin
    InsertString := sLineBreak + sLineBreak +'uses'+ sLineBreak +'  '+ AUnitName +';';
    GxOtaInsertTextIntoEditorAtPos(InsertString, UsesPos);
  end;

  // This needs to be done last since it changes the implementation offsets
  if not ToInterface then
  begin
    if Status = usInterface then
      RemoveFromIntfUses(AUnitName);
  end;

  Result := True;
end;

procedure TUsesManager.AddUnits(AUnits: TStrings; AToImplementation: Boolean);
var
  UnitName: string;
  i: Integer;
begin
  for i := 0 to AUnits.Count - 1 do
  begin
    UnitName := AUnits.Strings[i];
    if AToImplementation then
      AddToImpSection(UnitName)
    else
      AddToIntSection(UnitName);
  end;
end;

procedure TUsesManager.BuildUsesList;
var
  Section: (sImplementation, sInterface);
  InUses: Boolean;
  UsesItem: TUsesItem;
  LastCommaPos: Integer;
begin
  Section := sInterface;
  InUses := False;
  FParser.RunPos := 0;
  FBegOfImplUses := 0;
  FImplPosition := 0;
  FIntfPosition := 0;
  FEndOfIntfUses := 0;
  FBegOfIntfUses := 0;

  UsesItem := nil;
  LastCommaPos := 0;

  FParser.NextNoJunk;
  while FParser.TokenID <> tkNull do
  begin
    case FParser.TokenID of
      tkInterface:
        begin
          Section := sInterface;
          FIntfPosition := FParser.RunPos;
          InUses := False;
          LastCommaPos := 0;
        end;
      tkImplementation:
        begin
          Section := sImplementation;
          FImplPosition := FParser.RunPos;
          InUses := False;
          LastCommaPos := 0;
        end;
      tkUses:
        begin
          InUses := True;
          if Section = sImplementation then
            FBegOfImplUses := FParser.RunPos - Length('uses');
          if Section = sInterface then
            FBegOfIntfUses := FParser.RunPos - Length('uses');
          LastCommaPos := 0;
        end;
    else
      // If it is after the unit identifier
      if InUses and not (FParser.TokenID in [tkCompDirect, tkIn, tkString]) then
      begin
        if FParser.TokenID = tkIdentifier then
        begin
          if Section = sInterface then
            UsesItem := FInterfUses.Add
          else // Section = sImplementation
            UsesItem := FImplemUses.Add;
          {$IFOPT D+} Assert(UsesItem <> nil); {$ENDIF}

          UsesItem.Name := FParser.GetDottedIdentifierAtPos(True);
          UsesItem.EndPos := FParser.RunPos;
          UsesItem.BeginPos := UsesItem.EndPos - Length(UsesItem.Name);

          if LastCommaPos <> 0 then
            UsesItem.CommaBeforePos := LastCommaPos - 1;

          UsesItem.CommaAfterPos := 0;
        end // tkIdentifier
        else if FParser.TokenID = tkComma then
        begin
          LastCommaPos := FParser.RunPos;
          if UsesItem <> nil then
          begin
            UsesItem.CommaAfterPos := LastCommaPos - 1;
            if FParser.NextChar = ' ' then
              UsesItem.SpaceAfter := True;
          end;
        end
        else // FParser.TokenID <> tkComma
        begin
          InUses := False;
          if Section = sImplementation then
          begin
            FEndOfImplUses := FParser.RunPos;
            Break; // End of parsing
          end;
          if Section = sInterface then
            FEndOfIntfUses := FParser.RunPos;
        end; // Not comma
      end; // UsesFlag
    end;
    FParser.NextNoJunk;
  end;
end;

function TUsesManager.GetCurrentUnitName: string;
begin
  Result := Trim(ExtractPureFileName(FFileName));
end;

function TUsesManager.GetUsesStatus(const AUnitName: string): TUsesStatus;
begin
  if SameFileName(GetCurrentUnitName, Trim(AUnitName)) then
    Result := usInsideUnit
  else if FInterfUses.IndexOf(AUnitName) > -1 then
    Result := usInterface
  else if FImplemUses.IndexOf(AUnitName) > -1 then
    Result := usImplementation
  else
    Result := usNonExisting;
end;

{
  Remove a unit from a uses clause
  Algorithm to remove Unit1:
  If the unit at the start of the uses clause:
    uses Unit1;  // Only unit in the uses clause
     -> Remove the whole uses clause
    uses Unit1, Unit2; // There is a unit after Unit1
     -> Remove the unit name and trailing comma
  If the unit is the last one in the uses clause:
    uses Unit2, Unit1;
    -> Remove unitthe  name and comma before it
  If the unit the middle of the uses clause:
    uses Unit2, Unit1, Unit3; // Comma directly after the unit name
    -> The Unit name and trailing comma are deleted
    uses Unit2 ,Unit1 ,Unit3; // Comma not directly after the unit name
    -> The Unit name and preceeding comma are deleted
}
procedure TUsesManager.InternalRemoveFromUses(InInterface: Boolean; const AUnitName: string);
var
  DeletedUnit: TUsesItem;
  UnitIndex: Integer;
  BegPos, EndPos: Integer;
  UsesList: TUsesList;
begin
  if InInterface then
    UsesList := FInterfUses
  else
    UsesList := FImplemUses;

  UnitIndex := UsesList.IndexOf(AUnitName);
  if UnitIndex > -1 then
  begin
    // If this is the only uses unit, we delete the whole clause
    if UsesList.Count = 1 then
    begin
      if InInterface then
        RemoveInterfaceUses
      else
        RemoveImplementationUses;
    end
    else
    begin
      DeletedUnit := UsesList.Items[UnitIndex];
      if UnitIndex = 0 then // First in the uses clause
      begin
        if DeletedUnit.CommaAfterPos <> 0 then
          EndPos := DeletedUnit.CommaAfterPos + 1
        else
          EndPos := DeletedUnit.EndPos;
        BegPos := DeletedUnit.BeginPos;
      end
      else if UnitIndex = UsesList.Count-1 then // Last in the uses clause
      begin
        EndPos := DeletedUnit.EndPos;
        if DeletedUnit.CommaBeforePos <> 0 then
          BegPos := DeletedUnit.CommaBeforePos
        else
          BegPos := DeletedUnit.BeginPos;
      end
      else // In the middle of the uses clause
      begin
        if DeletedUnit.CommaAfterPos = DeletedUnit.EndPos then
        begin // Comma directly after unit
          BegPos := DeletedUnit.BeginPos;
          EndPos := DeletedUnit.CommaAfterPos + 1;
        end
        else // Comma before unit
        begin
          if DeletedUnit.CommaBeforePos <> 0 then
            BegPos := DeletedUnit.CommaBeforePos
          else
            BegPos := DeletedUnit.BeginPos;
          EndPos := DeletedUnit.EndPos;
        end;
      end;
      if DeletedUnit.SpaceAfter then
        Inc(EndPos);

      GxOtaDeleteTextFromPos(BegPos, EndPos - BegPos);
    end;
  end;
end;

// Remove the whole implementation uses clause
procedure TUsesManager.RemoveImplementationUses;
var
  BegIndex, Count: Integer;
begin
  if (FBegOfImplUses = 0) or (FEndOfImplUses = 0) then
    raise Exception.Create('RemoveImplementationUses: Begin or End of uses clause is not available!');

  BegIndex := FBegOfImplUses;
  Count := FEndOfImplUses - BegIndex;
  GxOtaDeleteTextFromPos(BegIndex, Count);
end;

procedure TUsesManager.RemoveFromImplUses(const AUnitName: string);
begin
  InternalRemoveFromUses(False, AUnitName);
end;

procedure TUsesManager.RemoveFromIntfUses(const AUnitName: string);
begin
  InternalRemoveFromUses(True, AUnitName);
end;

// Remove the whole interface uses clause
procedure TUsesManager.RemoveInterfaceUses;
var
  BegIndex, Count: Integer;
begin
  if (FBegOfIntfUses = 0) or (FEndOfIntfUses = 0) then
    raise Exception.Create('RemoveInterfaceUses: Begin or End of uses clause is not available!');

  BegIndex := FBegOfIntfUses;
  Count := FEndOfIntfUses - BegIndex;
  GxOtaDeleteTextFromPos(BegIndex, Count);
end;

function TUsesList.Add: TUsesItem;
begin
  Result := TUsesItem.Create;

  inherited Add(Result);
end;

function TUsesList.IndexOf(const AUnitName: string): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do
  begin
    if SameText(Items[Result].Name, AUnitName) then
      Break;
    Dec(Result);
  end;
end;

function TUsesList.GetItem(AIndex: Integer): TUsesItem;
begin
  Result := TUsesItem(Get(AIndex));
end;

procedure TUsesList.SetItem(AIndex: Integer; const Value: TUsesItem);
begin
  Put(AIndex, Value);
end;

function TUsesManager.UsesLineWouldBeTooLong(InsertPos, InsertLength: Integer): Boolean;
var
  EditView: IOTAEditView;
  InsertCharPos: TOTACharPos;
  InsertEditPos: TOTAEditPos;
begin
  EditView := GxOtaGetTopMostEditView;
  Assert(Assigned(EditView));
  InsertCharPos := GxOtaGetCharPosFromPos(InsertPos, EditView);
  EditView.ConvertPos(False, InsertEditPos, InsertCharPos);
  Result := (InsertEditPos.Col + InsertLength) > 80;
end;

function TUsesManager.IsPositionBeforeImplementation(Pos: Integer): Boolean;
begin
  Result := FImplPosition > Pos;
end;

end.

