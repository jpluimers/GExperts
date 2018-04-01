unit GX_UnitPositions;

interface

uses
  Classes, ToolsAPI, mPasLex;

type
  TUnitPosition = record
    Name: string;
    Position: Integer;
  end;

  TUnitPositions = class(TObject)
  private
    FParser: TmwPasLex;
    FFileContent: string;
    FPosList: TStringList;
    procedure GetPositions;
    function GetCount: Integer;
    function GetPosition(Index: Integer): TUnitPosition;
  public
    constructor Create(const SourceEditor: IOTASourceEditor);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Positions[Index: Integer]: TUnitPosition read GetPosition;
  end;

implementation

uses
  SysUtils, GX_OtaUtils, GX_GenericUtils, mwPasParserTypes;

resourcestring
  // todo: Why are these ResourceStrings? They will never get translated.
  SUnitName = 'unit';
  SProgramName = 'program';
  SLibraryName = 'library';
  SPackageName = 'package';
  SRequiresName = 'requires';
  SContainsName = 'contains';
  SInterfaceName = 'interface';
  SInterfaceUsesName = 'interface uses';
  SImplementationName = 'implementation';
  SImplementationUsesName = 'implementation uses';
  SInitializationName = 'initialization';
  SFinalizationName = 'finalization';
  SBeginName = 'begin';
  SEndName = 'end.';

{ TUnitPositions }

constructor TUnitPositions.Create(const SourceEditor: IOTASourceEditor);
begin
  inherited Create;

  Assert(Assigned(SourceEditor));
  FParser := nil;
  FPosList := TStringList.Create;
  FFileContent := GxOtaReadEditorTextToString(SourceEditor.CreateReader);
  FParser := TmwPasLex.Create;
  FParser.Origin := @FFileContent[1];
  GetPositions;
end;

destructor TUnitPositions.Destroy;
begin
  FreeAndNil(FPosList);
  FreeAndNil(FParser);
  inherited Destroy;
end;

function TUnitPositions.GetCount: Integer;
begin
  Result := FPosList.Count;
end;

function TUnitPositions.GetPosition(Index: Integer): TUnitPosition;
begin
  Result.Name := FPosList[Index];
  Result.Position := NativeInt(FPosList.Objects[Index]);
end;

procedure TUnitPositions.GetPositions;
var
  Section: (sUnknown, sInterface, sImplementation);
  EndPos: Integer;
  IsProgramOrLibrary: Boolean;
  FoundBeginAlready: Boolean;
begin
  Section := sUnknown;
  FParser.RunPos := 0;
  IsProgramOrLibrary := False;
  FoundBeginAlready := False;

  EndPos := -1;
  while FParser.TokenID <> tkNull do
  begin
    case FParser.TokenID of
      tkEnd: // Only the last 'end' will be recorded (see below)
        EndPos := FParser.TokenPos;
      tkBegin:
        begin
          if IsProgramOrLibrary and not FoundBeginAlready then
          begin
            FPosList.AddObject(SBeginName, TObject(FParser.TokenPos));
            FoundBeginAlready := True;
          end;
        end;
      tkProgram:
        begin
          FPosList.AddObject(SProgramName, TObject(FParser.TokenPos));
          IsProgramOrLibrary := True;
        end;
      tkLibrary:
        begin
          FPosList.AddObject(SLibraryName, TObject(FParser.TokenPos));
          IsProgramOrLibrary := True;
        end;
      tkPackage:
        begin
          FPosList.AddObject(SPackageName, TObject(FParser.TokenPos));
          IsProgramOrLibrary := True;
        end;
      tkUnit:
        FPosList.AddObject(SUnitName, TObject(FParser.TokenPos));
      tkFinalization:
        FPosList.AddObject(SFinalizationName, TObject(FParser.TokenPos));
      tkInitialization:
        FPosList.AddObject(SInitializationName, TObject(FParser.TokenPos));
      tkInterface:
        begin
          if Section = sUnknown then
          begin
            FPosList.AddObject(SInterfaceName, TObject(FParser.TokenPos));
            Section := sInterface;
          end;
        end;
      tkImplementation:
        begin
          Section := sImplementation;
          FPosList.AddObject(SImplementationName, TObject(FParser.TokenPos));
        end;
      tkContains:
        begin
          FPosList.AddObject(SContainsName, TObject(FParser.TokenPos));
        end;
      tkRequires:
        begin
          FPosList.AddObject(SRequiresName, TObject(FParser.TokenPos));
        end;
      tkUses:
        begin
          if Section = sImplementation then
            FPosList.AddObject(SImplementationUsesName, TObject(FParser.TokenPos))
          else
            FPosList.AddObject(SInterfaceUsesName, TObject(FParser.TokenPos));
        end;
    else // FI:W506
      // Ignore the token
    end;
    FParser.NextNoJunk;
  end;
  if EndPos <> -1 then
    FPosList.AddObject(SEndName, TObject(EndPos));
end;

end.
