unit GX_EditorExpertManager;

{$I GX_CondDefine.inc}

interface

uses
  Contnrs, GX_GenericClasses, GX_EditorExpert;

type
  TGxEditorExpertManager = class(TObject)
  private
    FEditorExpertList: TObjectList;
    function GetEditorExpert(const Index: Integer): TEditorExpert;
    function GetEditorExpertCount: Integer;
    procedure LoadEditorExperts;
    procedure FreeEditorExperts;
  public
    constructor Create;
    destructor Destroy; override;
    function FindExpert(const ExpertName: string; out Idx: integer): boolean;
    property EditorExpertList[const Index: Integer]: TEditorExpert read GetEditorExpert;
    property EditorExpertCount: Integer read GetEditorExpertCount;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, GX_ConfigurationInfo;

{ TGxEditorExpertManager }

constructor TGxEditorExpertManager.Create;
begin
  inherited Create;

  FEditorExpertList := TObjectList.Create(False);
  LoadEditorExperts;
end;

destructor TGxEditorExpertManager.Destroy;
begin
  FreeEditorExperts;

  FreeAndNil(FEditorExpertList);

  inherited Destroy;
end;

function TGxEditorExpertManager.GetEditorExpert(const Index: Integer): TEditorExpert;
begin
  if Index < GetEditorExpertCount then
    Result := FEditorExpertList[Index] as TEditorExpert
  else
    Result := nil;
end;

function TGxEditorExpertManager.GetEditorExpertCount: Integer;
begin
  Result := FEditorExpertList.Count;
end;


procedure TGxEditorExpertManager.LoadEditorExperts;
var
  i: Integer;
  EditorExpert: TEditorExpert;
begin
  ConfigInfo.EditorExpertsEnabled := True;
  for i := 0 to EditorExpertClassList.Count - 1 do
  begin
    EditorExpert := GetExpertClassByIndex(i).Create;
    EditorExpert.LoadSettings;
    FEditorExpertList.Add(EditorExpert);
  end;
end;

function TGxEditorExpertManager.FindExpert(const ExpertName: string; out Idx: integer): boolean;
var
  i: Integer;
begin
  for i := 0 to EditorExpertCount - 1 do
  begin
    if SameText(EditorExpertList[i].GetName, ExpertName) then
    begin
      Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGxEditorExpertManager.FreeEditorExperts;
var
  i: Integer;
begin
  {$IFOPT D+} SendDebug('Freeing the editor experts'); {$ENDIF}
  if FEditorExpertList <> nil then
  begin
    for i := 0 to FEditorExpertList.Count - 1 do
      FEditorExpertList[i].Free;

    FEditorExpertList.Clear;
  end;
end;

end.
