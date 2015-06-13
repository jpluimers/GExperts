// Current identifier location editor expert
// Contributed to GExperts by Max Vlasov <maksee@mail.ru>
// Additional edits by Erik Berry

unit GX_ePrevNextIdentifier;

interface

uses
  Classes, GX_EditorExpert;

type
  TBaseIdentExpert = class(TEditorExpert)
  private
    FSource: string;
    FPosition: Integer;
    procedure SetPosition(Value: Integer);
  protected
    // Source and Position are valid only inside InternalExecute
    property Source: string read FSource;
    property Position: Integer read FPosition write SetPosition;
    procedure InternalExecute; virtual; abstract;
  public
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

  TPrevIdentExpert = class(TBaseIdentExpert)
  private
    Previous: Boolean;
  protected
    function FindIdentAction(const Source: string; Pos: Integer;
      var FoundPos: Integer; var Ident: string): Boolean;
    procedure InternalExecute; override;
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TNextIdentExpert = class(TPrevIdentExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
  end;

implementation

uses
  SysUtils, Windows, ToolsAPI,
  GX_GenericUtils, GX_OtaUtils, GX_EditReader;

resourcestring
  SIdentHelpString =
    '  This expert detects the identifier under the cursor and ' +
    'allow you to quickly jump to the %s occurrence ' +
    'of that identifier in the same file.';

function CurrentIdent(const Source: string; CurPos: Integer;
  var Pos, Len: Integer): Boolean;
begin
  Result := False;

  while CurPos >= 1 do
    if IsCharIdentifier(Source[CurPos]) then
    begin
      Dec(CurPos);
      Result := True;
    end
    else if (not Result) and (CurPos >= 2) then
      if IsCharIdentifier(Source[CurPos - 1]) then
      begin
        Dec(CurPos, 2);
        Result := True;
      end
      else
        Break
    else
      Break;

  if Result then
  begin
    Pos := CurPos + 1;
    Inc(CurPos, 2);
    while (CurPos >= 1) and (CurPos <= Length(Source)) do
      if IsCharIdentifier(Source[CurPos]) then
        Inc(CurPos)
      else
        Break;

    Len := CurPos - Pos;
  end;
end;

function FindIdentAtPos(const Source: string; CurPos: Integer; Prev: Boolean;
  var Pos: Integer; var Ident: string): Boolean;
var
  StartPos: Integer;
  Id: string;
  Len: Integer;
begin
  Result := False;

  if CurrentIdent(Source, CurPos, StartPos, Len) then
  begin
    Id := Copy(Source, StartPos, Len);
    Result := FindTextIdent(Id, Source, StartPos, Prev, Pos);
    Ident := Id;
  end;
end;

{ TBaseIdentExpert }

procedure TBaseIdentExpert.SetPosition(Value: Integer);
begin
  GxOtaSetCurrentSourcePosition(Value);
end;

procedure TBaseIdentExpert.Execute;
var
  EditRead: TEditReader;
  SourceEditor: IOTASourceEditor;
begin
  FSource := '';
  FPosition := -1;
  SourceEditor := GxOtaGetCurrentSourceEditor;
  if SourceEditor = nil then
    Exit;

  EditRead := TEditReader.Create(SourceEditor.FileName);
  try
    FSource := EditRead.GetText;
    FPosition := EditRead.GetCurrentBufferPos + 1;
  finally
    FreeAndNil(EditRead);
  end;

  InternalExecute;
end;

function TBaseIdentExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TPrevIdentExpert }

constructor TPrevIdentExpert.Create;
begin
  inherited Create;
  Previous := True;
  if Self.ClassName = 'TPrevIdentExpert' then
    ShortCut := scCtrl + scAlt + VK_UP;
end;

procedure TPrevIdentExpert.InternalExecute;
var
  FoundPos: Integer;
  Ident: string;
begin
  if FindIdentAction(Source, Position, FoundPos, Ident) then
    Position := FoundPos
  else
    MessageBeep($FFFFFFFF);
end;

function TPrevIdentExpert.FindIdentAction(const Source: string; Pos: Integer;
  var FoundPos: Integer; var Ident: string): Boolean;
begin
  Result := FindIdentAtPos(Source, Pos, Previous, FoundPos, Ident);
end;

procedure TPrevIdentExpert.GetHelpString(List: TStrings);
begin
  List.Text := Format(SIdentHelpString, ['previous']);
end;

function TPrevIdentExpert.GetDisplayName: string;
begin
  Result := 'Previous Identifier Reference';
end;

class function TPrevIdentExpert.GetName: string;
begin
  Result := 'PreviousIdent';
end;

{ TNextIdentExpert }

constructor TNextIdentExpert.Create;
begin
  inherited Create;
  Previous := False;
  ShortCut := scCtrl + scAlt + VK_DOWN;
end;

function TNextIdentExpert.GetDisplayName: string;
begin
  Result := 'Next Identifier Reference';
end;

procedure TNextIdentExpert.GetHelpString(List: TStrings);
begin
  List.Text := Format(SIdentHelpString, ['next']);
end;

class function TNextIdentExpert.GetName: string;
begin
  Result := 'NextIdent';
end;

initialization
  RegisterEditorExpert(TPrevIdentExpert);
  RegisterEditorExpert(TNextIdentExpert);

end.

