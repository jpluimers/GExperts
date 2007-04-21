unit EditorStrings;

{ Adapted from Hidden Paths of Delphi 3, by Ray Lischner (converted to new OTA)
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.
}

{$I GX_CondDefine.inc}

interface

uses
  Classes, ToolsAPI;

type
  TEditorStrings = class(TStrings)
  private
    fStrings: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Str: string); override;
    procedure PutObject(Index: Integer; Obj: TObject); override;
    function GetPosition(Index: Integer): Longint; virtual;
    function GetCharPos(Index: Integer): TOTACharPos; virtual;
    property Strings: TStrings read fStrings;
  public
    constructor Create(Editor: IOTASourceEditor);
    destructor Destroy; override;
    procedure LoadFromEditor(Editor: IOTASourceEditor);
    procedure SaveToEditor(Editor: IOTASourceEditor);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Str: string); override;
    function PosToCharPos(Pos: Longint): TOTACharPos;
    function CharPosToPos(CharPos: TOTACharPos): Longint;
    property Position[Index: Integer]: Longint read GetPosition;
    property CharPos[Index: Integer]: TOTACharPos read GetCharPos;
  end;

implementation

uses SysUtils, GX_OtaUtils;

{ TEditorStrings }

{ Create an edit reader string list. }
constructor TEditorStrings.Create(Editor: IOTASourceEditor);
begin
  inherited Create;
  fStrings := TStringList.Create;
  LoadFromEditor(Editor);
end;

destructor TEditorStrings.Destroy;
begin
  FreeAndNil(fStrings);

  inherited Destroy;
end;

{ Load a string list from an editor interface. Read the edit
  reader as a stream. As each line is added to the string list,
  remember the position of that line in the stream. }
procedure TEditorStrings.LoadFromEditor(Editor: IOTASourceEditor);
var
  StrStream: TStringStream;
  Str: PChar;
  Pos, I: Integer;
begin
  StrStream := TStringStream.Create('');
  try
    { Read the entire buffer into StrStream. }
    GxOtaSaveReaderToStream(Editor.CreateReader, StrStream);
    { Copy every line from StrStream to the string list. }
    Strings.Text := StrStream.DataString;

    { Scan the string to find the buffer position of each line. }
    Str := PChar(StrStream.DataString);
    Pos := 0;
    for I := 0 to Count-1 do
    begin
      Strings.Objects[I] := TObject(Pos);
      Inc(Pos, Length(Strings[I]));
      if Str[Pos] = #13 then // TODO -oStefan -cCRLF: Check
        Inc(Pos);
      if Str[Pos] = #10 then
        Inc(Pos);
    end;
  finally
    FreeAndNil(StrStream);
  end;
end;

{ Save the string list to an editor interface. The string list
  does not keep track of specific changes, so replace the entire
  file with the text of the string list. }
procedure TEditorStrings.SaveToEditor(Editor: IOTASourceEditor);
var
  Writer: IOTAEditWriter;
begin
  Writer := GxOtaGetEditWriterForSourceEditor(Editor);
  try
    Writer.DeleteTo(High(Longint));
    Writer.Insert(PChar(ConvertToIDEEditorString(fStrings.Text)));
  finally
    Writer := nil;
  end;
end;

{ Get a string. }
function TEditorStrings.Get(Index: Integer): string;
begin
  Result := Strings[Index];
end;

{ Get an object, which is really the string position. }
function TEditorStrings.GetObject(Index: Integer): TObject;
begin
  Result := Strings.Objects[Index];
end;

{ Set a string. }
procedure TEditorStrings.Put(Index: Integer; const Str: string);
begin
  Strings[Index] := Str;
end;

{ Set a string's position. }
procedure TEditorStrings.PutObject(Index: Integer; Obj: TObject);
begin
  Objects[Index] := Obj;
end;

{ Return the number of lines in the list. }
function TEditorStrings.GetCount: Integer;
begin
  Result := Strings.Count;
end;

procedure TEditorStrings.Clear;
begin
  Strings.Clear;
end;

procedure TEditorStrings.Delete(Index: Integer);
begin
  Strings.Delete(Index);
end;

procedure TEditorStrings.Insert(Index: Integer; const Str: string);
begin
  Strings.Insert(Index, Str);
end;

{ For convenience, return a position as an Integer. }
function TEditorStrings.GetPosition(Index: Integer): Longint;
begin
  Result := Longint(Strings.Objects[Index]);
end;

{ Return a position as a character position. }
function TEditorStrings.GetCharPos(Index: Integer): TOTACharPos;
begin
  Result := PosToCharPos(GetPosition(Index));
end;

{ Get the buffer position given a character position.
  The character position position specifies a line of text.
  Retrieve the buffer position for the start of that line,
  and add the character index. If the character index is
  past the end of line, return the position of the line
  ending. }
function TEditorStrings.CharPosToPos(CharPos: TOTACharPos): Longint;
var
  Text: string;
begin
  { CharPos.Line is 1-based; Strings list is 0-based. }
  Text := Strings[CharPos.Line-1];
  if CharPos.CharIndex > Length(Text) then
    Result := Position[CharPos.Line-1] + Length(Text)
  else
    Result := Position[CharPos.Line-1] + CharPos.CharIndex;
end;

{ Convert a buffer position to a character position.
  Search for the line such that Pos is between the start
  and end positions of the line. That specifies the line
  number. The char index is the offset within the line.
  If Pos lies within a line ending, return the character
  index of the end of the line.

  Line indices are 1-based, and string list indices are
  0-based, so add 1 to get the real line number.

  Use binary search to locate the desired line quickly. }
function TEditorStrings.PosToCharPos(Pos: Longint): TOTACharPos;
var
  Lo, Mid, Hi: Integer;
begin
  Lo := 0;
  Hi := Strings.Count-1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Position[Mid] <= Pos then
      Lo := Mid+1
    else
      Hi := Mid-1
  end;

  Result.Line := Lo;
  if Pos >= Position[Lo-1]+Length(Strings[Lo-1]) then
    Result.CharIndex := Length(Strings[Lo-1])
  else
    Result.CharIndex := Pos - Position[Lo-1];
end;

end.
