unit GX_eSelectionEditorExpert;

interface

uses
  Classes, ToolsAPI, GX_EditorExpert;

type
  TNoSelectionMode = (nsmSelectLine, nsmSelectIdent, nsmError);

  TSelectionEditorExpert = class(TEditorExpert)
  private
    function BlockSelectionToLineEndType(SouceEditor: IOTASourceEditor;const Selection: string): Integer;
    procedure InternalExecute;
  protected
    // Return True to let the changed lines get copied back into the editor
    function ProcessSelected(Lines: TStrings): Boolean; virtual; abstract;
    function GetNoSelectionMode: TNoSelectionMode; virtual;
  public
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, GX_OtaUtils;

{ TSelectionEditorExpert }

// WARNING: This is a hack.
// GxOtaReplaceSelection has a problem with appending an
// unwanted CRLF for multiline selections to the line end
// The clean solution is just to call
//    GxOtaReplaceSelection(Editor, 0, CodeList.Text);
// and have the problem solved elsewhere, but this is hard...
// We don't remove the CRLF if the block ends in the first column,
// because then we delete a necessary CRLF.
// TODO 4 -cKylix -oAnyone: Test LF detection under Kylix
function TSelectionEditorExpert.BlockSelectionToLineEndType(SouceEditor: IOTASourceEditor;
  const Selection: string): Integer;
var
  View: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  Result := 0;
  View := GxOtaGetTopMostEditView(SouceEditor);
  Assert(Assigned(View));
  CharPos.Line := View.Block.EndingRow;
  CharPos.CharIndex := View.Block.EndingColumn;
  View.ConvertPos(False, EditPos, CharPos);
  // TODO 3 -cIssue -oAnyone: Why is a 2 necessary for this column check (tested in Delphi 6)
  if (Length(Selection) > 1) and (EditPos.Col > 2) then
  begin
    if Copy(Selection, Length(Selection) - 1, 2) = #13#10 then
      Result := 2
    else if Selection[Length(Selection)] = #10 then
      Result := 1;
  end;
end;

procedure TSelectionEditorExpert.Execute(Sender: TObject);
begin
  InternalExecute;
end;

function TSelectionEditorExpert.GetNoSelectionMode: TNoSelectionMode;
begin
  Result := nsmSelectLine;
end;

procedure TSelectionEditorExpert.InternalExecute;
resourcestring
  NoSelectionError = 'Please select some text first.';
var
  SourceEditor: IOTASourceEditor;
  CodeList: TStrings;
  LineEndType: Integer;
begin
  {$IFOPT D+} SendDebug('Executing TSelectionEditorExpert'); {$ENDIF}
  CodeList := TStringList.Create;
  try
    SourceEditor := GxOtaGetCurrentSourceEditor;
    if SourceEditor = nil then
      Exit;
    CodeList.Text := GxOtaGetCurrentSelection;

    if CodeList.Count = 0 then
    begin
      case GetNoSelectionMode of
        nsmSelectLine: GxOtaSelectCurrentLine(SourceEditor, False);
        nsmSelectIdent: GxOtaSelectCurrentIdent(SourceEditor, False);
        nsmError: raise Exception.Create(NoSelectionError);
      end;
      CodeList.Text := GxOtaGetCurrentSelection;
    end;

    if CodeList.Count = 0 then
      Exit;

    if ProcessSelected(CodeList) then
    begin
      {$IFOPT D+}SendDebug('Calling GxOtaReplaceSelection');{$ENDIF}
      if CodeList.Count = 1 then
        GxOtaReplaceSelection(SourceEditor, 0, TrimRight(CodeList.Text))
      else
      begin
        LineEndType := BlockSelectionToLineEndType(SourceEditor, CodeList.Text);
        if LineEndType = 2 then
          GxOtaReplaceSelection(SourceEditor, 0, Copy(CodeList.Text, 1,
            Length(CodeList.Text) - 2))
        else if LineEndType = 1 then
          GxOtaReplaceSelection(SourceEditor, 0, Copy(CodeList.Text, 1,
            Length(CodeList.Text) - 1))
        else
          GxOtaReplaceSelection(SourceEditor, 0, CodeList.Text);
      end;
    end;
  finally
    FreeAndNil(CodeList);
  end;
end;

end.

