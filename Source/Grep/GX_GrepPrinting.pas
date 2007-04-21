unit GX_GrepPrinting;

interface

{$I GX_CondDefine.inc}

uses
  Classes,
  Controls;

type
  TGrepOutputMode = (grPrint, grCopy, grFile);

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode);

implementation

uses
  GX_GrepBackend, GX_GenericUtils,
  SysUtils, Graphics, ComCtrls, Dialogs;

procedure SaveResults(RichEdit: TRichEdit);
var
  SaveDlg: TSaveDialog;
begin
  RichEdit.PlainText := True;
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.DefaultExt := 'txt';
    SaveDlg.Filter := 'Text Files (*.txt, *.log)|*.txt;*.log|All Files (*.*)|*.*';
    SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];
    if GetOpenSaveDialogExecute(SaveDlg) then
      RichEdit.Lines.SaveToFile(SaveDlg.FileName);
  finally
    FreeAndNil(SaveDlg);
  end;
end;

procedure PrintGeneric(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode);
var
  RichEdit: TRichEdit;
  FileResult: TFileResult;
  Line: string;
  i, j, c: Integer;
  LinePos: Integer;
  AMatchResult: TMatchResult;
  MIndx: Integer;
begin
  RichEdit := TRichEdit.Create(Owner);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Owner;
    RichEdit.Font.Name := 'Arial';
    RichEdit.Font.Size := 10;
    RichEdit.Clear;
    RichEdit.Lines.BeginUpdate;
    try
      for i := 0 to Results.Count - 1 do
      begin
        if Results.Objects[i] is TFileResult then
        begin
          RichEdit.Lines.Add('');  // space between file results

          FileResult := TFileResult(Results.Objects[i]);

          RichEdit.SelAttributes.Style := [fsBold];
          RichEdit.Lines.Add(FileResult.FileName);
          RichEdit.SelAttributes.Style := [];

          for j := 0 to FileResult.Count - 1 do
          begin
            LinePos := RichEdit.GetTextLen;
            Line := FileResult.Items[j].Line;
            c := LeftTrimChars(Line);
            with RichEdit do
            begin
              Lines.Add(Format('  %5d'#9, [FileResult.Items[j].LineNo]) + Line);
              // Now make the found Text bold
              for MIndx := 0 to  FileResult.Items[j].Matches.Count-1 do
              begin
                AMatchResult := FileResult.Items[j].Matches[MIndx];
                SelStart := LinePos + 7 - c + AMatchResult.SPos;
                SelLength := AMatchResult.EPos - AMatchResult.SPos + 1;
                SelAttributes.Style := [fsBold];
                SelLength := 0;
                SelAttributes.Style := [];
              end;
            end;
          end;
        end;
      end;
    finally
      RichEdit.Lines.EndUpdate;
    end;
    case Where of
      grPrint: RichEdit.Print('GExperts - Grep Search Results');
      grCopy:
        begin
          RichEdit.SelectAll;
          RichEdit.CopyToClipboard;
        end;
      grFile: SaveResults(RichEdit);
    end;
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode);
begin
  PrintGeneric(Owner, Results, Where);
end;

end.
