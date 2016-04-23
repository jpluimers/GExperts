unit GX_GrepPrinting;

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, GX_GrepBackend;

type
  TGrepOutputMode = (grPrint, grCopy, grFile);
  TSaveToFileMode = (sfPrintToFile, sfSaveToLoadable, sfBoth);

procedure SaveGrepResultsToFile(Owner: TWinControl; Results: TGrepHistoryList; AIniVersion: Integer;
  ASaveAll: Boolean; AMode: TSaveToFileMode; Where: TGrepOutputMode = grFile;
  AFileName: String = ''; ASplitCount: Integer = -1);

implementation

uses
  GX_GenericUtils,
  SysUtils, Graphics, ComCtrls, Dialogs, IniFiles, StrUtils, GX_GrepProgress;

function OpenSaveDialog(var AFileName: String): Boolean;
var
  SaveDlg: TSaveDialog;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.DefaultExt := 'txt';
    if Trim(AFileName) <> '' then
    begin
      if Trim(SaveDlg.FileName) <> '' then
        SaveDlg.InitialDir := ExtractFilePath(SaveDlg.FileName);
      SaveDlg.FileName := AFileName + '.' + SaveDlg.DefaultExt;
    end;
    SaveDlg.Filter := 'Text Files (*.txt, *.log)|*.txt;*.log|All Files (' +AllFilesWildCard+ ')|' + AllFilesWildCard;
    SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];
    Result := GetOpenSaveDialogExecute(SaveDlg);
    if Result then
      AFileName := SaveDlg.FileName;
  finally
    FreeAndNil(SaveDlg);
  end;
end;

procedure SaveResults(RichEdit: TRichEdit; AFileName: string; DoSaveDialog: Boolean);
begin
  RichEdit.PlainText := True;
  if not DoSaveDialog or OpenSaveDialog(AFileName) then
    RichEdit.Lines.SaveToFile(AFileName);
end;

procedure PrintGeneric(Owner: TWinControl; Results: TGrepHistorySortableList; Where: TGrepOutputMode;
  const AFileName: string; DoSaveDialog, ASaveAll: Boolean; ASplitCount: Integer);
var
  RichEdit: TRichEdit;

  procedure PrintResults(AResults: TStrings);
  var
    FileResult: TFileResult;
    Line: string;
    LinePos: Integer;
    i, j, c: Integer;
    AMatchResult: TMatchResult;
    MIndx: Integer;
  begin
    for i := 0 to AResults.Count - 1 do
    begin
      if AResults.Objects[i] is TFileResult then
      begin
        if RichEdit.Lines.Count > 0 then
          RichEdit.Lines.Add('');  // space between file AResults

        FileResult := TFileResult(AResults.Objects[i]);

        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.Lines.Add(FileResult.FileName);
        RichEdit.SelAttributes.Style := [];

        for j := 0 to FileResult.Count - 1 do
        begin
          LinePos := RichEdit.GetTextLen;
          Line := FileResult.Items[j].Line;
          c := LeftTrimChars(Line);

          RichEdit.Lines.Add(Format('  %5d'#9, [FileResult.Items[j].LineNo]) + Line);
          // Now make the found Text bold
          for MIndx := 0 to  FileResult.Items[j].Matches.Count-1 do
          begin
            AMatchResult := FileResult.Items[j].Matches[MIndx];
            RichEdit.SelStart := LinePos + 7 - c + AMatchResult.SPos;
            RichEdit.SelLength := AMatchResult.EPos - AMatchResult.SPos + 1;
            RichEdit.SelAttributes.Style := [fsBold];
            RichEdit.SelLength := 0;
            RichEdit.SelAttributes.Style := [];
          end;
        end;
      end;
    end;
  end;

var
  HistoryItem: TGrepHistoryListItem;
  I: Integer;
begin
  if Results.Count = 0 then
    Exit;

  RichEdit := TRichEdit.Create(Owner);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Owner;
    RichEdit.Font.Name := 'Arial';
    RichEdit.Font.Size := 10;
    RichEdit.Clear;
    RichEdit.Lines.BeginUpdate;
    try
      for I := 0 to Results.Count-1 do
      begin
        HistoryItem := Results.Items[I];

        if not ASaveAll and not HistoryItem.Checked then
          Continue;

        if RichEdit.Lines.Count > 0 then
        begin
          RichEdit.Lines.Add('');  // space between file AResults
          RichEdit.Lines.Add('');
        end ;

        RichEdit.SelAttributes.Style := [fsBold, fsUnderline];
        RichEdit.Lines.Add(HistoryItem.SearchText);
        RichEdit.SelAttributes.Style := [];

        PrintResults(HistoryItem.ResultList);

        if ASplitCount > 0 then
        begin
          Dec(ASplitCount);
          if ASplitCount = 0 then
            Break;
        end ;
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
      grFile: SaveResults(RichEdit, AFileName, DoSaveDialog);
    end;
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure SaveGrepResultsToFile(Owner: TWinControl; Results: TGrepHistoryList; AIniVersion: Integer;
  ASaveAll: Boolean; AMode: TSaveToFileMode; Where: TGrepOutputMode; AFileName: String; ASplitCount: Integer);
var
  AIni: TGrepIniFile;
  AFileNameExt, AFileNameForSave: String;
  AFileIndex: Integer;
begin
  if (Where = grFile) and not OpenSaveDialog(AFileName) then
    Exit;

  TfmGrepProgress.Start;

  AFileNameExt := ExtractFileExt(AFileName);
  AFileName := ChangeFileExt(AFileName, '');

  AFileIndex := 0;
  repeat
    AFileNameForSave := AFileName + IfThen(AFileIndex > 0, IntToStr(AFileIndex), '') + AFileNameExt;

    TfmGrepProgress.Progressing(AFileNameForSave);
    if AMode <> sfSaveToLoadable then
      PrintGeneric(Owner, Results.HistoryList, Where, AFileName, False, ASaveAll, ASplitCount);

    if AMode <> sfSaveToLoadable then
      TfmGrepProgress.Progressed;

    if AMode = sfPrintToFile then
      Exit;

    if (AMode <> sfBoth) and FileExists(AFileNameForSave) then
      DeleteFile(AFileNameForSave);

    AIni := TGrepIniFile.Create(AFileNameForSave);
    try
      Results.SaveToSettings(AIni, AIniVersion, ifmSingle, '', ASaveAll, False, False, 0, ASplitCount);
    finally
      AIni.Free;
    end;
    TfmGrepProgress.Progressed;

    Inc(AFileIndex);
  until not Results.AnyChecked;
end;

end.
