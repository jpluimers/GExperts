unit GX_GrepPrinting;

interface

{$I GX_CondDefine.inc}

uses
  Classes,
  Controls;

type
  TGrepOutputMode = (grPrint, grCopy, grFile);
  TSaveToFileMode = (sfPrintToFile, sfSaveToLoadable, sfBoth);

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode; AFileName: String = '');
procedure SaveGrepResultsToLoadableFile(Owner: TWinControl; Results: TObject; AMode: TSaveToFileMode;
  AIniVersion: Integer; AFileName: String = '');

implementation

uses
  GX_GrepBackend, GX_GenericUtils,
  SysUtils, Graphics, ComCtrls, Dialogs, IniFiles;

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

procedure SaveResults(RichEdit: TRichEdit; AFileName: String; DoSaveDialog: Boolean);
begin
  RichEdit.PlainText := True;
  if not DoSaveDialog or OpenSaveDialog(AFileName) then
    RichEdit.Lines.SaveToFile(AFileName);
end;

procedure PrintGeneric(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode;
  AFileName: String; DoSaveDialog: Boolean);
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
  HistoryItem: TGrepHistoryListItems;
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
      if Results.Objects[0] is TFileResult then
        PrintResults(Results)
      else if Results.Objects[0] is TGrepHistoryListItems then
      begin
        for I := 0 to Results.Count-1 do
        begin
          if RichEdit.Lines.Count > 0 then
          begin
            RichEdit.Lines.Add('');  // space between file AResults
            RichEdit.Lines.Add('');
          end ;

          HistoryItem := TGrepHistoryListItems(Results.Objects[I]);

          RichEdit.SelAttributes.Style := [fsBold, fsUnderline];
          RichEdit.Lines.Add(HistoryItem.GrepSettings.Pattern);
          RichEdit.SelAttributes.Style := [];

          PrintResults(HistoryItem.ResultList);
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
      grFile: SaveResults(RichEdit, AFileName, DoSaveDialog);
    end;
  finally
    FreeAndNil(RichEdit);
  end;
end;

procedure PrintGrepResults(Owner: TWinControl; Results: TStrings; Where: TGrepOutputMode;
  AFileName: String);
begin
  PrintGeneric(Owner, Results, Where, AFileName, True);
end;

procedure SaveGrepResultsToLoadableFile(Owner: TWinControl; Results: TObject; AMode: TSaveToFileMode;
  AIniVersion: Integer; AFileName: String);
var
  AIni: TGrepIniFile;
begin
  if not OpenSaveDialog(AFileName) then
    Exit;

  if AMode <> sfSaveToLoadable then
    if Results is TGrepHistoryList then
      PrintGeneric(Owner, TGrepHistoryList(Results), grFile, AFileName, False)
    else if Results is TGrepHistoryListItems then
      PrintGeneric(Owner, TGrepHistoryListItems(Results).ResultList, grFile, AFileName, False);

  if AMode = sfPrintToFile then
    Exit;

  if (AMode <> sfBoth) and FileExists(AFileName) then
    DeleteFile(AFileName);

  AIni := TGrepIniFile.Create(AFileName);
  try
    if Results is TGrepHistoryList then
      TGrepHistoryList(Results).SaveToSettings(AIni, AIniVersion, '')
    else if Results is TGrepHistoryListItems then
    begin
      AIni.WriteInteger(TGrepHistoryList.KeyName, 'IniVersion', AIniVersion);
      TGrepHistoryListItems(Results).WriteToIni(AIni, TGrepHistoryListItems.SubKeyName);
    end;
  finally
    AIni.Free;
  end;
end;

end.
