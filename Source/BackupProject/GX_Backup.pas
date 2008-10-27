unit GX_Backup;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, ExtCtrls, Dialogs, StdCtrls,
  ToolsAPI, GX_Progress, GX_Experts, GX_ConfigurationInfo,
  GX_Zipper, AbArcTyp, AbUtils;

type
  TBackupExpert = class;
  
  TBackupType = (btFile, btDir);

  TBackupScope = (bsActiveProject, bsProjectGroup);

  TfmBackup = class(TForm)
    pnlButtons: TPanel;
    pnlFiles: TPanel;
    gbxFiles: TGroupBox;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    pnlButtonsRight: TPanel;
    btnBackup: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    pnlFileList: TPanel;
    lbFiles: TListBox;
    pnlFileButtons: TPanel;
    pnlFileButtonsRight: TPanel;
    btnOptions: TButton;
    btnRemove: TButton;
    btnAdd: TButton;
    procedure btnBackupClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure lbFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDoAbortCollectingFiles: Boolean;
    FHaveCollectedFiles: Boolean;
    FZipEncrypted: Boolean;
    FZipPassword: string;
    FProgressForm: TfmProgress;
    FCurrentBackupScope: TBackupScope;
    FBackupExpert: TBackupExpert;
    FLibraryPath: TStringList;
    FFilesFoundNowhere: TStringList;
    FZipComponent: TGXZipper;
    procedure AbbreviaProgress(Sender : TObject; Progress : Byte; var Abort : Boolean);
    procedure FileFailure(Sender: TObject; Item: TAbArchiveItem;
      ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: Integer);
    procedure AfterFileListChange;
    function ListFiles(const FileName, UnitName, FormName: string): Boolean;
    procedure LocateFileOnPathAndAdd(FilesNotFound: TStrings);
    procedure DoCollectFilesFromSingleProject(IProject: IOTAProject);
    procedure DoCollectFiles;
    procedure PerformBackup(const Path, FileName: string);
    procedure AddBackupFile(const FileName: string);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure CollectFilesForBackup;
    procedure IncrementProgress;
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBackupExpert = class(TGX_Expert)
  private
    FFollowLibraryPath: Boolean;
    FBackupInc: Boolean;
    FBackupType: TBackupType;
    FBackupDir: string;
    FIncludeDir: Boolean;
    FBackupScope: TBackupScope;
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;

    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;

    property BackupDir: string read FBackupDir;
    property BackupScope: TBackupScope read FBackupScope;
    property BackupType: TBackupType read FBackupType;
    property DoBackupIncludedFiles: Boolean read FBackupInc;
    property DoIncludeDirInfoInZip: Boolean read FIncludeDir;
    property FollowLibraryPath: Boolean read FFollowLibraryPath;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows, SysUtils, AbConst,
  GX_GenericUtils, GX_OtaUtils, GX_MacroParser, GX_GxUtils,
  GX_BackupOptions, GX_BackupConfig, StrUtils;

const // Do not localize these constants.
  ItemSeparatorChar = '|';
  BackupIncludeDirective: array[0..8] of string =
  ( '{$I ',  '{$INCLUDE ',  '{$RESOURCE ',  '{$R ', '{#BACKUP ',
   '(*$I ', '(*$INCLUDE ', '(*$RESOURCE ', '(*$R ');

procedure AddFileToList(const FileSpec: string; List: TStrings);
var
  StoreFileName: string;
begin
  Assert(Assigned(List));
  StoreFileName := Trim(FileSpec);
  if (StoreFileName <> '') and (List.IndexOf(FileSpec) = -1) then
    List.Add(FileSpec);
end;

procedure ScanForIncludesAndAdd(const FileName: string;
  FoundFileList, NotFoundFileList: TStrings);

  function GetIncludePrefix(const Line: string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := Low(BackupIncludeDirective) to High(BackupIncludeDirective) do
    begin
      if CaseInsensitivePos(BackupIncludeDirective[i], Line) = 1 then
      begin
        Result := BackupIncludeDirective[i];
        Break;
      end;
    end;
  end;

  procedure GetPasIncludeLines(FileLines: TStrings; IncludeLines: TStrings);
  var
    i: Integer;
    Line: string;
    IncludePrefix: string;
    FileSpec: string;
    CommentPos: Integer;
  begin
    Assert(Assigned(FileLines));
    Assert(Assigned(IncludeLines));

    IncludeLines.Clear;
    for i := 0 to FileLines.Count - 1 do
    begin
      Line := Trim(FileLines[i]);
      IncludePrefix := GetIncludePrefix(Line);
      if IncludePrefix <> '' then
      begin
        FileSpec := Trim(Copy(Line, Length(IncludePrefix) + 1, 9999));
        CommentPos := Pos('//', FileSpec);
        if CommentPos > 1 then
          FileSpec := Copy(FileSpec, 1, CommentPos - 1);
        if FileSpec = '' then
          Continue;

        FileSpec := GetQuotedToken(FileSpec);
        if RightStr(FileSpec, 1) = '}' then
          FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 1));
        if RightStr(FileSpec, 2) = '*)' then
          FileSpec := Trim(Copy(FileSpec, 1, Length(FileSpec) - 2));

        if FileSpec <> '' then
          IncludeLines.Add(FileSpec);
      end;
    end;
  end;

  procedure GetCppPragmaBackupLines(FileLines: TStrings; IncludeLines: TStrings);
  var
    i, j : Integer;
    Line: string;
    ParsedLine: string;
    PragmaBackup: string;
    FileSpec: string;
    SlashCommentActive: Boolean;
    StarCommentActive: Boolean;
    QuoteActive: Boolean;
    DoubleQuoteActive: Boolean;
  begin
    Assert(Assigned(FileLines));
    Assert(Assigned(IncludeLines));

    StarCommentActive := False;

    IncludeLines.Clear;
    for i := 0 to FileLines.Count - 1 do
    begin
      SlashCommentActive := False;
      QuoteActive := False;
      DoubleQuoteActive := False;

      Line := Trim(FileLines[i]);
      ParsedLine := '';

      j := 1;

      while j <= Length(Line) do
      begin
        if QuoteActive then
        begin
          ParsedLine := ParsedLine + Line[j];
          if (Line[j] = #39) and ((j = 1) or (Line[j - 1] <> '\')) then
            QuoteActive := False;
        end
        else if DoubleQuoteActive then
        begin
          ParsedLine := ParsedLine + Line[j];
          if (Line[j] = '"') and ((j = 1) or (Line[j - 1] <> '\')) then
            DoubleQuoteActive := False;
        end
        else if StarCommentActive then
        begin
          if (Line[j] = '/') and (j > 1) and (Line[j - 1] = '*') then
            StarCommentActive := False;
        end
        else if not SlashCommentActive then
        begin
          case Line[j] of
            #39:
              begin
                QuoteActive := True;
                ParsedLine := ParsedLine + Line[j];
              end;
            '"':
              begin
                DoubleQuoteActive := True;
                ParsedLine := ParsedLine + Line[j];
              end;
            '/':
              if j <> Length(Line) then
              begin
                case Line[j + 1] of
                  '*':
                    begin
                      StarCommentActive := True;
                      Inc(j);
                    end;
                  '/': SlashCommentActive := True;
                else
                  ParsedLine := ParsedLine + Line[j];
                end
              end
              else
                ParsedLine := ParsedLine + Line[j];
          else
            ParsedLine := ParsedLine + Line[j];
          end;
        end;
        Inc(j);
      end;

      if (ParsedLine <> '') and (ParsedLine[1] = '#') then
      begin
        // This will break filenames with more than one consecutive space
        PragmaBackup := CompressWhiteSpace(Copy(ParsedLine, 2, MaxInt));
        if StrBeginsWith('pragma backup ', PragmaBackup) then
        begin
          FileSpec := Trim(Copy(ParsedLine, Pos('backup', ParsedLine) + 6, MaxInt));
          if FileSpec = '' then
            Continue;

          FileSpec := GetQuotedToken(FileSpec);
          if FileSpec <> '' then
            IncludeLines.Add(FileSpec);
        end;
      end;
    end;
  end;

  procedure TextFileToStrings(const FileName: string; Strings: TStringList);
  var
    AddString: string;
  begin
    Assert(Assigned(Strings));
    Strings.Clear;
    try
      // Don't try to open files with wildcard specifications
      if not FileNameHasWildcards(FileName) then
        if FileExists(FileName) then
          Strings.LoadFromFile(FileName);
    except
      // This might be a lie since we might have found it, but since we can not
      // 'Open' it and 'Read' it it is better to let the user know in now via a message
      // than find out later when the zip component crashes or reports an error.
      AddString := FileName + ItemSeparatorChar + FileName;
      EnsureStringInList(NotFoundFileList, AddString);
    end;
  end;

  procedure AddFilesForInclude(const Line: string; const FileName: string);
  var
    StoreFileName: string;
    BaseFileName: string;
    CompareExt: string;
  begin
    StoreFileName := Trim(Line);
    if StoreFileName = '' then
      Exit;
    CompareExt := AnsiLowerCase(StoreFileName);
    if (CompareExt = '*.dfm') or (CompareExt = '*.xfm') or (CompareExt = '*.nfm') then
      Exit; // Included elsewhere
    if SameText(StoreFileName, '*.res') then
      StoreFileName := ChangeFileExt(FileName, '.res');
    if SameText(StoreFileName, '*.dcr') then
      StoreFileName := ChangeFileExt(FileName, '.dcr');
    BaseFileName := StoreFileName;

    StoreFileName := ExpandFileName(StoreFileName);
    if FileNameHasWildcards(StoreFileName) then
      AddFileToList(StoreFileName, FoundFileList)
    else
    begin
      if FileExists(StoreFileName) then
      begin
        AddFileToList(StoreFileName, FoundFileList);
        if IsDprOrPas(StoreFileName) or IsCpp(StoreFileName) then
          ScanForIncludesAndAdd(StoreFileName, FoundFileList, NotFoundFileList);
      end
      else
      begin
        StoreFileName := BaseFileName + ItemSeparatorChar + FileName;
        AddFileToList(StoreFileName, NotFoundFileList);
      end;
    end;
  end;

var
  CurrentLine: string;
  OriginalPath: string;
  CurrentLineNum: Integer;
  FileLines: TStringList;
  IncludeLines: TStringList;
begin
  CurrentLine := '';
  FileLines := nil;
  IncludeLines := nil;
  OriginalPath := GetCurrentDir;
  try
    // Changing directories allows ExpandFileName to work on relative paths
    if DirectoryExists(ExtractFilePath(FileName)) then
      SafeChangeDirectory(ExtractFilePath(FileName));
    FileLines := TStringList.Create;
    IncludeLines := TStringList.Create;
    TextFileToStrings(FileName, FileLines);
    if IsDprOrPas(FileName) then
      GetPasIncludeLines(FileLines, IncludeLines)
    else if IsCpp(FileName) then
      GetCppPragmaBackupLines(FileLines, IncludeLines);

    for CurrentLineNum := 0 to IncludeLines.Count - 1 do
      AddFilesForInclude(IncludeLines[CurrentLineNum], FileName);
  finally
    FreeAndNil(FileLines);
    FreeAndNil(IncludeLines);
    SafeChangeDirectory(OriginalPath);
  end;
end;

{ TfmBackup }

function TfmBackup.ListFiles(const FileName, UnitName, FormName: string): Boolean;
var
  TempFileName: string;
begin
  Result := True;
  //{$IFOPT D+}SendDebug('Enumerating File: ' + FileName + ' Form: ' + FormName + ' Unit: ' + UnitName);{$ENDIF}

  // Do not backup DCP/BPI/DLL files
  TempFileName := ExtractUpperFileExt(FileName);
  if (TempFileName = '.DCP') or (TempFileName = '.BPI') or (TempFileName = '.DLL') then
    Exit;

  AddBackupFile(FileName);

  // Include matching header/bpr/bpk files for CPP files
  if (TempFileName = '.CPP') then
  begin
    AddBackupFile(ChangeFileExt(FileName, '.h')); // Do not localize.
    AddBackupFile(ChangeFileExt(FileName, '.bpr')); // Do not localize.
    AddBackupFile(ChangeFileExt(FileName, '.bpk')); // Do not localize.
  end;

  // Include both the DFM and XFM if they exist, just to be safe
  AddBackupFile(ChangeFileExt(FileName, '.dfm')); // Do not localize.
  AddBackupFile(ChangeFileExt(FileName, '.xfm')); // Do not localize.
  AddBackupFile(ChangeFileExt(FileName, '.nfm')); // Do not localize.
  AddBackupFile(ChangeFileExt(FileName, '.todo'));

  if FBackupExpert.DoBackupIncludedFiles and (IsDprOrPas(FileName) or IsCpp(FileName)) then
  begin
    //{$IFOPT D+}SendDebug('Scanning: ' + FileName);{$ENDIF}
    ScanForIncludesAndAdd(FileName, lbFiles.Items, FFilesFoundNowhere);
  end;

  Result := not FDoAbortCollectingFiles;
end;

constructor TfmBackup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaultFont(Self);

  FLibraryPath := TStringList.Create;
  FLibraryPath.Duplicates := dupIgnore;
  FFilesFoundNowhere := TStringList.Create;

  FZipEncrypted := False;
  FHaveCollectedFiles := False;
  LoadSettings;
end;

destructor TfmBackup.Destroy;
begin
  FreeAndNil(FFilesFoundNowhere);
  FreeAndNil(FLibraryPath);

  inherited Destroy;
end;

procedure TfmBackup.LocateFileOnPathAndAdd(FilesNotFound: TStrings);

  procedure SplitUpEntry(const Entry: string; var IncludedFile, RefererFile: string);
  var
    SeparatorPos: Integer;
  begin
    SeparatorPos := Pos(ItemSeparatorChar, Entry);
    if SeparatorPos > 0 then
    begin
      IncludedFile := Entry;
      Delete(IncludedFile, SeparatorPos, Length(Entry));
      RefererFile := Copy(Entry, SeparatorPos + 1, MaxInt);
    end
    else
    begin
      IncludedFile := Entry;
      RefererFile := '';
    end;
  end;

var
  i, j: Integer;
  IncludedFile: string;
  RefererFile: string;
  FileLocation: string;
begin
  // FilesNotFound is a list of files that have not been found, combined
  // with the filename which contained the reference to that file
  // Both parts are separated by the ItemSeparatorChar character ('|')
  // This routine scans the library path for the presence of files;
  // if a file is found, it is removed from the list of files not found
  // and adds it to the list(box) of files to be backed up.
  // Finally the list of *really* not found files is beautified.

{$IFOPT D+}
  if FilesNotFound.Count > 0 then
    SendDebugError('+++ Files not found:');
  for i := 0 to FilesNotFound.Count - 1 do
    SendDebugError(FilesNotFound[i]);
{$ENDIF D+}

  // Scan each directory on the library path whether
  // it contains any of the missing files.
  // Scan one directory completely before progressing
  // to the next to give the operating system's file cache
  // an easier job of reading the directory structure;
  // this should perform better than iterating over all files
  // and for each file trying to find the containing directory.
  for i := 0 to FLibraryPath.Count - 1 do
  begin
    j := FilesNotFound.Count - 1;
    while j >= 0 do
    begin
      SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
      FileLocation := FLibraryPath[i] + IncludedFile;
      if FileExists(FileLocation) then
      begin
        FilesNotFound.Delete(j);
        if lbFiles.Items.IndexOf(FileLocation) < 0 then
          AddBackupFile(FileLocation);
      end;
      Dec(j);
    end;
  end;

  // Finally post-process the list of *really* not found files
  // and give the list a pretty format now.
  for j := 0 to FilesNotFound.Count - 1 do
  begin
    SplitUpEntry(FilesNotFound[j], IncludedFile, RefererFile);
    FilesNotFound[j] := Format('%s (%s)', [IncludedFile, RefererFile]);
  end;
end;

procedure TfmBackup.DoCollectFiles;
var
  ProjectGroupFileName: string;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  if FCurrentBackupScope = bsActiveProject then
    DoCollectFilesFromSingleProject(GxOtaGetCurrentProject)
  else
  begin
    // First find the currently active project group.
    IProjectGroup := GxOtaGetProjectGroup;
    if IProjectGroup = nil then
      Exit;
    ProjectGroupFileName := GxOtaGetProjectGroupFileName;

    AddBackupFile(ProjectGroupFileName);

    for i := 0 to IProjectGroup.ProjectCount - 1 do
      DoCollectFilesFromSingleProject(IProjectGroup.Projects[i]);
  end;
end;

procedure TfmBackup.DoCollectFilesFromSingleProject(IProject: IOTAProject);
var
  i: Integer;
  IModuleInfo: IOTAModuleInfo;
  IEditor: IOTAEditor;
  FileName: string;
begin
  if IProject = nil then
    Exit;
  Assert(Assigned(FProgressForm));
  GxOtaGetEffectiveLibraryPath(FLibraryPath, IProject);

  // Gather project files
  for i := 0 to IProject.GetModuleFileCount - 1 do
  begin
    IEditor := IProject.GetModuleFileEditor(i);
    Assert(IEditor <> nil);

    FileName := IEditor.FileName;
    if FileName <> '' then
      ListFiles(IEditor.FileName, '', '');
  end;
  // Delphi 8 project files have ModuleFileCount=0
  ListFiles(IProject.FileName, '', '');

  for i := 0 to IProject.GetModuleCount - 1 do
  begin
    IModuleInfo := IProject.GetModule(i);
    Assert(IModuleInfo <> nil);

    FileName := IModuleInfo.FileName;
    if FileName <> '' then
      ListFiles(IModuleInfo.FileName, '', IModuleInfo.FormName);
  end;
end;

procedure TfmBackup.CollectFilesForBackup;
resourcestring
  SCollectingBackupFiles = 'Collecting Files...';
  SFilesNotFound = 'The following included files could not be found for backup:' + sLineBreak +
    sLineBreak +
    '%s';
begin
  lbFiles.Clear;
  FFilesFoundNowhere.Clear;

  Assert(FProgressForm = nil);
  FProgressForm := TfmProgress.Create(nil);
  try
    FProgressForm.Caption := SCollectingBackupFiles;
    Self.Enabled := False;
    FProgressForm.Show;
    FProgressForm.Progress.Max := 40;

    FDoAbortCollectingFiles := False;
    Screen.Cursor := crHourglass;
    lbFiles.Items.BeginUpdate;
    try
      DoCollectFiles;
    finally
      lbFiles.Sorted := True;
      lbFiles.Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;

    if FBackupExpert.FBackupInc then
    begin
      // The "FFilesFoundNowhere" list now contains everything that could not be
      // found scanning the source text. Process now the path in order to
      // possibly find items there.
      // "LocateFileOnPathAndAdd" will remove the files that it finds;
      // after returning, the list will contain those files that *really*
      // could not be found, not even on the library path.
      if FBackupExpert.FollowLibraryPath then
        LocateFileOnPathAndAdd(FFilesFoundNowhere);

      FFilesFoundNowhere.Text := StringReplace(FFilesFoundNowhere.Text,
        '|', ' from ', [rfReplaceAll]);
      FFilesFoundNowhere.Sort;
      Self.Enabled := True;
      FreeAndNil(FProgressForm);

      if FFilesFoundNowhere.Count > 0 then
        MessageDlg(Format(SFilesNotFound, [FFilesFoundNowhere.Text]), mtWarning, [mbOK], 0);
    end;
  finally
    Self.Enabled := True;
    FreeAndNil(FProgressForm);
  end;
  AfterFileListChange;
end;

procedure TfmBackup.PerformBackup(const Path, FileName: string);
var
  Cursor: IInterface;
  DestFile: string;
begin
  Assert(FProgressForm = nil);
  FZipComponent := nil;
  FProgressForm := TfmProgress.Create(nil);
  try
    DestFile := Path + FileName;
    FZipComponent := TGXZipper.Create(DestFile, fmCreate or fmShareDenyWrite);
    Cursor := TempHourGlassCursor;
    FProgressForm.Progress.Position := 0;
    Self.Enabled := False;
    FProgressForm.Show;
    Application.ProcessMessages;

    FProgressForm.Progress.Max := 100;
    FZipComponent.IncludePath := FBackupExpert.DoIncludeDirInfoInZip;
    if FZipEncrypted then
      FZipComponent.Password := FZipPassword;
    FZipComponent.OnProcessItemFailure := FileFailure;
    FZipComponent.OnArchiveProgress := AbbreviaProgress;
    FZipComponent.AddFiles(lbFiles.Items);
    FZipComponent.Save;
  finally
    Self.Enabled := True;
    FreeAndNil(FProgressForm);
    FreeAndNil(FZipComponent);
  end;
end;

procedure TfmBackup.btnBackupClick(Sender: TObject);
resourcestring
  SFileExists = 'File %s already exists, do you want to overwrite this file?';
  SDirectoryDoesNotExist = 'Directory %s does not exist, do you want to create this directory?';
const
  ZipExtension = '.zip'; // Do not localize.
var
  CurrentZipFileName: string;
  ZipFilePath: string;
  i: Integer;
  CurrentIdeFolder: string;
begin
  if FBackupExpert.BackupType = btFile then
  begin

    CurrentIdeFolder := GetCurrentDir;
    try
      if not GetOpenSaveDialogExecute(dlgSave) then
        Exit;
    finally
      SetCurrentDir(CurrentIdeFolder);
    end;

    CurrentZipFileName := dlgSave.FileName;

    if ExtractFileExt(CurrentZipFileName) = '' then
      CurrentZipFileName := CurrentZipFileName + ZipExtension;
    if FileExists(CurrentZipFileName) then
    begin
      if MessageDlg(Format(SFileExists, [CurrentZipFileName]), mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then
      begin
        Exit;
      end;
      DeleteFile(CurrentZipFileName);
    end;
  end
  else
  begin
    CurrentZipFileName := ReplaceStrings(FBackupExpert.BackupDir, True);
    ZipFilePath := ExtractFilePath(CurrentZipFileName);
    CurrentZipFileName := ExtractFileName(CurrentZipFileName);
    if not DirectoryExists(ZipFilePath) then
    begin
      if MessageDlg(Format(SDirectoryDoesNotExist, [ZipFilePath]), mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then
      begin
        Exit;
      end
      else
        ForceDirectories(ZipFilePath);
    end;

    if ExtractUpperFileExt(CurrentZipFileName) = '.ZIP' then
      CurrentZipFileName := ChangeFileExt(CurrentZipFileName, '');

    if FileExists(ZipFilePath + CurrentZipFileName + ZipExtension) then
    begin
      i := 1;
      while i < 999 do
      begin
        if not FileExists(ZipFilePath + CurrentZipFileName + IntToStr(i) + ZipExtension) then
        begin
          CurrentZipFileName := CurrentZipFileName + IntToStr(i) + ZipExtension;
          Break;
        end;
        Inc(i);
      end;
    end
    else
      CurrentZipFileName := CurrentZipFileName + ZipExtension;
  end;

  PerformBackup(ZipFilePath, CurrentZipFileName);

  SaveSettings;
  ModalResult := mrOk;
end;

procedure TfmBackup.btnAddClick(Sender: TObject);
var
  i: Integer;
  CurrentIdeFolder: string;
begin
  dlgOpen.Options := dlgOpen.Options + [ofAllowMultiSelect, ofFileMustExist] - [ofNoValidate];

  CurrentIdeFolder := GetCurrentDir;
  try
    if GetOpenSaveDialogExecute(dlgOpen) then
      for i := 0 to dlgOpen.Files.Count - 1 do
        AddBackupFile(dlgOpen.Files[i]);
  finally
    SetCurrentDir(CurrentIdeFolder);
    AfterFileListChange;
  end;
end;

procedure TfmBackup.btnRemoveClick(Sender: TObject);
var
  i: Integer;
  OldIndex: Integer;
begin
  i := 0;
  OldIndex := lbFiles.ItemIndex;
  while i <= lbFiles.Items.Count - 1 do
  begin
    if lbFiles.Selected[i] then
      lbFiles.Items.Delete(i)
    else
      Inc(i);
  end;
  OldIndex := Min(OldIndex, lbFiles.Items.Count - 1);
  if OldIndex > -1 then
  begin
    lbFiles.ItemIndex := OldIndex;
    lbFiles.Selected[OldIndex] := True;
  end;
  AfterFileListChange;
end;

procedure TfmBackup.FormDestroy(Sender: TObject);
begin
  // Aborting isn't actually possible with our current design
  FDoAbortCollectingFiles := True;
end;

procedure TfmBackup.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 10);
end;

procedure TfmBackup.FormActivate(Sender: TObject);
begin
  if not FHaveCollectedFiles then
  begin
    FHaveCollectedFiles := True;
    CollectFilesForBackup;
  end;
end;

procedure TfmBackup.btnOptionsClick(Sender: TObject);
var
  Dlg: TfmBackupOptions;
  RefreshRequired: Boolean;
begin
  Dlg := TfmBackupOptions.Create(nil);
  try
    Dlg.cbPassword.Checked := FZipEncrypted;
    Dlg.edPassword.Text := FZipPassword;
    Dlg.cbSearchLibraryPath.Checked := FBackupExpert.FollowLibraryPath;
    Dlg.rgScope.ItemIndex := Ord(FCurrentBackupScope);
    pnlButtons.Enabled := False;
    pnlFiles.Enabled := False;
    if Dlg.ShowModal = mrOk then
    begin
      FZipPassword := Dlg.edPassword.Text;
      FZipEncrypted := Dlg.cbPassword.Checked;
      RefreshRequired := not (FCurrentBackupScope = TBackupScope(Dlg.rgScope.ItemIndex));
      FCurrentBackupScope := TBackupScope(Dlg.rgScope.ItemIndex);
      RefreshRequired := RefreshRequired or (not (FBackupExpert.FFollowLibraryPath = Dlg.cbSearchLibraryPath.Checked));
      FBackupExpert.FFollowLibraryPath := Dlg.cbSearchLibraryPath.Checked;
      if RefreshRequired then
        CollectFilesForBackup;
    end;
  finally
    pnlButtons.Enabled := True;
    pnlFiles.Enabled := True;
    FreeAndNil(Dlg);
  end;
end;

procedure TfmBackup.lbFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (lbFiles.ItemIndex > -1) then
  begin
    btnRemove.Click;
    Key := 0;
  end;
end;

// Check for duplicates before adding files to the the backup list
// Prevents adding files twice when units are in two projects of a group
procedure TfmBackup.AddBackupFile(const FileName: string);
begin
  IncrementProgress;
  if lbFiles.Items.IndexOf(FileName) = -1 then // Should be case sensitive in Kylix
    if FileExists(FileName) then
      lbFiles.Items.Add(FileName);
end;

procedure TfmBackup.AfterFileListChange;
resourcestring
  SBackupFormCaptionUnknown  = 'Backup Project (%d+ Files)';
  SBackupFormCaptionPlural   = 'Backup Project (%d Files)';
  SBackupFormCaptionSingular = 'Backup Project (1 File)';
var
  NonWildcardFileCount: Integer;
  i: Integer;
begin
  ListboxHorizontalScrollbar(lbFiles);

  NonWildcardFileCount := 0;
  for i := 0 to lbFiles.Items.Count - 1 do
  begin
    if not FileNameHasWildcards(lbFiles.Items[i]) then
      Inc(NonWildcardFileCount);
  end;
  if NonWildcardFileCount < lbFiles.Items.Count then
    Caption := Format(SBackupFormCaptionUnknown, [NonWildcardFileCount])
  else if NonWildcardFileCount = 1 then
    Caption := SBackupFormCaptionSingular
  else
    Caption := Format(SBackupFormCaptionPlural, [NonWildcardFileCount]);
  btnBackup.Enabled := (lbFiles.Items.Count > 0);
end;

procedure TfmBackup.SaveSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteInteger(ConfigurationKey, 'Left', Left);
    Settings.WriteInteger(ConfigurationKey, 'Top', Top);
    Settings.WriteInteger(ConfigurationKey, 'Width', Width);
    Settings.WriteInteger(ConfigurationKey, 'Height', Height);
    Settings.WriteString(ConfigurationKey, 'LastZipDir', ExtractFilePath(dlgSave.FileName));
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmBackup.LoadSettings;
var
  Settings: TGExpertsSettings;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Left := Settings.ReadInteger(ConfigurationKey, 'Left', Left);
    Top := Settings.ReadInteger(ConfigurationKey, 'Top', Top);
    Width := Settings.ReadInteger(ConfigurationKey, 'Width', Width);
    Height := Settings.ReadInteger(ConfigurationKey, 'Height', Height);
    dlgSave.InitialDir := Settings.ReadString(ConfigurationKey, 'LastZipDir', '');
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmBackup.IncrementProgress;
begin
  if not Assigned(FProgressForm) then
    Exit;
  with FProgressForm.Progress do
    if Position = Max then
      Position := 0
    else
      Position := Position + 1;
end;

function TfmBackup.ConfigurationKey: string;
begin
  Result := TBackupExpert.ConfigurationKey;
end;

procedure TfmBackup.FileFailure(Sender: TObject; Item: TAbArchiveItem;
  ProcessType: TAbProcessType; ErrorClass: TAbErrorClass; ErrorCode: Integer);
begin
  if ErrorClass = ecAbbrevia then begin
    if ErrorCode <> AbDuplicateName then
      MessageDlg(AbStrRes(ErrorCode), mtError, [mbOK], 0)
  end
  else
    MessageDlg('Error processing file: ' + Item.FileName, mtError, [mbOK], 0);
end;

procedure TfmBackup.AbbreviaProgress(Sender: TObject; Progress: Byte; var Abort: Boolean);
begin
  FProgressForm.Progress.Position := Progress;
end;

{ TBackupExpert }

constructor TBackupExpert.Create;
begin
  inherited Create;
  FBackupType := btFile;
  FBackupDir := AddSlash('%PROJECTDIR%') + '%PROJECTNAME%'; // Do not localize.
  FIncludeDir := True;
  FBackupScope := bsActiveProject;
end;

function TBackupExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Backup Project...';
begin
  Result := SMenuCaption;
end;

class function TBackupExpert.GetName: string;
begin
  Result := 'BackupProject'; // Do not localize.
end;

procedure TBackupExpert.Click(Sender: TObject);
var
  Dlg: TfmBackup;
begin
  Dlg := TfmBackup.Create(nil);
  try
    SetFormIcon(Dlg);
    Dlg.FBackupExpert := Self;
    Dlg.FCurrentBackupScope := BackupScope;
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TBackupExpert.Configure;
var
  Dlg: TfmBackupConfig;
begin
  Dlg := TfmBackupConfig.Create(nil);
  try
    Dlg.cbBackupInc.Checked := FBackupInc;
    Dlg.cbIncludeDir.Checked := FIncludeDir;
    Dlg.rbBackupAskForFile.Checked := (FBackupType = btFile);
    Dlg.rbBackupToDirectory.Checked := (FBackupType = btDir);
    Dlg.edBackupDir.Text := FBackupDir;
    Dlg.rgDefaultScope.ItemIndex := Ord(FBackupScope);
    Dlg.cbSearchOnLibraryPath.Checked := FFollowLibraryPath;
    if Dlg.ShowModal = mrOk then
    begin
      if Dlg.rbBackupAskForFile.Checked then
        FBackupType := btFile
      else
        FBackupType := btDir;
      FBackupDir := Dlg.edBackupDir.Text;
      FBackupInc := Dlg.cbBackupInc.Checked;
      FIncludeDir := Dlg.cbIncludeDir.Checked;
      FBackupScope := TBackupScope(Dlg.rgDefaultScope.ItemIndex);
      FFollowLibraryPath := Dlg.cbSearchOnLibraryPath.Checked;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TBackupExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the following lines.
  Settings.WriteBool(ConfigurationKey, 'Include', FBackupInc);
  Settings.WriteEnumerated(ConfigurationKey, 'Type', TypeInfo(TBackupType), Ord(FBackupType));
  Settings.WriteString(ConfigurationKey, 'Directory', FBackupDir);
  Settings.WriteBool(ConfigurationKey, 'IncludeDir', FIncludeDir);
  Settings.WriteEnumerated(ConfigurationKey, 'BackupScope', TypeInfo(TBackupScope), Ord(FBackupScope));
  Settings.WriteBool(ConfigurationKey, 'FollowLibraryPath', FFollowLibraryPath);
end;

procedure TBackupExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the following lines.
  FBackupInc := Settings.ReadBool(ConfigurationKey, 'Include', FBackupInc);
  FBackupType := TBackupType(Settings.ReadEnumerated(ConfigurationKey, 'Type', TypeInfo(TBackupType), Ord(FBackupType)));
  FBackupDir := Settings.ReadString(ConfigurationKey, 'Directory', FBackupDir);
  FIncludeDir := Settings.ReadBool(ConfigurationKey, 'IncludeDir', FIncludeDir);
  FBackupScope := TBackupScope(Settings.ReadEnumerated(ConfigurationKey, 'BackupScope', TypeInfo(TBackupScope), Ord(FBackupScope)));
  FFollowLibraryPath := Settings.ReadBool(ConfigurationKey, 'FollowLibraryPath', FFollowLibraryPath);
end;

initialization
  RegisterGX_Expert(TBackupExpert);

end.

