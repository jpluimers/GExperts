{***************************************************************
 * Unit Name: GX_InsertAutoTodoExpert
 * Purpose  : Inserts to do comments for empty code blocks
 * Authors  : Peter Laman, Thomas Mueller
 ****************************************************************}

unit GX_InsertAutoTodo;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Menus,
  GX_BaseForm, GX_Experts, ExtCtrls;

type
  TfmInsertAutoTodoForm = class(TfmBaseForm)
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblTextToInsert: TLabel;
    btnLoadDetault: TButton;
    btnInsertPlaceholder: TButton;
    mmoTextToInsert: TMemo;
    pmuPlaceholders: TPopupMenu;
    lblStarWindows: TLabel;
    pnlFooter: TPanel;
    chkShowDoneDialog: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnLoadDetaultClick(Sender: TObject);
  private
    procedure InsertPlaceholderClick(Sender: TObject);
    procedure SetData(const AUsername, ATextToInsert: string; ADoneDialogEnabled: Boolean);
    procedure GetData(var AUsername, ATextToInsert: string; var ADoneDialogEnabled: Boolean);
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, Actions, ActnList, ToolsAPI, Types,
  GX_GExperts, GX_ConfigurationInfo, GX_uAutoTodoHandler, GX_dzVclUtils,
  GX_OtaUtils, GX_GenericUtils, GX_AutoTodoDone;

type
  EAutoTodo = class(Exception);

type
  TGxInsertAutoTodoExpert = class(TGX_Expert)
  private
    FUsername: string;
    FTextToInsert: string;
    FDoneDialogEnabled: Boolean;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
  end;

type
  TOffsetToCursorPos = class
  private
    FOffsets: array of integer;
  public
    constructor Create(_sl: TGXUnicodeStringList);
    function CalcCursorPos(_Offset: integer): TPoint;
  end;

{ TOffsetToCursorPos }

constructor TOffsetToCursorPos.Create(_sl: TGXUnicodeStringList);
var
  cnt: integer;
  i: Integer;
  CrLfLen: integer;
  Ofs: Integer;
begin
  inherited Create;
{$IFDEF GX_VER190_up}
  CrLfLen := Length(_sl.LineBreak);
{$ELSE}
  // Delphi < 2007 does not have the LineBreak property
  CrLfLen := 2;
{$ENDIF}
  cnt := _sl.Count;
  SetLength(FOffsets, cnt);
  Ofs := 1;
  for i := 0 to _sl.Count - 1 do begin
    FOffsets[i] := Ofs;
    Inc(Ofs, Length(_sl[i]) + CrLfLen);
  end;
end;

function TOffsetToCursorPos.CalcCursorPos(_Offset: integer): TPoint;
var
  i: integer;
begin
  i := 0;
  while (i < Length(FOffsets)) and (_Offset >= FOffsets[i]) do begin
    Inc(i);
  end;
  Result.Y := i - 1;
  Result.X := _Offset - FOffsets[Result.Y] + 1;
end;

{ TGxInsertAutoTodoExpert }

procedure TGxInsertAutoTodoExpert.Execute(Sender: TObject);

  function PointToCharPos(_Pnt: TPoint): TOTACharPos;
  begin
    Result.Line :=_Pnt.Y + 1;
    Result.CharIndex := _Pnt.X;
  end;

resourcestring
  str_NoEditor = 'No source editor';
  str_UnsupportedFileTypeS = 'Unsupported file type: %s';
  str_UnableToGetContentsS = 'Unable to get contents of %s';
var
  SourceEditor: IOTASourceEditor;
  FileName: string;
  Handler: TAutoTodoHandler;
  Patches: TStringList;
  Writer: IOTAEditWriter;
  Lines: TGXUnicodeStringList;
  Source: TGXUnicodeString;
  i: Integer;
  CurPos: Integer;
  PatchPos: Integer;
  TextLength: Integer;
  OffsToCP: TOffsetToCursorPos;
  cp: TPoint;
  EditView: IOTAEditView;
  Offset: Integer;
begin
    if not GxOtaTryGetCurrentSourceEditor(SourceEditor) then
      raise EAutoTodo.Create(str_NoEditor);
    FileName := SourceEditor.FileName;
    if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
      raise EAutoTodo.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

    Lines := TGXUnicodeStringList.Create;
    try
      if not GxOtaGetActiveEditorText(Lines, false) then
        raise EAutoTodo.CreateFmt(str_UnableToGetContentsS, [FileName]);
      Source := Lines.Text;
      if Source = '' then
        exit;

      TextLength := Length(Source);

      Patches := TStringList.Create;
      try
        Handler := TAutoTodoHandler.Create;
        try
          if FUsername = '*' then
            Handler.TodoUser := GetCurrentUser
          else
            Handler.TodoUser := FUsername;
          Handler.TextToInsert := FTextToInsert;

          Handler.Execute(Source, Patches);
        finally
          FreeAndNil(Handler);
        end;

        if Patches.Count > 0 then begin
          EditView := GxOtaGetTopMostEditView(SourceEditor);
          OffsToCP := TOffsetToCursorPos.Create(Lines);
          try
//            for i := Patches.Count - 1 downto 0 do
//              Insert(Patches[i], Source, Integer(Patches.Objects[i]) + 1);

            // due to the IDE using UTF-8 we need to convert PatchPos to line and offset
            // and then convert line and offset to the Offset into the edit buffer
            for i := 0 to Patches.Count - 1 do begin
              PatchPos := NativeInt(Patches.Objects[i]);
              cp := OffsToCP.CalcCursorPos(PatchPos);
              Offset := EditView.CharPosToPos(PointToCharPos(cp));
              Patches.Objects[i] := Pointer(Offset);
            end;
          finally
            FreeAndNil(OffsToCP);
          end;
          EditView := nil;

          Writer := SourceEditor.CreateUndoableWriter;
          CurPos := 0;
          for i := 0 to Patches.Count - 1 do begin
            PatchPos := NativeInt(Patches.Objects[i]);
            if PatchPos > CurPos then begin
              Writer.CopyTo(PatchPos);
              CurPos := PatchPos;
            end;
            Writer.Insert(PAnsiChar(ConvertToIDEEditorString(Patches[i])));
          end;
          if CurPos < TextLength then
            Writer.CopyTo(TextLength);
        end;

        if FDoneDialogEnabled then begin
          if TfmAutoTodoDone.Execute(Patches.Count) then begin
            FDoneDialogEnabled := False;
            SaveSettings;
          end;
        end;
      finally
        FreeAndNil(Patches);
      end;
    finally
      FreeAndNil(Lines);
    end;
end;

procedure TGxInsertAutoTodoExpert.Configure;
var
  Dialog: tfmInsertAutoTodoForm;
begin
  Dialog := tfmInsertAutoTodoForm.Create(nil);
  try
    Dialog.SetData(FUsername, FTextToInsert, FDoneDialogEnabled);
    if Dialog.ShowModal = mrOk then
    begin
     Dialog.GetData(FUsername, FTextToInsert, FDoneDialogEnabled);
     SaveSettings;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

constructor TGxInsertAutoTodoExpert.Create;
begin
  inherited Create;

  FUsername := '*'; // '*' means 'use Windows username'
  FTextToInsert := TAutoTodoHandler.GetDefaultTextToInsert;
  FDoneDialogEnabled := True;

  // we do not want a shortcut
  // ShortCut := Menus.ShortCut(Word('Z'), [ssCtrl, ssShift, ssAlt]);
end;

destructor TGxInsertAutoTodoExpert.Destroy;
begin
  // Free any created objects here
  inherited Destroy;
end;

function TGxInsertAutoTodoExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Comment Empty Code Blocks...';
begin
  Result := SMenuCaption;
end;

class function TGxInsertAutoTodoExpert.GetName: string;
begin
  Result := 'InsertAutoToDo';
end;

function TGxInsertAutoTodoExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGxInsertAutoTodoExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxInsertAutoTodoExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FUsername := Settings.ReadString('Username', FUsername);
  FTextToInsert := Settings.ReadString('TextToInsert', FTextToInsert);
  FDoneDialogEnabled := Settings.ReadBool('DoneDialogEnabled', FDoneDialogEnabled);
end;

procedure TGxInsertAutoTodoExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;
  Settings.WriteString('Username', FUsername);
  Settings.WriteString('TextToInsert', FTextToInsert);
  Settings.WriteBool('DoneDialogEnabled', FDoneDialogEnabled);
end;

procedure TGxInsertAutoTodoExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);
  // nothing else to do
end;

procedure TGxInsertAutoTodoExpert.UpdateAction(Action: TCustomAction);
const
  SAllowableFileExtensions = '.pas;.dpr;.inc';
begin
  Action.Enabled := FileMatchesExtensions(GxOtaGetCurrentSourceFile, SAllowableFileExtensions);
end;

{ TfmInsertAutoTodoForm }

constructor TfmInsertAutoTodoForm.Create(Owner: TComponent);
var
  Placeholders: TStringList;
  i: Integer;
begin
  inherited;
  Placeholders := TStringList.Create;
  try
    TAutoTodoHandler.GetPlaceholders(Placeholders);
    for i := 0 to Placeholders.Count - 1 do
      TPopupMenu_AppendMenuItem(pmuPlaceholders, Placeholders[i], InsertPlaceholderClick);
  finally
    FreeAndNil(Placeholders);
  end;
  TButton_AddDropdownMenu(btnInsertPlaceholder, pmuPlaceholders);
  mmoTextToInsert.Lines.Text := TAutoTodoHandler.GetDefaultTextToInsert;

  TControl_SetMinConstraints(Self);
  Constraints.MaxHeight := Height;
end;

procedure TfmInsertAutoTodoForm.GetData(var AUsername, ATextToInsert: string;
  var ADoneDialogEnabled: Boolean);
begin
  AUsername := edtUsername.Text;
  ATextToInsert := mmoTextToInsert.Lines.Text;
  ADoneDialogEnabled := chkShowDoneDialog.Checked;
end;

procedure TfmInsertAutoTodoForm.SetData(const AUsername, ATextToInsert: string;
  ADoneDialogEnabled: Boolean);
begin
  edtUsername.Text := AUsername;
  mmoTextToInsert.Lines.Text := ATextToInsert;
  chkShowDoneDialog.Checked := ADoneDialogEnabled;
end;

procedure TfmInsertAutoTodoForm.btnLoadDetaultClick(Sender: TObject);
begin
  mmoTextToInsert.Lines.Text := TAutoTodoHandler.GetDefaultTextToInsert;
end;

procedure TfmInsertAutoTodoForm.InsertPlaceholderClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  Str: string;
begin
  MenuItem := Sender as TMenuItem;
  Str := StripHotKey(MenuItem.Caption);
  mmoTextToInsert.SelText := '{' + Str + '}';
end;

initialization
  RegisterGX_Expert(TGxInsertAutoTodoExpert);
end.
