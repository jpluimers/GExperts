{***************************************************************
 * Unit Name: GX_InsertAutoTodoExpert
 * Purpose  : Inserts TODOs for empty code blocks
 * Authors  : Peter Laman, Thomas Mueller
 ****************************************************************}

unit GX_InsertAutoTodo;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Menus,
  GX_BaseForm, GX_Experts;

type
  tfmInsertAutoTodoForm = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    l_Username: TLabel;
    ed_Username: TEdit;
    l_TextToInsert: TLabel;
    b_ResetTextToInsert: TButton;
    b_Placeholder: TButton;
    m_TextToInsert: TMemo;
    pm_Placeholders: TPopupMenu;
    procedure b_ResetTextToInsertClick(Sender: TObject);
  private
    procedure mi_PlaceholderClick(Sender: TObject);
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Registry, ToolsAPI,
  GX_GExperts, GX_ConfigurationInfo, GX_uAutoTodoHandler, GX_dzVclUtils,
  GX_OtaUtils, GX_GenericUtils;

type
  EAutoTodo = class(Exception);

type
  TGxInsertAutoTodoExpert = class(TGX_Expert)
  private
    FUsername: string;
    FTextToInsert: string;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
    procedure Configure; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure Execute(Sender: TObject); override;
  end;

{ TGxInsertAutoTodoExpert }

procedure TGxInsertAutoTodoExpert.Execute(Sender: TObject);
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
  s: WideString;
  i: Integer;
  CurPos: Integer;
  PatchPos: Integer;
  TextLength: Integer;
begin
    SourceEditor := GxOtaGetCurrentSourceEditor;
    if not Assigned(SourceEditor) then
      raise EAutoTodo.Create(str_NoEditor);
    FileName := SourceEditor.FileName;
    if not (IsPascalSourceFile(FileName) or IsDelphiPackage(FileName) or FileMatchesExtension(FileName, '.tpl')) then
      raise EAutoTodo.CreateFmt(str_UnsupportedFileTypeS, [ExtractFileName(FileName)]);

    if not GxOtaGetActiveEditorTextAsUnicodeString(s, False) then
      raise EAutoTodo.CreateFmt(str_UnableToGetContentsS, [FileName]);
    if s = '' then
      exit;

    TextLength := Length(s);

    Handler := nil;
    Patches := TStringList.Create;
    try
      Handler := TAutoTodoHandler.Create;

      if FUsername = '*' then
        Handler.TodoUser := Handler.GetWindowsUser
      else
        Handler.TodoUser := FUsername;
      Handler.TextToInsert := FTextToInsert;

      Handler.Execute(s, Patches);

      Writer := SourceEditor.CreateUndoableWriter;

      CurPos := 0;
      for i := 0 to Patches.Count - 1 do begin
        PatchPos := Integer(Patches.Objects[i]);
        if PatchPos > CurPos then begin
          Writer.CopyTo(PatchPos);
          CurPos := PatchPos;
        end;
        Writer.Insert(PChar(Patches[i]));
      end;
      if CurPos < TextLength then
        Writer.CopyTo(TextLength);
    finally
      FreeAndNil(Handler);
      FreeAndNil(Patches);
    end;
end;

procedure TGxInsertAutoTodoExpert.Configure;
var
  frm: tfmInsertAutoTodoForm;
begin
  frm := tfmInsertAutoTodoForm.Create(nil);
  try
    frm.ed_Username.Text := FUsername;
    frm.m_TextToInsert.Lines.Text := FTextToInsert;
    if frm.ShowModal = mrOk then
    begin
      FUsername := frm.ed_Username.Text;
      FTextToInsert := frm.m_TextToInsert.Lines.Text;
      SaveSettings;
    end;
  finally
    frm.Free;
  end;
end;

constructor TGxInsertAutoTodoExpert.Create;
begin
  inherited Create;

  FUsername := '*'; // '*' means 'use Windows username'
  FTextToInsert := TAutoTodoHandler.GetDefaultTextToInsert;

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
  SMenuCaption = 'Insert Auto TODOs...';
begin
  Result := SMenuCaption;
end;

class function TGxInsertAutoTodoExpert.GetName: string;
begin
  Result := 'InsertAutoTodo';
end;

function TGxInsertAutoTodoExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGxInsertAutoTodoExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxInsertAutoTodoExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FUsername := Settings.ReadString(ConfigurationKey, 'Username', FUsername);
  FTextToInsert := Settings.ReadString(ConfigurationKey, 'TextToInsert', FTextToInsert);
end;

procedure TGxInsertAutoTodoExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  Settings.WriteString(ConfigurationKey, 'Username', FUsername);
  Settings.WriteString(ConfigurationKey, 'TextToInsert', FTextToInsert);
end;

procedure TGxInsertAutoTodoExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);
  // nothing else to do
end;

{ tfmInsertAutoTodoForm }

constructor tfmInsertAutoTodoForm.Create(Owner: TComponent);
var
  sl: TStringList;
  i: Integer;
begin
  inherited;
  sl := TStringList.Create;
  try
    TAutoTodoHandler.GetPlaceholders(sl);
    for i := 0 to sl.Count - 1 do
      TPopupMenu_AppendMenuItem(pm_Placeholders, sl[i], mi_PlaceholderClick);
  finally
    FreeAndNil(sl);
  end;
  TButton_AddDropdownMenu(b_Placeholder, pm_Placeholders);
  m_TextToInsert.Lines.Text := TAutoTodoHandler.GetDefaultTextToInsert;

  TControl_SetMinConstraints(Self);
  Constraints.MaxHeight := Height;
end;

procedure tfmInsertAutoTodoForm.b_ResetTextToInsertClick(Sender: TObject);
begin
  m_TextToInsert.Lines.Text := TAutoTodoHandler.GetDefaultTextToInsert;
end;

procedure tfmInsertAutoTodoForm.mi_PlaceholderClick(Sender: TObject);
var
  mi: TMenuItem;
  s: string;
begin
  mi := Sender as TMenuItem;
  s := StripHotKey(mi.Caption);
  m_TextToInsert.SelText := '{' + s + '}';
end;

initialization
  RegisterGX_Expert(TGxInsertAutoTodoExpert);
end.
