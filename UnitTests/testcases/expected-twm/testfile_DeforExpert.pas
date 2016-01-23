unit u_DeforExpert;

{$I delphiversions.inc}

interface

uses
  Windows,
  Messages,
  Controls,
  SysUtils,
  Classes,
  ToolsApi;

procedure Register;

implementation

uses
  Menus,
  Dialogs,
  Forms,
  GX_CodeFormatterTypes,
  u_DelforTypesEx,
  u_DelforRegistry,
  GX_CodeFormatterDone,
  w_DelforAbout,
  GX_CodeFormatterConfig,
  GX_CodeFormatterBookmarks,
  GX_CodeFormatterBreakpoints,
  GX_CodeFormatterEngine,
  GX_CodeFormatterDefaultSettings,
  GX_CodeFormatterSettings;

type
  TDelforExpert = class;

  TKeyboardNotifier = class(TNotifierObject, IOTAKeyboardBinding)
  private
    fWizard: TDelforExpert;
    fBindingIndex: Integer;
    fHotkey: TShortcutDesc;

    // IOTAKeyboardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    // registered Keyboard handling proc
    procedure KeyProc(const _Context: IOTAKeyContext; _KeyCode: TShortCut;
      var _BindingResult: TKeyBindingResult);
    procedure Unregister(_Idx: Integer); overload;
  public
    constructor Create(_Wizard: TDelforExpert; const _Shortcut: TShortcutDesc);
    destructor Destroy; override;
    procedure SetShortcut(_ShortCut: TShortCutDesc);
    procedure Unregister; overload;
  end;

  TDelforExpert = class(TNotifierObject, IOTAWizard)
  private
    FFormatter: TCodeFormatterEngine;
    FWizardSettings: TWizardSettings;
//    fNotifierIndex: Integer;
    fKeyboardNotifier: TKeyboardNotifier;
    function GetToolsMenu: TMenuItem;
    procedure FocusTopEditView;

    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    // IOTANotifier implemented by TNotifierObject

    // IOTAIdeNotifier
//    procedure AfterCompile(Succeeded: Boolean);
//    procedure BeforeCompile(const Project: IOTAProject;
//      var Cancel: Boolean);
//    procedure FileNotification(NotifyCode: TOTAFileNotification;
//      const FileName: string; var Cancel: Boolean);

  protected
    fSubMenu: TMenuItem;
    fDoneForm: TfmCodeFormatterDone;
    procedure FormatCurrent(_Sender: TObject);
    procedure Configure(_Sender: TObject);
    procedure About(_Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TDelforExpert }

constructor TDelforExpert.Create;
var
  ToolsMenu: TMenuItem;
  MenuItem: TMenuItem;
  Settings: TDefaultSettings;
begin
  inherited Create;
  try
    FFormatter := TCodeFormatterEngine.Create;
  except
    on e: exception do
      begin
        FFormatter := nil;
        MessageDlg(Format('%s: %s', [e.ClassName, e.Message]), mtError, [mbOK], 0);
        Exit;
      end;
  end;

  try
    Settings := BorlandDefaults;
    TDelforRegistry.ReadSettings(Settings.Parser, Settings.Wizard);
    FFormatter.Settings.Settings := Settings.Parser;
    FWizardSettings := Settings.Wizard;
  except
    // ignore
  end;

  fDoneForm := TfmCodeFormatterDone.Create(nil);
  ToolsMenu := GetToolsMenu;
  if not Assigned(ToolsMenu) then
    begin
      MessageDlg('Something is very wrong: The IDE''s main menu has less than 3 items.'#13#10 +
        'Wizard will not be installed.', mtError, [mbOK], 0);
      FFormatter.Free;
      FFormatter := nil;
      Exit;
    end;
  fSubmenu := TMenuItem.Create(nil);
  fSubMenu.Caption := 'Source Formatter';
  fSubMenu.Name := 'mi_twmsExpertMain';

  MenuItem := TMenuItem.Create(fSubMenu);
  MenuItem.Caption := 'Current &File';
  MenuItem.Name := 'mi_twmsExpertCurrent';
  MenuItem.OnClick := FormatCurrent;
  fSubMenu.Add(MenuItem);

  MenuItem := TMenuItem.Create(fSubMenu);
  MenuItem.Caption := '&Configure';
  MenuItem.Name := 'mi_twmsExpertConfigure';
  MenuItem.OnClick := Configure;
  fSubMenu.Add(MenuItem);

  MenuItem := TMenuItem.Create(fSubMenu);
  MenuItem.Caption := '&About';
  MenuItem.Name := 'mi_twmsExpertAbpit';
  MenuItem.OnClick := About;
  fSubMenu.Add(MenuItem);

  ToolsMenu.Insert(Settings.Wizard.ToolPosition, fSubMenu);

//  fNotifierIndex := (BorlandIdeServices as IOTAServices).AddNotifier(self);

  fKeyboardNotifier := TKeyboardNotifier.Create(self, Settings.Wizard.ShortCut);
end;

destructor TDelforExpert.Destroy;
begin
  if Assigned(fKeyboardNotifier) then
    fKeyboardNotifier.Unregister;
  FFormatter.Free;
  FFormatter := nil;
  fDoneForm.Free;
  fSubMenu.Free;
  inherited;
end;

function TDelforExpert.GetToolsMenu: TMenuItem;
var
  MainMenu: TMainMenu;
begin
  MainMenu := (BorlandIdeServices as INTAServices).MainMenu;
  Result := MainMenu.Items.Find('&Tools');
  if not Assigned(Result) then
    begin
      if MainMenu.Items.Count < 3 then
        begin
          Result := nil;
          Exit;
        end;
      Result := MainMenu.Items[MainMenu.Items.Count - 3];
    end;
end;

procedure TDelforExpert.Configure(_Sender: TObject);
var
  ToolsMenu: TMenuItem;
  Settings: TDefaultSettings;
begin
  if not Assigned(FFormatter) then
    exit;

  Settings.Parser := FFormatter.Settings.Settings;
  Settings.Wizard := FWizardSettings;

  if TfmCodeFormatterConfig.Execute(Settings, false) = mrOk then
    begin
      FWizardSettings := Settings.Wizard;
      FFormatter.Settings.Settings := Settings.Parser;
      ToolsMenu := GetToolsMenu;
      if Assigned(ToolsMenu) and Assigned(fSubMenu) and Assigned(FFormatter) then
        begin
          ToolsMenu.Remove(fSubMenu);
          ToolsMenu.Insert(FWizardSettings.ToolPosition, fSubMenu);
        end;
      if Assigned(fKeyboardNotifier) then
        fKeyboardNotifier.Unregister;
      fKeyboardNotifier := TKeyboardNotifier.Create(Self, FWizardSettings.ShortCut);
    end;
end;

procedure TDelforExpert.About(_Sender: TObject);
var
  frm: Tf_About;
begin
  frm := Tf_About.Create(nil);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TDelforExpert.FormatCurrent(_Sender: TObject);
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;

  function ReadCurrentFile(out _Content: string): Boolean;
  const
    BUFFSIZE = 10000;
  var
    Reader: IOTAEditReader;
    Buffer: PChar;
    s: string;
    BytesRead: LongInt;
    Position: longint;
  begin
    Reader := SourceEditor.CreateReader;
    Result := Assigned(Reader);
    if not Result then
      Exit;
    _Content := '';
    s := '';
    GetMem(Buffer, BUFFSIZE);
    try
      Position := 0;
      repeat
        BytesRead := Reader.GetText(Position, Buffer, BUFFSIZE);
        SetString(s, Buffer, BytesRead);
        _Content := _Content + s;
        Position := Position + BytesRead;
      until BytesRead <> BUFFSIZE;
    finally
      FreeMem(Buffer);
    end;
  end;

  procedure ReplaceCurrentFile(const _Content: string);
  var
    Writer: IOTAEditWriter;
  begin
    Writer := SourceEditor.CreateUndoableWriter;
    if not Assigned(Writer) then
      Exit;
    Writer.DeleteTo(MaxLongint);
    Writer.Insert(PChar(_Content));
  end;

var
  Filename: string;
  FullText: string;
  ShowDone: Boolean;
  Bookmarks: TBookmarkHandler;
  Breakpoints: TBreakpointHandler;
  Extension: string;
begin
  if not Assigned(FFormatter) then
    Exit;
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    begin
      Module := ModuleServices.CurrentModule;
      if not Assigned(Module) then
        Exit;

{$IFDEF delphi6}
      Editor := Module.CurrentEditor;
{$ELSE}
      Editor := Module.GetModuleFileEditor(0);
{$ENDIF}
      if not Assigned(Editor) or not Supports(Editor, IOTASourceEditor, SourceEditor) then
        Exit;

      Filename := SourceEditor.FileName;
      Extension := LowerCase(ExtractFileExt(Filename));
      if (Extension <> '.pas') and (Extension <> '.dpk') and (Extension <> '.dpr') and (Extension <> '.dpkw') then
        Exit;

      if not ReadCurrentFile(FullText) then
        Exit;

      Bookmarks := TBookmarkHandler.Create;
      try
        Breakpoints := TBreakpointHandler.Create;
        try
          Bookmarks.SaveBookmarks;
          Breakpoints.SaveBreakpoints;
          FFormatter.Settings.LoadCapFile(FWizardSettings.CapFileName);
          FFormatter.SetTextStr(PChar(FullText));
          if FFormatter.Execute then
            begin
              if FFormatter.Settings.FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept] then
                FFormatter.Settings.SaveCapFile(FWizardSettings.CapFileName);
              if FullText = FFormatter.GetTextStr then
                Exit; // nothing has changed, so we don't change the buffer
              FullText := FFormatter.GetTextStr;
              ReplaceCurrentFile(FullText);
              if FWizardSettings.ShowDoneDialog then
                begin
                  fDoneForm.ShowModal;
                  ShowDone := not fDoneForm.chk_DontShowAgain.Checked;
                  if Showdone <> FWizardSettings.ShowDoneDialog then
                    begin
                      FWizardSettings.ShowDoneDialog := ShowDone;
                      TDelforRegistry.WriteShowDone(ShowDone);
                    end;
                end;
            end;

          Breakpoints.RestoreBreakpoints;
          Bookmarks.RestoreBookmarks;
        finally
          Breakpoints.Free;
        end;
      finally
        Bookmarks.Free;
      end;
      FocusTopEditView;
    end;
end;

procedure TDelforExpert.FocusTopEditView;
var
  EditorServices: IOTAEditorServices;
  EditWindow: INTAEditWindow;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  if Assigned(EditorServices) then
    if Assigned(EditorServices.TopView) then
      begin
        EditWindow := EditorServices.TopView as INTAEditWindow;
        if Assigned(EditWindow) then
          if Assigned(EditWindow.Form) then
            EditWindow.Form.SetFocus;
      end;
end;

function TDelforExpert.GetName: string;
begin
  Result := 'dummzeuch.DelForExpert';
end;

procedure TDelforExpert.Execute;
begin
end;

function TDelforExpert.GetIDString: string;
begin
  Result := GetName;
end;

function TDelforExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{ TKeyboardNotifier }

constructor TKeyboardNotifier.Create(_Wizard: TDelforExpert;
  const _Shortcut: TShortcutDesc);
begin
  inherited Create;
  fWizard := _Wizard;
  fBindingIndex := -1;
  SetShortcut(_Shortcut);
end;

destructor TKeyboardNotifier.Destroy;
begin
  Unregister;
  inherited;
end;

procedure TKeyboardNotifier.BindKeyboard(const BindingServices: IOTAKeyBindingServices);

  procedure RegisterHotkey(_Hotkey: array of TShortcut);
  begin
    BindingServices.AddKeyBinding(_Hotkey, KeyProc, nil,
      kfImplicitShift or kfImplicitModifier, '', '');
  end;

begin
  if fHotkey.Second <> #0 then
    RegisterHotkey([fHotkey.First, Ord(fHotkey.Second)])
  else
    RegisterHotkey([fHotkey.First]);
end;

procedure TKeyboardNotifier.Unregister;
var
  Idx: Integer;
begin
  Idx := fBindingIndex;
  fBindingIndex := -1;
  if Idx <> -1 then
    Self.Unregister(Idx);
end;

procedure TKeyboardNotifier.Unregister(_Idx: Integer);
begin
  (BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(_Idx);
end;

function TKeyboardNotifier.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

procedure TKeyboardNotifier.KeyProc(const _Context: IOTAKeyContext;
  _KeyCode: TShortCut; var _BindingResult: TKeyBindingResult);
begin
  fWizard.FormatCurrent(nil);
  _BindingResult := krHandled;
end;

function TKeyboardNotifier.GetDisplayName: string;
begin
  Result := 'DelForExpert';
end;

function TKeyboardNotifier.GetName: string;
begin
  Result := fWizard.GetName;
end;

procedure TKeyboardNotifier.SetShortcut(_ShortCut: TShortCutDesc);
var
  KeyboardServices: IOTAKeyboardServices;
begin
  Unregister;
  fHotkey := _ShortCut;
  KeyboardServices := (BorlandIDEServices as IOTAKeyboardServices);
  fBindingIndex := KeyboardServices.AddKeyboardBinding(self);
end;

procedure Register;
begin
  RegisterPackageWizard(TDelforExpert.Create);
end;

end.
