unit GX_ePopupMenu;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  GX_BaseForm,
  GX_BaseExpert;

type
  TfmEditorPopupMenuExpertConfig = class(TfmBaseForm)
    pc_Main: TPageControl;
    ts_Experts: TTabSheet;
    ts_EditorExperts: TTabSheet;
    lb_EditorExperts: TListBox;
    lb_Experts: TListBox;
    lv_Selected: TListView;
    b_Ok: TButton;
    b_Cancel: TButton;
    b_Add: TButton;
    b_Remove: TButton;
    l_DuplicateShortcuts: TLabel;
    b_Default: TButton;
    b_ClearShortcut: TButton;
    procedure b_AddClick(Sender: TObject);
    procedure b_RemoveClick(Sender: TObject);
    procedure lb_EditorExpertsDblClick(Sender: TObject);
    procedure lb_ExpertsDblClick(Sender: TObject);
    procedure lv_SelectedEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lv_SelectedEdited(Sender: TObject; Item: TListItem; var s: string);
    procedure lv_SelectedDblClick(Sender: TObject);
    procedure lv_SelectedKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lv_SelectedChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure b_DefaultClick(Sender: TObject);
    procedure b_ClearShortcutClick(Sender: TObject);
  private
    function GetExpertIndex(const _ListView: TListView; const _Expert: TGX_BaseExpert): Integer;
    procedure CheckForDuplicates;
    procedure GetData(_sl: TStringList);
    procedure SetData(_sl: TStringList);
    procedure AddExpert(const _Key: string; const _ExpName: string; _Expert: TGX_BaseExpert);
    procedure RemoveExpert;
    procedure AddSelectedExpert;
    procedure AddSelectedEditorExpert;
    procedure EnableOKCancel;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Menus,
  StrUtils,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_dzVclUtils,
  GX_GExperts,
  GX_EditorExpertManager,
  GX_Experts,
  GX_MessageBox;

type
  TGxEditorPopupMenuExpert = class(TEditorExpert)
  private
    FFormHeight: Integer;
    FGExpertsShortcutMenu: TPopupMenu;
    FShortcuts: TStringList;
    procedure ShowConfigForm(_Sender: TObject);
    class procedure SetDefaults(_sl: TStringList);
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetDefaultShortCut: TShortCut; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  end;

{ TGxEditorPopupMenuExpert }

procedure TGxEditorPopupMenuExpert.Configure;
var
  frm: TfmEditorPopupMenuExpertConfig;
begin
  frm := TfmEditorPopupMenuExpertConfig.Create(nil);
  try
    frm.SetData(FShortcuts);
    frm.Height := FFormHeight;
    if frm.ShowModal <> mrok then
      Exit;
    frm.GetData(FShortcuts);
    FFormHeight := frm.Height;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TGxEditorPopupMenuExpert.Create;
begin
  inherited;
  FShortcuts := TStringList.Create;
end;

destructor TGxEditorPopupMenuExpert.Destroy;
begin
  FreeAndNil(FGExpertsShortcutMenu);
  FreeAndNil(FShortcuts);
  inherited;
end;

procedure TGxEditorPopupMenuExpert.Execute(Sender: TObject);
var
  ctl: TWinControl;
  pnt: TPoint;
  i: Integer;
  Key: string;
  idx: Integer;
  ExpName: string;
  Expert: TGX_BaseExpert;
begin
  ctl := Screen.ActiveControl;
  if Assigned(ctl) and (ctl.Name = 'Editor') and ctl.ClassNameIs('TEditControl') then begin
    if Assigned(FGExpertsShortcutMenu) then
      FreeAndNil(FGExpertsShortcutMenu);
    FGExpertsShortcutMenu := TPopupMenu.Create(nil);
    for i := 0 to FShortcuts.Count - 1 do begin
      Key := FShortcuts.Names[i];
      ExpName := FShortcuts.Values[Key];
      if (Key <> '') and (Key <> 'X') then begin
        if GExpertsInst.EditorExpertManager.FindExpert(ExpName, idx) then begin
          Expert := GExpertsInst.EditorExpertManager.EditorExpertList[idx];
          TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + Key + ' ' + Expert.GetDisplayName,
            Expert.Execute);
        end else if GExpertsInst.FindExpert(ExpName, idx) then begin
          Expert := GExpertsInst.ExpertList[idx];
          TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + Key + ' ' + Expert.GetDisplayName,
            Expert.Execute);
        end;
      end;
    end;
    TPopupMenu_AppendMenuItem(FGExpertsShortcutMenu, '&' + 'X' + ' ' + 'Configure',
      ShowConfigForm);
    pnt := ctl.ClientToScreen(Point(0, 0));
    FGExpertsShortcutMenu.Popup(pnt.X, pnt.Y);
  end;
end;

procedure TGxEditorPopupMenuExpert.ShowConfigForm(_Sender: TObject);
begin
  GExpertsInst.ShowConfigurationForm;
end;

function TGxEditorPopupMenuExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + Ord('H');
end;

function TGxEditorPopupMenuExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Editor Popup Menu';
begin
  Result := SDisplayName;
end;

function TGxEditorPopupMenuExpert.GetHelpString: string;
resourcestring
  SGxEditorPopupMenuExpertHelp =
    'Adds a new shortcut to the editor that shows a configurable popup menu ' +
    'as an alternative to the GExperts main menu.';
begin
  Result := SGxEditorPopupMenuExpertHelp;
end;

class function TGxEditorPopupMenuExpert.GetName: string;
const
  SName = 'EditorPopupMenu';
begin
  Result := SName;
end;

function TGxEditorPopupMenuExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TGxEditorPopupMenuExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FShortcuts.Clear;
  if Settings.SectionExists('menu') then begin
    Settings.ReadSectionValues('menu', FShortcuts);
  end else begin
    SetDefaults(FShortcuts);
  end;
  FFormHeight := Settings.ReadInteger('FormHeight', FFormHeight);
end;

procedure TGxEditorPopupMenuExpert.InternalSaveSettings(Settings: TExpertSettings);
var
  i: Integer;
  s: string;
  MnuSettings: TExpertSettings;
begin
  inherited;
  Settings.EraseSection('menu');
  MnuSettings := Settings.CreateExpertSettings('menu');
  try
    for i := 0 to FShortcuts.Count - 1 do begin
      s := FShortcuts.Names[i];
      MnuSettings.WriteString(s, FShortcuts.Values[s]);
    end;
  finally
    FreeAndNil(MnuSettings);
  end;
  Settings.WriteInteger('FormHeight', FFormHeight);
end;

class procedure TGxEditorPopupMenuExpert.SetDefaults(_sl: TStringList);
begin
  _sl.Add('A=Align');
  _sl.Add('B=BookmarksExpert');
  _sl.Add('C=ClassBrowser');
  _sl.Add('D=DateTime');
  _sl.Add('E=EditorExpertsMenu');
  _sl.Add('F=CodeFormatter');
  _sl.Add('G=GrepSearch');
  _sl.Add('H=ClipboardHistory');
  _sl.Add('I=SelectIdent');
  _sl.Add('J=SortLines');
  _sl.Add('L=CodeLibrarian');
  _sl.Add('M=MacroLibrary');
  _sl.Add('O=OpenFile');
  _sl.Add('P=ProcedureList');
  _sl.Add('R=ReverseStatement');
  _sl.Add('S=MessageDialog');
  _sl.Add('T=MacroTemplates');
  _sl.Add('U=UsesClauseMgr');
  _sl.Add('X=Configure');
end;

{ TfmEditorPopupMenuExpertConfig }

constructor TfmEditorPopupMenuExpertConfig.Create(_Owner: TComponent);
var
  i: Integer;
  EdExpManager: TGxEditorExpertManager;
  Expert: TGX_BaseExpert;
begin
  inherited;

  TControl_SetMinConstraints(Self);
  Self.Constraints.MaxWidth := Self.Width;

  for i := 0 to GExpertsInst.ExpertCount - 1 do begin
    Expert := GExpertsInst.ExpertList[i];
    lb_Experts.Items.AddObject(Expert.GetDisplayName, Expert);
  end;

  EdExpManager := GExpertsInst.EditorExpertManager;
  for i := 0 to EdExpManager.EditorExpertCount - 1 do begin
    Expert := EdExpManager.EditorExpertList[i];
    if not (Expert is TGxEditorPopupMenuExpert) then
      lb_EditorExperts.Items.AddObject(Expert.GetDisplayName, Expert);
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.EnableOKCancel;
begin
  b_Ok.Enabled := True;
  b_Cancel.Enabled := True;
end;

procedure TfmEditorPopupMenuExpertConfig.AddSelectedEditorExpert;
var
  ExpName: string;
  Ex: TEditorExpert;
begin
  TListBox_GetSelectedObject(lb_EditorExperts, Pointer(Ex));
  ExpName := Ex.GetName;
  AddExpert('', ExpName, Ex);
end;

procedure TfmEditorPopupMenuExpertConfig.AddSelectedExpert;
var
  ExpName: string;
  Ex: TGX_Expert;
begin
  TListBox_GetSelectedObject(lb_Experts, Pointer(Ex));
  ExpName := Ex.GetName;
  AddExpert('', ExpName, Ex);
end;

procedure TfmEditorPopupMenuExpertConfig.b_AddClick(Sender: TObject);
begin
  if pc_Main.ActivePage = ts_Experts then begin
    AddSelectedExpert;
  end else begin
    AddSelectedEditorExpert;
  end;
end;

// ---------------------------------------------

type
  TClearIndividualShortcutMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TClearIndividualShortcutMessage }

function TClearIndividualShortcutMessage.GetMessage: string;
resourcestring
  SClearIndividualShortcut =
    'This will remove the shortcut assigned to the individual expert. So this expert can ' +
    'only be called via the GExperts main menu and via this editor menu. ' +
    'Do you want to clear the shortcut?';
begin
  Result := SClearIndividualShortcut;
end;

procedure TfmEditorPopupMenuExpertConfig.b_ClearShortcutClick(Sender: TObject);
var
  li: TListItem;
  Ex: TGX_BaseExpert;
begin
  if ShowGxMessageBox(TClearIndividualShortcutMessage) <> mrYes then
    Exit;
  if not TListView_TryGetSelected(lv_Selected, li) then
    Exit;
  Ex := TGX_BaseExpert(li.Data);
  Ex.ShortCut := 0
end;

procedure TfmEditorPopupMenuExpertConfig.b_RemoveClick(Sender: TObject);
begin
  RemoveExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.b_DefaultClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    TGxEditorPopupMenuExpert.SetDefaults(sl);
    SetData(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.CheckForDuplicates;
var
  i: Integer;
  sl: TStringList;
  DupeFound: Boolean;
begin
  sl := TStringList.Create;
  try
    GetData(sl);
    sl.Sort;
    DupeFound := False;
    for i := 1 to sl.Count - 1 do
      if sl.Names[i] = sl.Names[i - 1] then begin
        DupeFound := True;
        break;
      end;
    l_DuplicateShortcuts.Visible := DupeFound;
  finally
    FreeAndNil(sl);
  end;
end;

function TfmEditorPopupMenuExpertConfig.GetExpertIndex(
  const _ListView : TListView;
  const _Expert: TGX_BaseExpert): Integer;
var
  i : Integer;
begin
  Result := -1;
  if not Assigned(_ListView) then Exit;
  if not Assigned(_Expert) then Exit;

  for i := 0 to _ListView.Items.Count-1 do
  begin
    if _ListView.Items[i].Data = _Expert then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.AddExpert(const _Key: string; const _ExpName: string;
  _Expert: TGX_BaseExpert);
var
  li: TListItem;
begin
  if GetExpertIndex(lv_Selected, _Expert) >= 0
  then begin
    Exit; // expert is already in "lv_Selected"
  end;

  li := lv_Selected.Items.Add;
  if _Key <> '' then
    li.Caption := _Key
  else
    li.Caption := LeftStr(_ExpName, 1);
  li.Data := _Expert;
  li.SubItems.Add(_Expert.GetDisplayName);
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.GetData(_sl: TStringList);
var
  i: Integer;
  li: TListItem;
begin
  _sl.Clear;
  for i := 0 to lv_Selected.Items.Count - 1 do begin
    li := lv_Selected.Items[i];
    if Assigned(li.Data) then begin
      if TObject(li.Data) is TGX_BaseExpert then
        _sl.Add(li.Caption + '=' + TGX_BaseExpert(li.Data).GetName);
    end;
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.lb_EditorExpertsDblClick(Sender: TObject);
begin
  AddSelectedEditorExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lb_ExpertsDblClick(Sender: TObject);
begin
  AddSelectedExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  case Change of
    ctState, ctText:
      EnableOKCancel;
  end;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedDblClick(Sender: TObject);
begin
  RemoveExpert;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedEdited(Sender: TObject; Item: TListItem;
  var s: string);
begin
  EnableOKCancel;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  b_Ok.Enabled := False;
  b_Cancel.Enabled := False;
end;

procedure TfmEditorPopupMenuExpertConfig.lv_SelectedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  li: TListItem;
begin
  if Key = VK_F2 then begin
    li := lv_Selected.Selected;
    if Assigned(li) then
      li.EditCaption;
    Key := 0;
  end;
end;

procedure TfmEditorPopupMenuExpertConfig.RemoveExpert;
begin
  if lv_Selected.ItemIndex = -1 then
    Exit;
  lv_Selected.DeleteSelected;
  CheckForDuplicates;
end;

procedure TfmEditorPopupMenuExpertConfig.SetData(_sl: TStringList);
var
  i: Integer;
  Key: string;
  ExpName: string;
  idx: Integer;
begin
  lv_Selected.OnChange := nil;
  lv_Selected.Items.BeginUpdate;
  try
    lv_Selected.Clear;
    for i := 0 to _sl.Count - 1 do begin
      Key := _sl.Names[i];
      ExpName := _sl.Values[Key];
      if GExpertsInst.FindExpert(ExpName, idx) then begin
        AddExpert(Key, ExpName, GExpertsInst.ExpertList[idx]);
      end else if GExpertsInst.EditorExpertManager.FindExpert(ExpName, idx) then begin
        AddExpert(Key, ExpName, GExpertsInst.EditorExpertManager.EditorExpertList[idx]);
      end;
    end;
  finally
    lv_Selected.Items.EndUpdate;
    lv_Selected.OnChange := lv_SelectedChange;
  end;
end;

initialization
  RegisterEditorExpert(TGxEditorPopupMenuExpert);
end.
