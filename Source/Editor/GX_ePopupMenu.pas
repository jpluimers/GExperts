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
    procedure b_AddClick(Sender: TObject);
    procedure b_RemoveClick(Sender: TObject);
    procedure lb_EditorExpertsDblClick(Sender: TObject);
    procedure lb_ExpertsDblClick(Sender: TObject);
    procedure lv_SelectedEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lv_SelectedEdited(Sender: TObject; Item: TListItem; var s: string);
    procedure lv_SelectedDblClick(Sender: TObject);
    procedure lv_SelectedKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lv_SelectedChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
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
  GX_Experts;

type
  TGxEditorPopupMenuExpert = class(TEditorExpert)
  private
    FFormHeight: integer;
    FGExpertsShortcutMenu: TPopupMenu;
    FShortcuts: TStringList;
    procedure ShowConfigForm(_Sender: TObject);
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
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
  ShortCut := scCtrl + Ord('H');
  FShortcuts := TStringList.Create;
end;

destructor TGxEditorPopupMenuExpert.Destroy;
begin
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
      if (Key <> '') then begin
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

function TGxEditorPopupMenuExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Editor Popup Menu';
begin
  Result := SDisplayName;
end;

procedure TGxEditorPopupMenuExpert.GetHelpString(List: TStrings);
resourcestring
  SGxEditorPopupMenuExpertHelp =
    'Adds a new shortcut to the editor that shows a configurable popup menu ' +
    'as an alternative to the GExperts main menu.';
begin
  List.Text := SGxEditorPopupMenuExpertHelp;
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

procedure TGxEditorPopupMenuExpert.InternalLoadSettings(Settings: TGExpertsSettings);
var
  MenuSection: string;
begin
  inherited;
  FShortcuts.Clear;
  MenuSection := ConfigurationKey + PathDelim + 'menu';
  Settings.ReadSectionValues(MenuSection, FShortcuts);
  FFormHeight :=  Settings.ReadInteger(ConfigurationKey, 'FormHeight', FFormHeight);
end;

procedure TGxEditorPopupMenuExpert.InternalSaveSettings(Settings: TGExpertsSettings);
var
  MenuSection: string;
  ExpSettings: TExpertSettings;
  i: Integer;
  s: string;
begin
  inherited;
  MenuSection := ConfigurationKey + PathDelim + 'menu';
  Settings.EraseSection(MenuSection);
  ExpSettings := Settings.CreateExpertSettings(MenuSection);
  try
    for i := 0 to FShortcuts.Count - 1 do begin
      s := FShortcuts.Names[i];
      ExpSettings.WriteString(s, FShortcuts.Values[s]);
    end;
  finally
    FreeAndNil(ExpSettings);
  end;
  Settings.WriteInteger(ConfigurationKey, 'FormHeight', FFormHeight);
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

procedure TfmEditorPopupMenuExpertConfig.b_RemoveClick(Sender: TObject);
begin
  RemoveExpert;
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

procedure TfmEditorPopupMenuExpertConfig.AddExpert(const _Key: string; const _ExpName: string;
  _Expert: TGX_BaseExpert);
var
  li: TListItem;
begin
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
  case change of
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
