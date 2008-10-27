// Dialog to select a macro template
unit GX_MacroSelect;

interface
uses
  Classes, Controls, Forms, ExtCtrls, StdCtrls, ComCtrls, CommCtrl, GX_MacroFile;

type
  TfmMacroSelect = class(TForm)
    pnlMain: TPanel;
    lvMacros: TListView;
    pnlFooter: TPanel;
    tbEnter: TMemo;
    pnlButtonsRight: TPanel;
    btnConfiguration: TButton;
    procedure tbEnterChange(Sender: TObject);
    procedure lstMacrosDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbEnterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
  private
    FMacroFile: TMacroFile;
    procedure SelectTemplate(Index: Integer);
    procedure LoadFormLayout;
    procedure SaveFormLayout;
    procedure SizeColumns;
    function ConfigurationKey: string;
  public
    function GetSelectedMacroCode: Integer;
    procedure LoadTemplates(AMacroFile: TMacroFile);
  end;

// Returns the index of the selected macro
function GetTemplateFromUser(ATemplateName: string; MacroFile: TMacroFile): Integer;

implementation

uses
  Messages, Windows,
  GX_MacroTemplates, GX_MacroTemplatesExpert, GX_ConfigurationInfo,
  GX_GenericUtils;

{$R *.dfm}

function GetTemplateFromUser(ATemplateName: string; MacroFile: TMacroFile): Integer;
begin
  Assert(Assigned(MacroFile));

  with TfmMacroSelect.Create(Application) do
  try
    LoadTemplates(MacroFile);
    tbEnter.Text := ATemplateName;
    tbEnter.SelStart := Length(ATemplateName);
    if ShowModal = mrOk then
      Result := GetSelectedMacroCode
    else
      Result := -1;
  finally
    Free;
  end;
end;

procedure TfmMacroSelect.SelectTemplate(Index: Integer);
begin
  lvMacros.Selected := lvMacros.Items[Index];
  lvMacros.ItemFocused := lvMacros.Selected;
  lvMacros.Selected.MakeVisible(False);
end;

procedure TfmMacroSelect.tbEnterChange(Sender: TObject);
var
  MacroName: string;
  ItemName: string;
  t: Integer;
begin
  MacroName := tbEnter.Text;
  for t := 0 to lvMacros.Items.Count - 1 do
  begin
    ItemName := lvMacros.Items[t].Caption;
    if Copy(ItemName, 1, Length(MacroName)) = MacroName then
    begin
      SelectTemplate(t);
      Exit;
    end;
  end;
  MessageBeep(MB_OK);
  lvMacros.Selected := nil;
end;

procedure TfmMacroSelect.lstMacrosDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmMacroSelect.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModalResult := mrOk;
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

function TfmMacroSelect.GetSelectedMacroCode: Integer;
var
  MacroName: string;
begin
  Result := -1;
  if lvMacros.Selected <> nil then
  begin
    MacroName := lvMacros.Selected.Caption;
    if Assigned(FMacroFile) then
      Result := FMacroFile.IndexOf(MacroName);
  end;
end;

procedure TfmMacroSelect.LoadTemplates(AMacroFile: TMacroFile);

  procedure AddMacroToList(const AMacroName, AMacroDesc: string);
  var
    ListItem: TListItem;
  begin
    ListItem := lvMacros.Items.Add;
    ListItem.Caption := AMacroName;
    ListItem.SubItems.Add(AMacroDesc);
  end;

  procedure FocusAndSelectFirstItem;
  begin
    if lvMacros.Items.Count > 0 then
      SelectTemplate(0);
  end;

var
  i: Integer;
begin
  FMacroFile := AMacroFile;
  lvMacros.Items.BeginUpdate;
  try
    lvMacros.Items.Clear;
    for i := 0 to AMacroFile.MacroCount - 1 do
      AddMacroToList(AMacroFile.MacroItems[i].Name, AMacroFile.MacroItems[i].Desc);
    FocusAndSelectFirstItem;
    SizeColumns;
  finally
    lvMacros.Items.EndUpdate;
  end;
end;

procedure TfmMacroSelect.tbEnterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) or (Key = VK_UP) or
    (Key = VK_NEXT) or (Key = VK_PRIOR) or
    (Key = VK_HOME) or (Key = VK_END) then
  begin
    SendMessage(lvMacros.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmMacroSelect.LoadFormLayout;
begin
  with TGExpertsSettings.Create(MacroTemplatesBaseKey) do
  try
    LoadForm(Self, ConfigurationKey, [fsSize]);
    lvMacros.Columns[0].Width := ReadInteger(ConfigurationKey, 'NameWidth', lvMacros.Columns[0].Width);
  finally
    Free;
  end;
end;

procedure TfmMacroSelect.SaveFormLayout;
begin
  with TGExpertsSettings.Create(MacroTemplatesBaseKey) do
  try
    if WindowState = wsNormal then // save only if not maximized/minimized
      SaveForm(Self, ConfigurationKey, [fsSize]);
    WriteInteger(ConfigurationKey, 'NameWidth', lvMacros.Columns[0].Width);
  finally
    Free;
  end;
end;

procedure TfmMacroSelect.FormCreate(Sender: TObject);
begin
  LoadFormLayout;
  SetDefaultFont(Self);
end;

procedure TfmMacroSelect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveFormLayout;
end;

procedure TfmMacroSelect.FormResize(Sender: TObject);
begin
  SizeColumns;
end;

procedure TfmMacroSelect.SizeColumns;
begin
  if lvMacros.Items.Count > 0 then
    ListView_SetColumnWidth(lvMacros.Handle, 1, ColumnTextWidth)
  else
    lvMacros.Columns[1].Width := 200;
end;

procedure TfmMacroSelect.btnConfigurationClick(Sender: TObject);
begin
  Assert(Assigned(FMacroFile));
  GetExpandMacroExpertReq.Configure;
  LoadTemplates(GetExpandMacroExpertReq.MacroFile);
end;

function TfmMacroSelect.ConfigurationKey: string;
begin
  Result := 'SelectMacro';
end;

end.

