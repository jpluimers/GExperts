unit GX_Configure;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  StdCtrls, ComCtrls, ExtCtrls, GX_EditorExpert, GX_BaseForm;

type
  TfmConfiguration = class(TfmBaseForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    pcConfig: TPageControl;
    tshExperts: TTabSheet;
    tshGeneral: TTabSheet;
    sbxExperts: TScrollBox;
    dlgHelpFile: TOpenDialog;
    tshIDE: TTabSheet;
    tshEditorExperts: TTabSheet;
    tshEditor: TTabSheet;
    gbxEditorTabs: TGroupBox;
    chkMultiLine: TCheckBox;
    chkHotTrack: TCheckBox;
    chkButtons: TCheckBox;
    gbxEditorToolBar: TGroupBox;
    chkEditorToolBar: TCheckBox;
    gbxIDEMenu: TGroupBox;
    dlgUIFont: TFontDialog;
    chkDisableEDTEnhancements: TCheckBox;
    chkEditTabButtonsFlat: TCheckBox;
    rgAlign: TRadioGroup;
    gbxTabDockHost: TGroupBox;
    chkMultiLineTabDockHost: TCheckBox;
    chkDefaultMultiLineTabDockHost: TCheckBox;
    gbxCompPalette: TGroupBox;
    chkCPMultiLine: TCheckBox;
    chkCPAsButtons: TCheckBox;
    chkCPTabsInPopup: TCheckBox;
    chkCPFlat: TCheckBox;
    chkCPTabsInPopupAlphaSort: TCheckBox;
    chkCPScrollOpposite: TCheckBox;
    chkCPRaggedRight: TCheckBox;
    chkAlphabetizeMenu: TCheckBox;
    chkHideWindowMenu: TCheckBox;
    chkMoveComponentMenu: TCheckBox;
    btnConfigureToolBar: TButton;
    chkPlaceGxMainMenuInToolsMenu: TCheckBox;
    chkMiddleButtonClose: TCheckBox;
    tshDebug: TTabSheet;
    chkEditorKeyTracing: TCheckBox;
    btnEnumerateModules: TButton;
    btnEumerateActions: TButton;
    btnGetFonts: TButton;
    btnAppBuilder: TButton;
    gbxIDEForms: TGroupBox;
    chkEnhanceDialogs: TCheckBox;
    chkOIFontNames: TCheckBox;
    gbxFonts: TGroupBox;
    btnOIFont: TButton;
    btnCPFont: TButton;
    chkOIFontEnabled: TCheckBox;
    chkCPFontEnabled: TCheckBox;
    gbxFileSaving: TGroupBox;
    lblEvery: TLabel;
    lblMinutes: TLabel;
    chkAutoSave: TCheckBox;
    edtMinutes: TEdit;
    udMinutes: TUpDown;
    btnEditView: TButton;
    pnlButtonsRight: TPanel;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    dlgFont: TFontDialog;
    pnlExperts: TPanel;
    gbxKeyboard: TGroupBox;
    btnConfigure: TButton;
    btnShortcut: TButton;
    meHelp: TMemo;
    chkDisableAllEditorExperts: TCheckBox;
    lvEditorExperts: TListView;
    pnlGeneral: TPanel;
    gbxLocations: TGroupBox;
    lblVCL: TLabel;
    lblConfig: TLabel;
    lblHelp: TLabel;
    sbVCLDir: TButton;
    sbConfigDir: TButton;
    sbHelpFile: TButton;
    edVCLPath: TEdit;
    edConfigPath: TEdit;
    edHelpFile: TEdit;
    gbxCustomFont: TGroupBox;
    chkUseCustomFont: TCheckBox;
    btnCustomFont: TButton;
    pnlGeneralSpacer: TPanel;
    pnlExpertLayout: TPanel;
    imgExpert: TImage;
    chkExpert: TCheckBox;
    edtExpert: THotKey;
    btnExpert: TButton;
    pnlExpertsFilter: TPanel;
    tmrFilter: TTimer;
    lblFilter: TLabel;
    edtFilter: TEdit;
    chkHideNavbar: TCheckBox;
    chkEnhanceSearchPaths: TCheckBox;
    chkEnhanceToolProperties: TCheckBox;
    chkReplaceListWithMemo: TCheckBox;
    chkAllowResize: TCheckBox;
    chkRememberPosition: TCheckBox;
    tshSuppressedMessages: TTabSheet;
    gbSuppressedMessages: TGroupBox;
    lbSuppressedMesages: TListBox;
    btnDeleteSuppressedMessage: TButton;
    btnClearSuppressedMessages: TButton;
    chkEnhanceInstallPackages: TCheckBox;
    procedure btnEnumerateModulesClick(Sender: TObject);
    procedure chkEditorKeyTracingClick(Sender: TObject);
    procedure sbVCLDirClick(Sender: TObject);
    procedure sbConfigDirClick(Sender: TObject);
    procedure sbHelpFileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure lvEditorExpertsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnShortCutClick(Sender: TObject);
    procedure EditShortCutClick(Sender: TObject);
    procedure chkDisableAllEditorExpertsClick(Sender: TObject);
    procedure chkFontEnabledClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure chkDisableEDTEnhancementsClick(Sender: TObject);
    procedure chkAutoSaveClick(Sender: TObject);
    procedure chkCPAsButtonsClick(Sender: TObject);
    procedure chkCPTabsInPopupClick(Sender: TObject);
    procedure chkCPMultiLineClick(Sender: TObject);

    procedure ConfigureEditorExpertClick(Sender: TObject);
    procedure chkButtonsClick(Sender: TObject);
    procedure chkEditorToolBarClick(Sender: TObject);
    procedure chkMultiLineTabDockHostClick(Sender: TObject);
    procedure sbxExpertsMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sbxExpertsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure btnConfigureToolBarClick(Sender: TObject);
    procedure pcConfigChange(Sender: TObject);
    procedure lvEditorExpertsDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnEumerateActionsClick(Sender: TObject);
    procedure btnGetFontsClick(Sender: TObject);
    procedure btnAppBuilderClick(Sender: TObject);
    procedure btnEditViewClick(Sender: TObject);
    procedure btnCustomFontClick(Sender: TObject);
    procedure tmrFilterTimer(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure chkEnhanceDialogsClick(Sender: TObject);
    procedure btnDeleteSuppressedMessageClick(Sender: TObject);
    procedure btnClearSuppressedMessagesClick(Sender: TObject);
  private
    FOIFont: TFont;
    FCPFont: TFont;
    FThumbSize: Integer;
    procedure HideUnsupportedIdeItems;
    procedure HideUnsupportedEditorItems;
    procedure ConfigureEditorExpertShortCut(EditorExpert: TEditorExpert);
    procedure LoadExperts;
    procedure SaveExperts;

    procedure LoadGeneral;
    procedure SaveGeneral;

    procedure LoadIdeEnhancements;
    procedure SaveIdeEnhancements;

    procedure LoadEditorEnhancements;
    procedure SaveEditorEnhancements;

    // Editor experts save themselves automatically
    // hence there is no SaveEditorExperts method
    procedure LoadEditorExperts;

    procedure LoadSuppressedMessages;

    procedure FilterVisibleExperts;
    procedure edVCLPathOnDropFiles(_Sender: TObject; _Files: TStrings);
    procedure edConfigPathDropFiles(_Sender: TObject; _Files: TStrings);
    procedure edHelpFileDropFiles(_Sender: TObject; _Files: TStrings);
    procedure UpdateIdeDialogCheckboxes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils,
  GX_GxUtils, GX_EditorEnhancements, GX_Experts, GX_IdeEnhance,
  GX_ConfigurationInfo, GX_EditorExpertManager, GX_MessageBox,
  GX_GExperts, GX_EditorShortcut, GX_MenuActions, GX_GenericUtils, GX_IdeUtils,
  GX_OtaUtils, GX_dzVclUtils, GX_KbdShortCutBroker;

resourcestring
  GExpertsShortcutDisabled = '<disabled>';

type
  TShowOldComCtrlVersionMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

procedure SetupGroupBox(Box: TGroupBox; Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to Box.ControlCount - 1 do
    Box.Controls[i].Enabled := Enable;
end;

// **************************************************************

constructor TfmConfiguration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOIFont := TFont.Create;
  FCPFont := TFont.Create;

  TWinControl_ActivateDropFiles(edVCLPath, edVCLPathOnDropFiles);
  TEdit_ActivateAutoComplete(edVCLPath, [acsFileSystem], [actSuggest]);
  TWinControl_ActivateDropFiles(edConfigPath, edConfigPathDropFiles);
  TEdit_ActivateAutoComplete(edConfigPath, [acsFileSystem], [actSuggest]);
  TWinControl_ActivateDropFiles(edHelpFile, edHelpFileDropFiles);
  TEdit_ActivateAutoComplete(edHelpFile, [acsFileSystem], [actSuggest]);

  pcConfig.ActivePage := tshExperts;
  LoadExperts;
  LoadGeneral;

  LoadEditorExperts;
  chkDisableAllEditorExperts.Checked := not ConfigInfo.EditorExpertsEnabled;

  LoadIdeEnhancements;

  chkPlaceGxMainMenuInToolsMenu.Checked := ConfigInfo.PlaceGxMainMenuInToolsMenu;
  chkAlphabetizeMenu.Checked := ConfigInfo.AlphabetizeMenu;
  chkHideWindowMenu.Checked := ConfigInfo.HideWindowMenu;
  chkMoveComponentMenu.Checked := ConfigInfo.MoveComponentMenu;

  LoadEditorEnhancements;
  chkEditorKeyTracing.Checked := GxOtaGetEditorKeyTracingEnabled;
  chkDisableEDTEnhancements.Checked := not EditorEnhancements.Enabled;
  HideUnsupportedIdeItems;

  HideUnsupportedEditorItems;

  LoadSuppressedMessages;

  tshDebug.TabVisible := False;
end;

destructor TfmConfiguration.Destroy;
begin
  FreeAndNil(FOIFont);
  FreeAndNil(FCPFont);

  inherited Destroy;
end;

procedure TfmConfiguration.edVCLPathOnDropFiles(_Sender: TObject; _Files: TStrings);
begin
  edVCLPath.Text := _Files[0];
end;

procedure TfmConfiguration.edConfigPathDropFiles(_Sender: TObject; _Files: TStrings);
begin
  edConfigPath.Text := _Files[0];
end;

procedure TfmConfiguration.edHelpFileDropFiles(_Sender: TObject; _Files: TStrings);
begin
  edHelpFile.Text := _Files[0];
end;

procedure TfmConfiguration.LoadGeneral;
begin
  edVCLPath.Text := ConfigInfo.VclPath;
  edConfigPath.Text := ConfigInfo.ConfigPath;
  edHelpFile.Text := ConfigInfo.HelpFile;
  chkUseCustomFont.Checked := ConfigInfo.EnableCustomFont;
  dlgUIFont.Font.Assign(ConfigInfo.CustomFont);
end;

procedure TfmConfiguration.SaveGeneral;
begin
  ConfigInfo.VclPath := edVCLPath.Text;
  ConfigInfo.ConfigPath := edConfigPath.Text;
  ConfigInfo.HelpFile := edHelpFile.Text;
  ConfigInfo.EnableCustomFont := chkUseCustomFont.Checked;
  ConfigInfo.CustomFont.Assign(dlgUIFont.Font);
  ConfigInfo.UpdateScreenForms;
end;

procedure TfmConfiguration.LoadExperts;
resourcestring
  SConfigureButtonCaption = 'Configure...';
var
  Panel: TPanel;
  i: Integer;
  AnExpert: TGX_Expert;
  RowWidth: Integer;
  RowHeight: Integer;
begin
  RowWidth := sbxExperts.Width * 3;
  RowHeight := pnlExpertLayout.Height;
  FThumbSize := RowHeight;
  for i := 0 to GExpertsInst.ExpertCount - 1 do
  begin
    Panel := TPanel.Create(Self);
    Panel.Parent := sbxExperts;
    Panel.SetBounds(0, i * RowHeight, RowWidth, RowHeight);
    Panel.Tag := i;
    Panel.FullRepaint := False;

    AnExpert := GExpertsInst.ExpertList[i];

    with TImage.Create(Self) do
    begin
      Parent := Panel;
      SetBounds(imgExpert.Left, imgExpert.Top, imgExpert.Width, imgExpert.Height);
      Picture.Bitmap.Assign(AnExpert.Bitmap);
      Transparent := True;
      Center := True;
      Stretch := False;
    end;

    with TCheckBox.Create(sbxExperts) do
    begin
      Parent := Panel;
      SetBounds(chkExpert.Left, chkExpert.Top, chkExpert.Width, chkExpert.Height);
      Caption := AnExpert.GetDisplayName;
      Checked := AnExpert.Active;
      Tag := i;
    end;

    with THotKey.Create(sbxExperts) do
    begin
      Parent := Panel;
      SetBounds(edtExpert.Left, edtExpert.Top, edtExpert.Width, edtExpert.Height);
      HotKey := AnExpert.ShortCut;
      Visible := AnExpert.HasMenuItem;
      Tag := i;
    end;

    if AnExpert.HasConfigOptions then
    begin
      with TButton.Create(Self) do
      begin
        Parent := Panel;
        Caption := SConfigureButtonCaption;
        SetBounds(btnExpert.Left, btnExpert.Top, btnExpert.Width, btnExpert.Height);
        OnClick := ConfigureEditorExpertClick;
        Tag := i;
      end;
    end;
  end;
  sbxExperts.VertScrollBar.Range := GExpertsInst.ExpertCount * RowHeight;
  pnlExpertLayout.Visible := False;
end;

procedure TfmConfiguration.SaveExperts;
var
  AControl: TControl;
  AnExpert: TGX_Expert;
  i: Integer;
begin
  for i := 0 to sbxExperts.ComponentCount - 1 do
  begin
    AControl := sbxExperts.Components[i] as TControl;

    AnExpert := GExpertsInst.ExpertList[AControl.Tag];
    if AControl is TCheckBox then
      AnExpert.Active := TCheckBox(AControl).Checked
    else if AControl is THotKey then
      AnExpert.ShortCut := THotKey(AControl).HotKey;
  end;

  for i := 0 to GExpertsInst.ExpertCount - 1 do
    GExpertsInst.ExpertList[i].SaveSettings;
end;

procedure TfmConfiguration.sbVCLDirClick(Sender: TObject);
var
  TempString: string;
begin
  TempString := edVCLPath.Text;
  if GetDirectory(TempString) then
    edVCLPath.Text := TempString;
end;

procedure TfmConfiguration.sbConfigDirClick(Sender: TObject);
var
  TempString: string;
begin
  TempString := edConfigPath.Text;
  if GetDirectory(TempString) then
    edConfigPath.Text := TempString;
end;

procedure TfmConfiguration.sbHelpFileClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  dlgHelpFile.InitialDir := ExtractFilePath(edHelpFile.Text);

  CurrentIdeFolder := GetCurrentDir;
  try
    if GetOpenSaveDialogExecute(dlgHelpFile) then
      edHelpFile.Text := dlgHelpFile.FileName;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

procedure TfmConfiguration.btnOKClick(Sender: TObject);
begin
  GxKeyboardShortCutBroker.BeginUpdate;
  try
    SaveGeneral;
    SaveExperts;
    SaveIdeEnhancements;
    SaveEditorEnhancements;
    ConfigInfo.SaveSettings;
    GXMenuActionManager.ArrangeMenuItems;
  finally
    GxKeyboardShortCutBroker.EndUpdate;
  end;

  ModalResult := mrOk;
end;

procedure TfmConfiguration.btnHelpClick(Sender: TObject);
var
  ActivePage: TTabSheet;
begin
  ActivePage := pcConfig.ActivePage;
  if ActivePage = tshEditorExperts then
    GxContextHelp(Self, 29)
  else if ActivePage = tshIDE then
    GxContextHelp(Self, 30)
  else if ActivePage = tshEditor then
    GxContextHelp(Self, 35)
  else
    GxContextHelp(Self, 12);
end;

procedure TfmConfiguration.ConfigureEditorExpertClick(Sender: TObject);
begin
  GExpertsInst.ExpertList[(Sender as TButton).Tag].Configure;
end;

procedure TfmConfiguration.lvEditorExpertsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  EditorExpert: TEditorExpert;
  Idx: integer;
begin
  {$IFOPT D+} SendDebug('TfmConfiguration.lvEditorExpertsChange'); {$ENDIF}

  if not TListView_TryGetSelected(lvEditorExperts, Idx) then
  begin
    btnConfigure.Enabled := False;
    btnShortCut.Enabled := False;
    meHelp.Clear;
    Exit;
  end;

  Assert(Assigned(GExpertsInst.EditorExpertManager));
  EditorExpert := GExpertsInst.EditorExpertManager.EditorExpertList[Idx];

  meHelp.Lines.BeginUpdate;
  try
    meHelp.Lines.Clear;
    EditorExpert.GetHelpString(meHelp.Lines);
  finally
    meHelp.SelStart := 0;
    meHelp.Lines.EndUpdate;
    btnConfigure.Enabled := EditorExpert.HasConfigOptions;
    btnShortCut.Enabled := True;
  end;
end;

procedure TfmConfiguration.btnClearSuppressedMessagesClick(Sender: TObject);
var
  Settings: TGExpertsSettings;
begin
  Settings := TGExpertsSettings.Create;
  try
    Settings.EraseSection(TGxMsgBoxAdaptor.ConfigurationKey);
  finally
    Settings.Free;
  end;
  LoadSuppressedMessages;
end;

procedure TfmConfiguration.btnConfigureClick(Sender: TObject);
var
  EditorExpert: TEditorExpert;
  Idx: Integer;
begin
  if not TListView_TryGetSelected(lvEditorExperts, Idx) then
    Exit;

  Assert(Assigned(GExpertsInst.EditorExpertManager));

  EditorExpert := GExpertsInst.EditorExpertManager.EditorExpertList[Idx];
  EditorExpert.Configure;
  EditorExpert.SaveSettings;
end;

procedure TfmConfiguration.btnShortCutClick(Sender: TObject);
var
  Idx: Integer;
begin
  if not TListView_TryGetSelected(lvEditorExperts, Idx) then
    Exit;

  Assert(Assigned(GExpertsInst.EditorExpertManager));

  ConfigureEditorExpertShortCut(GExpertsInst.EditorExpertManager.EditorExpertList[Idx])
end;

procedure TfmConfiguration.chkEditorKeyTracingClick(Sender: TObject);
begin
  GxOtaSetEditorKeyTracingEnabled(chkEditorKeyTracing.Checked);
end;

procedure TfmConfiguration.LoadEditorExperts;
var
  i: Integer;
  AnExpert: TEditorExpert;
  ListItem: TListItem;
  GxEditorExpertManager: TGxEditorExpertManager;
begin
  {$IFOPT D+} SendDebug('TfmConfiguration.LoadEditorExperts'); {$ENDIF}
  lvEditorExperts.Items.BeginUpdate;
  try
    lvEditorExperts.Items.Clear;
    if not Assigned(GExpertsInst.EditorExpertManager) then
      Exit;

    GxEditorExpertManager := GExpertsInst.EditorExpertManager;

    for i := 0 to GxEditorExpertManager.EditorExpertCount - 1 do
    begin
      AnExpert := GxEditorExpertManager.EditorExpertList[i];
      ListItem := lvEditorExperts.Items.Add;
      ListItem.Caption := AnExpert.DisplayName;
      ListItem.SubItems.Add(ShortCutToText(AnExpert.ShortCut));
    end;
  finally
    lvEditorExperts.Items.EndUpdate;
  end;
end;

procedure TfmConfiguration.LoadIdeEnhancements;
begin
  Assert(IdeEnhancements <> nil);

  // Multi-line component palette
  chkCPMultiLine.Checked := IdeEnhancements.CPMultiLine;
  chkCPAsButtons.Checked := IdeEnhancements.CPAsButtons;
  chkCPScrollOpposite.Checked := IdeEnhancements.CPScrollOpposite;
  chkCPRaggedRight.Checked := IdeEnhancements.CPRaggedRight;
  chkCPFlat.Checked := IdeEnhancements.CPFlatButtons;
  chkCPTabsInPopup.Checked := IdeEnhancements.CPTabsInPopup;
  chkCPTabsInPopupAlphaSort.Checked := IdeEnhancements.CPTabsInPopupAlphaSort;

  // Tab-docked hosts
  chkMultiLineTabDockHost.Checked := IdeEnhancements.MultiLineTabDockHost;
  chkDefaultMultiLineTabDockHost.Checked := IdeEnhancements.DefaultMultiLineTabDockHost;

  chkEnhanceDialogs.Checked := IdeEnhancements.EnhanceIDEForms;
  chkAllowResize.Checked := IdeEnhancements.IdeFormsAllowResize;
  chkRememberPosition.Checked := IdeEnhancements.IdeFormsRememberPosition;
  chkEnhanceSearchPaths.Checked := IdeEnhancements.EnhanceSearchPath;
  chkReplaceListWithMemo.Checked := IdeEnhancements.EnhanceSearchPathAggressive;
  chkEnhanceToolProperties.Checked := IdeEnhancements.EnhanceToolProperties;
  chkEnhanceInstallPackages.Checked := IdeEnhancements.EnhanceInstallPackages;
  UpdateIdeDialogCheckboxes;

  chkCPFontEnabled.Checked := IdeEnhancements.CPFontEnabled;
  FCPFont.Assign(IdeEnhancements.CPFont);

  // File saving
  chkAutoSave.Checked := IdeEnhancements.AutoSave;
  udMinutes.Position := IdeEnhancements.AutoSaveInterval;
  // Fonts
  chkOIFontEnabled.Checked := IdeEnhancements.OIFontEnabled;
  FOIFont.Assign(IdeEnhancements.OIFont);
  chkOIFontNames.Checked := IdeEnhancements.OICustomFontNames;

  chkFontEnabledClick(Self);
  chkAutoSaveClick(chkAutoSave);
  chkCPAsButtonsClick(chkCPAsButtons);
  chkCPTabsInPopupClick(chkCPTabsInPopup);
  chkCPMultiLineClick(chkCPMultiLine);
  chkMultiLineTabDockHostClick(chkMultiLineTabDockHost);
  chkCPMultiLineClick(chkCPMultiLine);

  chkDisableEDTEnhancementsClick(chkDisableEDTEnhancements);
end;

procedure TfmConfiguration.LoadEditorEnhancements;
begin
  Assert(EditorEnhancements <> nil);

  chkDisableEDTEnhancements.Checked := not EditorEnhancements.Enabled;

  Assert(EditorEnhancements.ToolbarActionsList <> nil);

  chkEditorToolBar.Checked := EditorEnhancements.ToolBarVisible;

  chkHotTrack.Checked := EditorEnhancements.HotTrack;
  chkMultiLine.Checked := EditorEnhancements.MultiLine;
  chkMiddleButtonClose.Checked := EditorEnhancements.MiddleButtonClose;
  chkButtons.Checked := EditorEnhancements.Buttons;
  chkEditTabButtonsFlat.Checked := EditorEnhancements.ButtonsFlat;

  chkHideNavbar.Checked := EditorEnhancements.HideNavbar;

  Assert(EditorEnhancements.ToolBarAlign in [alTop..alRight]);
  rgAlign.ItemIndex := Ord(EditorEnhancements.ToolBarAlign) - 1;

  chkDisableEDTEnhancementsClick(chkDisableEDTEnhancements);
end;

procedure TfmConfiguration.LoadSuppressedMessages;
var
  Section: string;
  Settings: TGExpertsSettings;
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Settings := TGExpertsSettings.Create;
    try
      Section := TGxMsgBoxAdaptor.ConfigurationKey;
      Settings.ReadSection(Section, sl);
      for i := sl.Count - 1 downto 0 do begin
        if not Settings.ReadBool(Section, sl[i], False) then
          sl.Delete(i);
      end;
      lbSuppressedMesages.Items.Assign(sl);
    finally
      Settings.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TfmConfiguration.SaveIdeEnhancements;
begin
  Assert(IdeEnhancements <> nil);

  // Multi-line tab dock host
  IdeEnhancements.MultiLineTabDockHost := chkMultiLineTabDockHost.Checked;
  IdeEnhancements.DefaultMultiLineTabDockHost := chkDefaultMultiLineTabDockHost.Checked;
  // Component palette
  IdeEnhancements.CPMultiLine := chkCPMultiLine.Checked;
  IdeEnhancements.CPScrollOpposite := chkCPScrollOpposite.Checked;
  IdeEnhancements.CPRaggedRight := chkCPRaggedRight.Checked;
  IdeEnhancements.CPAsButtons := chkCPAsButtons.Checked;
  IdeEnhancements.CPFlatButtons := chkCPFlat.Checked;
  IdeEnhancements.CPTabsInPopup := chkCPTabsInPopup.Checked;
  IdeEnhancements.CPTabsInPopupAlphaSort := chkCPTabsInPopupAlphaSort.Checked;

  IdeEnhancements.CPFontEnabled := chkCPFontEnabled.Checked;
  IdeEnhancements.OIFont.Assign(FOIFont);
  IdeEnhancements.CPFont.Assign(FCPFont);

  IdeEnhancements.EnhanceIDEForms := chkEnhanceDialogs.Checked;
  IdeEnhancements.IdeFormsAllowResize := chkAllowResize.Checked;
  IdeEnhancements.IdeFormsRememberPosition := chkRememberPosition.Checked;
  IdeEnhancements.EnhanceSearchPath := chkEnhanceSearchPaths.Checked;
  IdeEnhancements.EnhanceSearchPathAggressive := chkReplaceListWithMemo.Checked;
  IdeEnhancements.EnhanceInstallPackages := chkEnhanceInstallPackages.Checked;
  IdeEnhancements.EnhanceToolProperties := chkEnhanceToolProperties.Checked;

  // Menus
  ConfigInfo.PlaceGxMainMenuInToolsMenu := chkPlaceGxMainMenuInToolsMenu.Checked;
  GXMenuActionManager.Alphabetical := chkAlphabetizeMenu.Checked;
  GXMenuActionManager.HideWindowMenu := chkHideWindowMenu.Checked;
  GXMenuActionManager.MoveComponentMenu := chkMoveComponentMenu.Checked;

  // File saving
  IdeEnhancements.AutoSave := chkAutoSave.Checked;
  IdeEnhancements.AutoSaveInterval := udMinutes.Position;
  // Fonts
  IdeEnhancements.OIFontEnabled := chkOIFontEnabled.Checked;
  IdeEnhancements.OIFont.Assign(FOIFont);
  IdeEnhancements.OICustomFontNames := chkOIFontNames.Checked;

  IdeEnhancements.SaveSettings;
end;

procedure TfmConfiguration.SaveEditorEnhancements;
begin
  Assert(EditorEnhancements <> nil);

  Assert(EditorEnhancements.ToolbarActionsList <> nil);
  {$IFOPT D+} SendDebug('Clearing the toolbar actions'); {$ENDIF}

  {$IFOPT D+} SendDebug('Setting ToolBarVisible to ' + BooleanText(chkEditorToolBar.Checked)); {$ENDIF}
  EditorEnhancements.ToolBarVisible := chkEditorToolBar.Checked;
  {$IFOPT D+} SendDebug('Setting MultiLine Editor Tabs to ' + BooleanText(chkMultiLine.Checked)); {$ENDIF}
  EditorEnhancements.MultiLine := chkMultiLine.Checked;
  {$IFOPT D+} SendDebug('Setting Middle Button Close to ' + BooleanText(chkMiddleButtonClose.Checked)); {$ENDIF}
  EditorEnhancements.MiddleButtonClose := chkMiddleButtonClose.Checked;
  {$IFOPT D+} SendDebug('Setting HotTrack to ' + BooleanText(chkHotTrack.Checked)); {$ENDIF}
  EditorEnhancements.HotTrack := chkHotTrack.Checked;
  {$IFOPT D+} SendDebug('Setting Buttons to ' + BooleanText(chkButtons.Checked)); {$ENDIF}
  EditorEnhancements.Buttons := chkButtons.Checked;
  {$IFOPT D+} SendDebug('Setting ButtonsFlat to ' + BooleanText(chkEditTabButtonsFlat.Checked)); {$ENDIF}
  EditorEnhancements.ButtonsFlat := chkEditTabButtonsFlat.Checked;

  {$IFOPT D+} SendDebug('Setting ToolBarAlign to ' + IntToStr(rgAlign.ItemIndex)); {$ENDIF}
  Assert(rgAlign.ItemIndex >= 0);
  EditorEnhancements.ToolBarAlign := TAlign(rgAlign.ItemIndex + 1);

  {$IFOPT D+} SendDebug('Setting HideNabar to ' + BooleanText(chkHideNavbar.Checked)); {$ENDIF}
  EditorEnhancements.HideNavbar := chkHideNavbar.Checked;

  {$IFOPT D+} SendDebug('Setting EditorEnhancements.Enabled to ' + BooleanText(not chkDisableEDTEnhancements.Checked)); {$ENDIF}
  EditorEnhancements.Enabled := not chkDisableEDTEnhancements.Checked;

  {$IFOPT D+} SendDebug('Saving editor enhancements settings'); {$ENDIF}
  EditorEnhancements.SaveSettings;

  EditorEnhancements.ApplyToolbarSettings;
end;

procedure TfmConfiguration.chkDisableAllEditorExpertsClick(Sender: TObject);
var
  Enable: Boolean;
begin
  Enable := not chkDisableAllEditorExperts.Checked;
  meHelp.Lines.Clear;
  if not Enable then
  begin
    {$IFOPT D+} SendDebug('Freeing editor experts from the configuration dialog'); {$ENDIF}
    GExpertsInst.FreeEditorExperts;
  end
  else
  begin
    {$IFOPT D+} SendDebug('Loading editor experts from the configuration dialog'); {$ENDIF}
    GExpertsInst.LoadEditorExperts;
  end;
  lvEditorExperts.Enabled := Enable;
  ConfigInfo.EditorExpertsEnabled := Enable;
  btnShortCut.Enabled := Enable;
  btnConfigure.Enabled := Enable;
  if Enable then
    LoadEditorExperts
  else
    lvEditorExperts.Items.Clear;
  if Enable and (lvEditorExperts.Items.Count > 0) then
    lvEditorExperts.Items[0].Selected := True;
end;

procedure TfmConfiguration.chkDisableEDTEnhancementsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := not chkDisableEDTEnhancements.Checked;
  SetupGroupBox(gbxEditorTabs, EnableState);
  SetupGroupBox(gbxEditorToolBar, EnableState);
  chkEditorToolBarClick(chkEditorToolBar);
  chkButtonsClick(chkButtons);
end;

procedure TfmConfiguration.HideUnsupportedIdeItems;
begin
  if not MultilineTabDockHostPossible then begin
    gbxIDEMenu.Width := gbxTabDockHost.Left + gbxTabDockHost.Width - gbxIDEMenu.Left;
    gbxTabDockHost.Visible := False;
  end;

  if not ComponentPaletteAvailable then begin
    gbxCompPalette.Visible := False;
    // these are on the debug tab and normally not visible
    btnCPFont.Visible := False;
    chkCPFontEnabled.Visible := False;
  end;
end;

procedure TfmConfiguration.HideUnsupportedEditorItems;
begin
  tshEditor.TabVisible := EditorEnhancementsPossible;
  gbxEditorTabs.Visible := RunningDelphi7OrLess;
{$ifndef GX_VER300_up}
  chkHideNavbar.Visible := False;
{$endif}
end;

procedure TfmConfiguration.chkFontEnabledClick(Sender: TObject);
begin
  btnOIFont.Enabled := chkOIFontEnabled.Checked;
  btnCPFont.Enabled := chkCPFontEnabled.Checked;
end;

procedure TfmConfiguration.btnFontClick(Sender: TObject);
begin
  if Sender = btnOIFont then
  begin
    dlgFont.Font.Assign(FOIFont);
    if dlgFont.Execute then
      FOIFont.Assign(dlgFont.Font);
  end
  else if Sender = btnCPFont then
  begin
    dlgFont.Font.Assign(FCPFont);
    if dlgFont.Execute then
      FCPFont.Assign(dlgFont.Font);
  end;
end;

procedure TfmConfiguration.chkAutoSaveClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked;
  lblMinutes.Enabled := EnableState;
  lblEvery.Enabled := EnableState;
  edtMinutes.Enabled := EnableState;
  udMinutes.Enabled := EnableState;
end;

procedure TfmConfiguration.chkCPAsButtonsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled;
  chkCPFlat.Enabled := EnableState;
end;

procedure TfmConfiguration.chkCPTabsInPopupClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled;
  chkCPTabsInPopupAlphaSort.Enabled := EnableState;
end;

procedure TfmConfiguration.chkCPMultiLineClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled;

  chkCPScrollOpposite.Enabled := EnableState;
  chkCPRaggedRight.Enabled := EnableState;
  if not EnableState then
  begin
    chkCPScrollOpposite.Checked := False;
    chkCPRaggedRight.Checked := False;
  end;
end;

procedure TfmConfiguration.chkButtonsClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableEDTEnhancements.Checked;

  chkEditTabButtonsFlat.Enabled := EnableState;
end;

procedure TfmConfiguration.chkEditorToolBarClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled and
                 not chkDisableEDTEnhancements.Checked;

  rgAlign.Enabled := EnableState;
  btnConfigureToolBar.Enabled := EnableState;
end;

procedure TfmConfiguration.chkMultiLineTabDockHostClick(Sender: TObject);
var
  EnableState: Boolean;
begin
  EnableState := (Sender as TCheckBox).Checked and
                 (Sender as TCheckBox).Enabled;

  chkDefaultMultiLineTabDockHost.Enabled := EnableState;
end;

procedure TfmConfiguration.sbxExpertsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if pcConfig.ActivePage = tshExperts then begin
    sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position + FThumbSize;
    Handled := True;
  end;
end;

procedure TfmConfiguration.sbxExpertsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if pcConfig.ActivePage = tshExperts then begin
    sbxExperts.VertScrollBar.Position := sbxExperts.VertScrollBar.Position - FThumbSize;
    Handled := True;
  end;
end;

procedure TfmConfiguration.btnConfigureToolBarClick(Sender: TObject);
begin
  EditorEnhancements.ShowToolBarConfigurationDialog;
end;

procedure TfmConfiguration.btnEnumerateModulesClick(Sender: TObject);
begin
  GxOtaShowProjectModuleInformation;
end;

procedure TfmConfiguration.btnEumerateActionsClick(Sender: TObject);
begin
  GxOtaShowIDEActions;
end;

procedure TfmConfiguration.pcConfigChange(Sender: TObject);
begin
  // This forces the columns to size correctly with "Large Fonts"
  if pcConfig.ActivePage = tshEditorExperts then
  begin
    lvEditorExperts.Width := lvEditorExperts.Width + 1;
    lvEditorExperts.Width := lvEditorExperts.Width - 1;
  end;
  // Warn if the user has an old common controls DLL
  if pcConfig.ActivePage = tshEditor then
    ShowGxMessageBox(TShowOldComCtrlVersionMessage);
end;

procedure TfmConfiguration.lvEditorExpertsDblClick(Sender: TObject);
var
  EditorExpert: TEditorExpert;
  Idx: Integer;
begin
  if not TListView_TryGetSelected(lvEditorExperts, Idx) then
    Exit;

  Assert(Assigned(GExpertsInst.EditorExpertManager));

  EditorExpert := GExpertsInst.EditorExpertManager.EditorExpertList[Idx];
  if EditorExpert.HasConfigOptions then begin
    EditorExpert.Configure;
    EditorExpert.SaveSettings;
  end
  else
    ConfigureEditorExpertShortCut(EditorExpert);
end;

{ TShowOldComCtrlVersionMessage }

function TShowOldComCtrlVersionMessage.GetMessage: string;
resourcestring
  SOldComCtrlVersion = 'Your system has an old version of the Windows comctl32.dll.  ' +
    'The GExperts editor toolbar might not work correctly without upgrading to version 5 here: ' +
    'http://www.microsoft.com/msdownload/ieplatform/ie/comctrlx86.asp';
begin
  Result := SOldComCtrlVersion;
end;

function TShowOldComCtrlVersionMessage.ShouldShow: Boolean;
begin
  Result := GetComCtlVersion <= ComCtlVersionIE401;
end;

procedure TfmConfiguration.ConfigureEditorExpertShortCut(EditorExpert: TEditorExpert);
begin
  Assert(Assigned(EditorExpert));
  with TfmEditorShortcut.Create(nil) do
  begin
    try
      hkyShortCut.HotKey := EditorExpert.ShortCut;
      gbxShortCut.Caption := EditorExpert.DisplayName;

      if ShowModal = mrOk then
      begin
        EditorExpert.ShortCut := hkyShortCut.HotKey;
        EditorExpert.SaveSettings;

        with lvEditorExperts do
        begin
          Items[Selected.Index].Caption := EditorExpert.DisplayName;
          Items[Selected.Index].SubItems[0] := ShortCutToText(EditorExpert.ShortCut);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfmConfiguration.EditShortCutClick(Sender: TObject);
var
  AExpert: TGX_Expert;
begin
  AExpert := GExpertsInst.ExpertList[(Sender as TEdit).Tag];
  with TfmEditorShortcut.Create(nil) do
  begin
    try
      hkyShortCut.HotKey := TextToShortCut((Sender as TEdit).Text);
      gbxShortCut.Caption := AExpert.GetDisplayName;
      if ShowModal = mrOk then
      begin
        (Sender as TEdit).Text := ShortCutToText(hkyShortCut.HotKey);
        AExpert.ShortCut := hkyShortCut.HotKey;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfmConfiguration.edtFilterChange(Sender: TObject);
begin
  tmrFilter.Enabled := False;
  tmrFilter.Enabled := True;
end;

procedure TfmConfiguration.tmrFilterTimer(Sender: TObject);
begin
  FilterVisibleExperts;
  tmrFilter.Enabled := False;
end;

procedure TfmConfiguration.FilterVisibleExperts;
var
  Panel: TPanel;
  CheckBox: TCheckBox;
  i, CurrTop: Integer;
  SubText: string;
begin
  sbxExperts.VertScrollBar.Position := 0;
  SubText := Trim(edtFilter.Text);
  if SubText = '' then
    for i := 0 to sbxExperts.ControlCount - 1 do
    begin
      Panel := sbxExperts.Controls[i] as TPanel;
      if Panel <> pnlExpertLayout then
        Panel.Visible := True;
    end
  else
    for i := 0 to sbxExperts.ControlCount - 1 do
    begin
      Panel := sbxExperts.Controls[i] as TPanel;
      CheckBox := Panel.Controls[1] as TCheckBox;
      Panel.Visible := StrContains(SubText, CheckBox.Caption, False) and (Panel <> pnlExpertLayout);
    end;

  CurrTop := 0;
  for i := 0 to sbxExperts.ControlCount - 1 do
  begin
    Panel := sbxExperts.Controls[i] as TPanel;
    if Panel.Visible then
    begin
      Panel.Top := CurrTop;
      Inc(CurrTop, Panel.Height);
    end;
  end;
  sbxExperts.VertScrollBar.Range := CurrTop;
end;

procedure TfmConfiguration.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ActiveControl is THotKey then
    Exit;
  if (Key = Ord('D')) and (ssCtrl in Shift) then begin
    tshDebug.TabVisible := True;
    pcConfig.ActivePage := tshDebug;
  end;
end;

type TControlCracker = class(TControl);

procedure TfmConfiguration.btnGetFontsClick(Sender: TObject);
var
  Fonts: string;

  procedure AddControl(Control: TControl);
  begin
    if Assigned(Control) then
      Fonts := Fonts + Control.Name + ': ' + TControlCracker(Control).Font.Name + ' ' + IntToStr(TControlCracker(COntrol).Font.Size) + sLineBreak;
  end;

begin
  AddControl(GetIdeMainForm);
  AddControl(GetComponentPaletteTabControl);
  AddControl(GetObjectInspectorForm);
  ShowMessage(Fonts);
end;

procedure TfmConfiguration.btnAppBuilderClick(Sender: TObject);
begin
  OutputComponentList(GetIdeMainForm, False);
end;

procedure TfmConfiguration.btnEditViewClick(Sender: TObject);
begin
  GxOtaShowEditViewDetails;
end;

procedure TfmConfiguration.btnCustomFontClick(Sender: TObject);
begin
  dlgUIFont.Font.Assign(ConfigInfo.CustomFont);
  if dlgUIFont.Execute then
  begin
    chkUseCustomFont.Checked := True;
    Self.Font.Assign(dlgUIFont.Font);
  end;
end;

procedure TfmConfiguration.btnDeleteSuppressedMessageClick(Sender: TObject);
var
  s: string;
  Settings: TGExpertsSettings;
begin
  if not TListBox_GetSelected(lbSuppressedMesages, s) then
    Exit;
  Settings := TGExpertsSettings.Create;
  try
    Settings.WriteBool(TGxMsgBoxAdaptor.ConfigurationKey, s, False);
  finally
    Settings.Free;
  end;
  LoadSuppressedMessages;
end;

procedure TfmConfiguration.chkEnhanceDialogsClick(Sender: TObject);
begin
  UpdateIdeDialogCheckboxes;
end;

procedure TfmConfiguration.UpdateIdeDialogCheckboxes;
var
  EnableState: Boolean;
begin
  EnableState := chkEnhanceDialogs.Checked and
                 chkEnhanceDialogs.Enabled;
  chkAllowResize.Enabled := EnableState;
  chkRememberPosition.Enabled := EnableState;
  chkEnhanceSearchPaths.Enabled := EnableState;
  chkEnhanceToolProperties.Enabled := EnableState;
  chkReplaceListWithMemo.Enabled := EnableState;
  chkEnhanceInstallPackages.Enabled := EnableState;
end;

end.

