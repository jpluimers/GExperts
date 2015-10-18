unit GX_ClipboardHistory;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Menus,
  ComCtrls, ActnList, ToolWin,
  GX_Experts, GX_ConfigurationInfo, GX_IdeDock;

type
  TClipInfo = class(TObject)
  private
    FClipTimeStamp: string;
    FClipString: string;
  public
    property ClipTimeStamp: string read FClipTimeStamp write FClipTimeStamp;
    property ClipString: string read FClipString write FClipString;
  end;

  TfmClipboardHistory = class(TfmIdeDockForm)
    Splitter: TSplitter;
    mmoClipText: TMemo;
    MainMenu: TMainMenu;
    mitFile: TMenuItem;
    mitFileExit: TMenuItem;
    mitEdit: TMenuItem;
    mitEditCopy: TMenuItem;
    mitHelp: TMenuItem;
    mitHelpContents: TMenuItem;
    mitHelpAbout: TMenuItem;
    mitHelpHelp: TMenuItem;
    mitHelpSep1: TMenuItem;
    mitEditClear: TMenuItem;
    lvClip: TListView;
    ToolBar: TToolBar;
    tbnClear: TToolButton;
    tbnCopy: TToolButton;
    tbnHelp: TToolButton;
    tbnSep2: TToolButton;
    Actions: TActionList;
    actFileExit: TAction;
    actEditCopy: TAction;
    actEditClear: TAction;
    actHelpHelp: TAction;
    actHelpContents: TAction;
    actHelpAbout: TAction;
    actEditPasteToIde: TAction;
    tbnPaste: TToolButton;
    mitEditPasteToIde: TMenuItem;
    mitView: TMenuItem;
    actViewToolBar: TAction;
    mitViewToolBar: TMenuItem;
    tbnSep3: TToolButton;
    btnOptions: TToolButton;
    actViewOptions: TAction;
    mitViewOptions: TMenuItem;
    actRehookClipboard: TAction;
    mitFileRehookClipboard: TMenuItem;
    tbnDelete: TToolButton;
    actDelete: TAction;
    mitEditDelete: TMenuItem;
    tbnSep1: TToolButton;
    mitEditSep1: TMenuItem;
    pmListMenu: TPopupMenu;
    mitListCopy: TMenuItem;
    mitListPasteIntoIDE: TMenuItem;
    mitListDelete: TMenuItem;
    mitListSep2: TMenuItem;
    actEditPasteAsPascalString: TAction;
    mitEditPasteAsPascalString: TMenuItem;
    mitListPasteAsPascalString: TMenuItem;
    tbnPasteAsPascal: TToolButton;
    pnlPasteAsOptions: TPanel;
    actViewPasteAsOptions: TAction;
    tbnViewPasteAs: TToolButton;
    tbnSep4: TToolButton;
    ShowPasteAsoptions1: TMenuItem;
    lblMaxEntries: TLabel;
    cbPasteAsType: TComboBox;
    chkCreateQuotedStrings: TCheckBox;
    chkAddExtraSpaceAtTheEnd: TCheckBox;
    actEditCopyFromPascalString: TAction;
    actEditReplaceAsPascalString: TAction;
    mitEditCopyfromPascalstring: TMenuItem;
    mitEditReplaceasPascalstring: TMenuItem;
    mitListSep1: TMenuItem;
    mitListCopyfromPascalstring: TMenuItem;
    mitReplaceasPascalstring: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvClipDblClick(Sender: TObject);
    procedure lvClipChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditClearExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpHelpExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure lvClipKeyPress(Sender: TObject; var Key: Char);
    procedure actEditPasteToIdeExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actViewToolBarExecute(Sender: TObject);
    procedure actViewOptionsExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actRehookClipboardExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEditPasteAsPascalStringExecute(Sender: TObject);
    procedure actViewPasteAsOptionsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FHelperWindow: TWinControl;
    IgnoreClip: Boolean;
    FDataList: TList;
    FLoading: Boolean;
    SplitterRatio: Double;
    procedure ClearDataList;
    procedure LoadClips;
    procedure SaveClips;
    function ConfigurationKey: string;
    procedure HookClipboard;
    function ClipInfoForItem(Item: TListItem): TClipInfo;
    function ClipInfoFromPointer(Ptr: Pointer): TClipInfo;
    function HaveSelectedItem: Boolean;
    procedure RemoveDataListItem(Index: Integer);
    function GetSelectedItemsText: string;
    procedure WmDrawClipBoard;
    procedure AddClipItem(const AClipText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveSettings;
    procedure LoadSettings;
  end;

  TClipExpert = class(TGX_Expert)
  private
    FMaxClip: Integer;
    FAutoStart: Boolean;
    FAutoClose: Boolean;
    FStoragePath: string;
    function GetStorageFile: string;
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    property MaxClip: Integer read FMaxClip write FMaxClip;
    property StorageFile: string read GetStorageFile;
  end;

var
  fmClipboardHistory: TfmClipboardHistory = nil;
  ClipExpert: TClipExpert = nil;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows, Messages, SysUtils, Clipbrd, Dialogs, Math, StrUtils, OmniXML,
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_dzVclUtils,
  GX_GExperts, GX_ClipboardOptions, GX_SharedImages, GX_XmlUtils,
  GX_PasteAs;

const
  ClipStorageFileName = 'ClipboardHistory.xml';

type
  THelperWinControl = class(TWinControl)
  private
    FPrevWindow: HWnd;
    procedure WMChangeCBChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WMDrawClipBoard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TClipData = record
    FirstLine: String;
    Count: Integer;
  end;

function FirstLineOfText(const AClipString: string): TClipData;
begin
  Result.FirstLine := '';
  with TStringList.Create do
  try
    Text := AClipString;
    Result.Count := Count;
    if Result.Count > 0 then
      Result.FirstLine := Strings[0];
  finally
    Free;
  end;
end;


{ THelperWinControl }

constructor THelperWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'ClipboardChainHelperWindow';
  // The clipboard chaining only works properly if this window is
  // not parented by the the clip form.  The desktop window may not
  // be the best window to hook but it works.
  ParentWindow := GetDesktopWindow;
  Visible := False;
  {$IFOPT D+} SendDebug('In THelperWinControl Create'); {$ENDIF}
  FPrevWindow := SetClipBoardViewer(Self.Handle);
  {$IFOPT D+} SendDebug('FPrevWindow = ' + IntToStr(FPrevWindow)); {$ENDIF}
end;

destructor THelperWinControl.Destroy;
begin
  //{$IFOPT D+} SendDebug('In THelperWinControl Destroy'); {$ENDIF}
  try
    ChangeClipBoardChain(Self.Handle, FPrevWindow);
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugError('Clip Chain Destroy: ' + E.Message); {$ENDIF}
    end;
  end;
  inherited Destroy;
end;

procedure THelperWinControl.WMChangeCBChain(var Msg: TMessage);
begin
  {$IFOPT D+} SendDebug('In THelperWinControl WMChangeCBChain'); {$ENDIF}
  if Msg.WParam = WPARAM(FPrevWindow) then
    FPrevWindow := Msg.lParam
  else if (FPrevWindow <> 0) then
    SendMessage(FPrevWindow, WM_CHANGECBCHAIN, Msg.WParam, Msg.LParam);
  //Msg.Result := 0; //??
end;

procedure THelperWinControl.WMDrawClipBoard(var Msg: TMessage);
begin
  try
    {$IFOPT D+} SendDebug('In THelperWinControl WMDrawClipBoard'); {$ENDIF}
    if not Assigned(fmClipboardHistory) then
      Exit;
    fmClipboardHistory.WmDrawClipBoard;
  finally
    if FPrevWindow <> 0 then
      SendMessage(FPrevWindow, WM_DRAWCLIPBOARD, Msg.WParam, Msg.LParam);
  end;
end;

{ TfmClipboardHistory }

procedure TfmClipboardHistory.FormCreate(Sender: TObject);
begin
  inherited;
  PasteAsHandler.GetTypeText(cbPasteAsType.Items);
  cbPasteAsType.DropDownCount := Integer(High(TPasteAsType)) + 1;
  cbPasteAsType.ItemIndex := Integer(PasteAsHandler.PasteAsType);
  chkCreateQuotedStrings.Checked := PasteAsHandler.CreateQuotedString;
  chkAddExtraSpaceAtTheEnd.Checked := PasteAsHandler.AddExtraSpaceAtTheEnd;
end;

procedure TfmClipboardHistory.FormResize(Sender: TObject);
begin
  mmoClipText.Height := Trunc(SplitterRatio * (mmoClipText.Height + lvClip.Height));
end;

procedure TfmClipboardHistory.SplitterMoved(Sender: TObject);
begin
  SplitterRatio := mmoClipText.Height / (lvClip.Height + mmoClipText.Height);
  FormResize(Self);
end;

procedure TfmClipboardHistory.ClearDataList;
var
  i: Integer;
begin
  if Assigned(FDataList) then
  begin
    for i := 0 to FDataList.Count - 1 do
      ClipInfoFromPointer(FDataList.Items[i]).Free;
    FDataList.Clear;
  end;
  lvClip.Items.Clear;
end;

procedure TfmClipboardHistory.Clear;
begin
  ClearDataList;
  mmoClipText.Lines.Clear;
end;

procedure TfmClipboardHistory.SaveSettings;
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
    Settings.WriteInteger(ConfigurationKey, 'SplitterRatio', Round(SplitterRatio * 100));
    Settings.WriteBool(ConfigurationKey, 'ViewToolBar', ToolBar.Visible);
    Settings.WriteBool(ConfigurationKey, 'PasteAsOptions', pnlPasteAsOptions.Visible);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmClipboardHistory.LoadSettings;
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
    SplitterRatio := Settings.ReadInteger(ConfigurationKey, 'SplitterRatio', 50) / 100;
    mmoClipText.Height :=  Trunc(SplitterRatio * (mmoClipText.Height + lvClip.Height));
    ToolBar.Visible := Settings.ReadBool(ConfigurationKey, 'ViewToolBar', True);
    pnlPasteAsOptions.Visible := Settings.ReadBool(ConfigurationKey, 'PasteAsOptions', True);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmClipboardHistory.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfmClipboardHistory.lvClipDblClick(Sender: TObject);
begin
  actEditCopy.Execute;
end;

procedure TfmClipboardHistory.LoadClips;
var
  Doc: IXmlDocument;
  Nodes: IXMLNodeList;
  i: Integer;
  TimeStr: string;
  ClipStr: string;
  Info: TClipInfo;
  ClipItem: TListItem;
  TimeNode: IXMLNode;
  ClipData: TClipData;
begin
  ClearDataList;
  Doc := CreateXMLDoc;
  if FileExists(ClipExpert.StorageFile) then begin
    Doc.Load(ClipExpert.StorageFile);
    if not Assigned(Doc.DocumentElement) then
      Exit;
    Nodes := Doc.DocumentElement.selectNodes('ClipItem');
    lvClip.Items.BeginUpdate;
    try
      FLoading := True;
      for i := 0 to Nodes.Length - 1 do
      begin
        if i >= ClipExpert.MaxClip then
          Break;
        TimeNode := Nodes.Item[i].Attributes.GetNamedItem('DateTime');
        if Assigned(TimeNode) then
          TimeStr := TimeNode.NodeValue
        else
          TimeStr := TimeToStr(Time);
        ClipStr := GX_XmlUtils.GetCDataSectionTextOrNodeText(Nodes.Item[i]);

        Info := TClipInfo.Create;
        FDataList.Add(Info);
        Info.ClipString := ClipStr;
        Info.ClipTimeStamp := TimeStr;

        ClipItem := lvClip.Items.Add;
        ClipItem.Caption := Info.ClipTimeStamp;
        ClipData := FirstLineOfText(ClipStr);
        ClipItem.SubItems.Add(IntToStr(ClipData.Count));
        ClipItem.SubItems.Add(Trim(ClipData.FirstLine));
        ClipItem.Data := Info;
      end;
    finally
      lvClip.Items.EndUpdate;
      FLoading := False;
    end;
    if lvClip.Items.Count > 0 then
    begin
      lvClip.Selected := lvClip.Items[0];
      lvClip.ItemFocused := lvClip.Selected;
    end;
  end;
end;

procedure TfmClipboardHistory.SaveClips;
var
  Doc: IXmlDocument;
  Root: IXMLElement;
  i: Integer;
  ClipItem: IXMLElement;
  ClipText: IXMLCDATASection;
begin
  // We are calling SaveClips from the destructor where
  // we may be in a forced clean-up due to an exception.
  if ExceptObject <> nil then
    Exit;

  Doc := CreateXMLDoc;
  AddXMLHeader(Doc);
  Root := Doc.CreateElement('Clips');
  Doc.AppendChild(Root);
  for i := 0 to FDataList.Count - 1 do
  begin
    ClipItem := Doc.CreateElement('ClipItem');
    ClipItem.SetAttribute('DateTime', ClipInfoFromPointer(FDataList[i]).ClipTimeStamp);
    ClipText := Doc.CreateCDATASection(EscapeCDataText(ClipInfoFromPointer(FDataList[i]).ClipString));
    ClipItem.AppendChild(ClipText);
    Root.AppendChild(ClipItem);
  end;
  if PrepareDirectoryForWriting(ExtractFileDir(ClipExpert.StorageFile)) then
    Doc.Save(ClipExpert.StorageFile, ofFlat);
end;

procedure TfmClipboardHistory.lvClipChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if FLoading or (csDestroying in ComponentState) then
    Exit;
  if lvClip.Selected <> nil then
    mmoClipText.Lines.Text := GetSelectedItemsText
  else
    mmoClipText.Clear;
end;

constructor TfmClipboardHistory.Create(AOwner: TComponent);
resourcestring
  SLoadingFailed = 'Loading of stored clipboard clips failed.' + sLineBreak;
begin
  inherited;

  SetToolbarGradient(ToolBar);
  {$IFOPT D+} SendDebug('Creating clipboard history data list'); {$ENDIF}
  FDataList := TList.Create;
  SplitterRatio := 0.50;
  LoadSettings;

  CenterForm(Self);
  IgnoreClip := False;

  // With large fonts, the TMenuToolBar ends up below the ToolBar, this fixes it
  ToolBar.Align := alNone;
  ToolBar.Top := 200;
  ToolBar.Align := alTop;

  pnlPasteAsOptions.Top := ToolBar.Top + ToolBar.Height;

  HookClipboard;

  // Now load any saved clips from our XML storage.
  // Since we do not depend on these snippets, continue
  // even in the presence of an exception.
  try
    LoadClips;
  except
    on E: Exception do
    begin
      GxLogAndShowException(E, SLoadingFailed);
      // Swallow exceptions
    end;
  end;
end;

destructor TfmClipboardHistory.Destroy;
begin
  SaveClips;

  // Now free everything.
  ClearDataList;
  FreeAndNil(FDataList);
  FreeAndNil(FHelperWindow);
  SaveSettings;

  inherited Destroy;
  fmClipboardHistory := nil;
end;

procedure TfmClipboardHistory.AddClipItem(const AClipText: string);
var
  Info: TClipInfo;
  ClipItem: TListItem;
  ClipData: TClipData;
begin
  Info := TClipInfo.Create;
  FDataList.Insert(0, Info);
  Info.ClipString := AClipText;
  Info.ClipTimeStamp := TimeToStr(Time);

  ClipItem := lvClip.Items.Insert(0);
  ClipItem.Caption := Info.ClipTimeStamp;
  ClipData := FirstLineOfText(Info.ClipString);
  ClipItem.SubItems.Add(IntToStr(ClipData.Count));
  ClipItem.SubItems.Add(Trim(ClipData.FirstLine));
  ClipItem.Data := Info;
end;

procedure TfmClipboardHistory.actEditCopyExecute(Sender: TObject);
var
  idx: Integer;
  Buffer: string;
  AsPascalString: Boolean;

  function GetCopyText(AText: String): String;
  var
    AList: TStringList;
    ALine: String;
    APasteAsHandler: TPasteAsHandler;
  begin
    Result := AText;
    if AsPascalString then
    begin
      AList := TStringList.Create;
      try
        AList.Text := AText;

        if AList.Count = 1 then
        begin
          ALine := AList[0];
          ALine := AnsiReplaceText(ALine, '#$D#$A', #$D#$A);
          ALine := AnsiReplaceText(ALine, '#13#10', #13#10);
          AList.Text := ALine;
        end;

        if actViewPasteAsOptions.Checked then
        begin
          APasteAsHandler := TPasteAsHandler.Create;
          try
            APasteAsHandler.PasteAsType := TPasteAsType(cbPasteAsType.ItemIndex);
            APasteAsHandler.CreateQuotedString := chkCreateQuotedStrings.Checked;
            APasteAsHandler.AddExtraSpaceAtTheEnd := chkAddExtraSpaceAtTheEnd.Checked;

            Result := APasteAsHandler.ConvertFromString(AList, False);
          finally
            APasteAsHandler.Free;
          end;
        end
        else
          Result := PasteAsHandler.ConvertFromString(AList, False);
      finally
        AList.Free;
      end;
    end;
  end;

begin
  try
    AsPascalString := Sender = actEditCopyFromPascalString;

    if mmoClipText.SelLength = 0 then
    begin
      if lvClip.SelCount = 1 then
      begin
        IgnoreClip := True;
        try
          idx := lvClip.Selected.Index;
          Buffer := GetCopyText(mmoClipText.Text);
          Clipboard.AsText := Buffer;

          lvClip.Items.Delete(idx);
          ClipInfoFromPointer(FDataList[idx]).Free;
          FDataList.Delete(idx);

          AddClipItem(Buffer);

          lvClip.Selected := lvClip.Items[0];
          lvClip.ItemFocused := lvClip.Selected;
        finally
          IgnoreClip := False;
        end;
      end
      else
        Clipboard.AsText := GetCopyText(GetSelectedItemsText);
    end
    else
//      mmoClipText.CopyToClipBoard;
      Clipboard.AsText := GetCopyText(mmoClipText.Text);

    if ClipExpert.FAutoClose then
      Self.Close;
  finally
    Application.ProcessMessages;
  end;
end;

procedure TfmClipboardHistory.actEditClearExecute(Sender: TObject);
resourcestring
  SConfirmClearClipHistory = 'Clear the clipboard history?';
begin
  if MessageDlg(SConfirmClearClipHistory, mtConfirmation, [mbOK, mbCancel], 0) = mrOk then
    Self.Clear;
end;

procedure TfmClipboardHistory.actFileExitExecute(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfmClipboardHistory.actHelpHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 13);
end;

procedure TfmClipboardHistory.actHelpContentsExecute(Sender: TObject);
begin
  GxContextHelpContents(Self);
end;

procedure TfmClipboardHistory.actHelpAboutExecute(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmClipboardHistory.lvClipKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;

  if Key = #13 then
    actEditCopy.Execute;
end;

procedure TfmClipboardHistory.actEditPasteToIdeExecute(Sender: TObject);
begin
  if mmoClipText.SelLength = 0 then
    GxOtaInsertTextIntoEditor(mmoClipText.Text)
  else
    GxOtaInsertTextIntoEditor(mmoClipText.SelText);
end;

procedure TfmClipboardHistory.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actEditCopy.Enabled := (mmoClipText.SelLength > 0) or HaveSelectedItem;
  actEditPasteToIde.Enabled := actEditCopy.Enabled;
  actDelete.Enabled := HaveSelectedItem;
  actViewToolBar.Checked := ToolBar.Visible;
  actViewPasteAsOptions.Checked := pnlPasteAsOptions.Visible;
end;

procedure TfmClipboardHistory.actViewToolBarExecute(Sender: TObject);
begin
  ToolBar.Visible := not ToolBar.Visible;
end;

procedure TfmClipboardHistory.actViewOptionsExecute(Sender: TObject);
begin
  ClipExpert.Configure;
end;

procedure TfmClipboardHistory.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

function TfmClipboardHistory.ConfigurationKey: string;
begin
  Result := TClipExpert.ConfigurationKey + PathDelim + 'Window';
end;

procedure TfmClipboardHistory.HookClipboard;
begin
  FreeAndNil(FHelperWindow);
  {$IFOPT D+} SendDebug('Creating clipboard history THelperWinControl'); {$ENDIF}
  // The helper window is parented by the Desktop Window and
  // it chains the clipboard for us.
  FHelperWindow := THelperWinControl.Create(nil);
  {$IFOPT D+} SendDebug('Clipboard history helper window created'); {$ENDIF}
end;

procedure TfmClipboardHistory.actRehookClipboardExecute(Sender: TObject);
begin
  IgnoreClip := True;
  try
    HookClipboard;
  finally
    IgnoreClip := False;
  end;
end;

function TfmClipboardHistory.ClipInfoForItem(Item: TListItem): TClipInfo;
begin
  Assert(Assigned(Item));
  Assert(Assigned(Item.Data));
  Result := ClipInfoFromPointer(Item.Data);
end;

function TfmClipboardHistory.ClipInfoFromPointer(Ptr: Pointer): TClipInfo;
begin
  Assert(Assigned(Ptr));
  Result := TObject(Ptr) as TClipInfo;
end;

function TfmClipboardHistory.HaveSelectedItem: Boolean;
begin
  Result := Assigned(lvClip.Selected);
end;

procedure TfmClipboardHistory.RemoveDataListItem(Index: Integer);
var
  ClipInfo: TClipInfo;
begin
  Assert(Assigned(FDataList));
  Assert(Index < FDataList.Count);
  ClipInfo := ClipInfoFromPointer(FDataList.Items[Index]);
  FreeAndNil(ClipInfo);
  FDataList.Delete(Index);
end;

procedure TfmClipboardHistory.actDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  if not HaveSelectedItem then
    Exit;
  for i := lvClip.Items.Count - 1 downto 0 do
  begin
    if lvClip.Items[i].Selected then
    begin
      lvClip.Items.Delete(i);
      RemoveDataListItem(i);
    end;
  end;
  mmoClipText.Clear;
end;

function TfmClipboardHistory.GetSelectedItemsText: string;
var
  i: Integer;
  ClipItem: TListItem;
begin
  Result := '';
  for i := lvClip.Items.Count - 1 downto 0 do
  begin
    ClipItem := lvClip.Items[i];
    if ClipItem.Selected then
    begin
      if NotEmpty(Result) and (not HasTrailingEOL(Result)) then
        Result := Result + sLineBreak;
      Result := Result + ClipInfoForItem(ClipItem).ClipString;
    end;
  end;
end;

procedure TfmClipboardHistory.actViewPasteAsOptionsExecute(Sender: TObject);
begin
  pnlPasteAsOptions.Visible := not pnlPasteAsOptions.Visible;
  if pnlPasteAsOptions.Visible then
    pnlPasteAsOptions.Top := ToolBar.Top + ToolBar.Height;
end;

procedure TfmClipboardHistory.actEditPasteAsPascalStringExecute(Sender: TObject);
var
  AFromList: TStringList;
  APasteAsHandler: TPasteAsHandler;
  IsReplace: Boolean;
begin
  IsReplace := Sender = actEditReplaceAsPascalString;

  AFromList := TStringList.Create;
  try
    if mmoClipText.SelLength = 0 then
      AFromList.Text := mmoClipText.Text
    else
      AFromList.Text := mmoClipText.SelText;

    if actViewPasteAsOptions.Checked then
    begin
      APasteAsHandler := TPasteAsHandler.Create;
      try
        APasteAsHandler.PasteAsType := TPasteAsType(cbPasteAsType.ItemIndex);
        APasteAsHandler.CreateQuotedString := chkCreateQuotedStrings.Checked;
        APasteAsHandler.AddExtraSpaceAtTheEnd := chkAddExtraSpaceAtTheEnd.Checked;

        if IsReplace then
          APasteAsHandler.ConvertFromString(AFromList, True);

        APasteAsHandler.ConvertToString(AFromList, False)
      finally
        APasteAsHandler.Free;
      end;
    end
    else
    begin
      if IsReplace then
        PasteAsHandler.ConvertFromString(AFromList, True);

      PasteAsHandler.ConvertToString(AFromList, False);
    end;
  finally
    AFromList.Free;
  end;
end;

procedure TfmClipboardHistory.WmDrawClipBoard;
var
  ItemCount: Integer;
  ClipText: string;
  Handle: Cardinal;
  DataSize: Cardinal;
begin
  if IgnoreClip then
    Exit;
  try
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      Clipboard.Open;
      try
        Handle := Clipboard.GetAsHandle(CF_TEXT);
        DataSize := GlobalSize(Handle);  // This function might over-estimate by a few bytes
      finally
        Clipboard.Close;
      end;
      // Don't try to save clipboard items over 512 KB for speed reasons
      if DataSize > ((1024 * 512) + 32) then
        Exit;

      ClipText := Clipboard.AsText;
      if (FDataList.Count = 0) or
         (TClipInfo(FDataList[0]).ClipString <> clipText) then begin
        {$IFOPT D+} SendDebug('New clipboard text detected'); {$ENDIF}
        mmoClipText.Text := ClipText;

        AddClipItem(ClipText);

        ItemCount := lvClip.Items.Count;
        if ItemCount > ClipExpert.MaxClip then
        begin
          Dec(ItemCount);
          lvClip.Items.Delete(ItemCount);
          TClipInfo(FDataList[ItemCount]).Free;
          FDataList.Delete(ItemCount);
        end;
        lvClip.Selected := nil;
        lvClip.Selected := lvClip.Items[0];
        lvClip.ItemFocused := lvClip.Selected;

        TListView_Resize(lvClip);
      end;
    end;
  except
    on E: Exception do
    begin
      // Ignore exceptions
    end;
  end;
end;

{ TClipExpert }

constructor TClipExpert.Create;
begin
  inherited Create;
  FStoragePath := ConfigInfo.ConfigPath;

  FMaxClip := 20;

  FreeAndNil(ClipExpert);
  ClipExpert := Self;
end;

destructor TClipExpert.Destroy;
begin
  FreeAndNil(fmClipboardHistory);
  ClipExpert := nil;

  inherited Destroy;
end;

function TClipExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Clipboard &History';
begin
  Result := SMenuCaption;
end;

class function TClipExpert.GetName: string;
begin
  Result := 'ClipboardHistory';
end;

procedure TClipExpert.Click(Sender: TObject);
begin
  // If the form doesn't exist, create it.
  if fmClipboardHistory = nil then
  begin
    fmClipboardHistory := TfmClipboardHistory.Create(nil);
    SetFormIcon(fmClipboardHistory);
  end;
  IdeDockManager.ShowForm(fmClipboardHistory);
  fmClipboardHistory.lvClip.SetFocus;
end;

procedure TClipExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize.
  FMaxClip := Min(Settings.ReadInteger(ConfigurationKey, 'Maximum', 20), 100);
  FAutoStart := Settings.ReadBool(ConfigurationKey, 'AutoStart', False);
  FAutoClose := Settings.ReadBool(ConfigurationKey, 'AutoClose', False);

  // This procedure is only called once, so it is safe to
  // register the form for docking here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmClipboardHistory, fmClipboardHistory, 'fmClipboardHistory');

  if FAutoStart and (fmClipboardHistory = nil) then
    fmClipboardHistory := TfmClipboardHistory.Create(nil);
end;

procedure TClipExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize.
  Settings.WriteInteger(ConfigurationKey, 'Maximum', FMaxClip);
  Settings.WriteBool(ConfigurationKey, 'AutoStart', FAutoStart);
  Settings.WriteBool(ConfigurationKey, 'AutoClose', FAutoClose);
end;

procedure TClipExpert.Configure;
var
  Dlg: TfmClipboardOptions;
begin
  Dlg := TfmClipboardOptions.Create(nil);
  try
    Dlg.edtMaxClip.Text := IntToStr(FMaxClip);
    Dlg.chkAutoStart.Checked := FAutoStart;
    Dlg.chkAutoClose.Checked := FAutoClose;
    if Dlg.ShowModal = mrOk then
    begin
      FAutoStart := Dlg.chkAutoStart.Checked;
      FAutoClose := Dlg.chkAutoClose.Checked;
      FMaxClip := Min(StrToIntDef(Dlg.edtMaxClip.Text, 20), 1000);
      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TClipExpert.GetStorageFile: string;
begin
  Result := FStoragePath + ClipStorageFileName;
end;

procedure TClipExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
      FreeAndNil(fmClipboardHistory);
  end;
end;

initialization
  RegisterGX_Expert(TClipExpert);
end.

