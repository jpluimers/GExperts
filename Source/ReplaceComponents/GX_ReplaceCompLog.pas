// Original Author: Piotr Likus
// Replace Components log window
unit GX_ReplaceCompLog;

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  GX_ReplaceCompData, GX_BaseForm;

type
  TfmReplaceCompLog = class(TfmBaseForm)
    pnlHeader: TPanel;
    lblDest: TLabel;
    edtDestClassName: TEdit;
    edtSourceClassName: TEdit;
    lblSource: TLabel;
    pnlLog: TPanel;
    pnlBottom: TPanel;
    pnlDetails: TPanel;
    lbxPreviewItems: TListBox;
    lvLogItems: TListView;
    Splitter: TSplitter;
    dlgGetExportFile: TSaveDialog;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnSaveAs: TButton;
    btnCopy: TButton;
    btnClose: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lvLogItemsClick(Sender: TObject);
    procedure lvLogItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FConfigData: TReplaceCompData;
    FLogEvents: TCompRepEventList;
    FFocusedItem: TListItem; 
    function ConfigurationKey: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadAll;
    procedure AddLogEvent(AEvent: TCompRepEvent);
    procedure LoadItems;
    procedure UpdatePreviewPanel;
    function GetFocusedItem: TListItem;
    function GetFocusedEvent: TCompRepEvent;
    procedure LoadPreviewItems(AEvent: TCompRepEvent);
    procedure SaveToClipboard;
    procedure GetSelectedItems(Items: TList);
    procedure FormatEventsAsTabs(EventList: TList; OutList: TStringList);
    procedure FormatEventsAsCSV(EventList: TList; OutList: TStringList);
    procedure SaveAsCSV(const AFileName: string);
    procedure SaveAsXML(const AFileName: string);
  public
    constructor Create(Owner: TComponent; ConfigData: TReplaceCompData;
      const SourceClassName, DestClassName: string; LogEvents: TCompRepEventList); reintroduce;
  end;

implementation

uses
  SysUtils, Clipbrd, Gx_GenericUtils, GX_ReplaceCompUtils, GX_ConfigurationInfo,
  GX_dzVclUtils;
  
{$R *.dfm}

{ TfmReplaceCompLog }

constructor TfmReplaceCompLog.Create(Owner: TComponent;
  ConfigData: TReplaceCompData;
  const SourceClassName, DestClassName: string; LogEvents: TCompRepEventList);
begin
  inherited Create(Owner);

  TControl_SetMinConstraints(Self);
  FConfigData := ConfigData;
  FLogEvents := LogEvents;
  edtSourceClassName.Text := SourceClassName;
  edtDestClassName.Text := DestClassName;
  LoadSettings;
end;

function TfmReplaceCompLog.ConfigurationKey: string;
begin
  Result := FConfigData.RootConfigurationKey + PathDelim + Self.ClassName+'.Window';
end;

procedure TfmReplaceCompLog.LoadSettings;
var
  Settings: TGExpertsSettings;
  i: Integer;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    Settings.LoadForm(Self, ConfigurationKey + '\Window');
    pnlBottom.Height := Settings.ReadInteger(ConfigurationKey + '\Window', 'ListSplitter',
      pnlBottom.Height);

    for i := 0 to lvLogItems.Columns.Count-1 do
      lvLogItems.Columns[i].Width :=
        Settings.ReadInteger(ConfigurationKey + '\Window', 'Col'+IntToStr(i)+'.Width',
          lvLogItems.Columns[i].Width);
  finally
    FreeAndNil(Settings);
  end;
  EnsureFormVisible(Self);
end;

procedure TfmReplaceCompLog.SaveSettings;
var
  Settings: TGExpertsSettings;
  i: Integer;
begin
  // Do not localize.
  Settings := TGExpertsSettings.Create;
  try
    if not (WindowState in [wsMinimized, wsMaximized]) then
    begin
      Settings.SaveForm(Self, ConfigurationKey + '\Window');
      Settings.WriteInteger(ConfigurationKey + '\Window', 'ListSplitter', pnlBottom.Height);
    end;

    for i := 0 to lvLogItems.Columns.Count-1 do
      Settings.WriteInteger(ConfigurationKey + '\Window', 'Col'+IntToStr(i)+'.Width',
        lvLogItems.Columns[i].Width);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TfmReplaceCompLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TfmReplaceCompLog.FormShow(Sender: TObject);
begin
  LoadAll;
end;

procedure TfmReplaceCompLog.LoadAll;
begin
  LoadItems;

  if lvLogItems.Items.Count>0 then
    lvLogItems.Selected := lvLogItems.Items[0];
end;

procedure TfmReplaceCompLog.LoadItems;
var
  i: Integer;
begin
  lvLogItems.Items.Clear;
  UpdatePreviewPanel;
  lvLogItems.Items.BeginUpdate;
  try
    for i := 0 to FLogEvents.Count-1 do
      AddLogEvent(FLogEvents[i]);
  finally
    lvLogItems.Items.EndUpdate;
  end;
end;

procedure TfmReplaceCompLog.AddLogEvent(AEvent: TCompRepEvent);
var
  ListItem: TListItem;
begin
  ListItem := lvLogItems.Items.Add;
  ListItem.Caption := DateTimeToStr(AEvent.When);
  if AEvent.IsError then
    ListItem.SubItems.Add('!')
  else
    ListItem.SubItems.Add('');
  ListItem.SubItems.Add(AEvent.ObjectText);
  ListItem.SubItems.Add(AEvent.FlatText);
  ListItem.Data := AEvent;
end;

procedure TfmReplaceCompLog.lvLogItemsClick(Sender: TObject);
begin
  UpdatePreviewPanel;
end;

procedure TfmReplaceCompLog.UpdatePreviewPanel;
var
  Event: TCompRepEvent;
begin
  Event := GetFocusedEvent;
  if Event = nil then
  begin
    lbxPreviewItems.Color := clBtnFace;
    Exit;
  end
  else
  begin
    lbxPreviewItems.Color := clWindow;
    LoadPreviewItems(Event);
  end;
end;

procedure TfmReplaceCompLog.LoadPreviewItems(AEvent: TCompRepEvent);
resourcestring
  SLayout =
    'Event: %EventType%'+#10+
    'Message:'+#10+
    '%Text%'+#10+
    'FileName: %FileName%'+#10+
    'Object: %ObjectClass%, %ObjectSearchName%; Component: %ComponentName%'+#10+
    '%ErrorPart%'+
    'Context: %Context%'+#10+
    'Child stack: %ChildStack%';
var
  Items: TStringList;
  EventText: string;
begin
  lbxPreviewItems.Clear;

  EventText := AEvent.FormatEventAsText(SLayout);

  Items := TStringList.Create;
  try
    Items.Text := EventText;
    lbxPreviewItems.Items.AddStrings(Items);
  finally
    FreeAndNil(Items);
  end;
end;

function TfmReplaceCompLog.GetFocusedEvent: TCompRepEvent;
var
  Item: TListItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) then
    Result := TCompRepEvent(Item.Data)
  else
    Result := nil;
end;

function TfmReplaceCompLog.GetFocusedItem: TListItem;
begin
  if lvLogItems.Items.IndexOf(FFocusedItem) >= 0 then
    Result := FFocusedItem
  else
    Result := nil;
end;

procedure TfmReplaceCompLog.lvLogItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  FFocusedItem := Item;
  UpdatePreviewPanel;
end;

procedure TfmReplaceCompLog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmReplaceCompLog.btnCopyClick(Sender: TObject);
begin
  SaveToClipboard;
end;

procedure TfmReplaceCompLog.GetSelectedItems(Items: TList);
var
  i: Integer;
begin
  Items.Clear;
  for i := 0 to lvLogItems.Items.Count-1 do
    if lvLogItems.Items[i].Selected then
      Items.Add(lvLogItems.Items[i].Data);
end;

procedure TfmReplaceCompLog.SaveToClipboard;
var
  List: TList;
  StrList: TStringList;
begin
  List := TList.Create;
  try
    GetSelectedItems(List);

    StrList := TStringList.Create;
    try
      FormatEventsAsTabs(List, StrList);
      ClipBoard.AsText := StrList.Text;
    finally
      FreeAndNil(StrList);
    end;

  finally
    FreeAndNil(List);
  end;
end;

procedure TfmReplaceCompLog.FormatEventsAsTabs(EventList: TList;
  OutList: TStringList);
resourcestring
  SLayout =
    '%When%'#9'%EventType%'#9'%FlatText%'#9'%FileName%'#9+
    '%ObjectClass%'#9'%ObjectSearchName%'#9'%ComponentName%'#9+
    '%ErrorPart%'#9+
    '%FlatContext%'#9+'%ChildStack%';
var
  i: Integer;
  Line: string;
begin
  for i := 0 to EventList.Count-1 do
  begin
    Line := TCompRepEvent(EventList[i]).FormatEventAsText(SLayout);
    OutList.Add(Line);
  end;
end;

procedure TfmReplaceCompLog.SaveAsCSV(const AFileName: string);
var
  List: TList;
  StrList: TStringList;
begin
  List := TList.Create;
  try
    GetSelectedItems(List);

    StrList := TStringList.Create;
    try
      FormatEventsAsCSV(List, StrList);
      StrList.SaveToFile(AFileName);
    finally
      FreeAndNil(StrList);
    end;

  finally
    FreeAndNil(List);
  end;
end;

procedure TfmReplaceCompLog.FormatEventsAsCSV(EventList: TList;
  OutList: TStringList);
var
  i: Integer;
  Line: string;
  Event: TCompRepEvent;
begin
  for i := 0 to EventList.Count-1 do
  begin
    Line := '';
    Event := TCompRepEvent(EventList[i]);

    Line := CSVAddItem(Line, DateTimeToStr(Event.When));
    Line := CSVAddItem(Line, Event.EventType);
    Line := CSVAddItem(Line, Event.FlatText);
    Line := CSVAddItem(Line, Event.FileName);
    Line := CSVAddItem(Line, Event.ObjectClass);
    Line := CSVAddItem(Line, Event.ObjectSearchName);
    Line := CSVAddItem(Line, Event.ComponentName);
    Line := CSVAddItem(Line, FlatLine(Event.FormatEventAsText('%ErrorPart%')));
    Line := CSVAddItem(Line, FlatLine(Event.Context));
    Line := CSVAddItem(Line, Event.StackText);

    OutList.Add(Line);
  end;
end;

procedure TfmReplaceCompLog.SaveAsXML(const AFileName: string);
var
  List: TList;
begin
  List := TList.Create;
  try
    GetSelectedItems(List);
    FLogEvents.SaveAsXML(AFileName, List);
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmReplaceCompLog.btnSaveAsClick(Sender: TObject);
begin
  if dlgGetExportFile.Execute then
  begin
    if Pos('XML', UpperCase(ExtractFileExt(dlgGetExportFile.FileName)))>0 then
      SaveAsXML(dlgGetExportFile.FileName)
    else
      SaveAsCSV(dlgGetExportFile.FileName);
  end;
end;

procedure TfmReplaceCompLog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
  begin
    ListViewSelectAll(lvLogItems);
    Key := 0;
  end;
end;

end.

