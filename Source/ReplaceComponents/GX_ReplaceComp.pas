unit GX_ReplaceComp;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ToolsAPI,
  RplWizInfo, GX_ReplaceCompData;

type
  TfmReplaceComp = class(TForm)
    lblSeach: TLabel;
    cbSearch: TComboBox;
    lblReplace: TLabel;
    cbReplace: TComboBox;
    gbxScope: TGroupBox;
    rbAllOnCurrentForm: TRadioButton;
    rbAllInProject: TRadioButton;
    rbSelectedOnCurrentForm: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    gbxOptions: TGroupBox;
    chkLogChanges: TCheckBox;
    chkOverwriteLog: TCheckBox;
    chkShowLogWin: TCheckBox;
    chkIgnoreErrors: TCheckBox;
    chkLogValues: TCheckBox;
    btnSettings: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbSearchChange(Sender: TObject);
    procedure cbSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnSettingsClick(Sender: TObject);
  private
    FController: TCompRepController;
    procedure SetOptionsForNoCurrentForm;
    procedure SetOptionsForNoComponentsSelected;
    procedure LoadComponentList;
    procedure ReplaceComponentsOnCurrentForm(OnlySelected: Boolean);
    procedure ReplaceComponentsOnAllForms;
    procedure ReplaceComponentsOnForm(FormEditor: IOTAFormEditor; OnlySelected: Boolean);
    procedure SetupControls;
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    function PrepareController: TCompRepController;
    procedure ShowLogWin(const SourceClassName, DestClassName: string; LogEvents: TCompRepEventList);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Dialogs, ActnList,
  GX_Experts, GX_GxUtils, GX_OtaUtils, GX_GenericUtils, GX_GenericClasses,
  GX_ReplaceCompMapList, GX_ConfigurationInfo, GX_ReplaceCompLog, GX_ReplaceCompUtils;

type
  TMapScoreList = class;

  TCompRepControllerReal = class(TCompRepController)
  private
    FOverwriteLog: Boolean;
    FLogChanges: Boolean;
    FIgnoreErrors: Boolean;
    FShowLogWin: Boolean;
    FLogValues: Boolean;
    FConfigData: TReplaceCompData;
    FLogEvents: TCompRepEventList;

    FFileName: string;
    FNameStack: TStringList;
    FObjectStack: TStringList;

    FLog: TStringList;

    procedure AssignContext(Event: TCompRepEvent);
    function NewEvent: TCompRepEvent;
    function StackText: string;
    procedure GetCurrentObject(var ObjectName: string; var ObjectPtr: TObject);

    function GetLogFileName: string;
    procedure StartLog;
    procedure SaveLog;
    procedure DeleteLog;
    procedure SaveEventToLog(AEvent: TCompRepEvent);
    function FormatEventForMessage(AEvent: TCompRepEvent): string;
    function TryAddMapping(MapItem: TCompRepMapItem; const ASrcClassName, ADestClassName, APropName,
      AMapPropName: string; Scores: TMapScoreList; Mappings: TCompRepMapList): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure HandleException(E: Exception; const Context: string); override;
    procedure LogMsg(const AMsg: string); override;

    procedure SignalBegin; override;
    procedure SignalEnd; override;

    procedure SignalFileBegin(const AFileName: string); override;
    procedure SignalFileEnd(const AFileName: string); override;
    procedure SignalStackBegin(const AName: string); override;
    procedure SignalStackEnd(const AName: string); override;
    procedure SignalObjectBegin(const AName: string; AObject: TObject); override;
    procedure SignalObjectEnd(const AName: string; AObject: TObject); override;

    procedure PrepareMappingForProp(const AClassName, APropName: string; Mappings: TCompRepMapList); override;
    procedure PrepareConstMappingForProp(const AClassName, APropName: string; Mappings: TCompRepMapList); override;

    function IsLogValuesForced: Boolean; override;

    property ConfigData: TReplaceCompData read FConfigData write FConfigData;
    property IgnoreErrors: Boolean read FIgnoreErrors write FIgnoreErrors;
    property LogChanges: Boolean read FLogChanges write FLogChanges;
    property OverwriteLog: Boolean read FOverwriteLog write FOverwriteLog;
    property ShowLogWin: Boolean read FShowLogWin write FShowLogWin;
    property LogValues: Boolean read FLogValues write FLogValues;

    property LogEvents: TCompRepEventList read FLogEvents;
  end;

  TReplaceCompExpert = class(TGX_Expert)
  private
    FCROShowLogWin: Boolean;
    FCROIgnoreErrors: Boolean;
    FCROLogChanges: Boolean;
    FCROOverwriteLog: Boolean;
    FConfigData: TReplaceCompData;
    FCROLogValues: Boolean;
    FLastReplaceClass: string;
    procedure PrepareConfigData;
    procedure UnprepareConfigData;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetActionCaption: string; override;
    class function GetName: string; override;
    class function ConfigurationKey: string; override;
    procedure Click(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    procedure Configure; override;
    procedure SetActive(New: Boolean); override;
    function HasDesignerMenuItem: Boolean; override;

    property CROIgnoreErrors: Boolean read FCROIgnoreErrors write FCROIgnoreErrors;
    property CROLogChanges: Boolean read FCROLogChanges write FCROLogChanges;
    property CROOverwriteLog: Boolean read FCROOverwriteLog write FCROOverwriteLog;
    property CROShowLogWin: Boolean read FCROShowLogWin write FCROShowLogWin;
    property CROLogValues: Boolean read FCROLogValues write FCROLogValues;
    property LastReplaceClass: string read FLastReplaceClass write FLastReplaceClass;
  end;

  TMapScore = class
    SourceDepth: Integer;
    DestDepth: Integer;
    Item: TCompRepMapItem;
  end;

  TMapScoreList = class(TGxObjectDictionary)
  public
    procedure AddScore(const APropName: string; ASourceDepth, ADestDepth: Integer; MapItem: TCompRepMapItem);
    procedure GetScore(const APropName: string; var SourceDepth, DestDepth: Integer; var MapItem: TCompRepMapItem);
  end;

var
  ReplaceCompExpert: TReplaceCompExpert = nil;

procedure TfmReplaceComp.LoadComponentList;
var
  OldGroup: TPersistentClass;
begin
  cbSearch.Items.BeginUpdate;
  cbReplace.Items.BeginUpdate;
  try
    if GxOtaActiveDesignerIsVCL then
      OldGroup := ActivateClassGroup(TControl)
    else
      OldGroup := nil; // Not necessary?: ActivateClassGroup(QControls.TControl);
    try
      GxOtaGetInstalledComponentList(cbSearch.Items, True);
      cbReplace.Items.Text := cbSearch.Items.Text;
    finally
      if OldGroup <> nil then
        ActivateClassGroup(OldGroup);
    end;

  finally
    cbReplace.Items.EndUpdate;
    cbSearch.Items.EndUpdate;
  end;
end;

procedure TfmReplaceComp.SetOptionsForNoCurrentForm;
begin
  // There is no "current" form, so the only settings
  // that make sense are as follows:
  rbSelectedOnCurrentForm.Enabled := False;
  rbAllOnCurrentForm.Enabled := False;
  rbAllInProject.Checked := True;
end;

procedure TfmReplaceComp.SetOptionsForNoComponentsSelected;
begin
  // No components are selected on the "current" form,
  // so the only settings that make sense are as follows:
  rbSelectedOnCurrentForm.Enabled := False;
  rbAllOnCurrentForm.Enabled := True;
  rbAllOnCurrentForm.Checked := True;
end;

procedure TfmReplaceComp.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 19);
end;

procedure TfmReplaceComp.btnOKClick(Sender: TObject);
resourcestring
  SComponentNotSelectedForSearch = 'You have not selected a component to search for.';
  SComponentNotSelectedForReplace = 'You have not selected a component to replace.';
  SUnknComponentNameForSearch = '%s is not a valid component class to search for.';
  SUnknComponentNameForRep = '%s is not a valid component class to replace with.';
var
  SearchComponent: string;
  ReplaceComponent: string;
begin
  SearchComponent := Trim(cbSearch.Text);
  if SearchComponent = '' then
    raise Exception.Create(SComponentNotSelectedForSearch);
  if GetClass(SearchComponent) = nil then
    ActivateClassGroup(TControl); // This isn't actually right, but probably saves some errors for the most common VCL projects
  if GetClass(SearchComponent) = nil then
    raise Exception.CreateFmt(SUnknComponentNameForSearch, [SearchComponent]);

  ReplaceComponent := Trim(cbReplace.Text);
  if ReplaceComponent = '' then
    raise Exception.Create(SComponentNotSelectedForReplace)
  else if GetClass(ReplaceComponent) = nil then
    raise Exception.CreateFmt(SUnknComponentNameForRep, [ReplaceComponent]);

  SaveFormSettings;

  ModalResult := mrOk;
  Screen.Cursor := crHourglass;
  try
    FController := PrepareController;
    try
      FController.SourceClassName := SearchComponent;
      FController.DestClassName := ReplaceComponent;

      FController.SignalBegin;
      try
        if rbSelectedOnCurrentForm.Checked then
          ReplaceComponentsOnCurrentForm(True)
        else if rbAllOnCurrentForm.Checked then
          ReplaceComponentsOnCurrentForm(False)
        else if rbAllInProject.Checked then
          ReplaceComponentsOnAllForms;
      finally
        FController.SignalEnd;
      end;
      // Refresh the object inspector, since some properties have changed
      GxOtaRefreshCurrentDesigner;

      if TCompRepControllerReal(FController).ShowLogWin then
        ShowLogWin(SearchComponent, ReplaceComponent, TCompRepControllerReal(FController).LogEvents);
    finally
      FreeAndNil(FController);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmReplaceComp.ShowLogWin(const SourceClassName, DestClassName: string; LogEvents: TCompRepEventList);
var
  Dlg: TfmReplaceCompLog;
begin
  Assert(Assigned(ReplaceCompExpert));

  ReplaceCompExpert.PrepareConfigData;

  Dlg := TfmReplaceCompLog.Create(nil, ReplaceCompExpert.FConfigData, SourceClassName, DestClassName, LogEvents);
  try
    Dlg.Icon := Self.Icon;
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfmReplaceComp.cbSearchChange(Sender: TObject);
var
  OkIsEnabled: Boolean;
  SearchComponent: string;
  ReplaceComponent: string;
begin
  SearchComponent := Trim(cbSearch.Text);
  ReplaceComponent := Trim(cbReplace.Text);

  OkIsEnabled := (Length(SearchComponent) > 0) and
                 (Length(ReplaceComponent) > 0) and
                 not SameText(SearchComponent, ReplaceComponent);

  btnOK.Enabled := OkIsEnabled;
end;

procedure TfmReplaceComp.cbSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  SendingComboBox: TComboBox;
begin
  if Key = VK_BACK then
  begin
    SendingComboBox := Sender as TComboBox;
    Assert(Assigned(SendingComboBox));

    SendingComboBox.Text := Copy(SendingComboBox.Text, 1, SendingComboBox.SelStart-1);
    Key := 0;

    cbSearchChange(SendingComboBox);
  end;
end;

function TfmReplaceComp.PrepareController: TCompRepController;
var
  NativeResult: TCompRepControllerReal;
begin
  Assert(Assigned(ReplaceCompExpert));

  Result := TCompRepControllerReal.Create;
  NativeResult := TCompRepControllerReal(Result);
  ReplaceCompExpert.PrepareConfigData;
  NativeResult.ConfigData := ReplaceCompExpert.FConfigData;
  NativeResult.IgnoreErrors := ReplaceCompExpert.CROIgnoreErrors;
  NativeResult.LogChanges := ReplaceCompExpert.CROLogChanges;
  NativeResult.OverwriteLog := ReplaceCompExpert.CROOverwriteLog;
  NativeResult.ShowLogWin := ReplaceCompExpert.CROShowLogWin;
  NativeResult.LogValues := ReplaceCompExpert.CROLogValues;
end;

procedure TfmReplaceComp.ReplaceComponentsOnForm(FormEditor: IOTAFormEditor; OnlySelected: Boolean);
resourcestring
  SFileStart = 'Start of file processing';
  SFileProc = 'File processing';
  SFileFreeInfo = 'Destroying module information';
  SCompSearch = 'Component search';
  SReplaceComp = 'Component replacing';
var
  FormInfo: TFormInfo;
  CompList: TStringList;
  FileName: string;
  Context: string;
begin
  Assert(Assigned(FormEditor));

  CompList := nil;
  FileName := '';
  try
    try
      FileName := FormEditor.FileName;
      FController.SignalFileBegin(FileName);
    except
      on E: Exception do
      begin
        FController.SignalFileBegin(FileName);
        FController.HandleException(E, SFileStart);
      end;
    end;

    Context := SFileProc;
    try
      FormInfo := TFormInfo.Create(FController, FormEditor);
      try
        CompList := TStringList.Create;
        Context := SCompSearch;
        if OnlySelected then
          FormInfo.GetSelectedComponents(CompList, cbSearch.Text)
        else
          FormInfo.GetMatchingComponents(CompList, cbSearch.Text);
        Context := SReplaceComp;
        FormInfo.ReplaceComponents(CompList, cbReplace.Text);
        Context := SFileFreeInfo;
      finally
        FreeAndNil(FormInfo);
        FreeAndNil(CompList);
      end;
    except
      on e: Exception do
        FController.HandleException(E, Context);
    end;
  finally
    FController.SignalFileEnd(FileName);
  end;
end;

procedure TfmReplaceComp.ReplaceComponentsOnCurrentForm(OnlySelected: Boolean);
var
  CurrentModule: IOTAModule;
  CurrentForm: IOTAFormEditor;
begin
  CurrentModule := GxOtaGetCurrentModule;
  Assert(Assigned(CurrentModule));

  CurrentForm := GxOtaGetFormEditorFromModule(CurrentModule);
  Assert(Assigned(CurrentForm));

  ReplaceComponentsOnForm(CurrentForm, OnlySelected);
end;

procedure TfmReplaceComp.ReplaceComponentsOnAllForms;
var
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  i: Integer;
begin
  CurrentProject := GxOtaGetCurrentProject;
  Assert(Assigned(CurrentProject));

  for i := 0 to CurrentProject.GetModuleCount - 1 do
  begin
    ModuleInfo := CurrentProject.GetModule(i);
    Assert(Assigned(ModuleInfo));

    // Ignore non-project units like Forms.pas and .dcp files
    if (Trim(ModuleInfo.FileName) = '') or IsDcp(ModuleInfo.FileName) or IsExecutable(ModuleInfo.FileName) then
      Continue;

    Module := ModuleInfo.OpenModule;
    if not Assigned(Module) then
      Continue;

    FormEditor := GxOtaGetFormEditorFromModule(Module);
    if Assigned(FormEditor) then
      ReplaceComponentsOnForm(FormEditor, False);
  end;
end;

constructor TfmReplaceComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GxSetDefaultFont(Self);

  LoadFormSettings;
  SetupControls;
end;

procedure TfmReplaceComp.SetupControls;
resourcestring
  SOpenSomethingFirst = 'Please open a project or unit first.';
var
  FirstSelectedComponentTypeName: string;
  CurrentModule: IOTAModule;
  FormEditor: IOTAFormEditor;
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  i: Integer;
begin
  LoadComponentList;

  CurrentModule := GxOtaGetCurrentModule;
  if Assigned(CurrentModule) then
    FormEditor := GxOtaGetFormEditorFromModule(CurrentModule)
  else
    FormEditor := nil;

  if not Assigned(FormEditor) then
  begin
    if GxOtaGetCurrentProject = nil then
      raise Exception.Create(SOpenSomethingFirst);
    SetOptionsForNoCurrentForm;
    Exit;
  end;

  if GxOtaSelectedComponentIsRoot(FormEditor) then
  begin
    // The "selected component" is the form itself - hence there are
    // no components on that form that are selected.
    SetOptionsForNoComponentsSelected;
    Exit;
  end;

  SelCount := FormEditor.GetSelCount;
  if not (SelCount > 0) then
    Exit;

  CurrentComponent := FormEditor.GetSelComponent(0);
  Assert(Assigned(CurrentComponent));

  FirstSelectedComponentTypeName := CurrentComponent.GetComponentType;

  i := cbSearch.Items.IndexOf(FirstSelectedComponentTypeName);
  if i > -1 then
  begin
    cbSearch.ItemIndex := i;
    ActiveControl := cbReplace;
  end;
  cbSearchChange(cbSearch);
end;

procedure TfmReplaceComp.LoadFormSettings;
begin
  if ReplaceCompExpert = nil then
    Exit;
  chkIgnoreErrors.Checked := ReplaceCompExpert.CROIgnoreErrors;
  chkLogChanges.Checked := ReplaceCompExpert.CROLogChanges;
  chkOverwriteLog.Checked := ReplaceCompExpert.CROOverwriteLog;
  chkShowLogWin.Checked := ReplaceCompExpert.CROShowLogWin;
  chkLogValues.Checked := ReplaceCompExpert.CROLogValues;
  cbReplace.Text := ReplaceCompExpert.LastReplaceClass;
end;

procedure TfmReplaceComp.SaveFormSettings;
begin
  if ReplaceCompExpert = nil then
    Exit;
  ReplaceCompExpert.CROIgnoreErrors := chkIgnoreErrors.Checked;
  ReplaceCompExpert.CROLogChanges := chkLogChanges.Checked;
  ReplaceCompExpert.CROOverwriteLog := chkOverwriteLog.Checked;
  ReplaceCompExpert.CROShowLogWin := chkShowLogWin.Checked;
  ReplaceCompExpert.CROLogValues := chkLogValues.Checked;
  ReplaceCompExpert.LastReplaceClass := cbReplace.Text;

  ReplaceCompExpert.SaveSettings;
end;

{ TReplaceCompExpert }

procedure TReplaceCompExpert.UpdateAction(Action: TCustomAction);
var
  CurrentModule: IOTAModule;
  FormEditor: IOTAFormEditor;
begin
  CurrentModule := GxOtaGetCurrentModule;
  if Assigned(CurrentModule) then
    FormEditor := GxOtaGetFormEditorFromModule(CurrentModule)
  else
    FormEditor := nil;

  Action.Enabled := Assigned(FormEditor) or (GxOtaGetCurrentProject <> nil);
end;

procedure TReplaceCompExpert.Click(Sender: TObject);
resourcestring
  NoSupportError = 'Due to limitations of the IDE, Replace Components can not support VCL.NET projects.';
begin
  // Replace components requires IOTAComponent interfaces and access to native TPersistent objects
  if GxOtaActiveDesignerIsNFM then
    raise Exception.Create(NoSupportError);
  with TfmReplaceComp.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TReplaceCompExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Replace &Components...';
begin
  Result := SMenuCaption;
end;

class function TReplaceCompExpert.GetName: string;
begin
  Result := 'ReplaceComponents';
end;

function TReplaceCompExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TReplaceCompExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the following lines
  FCROIgnoreErrors := Settings.ReadBool(ConfigurationKey, 'IgnoreErrors', False);
  FCROLogChanges := Settings.ReadBool(ConfigurationKey, 'LogChanges', False);
  FCROOverwriteLog := Settings.ReadBool(ConfigurationKey, 'OverwriteLog', False);
  FCROShowLogWin := Settings.ReadBool(ConfigurationKey, 'ShowLogWin', False);
  FCROLogValues := Settings.ReadBool(ConfigurationKey, 'LogValues', False);
  FLastReplaceClass := Settings.ReadString(ConfigurationKey, 'LastReplaceClass', '');
end;

procedure TReplaceCompExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the following lines
  Settings.WriteBool(ConfigurationKey, 'IgnoreErrors', FCROIgnoreErrors);
  Settings.WriteBool(ConfigurationKey, 'LogChanges', FCROLogChanges);
  Settings.WriteBool(ConfigurationKey, 'OverwriteLog', FCROOverwriteLog);
  Settings.WriteBool(ConfigurationKey, 'ShowLogWin', FCROShowLogWin);
  Settings.WriteBool(ConfigurationKey, 'LogValues', FCROLogValues);
  Settings.WriteString(ConfigurationKey, 'LastReplaceClass', FLastReplaceClass);
end;

constructor TReplaceCompExpert.Create;
begin
  inherited;
  if ReplaceCompExpert = nil then
    ReplaceCompExpert := Self;
end;

destructor TReplaceCompExpert.Destroy;
begin
  UnprepareConfigData;
  if ReplaceCompExpert = Self then
    ReplaceCompExpert := nil;
  inherited;
end;

class function TReplaceCompExpert.ConfigurationKey: string;
begin
  Result := 'ReplaceComponents';
end;

procedure TReplaceCompExpert.PrepareConfigData;
begin
  if not Assigned(FConfigData) then
  begin
    FConfigData := TReplaceCompData.Create;
    FConfigData.RootConfigurationKey := ConfigurationKey;
    FConfigData.ReloadData;
  end;
end;

procedure TReplaceCompExpert.UnprepareConfigData;
begin
  FreeAndNil(FConfigData);
end;

procedure TReplaceCompExpert.Configure;
var
  Dlg: TfmReplaceCompMapList;
begin
  PrepareConfigData;
  Dlg := TfmReplaceCompMapList.Create(nil, FConfigData);
  try
    SetFormIcon(Dlg);
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TReplaceCompExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);
  if Active then
    PrepareConfigData
  else
    UnprepareConfigData;
end;

function TReplaceCompExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

{ TCompRepControllerReal }

constructor TCompRepControllerReal.Create;
begin
  inherited Create;
  FLogEvents := TCompRepEventList.Create;
  FNameStack := TStringList.Create;
  FObjectStack := TStringList.Create;
  FLog := TStringList.Create;
end;

destructor TCompRepControllerReal.Destroy;
begin
  FreeAndNil(FLog);
  FreeAndNil(FObjectStack);
  FreeAndNil(FNameStack);
  FreeAndNil(FLogEvents);
  inherited;
end;

function TCompRepControllerReal.NewEvent: TCompRepEvent;
begin
  Result := TCompRepEvent.Create;
  FLogEvents.Add(Result);
  Result.When := Now;
  AssignContext(Result);
end;

procedure TCompRepControllerReal.HandleException(E: Exception; const Context: string);
resourcestring
  SErrorPfx = 'Error occured: ';
  SErrorSuff = #10+#10+'Ignore this error and continue?';
var
  Event: TCompRepEvent;
  MrRes: Integer;
  Msg: string;
begin
  if E is EAbort then
    Abort;
    
  Event := NewEvent;

  Event.ErrorClass := E.ClassName;
  Event.Text := SErrorPfx+E.Message;
  Event.Context := Context;

  if LogChanges then
    SaveEventToLog(Event);

  if not IgnoreErrors then
  begin
    Msg := FormatEventForMessage(Event)+SErrorSuff;
    MrRes := MessageDlg(Msg, mtError, [mbYes, mbAbort, mbYesToAll], 0);
    if MrRes = mrAbort then
      Abort;
    if MrRes = mrYesToAll then
      IgnoreErrors := True;
  end;
end;

procedure TCompRepControllerReal.LogMsg(const AMsg: string);
var
  Event: TCompRepEvent;
begin
  Event := NewEvent;
  Event.Text := AMsg;

  if LogChanges then
    SaveEventToLog(Event);
end;

procedure TCompRepControllerReal.AssignContext(Event: TCompRepEvent);
resourcestring
  SInvalid = 'Invalid';
var
  ObjectName: string;
  ObjectPtr: TObject;
begin
  Event.FileName := FFileName;
  Event.SourceClassName := Self.SourceClassName;
  Event.DestClassName := Self.DestClassName;
  Event.StackText := Self.StackText;

  GetCurrentObject(ObjectName, ObjectPtr);

  Event.ObjectSearchName := ObjectName;
  if Assigned(ObjectPtr) then
  begin
    try
      Event.ObjectClass := ObjectPtr.ClassName;
      if ObjectPtr is TComponent then
        Event.ComponentName := TComponent(ObjectPtr).Name;
    except
      on E: Exception do
      begin
        Event.ObjectClass := SInvalid;
      end;
    end;
  end;
end;

procedure TCompRepControllerReal.GetCurrentObject(var ObjectName: string;
  var ObjectPtr: TObject);
begin
  if FObjectStack.Count>0 then
  begin
    ObjectName := FObjectStack[FObjectStack.Count-1];
    ObjectPtr := FObjectStack.Objects[FObjectStack.Count-1];
  end
  else
  begin
    ObjectName := '';
    ObjectPtr := nil;
  end;
end;

function TCompRepControllerReal.StackText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FNameStack.Count-1 do
    if i > 0 then
      Result := Result + '\'+ FNameStack[i]
    else
      Result := FNameStack[i];
end;

procedure TCompRepControllerReal.SignalFileBegin(const AFileName: string);
begin
  FFileName := AFileName;
end;

procedure TCompRepControllerReal.SignalFileEnd(const AFileName: string);
begin
  FFileName := '';
end;

procedure TCompRepControllerReal.SignalObjectBegin(const AName: string;
  AObject: TObject);
begin
  FObjectStack.AddObject(AName, AObject);
end;

procedure TCompRepControllerReal.SignalObjectEnd(const AName: string; AObject: TObject);
begin
  with FObjectStack do
    if (Count > 0) and (FObjectStack[Count-1] = AName) then
      Delete(Count-1);
end;

procedure TCompRepControllerReal.SignalStackBegin(const AName: string);
begin
  FNameStack.Add(AName);
end;

procedure TCompRepControllerReal.SignalStackEnd(const AName: string);
begin
  with FNameStack do
    if (Count > 0) and (FNameStack[Count-1] = AName) then
      Delete(Count-1);
end;

function TCompRepControllerReal.TryAddMapping(MapItem: TCompRepMapItem;
  const ASrcClassName, ADestClassName, APropName, AMapPropName: string;
  Scores: TMapScoreList; Mappings: TCompRepMapList): Boolean;
var
  OldScoreSrc, OldScoreDest: Integer;
  NewScoreSrc, NewScoreDest: Integer;
  NewMapItem: TCompRepMapItem;
begin
  Result := False;
  if MapItem.ExtractCorePropName(AMapPropName) <> APropName then
    Exit;

  NewScoreSrc := ClassLevel(GetClass(MapItem.SourceClassName),
    GetClass(ASrcClassName));
  NewScoreDest := ClassLevel(GetClass(MapItem.DestClassName),
    GetClass(ADestClassName));
  Scores.GetScore(AMapPropName, OldScoreSrc, OldScoreDest, NewMapItem);

  if ((OldScoreSrc >= NewScoreSrc) or (OldScoreSrc = -1)) and
     ((OldScoreDest >= NewScoreDest) or (OldScoreDest = -1)) and
    (NewScoreSrc >= 0) and
    (NewScoreDest >= 0)
  then
  begin
    Result := True;
    if NewMapItem = nil then
    begin
      NewMapItem := TCompRepMapItem.Create;
      Mappings.Add(NewMapItem)
    end;

    NewMapItem.AssignMapping(MapItem);

    Scores.AddScore(AMapPropName, NewScoreSrc, NewScoreDest, NewMapItem);
  end;
end;

{ Prepare all relevant mappings for DestClassName and specified params }
procedure TCompRepControllerReal.PrepareMappingForProp(const AClassName,
  APropName: string; Mappings: TCompRepMapList);
var
  i, j: Integer;
  MapItem, WorkItem: TCompRepMapItem;
  Scores: TMapScoreList;
begin
  WorkItem := TCompRepMapItem.Create;
  try
    Scores := TMapScoreList.Create;
    try
      for i := 0 to FConfigData.MapGroupList.Count-1 do
        for j := 0 to FConfigData.MapGroupList[i].Items.Count-1 do
        begin
          MapItem := FConfigData.MapGroupList[i].Items[j];
          if (not TryAddMapping(MapItem, AClassName, DestClassName, APropName,
              MapItem.SourcePropName, Scores, Mappings))
            and
              MapItem.BiDirEnabled
          then
          begin
            WorkItem.AssignMapping(MapItem);
            WorkItem.Reverse;
            TryAddMapping(WorkItem, AClassName, DestClassName, APropName,
              WorkItem.SourcePropName, Scores, Mappings);
          end;
        end;
    finally
      FreeAndNil(Scores);
    end;
  finally
    FreeAndNil(WorkItem);
  end;
end;

{ Prepare all relevant mappings with constant assignment }
procedure TCompRepControllerReal.PrepareConstMappingForProp(
  const AClassName, APropName: string; Mappings: TCompRepMapList);
var
  i, j: Integer;
  MapItem, WorkItem: TCompRepMapItem;
  Scores: TMapScoreList;
begin
  WorkItem := TCompRepMapItem.Create;
  try
    Scores := TMapScoreList.Create;
    try
      for i := 0 to FConfigData.MapGroupList.Count-1 do
        for j := 0 to FConfigData.MapGroupList[i].Items.Count-1 do
        begin
          MapItem := FConfigData.MapGroupList[i].Items[j];
          if not MapItem.UseConstValue then
            Continue;

          if (not TryAddMapping(MapItem, SourceClassName, AClassName, APropName,
              MapItem.DestPropName, Scores, Mappings))
            and
              MapItem.BiDirEnabled
          then
          begin
            WorkItem.AssignMapping(MapItem);
            WorkItem.Reverse;
            TryAddMapping(WorkItem, SourceClassName, AClassName, APropName,
              MapItem.DestPropName, Scores, Mappings);
          end;
        end;
    finally
      FreeAndNil(Scores);
    end;
  finally
    FreeAndNil(WorkItem);
  end;
end;

function TCompRepControllerReal.GetLogFileName: string;
begin
  Result := AddSlash(ConfigInfo.ConfigPath) + 'ReplaceComp.log'
end;

procedure TCompRepControllerReal.StartLog;
var
  FileName: string;
begin
  if not LogChanges then Exit;

  if OverwriteLog then
    DeleteLog;
    
  FLog.Clear;
  FileName := GetLogFileName;
  if FileExists(FileName) then
    FLog.LoadFromFile(FileName);
end;

procedure TCompRepControllerReal.DeleteLog;
begin
  DeleteFile(PChar(GetLogFileName));
end;

procedure TCompRepControllerReal.SaveLog;
begin
  FLog.SaveToFile(GetLogFileName);
end;

procedure TCompRepControllerReal.SignalBegin;
begin
  if LogChanges then
    StartLog;
end;

procedure TCompRepControllerReal.SignalEnd;
begin
  if LogChanges then
    SaveLog;
end;

function TCompRepControllerReal.FormatEventForMessage(AEvent: TCompRepEvent): string;
resourcestring
SLayout =
'Error occured during replace:'+#10+
#10+
'Message: %Text%'+#10+
#10+
'Error class: %ErrorClass%'+#10+
'FileName: %FileName%'+#10+
'Source class: %SourceClassName%, Destination class: %DestClassName%'+#10+
'Object: %ObjectClass%, %ObjectSearchName%'+#10+
'Context: %Context%';
begin
  Result := AEvent.FormatEventAsText(SLayout);
end;

procedure TCompRepControllerReal.SaveEventToLog(AEvent: TCompRepEvent);
resourcestring
SLayout =
'--> [%When%] %EventType%'+#10+
'>Message:'+#10+
'%Text%'+#10+
'>Context:'+#10+
'FileName: %FileName%'+#10+
'Source class: %SourceClassName%, Destination class: %DestClassName%'+#10+
'Object: %ObjectClass%, %ObjectSearchName%'+#10+
'%ErrorPart%'+
'Context: %Context%'+#10+
'-->';

var
  Items: TStringList;
  EventText: string;
begin
  EventText := AEvent.FormatEventAsText(SLayout);

  Items := TStringList.Create;
  try
    Items.Text := EventText;
    FLog.AddStrings(Items);
  finally
    FreeAndNil(Items);
  end;

  SaveLog;
end;

function TCompRepControllerReal.IsLogValuesForced: Boolean;
begin
  Result := FLogValues;
end;

{ TMapScoreList }

procedure TMapScoreList.AddScore(const APropName: string; ASourceDepth,
  ADestDepth: Integer; MapItem: TCompRepMapItem);
var
  NewScore: TMapScore;
begin
  NewScore := FindObject(APropName) as TMapScore;
  if not Assigned(NewScore) then
  begin
    NewScore := TMapScore.Create;
    inherited AddWithCode(APropName, NewScore);
  end;

  NewScore.SourceDepth := ASourceDepth;
  NewScore.DestDepth := ADestDepth;
  NewScore.Item := MapItem;
end;

procedure TMapScoreList.GetScore(const APropName: string; var SourceDepth,
  DestDepth: Integer; var MapItem: TCompRepMapItem);
var
  MapScore: TMapScore;
begin
  MapScore := FindObject(APropName) as TMapScore;
  if Assigned(MapScore) then
  begin
    SourceDepth := MapScore.SourceDepth;
    DestDepth := MapScore.DestDepth;
    MapItem := MapScore.Item;
  end
  else
  begin
    SourceDepth := -1;
    DestDepth := -1;
    MapItem := nil;
  end;
end;

procedure TfmReplaceComp.btnSettingsClick(Sender: TObject);
begin
  ReplaceCompExpert.Configure;
end;

initialization
  ReplaceCompExpert := nil;
  RegisterGX_Expert(TReplaceCompExpert);
end.

