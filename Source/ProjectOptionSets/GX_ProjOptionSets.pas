// Original Author: John Hansen <John_Hansen@tcsmgmt.com>
unit GX_ProjOptionSets;

{$I GX_CondDefine.inc}

interface

uses
  Windows, ToolsAPI, TypInfo, Classes, Controls, Forms, StdCtrls,
  ExtCtrls, CheckLst, Menus, ComCtrls, ToolWin, ActnList, OmniXml,
  GX_IdeDock, GX_Experts, GX_ConfigurationInfo;

type
  TfmProjOptionSets = class;

  TProjOptionSetsExpert = class(TGX_Expert)
  private
    function GetStorageFile: string;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Click(Sender: TObject); override;
    property StorageFile: string read GetStorageFile;
    function HasConfigOptions: Boolean; override;
  end;

  TOptionValueFunc = function(const AOption: string): string of object;

  TfmProjOptionSets = class(TfmIdeDockForm)
    pmuPrjOptions: TPopupMenu;
    mniPrjClearAll: TMenuItem;
    mniPrjCheckAll: TMenuItem;
    mniPrjSortByName: TMenuItem;
    mniPrjSortByCheckmark: TMenuItem;
    mniPrjAscending: TMenuItem;
    mniPrjDescending: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    mniModifyPrjOptionValues: TMenuItem;
    pmuEnvOptions: TPopupMenu;
    mniModifyEnvOptionValues: TMenuItem;
    pcSettings: TPageControl;
    tabSets: TTabSheet;
    lstSets: TListBox;
    tabProject: TTabSheet;
    lblProjectSettings: TLabel;
    pnlFilterComboHost: TPanel;
    cbFilter: TComboBox;
    pnlCheckListHost: TPanel;
    tabEnvironment: TTabSheet;
    lblCheckListNot: TLabel;
    pnlCurrentSet: TPanel;
    ToolBar: TToolBar;
    Actions: TActionList;
    actNewSet: TAction;
    actDeleteSet: TAction;
    actSaveSets: TAction;
    actApplySet: TAction;
    actHelp: TAction;
    tbnNewSet: TToolButton;
    tbnDeleteSet: TToolButton;
    tbnSaveSets: TToolButton;
    tbnApplyToProject: TToolButton;
    tbnSep1: TToolButton;
    tbnHelp: TToolButton;
    pmuSets: TPopupMenu;
    mitPopAdd: TMenuItem;
    mitPopApply: TMenuItem;
    mitPopDelete: TMenuItem;
    mitPopSave: TMenuItem;
    mitPopHelp: TMenuItem;
    mitPopSep: TMenuItem;
    actRenameSet: TAction;
    mitPopRename: TMenuItem;
    procedure lstSetsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mniPrjClearAllClick(Sender: TObject);
    procedure mniPrjCheckAllClick(Sender: TObject);
    procedure mniPrjSortByCheckmarkClick(Sender: TObject);
    procedure mniPrjDescendingClick(Sender: TObject);
    procedure pmuPrjOptionsPopup(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure mniModifyEnvOptionValuesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure actNewSetExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actApplySetExecute(Sender: TObject);
    procedure actSaveSetsExecute(Sender: TObject);
    procedure actDeleteSetExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actRenameSetExecute(Sender: TObject);
    procedure mniModifyPrjOptionValuesClick(Sender: TObject);
  private
    lstPrjOptions: TCheckListBox;
    lstEnvOptions: TCheckListBox;
    FPrjSetOptions: TStringList;
    FEnvSetOptions: TStringList;
    FSetChanged: Boolean;
    FLastLoadedSet: string;
    FDom: IXMLDocument;
    FPrjOptions: IOTAProjectOptions;
    FEnvOptions: IOTAEnvironmentOptions;
    FProjItemIndex: Integer;
    FEnvItemIndex: Integer;
    function ProjOptsExpert: TProjOptionSetsExpert;
    procedure lstEnvironmentOptClickCheck(Sender: TObject);
    procedure lstProjectOptClickCheck(Sender: TObject);
    procedure lstEnvironmentOptMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lstProjectOptMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProcessListboxCheckClick(Listbox: TCheckListBox; Options: TStringList);
    procedure HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure SetupListOptionsControls;
    procedure FillFilterBox;
    procedure GenericClearOptionList(AList: TCheckListBox);
    procedure ClearEnvOptionList;
    procedure ClearPrjOptionList;
    procedure SetAllChecks(AList: TCheckListBox; ACheckFlag: Boolean);
    procedure LoadPrjOptionList;
    procedure LoadEnvOptionList;
    procedure LoadOptionSetList;
    procedure LoadStorageIntoDOM;
    procedure SaveDOMToStorageFile;
    procedure LoadSetOptions;
    procedure SaveSetOptions;
    procedure ApplySetOptions;
    procedure DeleteSetFromStorage;
    function  FindSetByName(const Name: string): IXMLElement;
    function  AddSetToDOM(const Name: string): IXMLElement;
    function  GetNodeAttributeValue(Element: IXMLElement; const Name: string): string;
    function GetVariantValueAsString(AValue: Variant; AKind: TTypeKind): string;
    function GetPrjOptionValue(const AOption: string): string;
    function GetEnvOptionValue(const AOption: string): string;
    procedure SetPrjOptionValue(const AOption, AValue: string);
    procedure SetEnvOptionValue(const AOption, AValue: string);
    function GetPrjOptionValueAsString(const AOption: string): string;
    function GetEnvironmentOptions: IOTAEnvironmentOptions;
    function GetCurrentSetName: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RefreshPrjCheckmarks;
    procedure RefreshEnvCheckmarks;
    procedure AddNewOptionSet(const SetName: string);
    function HaveSelectedSet: Boolean;
    function ConfigurationKey: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  fmProjOptionSets: TfmProjOptionSets;
  PrjOptSetsExpert: TProjOptionSetsExpert;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, Clipbrd, {$ENDIF}
  Variants, SysUtils, Messages, Dialogs,
  GX_GxUtils, GX_GenericUtils, GX_IdeUtils, GX_OtaUtils,
  GX_VerDepConst, GX_ProjOptMap, GX_SharedImages, GX_XmlUtils;

resourcestring
  SOptValue = '%s value';
  SOptSaved = 'Saved: ';
  SOptCurrent = 'Current: ';
  SValUnknown = 'unknown';
  SCurrentSet = 'Current Set: ';

const
  ROOT_NODE    = 'ProjectOptionSets';
  SET_NODE     = 'Set';
  ATTR_NODE    = 'Name';
  ENV_OPT_NODE = 'EnvironmentOptions';
  PRJ_OPT_NODE = 'ProjectOptions';
  OPT_NODE     = 'Option';

  {
    XML file structure:
    <ProjectOptionSets>
      <Set Name="SetName">
        <ProjectOptions>
          <Option Name="OptionName">OptionValue</Option>
          <Option Name="OptionName2">OptionValue2</Option>
        </ProjectOptions>
        <EnvironmentOptions>
          <Option Name="OptionName3">OptionValue3</Option>
          <Option Name="OptionName4">OptionValue4</Option>
        </EnvironmentOptions>
      </Set>
      <Set Name="SetName2">
      </Set>
    </ProjectOptionSets>
  }


type
  TKindObject = class(TObject)
  private
    FOptionKind: TTypeKind;
  public
    property OptionKind: TTypeKind read FOptionKind write FOptionKind;
  end;

  TJCHListSortCompare = function(Item1, Item2: Integer): Integer of object;
  TGetHintEvent = procedure(Sender: TObject; const CursorPos: TPoint; var HintStr: string) of object;

  TCheckListBoxWithHints = class(TCheckListBox)
  private
    FOnGetHint: TGetHintEvent;
    FSortAscend: Boolean;
    FSortByString: Boolean;
    FMouseDownIndex: Integer;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetSortAscend(const Value: Boolean);
    procedure SetSortByString(const Value: Boolean);
    procedure QuickSort(L, R: Integer; SCompare: TJCHListSortCompare);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoOnGetHint(const CursorPos: TPoint; var HintStr: string); // virtual;
    function  CompareByStringAscending(Item1, Item2: Integer): Integer;
    function  CompareByStringDescending(Item1, Item2: Integer): Integer;
    function  CompareByCheckAscending(Item1, Item2: Integer): Integer;
    function  CompareByCheckDescending(Item1, Item2: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SortList(Compare: TJCHListSortCompare);
    procedure Resort;
    property SortAscending: Boolean read FSortAscend write SetSortAscend;
    property SortByString: Boolean read FSortByString write SetSortByString;
    property OnGetHint: TGetHintEvent read FOnGetHint write FOnGetHint;
  end;

{ TCheckListBoxWithHints }

procedure TCheckListBoxWithHints.QuickSort(L, R: Integer; SCompare: TJCHListSortCompare);
var
  I, J, P: Integer;
  tmpObj: TObject;
  tmpStr: string;
  tmpChecked: Boolean;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(I, P) < 0 do Inc(I);
      while SCompare(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        // exchange I and J
        tmpStr           := Items[I];
        tmpObj           := Items.Objects[I];
        tmpChecked       := Self.Checked[I];

        Items[I]         := Items[J];
        Items.Objects[I] := Items.Objects[J];
        Self.Checked[I]  := Self.Checked[J];

        Items[J]         := tmpStr;
        Items.Objects[J] := tmpObj;
        Self.Checked[J]  := tmpChecked;
        if P = I then
          P := J
        else if P = J then
          P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCheckListBoxWithHints.CMHintShow(var Message: TMessage);
var
  NewHintStr: string;
begin
  NewHintStr := TCMHintShow(Message).HintInfo^.HintStr;
  DoOnGetHint(TCMHintShow(Message).HintInfo^.CursorPos, NewHintStr);
  TCMHintShow(Message).HintInfo^.HintStr := NewHintStr;
  inherited;
end;

constructor TCheckListBoxWithHints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortAscend := True;
  FSortByString := True;
  IntegralHeight := True;
  FMouseDownIndex := -1;
end;

procedure TCheckListBoxWithHints.DoOnGetHint(const CursorPos: TPoint; var HintStr: string);
begin
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, CursorPos, HintStr);
end;

procedure TCheckListBoxWithHints.SetSortAscend(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortAscend := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SetSortByString(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortByString := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SortList(Compare: TJCHListSortCompare);
begin
  if Items.Count = 0 then Exit;
  Items.BeginUpdate;
  try
    QuickSort(0, Items.Count - 1, Compare);
  finally
    Items.EndUpdate;
  end;
end;

procedure TCheckListBoxWithHints.Resort;
begin
  if SortByString then
  begin
    if SortAscending then
      SortList(CompareByStringAscending)
    else
      SortList(CompareByStringDescending);
  end
  else
  begin
    if SortAscending then
      SortList(CompareByCheckAscending)
    else
      SortList(CompareByCheckDescending);
  end;
end;

function TCheckListBoxWithHints.CompareByCheckAscending(Item1, Item2: Integer): Integer;
begin
  Result := 0;
  if Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := -1
  else if Self.Checked[Item1] and Self.Checked[Item2] then
    Result := AnsiCompareText(Items[Item1], Items[Item2])
  else if not Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := AnsiCompareText(Items[Item1], Items[Item2])
  else if not Self.Checked[Item1] and Self.Checked[Item2] then
    Result := 1;
end;

function TCheckListBoxWithHints.CompareByCheckDescending(Item1, Item2: Integer): Integer;
begin
  Result := 0;
  if Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := 1
  else if Self.Checked[Item1] and Self.Checked[Item2] then
    Result := AnsiCompareText(Items[Item2], Items[Item1])
  else if not Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := AnsiCompareText(Items[Item2], Items[Item1])
  else if not Self.Checked[Item1] and Self.Checked[Item2] then
    Result := -1;
end;

function TCheckListBoxWithHints.CompareByStringAscending(Item1, Item2: Integer): Integer;
begin
  Result := AnsiCompareText(Items[Item1], Items[Item2]);
end;

function TCheckListBoxWithHints.CompareByStringDescending(Item1, Item2: Integer): Integer;
begin
  Result := AnsiCompareText(Items[Item2], Items[Item1]);
end;

procedure TCheckListBoxWithHints.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  // It would be very nice to have the hint permanently up,
  // and have it tracking the current item under the
  // mouse pointer.
end;

procedure TCheckListBoxWithHints.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //Assert(FMouseDownIndex = -1);
  FMouseDownIndex := ItemAtPos(Point(X, Y), True);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCheckListBoxWithHints.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownIndex := -1;
  inherited MouseUp(Button, Shift, X, Y);
end;

{ TProjOptionSetsExpert }

procedure TProjOptionSetsExpert.Click(Sender: TObject);
begin
  if fmProjOptionSets = nil then
    fmProjOptionSets := TfmProjOptionSets.Create(nil);
  SetFormIcon(fmProjOptionSets);
  IdeDockManager.ShowForm(fmProjOptionSets);
end;

constructor TProjOptionSetsExpert.Create;
begin
  inherited Create;

  FreeAndNil(PrjOptSetsExpert);
  PrjOptSetsExpert := Self;
end;

destructor TProjOptionSetsExpert.Destroy;
begin
  FreeAndNil(fmProjOptionSets);

  PrjOptSetsExpert := nil;

  inherited Destroy;
end;

function TProjOptionSetsExpert.GetActionCaption: string;
resourcestring
  SProjOptionsMenuCaption = 'Project &Option Sets';
begin
  Result := SProjOptionsMenuCaption;
end;

class function TProjOptionSetsExpert.GetName: string;
begin
  Result := 'ProjOptionSets'; // Do not localize.
end;

function TProjOptionSetsExpert.GetStorageFile: string;
begin
  Result := AddSlash(ConfigInfo.GetConfigPath) +
    'ProjectOptionSets-' + IdeProductName + MajorVersionNumberChar + '.xml';
end;

procedure TProjOptionSetsExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // This procedure is only called once so it is safe to register the form here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmProjOptionSets,
      fmProjOptionSets, 'fmProjOptionSets');
end;

procedure TProjOptionSetsExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
      FreeAndNil(fmProjOptionSets)
  end;
end;

procedure TProjOptionSetsExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaHaveCurrentProject;
end;

function TProjOptionSetsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TfmProjOptionSets }

constructor TfmProjOptionSets.Create(AOwner: TComponent);
resourcestring
  SModifyOptionValuesMenuCaption = '&Modify Option Values...';
var
  MainForm: TCustomForm;
  ProjectOptions: TComponent;
begin
  inherited Create(AOwner);

  FLastLoadedSet := '';
  SetupListOptionsControls;
  FillFilterBox;

  LoadPrjOptionList;
  LoadEnvOptionList;

  TCheckListBoxWithHints(lstPrjOptions).SortByString := True;
  TCheckListBoxWithHints(lstEnvOptions).SortByString := True;

  FDom := nil; // start with nil DOM
  FPrjSetOptions := TStringList.Create;
  FEnvSetOptions := TStringList.Create;

  LoadSettings;

  // Note: Optimally, we would go through IOTAOptions.EditOptions on our
  //   IOTAProjectOptions, but this fails when no project is loaded
  //   The TAction will work always, in particular for default project
  //   options, when there is no project loaded in the IDE.
  MainForm := GetIdeMainForm;
  if Assigned(MainForm) then
  begin
    ProjectOptions := MainForm.FindComponent('ProjectOptionsCommand');
    if (ProjectOptions <> nil) and (ProjectOptions is TBasicAction) then
    begin
      mniModifyPrjOptionValues.Action := ProjectOptions as TBasicAction;
      mniModifyPrjOptionValues.Caption := SModifyOptionValuesMenuCaption;
    end;
  end;
  if mniModifyPrjOptionValues.Action = nil then
    mniModifyPrjOptionValues.Enabled := False;

  // Open the XML storage file and load it into the DOM
  LoadStorageIntoDOM;
  // Load the saved list of project option sets
  LoadOptionSetList;
end;

destructor TfmProjOptionSets.Destroy;
begin
  SaveSettings;
  ClearPrjOptionList;

  // Now free our objects
  FDom := nil;
  FreeAndNil(FPrjSetOptions);
  FreeAndNil(FEnvSetOptions);
  
  inherited Destroy;

  fmProjOptionSets := nil;
end;

procedure TfmProjOptionSets.lstSetsClick(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    if GetCurrentSetName <> FLastLoadedSet then
    begin
      LoadSetOptions;
      FLastLoadedSet := GetCurrentSetName;
      FSetChanged := False;
    end;
  end;
end;

procedure TfmProjOptionSets.AddNewOptionSet(const SetName: string);
var
  ItemPos: Integer;
begin
  ItemPos := lstSets.Items.IndexOf(SetName);
  if ItemPos = -1 then
  begin
    ItemPos := lstSets.Items.Add(SetName);
    // Update our set list
    AddSetToDOM(SetName);
  end;
  lstSets.ItemIndex := ItemPos;
  lstSetsClick(lstSets);
  FSetChanged := True;
end;

resourcestring
  SAllOptions = 'All options';

procedure TfmProjOptionSets.LoadPrjOptionList;
{$IFOPT D+}
{$DEFINE CheckProjectOptionMap}
{$ENDIF}
{$IFDEF CheckProjectOptionMap}
const
  MissingEntryFormat =
'    (' +sLineBreak+
'      Name: ''%s'';' +sLineBreak+
'      AssumedTypeKind: %s;' +sLineBreak+
'      Description: ''%s'';' +sLineBreak+
'      Categories: [ocUnknown];' +sLineBreak+
'      Translator: GxStringOptionTranslator;' +sLineBreak+
'    ),' +sLineBreak;
{$ENDIF CheckProjectOptionMap}
var
  j: Integer;
  OptionNames: TOTAOptionNameArray;
  tmpObj: TKindObject;
{$IFDEF CheckProjectOptionMap}
  i: Integer;
  MissingEntries: string;
{$ENDIF CheckProjectOptionMap}
begin
  OptionNames := nil;
  ClearPrjOptionList;
  FPrjOptions := GxOtaGetActiveProjectOptions;
  if Assigned(FPrjOptions) then
  try
      OptionNames := FPrjOptions.GetOptionNames;
      for j := Low(OptionNames) to High(OptionNames) do
      begin
      {$IFDEF CheckProjectOptionMap}
        i := High(GxOptionsMap);
        while i >= Low(GxOptionsMap) do
          begin
            if SameText(GxOptionsMap[i].Name, OptionNames[j].Name) then
              Break;
            Dec(i);
          end;

        if i < Low(GxOptionsMap) then
        begin
          // Add missing project option to list
          MissingEntries := MissingEntries + Format(MissingEntryFormat,
            [OptionNames[j].Name, GetEnumName(TypeInfo(TTypeKind), Integer(OptionNames[j].Kind)), OptionNames[j].Name]);
        end
        else
        begin
          // Sanity check: do we handle the IDE's option
          // with the right type ourselves?
          if GxOptionsMap[i].AssumedTypeKind <> tkUnknown then
            Assert(GxOptionsMap[i].AssumedTypeKind = OptionNames[j].Kind, 'Wrong type kind for option ' + GxOptionsMap[i].Name);
        end;
      {$ENDIF CheckProjectOptionMap}

        // Load options honoring filter selection
        if ((cbFilter.Text = sAllOptions) or
            (CategoryTextToCategory(cbFilter.Text) in GetOptionCategories(OptionNames[j].Name))
           ) and
           OptionIsAppropriateForIde(GetOptionCategories(OptionNames[j].Name)) then
        begin
          tmpObj := TKindObject.Create;
          try
            tmpObj.OptionKind := OptionNames[j].Kind;
            lstPrjOptions.Items.AddObject(OptionNames[j].Name, tmpObj);
          finally
            FreeAndNil(tmpObj);
          end;
        end;
        // Done loading option list
      end;

    {$IFDEF CheckProjectOptionMap}
      if MissingEntries <> '' then
      begin
        Clipboard.AsText := MissingEntries;
        // Do not localize; this is just test code
        MessageDlg('There are missing project options in GX_ProjOptMap.pas:' +#13#10+ MissingEntries, mtWarning, [mbOK], 0);
      end;
    {$ENDIF CheckProjectOptionMap}
  finally
    FPrjOptions := nil;
  end;
end;

procedure TfmProjOptionSets.LoadEnvOptionList;
var
  OptionNames: TOTAOptionNameArray;

  {$IFOPT D+}
    procedure DumpAllOptionNamesToDebugWindow;
    var
      i: Integer;
    begin
      SendDebug('Dumping all environment option names from IDE list:');
      for i := Low(OptionNames) to High(OptionNames) do
        SendDebug(OptionNames[i].Name);
    end;
  {$ENDIF D+}

    procedure AddEnvOptionItem(const OptionName: string);
    var
      i: Integer;
      tmpObj: TKindObject;
      EnvOption: TOTAOptionName;
    begin
      i := High(OptionNames);
      while i >= Low(OptionNames) do
      begin
        if OptionNames[i].Name = OptionName then  // Case-sensitive comparison
        begin
          EnvOption := OptionNames[i];
          Break;
        end;
        Dec(i);
      end;

      //  Assert(i >= Low(OptionNames), 'Could not find environment option ' + OptionName);
      if i >= Low(OptionNames) then
      begin
        tmpObj := TKindObject.Create;
        try
          tmpObj.OptionKind := EnvOption.Kind;
          lstEnvOptions.Items.AddObject(EnvOption.Name, tmpObj);
        except
          on E: Exception do
          begin
            FreeAndNil(tmpObj);
            raise;
          end;
        end;
      end;
    end;

begin
  ClearEnvOptionList;

  FEnvOptions := GetEnvironmentOptions;
  if Assigned(FEnvOptions) then
  try
    try
      OptionNames := FEnvOptions.GetOptionNames;
    except
      SetLength(OptionNames, 0); // Delphi 9 does not support these correctly, so ignore them
    end;
    if not Assigned(OptionNames) then
      Exit;

    {$IFOPT D+}
      // DumpAllOptionNamesToDebugWindow;
    {$ENDIF D+}

    // Note: we MANUALLY add to this list
    // as globally adding with all environment
    // options would pollute the list substantially
    // with irrelevant options and hence reduce
    // the usability of this feature.

    // The goal is to identify options that are
    // useful for individual project management.
    // Add when/where necessary

    AddEnvOptionItem('LibraryPath');
    AddEnvOptionItem('DotNetLibraryPath');
    AddEnvOptionItem('CppSearchPath');

    AddEnvOptionItem('BrowsingPath');
    AddEnvOptionItem('DotNetBrowsingPath');
    AddEnvOptionItem('CppBrowsingPath');

    AddEnvOptionItem('RepositoryDir');

    AddEnvOptionItem('PackageSearchPath');
    AddEnvOptionItem('PackageDPLOutput');
    AddEnvOptionItem('PackageDCPOutput');

    AddEnvOptionItem('DeclarationInformation');
    AddEnvOptionItem('ScopeSort');

    AddEnvOptionItem('WarnOnPackageRebuild');

    AddEnvOptionItem('StepProgramBlock');

    AddEnvOptionItem('DFMAsText');
    AddEnvOptionItem('AutoCreateForms');
  finally
    FEnvOptions := nil;
  end;
end;

procedure TfmProjOptionSets.FormShow(Sender: TObject);
begin
  // Add a default, empty set if none exist
  if lstSets.Items.Count = 0 then
    AddNewOptionSet('Default');

  // Load project options in case they have not
  // been available yet; this takes care of the
  // case where the form is shown when no project
  // is open. Without these two lines, after doing
  // the above, it would be impossible to use the
  // expert, as no options would ever be re-retrieved.
  if not Assigned(FPrjOptions) then
    LoadPrjOptionList;

  if not Assigned(FEnvOptions) then
    LoadEnvOptionList;

  RefreshPrjCheckmarks;
  RefreshEnvCheckmarks;
  // Set item index in lstSets
  if lstSets.Items.Count > 0 then
    lstSets.ItemIndex := 0;
  lstSetsClick(lstSets);

  // If we (un)dock, sort order is lost and we need to resort
  if Assigned(lstPrjOptions) then
    TCheckListBoxWithHints(lstPrjOptions).Resort;

  if Assigned(lstEnvOptions) then
    TCheckListBoxWithHints(lstEnvOptions).Resort;
end;

procedure TfmProjOptionSets.LoadOptionSetList;
var
  Nodes: IXMLNodeList;
  SetName: string;
  i: Integer;
  Node: IXMLNode;
begin
  Assert(lstSets.Items.Count = 0, 'Set list already populated?');
  // Load list of sets from our DOM
  Nodes := FDom.documentElement.ChildNodes;
  for i := 0 to Nodes.Length - 1 do
  begin
    // Each node is a Set (maybe)
    Node := Nodes.Item[i];
    if Node.NodeType <> ELEMENT_NODE then
      Continue;
    SetName := GetNodeAttributeValue(Node as IXMLElement, ATTR_NODE);
    if SetName = '' then
      Continue;
    lstSets.Items.Add(SetName);
  end;
end;

procedure TfmProjOptionSets.ProcessListboxCheckClick(Listbox: TCheckListBox; Options: TStringList);
var
  Index, TheItemIndex: Integer;
  ItemString: string;
begin
  TheItemIndex := Listbox.ItemIndex;
  //{$IFOPT D+} SendDebug('Current Options: ' + Options.Text);  {$ENDIF}
  FSetChanged := True;
  Assert(TheItemIndex > -1, 'Check event with no selected item');
  ItemString := Listbox.Items[TheItemIndex];
  if Listbox.Checked[TheItemIndex] then
  begin // Add a new checked item to the stored list
    Index := Options.IndexOfName(ItemString);
    Assert(Index = -1, 'Check event for item in Options');
    ItemString := ItemString + '=['+SValUnknown+']';
    Options.Add(ItemString);
  end
  else
  begin // Remove an item from the stored list
    Index := Options.IndexOfName(ItemString);
    Assert(Index > -1, 'Uncheck event for item not in Options');
    Options.Delete(Index);
  end;
end;

procedure TfmProjOptionSets.lstEnvironmentOptClickCheck(Sender: TObject);
begin
  if lstEnvOptions.ItemIndex <> FEnvItemIndex then
    lstEnvOptions.ItemIndex := FEnvItemIndex;
  ProcessListboxCheckClick(lstEnvOptions, FEnvSetOptions);
end;

procedure TfmProjOptionSets.lstProjectOptClickCheck(Sender: TObject);
begin
  if lstPrjOptions.ItemIndex <> FProjItemIndex then
    lstPrjOptions.ItemIndex := FProjItemIndex;
  ProcessListboxCheckClick(lstPrjOptions, FPrjSetOptions);
end;

procedure TfmProjOptionSets.LoadSetOptions;
var
  SetNode: IXMLElement;

  procedure LoadTheOptions(List: TStringList; const NodeName: string);
  var
    Nodes: IXMLNodeList;
    NumOptions, i: Integer;
    OptionNode: IXMLNode;
    Container: IXMLElement;
    Option: string;
  begin
    Nodes := SetNode.SelectNodes(NodeName);
    // It is possible that the container node (ProjectOptions & EnvironmentOptions)
    // do not yet exist.
    if Nodes.Length > 0 then
    begin
      Container := (Nodes.Item[0] as IXMLElement);
      Assert(Container.NodeType = ELEMENT_NODE, 'Invalid storage file structure');
      Nodes := Container.SelectNodes('Option');
      NumOptions := Nodes.Length;
      Assert((NumOptions >= 0) and (NumOptions <= Length(GxOptionsMap)), 'Corrupt options storage file: ' + IntToStr(NumOptions));
      for i := 0 to NumOptions - 1 do
      begin
        // Now build Option string
        OptionNode := Nodes.Item[i];
        if OptionNode.NodeType <> ELEMENT_NODE then
          Continue;
        Option := GetNodeAttributeValue(OptionNode as IXMLElement, ATTR_NODE);
        if Option = '' then Continue;
        Option := Option + '=' + OptionNode.Text;
        List.Add(Option);
      end;
    end;
  end;

begin
  // Load options from our DOM
  FPrjSetOptions.Clear;
  FEnvSetOptions.Clear;

  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
  begin
    // After we've found our set, load the ProjectOptions and then the EnvironmentOptions
    LoadTheOptions(FPrjSetOptions, PRJ_OPT_NODE);
    LoadTheOptions(FEnvSetOptions, ENV_OPT_NODE);
  end;

  RefreshPrjCheckmarks;
  RefreshEnvCheckmarks;
  FSetChanged := False;
end;

procedure TfmProjOptionSets.ApplySetOptions;
var
  j: Integer;
  OptionName: string;
begin
  // This method uses FPrjSetOptions and updates the ProjectOptions
  FPrjOptions := GxOtaGetActiveProjectOptions;
  try
    if Assigned(FPrjOptions) then
    begin
      for j := 0 to FPrjSetOptions.Count - 1 do
      begin
        OptionName := FPrjSetOptions.Names[j];
        Assert(Trim(OptionName) <> '', 'Empty option name in ApplySetOptions: ' + OptionName);
        SetPrjOptionValue(OptionName, FPrjSetOptions.Values[OptionName]);
      end;
    end;
  finally
    FPrjOptions := nil;
  end;

  // Now update the environment options also
  FEnvOptions := GetEnvironmentOptions;
  try
    if Assigned(FEnvOptions) then
    begin
      for j := 0 to FEnvSetOptions.Count - 1 do
      begin
        OptionName := FEnvSetOptions.Names[j];
        SetEnvOptionValue(OptionName, FEnvSetOptions.Values[OptionName]);
      end;
    end;
  finally
    FEnvOptions := nil;
  end;
end;

procedure TfmProjOptionSets.SaveSetOptions;
var
  SetNode: IXMLElement;
  Container: IXMLElement;
  i: Integer;

  function AddOption(const OptionName: string; ValueFunc: TOptionValueFunc): IXMLElement;
  begin
    Result := FDom.CreateElement(OPT_NODE);
    Result.Text := ValueFunc(OptionName);
    Result.SetAttribute(ATTR_NODE, OptionName);
    Container.AppendChild(Result);
  end;

begin
  // Save the current set to our DOM
  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
    SetNode.ChildNodes.Clear
  else begin
    // If the set node doesn't exist at all then we need to create it
    SetNode := AddSetToDOM(GetCurrentSetName);
  end;

  Container := FDom.CreateElement(PRJ_OPT_NODE);
  SetNode.AppendChild(Container);

  FPrjOptions := GxOtaGetActiveProjectOptions;
  try
    if Assigned(FPrjOptions) then
    begin
      // Iterate over FPrjSetOptions due to filtering
      for i := 0 to FPrjSetOptions.Count - 1 do
        AddOption(FPrjSetOptions.Names[i], GetPrjOptionValue);
    end;
  finally
    FPrjOptions := nil;
  end;

  Container := FDom.CreateElement(ENV_OPT_NODE);
  SetNode.AppendChild(Container);

  FEnvOptions := GetEnvironmentOptions;
  try
    if Assigned(FEnvOptions) then
    begin
      for i := 0 to lstEnvOptions.Items.Count - 1 do
      begin
        if lstEnvOptions.Checked[i] then
          AddOption(lstEnvOptions.Items[i], GetEnvOptionValue);
      end;
    end;
  finally
    FEnvOptions := nil;
  end;

  RefreshPrjCheckmarks;
  RefreshEnvCheckmarks;
  FSetChanged := False;
end;

procedure TfmProjOptionSets.DeleteSetFromStorage;
var
  SetNode: IXMLNode;
begin
  // Delete a set from our DOM (GetCurrentSetName)
  SetNode := FindSetByName(GetCurrentSetName);
  if SetNode <> nil then
    SetNode.ParentNode.RemoveChild(SetNode);
end;

function TfmProjOptionSets.AddSetToDOM(const Name: string): IXMLElement;
begin
  Assert(not Assigned(FindSetByName(Name)));
  Result := FDom.CreateElement(SET_NODE);
  FDom.documentElement.AppendChild(Result);
  Result.SetAttribute(ATTR_NODE, Name);
end;

function TfmProjOptionSets.GetVariantValueAsString(AValue: Variant; AKind: TTypeKind): string;
begin
  case AKind of
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
    tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString, tkVariant,
    tkArray, tkRecord, tkInterface, tkInt64, tkDynArray {$IFDEF GX_VER200_up}, tkUString {$ENDIF}:
      Result := VarAsType(AValue, varString);
  else
    {$IFOPT D+} SendDebug('Unknown value passed to GetValueAsString');  {$ENDIF}
    Result := SValUnknown;
  end;
end;

procedure TfmProjOptionSets.GenericClearOptionList(AList: TCheckListBox);
var
  i: Integer;
begin
  if Assigned(AList) then
  begin
    for i := 0 to AList.Items.Count - 1 do
      AList.Items.Objects[i].Free;

    AList.Items.Clear;
  end;
end;

procedure TfmProjOptionSets.ClearPrjOptionList;
begin
  GenericClearOptionList(lstPrjOptions);
end;

procedure TfmProjOptionSets.ClearEnvOptionList;
begin
  GenericClearOptionList(lstEnvOptions);
end;

procedure TfmProjOptionSets.SetAllChecks(AList: TCheckListBox; ACheckFlag: Boolean);
var
  i: Integer;
begin
  for i := 0 to AList.Items.Count - 1 do
    AList.Checked[i] := ACheckFlag;
end;

procedure TfmProjOptionSets.mniPrjClearAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  SetAllChecks(lstPrjOptions, False);
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    Index := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    if Index > -1 then
      FPrjSetOptions.Delete(Index);
  end;
  FSetChanged := True;
end;

procedure TfmProjOptionSets.mniPrjCheckAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  SetAllChecks(lstPrjOptions, True);
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    Assert(Trim(lstPrjOptions.Items[i]) <> '', 'Empty option name in mniPrjCheckAllClick');
    Index := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    if Index = -1 then
      FPrjSetOptions.Add(lstPrjOptions.Items[i]+'=['+SValUnknown+']');
  end;
  FSetChanged := True;
end;

procedure TfmProjOptionSets.SetupListOptionsControls;
begin
  lstPrjOptions := TCheckListBoxWithHints.Create(Self);
  lstPrjOptions.Align := alClient;
  lstPrjOptions.Parent := pnlCheckListHost;
  lstPrjOptions.PopupMenu := pmuPrjOptions;
  lstPrjOptions.OnClickCheck := lstProjectOptClickCheck;
  lstPrjOptions.OnMouseDown  := lstProjectOptMouseDown;
  lstPrjOptions.ShowHint := True;
  TCheckListBoxWithHints(lstPrjOptions).OnGetHint := HandleOnGetHintPrj;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstPrjOptions).OnMouseMove := lstOptionsMouseMove;

  lstEnvOptions := TCheckListBoxWithHints.Create(Self);
  lstEnvOptions.Align := alClient;
  lstEnvOptions.Parent := tabEnvironment;
  lstEnvOptions.PopupMenu := pmuEnvOptions;
  lstEnvOptions.OnClickCheck := lstEnvironmentOptClickCheck;
  lstEnvOptions.OnMouseDown  := lstEnvironmentOptMouseDown;
  lstEnvOptions.ShowHint := True;
  TCheckListBoxWithHints(lstEnvOptions).OnGetHint := HandleOnGetHintEnv;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstEnvOptions).OnMouseMove := lstOptionsMouseMove;
end;

procedure TfmProjOptionSets.FillFilterBox;
var
  i: TGxOptionCategory;
begin
  cbFilter.Items.Add(SAllOptions);
  cbFilter.ItemIndex := 0;

  for i := Low(TGxOptionCategory) to High(TGxOptionCategory) do
  begin
    if OptionCategoryIsAppropriateForIde(i) then
      cbFilter.Items.AddObject(GxOptionsCategoryText[i], Pointer(i));
  end;
end;

function TfmProjOptionSets.GetPrjOptionValue(const AOption: string): string;
begin
  Result := GetPrjOptionValueAsString(AOption);
end;

function TfmProjOptionSets.GetEnvOptionValue(const AOption: string): string;
var
  idx: Integer;
  tmpObj: TKindObject;
begin
  Result := '';
  if Assigned(FEnvOptions) then
  begin
    idx := lstEnvOptions.Items.IndexOf(AOption);
    if idx <> -1 then
    begin
      tmpObj := TKindObject(lstEnvOptions.Items.Objects[idx]);
      Result := GetVariantValueAsString(FEnvOptions.Values[AOption], tmpObj.OptionKind);
    end;
  end;
end;

function TfmProjOptionSets.GetEnvironmentOptions: IOTAEnvironmentOptions;
begin
  Result := GxOtaGetIDEServices.GetEnvironmentOptions;
end;

procedure ProcessTranslatedValueForLineBreaks(var Value: string);
const
  MaxCols = 30;
begin
  // If our value string does not contain any
  // line breaks, insert them manually.
  // Let's assume that we are using paths
  // and that the semicolon is our preferred
  // delimiter - so break at the semicolon.
  Value := WrapText(Value, sLineBreak, [PathSep], MaxCols);
end;

procedure TfmProjOptionSets.HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstPrjOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstPrjOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstPrjOptions.ItemIndex;

  if idx <> -1 then
  begin
    OptionName := lstPrjOptions.Items[idx];
    HintStr := GetOptionDescription(OptionName);
    if HintStr <> '' then
      HintStr := HintStr + sLineBreak;
    HintStr := HintStr + Format(SOptValue, [OptionName]);
    if lstPrjOptions.Checked[idx] and
      (FPrjSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + sLineBreak;
      // Get the saved value
      TranslatedValueString :=
        TranslatedValue(OptionName, FPrjSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;
    HintStr := HintStr + sLineBreak;
    // Get current value
    FPrjOptions := GxOtaGetActiveProjectOptions;
    try
      TranslatedValueString :=
        TranslatedValue(OptionName, GetPrjOptionValue(OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      FPrjOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstEnvOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstEnvOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstEnvOptions.ItemIndex;

  if idx <> -1 then
  begin
    OptionName := lstEnvOptions.Items[idx];

    //HintStr := GetEnvOptionDescription(OptionName);
    HintStr := OptionName;

    if HintStr <> '' then
      HintStr := HintStr + sLineBreak;

    HintStr := HintStr + Format(SOptValue, [OptionName]);

    if lstEnvOptions.Checked[idx] and
       (FEnvSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + sLineBreak;
      // Get the saved value.  Note that translation always fails, since
      // we don't have maps to translators for any environment options
      TranslatedValueString :=
        TranslatedValue(OptionName, FEnvSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;

    HintStr := HintStr + sLineBreak;

    // Get current value
    FEnvOptions := GetEnvironmentOptions;
    try
      TranslatedValueString := TranslatedValue(OptionName, GetEnvOptionValue(OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      FEnvOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.SaveSettings;
begin
  // Do not localize any of the below items
  with TGExpertsSettings.Create do
  try
    WriteInteger(ConfigurationKey, 'Height', Height);
    WriteInteger(ConfigurationKey, 'Width', Width);
  finally
    Free;
  end;
end;

procedure TfmProjOptionSets.LoadSettings;
begin
  // Do not localize any of the below items
  with TGExpertsSettings.Create do
  try
    Width := ReadInteger(ConfigurationKey, 'Width', Width);
    Height := ReadInteger(ConfigurationKey, 'Height', Height);
  finally
    Free;
  end;
end;

procedure TfmProjOptionSets.mniPrjSortByCheckmarkClick(Sender: TObject);
begin
  TCheckListBoxWithHints(lstPrjOptions).SortByString := (Sender = mniPrjSortByName);
end;

procedure TfmProjOptionSets.mniPrjDescendingClick(Sender: TObject);
begin
  TCheckListBoxWithHints(lstPrjOptions).SortAscending := (Sender = mniPrjAscending);
end;

procedure TfmProjOptionSets.pmuPrjOptionsPopup(Sender: TObject);
begin
  if TCheckListBoxWithHints(lstPrjOptions).SortAscending then
    mniPrjAscending.Checked := True
  else
    mniPrjDescending.Checked := True;

  if TCheckListBoxWithHints(lstPrjOptions).SortByString then
    mniPrjSortByName.Checked := True
  else
    mniPrjSortByCheckmark.Checked := True;
end;

function TfmProjOptionSets.ProjOptsExpert: TProjOptionSetsExpert;
begin
  Result := PrjOptSetsExpert;
end;

procedure TfmProjOptionSets.cbFilterChange(Sender: TObject);
begin
  lstPrjOptions.Items.BeginUpdate;
  try
    LoadPrjOptionList;
    RefreshPrjCheckmarks;
  finally
    lstPrjOptions.Items.EndUpdate;
  end;
end;

procedure TfmProjOptionSets.RefreshPrjCheckmarks;
var
  i, j: Integer;
begin
  if not Assigned(FPrjSetOptions) then
    Exit;
  // Now iterate over lstPrjOptions and check any items in FPrjSetOptions
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    j := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    lstPrjOptions.Checked[i] := (j > -1);
  end;
  // Resort the list
  TCheckListBoxWithHints(lstPrjOptions).Resort;
end;

procedure TfmProjOptionSets.mniModifyEnvOptionValuesClick(Sender: TObject);
var
  IEnvironmentOptions: IOTAEnvironmentOptions;
begin
  IEnvironmentOptions := GetEnvironmentOptions;
  if Assigned(IEnvironmentOptions) then
    IEnvironmentOptions.EditOptions;
end;

procedure TfmProjOptionSets.RefreshEnvCheckmarks;
var
  i, j: Integer;
begin
  if not Assigned(FEnvSetOptions) then
    Exit;
  // Now iterate across FEnvSetOptions and check any items in lstEnvOptions that match
  for i := 0 to lstEnvOptions.Items.Count - 1 do
  begin
    j := FEnvSetOptions.IndexOfName(lstEnvOptions.Items[i]);
    lstEnvOptions.Checked[i] := (j > -1);
  end;
  // Resort the list
  TCheckListBoxWithHints(lstEnvOptions).Resort;
end;

procedure TfmProjOptionSets.SetEnvOptionValue(const AOption, AValue: string);
begin
  if Assigned(FEnvOptions) then
    FEnvOptions.Values[AOption] := AValue;
end;

procedure TfmProjOptionSets.SetPrjOptionValue(const AOption, AValue: string);
var
  VersionKeys: TStrings;
begin
  if Assigned(FPrjOptions) then
  begin
    //{$IFOPT D+} SendDebugFmt('Setting %s to %s (currently %s)', [AOption, AValue, FPrjOptions.Values[AOption]]);  {$ENDIF}
    try
      // BCB 5.01 AVs here on the LibDir setting every time
      if AOption = 'Keys' then begin
        if GxOtaGetVersionInfoKeysStrings(VersionKeys) then
          VersionKeys.Text := AValue;
      end else
        FPrjOptions.Values[AOption] := AValue;
    except on E: Exception do
      raise Exception.Create(Format('Error setting option %s to "%s" (%s).  IDE bug?', [AOption, AValue, E.Message]));
    end;
  end;
end;

procedure TfmProjOptionSets.FormCreate(Sender: TObject);
begin
  inherited;
  SetToolbarGradient(ToolBar);
  SetDefaultFont(Self);
  pcSettings.ActivePage := tabSets;
end;

function TfmProjOptionSets.GetCurrentSetName: string;
begin
  Assert(HaveSelectedSet, 'No selected set in GetCurrentSetName');
  Result := lstSets.Items[lstSets.ItemIndex];
end;

function TfmProjOptionSets.HaveSelectedSet: Boolean;
begin
  Result := lstSets.ItemIndex <> -1;
end;

procedure TfmProjOptionSets.FormHide(Sender: TObject);
begin
  FLastLoadedSet := '';
end;

procedure TfmProjOptionSets.actNewSetExecute(Sender: TObject);
var
  NewSetName: string;
begin
  if InputQuery('Option Set Name', 'Option Set Name', NewSetName) then
  begin
    NewSetName := Trim(NewSetName);
    if NewSetName = '' then
      Exit;
    AddNewOptionSet(NewSetName);
  end;
end;

procedure TfmProjOptionSets.ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actDeleteSet.Enabled := HaveSelectedSet;
  actSaveSets.Enabled := HaveSelectedSet;
  actApplySet.Enabled := HaveSelectedSet and not FSetChanged;
  lstPrjOptions.Enabled := HaveSelectedSet;
  lstEnvOptions.Enabled := HaveSelectedSet;
  cbFilter.Enabled := HaveSelectedSet;
  actRenameSet.Enabled := HaveSelectedSet;
  if HaveSelectedSet then
    pnlCurrentSet.Caption := SCurrentSet + GetCurrentSetName
  else
    pnlCurrentSet.Caption := SCurrentSet + '<None>';
  Handled := True;
end;

procedure TfmProjOptionSets.actApplySetExecute(Sender: TObject);
var
  Cursor: IInterface;
begin
  if HaveSelectedSet then
  begin
    Cursor := TempHourGlassCursor;
    ApplySetOptions;
  end;
end;

procedure TfmProjOptionSets.actSaveSetsExecute(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    // Overwrite the current set in the DOM
    SaveSetOptions;
    // Now reload the set options
    LoadSetOptions;
  end;
  SaveDOMToStorageFile;
end;

procedure TfmProjOptionSets.actDeleteSetExecute(Sender: TObject);
resourcestring
  sDeleteMsg = 'Are you sure you want to delete this option set?';
begin
  if HaveSelectedSet then
  begin
    if MessageDlg(sDeleteMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Delete a set from our DOM
      DeleteSetFromStorage;
      // Delete the item from our list
      lstSets.Items.Delete(lstSets.ItemIndex);
      FLastLoadedSet := '';
      // Clear checks from the option lists, since no set is selected
      SetAllChecks(lstPrjOptions, False);
      FPrjSetOptions.Clear;
      SetAllChecks(lstEnvOptions, False);
      FEnvSetOptions.Clear;
    end;
  end;
end;

procedure TfmProjOptionSets.actHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 27);
end;

procedure TfmProjOptionSets.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfmProjOptionSets.lstEnvironmentOptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FEnvItemIndex := lstEnvOptions.ItemAtPos(Point(X, Y), True);
end;

procedure TfmProjOptionSets.lstProjectOptMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FProjItemIndex := lstPrjOptions.ItemAtPos(Point(X, Y), True);
end;

procedure TfmProjOptionSets.LoadStorageIntoDOM;
var
  Root: IXMLElement;
begin
  FDom := CreateXMLDoc;
  if FileExists(ProjOptsExpert.StorageFile) and (GetFileSize(ProjOptsExpert.StorageFile) <> 0) then
    FDom.Load(ProjOptsExpert.StorageFile)
  else begin
    AddXMLHeader(FDom);
    Root := FDom.CreateElement(ROOT_NODE);
    FDom.DocumentElement := Root;
  end;
end;

procedure TfmProjOptionSets.SaveDOMToStorageFile;
begin
  // We are calling SaveDOMToStorageFile from the destructor where
  // we may be in a forced clean-up due to an exception.
  if ExceptObject <> nil then
    Exit;

  FDom.Save(ProjOptsExpert.StorageFile, ofIndent);
end;

function TfmProjOptionSets.FindSetByName(const Name: string): IXMLElement;
var
  Nodes: IXMLNodeList;
  Node: IXMLNode;
  i: Integer;
  SetName: string;
begin
  Result := nil;
  Nodes := FDom.documentElement.SelectNodes(SET_NODE);
  for i := 0 to Nodes.Length - 1 do begin
    Node := Nodes.Item[i];
    SetName := GetNodeAttributeValue((Node as IXMLElement), ATTR_NODE);
    if (SetName = Name) and (Node.NodeType = ELEMENT_NODE) then
    begin
      Result := Node as IXmlElement;
      Break;
    end;
  end;
end;

function TfmProjOptionSets.GetNodeAttributeValue(Element: IXMLElement; const Name: string): string;
var
  List: IXMLNamedNodeMap;
  A: IXMLNode;
begin
  Result := '';
  List := Element.Attributes;
  if List = nil then
    Exit;
  A := List.GetNamedItem(Name);
  if A = nil then
    Exit;
  Result := A.NodeValue;
end;

procedure TfmProjOptionSets.actRenameSetExecute(Sender: TObject);
var
  NewSetName: string;
  SetNode: IXMLElement;
begin
  if not HaveSelectedSet then
    Exit;
  NewSetName := GetCurrentSetName;
  if InputQuery('Option Set Name', 'Option Set Name', NewSetName) then
  begin
    NewSetName := Trim(NewSetName);
    if NewSetName <> '' then
    begin
      SetNode := FindSetByName(GetCurrentSetName);
      Assert(Assigned(SetNode));
      SetNode.SetAttribute(ATTR_NODE, NewSetName);
      lstSets.Items[lstSets.ItemIndex] := NewSetName;
    end;
  end;
end;

function TfmProjOptionSets.GetPrjOptionValueAsString(const AOption: string): string;
var
  idx: Integer;
  tmpObj: TKindObject;
begin
  Result := '';
  if Assigned(FPrjOptions) then
  begin
    idx := lstPrjOptions.Items.IndexOf(AOption);
    if idx <> -1 then
    begin
      tmpObj := TKindObject(lstPrjOptions.Items.Objects[idx]);
      if (tmpObj.OptionKind = tkClass) and (AOption = 'Keys') then
        Result := GxOtaGetVersionInfoKeysString
      else
        Result := GetVariantValueAsString(FPrjOptions.Values[AOption], tmpObj.OptionKind);
    end;
  end;
end;

function TfmProjOptionSets.ConfigurationKey: string;
begin
  Result := TProjOptionSetsExpert.ConfigurationKey;
end;

procedure TfmProjOptionSets.mniModifyPrjOptionValuesClick(Sender: TObject);
begin
  // This event is assignned at runtime to an internal IDE event handler
end;

initialization
  RegisterGX_Expert(TProjOptionSetsExpert);

end.

