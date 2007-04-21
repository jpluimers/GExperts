unit GX_SelectComponents;

{$I GX_CondDefine.inc}

// TODO: Prevent selecting a VCL form + components, since it isn't actually possible

interface

uses
  Classes, Forms, Controls, ExtCtrls, ToolsAPI, ComCtrls, StdCtrls, Dialogs,
  ActnList, ImgList, Graphics, Buttons, DesignWindows;

type
  TComponentInfo = record
    rName: WideString;
    rType: WideString;
  end;

  TSelectComponentsForm = class(TForm)
    TreeView: TTreeView;
    ActionList: TActionList;
    FindPanel: TPanel;
    SearchEdit: TEdit;
    StayOnTopCheckBox: TCheckBox;
    SelectAllButton: TBitBtn;
    SelectAllAction: TAction;
    BottomPanel: TPanel;
    ExactNameCheckBox: TCheckBox;
    ExactTypeCheckBox: TCheckBox;
    TreePanel: TPanel;
    ResizeButton: TBitBtn;
    ChangeModeAction: TAction;
    FilterLabel: TLabel;
    procedure TreeViewClick(aSender: TObject);
    procedure TreeViewKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure SearchEditChange(aSender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StayOnTopCheckBoxClick(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var aKey: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SearchEditKeyPress(Sender: TObject; var aKey: Char);
    procedure ExactCheckBoxClick(Sender: TObject);
    procedure ChangeModeActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ChangeModeActionHint(var HintStr: String; var CanShow: Boolean);
  private
    FNodesList: TList;
    FCurrentNode: TTreeNode;
    FFormEditor: IOTAFormEditor;
    FMiniMode: Boolean;
    FStayOnTop: Boolean;
    FLastHeight: Integer;

    procedure Init;
    procedure FocusSearchEdit;
    procedure ChangeMode(const aMiniMode: Boolean);

    procedure ChildComponentCallback(aParam: Pointer; aComponent: IOTAComponent; var aResult: Boolean);
    procedure SelectCurrentComponent;
    procedure FillTreeView(const aFromComponent: IOTAComponent);

    procedure SelectComponentOnForm(const aName: WideString; const aAddToSelection: Boolean = False);

    procedure SetCurrentNode(const aNode: TTreeNode);
    procedure FindNextNode;
    procedure FindPrevNode;
    procedure FilterNodes(const aFilter: TComponentInfo);
    procedure SetStayOnTop(const aStayOnTop: Boolean);
  protected
    property CurrentNode: TTreeNode read FCurrentNode write SetCurrentNode;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property MiniMode: Boolean read FMiniMode  write ChangeMode;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop;
    property LastHeight: Integer read FLastHeight;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Messages, TypInfo,
  GX_Experts, GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_SharedImages,
  GX_ConfigurationInfo;

type
  TComponentSelectExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(aAction: TCustomAction); override;
  public
    class function GetName: string; override;

    function GetActionCaption: string; override;
    procedure Click(aSender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

var
  TheForm: TSelectComponentsForm;
  Filter: TComponentInfo;
  LastComponentName: WideString;

procedure GetInfo(const aTreeNode: TTreeNode; const aGetType: Boolean; var aInfo: TComponentInfo); overload;
var
  aPos: Integer;
begin
  with aInfo do
  begin
    rName := UpperCase(aTreeNode.Text);
    aPos := Pos(' : ', rName);

    if aPos > 0 then
    begin
      if aGetType then
        rType := Copy(rName, aPos + 3, Length(rName));
      rName := Copy(rName, 1, aPos - 1);
    end;
  end;
end;

function GetInfo(const aText: WideString): TComponentInfo; overload;
var
  aPos: Integer;
begin
  with Result do
  begin
    rName := UpperCase(aText);
    aPos  := Pos(':', rName);

    if aPos > 0 then
    begin
      rType := Trim(Copy(rName, aPos + 1, Length(rName)));
      rName := Trim(Copy(rName, 1, aPos - 1));
    end;
  end;
end;

function FilterToText(const aFilter: TComponentInfo) : WideString;
begin
  with aFilter do
  begin
    Result := rName;

    if rType <> '' then
      Result := Result + ':' + rType;
  end;
end;

procedure TSelectComponentsForm.SelectComponentOnForm(const aName: WideString;
  const aAddToSelection: Boolean);
var
  aComponent: IOTAComponent;
begin
  aComponent := FFormEditor.FindComponent(aName);
  TreeView.MultiSelect := aAddToSelection;

  if Assigned(aComponent) then
  begin
    LastComponentName := aName;
    aComponent.Select(aAddToSelection);
  end;
end;

procedure TSelectComponentsForm.SelectCurrentComponent;
var
  aInfo: TComponentInfo;
begin
  if Assigned(FFormEditor) and Assigned(TreeView.Selected) then
  begin
    GetInfo(TreeView.Selected, False, aInfo);
    SelectComponentOnForm(aInfo.rName);
  end;
end;

procedure TSelectComponentsForm.SelectAllActionExecute(Sender: TObject);
var
  aIndex: Integer;
  aNode: TTreeNode;
  aInfo: TComponentInfo;
begin
  for aIndex := 0 to Pred(FNodesList.Count) do
  begin
    aNode := FNodesList [aIndex];
    GetInfo(aNode, False, aInfo);
    SelectComponentOnForm(aInfo.rName, aIndex > 0);
  end;

  TreeView.Select(FNodesList);
end;

procedure TSelectComponentsForm.SetCurrentNode(const aNode: TTreeNode);
begin
  if FNodesList.Count > 0 then
  begin
    FCurrentNode := aNode;
    if not Assigned(FCurrentNode) or (FNodesList.IndexOf(FCurrentNode) < 0) then
      FCurrentNode := FNodesList.First;
    TreeView.Select(FCurrentNode);
    SelectCurrentComponent;
  end
  else FCurrentNode := nil;

  if Assigned(FCurrentNode) then
    SearchEdit.Font.Color := clBlack
  else
    SearchEdit.Font.Color := clRed;
end;

procedure TSelectComponentsForm.AfterConstruction;
begin
  inherited;
  FNodesList := TList.Create;
end;

procedure TSelectComponentsForm.BeforeDestruction;
begin
  FreeAndNil(FNodesList);
  inherited;
end;

procedure TSelectComponentsForm.ChangeMode(const aMiniMode: Boolean);
var
  BestFitHeight: Integer;
begin
  if aMiniMode = FMiniMode then
    Exit;

  FMiniMode := aMiniMode;

  if FMiniMode then
    FLastHeight := Height;

  TreePanel.Visible := not FMiniMode;

  if FMiniMode then begin
    ChangeModeAction.ImageIndex := ImageIndexExpand;
    // The above fails to change the image in D6 at least
    ResizeButton.Glyph.Assign(nil);
    GetSharedImageList.GetBitmap(ImageIndexExpand, ResizeButton.Glyph);
    BestFitHeight := Height - ClientHeight + FindPanel.Height;
    Constraints.MinHeight := BestFitHeight;
    Constraints.MaxHeight := BestFitHeight;
    ClientHeight := FindPanel.Height;
  end
  else
  begin
    Constraints.MinHeight := 175;
    Constraints.MaxHeight := 0;
    Height := FLastHeight;
    ChangeModeAction.ImageIndex := ImageIndexContract;
    ResizeButton.Glyph.Assign(nil);
    GetSharedImageList.GetBitmap(ImageIndexContract, ResizeButton.Glyph);
  end;

  FocusSearchEdit;
end;

procedure TSelectComponentsForm.ChangeModeActionExecute(Sender: TObject);
begin
  MiniMode := not MiniMode;
end;

procedure TSelectComponentsForm.ChildComponentCallback(aParam: Pointer;
  aComponent: IOTAComponent; var aResult: Boolean);
var
  aTreeNode: TTreeNode;
  aName: WideString;
begin
  aName := GxOtaGetComponentName(aComponent);
  aTreeNode := TreeView.Items.AddChildObject(TTreeNode(aParam), aName + ' : ' + aComponent.GetComponentType, nil);
  aComponent.GetChildren(aTreeNode, ChildComponentCallback);
  aResult := True;
end;

procedure TSelectComponentsForm.ExactCheckBoxClick(Sender: TObject);
begin
  FocusSearchEdit;
  FilterNodes(Filter);
end;

procedure TSelectComponentsForm.FillTreeView(const aFromComponent: IOTAComponent);
begin
  if TreeView.Items.GetFirstNode <> nil then
    aFromComponent.GetChildren(TreeView.Items.GetFirstNode, ChildComponentCallback);
end;

procedure TSelectComponentsForm.FilterNodes(const aFilter: TComponentInfo);
var
  aByName: Boolean;
  aByType: Boolean;
  aExactName: Boolean;
  aExactType: Boolean;
  aNameMatch: Boolean;
  aTypeMatch: Boolean;
  aNodeIndex: Integer;
  aTreeNode: TTreeNode;
  aInfo: TComponentInfo;
  aFound: Boolean;
begin
  FNodesList.Clear;

  TreeView.Items.BeginUpdate;
  try
    aByName := aFilter.rName <> '';
    aByType := aFilter.rType <> '';

    aExactName := ExactNameCheckBox.Checked;
    aExactType := ExactTypeCheckBox.Checked;

    for aNodeIndex := 0 to Pred(TreeView.Items.Count) do
    begin
      aTreeNode := TreeView.Items [aNodeIndex];
      GetInfo(aTreeNode, aByType, aInfo);

      aNameMatch := aByName and
        (not aExactName and (Pos(aFilter.rName, aInfo.rName) > 0) or
        (aExactName and SameText(aFilter.rName, aInfo.rName)));
                        
      aTypeMatch := aByType and
        (not aExactType and (Pos(aFilter.rType, aInfo.rType) > 0) or
        (aExactType and SameText(aFilter.rType, aInfo.rType)));

      aFound := (aByName and not aByType and aNameMatch) or
        (not aByName and aByType and aTypeMatch) or
        (aByName and aByType and aNameMatch and aTypeMatch);

      if aFound then
        FNodesList.Add(aTreeNode);

      if aFound then // Images disabled for now since D6 fails to show the right images, set StateIndex as well 
        aTreeNode.ImageIndex := ImageIndexArrow
      else
        aTreeNode.ImageIndex := -1;
    end;
  finally
    TreeView.Items.EndUpdate;
  end;

  CurrentNode := CurrentNode;
end;

procedure TSelectComponentsForm.SearchEditChange(aSender: TObject);
begin
  Filter := GetInfo(SearchEdit.Text);

  FilterNodes(Filter);
end;

procedure TSelectComponentsForm.SearchEditKeyPress(Sender: TObject; var aKey: Char);
var
  aReset : Boolean;
begin
  aReset := True;

  case Ord(aKey) of
    VK_RETURN: SelectAllAction.Execute;
    VK_ESCAPE: ;
    VK_SPACE: SearchEdit.Clear;
    else
      aReset := False;
  end;

  if aReset then
    aKey := Chr(0);
end;

procedure TSelectComponentsForm.SearchEditKeyUp(Sender: TObject; var aKey: Word; Shift: TShiftState);
var
  aReset: Boolean;
begin
  aReset := True;

  case aKey of
    VK_UP:
      FindPrevNode;
    VK_DOWN:
      FindNextNode;
    else
      aReset := False;
  end;

  if aReset then
    aKey := 0;
end;

procedure TSelectComponentsForm.FindNextNode;
var
  aNodeIndex : Integer;
begin
  if FNodesList.Count <= 0 then
  begin
    CurrentNode := nil;
    Exit;
  end;

  aNodeIndex := FNodesList.IndexOf(CurrentNode);

  if (aNodeIndex > -1) and (aNodeIndex < Pred(FNodesList.Count)) then
  begin
    Inc(aNodeIndex);
    CurrentNode := FNodesList[aNodeIndex];
  end
  else
    CurrentNode := FNodesList.First;
end;

procedure TSelectComponentsForm.FindPrevNode;
var
  aNodeIndex: Integer;
begin
  if FNodesList.Count <= 0 then
  begin
    CurrentNode := nil;
    Exit;
  end;

  aNodeIndex := FNodesList.IndexOf(CurrentNode);

  if (aNodeIndex > 0) and (aNodeIndex < FNodesList.Count) then
  begin
    Dec(aNodeIndex);
    CurrentNode := FNodesList[aNodeIndex];
  end
  else
    CurrentNode := FNodesList.Last;
end;

procedure TSelectComponentsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_ESCAPE) then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TSelectComponentsForm.FocusSearchEdit;
begin
  if SearchEdit.CanFocus then
    SearchEdit.SetFocus;
end;

procedure TSelectComponentsForm.FormActivate(Sender: TObject);
var
  aName: WideString;
  aInfo: TComponentInfo;
  aNodeIndex: Integer;
  aTreeNode: TTreeNode;
begin
  with Filter do
  try
    Init;
    aName := LastComponentName;

    FocusSearchEdit;
    SearchEdit.Text := FilterToText(Filter);
    SearchEdit.SelectAll;
    SearchEditChange(SearchEdit);

    for aNodeIndex := 0 to Pred(TreeView.Items.Count) do
    begin
      aTreeNode := TreeView.Items[aNodeIndex];
      GetInfo(aTreeNode, False, aInfo);
      if aName = aInfo.rName then
      begin
        CurrentNode := aTreeNode;
        Exit;
      end;
    end;
  except
  end;
end;

procedure TComponentSelectExpert.UpdateAction(aAction: TCustomAction);
begin
  aAction.Enabled := GxOtaCurrentlyEditingForm;
end;

procedure TComponentSelectExpert.Click(aSender: TObject);
begin
  if not Assigned(TheForm) then
    TheForm := TSelectComponentsForm.Create(nil);

  TheForm.Show;
end;

function TComponentSelectExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Select Components...';
begin
  Result := SMenuCaption;
end;

class function TComponentSelectExpert.GetName: string;
begin
  Result := 'SelectComponents';
end;

function TComponentSelectExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.CurrentModule;
end;

procedure TSelectComponentsForm.Init;
var
  aParentName: WideString;
  aParentType: WideString;
  aComponent: IOTAComponent;
begin
  TreeView.Items.BeginUpdate;
  try
    FNodesList.Clear;
    
    SearchEdit.Enabled := False;
    TreeView.Items.Clear;

    if not GxOtaCurrentlyEditingForm then
      Abort;

    FFormEditor := GxOtaGetFormEditorFromModule(GxOtaGetCurrentModule);

    if not Assigned(FFormEditor) then
      Abort;

    aComponent := FFormEditor.GetRootComponent;

    if not Assigned(aComponent) then
      Abort;

    aParentType := aComponent.GetComponentType;
    aParentName := GxOtaGetComponentName(aComponent);

    TreeView.Items.Add(nil, aParentName + ' : ' + aParentType);

    FillTreeView(aComponent);
    TreeView.FullExpand;
    TreeView.Selected := TreeView.Items.GetFirstNode;
    TreeView.Selected.MakeVisible;

    SearchEdit.Enabled := True;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TSelectComponentsForm.SetStayOnTop(const aStayOnTop : Boolean);
begin
  if aStayOnTop = FStayOnTop then
    Exit;

  FStayOnTop := aStayOnTop;

  if FStayOnTop then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  StayOnTopCheckBox.Checked := FStayOnTop;

  FocusSearchEdit;
end;

procedure TSelectComponentsForm.StayOnTopCheckBoxClick(Sender: TObject);
begin
  StayOnTop := StayOnTopCheckBox.Checked;
end;

procedure TSelectComponentsForm.TreeViewClick(aSender: TObject);
begin
  SelectCurrentComponent;
end;

procedure TSelectComponentsForm.TreeViewKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  SelectCurrentComponent;
end;

procedure TSelectComponentsForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  SelectAllAction.Enabled := FNodesList.Count > 0;
  if SelectAllAction.Enabled then
    SelectAllAction.Caption := '&Select ' + IntToStr(FNodesList.Count)
  else
    SelectAllAction.Caption := 'Select';
end;

procedure TSelectComponentsForm.ChangeModeActionHint(var HintStr: String; var CanShow: Boolean);
begin
  if FMiniMode then
    HintStr := 'Expand'
  else
    HintStr := 'Contract';
end;

initialization
  RegisterGX_Expert(TComponentSelectExpert);

finalization
  FreeAndNil(TheForm);

end.
