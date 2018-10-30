unit GX_TabOrder;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Forms, Controls, ExtCtrls, Buttons, ActnList, ToolsAPI, ComCtrls, StdCtrls, GX_BaseForm;

type
  TfmTabOrder = class(TfmBaseForm)
    gbxComponents: TGroupBox;
    btnOK: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    pnlButtons: TPanel;
    pnlComponents: TPanel;
    btnOrderByPosition: TButton;
    btnResetOrder: TButton;
    pnlComponentTree: TPanel;
    tvComps: TTreeView;
    b_MoveUp: TBitBtn;
    b_MoveDown: TBitBtn;
    TheActionList: TActionList;
    act_MoveUp: TAction;
    act_MoveDown: TAction;
    procedure btnHelpClick(Sender: TObject);
    procedure tvCompsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvCompsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvCompsClick(Sender: TObject);
    procedure tvCompsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOrderByPositionClick(Sender: TObject);
    procedure btnResetOrderClick(Sender: TObject);
    procedure act_MoveDownExecute(Sender: TObject);
    procedure act_MoveUpExecute(Sender: TObject);
  private
    FormEditor: IOTAFormEditor;
    FBiDiMode: TBiDiMode;
    procedure ChildComponentCallback(Param: Pointer; Component: IOTAComponent; var Result: Boolean);
    procedure SelectCurrentComponent;
    procedure FillTreeView(FromComponent: IOTAComponent);
    procedure ShowOrderButtons(Value: Boolean);
    procedure SortTreeViewComponentsByXYPosition;
    procedure SortTreeViewComponentsByOriginalTabOrder;
  public
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, TypInfo,
  GX_Experts, GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_dzVclUtils;

const
  TabOrderPropertyName = 'TabOrder';
  TabStopPropertyName = 'TabStop';
  TopPropertyName = 'Top';
  LeftPropertyName = 'Left';

type
  TComponentData = class(TComponent)
  public
    X: Integer;
    Y: Integer;
    TabOrder: Integer;
  end;

  TTabExpert = class(TGX_Expert)
  private
    function GetSelectedComponentsFromCurrentForm(FormEditor: IOTAFormEditor;
      var ParentName, ParentType: WideString): TInterfaceList;
    procedure ShowTabOrderForm;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

{ TfmTabOrder }

constructor TfmTabOrder.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

procedure TfmTabOrder.act_MoveDownExecute(Sender: TObject);
var
  Current: TTreeNode;
  Next: TTreeNode;
  Dest: TTreeNode;
  WasExpanded: Boolean;
begin
  Current :=  tvComps.Selected;
  if not Assigned(Current) then
    Exit; //==>
  Next := Current.getNextSibling;
  if not Assigned(Next) then
    Exit; //==>
  Dest := Next.getNextSibling;
  WasExpanded:= Current.Expanded;
  if Assigned(Dest) then
    Current.MoveTo(dest, naInsert)
  else
    Current.MoveTo(Next, naAdd);
  Current.Expanded:= WasExpanded;
end;

procedure TfmTabOrder.act_MoveUpExecute(Sender: TObject);
var
  Current: TTreeNode;
  Dest: TTreeNode;
  WasExpanded: Boolean;
begin
  Current :=  tvComps.Selected;
  if not Assigned(Current) then
    Exit; //==>

  Dest := Current.getPrevSibling;
  if not Assigned(Dest) then
    Exit; //==>
  WasExpanded:= Current.Expanded;
  Current.MoveTo(Dest, naInsert);
  Current.Expanded:= WasExpanded;
end;

procedure TfmTabOrder.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 11);
end;

procedure TfmTabOrder.SelectCurrentComponent;
var
  Component: IOTAComponent;
  i: Integer;
begin
  if Assigned(FormEditor) and Assigned(tvComps.Selected) then
  begin
    i := Pos(':', tvComps.Selected.Text);
    if i > 0 then
      Component := FormEditor.FindComponent(Copy(tvComps.Selected.Text, 1, i-1))
    else
      Component := FormEditor.FindComponent(tvComps.Selected.Text);
    if Assigned(Component) then
      Component.Select(False)
    else
      FormEditor.GetRootComponent.Select(False);
  end;
end;

procedure TfmTabOrder.ChildComponentCallback(Param: Pointer;
  Component: IOTAComponent; var Result: Boolean);
var
  ComponentData: TComponentData;
  TreeNode: TTreeNode;
  AName: WideString;
begin
  if (GxOtaPropertyExists(Component, TabStopPropertyName) and
      GxOtaPropertyExists(Component, TabOrderPropertyName)) or
     (Component.GetControlCount > 0) then
  begin
    // These are freed by the owner form
    ComponentData := TComponentData.Create(Self);
    ShowOrderButtons(True);
    Component.GetPropValueByName(LeftPropertyName, ComponentData.X);
    Component.GetPropValueByName(TopPropertyName, ComponentData.Y);
    Component.GetPropValueByName(TabOrderPropertyName, ComponentData.TabOrder);
    AName := GxOtaGetComponentName(Component);
    TreeNode := tvComps.Items.AddChildObject(TTreeNode(Param), AName + ': ' +
      Component.GetComponentType, ComponentData);
    Component.GetChildren(TreeNode, ChildComponentCallback);
  end;
  Result := True;
end;

function CustomSortProcByPos(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  Result := 0;
  if Assigned(Node1) and Assigned(Node2) and Assigned(Node1.Data) and Assigned(Node2.Data) then
  begin
    Result := TComponentData(Node1.Data).Y - TComponentData(Node2.Data).Y;
    if Result = 0 then
    begin
      if TBiDiMode(Data) in [bdRightToLeft, bdRightToLeftNoAlign, bdRightToLeftReadingOnly] then
        Result := TComponentData(Node2.Data).X - TComponentData(Node1.Data).X
      else
        Result := TComponentData(Node1.Data).X - TComponentData(Node2.Data).X;
    end;
  end;
end;

function CustomSortProcByTabOrder(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  Result := 0;
  if Assigned(Node1) and Assigned(Node2) and Assigned(Node1.Data) and Assigned(Node2.Data) then
    Result := TComponentData(Node1.Data).TabOrder - TComponentData(Node2.Data).TabOrder;
end;

procedure TfmTabOrder.SortTreeViewComponentsByXYPosition;
begin
{$T-}
  tvComps.CustomSort(@CustomSortProcByPos, Ord(FBiDiMode));
{$T+}
end;

procedure TfmTabOrder.SortTreeViewComponentsByOriginalTabOrder;
begin
{$T-}
  tvComps.CustomSort(@CustomSortProcByTabOrder, 0);
{$T+}
end;

procedure TfmTabOrder.FillTreeView(FromComponent: IOTAComponent);
begin
  if tvComps.Items.GetFirstNode <> nil then
    FromComponent.GetChildren(tvComps.Items.GetFirstNode, ChildComponentCallback);
end;

procedure TfmTabOrder.ShowOrderButtons(Value: Boolean);
begin
  btnOrderByPosition.Visible := Value;
  btnResetOrder.Visible := Value;
end;

{ TTabExpert }

procedure TTabExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaCurrentlyEditingForm;
end;

procedure TTabExpert.Execute(Sender: TObject);
begin
  ShowTabOrderForm;
end;

function TTabExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Set Tab &Order...';
begin
  Result := SMenuCaption;
end;

class function TTabExpert.GetName: string;
begin
  Result := 'SetTabOrder';
end;

function TTabExpert.GetSelectedComponentsFromCurrentForm(FormEditor: IOTAFormEditor;
  var ParentName, ParentType: WideString): TInterfaceList;
resourcestring
  SSameParentRequired = 'All selected controls must have the same parent.';
  SNoComponentInterface = 'Unable to obtain component interface for component %d.  ' +
    'Make sure all selected components are descendents of TWinControl/' +
    'TWidgetControl and have valid Parent properties.';
var
  CurrentComponent: IOTAComponent;
  {$IFNDEF GX_VER160_up}
  CurrentComponentParent: TComponent;
  NativeComponent: TComponent;
  BaseComponentParent: TComponent;
  {$ELSE  GX_VER160_up}
  CurrentComponentParent: IOTAComponent;
  BaseComponentParent: IOTAComponent;
  {$ENDIF GX_VER160_up}
  i: Integer;
  ComponentSelCount: Integer;
begin
  ParentName := '';
  Result := TInterfaceList.Create;
  try
    BaseComponentParent := nil;
    ComponentSelCount := FormEditor.GetSelCount;
    for i := 0 to ComponentSelCount - 1 do
    begin
      CurrentComponent := FormEditor.GetSelComponent(i);
      if not Assigned(CurrentComponent) then
        raise Exception.CreateFmt(SNoComponentInterface, [i]);

      // All selected components need to have the same parent
      {$IFNDEF GX_VER160_up}
      NativeComponent := GxOtaGetNativeComponent(CurrentComponent);
      Assert(Assigned(NativeComponent));
      CurrentComponentParent := NativeComponent.GetParentComponent;
      if not Assigned(CurrentComponentParent) then
        Continue // Ignore components without a parent (non-visual components)
      else if BaseComponentParent = nil then
        BaseComponentParent := CurrentComponentParent;
      if BaseComponentParent <> CurrentComponentParent then
        raise Exception.Create(SSameParentRequired);
      {$ELSE  GX_VER160_up}
      CurrentComponentParent := CurrentComponent.GetParent;
      if not Assigned(CurrentComponentParent) then
        Continue // Ignore components without a parent (non-visual components)
      else if BaseComponentParent = nil then
        BaseComponentParent := CurrentComponentParent;
      if not GxOtaComponentsAreEqual(BaseComponentParent, CurrentComponentParent) then
        raise Exception.Create(SSameParentRequired);
      {$ENDIF GX_VER160_up}

      // Only use controls with a TabStop property of
      // type Boolean (which is an enumeration)
      if CurrentComponent.GetPropTypeByName('TabStop') = tkEnumeration then
      begin
        // ... and make dead sure that they have the relevant properties
        if GxOtaPropertyExists(CurrentComponent, TabStopPropertyName) and
           GxOtaPropertyExists(CurrentComponent, TabOrderPropertyName) then
        begin
          if ParentName = '' then
          begin
            {$IFNDEF GX_VER160_up}
            ParentName := BaseComponentParent.Name;
            ParentType := BaseComponentParent.ClassName;
            {$ELSE  GX_VER160_up}
            ParentName := GxOtaGetComponentName(BaseComponentParent);
            ParentType := BaseComponentParent.GetComponentType;
            {$ENDIF GX_VER160_up}
          end;
          Result.Add(CurrentComponent);
        end;
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TTabExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TTabExpert.ShowTabOrderForm;
resourcestring
  SNoFormForCurrentModule = 'There is no form for the current source module.';
var
  FormEditor: IOTAFormEditor;
  ComponentList: TInterfaceList;
  ParentName,
  ParentType: WideString;
  AComponentName,
  AComponentType: WideString;
  TabOrderForm: TfmTabOrder;
  AComponent: IOTAComponent;
  TreeNode: TTreeNode;
  IsMultiSelection: Boolean;
  i: Integer;
  ComponentData: TComponentData;
  FirstTabOrder: Integer;

  procedure UseRootComponent;
  begin
    AComponent := FormEditor.GetRootComponent;
    Assert(Assigned(AComponent));
    ParentType := AComponent.GetComponentType;
    ParentName := GxOtaGetComponentName(AComponent);
  end;

begin
  ComponentList := nil;
  AComponent := nil;
  IsMultiSelection := False;
  FirstTabOrder := -1;

  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    raise Exception.Create(SNoFormForCurrentModule);

  if FormEditor.GetSelCount > 1 then
  begin
    IsMultiSelection := True;
    ComponentList := GetSelectedComponentsFromCurrentForm(FormEditor, ParentName, ParentType);
    if ComponentList.Count = 0 then
      Exit;
  end
  else if (FormEditor.GetSelCount = 1) then
  begin
    AComponent := IOTAComponent(FormEditor.GetSelComponent(0));
    if AComponent.GetControlCount = 0 then
      UseRootComponent
    else
    begin
      ParentType := AComponent.GetComponentType;
      ParentName := GxOtaGetComponentName(AComponent);
    end;
  end
  else
    UseRootComponent;

  TabOrderForm := TfmTabOrder.Create(nil);
  try
    TabOrderForm.ShowOrderButtons(False);
    TabOrderForm.FormEditor := FormEditor;
    SetFormIcon(TabOrderForm);

    TreeNode := TabOrderForm.tvComps.Items.Add(nil, ParentName + ': ' + ParentType);

    if IsMultiSelection then
    begin
      if Assigned(ComponentList) then
      try
        if ComponentList.Count > 0 then
        begin
          for i := 0 to ComponentList.Count-1 do
          begin
            AComponent := ComponentList.Items[i] as IOTAComponent;
            Assert(Assigned(AComponent));
            AComponentType := AComponent.GetComponentType;
            AComponentName := GxOtaGetComponentName(AComponent);

            ComponentData := TComponentData.Create(TabOrderForm);
            TabOrderForm.ShowOrderButtons(True);
            AComponent.GetPropValueByName('Left', ComponentData.X);
            AComponent.GetPropValueByName('Top', ComponentData.Y);
            AComponent.GetPropValueByName(TabOrderPropertyName, ComponentData.TabOrder);
            if (FirstTabOrder > ComponentData.TabOrder) or (FirstTabOrder = -1) then
              FirstTabOrder := ComponentData.TabOrder;
            AComponentName := GxOtaGetComponentName(AComponent);
            TabOrderForm.tvComps.Items.AddChildObject(TreeNode, AComponentName + ': ' + AComponentType,ComponentData);
          end;
        end;
      finally
        FreeAndNil(ComponentList);
      end;
    end
    else
    begin
      TabOrderForm.FBiDiMode := GxOtaGetFormBiDiMode(FormEditor);
      TabOrderForm.FillTreeView(AComponent);
      TabOrderForm.SortTreeViewComponentsByXYPosition;
    end;

    TabOrderForm.tvComps.FullExpand;
    TabOrderForm.tvComps.Selected := TabOrderForm.tvComps.Items.GetFirstNode;
    CenterForm(TabOrderForm);
    if FirstTabOrder < 0 then
      FirstTabOrder := 0;
    if TabOrderForm.ShowModal = mrOk then
    begin
      TreeNode := TabOrderForm.tvComps.Items.GetFirstNode;
      TreeNode := TreeNode.GetNext;
      while TreeNode <> nil do
      begin
        AComponent := FormEditor.FindComponent(Copy(TreeNode.Text, 1, Pos(':', TreeNode.Text)-1));
        if AComponent <> nil then
        begin
          i := TreeNode.Index + FirstTabOrder;
          AComponent.SetPropByName('TabOrder', i);
        end;
        TreeNode := TreeNode.GetNext;
      end;
      IncCallCount;
    end;
  finally
    FreeAndNil(TabOrderForm);
  end;
end;

procedure TfmTabOrder.tvCompsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  TargetNode, SourceNode: TTreeNode;
begin
  TargetNode := TTreeView(Sender).GetNodeAt(X, Y);
  if TargetNode <> nil then
  begin
    SourceNode := TTreeView(Sender).Selected;
    SourceNode.MoveTo(TargetNode, naInsert);
    TTreeView(Sender).Selected := SourceNode;
  end;
end;

procedure TfmTabOrder.tvCompsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetNode: TTreeNode;
  SourceNode: TTreeNode;
begin
  Assert(Sender is TTreeView);
  TargetNode := TTreeView(Sender).GetNodeAt(X, Y);
  SourceNode := TTreeView(Sender).Selected;
  Assert(Assigned(SourceNode));
  if SourceNode = TargetNode then
    Accept := False
  else if (Source = Sender) and (TargetNode <> nil) then
    Accept := SourceNode.Parent = TargetNode.Parent
  else
    Accept := False;
end;

procedure TfmTabOrder.tvCompsClick(Sender: TObject);
begin
  SelectCurrentComponent;
end;

procedure TfmTabOrder.tvCompsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SelectCurrentComponent;
end;

procedure TfmTabOrder.btnOrderByPositionClick(Sender: TObject);
begin
  SortTreeViewComponentsByXYPosition;
end;

procedure TfmTabOrder.btnResetOrderClick(Sender: TObject);
begin
  SortTreeViewComponentsByOriginalTabOrder;
end;

function TTabExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

initialization
  RegisterGX_Expert(TTabExpert);
end.

