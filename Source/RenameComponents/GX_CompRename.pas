unit GX_CompRename;

(* Things left to consider:
 *  - Search a new component's ancestors to get an alternate rename rule
 *  - Remove timer hack somehow?
 *  - Allow explicit %d rename rules to apply before the rename dialog shows
 *)

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ToolsAPI, ComCtrls, Buttons,
  GX_Experts, GX_ConfigurationInfo, GX_EditorChangeServices, Contnrs,
  GX_BaseForm, GX_dzSpeedBitBtn;

type
  TCompRenameExpert = class;

  TIsValidComponentName = function (const OldName, NewName: WideString; var Reason: WideString): Boolean of object;

  // Simple rename dialog that shows the old and new component name
  TfmCompRename = class(TfmBaseForm)
    lblOldName: TLabel;
    edtOldName: TEdit;
    lblNewName: TLabel;
    edtNewName: TEdit;
    btnCancel: TButton;
    btnOK: TButton;
    lblReason: TLabel;
    btnSettings: TButton;
    pc_Additional: TPageControl;
    ts_Align: TTabSheet;
    grp_Margins: TGroupBox;
    ed_MarginTop: TEdit;
    l_MarginTop: TLabel;
    ed_MarginLeft: TEdit;
    l_MarginLeft: TLabel;
    l_MarginRight: TLabel;
    ed_MarginRight: TEdit;
    ed_MarginBottom: TEdit;
    l_MarginBottom: TLabel;
    chk_WithMargins: TCheckBox;
    ts_Anchors: TTabSheet;
    b_AnchorLeft: TBitBtn;
    b_AnchorRight: TBitBtn;
    b_AnchorTop: TBitBtn;
    b_AnchorBottom: TBitBtn;
    b_AlignTop: TBitBtn;
    b_AlignLeft: TBitBtn;
    b_AlignRight: TBitBtn;
    b_AlignClient: TBitBtn;
    b_AlignBottom: TBitBtn;
    b_AlignNone: TBitBtn;
    b_AlignCustom: TBitBtn;
    b_Margins0: TButton;
    b_Margins3: TButton;
    b_Margins6: TButton;
    b_Margins8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtNewNameChange(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure pc_AdditionalChange(Sender: TObject);
    procedure b_AlignTopClick(Sender: TObject);
    procedure b_AlignLeftClick(Sender: TObject);
    procedure b_AlignClientClick(Sender: TObject);
    procedure b_AlignRightClick(Sender: TObject);
    procedure b_AlignBottomClick(Sender: TObject);
    procedure b_AlignNoneClick(Sender: TObject);
    procedure b_AlignCustomClick(Sender: TObject);
    procedure b_Margins0Click(Sender: TObject);
    procedure b_Margins3Click(Sender: TObject);
    procedure b_Margins6Click(Sender: TObject);
    procedure b_Margins8Click(Sender: TObject);
  private
    FIsValidComponentName: TIsValidComponentName;
    FProperties: TObjectList;
    FAnchorButtons: array[TAnchorKind] of TdzSpeedBitBtn;
    FAlignButtons: array[TAlign] of TdzSpeedBitBtn;
    function GetNewName: WideString;
    function GetOldName: WideString;
    procedure SetNewName(const Value: WideString);
    procedure SetOldName(const Value: WideString);
    procedure AddComponentProperty(PropertyName, Value: WideString);
    function GetComponentProperty(Index: Integer): WideString;
    procedure SetAlign(const _Component: IOTAComponent);
    procedure GetAlign(const _Component: IOTAComponent);
    procedure SetAnchors(const _Component: IOTAComponent);
    procedure GetAnchors(const _Component: IOTAComponent);
    procedure HandleAlignButtons(_Align: TAlign);
    procedure SetMargins(_Value: integer);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property OldName: WideString read GetOldName write SetOldName;
    property NewName: WideString read GetNewName write SetNewName;
    function Execute: TModalResult;
    procedure SetRuleSelection(SelStart, SelEnd: Integer);
    property OnIsValidComponentName: TIsValidComponentName read FIsValidComponentName write FIsValidComponentName;
  end;

  TCompRenameNotifier = class(TInterfacedObject, IGxEditorNotification)
  private
    FChangeServiceNotifierIndex: Integer;
    FClient: TCompRenameExpert;
  protected
    // IGxEditorNotification
    procedure NewModuleOpened(const Module: IOTAModule);
    procedure SourceEditorModified(const SourceEditor: IOTASourceEditor);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: string);
    function EditorKeyPressed(const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
    function GetIndex: Integer;
  protected
    procedure Attach;
  public
    constructor Create(const Client: TCompRenameExpert);
    destructor Destroy; override;

    procedure Detach;
  end;

  TCompRenameExpert = class(TGX_Expert)
  private
    FCompRenameNotifier: TCompRenameNotifier;
    FRenameRuleList: TStringList;
    FShowDialog: Boolean;
    FAutoAddClasses: Boolean;
    FComponentNames: TStringList;
    FFormNames: TStringList;
    FTimer: TTimer;
    FTimerCount: Integer;
    FFormEditor: IOTAFormEditor;
    function DoRename(const Component: IOTAComponent; UseRules: Boolean): TModalResult;
  protected
    procedure AddNewClass(const AClassName: WideString);
    procedure DoOnTimer(Sender: TObject);
    function GetClassRenameRule(const AClassName: WideString): WideString;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
    procedure ComponentRenamed(const FormEditor: IOTAFormEditor;
      Component: IOTAComponent; const OldName, NewName: WideString);
    procedure FormEditorModified(const FormEditor: IOTAFormEditor);
    function IsValidComponentName(const OldName, NewName: WideString; var Reason: WideString): Boolean;
    procedure AddNotifier;
    procedure RemoveNotifier;
    function IsDefaultComponentName(Component: IOTAComponent; const NewName: WideString): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    procedure SetActive(New: Boolean); override;
    function IsDefaultActive: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Menus, GX_CompRenameConfig, GX_OtaUtils, GX_GenericUtils,
  GX_IdeUtils, Graphics, GX_GxUtils, TypInfo, GX_dzVclUtils;

resourcestring
  SPropertyNotFound = 'Property not found';

var
  PrivateCompRenameExpert: TCompRenameExpert;

{ TfmCompRename }

function TfmCompRename.Execute: TModalResult;
begin
  ActiveControl := edtNewName;
//  lblReason.Top := btnOK.Top + Round((btnOK.Height / 2) - (lblReason.Height / 2));
  Result := ShowModal;
end;

function TfmCompRename.GetNewName: WideString;
begin
  Result := Trim(edtNewName.Text);
end;

function TfmCompRename.GetOldName: WideString;
begin
  Result := Trim(edtOldName.Text);
end;

procedure TfmCompRename.pc_AdditionalChange(Sender: TObject);
begin
  inherited;
  if pc_Additional.ActivePage = ts_Align then
    TWinControl_SetFocus(b_AlignLeft)
  else if pc_Additional.ActivePage = ts_Anchors then
    TWinControl_SetFocus(b_AnchorTop);
end;

procedure TfmCompRename.GetAlign(const _Component: IOTAComponent);
var
  BoolValue: LongBool;
  CompMargins: TObject;
  al: TAlign;
  IntValue: Integer;
begin
  if not GxOtaActiveDesignerIsVCL then begin
    // for whatever reason this currently does not work for FMX
    Exit; //==>
  end;

  if not ts_Align.TabVisible then
    Exit; //==>

  al := Low(TAlign);
  while al < High(TAlign) do begin
    if FAlignButtons[al].Down then
      Break;
    Inc(al);
  end;
  IntValue := Ord(al);
  _Component.SetPropByName('Align', IntValue);

  if not grp_Margins.Visible then
    Exit; //==>

  if chk_WithMargins.Visible then begin
    BoolValue := chk_WithMargins.Checked;
    _Component.SetPropByName('AlignWithMargins', BoolValue);
  end;
  if _Component.GetPropTypeByName('Margins') = tkClass then begin
    CompMargins := GetObjectProp((_Component as INTAComponent).GetComponent, 'Margins');
    if TryStrToInt(ed_MarginTop.Text, IntValue) then
      SetOrdProp(CompMargins, 'Top', IntValue);
    if TryStrToInt(ed_MarginLeft.Text, IntValue) then
      SetOrdProp(CompMargins, 'Left', IntValue);
    if TryStrToInt(ed_MarginRight.Text, IntValue) then
      SetOrdProp(CompMargins, 'Right', IntValue);
    if TryStrToInt(ed_MarginBottom.Text, IntValue) then
      SetOrdProp(CompMargins, 'Bottom', IntValue);
  end;
end;

procedure TfmCompRename.SetAlign(const _Component: IOTAComponent);
var
  BoolValue: LongBool;
  CompMargins: TObject;
  IntValue: Integer;
  AlignValue: TAlign;
  al: TAlign;
begin
  if not GxOtaActiveDesignerIsVCL then begin
    ts_Align.TabVisible := False;
    // for whatever reason this currently does not work for FMX
    Exit; //==>
  end;

  // GetPropValueByName expects 4 byte values for enums
  if not _Component.GetPropValueByName('Align', IntValue) then begin
    ts_Align.TabVisible := False;
    Exit; //==>
  end;

  if (IntValue < Ord(Low(TAlign))) or (Ord(High(TAlign)) < IntValue) then begin
    ts_Align.TabVisible := False;
    Exit; //==>
  end;
  AlignValue := TAlign(IntValue);

  for al := Low(TAlign) to High(TAlign) do
    FAlignButtons[al].Down := (al = AlignValue);

  if _Component.GetPropTypeByName('Margins') <> tkClass then begin
    grp_Margins.Visible := False;
    chk_WithMargins.Visible := False;
    Exit; //==>
  end;

  // GetPropValueByName expects 4 byte values for booleans too
  if not _Component.GetPropValueByName('AlignWithMargins', BoolValue) then begin
    chk_WithMargins.Visible := False;
  end;
  chk_WithMargins.Checked := BoolValue;
  CompMargins := GetObjectProp((_Component as INTAComponent).GetComponent, 'Margins');
  IntValue := GetOrdProp(CompMargins, 'Top');
  ed_MarginTop.Text := IntToStr(IntValue);
  IntValue := GetOrdProp(CompMargins, 'Left');
  ed_MarginLeft.Text := IntToStr(IntValue);
  IntValue := GetOrdProp(CompMargins, 'Right');
  ed_MarginRight.Text := IntToStr(IntValue);
  IntValue := GetOrdProp(CompMargins, 'Bottom');
  ed_MarginBottom.Text := IntToStr(IntValue);
end;

procedure TfmCompRename.GetAnchors(const _Component: IOTAComponent);
var
  AnchorValue: TAnchors;
  IntValue: Integer;
  ak: TAnchorKind;
begin
  if ts_Anchors.TabVisible then begin
    AnchorValue := [];
    for ak  := Low(TAnchorKind) to High(TAnchorKind) do begin
      if FAnchorButtons[ak].Down then
        Include(AnchorValue, ak);
    end;
    IntValue := 0;
    Move(AnchorValue, IntValue, SizeOf(AnchorValue));
    _Component.SetPropByName('Anchors', AnchorValue);
  end;
end;

procedure TfmCompRename.SetAnchors(const _Component: IOTAComponent);
var
  AnchorValue: TAnchors;
  IntValue: Integer;
  ak: TAnchorKind;
begin
  if not _Component.GetPropValueByName('Anchors', IntValue) then begin
    ts_Anchors.TabVisible := False;
  end else begin
    Move(IntValue, AnchorValue, SizeOf(AnchorValue));
    for ak := Low(TAnchorKind) to High(TAnchorKind) do
      FAnchorButtons[ak].Down := (ak in AnchorValue);
  end;
end;

procedure TfmCompRename.SetNewName(const Value: WideString);
begin
  edtNewName.Text := Value;
  edtNewName.Modified := False;
end;

procedure TfmCompRename.SetOldName(const Value: WideString);
begin
  edtOldName.Text := Value;
end;

procedure TfmCompRename.edtNewNameChange(Sender: TObject);
var
  OldName: WideString;
  NewName: WideString;
  Reason: WideString;
begin
  if Assigned(FIsValidComponentName) then
  begin
    OldName := edtOldName.Text;
    NewName := edtNewName.Text;
    btnOK.Enabled := FIsValidComponentName(OldName, NewName, Reason);
    if btnOK.Enabled then
      lblReason.Visible := False
    else
    begin
      lblReason.Caption := Reason;
      lblReason.Visible := True;
    end;
  end
  else
    btnOK.Enabled := IsValidIdent(edtNewName.Text);
end;

procedure TfmCompRename.SetRuleSelection(SelStart, SelEnd: Integer);
begin
  edtNewName.SelStart := SelStart;
  if SelEnd > SelStart then
    edtNewName.SelLength := SelEnd - SelStart;
end;

procedure TfmCompRename.AddComponentProperty(PropertyName, Value: WideString);

  procedure VerticallyMove(_cntrl: Tcontrol; _by: Integer);
  begin
    _cntrl.Top := _cntrl.Top + _by;
  end;

var
  Lbl: TLabel;
  Edit: TEdit;
  diff: Integer;
begin
  PropertyName := Trim(PropertyName);
  if PropertyName = '' then
    Exit; //==>

  Lbl := TLabel.Create(Self);
  Lbl.Parent := Self;
  Lbl.Top := (FProperties.Count + 1) * (lblNewName.Top - lblOldName.Top) + lblNewName.Top;
  Lbl.Left := lblNewName.Left;
  Lbl.Caption := PropertyName;
  Edit := TEdit.Create(Self);
  Edit.Parent := Self;
  Edit.Top := (FProperties.Count + 1) * (edtNewName.Top - edtOldName.Top) + edtNewName.Top;
  Edit.Left := edtNewName.Left;
  Edit.Width := edtNewName.Width;
  Edit.Text := Value;
  Edit.TabOrder := FProperties.Count + 2;
  FProperties.Add(Edit);

  if Edit.Text = SPropertyNotFound then begin
    Edit.ReadOnly := True;
    Edit.Color := clBtnFace;
    Edit.TabStop := False;
    Lbl.Font.Color := clGrayText;
  end;

  diff := edtNewName.Top - edtOldName.Top;
  Height := Height + diff;
  VerticallyMove(btnOK, diff);
  VerticallyMove(btnCancel, diff);
  VerticallyMove(btnSettings, diff);
  VerticallyMove(lblReason, diff);
end;

function TfmCompRename.GetComponentProperty(Index: Integer): WideString;
begin
  if (Index >=0) and (Index < FProperties.Count) then
    Result := (FProperties[Index] as TEdit).Text
  else
    raise Exception.Create('Invalid property index in TfmCompRename.GetComponentProperty');
end;

constructor TfmCompRename.Create(Owner: TComponent);
begin
  inherited;
  FProperties := TObjectList.Create(False);

  FAlignButtons[alTop] := TdzSpeedBitBtn.Create(b_AlignTop);
  FAlignButtons[alLeft] := TdzSpeedBitBtn.Create(b_AlignLeft);
  FAlignButtons[alClient] := TdzSpeedBitBtn.Create(b_AlignClient);
  FAlignButtons[alRight] := TdzSpeedBitBtn.Create(b_AlignRight);
  FAlignButtons[alBottom] := TdzSpeedBitBtn.Create(b_AlignBottom);
  FAlignButtons[alNone] := TdzSpeedBitBtn.Create(b_AlignNone);
  FAlignButtons[alCustom] := TdzSpeedBitBtn.Create(b_AlignCustom);

  FAnchorButtons[akTop] := TdzSpeedBitBtn.Create(b_AnchorTop);
  FAnchorButtons[akLeft] := TdzSpeedBitBtn.Create(b_AnchorLeft);
  FAnchorButtons[akRight] := TdzSpeedBitBtn.Create(b_AnchorRight);
  FAnchorButtons[akBottom] := TdzSpeedBitBtn.Create(b_AnchorBottom);
end;

destructor TfmCompRename.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

{ TCompRenameNotifier }

procedure TCompRenameNotifier.Attach;
begin
  Assert(FChangeServiceNotifierIndex = -1);
  FChangeServiceNotifierIndex := GxEditorChangeServices.AddNotifier(Self);
end;

procedure TCompRenameNotifier.ComponentRenamed(const FormEditor: IOTAFormEditor;
  Component: IOTAComponent; const OldName, NewName: string);
begin
  if Assigned(FClient) then
    FClient.ComponentRenamed(FormEditor, Component, OldName, NewName);
end;

constructor TCompRenameNotifier.Create(const Client: TCompRenameExpert);
begin
  inherited Create;

  if Assigned(Client) then
  begin
    FClient := Client;
    FChangeServiceNotifierIndex := -1;
    Attach;
  end;
end;

destructor TCompRenameNotifier.Destroy;
begin
  Detach;
  FClient := nil;
  inherited Destroy;
end;

procedure TCompRenameNotifier.Detach;
begin
  GxEditorChangeServices.RemoveNotifierIfNecessary(FChangeServiceNotifierIndex);
end;

function TCompRenameNotifier.EditorKeyPressed(
  const SourceEditor: IOTASourceEditor; CharCode: Word; KeyData: Integer): Boolean;
begin
  Result := False;
  // Nothing
end;

procedure TCompRenameNotifier.FormEditorModified(const FormEditor: IOTAFormEditor);
begin
  if Assigned(FClient) then
    FClient.FormEditorModified(FormEditor);
end;

function TCompRenameNotifier.GetIndex: Integer;
begin
  Result := FChangeServiceNotifierIndex;
end;

procedure TCompRenameNotifier.NewModuleOpened(const Module: IOTAModule);
begin // FI:W519
  // Nothing
end;

procedure TCompRenameNotifier.SourceEditorModified(const SourceEditor: IOTASourceEditor);
begin // FI:W519
  // Nothing
end;

{ TCompRenameExpert }

procedure TCompRenameExpert.AddNewClass(const AClassName: WideString);
begin
  Assert(Assigned(FRenameRuleList));

  // Classname already in list?
  if FRenameRuleList.IndexOfName(AClassName) < 0 then
  begin
    // No -> add "TClass=Class" as new entry
    FRenameRuleList.Add(AClassName+'='+Copy(AClassName, 2, Length(AClassName)-1));
    SaveSettings;
  end;
end;

procedure TCompRenameExpert.Execute(Sender: TObject);
var
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  i: Integer;
begin
  if not GxOtaFormEditorHasSelectedComponent then
    Configure
  else
  begin
    if not GxOtaTryGetCurrentFormEditor(FFormEditor) then
      Exit;
    SelCount := FFormEditor.GetSelCount;
    for i := 0 to SelCount - 1 do
    begin
      CurrentComponent := FFormEditor.GetSelComponent(i);
      if DoRename(CurrentComponent, False) = mrCancel then
        Break;
    end;
  end;

  IncCallCount;
end;

procedure TCompRenameExpert.Configure;
var
  Dialog: TfmCompRenameConfig;
begin
  Dialog := TfmCompRenameConfig.Create(nil);
  try
    Dialog.chkShowDialog.Checked := FShowDialog;
    Dialog.chkAutoAdd.Checked := FAutoAddClasses;
    SetFormIcon(Dialog);
    FRenameRuleList.Sort;
    Dialog.ValueList.Assign(FRenameRuleList);
    if Dialog.Execute then
    begin
      FShowDialog := Dialog.chkShowDialog.Checked;
      FAutoAddClasses := Dialog.chkAutoAdd.Checked;
      FRenameRuleList.Assign(Dialog.ValueList);
      SaveSettings;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TCompRenameExpert.ComponentRenamed(const FormEditor: IOTAFormEditor;
    Component: IOTAComponent; const OldName, NewName: WideString);
begin
  // Bug: Delphi 8 can not set string properties on components
  if RunningDelphi8 then
    Exit;

  if (not Assigned(FRenameRuleList)) or (FRenameRuleList.Count < 1) then
    Exit;
  if Active and Assigned(FormEditor) and (OldName = '') and (NewName > '') then
  begin
    // If the form being edited isn't the active designer, assume some
    // automated tool is doing the editing, and GExperts ignores the change
    if FormEditor.FileName <> GxOtaGetCurrentSourceFile then
      Exit;
    // Don't change the names of components that are not a default component
    // name based on the class.  This prevents renaming pasted components.
    if Assigned(Component) and IsDefaultComponentName(Component, NewName) then
    begin
      FComponentNames.Add(NewName);
      FFormNames.Add(FormEditor.FileName);
      FTimerCount := 0;
      FTimer.Enabled := True;
    end;
  end;
end;

constructor TCompRenameExpert.Create;
begin
  inherited Create;
  PrivateCompRenameExpert := Self;
  FComponentNames := TStringList.Create;
  FFormNames := TStringList.Create;
  FRenameRuleList := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := DoOnTimer;
  AddNotifier;
end;

destructor TCompRenameExpert.Destroy;
var
  i: Integer;
begin
  PrivateCompRenameExpert := nil;
  RemoveNotifier;
  FreeAndNil(FTimer);
  if Assigned(FRenameRuleList) then
  begin
    for i := 0 to FRenameRuleList.Count -1 do
    begin
      FRenameRuleList.Objects[i].Free;
      FRenameRuleList.Objects[i] := nil;
    end;
  end;
  FreeAndNil(FRenameRuleList);
  FreeAndNil(FComponentNames);
  FreeAndNil(FFormNames);
  inherited;
end;

function TCompRenameExpert.DoRename(const Component: IOTAComponent; UseRules: Boolean): TModalResult;
var
  i: Integer;
  Pipe1Pos: Integer;
  Pipe2Pos: Integer;
  PlaceHolderPos: Integer;
  ClassName: WideString;
  RenameRule: WideString;
  CompName: WideString;
  PropName: WideString;
  PropValue: String;
  UsePropValue: Boolean;
  SearchName: WideString;
  Reason: WideString;
  Dialog: TfmCompRename;
  ShowDialog: Boolean;
  OtherProps: TStringList;
  Index: Integer;
begin
  Assert(Assigned(Component));
  Assert(Assigned(FFormEditor));
  Result := mrOk;

  // Get the component class name
  ClassName := Component.GetComponentType;

  // Get the desired rename rule for this class
  RenameRule := GetClassRenameRule(ClassName);
  if RenameRule = '' then
  begin
    if FAutoAddClasses then
    begin
      AddNewClass(ClassName);
      RenameRule := GetClassRenameRule(ClassName);
    end;
  end;

  Pipe2Pos := -1;
  Pipe1Pos := Pos(WideString('|'), RenameRule);
  if Pipe1Pos > 0 then begin
    Delete(RenameRule, Pipe1Pos, 1);
    Pipe2Pos := LastCharPos(RenameRule, '|');
    if (Pipe2Pos >= Pipe1Pos) then
      Delete(RenameRule, Pipe2Pos, 1);
  end;

  PlaceHolderPos := Pos(WideString('%d'), RenameRule);
  if PlaceHolderPos > 0 then
    Delete(RenameRule, PlaceHolderPos, 2);

  CompName := GxOtaGetComponentName(Component);
  if CompName = '' then
    Exit;

  ShowDialog := FShowDialog;
  if not UseRules and (Length(RenameRule) > 0) then
  begin
    ShowDialog := True;
    UseRules := IsDefaultComponentName(Component, CompName);
  end;

  if not UseRules or (Length(RenameRule) > 0) then
  begin
    if ShowDialog or not UseRules then
    begin
      Dialog := TfmCompRename.Create(nil);
      try
        Dialog.OnIsValidComponentName := IsValidComponentName;
        Dialog.OldName := CompName;

        Dialog.SetAlign(Component);
        Dialog.SetAnchors(Component);

        Index := FRenameRuleList.IndexOfName(Component.GetComponentType);
        if Index <> -1 then
        begin
          OtherProps := FRenameRuleList.Objects[Index] as TStringList;
          if Assigned(OtherProps) then
          begin
            for i := 0 to OtherProps.Count - 1 do
            begin
              UsePropValue := False;
              PropName := OtherProps.Names[i];
              if PropName = '' then
                PropName := OtherProps[i];
              // Consolidate with code below
              if GxOtaPropertyExists(Component, PropName) then
              begin
                if UseRules then
                begin
                  PropValue := OtherProps.Values[PropName];
                  if PropValue <> '' then
                  begin
                    PropValue := AnsiDequotedStr(PropValue, #39);
                    if (PropValue= #39#39) then
                      PropValue := '';
                    UsePropValue := True;
                  end;
                end;
                if UsePropValue then
                  Dialog.AddComponentProperty(PropName, PropValue)
                else
                  Dialog.AddComponentProperty(PropName,
                    GxOtaGetComponentPropertyAsString(Component, PropName, True));
              end
              else
                Dialog.AddComponentProperty(PropName, SPropertyNotFound);
            end;
          end
        end
        else
          OtherProps := nil;

        if UseRules then
        begin
          Dialog.NewName := RenameRule;
          if Pipe1Pos > 0 then
            Dialog.SetRuleSelection(Pipe1Pos - 1, Pipe2Pos - 1)
          else
            Dialog.SetRuleSelection(Length(RenameRule), Length(RenameRule));
        end
        else
        begin
          Dialog.NewName := CompName;
          Dialog.SetRuleSelection(0, Length(CompName));
        end;

        Result := Dialog.Execute;
        if Result = mrOk then
        begin
          CompName := Dialog.NewName;
          GxOtaSetComponentName(Component, CompName);

          Dialog.GetAlign(Component);
          Dialog.GetAnchors(Component);

          if Assigned(OtherProps) then
          begin
            for i := 0 to OtherProps.Count - 1 do
            begin
              PropName := OtherProps.Names[i];
              if PropName = '' then
                PropName := OtherProps[i];
              if GxOtaPropertyExists(Component, PropName) then
                GxOtaSetComponentPropertyAsString(Component, PropName,
                  Dialog.GetComponentProperty(i));
            end;
          end;
        end;
      finally
        FreeAndNil(Dialog);
      end;
    end
    else
    begin
      // Try to find a new name without user interaction
      CompName := '';

      if PlaceHolderPos > 0 then
        Insert('%d', RenameRule, PlaceHolderPos)
      else
        RenameRule := RenameRule + '%d';

      for i := 1 to 100 do
      begin
        SearchName := Format(RenameRule, [i]);
        if GxOtaComponentsAreEqual(Component, FFormEditor.FindComponent(SearchName)) then
          Exit; // The component already matches the rename rule's result

        if IsValidComponentName(CompName, SearchName, Reason) then
        begin
          CompName := SearchName;
          Break;
        end;
      end;
      if Length(CompName) > 0 then
        GxOtaSetComponentName(Component, CompName);

      if UseRules then
      begin
        Index := FRenameRuleList.IndexOfName(Component.GetComponentType);
        if Index <> -1 then
        begin
          OtherProps := FRenameRuleList.Objects[Index] as TStringList;
          if Assigned(OtherProps) then
          begin
            for i := 0 to OtherProps.Count - 1 do
            begin
              PropName := OtherProps.Names[i];
              if (PropName <> '') and GxOtaPropertyExists(Component, PropName) then
              begin
                PropValue := OtherProps.Values[PropName];
                if PropValue <> '' then
                begin
                  PropValue := AnsiDequotedStr(PropValue, #39);
                  if (PropValue= #39#39) then
                    PropValue := '';
                  GxOtaSetComponentPropertyAsString(Component, PropName, PropValue);
                end;
              end;
            end;
          end
        end;
      end;
    end;
  end
end;

procedure TCompRenameExpert.DoOnTimer(Sender: TObject);
var
  i: Integer;
  FormName: WideString;
  SearchName: WideString;
  Component: IOTAComponent;
begin
  FTimer.Enabled := False;
  try
    if (GxOtaGetCurrentFormEditor = nil) then
      Exit;
    Assert(FFormNames.Count = FComponentNames.Count);
    for i := 0 to FComponentNames.Count - 1 do
    begin
      // Try to locate the component with given name
      FormName := FFormNames[i];
      SearchName := FComponentNames[i];
      if (FormName = '') or (SearchName = '') then
        Break;
      FFormEditor :=  GxOtaGetFormEditorForFileName(FormName);
      if not Assigned(FFormEditor) then
        Break;

      Component := FFormEditor.FindComponent(SearchName);
      if Assigned(Component) then
      begin
        if GxOtaIsInheritedComponent(Component) then
          Break;
        if DoRename(Component, True) = mrCancel then
          Break;
      end;
    end;
  finally
    FFormEditor := nil;
    Component := nil;
    FComponentNames.Clear;
    FFormNames.Clear;
  end;
end;

procedure TCompRenameExpert.FormEditorModified(const FormEditor: IOTAFormEditor);
begin // FI:W519
  // Nothing
end;

function TCompRenameExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Rename Components...';
begin
  Result := SMenuCaption;
end;

function TCompRenameExpert.GetClassRenameRule(const AClassName: WideString): WideString;
begin
  Assert(Assigned(FRenameRuleList));
  Result := FRenameRuleList.Values[AClassName];
end;

function TCompRenameExpert.GetDefaultShortCut: TShortCut;
begin
  Result := Menus.ShortCut(VK_F2, [ssShift]);
end;

class function TCompRenameExpert.GetName: string;
begin
  Result := 'RenameComponents';
end;

function TCompRenameExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TCompRenameExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

procedure TCompRenameExpert.InternalLoadSettings(Settings: TExpertSettings);
var
  OtherProps: TStringList;
  i: Integer;
begin
  inherited InternalLoadSettings(Settings);
  Assert(Assigned(FRenameRuleList));
  FShowDialog := Settings.ReadBool('ShowDialog', False);
  FAutoAddClasses := Settings.ReadBool('AutoAdd', True);
  if Settings.SectionExists('Items') then
    Settings.ReadStrings('Items', FRenameRuleList)
  else
    Settings.ReadStrings('', FRenameRuleList);
  FRenameRuleList.Sort;

  OtherProps := nil;
  for i := 0 to FRenameRuleList.Count - 1 do
  begin
    if not Assigned(OtherProps) then
      OtherProps := TStringList.Create;
    Settings.ReadStrings(FRenameRuleList.Names[i], OtherProps);
    if OtherProps.Count > 0 then
    begin
      FRenameRuleList.Objects[i] := OtherProps;
      OtherProps := nil;
    end;
  end;
  FreeAndNil(OtherProps);
end;

procedure TCompRenameExpert.InternalSaveSettings(Settings: TExpertSettings);
var
  i: Integer;
  OtherProps: TStringList;
  cnt: integer;
begin
  inherited InternalSaveSettings(Settings);
  Assert(Assigned(FRenameRuleList));

  if Settings.ValueExists('Count') then begin
    // clean up old entries that were directly in the section, we now write
    // to a subsection
    cnt := Settings.ReadInteger('Count', 0);
    for i := 0 to cnt - 1  do begin
      Settings.DeleteKey(Format('Item%d', [i]));
    end;
    Settings.DeleteKey('Count');
  end;

  Settings.WriteStrings('Items', FRenameRuleList);
  Settings.WriteBool('ShowDialog', FShowDialog);
  Settings.WriteBool('AutoAdd', FAutoAddClasses);

  for i := 0 to FRenameRuleList.Count - 1 do
  begin
    OtherProps := FRenameRuleList.Objects[i] as TStringList;
    if Assigned(OtherProps) then
      Settings.WriteStrings(FRenameRuleList.Names[i], OtherProps);
  end;
end;

function TCompRenameExpert.IsValidComponentName(const OldName, NewName: WideString; var Reason: WideString): Boolean;
resourcestring
  InalidIdent = 'Invalid identifier';
  DuplicateName = 'Duplicate name';
var
  FoundComponent: IOTAComponent;
  FoundName: WideString;
begin
  Reason := '';
  FoundName := '';
  Result := IsValidIdent(NewName);
  if not Result then
    Reason := InalidIdent;
  if Result and Assigned(FFormEditor) then
  begin
    FoundComponent := FFormEditor.FindComponent(NewName);
    if Assigned(FoundComponent) then
      FoundName := GxOtaGetComponentName(FoundComponent);
    Result := (not Assigned(FoundComponent)) or (FoundName = OldName);
    if not Result then
      Reason := DuplicateName;
  end;
end;

procedure TCompRenameExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      AddNotifier
    else
      RemoveNotifier;
  end;
end;

procedure TCompRenameExpert.AddNotifier;
begin
  if not Assigned(FCompRenameNotifier) then
    FCompRenameNotifier := TCompRenameNotifier.Create(Self);
end;

procedure TCompRenameExpert.RemoveNotifier;
begin
  if Assigned(FCompRenameNotifier) then
  begin
    FCompRenameNotifier.Detach;
    FCompRenameNotifier := nil;
  end;
end;

function TCompRenameExpert.IsDefaultComponentName(Component: IOTAComponent; const NewName: WideString): Boolean;
var
  Prefix: WideString;
  Suffix: WideString;
begin
  Assert(Assigned(Component));
  Result := False;
  Prefix := Component.GetComponentType;
  if (Length(Prefix) > 1) and (Prefix[1] = 'T') then
    Prefix := Copy(Prefix, 2, 999);
  if not StrBeginsWith(Prefix, NewName) then
    Exit;
  Suffix := Copy(NewName, Length(Prefix) + 1, 999);
  if StrToIntDef(Suffix, -999) <> -999 then
    Result := True;
end;

function TCompRenameExpert.IsDefaultActive: Boolean;
begin
  // IDE Bug: This expert does not work under Delphi 8
  Result := not RunningDelphi8;
end;

procedure TfmCompRename.FormCreate(Sender: TObject);
begin
  SetModalFormPopupMode(Self);
  lblReason.Font.Color := clRed;
end;

procedure TfmCompRename.btnSettingsClick(Sender: TObject);
begin
  Assert(Assigned(PrivateCompRenameExpert));
  PrivateCompRenameExpert.Configure;
end;

procedure TfmCompRename.HandleAlignButtons(_Align: TAlign);
var
  al: TAlign;
begin
  if FAlignButtons[_Align].Down then begin
    for al := Low(FAlignButtons) to High(FAlignButtons) do
      if al <> _Align then
        FAlignButtons[al].Down := False;
  end else
    FAlignButtons[_Align].Down := True;
end;

procedure TfmCompRename.b_AlignBottomClick(Sender: TObject);
begin
  HandleAlignButtons(alBottom);
end;

procedure TfmCompRename.b_AlignClientClick(Sender: TObject);
begin
  HandleAlignButtons(alClient);
end;

procedure TfmCompRename.b_AlignCustomClick(Sender: TObject);
begin
  HandleAlignButtons(alCustom);
end;

procedure TfmCompRename.b_AlignLeftClick(Sender: TObject);
begin
  HandleAlignButtons(alLeft);
end;

procedure TfmCompRename.b_AlignNoneClick(Sender: TObject);
begin
  HandleAlignButtons(alNone);
end;

procedure TfmCompRename.b_AlignRightClick(Sender: TObject);
begin
  HandleAlignButtons(alRight);
end;

procedure TfmCompRename.b_AlignTopClick(Sender: TObject);
begin
  HandleAlignButtons(alTop);
end;

procedure TfmCompRename.SetMargins(_Value: integer);
var
  s: string;
begin
  s := IntToStr(_Value);
  ed_MarginTop.Text := s;
  ed_MarginLeft.Text := s;
  ed_MarginRight.Text := s;
  ed_MarginBottom.Text := s;
end;

procedure TfmCompRename.b_Margins0Click(Sender: TObject);
begin
  SetMargins(0);
end;

procedure TfmCompRename.b_Margins3Click(Sender: TObject);
begin
  SetMargins(3);
end;

procedure TfmCompRename.b_Margins6Click(Sender: TObject);
begin
  SetMargins(6);
end;

procedure TfmCompRename.b_Margins8Click(Sender: TObject);
begin
  SetMargins(8);
end;

initialization
  RegisterGX_Expert(TCompRenameExpert);

end.

