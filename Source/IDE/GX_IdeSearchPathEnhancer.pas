unit GX_IdeSearchPathEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  StdCtrls,
  Forms;

type
  TGxIdeSearchPathEnhancer = class
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(_Value: Boolean); // static;
    class function GetAggressive: Boolean; // static;
    class procedure SetAggressive(_Value: Boolean); // static;
  end;

implementation

uses
  Windows,
  Controls,
  Menus,
  Buttons,
  ActnList,
  ComCtrls,
  GX_IdeFormEnhancer,
  GX_dzVclUtils,
  GX_dzFileUtils,
  StrUtils;

var
  gblAggressive: Boolean;

type
  TSearchPathEnhancer = class
  private
    FCallbackHandle: TFormChangeHandle;
    FIsAutocompleteEnabled: Boolean;
    FListbox: TListbox;
    FMemo: TMemo;

    FPageControl: TPageControl;
    FTabSheetList: TTabSheet;
    FTabSheetMemo: TTabSheet;

    FEdit: TEdit;
    FUpClick: TNotifyEvent;
    FDownClick: TNotifyEvent;
    FUpBtn: TBitBtn;
    FDownBtn: TBitBtn;
    FDeleteButton: TButton;
    FDeleteInvalidButton: TButton;
    FReplaceButton: TButton;
    FAddButton: TButton;
    FDeleteInvalidHandler: TNotifyEvent;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
    function IsSearchPathForm(_Form: TCustomForm): Boolean;
    function TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
    procedure HandleMemoChange(_Sender: TObject);
    procedure HandleUpButton(_Sender: TObject);
    procedure HandleDownButton(_Sender: TObject);
    procedure HandleAddBtn(_Sender: TObject);
    procedure PageControlChanging(_Sender: TObject; var AllowChange: Boolean);
    procedure HandleDeleteInvalid(_Sender: TObject);
    //procedure HandleReplaceBtn(_Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TheSearchPathEnhancer: TSearchPathEnhancer = nil;

{ TGxIdeSearchPathEnhancer }

class function TGxIdeSearchPathEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheSearchPathEnhancer);
end;

class procedure TGxIdeSearchPathEnhancer.SetEnabled(_Value: Boolean);
begin
  if not _Value then
    FreeAndNil(TheSearchPathEnhancer)
  else if not Assigned(TheSearchPathEnhancer) then
    TheSearchPathEnhancer := TSearchPathEnhancer.Create;
end;

class function TGxIdeSearchPathEnhancer.GetAggressive: Boolean;
begin
  Result := gblAggressive;
end;

class procedure TGxIdeSearchPathEnhancer.SetAggressive(_Value: Boolean);
begin
  gblAggressive := _Value;
end;

{ TSearchPathEnhancer }

constructor TSearchPathEnhancer.Create;
begin
  inherited Create;
  FCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TSearchPathEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FCallbackHandle);
  inherited;
end;

procedure TSearchPathEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
begin
  frm := Screen.ActiveCustomForm;
  if not IsSearchPathForm(frm) then
    Exit;

  if _Sender is TEdit then
    TEdit(_Sender).Text := _Files[0]
  else if _Sender is TMemo then
    TMemo(_Sender).Lines.AddStrings(_Files)
  else if _Sender is TListbox then
    TListbox(_Sender).Items.AddStrings(_Files);
end;

function TSearchPathEnhancer.TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
var
  cmp: TComponent;
begin
  Result := False;
  cmp := _Form.FindComponent('ElementEdit');
  if not Assigned(cmp) then
    Exit;
  if not (cmp is TEdit) then
    Exit;

  _ed := cmp as TEdit;
  Result := True;
end;

type
  TSearchPathDialogClassArr = array[0..2] of string;
const
  SearchPathDialogClassArr: TSearchPathDialogClassArr = (
    'TInheritedListEditDlg', 'TInheritedListEditDlg', 'TOrderedListEditDlg'
    );

{$IFDEF GX_VER220_up}
// Delphi XE and up
const
  SearchPathDialogName = 'InheritedListEditDlg';
  SearchPathDialogCaption = 'Search Path';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ELSE GX_VER220_up}
{$IFDEF GX_VER200_up}
// Delphi 2009 and up
const
  SearchPathDialogName = 'InheritedListEditDlg';
  SearchPathDialogCaption = 'Search path';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ELSE GX_VER200_up}
// Delphi 2007 and earlier
const
  SearchPathDialogName = 'OrderedListEditDlg';
  SearchPathDialogCaption = 'Directories';
  SearchPathDialogCaptionFR = 'Chemin de recherche';
  SearchPathDialogCaptionDE = 'Verzeichnisse';
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER220_up}

function TSearchPathEnhancer.IsSearchPathForm(_Form: TCustomForm): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(_Form) then
    Exit;
  for i := Low(SearchPathDialogClassArr) to High(SearchPathDialogClassArr) do begin
    Result := Result or SameText(_Form.ClassName, SearchPathDialogClassArr[i]);
  end;
  if not Result then
    Exit;
  Result := False;
  if not SameText(_Form.Name, SearchPathDialogName) then
    Exit;
  if not SameText(_Form.Caption, SearchPathDialogCaption)
    and not SameText(_Form.Caption, SearchPathDialogCaptionFR)
    and not SameText(_Form.Caption, SearchPathDialogCaptionDE) then
    Exit;
  Result := True;
end;

procedure TSearchPathEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
var
  TheActionList: TActionList;

  function TryFindBitBtn(const _BtnName: string; out _Btn: TBitBtn): Boolean;
  var
    cmp: TComponent;
  begin
    cmp := _Form.FindComponent(_BtnName);
    Result := Assigned(cmp) and (cmp is TBitBtn);
    if Result then
      _Btn := cmp as TBitBtn;
  end;

  function TryFindButton(const _BtnName: string; out _Btn: TButton): Boolean;
  var
    cmp: TComponent;
  begin
    cmp := _Form.FindComponent(_BtnName);
    Result := Assigned(cmp) and (cmp is TButton);
    if Result then
      _Btn := cmp as TButton
    else
      _Btn := nil;
  end;

  procedure AssignActionToButton(const _BtnName: string; const _Caption: string;
    _OnExecute: TNotifyEvent; _Shortcut: TShortCut; out _Btn: TBitBtn; out _OnClick: TNotifyEvent);
  var
    act: TAction;
  begin
    // Unfortunately we can't just assign the button's OnClick event to the
    // OnExecute event of the action because Delphi apparently assigns the
    // same event to both buttons and then checks which one was pressed
    // by inspecting the Sender parameter. So, instead we save the OnClick
    // event, assign our own event to OnExecute and there call the original
    // OnClick event with the original Sender parameter.
    if TryFindBitBtn(_BtnName, _Btn) then begin
      _OnClick := _Btn.OnClick;
      act := TActionlist_Append(TheActionList, '', _OnExecute, _Shortcut);
      act.Hint := _Caption;
      _Btn.Action := act;
      _Btn.ShowHint := True;
    end;
  end;

var
  cmp: TComponent;
  btn: TButton;
begin
  FListbox := nil;
  FMemo := nil;

  if not IsSearchPathForm(_Form) or not TryGetElementEdit(_Form, FEdit) then begin
    FIsAutocompleteEnabled := False;
    Exit;
  end;

  if not FIsAutocompleteEnabled and (_Form.FindComponent('TheActionList') = nil) then begin
    TEdit_ActivateAutoComplete(FEdit, [acsFileSystem], [actSuggest]);
    FIsAutocompleteEnabled := True;
    TWinControl_ActivateDropFiles(FEdit, HandleFilesDropped);

    cmp := _Form.FindComponent('CreationList');
    if Assigned(cmp) and (cmp is TListbox) then begin
      FListbox := TListbox(cmp);

      if gblAggressive then begin
        TheActionList := TActionList.Create(_Form);
        TheActionList.Name := 'TheActionList';

        // Assign shortcuts to the Up/Down buttons via actions
        AssignActionToButton('UpButton', 'Move Up', HandleUpButton, ShortCut(VK_UP, [ssCtrl]), FUpBtn, FUpClick);
        AssignActionToButton('DownButton', 'Move Down', HandleDownButton, ShortCut(VK_DOWN, [ssCtrl]), FDownBtn, FDownClick);

        TWinControl_ActivateDropFiles(FListbox, HandleFilesDropped);
        FPageControl := TPageControl.Create(_Form);
        FTabSheetList := TTabSheet.Create(_Form);
        FTabSheetMemo := TTabSheet.Create(_Form);
        FPageControl.Name := 'pc_PathList';
        FPageControl.Parent := _Form;
        FPageControl.BoundsRect := FListbox.BoundsRect;
        FPageControl.Anchors := [akLeft, akTop, akRight, akBottom];
        FPageControl.TabPosition := tpBottom;
        FPageControl.ActivePage := FTabSheetList;
        FPageControl.OnChanging := PageControlChanging;

        FTabSheetList.Name := 'ts_List';
        FTabSheetList.Parent := FPageControl;
        FTabSheetList.PageControl := FPageControl;
        FTabSheetList.Caption := 'List';

        FTabSheetMemo.Name := 'ts_Memo';
        FTabSheetMemo.Parent := FPageControl;
        FTabSheetMemo.PageControl := FPageControl;
        FTabSheetMemo.Caption := 'Memo';

        FMemo := TMemo.Create(_Form);
        FMemo.Parent := FTabSheetMemo;
        FMemo.Align := alClient;
        FMemo.Lines.Text := FListbox.Items.Text;
        FMemo.OnChange := Self.HandleMemoChange;
        FMemo.ScrollBars := ssBoth;
        FMemo.WordWrap := False;

        FListbox.Parent := FTabSheetList;
        FListbox.Align := alClient;

        TWinControl_ActivateDropFiles(FMemo, HandleFilesDropped);

        if TryFindButton('AddButton', FAddButton) then begin
          FAddButton.OnClick := HandleAddBtn;
        end;
        TryFindButton('ReplaceButton', FReplaceButton);
        TryFindButton('DeleteButton', FDeleteButton);
        if TryFindButton('DeleteInvalidBtn', FDeleteInvalidButton) then begin
          FDeleteInvalidHandler := FReplaceButton.OnClick;
          FReplaceButton.OnClick := HandleDeleteInvalid;
        end;

        if TryFindButton('OkButton', btn) then
          btn.Caption := '&OK';
      end;
      cmp := _Form.FindComponent('InvalidPathLbl');
      if cmp is TLabel then
        TLabel(cmp).Caption := TLabel(cmp).Caption + ' Drag and drop is enabled.';
    end;
  end;
end;

procedure TSearchPathEnhancer.HandleDeleteInvalid(_Sender: TObject);
var
  Dirs: TStringList;
  i: Integer;
  RecurseIdx: Integer;
begin
  if FPageControl.ActivePage = FTabSheetList then begin
    if Assigned(FDeleteInvalidHandler) then begin
//      MessageBox(0, 'delete invalid', '', MB_ICONINFORMATION or MB_OK);
      FDeleteInvalidHandler(_Sender);
    end;
  end else begin
    if FEdit.Text = '' then
      Exit;
    Dirs := TStringList.Create;
    try
      TSimpleDirEnumerator.EnumDirsOnly(FEdit.Text + '\*', Dirs, True);
      for i := Dirs.Count - 1 downto 0 do begin
        if StartsStr('.', Dirs[i]) then
          Dirs.Delete(i);
      end;
      RecurseIdx := 0;
      while RecurseIdx < Dirs.Count do begin
        TSimpleDirEnumerator.EnumDirsOnly(Dirs[RecurseIdx] + '\*', Dirs, True);
        for i := Dirs.Count - 1 downto 0 do begin
          if StartsStr('.', Dirs[i]) then
            Dirs.Delete(i);
        end;
        Inc(RecurseIdx);
      end;
      FMemo.Lines.AddStrings(Dirs);
    finally
      FreeAndNil(Dirs);
    end;
    MessageBox(0, 'add recursive', '', MB_ICONINFORMATION or MB_OK);
  end;
end;

procedure TSearchPathEnhancer.PageControlChanging(_Sender: TObject; var AllowChange: Boolean);

  procedure TrySetButtonVisibility(_Btn: TButton; _Visible: Boolean);
  begin
    if Assigned(_Btn) then
      _Btn.Visible := _Visible;
  end;

begin
  if FPageControl.ActivePage = FTabSheetList then begin
    FMemo.Lines.Text := FListbox.Items.Text;
    FMemo.CaretPos := Point(FMemo.CaretPos.X, FListbox.ItemIndex);
    TrySetButtonVisibility(FDeleteButton, False);
    if Assigned(FDeleteInvalidButton) then begin
      FDeleteInvalidButton.Caption := 'Add Recursive';
      FDeleteInvalidButton.OnClick := HandleDeleteInvalid;
    end;
    TrySetButtonVisibility(FReplaceButton, False);
  end else begin
    FListbox.ItemIndex := FMemo.CaretPos.Y;
    TrySetButtonVisibility(FDeleteButton, True);
    if Assigned(FDeleteInvalidButton) then begin
      FDeleteInvalidButton.Caption := 'Delete Invalid &Paths';
      FDeleteInvalidButton.OnClick := HandleDeleteInvalid;
    end;
    TrySetButtonVisibility(FReplaceButton, True);
  end;
end;

procedure TSearchPathEnhancer.HandleUpButton(_Sender: TObject);
var
  LineIdx: Integer;
  Pos: TPoint;
begin
  if FPageControl.ActivePage = FTabSheetMemo then begin
    Pos := FMemo.CaretPos;
    LineIdx := Pos.Y;
    if LineIdx > 0 then
      FMemo.Lines.Exchange(LineIdx - 1, LineIdx);
    FMemo.SetFocus;
    Pos.Y := Pos.Y - 1;
    FMemo.CaretPos := Pos;
  end else
    FUpClick(FUpBtn);
end;

procedure TSearchPathEnhancer.HandleDownButton(_Sender: TObject);
var
  LineIdx: Integer;
begin
  if FPageControl.ActivePage = FTabSheetMemo then begin
    LineIdx := FMemo.CaretPos.Y;
    if LineIdx < FMemo.Lines.Count - 1 then
      FMemo.Lines.Exchange(LineIdx, LineIdx + 1);
    FMemo.SetFocus;
  end else
    FDownClick(FDownBtn);
end;

procedure TSearchPathEnhancer.HandleAddBtn(_Sender: TObject);
begin
  if FPageControl.ActivePage = FTabSheetMemo then begin
    FMemo.Lines.Add(FEdit.Text);
  end else begin
    FListbox.Items.Add(FEdit.Text);
  end;
end;

{
procedure TSearchPathEnhancer.HandleReplaceBtn(_Sender: TObject);
var
  LineIdx: Integer;
begin
  if Assigned(FMemo) then begin
    LineIdx := FMemo.CaretPos.Y;
    FMemo.Lines[LineIdx] := FEdit.Text;
  end;
end;
}

procedure TSearchPathEnhancer.HandleMemoChange(_Sender: TObject);
begin
  if Assigned(FListbox) and Assigned(FMemo) then
    FListbox.Items := FMemo.Lines;
end;

initialization
finalization
  gblAggressive := False;
  TGxIdeSearchPathEnhancer.SetEnabled(False);
end.
