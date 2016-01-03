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
  Controls,
  Menus,
  Buttons,
  ActnList,
  GX_IdeFormEnhancer,
  GX_dzVclUtils, Windows;

var
  gblAggressive: Boolean;

type
  TSearchPathEnhancer = class
  private
    FCallbackHandle: TFormChangeHandle;
    FIsAutocompleteEnabled: Boolean;
    FListbox: TListbox;
    FMemo: TMemo;
    FEdit: TEdit;
    FUpClick: TNotifyEvent;
    FDownClick: TNotifyEvent;
    FUpBtn: TBitBtn;
    FDownBtn: TBitBtn;
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
  ed: TEdit;
begin
  frm := Screen.ActiveCustomForm;
  if not IsSearchPathForm(frm) then
    Exit;
  if not TryGetElementEdit(frm, ed) then
    Exit;

  ed.Text := _Files[0];
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
      _btn := cmp as TBitBtn;
  end;

  function TryFindButton(const _BtnName: string; out _Btn: TButton): Boolean;
  var
    cmp: TComponent;
  begin
    cmp := _Form.FindComponent(_BtnName);
    Result := Assigned(cmp) and (cmp is TButton);
    if Result then
      _btn := cmp as TButton;
  end;

var
  cmp: TComponent;
  act_Up: TAction;
  act_Down: TAction;
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
    if Assigned(cmp) and (cmp is TListBox) then begin
      FListbox := TListBox(cmp);

      TheActionList := TActionList.Create(_Form);
      TheActionList.Name := 'TheActionList';

      // Assign shortcuts to the Up/Down buttons via actions
      if TryFindBitBtn('UpButton', FUpBtn) then begin
        // Unfortunately we can't just assign the button's OnClick event to the
        // OnExecute event of the action because Delphi apparently assigns the
        // same event to both buttons and then checks which one was pressed
        // by inspecting the Sender parameter. So, instead we save the OnClick
        // event, assign our own event to OnExecute and there call the original
        // OnClick event with the original Sender parameter.
        FUpClick := FUpBtn.OnClick;
        act_Up := TActionlist_Append(TheActionList, '', HandleUpButton, ShortCut(VK_UP, [ssCtrl]));
        FUpBtn.Action := act_Up;
      end;
      if TryFindBitBtn('DownButton', FDownBtn) then begin
        FDownClick := FDownBtn.OnClick;
        act_Down := TActionlist_Append(TheActionList, '', HandleDownButton, ShortCut(VK_DOWN, [ssCtrl]));
        FDownBtn.Action := act_Down;
      end;

      if not gblAggressive then begin
        TWinControl_ActivateDropFiles(FListbox, HandleFilesDropped);
      end else begin
        FMemo := TMemo.Create(_Form);
        FMemo.Parent := _Form;
        FMemo.BoundsRect := FListbox.BoundsRect;
        FMemo.Anchors := [akLeft, akTop, akRight, akBottom];
        FMemo.Lines.Text := FListbox.Items.Text;
        FMemo.OnChange := Self.HandleMemoChange;
        FListbox.Visible := False;
        TWinControl_ActivateDropFiles(FMemo, HandleFilesDropped);

        if TryFindButton('DeleteButton', btn) then begin
          // Does it still make sense to have a delete button?
          btn.Enabled := False;
        end;
        if TryFindButton('DeleteInvalidBtn', btn) then begin
          // todo: Figure out a way to implement this button
          // Maybe call the original event and copy the new listbox
          // content over to the memo?
          btn.Enabled := False;
        end;
        if TryFindButton('AddButton', btn) then begin
          btn.OnClick := HandleAddBtn;
        end;
        if TryFindButton('ReplaceButton', btn) then begin
          // Until I have figured out how to sensibly mark the current line in the memo
          // so the user knows what he is about to replace, I'll disable this button.
          btn.Enabled := False;
          btn.OnClick := nil; // HandleReplaceBtn;
        end;
      end;
      cmp := _Form.FindComponent('InvalidPathLbl');
      if cmp is TLabel then
        TLabel(cmp).Caption := TLabel(cmp).Caption + ' Drag and drop is enabled.';
    end;
  end;
end;

procedure TSearchPathEnhancer.HandleUpButton(_Sender: TObject);
var
  LineIdx: Integer;
  Pos: TPoint;
begin
  if Assigned(FMemo) then begin
    Pos := FMemo.CaretPos;
    LineIdx := Pos.Y;
    if LineIdx > 0 then
      FMemo.Lines.Exchange(LineIdx-1, LineIdx);
    FMemo.SetFocus;
    Pos.Y := Pos.Y -1 ;
    FMemo.CaretPos := Pos;
  end else
    FUpClick(FUpBtn);
end;

procedure TSearchPathEnhancer.HandleDownButton(_Sender: TObject);
var
  LineIdx: Integer;
begin
  if Assigned(FMemo) then begin
    LineIdx := FMemo.CaretPos.Y;
    if LineIdx < FMemo.Lines.Count - 1 then
      FMemo.Lines.Exchange(LineIdx, LineIdx + 1);
    FMemo.SetFocus;
  end else
    FDownClick(FDownBtn);
end;

procedure TSearchPathEnhancer.HandleAddBtn(_Sender: TObject);
begin
  if Assigned(FMemo) then
    FMemo.Lines.Add(FEdit.Text);
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
  if Assigned(FListbox)  and Assigned(FMemo) then
    FListbox.Items := FMemo.Lines;
end;

initialization
finalization
  gblAggressive := False;
  TGxIdeSearchPathEnhancer.SetEnabled(False);
end.
