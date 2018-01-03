unit GX_IdeSearchPathEnhancer;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

type
  TGxIdeSearchPathEnhancer = class
  public
    class function GetEnabled: Boolean;
    class procedure SetEnabled(_Value: Boolean);
  end;

implementation

uses
  Windows,
  Messages,
  Controls,
  StdCtrls,
  Forms,
  Menus,
  Buttons,
  ActnList,
  StrUtils,
  ComCtrls,
  GX_IdeFormEnhancer,
  GX_dzVclUtils,
  GX_dzClassUtils,
  GX_dzFileUtils,
  GX_OtaUtils,
  GX_GenericUtils,
  GX_IdeFavoritesList,
  GX_ConfigurationInfo,
  GX_IdeSearchPathFavoriteEdit,
  GX_IdeDialogEnhancer;

type
  ///<summary>
  /// defines the strings used to identify the search path edit dialog </summary>
  TSearchPathDlgStrings = record
    DialogClass: string;
    DialogName: string;
    DialogCaptionEn: string;
    DialogCaptionFr: string;
    DialogCaptionDe: string;
  end;

type
  TSearchPathEnhancer = class(TIdeDialogEnhancer)
  private
    FForm: TCustomForm;
    FListbox: TListBox;
    FMemo: TMemo;

    FPageControl: TPageControl;
    FTabSheetList: TTabSheet;
    FTabSheetMemo: TTabSheet;

    FEdit: TEdit;
    FUpClick: TNotifyEvent;
    FDownClick: TNotifyEvent;
    FUpBtn: TCustomButton;
    FDownBtn: TCustomButton;
    FDeleteBtn: TCustomButton;
    FDeleteInvalidBtn: TCustomButton;
    FReplaceBtn: TCustomButton;
    FAddBtn: TCustomButton;
    FMakeRelativeBtn: TButton;
    FMakeAbsoluteBtn: TButton;
    FAddRecursiveBtn: TButton;
    FFavoritesPm: TPopupMenu;
    FFavoritesBtn: TButton;
    FFavorites: TStringList;
    FEnabled: Boolean;
    FAddDotsBtn: TButton;
    FDelDotsBtn: TButton;
{$IFNDEF GX_VER300_up}
    FBrowseBtn: TCustomButton;
    FBrowseClick: TNotifyEvent;
    procedure BrowseBtnClick(_Sender: TObject);
{$ENDIF GX_VER300_up}
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    function TryGetElementEdit(_Form: TCustomForm; out _ed: TEdit): Boolean;
    procedure HandleMemoChange(_Sender: TObject);
    procedure UpBtnClick(_Sender: TObject);
    procedure DownBtnClick(_Sender: TObject);
    procedure AddBtnClick(_Sender: TObject);
    procedure PageControlChanging(_Sender: TObject; var AllowChange: Boolean);
    procedure MakeRelativeBtnClick(_Sender: TObject);
    procedure MakeAbsoluteBtnClick(_Sender: TObject);
    procedure AddRecursiveBtnClick(_Sender: TObject);
    function MatchesDlg(_Form: TCustomForm; _Strings: TSearchPathDlgStrings): Boolean;
    procedure FavoritesBtnClick(_Sender: TObject);
    procedure FavoritesPmConfigureClick(_Sender: TObject);
    procedure InitFavoritesMenu;
    procedure FavoritesPmHandleFavoriteClick(_Sender: TObject);
    procedure LoadSettings;
    procedure SaveSettings;
    function ConfigurationKey: string;
    procedure PageControlChange(_Sender: TObject);
    procedure EditEntry(_Sender: TWinControl; var _Name, _Value: string; var _OK: Boolean);
    procedure AddDotsBtnClick(_Sender: TObject);
    procedure DelDotsBtnClick(_Sender: TObject);
    procedure GetSelectedMemoLines(out _StartIdx, _EndIdx: Integer);
    procedure SelectMemoLines(out _StartIdx, _EndIdx: Integer);
  protected
    function IsDesiredForm(_Form: TCustomForm): Boolean; override;
    procedure EnhanceForm(_Form: TForm); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  TheSearchPathEnhancer: TSearchPathEnhancer = nil;

{ TGxIdeSearchPathEnhancer }

class function TGxIdeSearchPathEnhancer.GetEnabled: Boolean;
begin
  Result := Assigned(TheSearchPathEnhancer) and TheSearchPathEnhancer.Enabled;
end;

class procedure TGxIdeSearchPathEnhancer.SetEnabled(_Value: Boolean);
begin
  if not _Value then begin
    if Assigned(TheSearchPathEnhancer) then
      TheSearchPathEnhancer.Enabled := False;
  end else begin
    if not Assigned(TheSearchPathEnhancer) then
      TheSearchPathEnhancer := TSearchPathEnhancer.Create;
    TheSearchPathEnhancer.Enabled := True;
  end;
end;

{ TSearchPathEnhancer }

constructor TSearchPathEnhancer.Create;
begin
  inherited Create;
  FFavorites := TStringList.Create;
  LoadSettings;
//  FFavorites.Values['jvcl'] := '..\libs\jvcl\common;..\libs\jvcl\run;..\libs\jvcl\resources';
//  FFavorites.Values['jcl'] := '..\libs\jcl\source\include;..\libs\jcl\source\include\jedi;..\libs\jcl\source\common;..\libs\jcl\source\windows;..\libs\jcl\source\vcl';
end;

destructor TSearchPathEnhancer.Destroy;
begin
  FreeAndNil(FFavorites);
  inherited;
end;

function TSearchPathEnhancer.ConfigurationKey: string;
begin
  Result := 'IDEEnhancements';
end;

procedure TSearchPathEnhancer.LoadSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    ExpSettings.ReadStrings('SearchPathFavorites', FFavorites);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TSearchPathEnhancer.SaveSettings;
var
  Settings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  Assert(ConfigInfo <> nil, 'No ConfigInfo found');

  // do not localize any of the below items
  ExpSettings := nil;
  Settings := TGExpertsSettings.Create;
  try
    ExpSettings := Settings.CreateExpertSettings(ConfigurationKey);
    ExpSettings.WriteStrings('SearchPathFavorites', FFavorites);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(Settings);
  end;
end;

procedure TSearchPathEnhancer.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
var
  frm: TCustomForm;
begin
  frm := Screen.ActiveCustomForm;
  if not IsDesiredForm(frm) then
    Exit;

  if _Sender is TEdit then
    TEdit(_Sender).Text := _Files[0]
  else if _Sender is TMemo then
    TMemo(_Sender).Lines.AddStrings(_Files)
  else if _Sender is TListBox then
    TListBox(_Sender).Items.AddStrings(_Files);
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

//  SearchPathDialogClassArr: TSearchPathDialogClassArr = (
//    'TInheritedListEditDlg', 'TInheritedListEditDlg', 'TOrderedListEditDlg'

{$IFDEF GX_VER300_up}
// Delphi 10 and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER300_up}
{$IFDEF GX_VER220_up}
// Delphi XE and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER220_up}
{$IFDEF GX_VER200_up}
// Delphi 2009 and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER200_up}
// Delphi 2007 and earlier
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER220_up}
{$ENDIF GX_VER300_up}

function TSearchPathEnhancer.MatchesDlg(_Form: TCustomForm; _Strings: TSearchPathDlgStrings): Boolean;
begin
  Result := False;
  if not SameText(_Form.ClassName, _Strings.DialogClass) then
    Exit; //==>
  if not SameText(_Form.Name, _Strings.DialogName) then
    Exit; //==>
  if not SameText(_Form.Caption, _Strings.DialogCaptionEn)
    and not SameText(_Form.Caption, _Strings.DialogCaptionFr)
    and not SameText(_Form.Caption, _Strings.DialogCaptionDe) then
    Exit;
  Result := True;
end;

function TSearchPathEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  if FEnabled then begin
    if Assigned(_Form) then begin
      Result := True;
      if MatchesDlg(_Form, ProjectSearchPathDlg) then
        Exit; //==>
      if MatchesDlg(_Form, LibrarySearchPathDlg) then
        Exit; //==>
    end;
  end;
  Result := False;
end;

type
  TCustomButtonHack = class(TCustomButton)
  end;

procedure TSearchPathEnhancer.EnhanceForm(_Form: TForm);
var
  TheActionList: TActionList;

  function TryFindButton(const _BtnName: string; out _Btn: TCustomButton): Boolean;
  begin
    Result := TComponent_FindComponent(_Form, _BtnName, True, TComponent(_Btn), TButton);
  end;

  procedure AssignActionToButton(const _BtnName: string; const _Caption: string;
    _OnExecute: TNotifyEvent; _Shortcut: TShortCut; out _Btn: TCustomButton; out _OnClick: TNotifyEvent);
  var
    act: TAction;
    cmp: TComponent;
  begin
    // Unfortunately we can't just assign the button's OnClick event to the
    // OnExecute event of the action because Delphi apparently assigns the
    // same event to both buttons and then checks which one was pressed
    // by inspecting the Sender parameter. So, instead we save the OnClick
    // event, assign our own event to OnExecute and there call the original
    // OnClick event with the original Sender parameter.
    _Btn := nil;
    _OnClick := nil;
    if TComponent_FindComponent(_Form, _BtnName, True, cmp) then begin
      if (cmp is TButton) or (cmp is TBitBtn) then begin
        _Btn := cmp as TCustomButton;
        _OnClick := TCustomButtonHack(_Btn).OnClick;
        act := TActionlist_Append(TheActionList, '', _OnExecute, _Shortcut);
        act.Hint := _Caption;
        _Btn.Action := act;
        _Btn.ShowHint := True;
      end;
    end;
  end;

var
  cmp: TComponent;
  btn: TCustomButton;
begin
  if not TryGetElementEdit(_Form, FEdit) then
    Exit;

  if _Form.FindComponent('GXTheActionList') = nil then begin
    FForm := _Form;
    TEdit_ActivateAutoComplete(FEdit, [acsFileSystem], [actSuggest]);
    TWinControl_ActivateDropFiles(FEdit, HandleFilesDropped);

    cmp := _Form.FindComponent('CreationList');
    if Assigned(cmp) and (cmp is TListBox) then begin
      FListbox := TListBox(cmp);

      TheActionList := TActionList.Create(_Form);
      TheActionList.Name := 'GXTheActionList';

        // Assign shortcuts to the Up/Down buttons via actions
      AssignActionToButton('UpButton', 'Move Up', UpBtnClick, ShortCut(VK_UP, [ssCtrl]), FUpBtn, FUpClick);
      AssignActionToButton('DownButton', 'Move Down', DownBtnClick, ShortCut(VK_DOWN, [ssCtrl]), FDownBtn, FDownClick);
{$IFNDEF GX_VER300_up} // RAD Studio 10 Seattle (24; BDS 17)
      // Delphi 10 and later no longer uses SelectDirectory, so we don't need to fix it.
      AssignActionToButton('BrowseButton', 'Browse', BrowseBtnClick, ShortCut(VK_DOWN, [ssAlt]), FBrowseBtn, FBrowseClick);
{$ENDIF GX_VER300_up}

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
      FPageControl.OnChange := PageControlChange;
      FPageControl.OnChanging := PageControlChanging;

      FTabSheetList.Name := 'ts_List';
      FTabSheetList.Parent := FPageControl;
      FTabSheetList.PageControl := FPageControl;
      FTabSheetList.Caption := '&List';

      FTabSheetMemo.Name := 'ts_Memo';
      FTabSheetMemo.Parent := FPageControl;
      FTabSheetMemo.PageControl := FPageControl;
      FTabSheetMemo.Caption := '&Memo';

      FMemo := TMemo.Create(_Form);
      FMemo.Parent := FTabSheetMemo;
      FMemo.Align := alClient;
      FMemo.HideSelection := False;
      FMemo.Lines.Text := FListbox.Items.Text;
      FMemo.OnChange := Self.HandleMemoChange;
      FMemo.ScrollBars := ssBoth;
      FMemo.WordWrap := False;

      FListbox.Parent := FTabSheetList;
      FListbox.Align := alClient;

      FDelDotsBtn := TButton.Create(_Form);
      FDelDotsBtn.Name := 'DelDotsBtn';
      FDelDotsBtn.Parent := _Form;
      FDelDotsBtn.Left := FPageControl.Left + FPageControl.Width - FDelDotsBtn.Width;
      FDelDotsBtn.Top := FPageControl.Top + FPageControl.Height;
      FDelDotsBtn.Anchors := [akRight, akBottom];
      FDelDotsBtn.Caption := 'Del ..\';
      FDelDotsBtn.OnClick := DelDotsBtnClick;

      FAddDotsBtn := TButton.Create(_Form);
      FAddDotsBtn.Name := 'AddDotsBtn';
      FAddDotsBtn.Parent := _Form;
      FAddDotsBtn.Left := FDelDotsBtn.Left - FAddDotsBtn.Width - 8;
      FAddDotsBtn.Top := FPageControl.Top + FPageControl.Height;
      FAddDotsBtn.Anchors := [akRight, akBottom];
      FAddDotsBtn.Caption := 'Add ..\';
      FAddDotsBtn.OnClick := AddDotsBtnClick;

      if Assigned(FUpBtn) then begin
        FFavoritesBtn := TButton.Create(_Form);
        FFavoritesBtn.Parent := _Form;
        FFavoritesBtn.Left := FUpBtn.Left;
        FFavoritesBtn.Top := FPageControl.Top;
        FFavoritesBtn.Width := FUpBtn.Width;
        FFavoritesBtn.Height := FUpBtn.Height;
        FFavoritesBtn.Anchors := [akRight, akTop];
        FFavoritesBtn.Caption := '&Fav';
        FFavoritesBtn.OnClick := FavoritesBtnClick;
        FFavoritesBtn.TabOrder := FPageControl.TabOrder + 1;
        FFavoritesBtn.Visible := False;
        FFavoritesPm := TPopupMenu.Create(_Form);
        InitFavoritesMenu;
      end;

      TWinControl_ActivateDropFiles(FMemo, HandleFilesDropped);

      if TryFindButton('AddButton', FAddBtn) then begin
        TCustomButtonHack(FAddBtn).OnClick := AddBtnClick;
      end;
      if TryFindButton('ReplaceButton', FReplaceBtn) then begin
        FMakeRelativeBtn := TButton.Create(_Form);
        FMakeRelativeBtn.Name := 'MakeRelativeBtn';
        FMakeRelativeBtn.Parent := FReplaceBtn.Parent;
        FMakeRelativeBtn.BoundsRect := FReplaceBtn.BoundsRect;
        FMakeRelativeBtn.Anchors := [akRight, akBottom];
        FMakeRelativeBtn.Caption := 'Make Relative';
        FMakeRelativeBtn.Visible := False;
        FMakeRelativeBtn.OnClick := MakeRelativeBtnClick;
      end;
      if TryFindButton('DeleteButton', FDeleteBtn) then begin
        FMakeAbsoluteBtn := TButton.Create(_Form);
        FMakeAbsoluteBtn.Name := 'MakeAbsoluteBtn';
        FMakeAbsoluteBtn.Parent := FDeleteBtn.Parent;
        FMakeAbsoluteBtn.BoundsRect := FDeleteBtn.BoundsRect;
        FMakeAbsoluteBtn.Anchors := [akRight, akBottom];
        FMakeAbsoluteBtn.Caption := 'Make Absolute';
        FMakeAbsoluteBtn.Visible := False;
        FMakeAbsoluteBtn.OnClick := MakeAbsoluteBtnClick;
      end;
      if TryFindButton('DeleteInvalidBtn', FDeleteInvalidBtn) then begin
        FAddRecursiveBtn := TButton.Create(_Form);
        FAddRecursiveBtn.Name := 'AddRecursiveBtn';
        FAddRecursiveBtn.Parent := FDeleteInvalidBtn.Parent;
        FAddRecursiveBtn.BoundsRect := FDeleteInvalidBtn.BoundsRect;
        FAddRecursiveBtn.Anchors := [akRight, akBottom];
        FAddRecursiveBtn.Caption := 'Add Recursive';
        FAddRecursiveBtn.Visible := False;
        FAddRecursiveBtn.OnClick := AddRecursiveBtnClick;
      end;

      if TryFindButton('OkButton', btn) then
        TCustomButtonHack(btn).Caption := '&OK';

      cmp := _Form.FindComponent('InvalidPathLbl');
      if cmp is TLabel then
        TLabel(cmp).Caption := TLabel(cmp).Caption + ' Drag and drop is enabled.';
    end;
  end;
end;

procedure TSearchPathEnhancer.GetSelectedMemoLines(out _StartIdx, _EndIdx: Integer);
var
  SelStart: Integer;
  SelEnd: Integer;
begin
  SendMessage(FMemo.Handle, EM_GETSEL, Longint(@SelStart), Longint(@SelEnd));
  _StartIdx := SendMessage(FMemo.Handle, EM_LINEFROMCHAR, SelStart, 0);
  _EndIdx := SendMessage(FMemo.Handle, EM_LINEFROMCHAR, SelEnd - 1, 0);
end;

procedure TSearchPathEnhancer.SelectMemoLines(out _StartIdx, _EndIdx: Integer);
var
  SelStart: Integer;
  SelEnd: Integer;
begin
  SelStart := SendMessage(FMemo.Handle, EM_LINEINDEX, _StartIdx, 0);
  SelEnd := SendMessage(FMemo.Handle, EM_LINEINDEX, _EndIdx + 1, 0);
  SendMessage(FMemo.Handle, EM_SETSEL, SelStart, SelEnd);
end;

procedure TSearchPathEnhancer.AddDotsBtnClick(_Sender: TObject);
var
  i: Integer;
  s: string;
  sl: TStringList;
  StartIdx: Integer;
  EndIdx: Integer;
begin
  sl := TStringList.Create;
  try
    GetSelectedMemoLines(StartIdx, EndIdx);
    sl.Assign(FMemo.Lines);
    for i := StartIdx to EndIdx do begin
      s := sl[i];
      s := '..\' + s;
      sl[i] := s;
    end;
    FMemo.Lines.Assign(sl);
    SelectMemoLines(StartIdx, EndIdx);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TSearchPathEnhancer.DelDotsBtnClick(_Sender: TObject);
var
  i: Integer;
  s: string;
  sl: TStringList;
  StartIdx: Integer;
  EndIdx: Integer;
begin
  sl := TStringList.Create;
  try
    GetSelectedMemoLines(StartIdx, EndIdx);
    sl.Assign(FMemo.Lines);
    for i := StartIdx to EndIdx do begin
      s := sl[i];
      if LeftStr(s, 3) = '..\' then begin
        s := Copy(s, 4, MaxInt);
        sl[i] := s;
      end;
    end;
    FMemo.Lines.Assign(sl);
    SelectMemoLines(StartIdx, EndIdx);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TSearchPathEnhancer.InitFavoritesMenu;
var
  i: Integer;
  mi: TMenuItem;
  FavName: string;
begin
  FFavoritesPm.Items.Clear;
  for i := 0 to FFavorites.Count - 1 do begin
    FavName := FFavorites.Names[i];
    mi := TPopupMenu_AppendMenuItem(FFavoritesPm, FavName, FavoritesPmHandleFavoriteClick);
    mi.Tag := i + 1;
  end;
  TPopupMenu_AppendMenuItem(FFavoritesPm, 'Configure ...', FavoritesPmConfigureClick);
end;

procedure TSearchPathEnhancer.FavoritesBtnClick(_Sender: TObject);
var
  pnt: TPoint;
begin
  pnt := FFavoritesBtn.ClientToScreen(Point(0, FFavoritesBtn.Height));
  FFavoritesPm.Popup(pnt.X, pnt.Y);
end;

procedure TSearchPathEnhancer.FavoritesPmConfigureClick(_Sender: TObject);
resourcestring
  SFavSearchPaths = 'Favorite Search Paths';
begin
  Tf_GxIdeFavoritesList.Execute(FForm, SFavSearchPaths, EditEntry, FFavorites);
  SaveSettings;
  InitFavoritesMenu;
end;

procedure TSearchPathEnhancer.EditEntry(_Sender: TWinControl;
  var _Name, _Value: string; var _OK: Boolean);
begin
  _OK := Tf_IdeSearchPathFavoriteEdit.Execute(_Sender, _Name, _Value)
end;

procedure TSearchPathEnhancer.FavoritesPmHandleFavoriteClick(_Sender: TObject);
var
  mi: TMenuItem;
  sl: TStringList;
  FavName: string;
begin
  mi := _Sender as TMenuItem;
  sl := TStringList.Create;
  try
    FavName := FFavorites.Names[mi.Tag - 1];
    sl.Delimiter := ';';
    sl.DelimitedText := FFavorites.Values[FavName];
    if FPageControl.ActivePage = FTabSheetList then
      FListbox.Items.AddStrings(sl)
    else
      FMemo.Lines.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TSearchPathEnhancer.MakeAbsoluteBtnClick(_Sender: TObject);
var
  i: Integer;
  ProjectFile: string;
  ProjectDir: string;
  AbsoluteDir: string;
  RelativeDir: string;
begin
  ProjectFile := GxOtaGetCurrentProjectFileName(False);
  if ProjectFile = '' then
    Exit; //==>
  ProjectDir := ExtractFilePath(ProjectFile);
  FMemo.Lines.BeginUpdate;
  try
    for i := 0 to FMemo.Lines.Count - 1 do begin
      RelativeDir := FMemo.Lines[i];
      if not StrBeginsWith('$(BDS)\', RelativeDir, False) then begin
        AbsoluteDir := TFileSystem.ExpandFileNameRelBaseDir(RelativeDir, ProjectDir);
        FMemo.Lines[i] := AbsoluteDir;
      end;
    end;
    RelativeDir := FEdit.Text;
    if not StrBeginsWith('$(BDS)\', RelativeDir, False) then begin
      AbsoluteDir := TFileSystem.ExpandFileNameRelBaseDir(RelativeDir, ProjectDir);
      FEdit.Text := AbsoluteDir;
    end;
  finally
    FMemo.Lines.EndUpdate;
  end;
end;

procedure TSearchPathEnhancer.MakeRelativeBtnClick(_Sender: TObject);
var
  i: Integer;
  ProjectFile: string;
  ProjectDir: string;
  AbsoluteDir: string;
  RelativeDir: string;
begin
  ProjectFile := GxOtaGetCurrentProjectFileName(True);
  if ProjectFile = '' then
    Exit; //==>
  ProjectDir := ExtractFilePath(ProjectFile);
  FMemo.Lines.BeginUpdate;
  try
    for i := 0 to FMemo.Lines.Count - 1 do begin
      AbsoluteDir := FMemo.Lines[i];
      if not StrBeginsWith('$(BDS)\', AbsoluteDir, False) then begin
        RelativeDir := ExtractRelativePath(IncludeTrailingPathDelimiter(ProjectDir), AbsoluteDir);
        FMemo.Lines[i] := RelativeDir;
      end;
    end;
    AbsoluteDir := FEdit.Text;
    if not StrBeginsWith('$(BDS)\', AbsoluteDir, False) then begin
      RelativeDir := ExtractRelativePath(IncludeTrailingPathDelimiter(ProjectDir), AbsoluteDir);
      FEdit.Text := RelativeDir;
    end;
  finally
    FMemo.Lines.EndUpdate;
  end;
end;

procedure TSearchPathEnhancer.AddRecursiveBtnClick(_Sender: TObject);
var
  Dirs: TStringList;
  i: Integer;
  RecurseIdx: Integer;
begin
  if FEdit.Text = '' then
    Exit;
  Dirs := TStringList.Create;
  try
    TSimpleDirEnumerator.EnumDirsOnly(FEdit.Text + '\*', Dirs, True);
    for i := Dirs.Count - 1 downto 0 do begin
      if AnsiStartsStr('.', Dirs[i]) then
        Dirs.Delete(i);
    end;
    RecurseIdx := 0;
    while RecurseIdx < Dirs.Count do begin
      TSimpleDirEnumerator.EnumDirsOnly(Dirs[RecurseIdx] + '\*', Dirs, True);
      for i := Dirs.Count - 1 downto 0 do begin
        if AnsiStartsStr('.', Dirs[i]) then
          Dirs.Delete(i);
      end;
      Inc(RecurseIdx);
    end;
    FMemo.Lines.AddStrings(Dirs);
  finally
    FreeAndNil(Dirs);
  end;
end;

procedure TSearchPathEnhancer.PageControlChanging(_Sender: TObject; var AllowChange: Boolean);
var
  SwitchingToMemo: Boolean;
begin
  SwitchingToMemo := (FPageControl.ActivePage = FTabSheetList);
  if SwitchingToMemo then begin
    FMemo.Lines.Text := FListbox.Items.Text;
    FMemo.CaretPos := Point(FMemo.CaretPos.X, FListbox.ItemIndex);
  end else begin
    FListbox.ItemIndex := FMemo.CaretPos.Y;
  end;
end;

procedure TSearchPathEnhancer.PageControlChange(_Sender: TObject);

  procedure TrySetButtonVisibility(_Btn: TCustomButton; _Visible: Boolean);
  begin
    if Assigned(_Btn) then
      _Btn.Visible := _Visible;
  end;

var
  SwitchedToMemo: Boolean;
begin
  SwitchedToMemo := (FPageControl.ActivePage = FTabSheetMemo);

  TrySetButtonVisibility(FFavoritesBtn, SwitchedToMemo);
  TrySetButtonVisibility(FDeleteBtn, not SwitchedToMemo);
  TrySetButtonVisibility(FMakeAbsoluteBtn, SwitchedToMemo);
  TrySetButtonVisibility(FDeleteInvalidBtn, not SwitchedToMemo);
  TrySetButtonVisibility(FAddRecursiveBtn, SwitchedToMemo);
  TrySetButtonVisibility(FReplaceBtn, not SwitchedToMemo);
  TrySetButtonVisibility(FMakeRelativeBtn, SwitchedToMemo);
end;

procedure TSearchPathEnhancer.UpBtnClick(_Sender: TObject);
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

procedure TSearchPathEnhancer.DownBtnClick(_Sender: TObject);
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

procedure TSearchPathEnhancer.AddBtnClick(_Sender: TObject);
var
  Idx: Integer;
begin
  if FPageControl.ActivePage = FTabSheetMemo then begin
    FMemo.Lines.Add(FEdit.Text);
  end else begin
    Idx := FListbox.Items.Add(FEdit.Text);
    // In order to prevent adding the path twice, we need to select the new entry and
    // call the OnClick event.
    FListbox.ItemIndex := Idx;
    FListbox.OnClick(FListbox);
  end;
end;

{$IFNDEF GX_VER300_up}

procedure TSearchPathEnhancer.BrowseBtnClick(_Sender: TObject);
var
  ProjectFile: string;
  ProjectDir: string;
  Dir: string;
begin
  Dir := FEdit.Text;
  ProjectFile := GxOtaGetCurrentProjectFileName(True);
  if ProjectFile <> '' then begin
    ProjectDir := ExtractFilePath(ProjectFile);
    Dir := TFileSystem.ExpandFileNameRelBaseDir(Dir, ProjectDir);
  end;
  if GetDirectory(FBrowseBtn.Hint, Dir, FBrowseBtn) then
    FEdit.Text := Dir;
end;
{$ENDIF GX_VER300_up}

procedure TSearchPathEnhancer.HandleMemoChange(_Sender: TObject);
begin
  if Assigned(FListbox) and Assigned(FMemo) then
    FListbox.Items := FMemo.Lines;
end;

initialization
finalization
  FreeAndNil(TheSearchPathEnhancer);
end.
