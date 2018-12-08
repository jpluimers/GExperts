unit GX_eIfDef;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  Grids,
  ExtCtrls,
  Graphics,
  ActnList,
  Actions,
  Menus,
  Messages,
  Contnrs,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_dzCompilerAndRtlVersions,
  GX_BaseForm,
  GX_BaseExpert,
  GX_GenericUtils;

{$IF RTLVersion <= RtlVersionDelphi2005}
// Delphi < 2006 does not have the MouseLeave event so we implement it via
// an interposer class using http://stackoverflow.com/a/3182185/49925
type
  TPageControl = class(ComCtrls.TPageControl)
  private
    FMouseTracking: Boolean;
    FOnMouseLeave: TNotifyEvent;
    procedure WMMouseLeave(var Msg: TMessage); message WM_MOUSELEAVE;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;
{$IFEND}

type
  TIfDefExpert = class(TEditorExpert)
  private
    FAppendComment: Boolean;
  protected
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDisplayName: string; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

type
  TfmConfigureIfDef = class(TfmBaseForm)
    pc_IfClasses: TPageControl;
    p_Bottom: TPanel;
    b_OK: TButton;
    b_Cancel: TButton;
    chk_AppendComment: TCheckBox;
    b_Open: TButton;
    b_Add: TButton;
    TheActionList: TActionList;
    act_Open: TAction;
    act_Add: TAction;
    pm_IncludeFiles: TPopupMenu;
    procedure pc_IfClassesChange(Sender: TObject);
    procedure chk_AppendCommentClick(Sender: TObject);
    procedure act_OpenExecute(Sender: TObject);
    procedure act_AddExecute(Sender: TObject);
    procedure pc_IfClassesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pc_IfClassesMouseLeave(Sender: TObject);
  private
    FText: string;
    FSearchPath: TStringList;
    FFindThread: TFileFindThread;
    FTabDefinitions: TObjectList;
    procedure InitCompilerVersion;
    procedure InitVerXxx;
    procedure InitOptions;
    procedure InitRtlVersion;
    procedure InitIncludes;
    procedure IncludeFilesListReady;
    procedure OnInludeFileSelected(_Sender: TObject);
    function AddIncludePage(_No: Integer; const _FullFn: string; _IsIncluded: Boolean): TTabSheet;
  public
    class function Execute(_bmp: TBitmap; var _AppendComment: Boolean;
      out _Text: string; out _IncludeFile: string): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    function GetIncludeFile: string;
  end;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  StrUtils,
  GX_OtaUtils,
  GX_dzVclUtils;

{ TIfDefExpert }

constructor TIfDefExpert.Create;
begin
  inherited Create;
end;

procedure TIfDefExpert.Execute(Sender: TObject);
var
  InsertString: string;
  IncFile: string;
  Lines: TGXUnicodeStringList;
  i: Integer;
  s: string;
  CurPos: TOTAEditPos;
  EditPos: TOTAEditPos;
begin
  if not TfmConfigureIfDef.Execute(GetBitmap, FAppendComment, InsertString, IncFile) then
    Exit; //==>

  IncCallCount;

  GxOtaInsertLineIntoEditor(InsertString);

  if IncFile <> '' then begin
    // if an include file is necessary for the ifdef, add the include file at the beginning
    CurPos := GxOtaGetCurrentEditPos();
    Lines := TGXUnicodeStringList.Create;
    try
      if not GxOtaGetActiveEditorText(Lines, False) then
        Exit; //==>
      for i := 0 to Lines.Count - 1 do begin
        s := Lines[i];
        s := Trim(s);
        if SameText(s, 'INTERFACE') then begin
          EditPos.Col := 1;
          EditPos.Line := i + 2; // +1 for index -> number, +1 for next line
          GxOtaGotoEditPos(EditPos);
          GxOtaInsertLineIntoEditor('{$I ' + IncFile + '}' + CRLF);
          if CurPos.Line > EditPos.Line then
            Inc(CurPos.Line);
          GxOtaGotoEditPos(CurPos);
          Exit; //==>
        end;
      end;
    finally
      FreeAndNil(Lines);
    end;
  end;
end;

function TIfDefExpert.GetDisplayName: string;
resourcestring
  SIfDefExpertName = 'IF directive';
begin
  Result := SIfDefExpertName;
end;

function TIfDefExpert.GetHelpString: string;
resourcestring
  SIfDefExpertHelp =
    '  This expert inserts a {$IF/IFDEF/IFNDEF} directive into the source code';
begin
  Result := SIfDefExpertHelp;
end;

class function TIfDefExpert.GetName: string;
begin
  Result := 'IfDef';
end;

function TIfDefExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

procedure TIfDefExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);

  // Do not localize any of the below items
  FAppendComment := Settings.ReadBool('AppendComment', False);
end;

procedure TIfDefExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);

  // Do not localize any of the below items
  Settings.WriteBool('AppendComment', FAppendComment);
end;

{ TfmConfigureIfDef }

class function TfmConfigureIfDef.Execute(_bmp: TBitmap; var _AppendComment: Boolean;
  out _Text: string; out _IncludeFile: string): Boolean;
var
  frm: TfmConfigureIfDef;
begin
  frm := TfmConfigureIfDef.Create(Application);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.chk_AppendComment.Checked := _AppendComment;
    Result := frm.ShowModal = mrOk;
    if Result then begin
      _AppendComment := frm.chk_AppendComment.Checked;
      _Text := frm.FText;
      _IncludeFile := frm.GetIncludeFile;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmConfigureIfDef.Create(_Owner: TComponent);
var
  r: TRect;
begin
  inherited;

  p_Bottom.BevelOuter := bvNone;

  TControl_SetMinConstraints(Self);

  pc_IfClasses.OnMouseLeave := pc_IfClassesMouseLeave;

  FSearchPath := TStringList.Create;
  GxOtaGetEffectiveLibraryPath(FSearchPath);

  FTabDefinitions := TObjectList.Create;

  FFindThread := TFileFindThread.Create;
  FFindThread.FileMasks.Add('*.inc');
  FFindThread.SearchDirs.Assign(FSearchPath);
  FFindThread.OnFindComplete := IncludeFilesListReady;
  FFindThread.StartFind;

  InitCompilerVersion;
  InitRtlVersion;
  InitVerXxx;
  InitOptions;
  InitIncludes;

  // tabheight = pc_IfClasses.Height - pc_IfClasses.TabRect.height - tabsheet1.borderwidth;
  r := pc_IfClasses.TabRect(0);
  b_Add.Height := r.Bottom - r.Top;
  b_Add.Top := r.Top;
  b_Add.Left := pc_IfClasses.Width - b_Add.Width;

  pc_IfClassesChange(pc_IfClasses);
end;

destructor TfmConfigureIfDef.Destroy;
begin
  if Assigned(FFindThread) then begin
    FFindThread.OnFindComplete := nil;
    FFindThread.Terminate;
  end;
  FreeAndNil(FFindThread);
  FreeAndNil(FSearchPath);
  FreeAndNil(FTabDefinitions);
  inherited;
end;

{ TIfdefTabDefinition }

type
  TIfdefTabDefinition = class
  private
    FRow: Integer;
  protected
    FForm: TfmConfigureIfDef;
    FTabSheet: TTabSheet;
    FStringGrid: TStringGrid;
    FPanel: TPanel;
    FEdit: TEdit;
    FSelStart: Integer;
    FSelLen: Integer;
    FTextFormatStr: string;
    FFilename: string;
    FIsIncluded: Boolean;
    procedure UpdateEditText(_Row: Integer);
    procedure HandleEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleSelectCell(_Sender: TObject; _Col, _Row: Integer; var _CanSelect: Boolean);
    procedure HandleEditEnter(_Sender: TObject);
    procedure HandleEditChange(_Sender: TObject);
  public
    constructor Create(_Form: TfmConfigureIfDef; _pc: TPageControl; const _Caption: string;
      _SelStart, _SelLen: Integer; const _TextFormatStr: string;
      _IsIncluded: Boolean = True; const _Filename: string = '');
    procedure InitEvents;
    procedure AddGridRow(const _Value, _Desc: string; _LineNo: Integer = 0);
    function GetLineNoOfCurrentEntry: Integer;
    property Filename: string read FFilename;
    property StringGrid: TStringGrid read FStringGrid;
    property Edit: TEdit read FEdit;
    property IsIncluded: Boolean read FIsIncluded;
  end;

constructor TIfdefTabDefinition.Create(_Form: TfmConfigureIfDef; _pc: TPageControl;
  const _Caption: string; _SelStart, _SelLen: Integer; const _TextFormatStr: string;
  _IsIncluded: Boolean = True; const _Filename: string = '');
begin
  FForm := _Form;
  FSelStart := _SelStart;
  FSelLen := _SelLen;
  FTextFormatStr := _TextFormatStr;
  FFilename := _Filename;
  FIsIncluded := _IsIncluded;
  FRow := 0;

  FTabSheet := TTabSheet.Create(_Form);
  FTabSheet.Name := '';
  FTabSheet.Parent := _pc;
  FTabSheet.PageControl := _pc;
  FTabSheet.Caption := _Caption + ' ';
  if FFilename <> '' then
    FTabSheet.Hint := FFilename;

  // this is how the PageControl knows how to deal with this tab
  FTabSheet.Tag := Integer(Self);

  FStringGrid := TStringGrid.Create(_Form);
  FStringGrid.Name := '';
  FStringGrid.Parent := FTabSheet;
  FStringGrid.Align := alClient;
  FStringGrid.ColCount := 2;
  FStringGrid.FixedCols := 0;
  FStringGrid.FixedRows := 0;
  FStringGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect];
  FStringGrid.TabOrder := 0;
  FStringGrid.ColWidths[0] := 50;
  FStringGrid.ColWidths[1] := 200;

  FPanel := TPanel.Create(_Form);
  FPanel.Name := '';
  FPanel.Parent := FTabSheet;
  FPanel.Align := alBottom;
  FPanel.BevelOuter := bvNone;
  FPanel.Height := 25;
  FPanel.TabOrder := 1;

  FEdit := TEdit.Create(_Form);
  FEdit.Name := '';
  FEdit.Parent := FPanel;
  FEdit.Align := alClient;
  FEdit.TabOrder := 0;
end;

function TIfdefTabDefinition.GetLineNoOfCurrentEntry: Integer;
var
  r: Integer;
begin
  r := FStringGrid.Row;
  if (r >= FStringGrid.FixedRows) and (r < FStringGrid.RowCount) then
    Result := GXNativeInt(FStringGrid.Objects[0, r])
  else
    Result := 0;
end;

procedure TIfdefTabDefinition.InitEvents;
begin
  TGrid_Resize(FStringGrid, [roUseGridWidth, roUseAllRows]);
  FStringGrid.OnSelectCell := HandleSelectCell;
  FEdit.OnChange := HandleEditChange;
  FEdit.OnEnter := HandleEditEnter;
  FEdit.OnKeyDown := HandleEditKeyDown;

  UpdateEditText(FStringGrid.Row);
end;

procedure TIfdefTabDefinition.AddGridRow(const _Value, _Desc: string; _LineNo: Integer);
begin
  FStringGrid.RowCount := FRow + 1;
  FStringGrid.Cells[0, FRow] := _Value;
  FStringGrid.Cells[1, FRow] := _Desc;
  FStringGrid.Objects[0, FRow] := Pointer(_LineNo);
  Inc(FRow);
end;

procedure TIfdefTabDefinition.UpdateEditText(_Row: Integer);
var
  CellText: string;
  SelText: string;
  NewText: string;
begin
  CellText := FStringGrid.Cells[0, _Row];

  SelText := FEdit.SelText;
  NewText := Format(FTextFormatStr, [CellText]);
  if FForm.chk_AppendComment.Checked then begin
    CellText := FStringGrid.Cells[1, _Row];
    NewText := NewText + ' // ' + CellText;
  end;
  FEdit.Text := NewText;
  FEdit.SelStart := FSelStart;
  FEdit.SelLength := FSelLen;
  if SelText <> '' then begin
    FEdit.SelText := SelText;
    FEdit.SelStart := FSelStart;
    FEdit.SelLength := FSelLen;
  end;
end;

procedure TIfdefTabDefinition.HandleSelectCell(_Sender: TObject; _Col, _Row: Integer;
  var _CanSelect: Boolean);
begin
  UpdateEditText(_Row);
end;

procedure TIfdefTabDefinition.HandleEditChange(_Sender: TObject);
begin
  FForm.FText := FEdit.Text;
end;

procedure TIfdefTabDefinition.HandleEditEnter(_Sender: TObject);
begin
  FEdit.SelStart := FSelStart;
  FEdit.SelLength := FSelLen;
end;

procedure TIfdefTabDefinition.HandleEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    FStringGrid.Perform(WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmConfigureIfDef.pc_IfClassesChange(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    act_Open.Enabled := (def.Filename <> '');
    TGrid_Resize(def.StringGrid, [roUseGridWidth, roUseAllRows]);
    TWinControl_SetFocus(def.Edit);
    FText := def.Edit.Text;
  end;
end;

function TfmConfigureIfDef.GetIncludeFile: string;
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  Result := '';
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    if not def.IsIncluded then
      Result := ExtractFileName(def.Filename);
  end;
end;

procedure TfmConfigureIfDef.pc_IfClassesMouseLeave(Sender: TObject);
begin
  pc_IfClasses.Hint := '';
  pc_IfClasses.ShowHint := False;
end;

procedure TfmConfigureIfDef.pc_IfClassesMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  hintPause: Integer;
  TabIndex: Integer;
begin
  TabIndex := pc_IfClasses.IndexOfTabAt(X, Y);
  if (TabIndex >= 0) and (pc_IfClasses.Hint <> pc_IfClasses.Pages[TabIndex].Hint) then begin
    hintPause := Application.hintPause;
    try
      if pc_IfClasses.Hint <> '' then
        Application.hintPause := 0;
      Application.CancelHint;
      pc_IfClasses.Hint := pc_IfClasses.Pages[TabIndex].Hint;
      pc_IfClasses.ShowHint := True;
      Application.ProcessMessages; // force hint to appear
    finally
      Application.hintPause := hintPause;
    end;
  end;
end;

procedure TfmConfigureIfDef.act_AddExecute(Sender: TObject);
var
  pnt: TPoint;
begin
  pnt := Point(0, b_Add.Height);
  pnt := b_Add.ClientToScreen(pnt);
  pm_IncludeFiles.Popup(pnt.X, pnt.Y);
end;

procedure TfmConfigureIfDef.act_OpenExecute(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
  LineNo: Integer;
begin
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    if def.Filename = '' then
      Exit;

    LineNo := def.GetLineNoOfCurrentEntry;
    GxOtaGoToFileLine(def.Filename, LineNo);
    ModalResult := mrCancel;
  end;
end;

procedure TfmConfigureIfDef.chk_AppendCommentClick(Sender: TObject);
var
  ts: TTabSheet;
  def: TIfdefTabDefinition;
begin
  pc_IfClassesChange(nil);
  ts := pc_IfClasses.ActivePage;
  if ts.Tag <> 0 then begin
    def := TIfdefTabDefinition(ts.Tag);
    def.UpdateEditText(def.StringGrid.Row);
  end;
end;

procedure TfmConfigureIfDef.InitOptions;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&Options', 9, 1, '{$IFOPT %s+}');
  FTabDefinitions.Add(def);
  def.AddGridRow('A', 'Align');
  def.AddGridRow('B', 'BoolEval');
  def.AddGridRow('C', 'Assertions');
  def.AddGridRow('D', 'DebugInfo');
  def.AddGridRow('E', '???');
  def.AddGridRow('F', '???');
  def.AddGridRow('G', 'ImportedData');
  def.AddGridRow('H', 'LongStrings');
  def.AddGridRow('I', 'IoChecks');
  def.AddGridRow('J', 'WriteableConsts');
  def.AddGridRow('K', '???');
  def.AddGridRow('L', 'LocalSymbols');
  def.AddGridRow('M', 'TypeInfo');
  def.AddGridRow('N', '???');
  def.AddGridRow('O', 'Optimization');
  def.AddGridRow('P', 'OpenStrings');
  def.AddGridRow('Q', 'OverflowChecks');
  def.AddGridRow('R', 'RangeChecks');
  def.AddGridRow('S', '???');
  def.AddGridRow('T', 'TypedAddress');
  def.AddGridRow('U', 'SafeDivide');
  def.AddGridRow('V', 'VarStringChecks');
  def.AddGridRow('W', 'StackFrames');
  def.AddGridRow('X', 'ExtendedSyntax');
  def.AddGridRow('Y', 'ReferenceInfo / DefinitionInfo');
  def.AddGridRow('Z', '???');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitVerXxx;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&VERxxx', 4, 1, '{$IFNDEF %s}');
  FTabDefinitions.Add(def);
  def.AddGridRow('VER330', 'Delphi 10.3 Rio / BDS 20');
  def.AddGridRow('VER320', 'Delphi 10.2 Tokyo / BDS 19');
  def.AddGridRow('VER310', 'Delphi 10.1 Berlin / BDS 18');
  def.AddGridRow('VER300', 'Delphi 10.0 Seattle / BDS 17');
  def.AddGridRow('VER290', 'Delphi XE8 / BDS 16');
  def.AddGridRow('VER280', 'Delphi XE7 / BDS 15');
  def.AddGridRow('VER270', 'Delphi XE6 / BDS 14');
  def.AddGridRow('VER265', 'AppMethod');
  def.AddGridRow('VER260', 'Delphi XE5 / BDS 12');
  def.AddGridRow('VER250', 'Delphi XE4 / BDS 11');
  def.AddGridRow('VER240', 'Delphi XE3 / BDS 10');
  def.AddGridRow('VER230', 'Delphi XE2 / BDS 9');
  def.AddGridRow('VER220', 'Delphi XE1 / BDS 8');
  def.AddGridRow('VER210', 'Delphi 2010 / BDS 7');
  def.AddGridRow('VER200', 'Delphi 2009 / BDS 6');
  def.AddGridRow('VER190', 'Delphi 2007 .NET');
  def.AddGridRow('VER185', 'Delphi 2007 / BDS 4');
  def.AddGridRow('VER180', 'Delphi 2006/2007 / BDS 3');
  def.AddGridRow('VER170', 'Delphi 2005 / BDS 2');
  def.AddGridRow('VER160', 'Delphi 8 .NET / BDS 1');
  def.AddGridRow('VER150', 'Delphi 7');
  def.AddGridRow('VER140', 'Delphi 6');
  def.AddGridRow('VER130', 'Delphi 5');
  def.AddGridRow('VER120', 'Delphi 4');
  def.AddGridRow('VER100', 'Delphi 3');
  def.AddGridRow('VER90', 'Delphi 2');
  def.AddGridRow('VER80', 'Delphi 1');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitRtlVersion;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&RtlVersion', 16, 2, '{$IF RtlVersion >= %s}');
  FTabDefinitions.Add(def);
  def.AddGridRow('33', 'Delphi 10.3 Rio / BDS 20');
  def.AddGridRow('32', 'Delphi 10.2 Tokyo / BDS 19');
  def.AddGridRow('31', 'Delphi 10.1 Berlin / BDS 18');
  def.AddGridRow('30', 'Delphi 10.0 Seattle / BDS 17');
  def.AddGridRow('29', 'Delphi XE8 / BDS 16');
  def.AddGridRow('28', 'Delphi XE7 / BDS 15');
  def.AddGridRow('27', 'Delphi XE6 / BDS 14');
//  def.AddGridRow('26.5', 'AppMethod'); ???
  def.AddGridRow('26', 'Delphi XE5 / BDS 12');
  def.AddGridRow('25', 'Delphi XE4 / BDS 11');
  def.AddGridRow('24', 'Delphi XE3 / BDS 10');
  def.AddGridRow('23', 'Delphi XE2 / BDS 9');
  def.AddGridRow('22', 'Delphi XE1 / BDS 8');
  def.AddGridRow('21', 'Delphi 2010 / BDS 7');
  def.AddGridRow('20', 'Delphi 2009 / BDS 6');
//  def.AddGridRow('19', 'Delphi 2007 .NET'); ???
  def.AddGridRow('18', 'Delphi 2007 / BDS 4');
  def.AddGridRow('18', 'Delphi 2006 / BDS 3');
  def.AddGridRow('17', 'Delphi 2005 / BDS 2');
  def.AddGridRow('16', 'Delphi 8 .NET / BDS 1');
  def.AddGridRow('15', 'Delphi 7');
  def.AddGridRow('14', 'Delphi 6');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitCompilerVersion;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&CompilerVersion', 21, 2, '{$IF CompilerVersion >= %s}');
  FTabDefinitions.Add(def);
  def.AddGridRow('33', 'Delphi 10.3 Rio / BDS 20');
  def.AddGridRow('32', 'Delphi 10.2 Tokyo / BDS 19');
  def.AddGridRow('31', 'Delphi 10.1 Berlin / BDS 18');
  def.AddGridRow('30', 'Delphi 10.0 Seattle / BDS 17');
  def.AddGridRow('29', 'Delphi XE8 / BDS 16');
  def.AddGridRow('28', 'Delphi XE7 / BDS 15');
  def.AddGridRow('27', 'Delphi XE6 / BDS 14');
  def.AddGridRow('26.5', 'AppMethod');
  def.AddGridRow('26', 'Delphi XE5 / BDS 12');
  def.AddGridRow('25', 'Delphi XE4 / BDS 11');
  def.AddGridRow('24', 'Delphi XE3 / BDS 10');
  def.AddGridRow('23', 'Delphi XE2 / BDS 9');
  def.AddGridRow('22', 'Delphi XE1 / BDS 8');
  def.AddGridRow('21', 'Delphi 2010 / BDS 7');
  def.AddGridRow('20', 'Delphi 2009 / BDS 6');
  def.AddGridRow('19', 'Delphi 2007 .NET');
  def.AddGridRow('18.5', 'Delphi 2007 / BDS 4');
  def.AddGridRow('18', 'Delphi 2006 / BDS 3');
  def.AddGridRow('17', 'Delphi 2005 / BDS 2');
  def.AddGridRow('16', 'Delphi 8 .NET / BDS 1');
  def.AddGridRow('15', 'Delphi 7');
  def.AddGridRow('14', 'Delphi 6');
  def.InitEvents;
end;

type
  TIncDirective = class
  private
    FLineIdx: Integer;
    FComment: string;
  public
    constructor Create(_LineIdx: Integer; const _Comment: string);
    property LineIdx: Integer read FLineIdx;
    property Comment: string read FComment;
  end;

function IsCompilerDirective(const _Line: string; const _Directive: string;
  out _Value: string; out _Comment: string): Boolean;
var
  Incl: string;
  s: string;
  p: Integer;
begin
  _Comment := '';
  Incl := '{' + _Directive + ' ';
  s := Trim(_Line);
  Result := StartsText(Incl, s);
  if Result then begin
    p := Pos('}', s);
    if p > Length(Incl) then begin
      _Value := Copy(s, Length(Incl) + 1, p - 1 - Length(Incl));
      _Value := Trim(_Value);
      _Value := AnsiDequotedStr(_Value, '''');
      s := Copy(s, p + 1);
      p := Pos('//', s);
      if p > 0 then
        _Comment := Trim(Copy(s, p + 2));
    end;
  end;
end;

function TfmConfigureIfDef.AddIncludePage(_No: Integer; const _FullFn: string; _IsIncluded: Boolean): TTabSheet;
const
  DEFINE_STR = '$DEFINE';
  UNDEF_STR = '$UNDEF';
  NOT_DEFINE_STR = '.$DEFINE';
  NOT_UNDEF_STR = '.$UNDEF';
var
  def: TIfdefTabDefinition;
  Lines: TGXUnicodeStringList;
  LineIdx: Integer;
  Line: string;
  Define: string;
  Comment: string;
  Directives: TStringList;
  Idx: Integer;
  id: TIncDirective;
begin
  Result := nil;
  Directives := nil;
  Lines := TGXUnicodeStringList.Create;
  try
    Directives := TStringList.Create;
    Directives.Sorted := True;
    Lines.LoadFromFile(_FullFn);
    for LineIdx := 0 to Lines.Count - 1 do begin
      Line := Lines[LineIdx];
      Line := Trim(Line);
      if IsCompilerDirective(Line, DEFINE_STR, Define, Comment)
        or IsCompilerDirective(Line, UNDEF_STR, Define, Comment)
        or IsCompilerDirective(Line, NOT_DEFINE_STR, Define, Comment)
        or IsCompilerDirective(Line, NOT_UNDEF_STR, Define, Comment) then begin
        if not Directives.Find(Define, Idx) then begin
          Idx := Directives.Add(Define);
          Directives.Objects[Idx] := TIncDirective.Create(LineIdx, Comment);
        end;
      end;
    end;
    if Directives.Count > 0 then begin
      def := TIfdefTabDefinition.Create(Self, pc_IfClasses,
        Format('&%d %s', [_No, ExtractFileName(_FullFn)]), 4, 1, '{$IFNDEF %s}', _IsIncluded, _FullFn);
      FTabDefinitions.Add(def);
      for Idx := 0 to Directives.Count - 1 do begin
        Define := Directives[Idx];
        id := TIncDirective(Directives.Objects[Idx]);
        def.AddGridRow(Define, id.Comment, id.LineIdx + 1);
        FreeAndNil(id);
      end;
      def.InitEvents;
      Result := def.FTabSheet;
    end;
  finally
    FreeAndNil(Directives);
    FreeAndNil(Lines);
  end;
end;

procedure TfmConfigureIfDef.InitIncludes;

  procedure AddIfFound(var _Number: Integer; const _fn: string);
  var
    i: Integer;
    FullFn: string;
  begin
    for i := 0 to FSearchPath.Count - 1 do begin
      FullFn := AddSlash(FSearchPath[i]) + _fn;
      if FileExists(FullFn) then begin
        AddIncludePage(_Number, FullFn, True);
        Inc(_Number);
      end;
    end;
  end;

const
  INCLUDE_STR = '$Include';
  I_STR = '$I';
var
  Lines: TGXUnicodeStringList;
  i: Integer;
  s: string;
  fn: string;
  Number: Integer;
  Comment: string;
begin
  Number := 1;
  Lines := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorText(Lines, False) then
      Exit;
    for i := 0 to Lines.Count - 1 do begin
      s := Lines[i];
      if IsCompilerDirective(s, INCLUDE_STR, fn, Comment)
        or IsCompilerDirective(s, I_STR, fn, Comment) then begin
        AddIfFound(Number, fn);
      end;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

procedure TfmConfigureIfDef.IncludeFilesListReady;

  function IsKnownIncFile(const _fn: string): Boolean;
  var
    DefIdx: Integer;
    def: TIfdefTabDefinition;
  begin
    for DefIdx := 0 to FTabDefinitions.Count - 1 do begin
      def := FTabDefinitions[DefIdx] as TIfdefTabDefinition;
      if SameText(_fn, def.Filename) then begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

var
  ResIdx: Integer;
  fn: string;
begin
  // This is called via Synchronize.
  // Can we be sure that the static tabs have already been created?
  for ResIdx := 0 to FFindThread.Results.Count - 1 do begin
    fn := FFindThread.Results[ResIdx];
    if not IsKnownIncFile(fn) then begin
      TPopupMenu_AppendMenuItem(pm_IncludeFiles, fn, OnInludeFileSelected);
    end;
  end;
end;

procedure TfmConfigureIfDef.OnInludeFileSelected(_Sender: TObject);
var
  DefIdx: Integer;
  def: TIfdefTabDefinition;
  Number: Integer;
  mi: TMenuItem;
  ts: TTabSheet;
begin
  mi := _Sender as TMenuItem;
  Number := 1;
  for DefIdx := 0 to FTabDefinitions.Count - 1 do begin
    def := FTabDefinitions[DefIdx] as TIfdefTabDefinition;
    if def.Filename <> '' then
      Inc(Number);
  end;
  ts := AddIncludePage(Number, StripHotkey(mi.Caption), False);
  if Assigned(ts) then
    pc_IfClasses.ActivePage := ts;
  mi.Free;
end;

{ TIncDirective }

constructor TIncDirective.Create(_LineIdx: Integer; const _Comment: string);
begin
  inherited Create;
  FLineIdx := _LineIdx;
  FComment := _Comment;
end;

{$IF RTLVersion <= RtlVersionDelphi2005}

{ TPageControl }

procedure TPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  mEvnt: TTrackMouseEvent;
begin
  inherited;
  if not FMouseTracking then begin
    mEvnt.cbSize := SizeOf(mEvnt);
    mEvnt.dwFlags := TME_LEAVE;
    mEvnt.hwndTrack := Handle;
    TrackMouseEvent(mEvnt);
    FMouseTracking := True;
  end;
end;

procedure TPageControl.WMMouseLeave(var Msg: TMessage);
begin
  Msg.Result := 0;
  FMouseTracking := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$IFEND}

initialization
  RegisterEditorExpert(TIfDefExpert);
end.

