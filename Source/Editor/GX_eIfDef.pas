unit GX_eIfDef;

interface

{$I GX_CondDefine.inc}

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
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_BaseForm,
  GX_BaseExpert;

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
    procedure pc_IfClassesChange(Sender: TObject);
    procedure chk_AppendCommentClick(Sender: TObject);
  private
    FText: string;
    procedure InitCompilerVersion;
    procedure InitVerXxx;
    procedure InitOptions;
    procedure InitRtlVersion;
    procedure InitIncludes;
  public
    class function Execute(_bmp: TBitmap; var _AppendComment: Boolean; out _Text: string): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Messages,
  ToolsAPI,
  StrUtils,
  GX_OtaUtils,
  GX_dzVclUtils,
  GX_GenericUtils;

{$R *.dfm}

{ TIfDefExpert }

constructor TIfDefExpert.Create;
begin
  inherited Create;
end;

procedure TIfDefExpert.Execute(Sender: TObject);
var
  InsertString: string;
begin
  if not TfmConfigureIfDef.Execute(GetBitmap, FAppendComment, InsertString) then
    Exit; //==>
  GxOtaInsertLineIntoEditor(InsertString);
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

class function TfmConfigureIfDef.Execute(_bmp: TBitmap; var _AppendComment: Boolean; out _Text: string): Boolean;
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
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmConfigureIfDef.Create(_Owner: TComponent);
begin
  inherited;

  p_Bottom.BevelOuter := bvNone;

  TControl_SetMinConstraints(Self);

  InitCompilerVersion;
  InitRtlVersion;
  InitVerXxx;
  InitOptions;
  InitIncludes;

  pc_IfClassesChange(pc_IfClasses);
end;

destructor TfmConfigureIfDef.Destroy;
begin
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
    procedure UpdateEditText(_Row: Integer);
    procedure AddGridRow(const _Value, _Desc: string);
    procedure HandleEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleSelectCell(_Sender: TObject; _Col, _Row: Integer; var _CanSelect: Boolean);
    procedure HandleEditEnter(_Sender: TObject);
    procedure HandleEditChange(_Sender: TObject);
  public
    constructor Create(_Form: TfmConfigureIfDef; _pc: TPageControl; const _Caption: string;
      _SelStart, _SelLen: Integer; const _TextFormatStr: string);
    procedure InitEvents;
  end;

constructor TIfdefTabDefinition.Create(_Form: TfmConfigureIfDef; _pc: TPageControl;
  const _Caption: string; _SelStart, _SelLen: Integer; const _TextFormatStr: string);
begin
  FForm := _Form;
  FSelStart := _SelStart;
  FSelLen := _SelLen;
  FTextFormatStr := _TextFormatStr;
  FRow := 0;

  FTabSheet := TTabSheet.Create(_Form);
  FTabSheet.Name := '';
  FTabSheet.Parent := _pc;
  FTabSheet.PageControl := _pc;
  FTabSheet.Caption := _Caption + ' ';

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

procedure TIfdefTabDefinition.InitEvents;
begin
  TGrid_Resize(FStringGrid, [roUseGridWidth, roUseAllRows]);
  FStringGrid.OnSelectCell := HandleSelectCell;
  FEdit.OnChange := HandleEditChange;
  FEdit.OnEnter := HandleEditEnter;
  FEdit.OnKeyDown := HandleEditKeyDown;

  UpdateEditText(FStringGrid.Row);
end;

procedure TIfdefTabDefinition.AddGridRow(const _Value, _Desc: string);
begin
  FStringGrid.RowCount := FRow + 1;
  FStringGrid.Cells[0, FRow] := _Value;
  FStringGrid.Cells[1, FRow] := _Desc;
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
    TWinControl_SetFocus(def.FEdit);
    FText := def.FEdit.Text;
  end;
end;

procedure TfmConfigureIfDef.chk_AppendCommentClick(Sender: TObject);
begin
  pc_IfClassesChange(nil);
end;

procedure TfmConfigureIfDef.InitOptions;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&Options', 9, 1, '{$IFOPT %s+}');
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
  def.AddGridRow('VER310', 'Delphi 10.1 Berlin / BDS 17');
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
  def.AddGridRow('31', 'Delphi 10.1 Berlin / BDS 17');
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
//  def.AddGridRow('17', 'Delphi 2005 / BDS 2');
//  def.AddGridRow('16', 'Delphi 8 .NET / BDS 1');
//  def.AddGridRow('15', 'Delphi 7');
//  def.AddGridRow('14', 'Delphi 6');
  def.InitEvents;
end;

procedure TfmConfigureIfDef.InitCompilerVersion;
var
  def: TIfdefTabDefinition;
begin
  def := TIfdefTabDefinition.Create(Self, pc_IfClasses, '&CompilerVersion', 21, 2, '{$IF CompilerVersion >= %s}');
  def.AddGridRow('31', 'Delphi 10.1 Berlin / BDS 17');
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

procedure TfmConfigureIfDef.InitIncludes;

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
    Result := StrBeginsWith(Incl, s, False);
    if Result then begin
      p := Pos('}', s);
      if p > Length(Incl) then begin
        _Value := Copy(s, Length(Incl) + 1, p - 1 - Length(Incl));
        _Value := Trim(_Value);
        _Value := AnsiDequotedStr(_Value, '''');
        s := Copy(s, p + 1, MaxInt);
        p := Pos('//', s);
        if p > 0 then
          _Comment := Trim(Copy(s, p + 2, MaxInt));
      end;
    end;
  end;

  procedure AddIncludePage(var _Paths: TStrings; _No: Integer; const _IncFn: string);
  const
    DEFINE_STR = '$DEFINE';
    UNDEF_STR = '$UNDEF';
    NOT_DEFINE_STR = '.$DEFINE';
    NOT_UNDEF_STR = '.$UNDEF';
  var
    def: TIfdefTabDefinition;
    i: Integer;
    FullFn: string;
    Lines: TGXUnicodeStringList;
    LineIdx: Integer;
    Line: string;
    Define: string;
    Comment: string;
    Directives: TStringList;
    Idx: Integer;
    pc: PChar;
  begin
    if not Assigned(_Paths) then begin
      _Paths := TStringList.Create;
      GxOtaGetProjectSourcePathStrings(_Paths);
    end;
    for i := 0 to _Paths.Count - 1 do begin
      FullFn := AddSlash(_Paths[i]) + _IncFn;
      if FileExists(FullFn) then begin
        Directives := nil;
        Lines := TGXUnicodeStringList.Create;
        try
          Directives := TStringList.Create;
          Directives.Sorted := True;
          Lines.LoadFromFile(FullFn);
          for LineIdx := 0 to Lines.Count - 1 do begin
            Line := Lines[LineIdx];
            Line := Trim(Line);
            if IsCompilerDirective(Line, DEFINE_STR, Define, Comment)
              or IsCompilerDirective(Line, UNDEF_STR, Define, Comment)
              or IsCompilerDirective(Line, NOT_DEFINE_STR, Define, Comment)
              or IsCompilerDirective(Line, NOT_UNDEF_STR, Define, Comment) then begin
              if not Directives.Find(Define, Idx) then begin
                Idx := Directives.Add(Define);
                if Comment <> '' then
                  Directives.Objects[Idx] := Pointer(StrNew(PChar(Comment)));
              end;
            end;
          end;
          if Directives.Count > 0 then begin
            def := TIfdefTabDefinition.Create(Self, pc_IfClasses, Format('&%d %s', [_No, _IncFn]), 4, 1, '{$IFNDEF %s}');
            for Idx := 0 to Directives.Count - 1 do begin
              Define := Directives[Idx];
              pc := PChar(Directives.Objects[Idx]);
              if Assigned(pc) then begin
                Comment := pc;
                StrDispose(pc);
              end else
                Comment := '';
              def.AddGridRow(Define, Comment);
            end;
            def.InitEvents;
          end;
        finally
          FreeAndNil(Directives);
          FreeAndNil(Lines);
        end;
        Exit;
      end;
    end;
  end;

const
  INCLUDE_STR = '$Include';
  I_STR = '$I';
var
  Lines: TGXUnicodeStringList;
  Paths: TStrings;
  i: Integer;
  s: string;
  fn: string;
  No: Integer;
  Comment: string;
begin
  No := 1;
  Paths := nil;
  Lines := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorText(Lines, False) then
      Exit;
    for i := 0 to Lines.Count - 1 do begin
      s := Lines[i];
      if IsCompilerDirective(s, INCLUDE_STR, fn, Comment)
        or IsCompilerDirective(s, I_STR, fn, Comment) then begin
        AddIncludePage(Paths, No, fn);
        Inc(No);
      end;
    end;
  finally
    FreeAndNil(Paths);
    FreeAndNil(Lines);
  end;
end;

initialization
  RegisterEditorExpert(TIfDefExpert);
end.
