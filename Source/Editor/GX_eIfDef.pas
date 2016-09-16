unit GX_eIfDef;

interface

{$I GX_CondDefine.inc}

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  Grids,
  ExtCtrls,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_BaseForm,
  GX_BaseExpert;

type
  TIfDefExpert = class(TEditorExpert)
  private
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
    procedure pc_IfClassesChange(Sender: TObject);
  private
    FText: string;
    procedure InitCompilerVersion;
    procedure InitVerXxx;
    procedure InitOptions;
    procedure InitRtlVersion;
  public
    class function Execute(out _Text: string): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Messages,
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
  if not TfmConfigureIfDef.Execute(InsertString) then
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

{ TfmConfigureIfDef }

class function TfmConfigureIfDef.Execute(out _Text: string): Boolean;
var
  frm: TfmConfigureIfDef;
begin
  frm := TfmConfigureIfDef.Create(Application);
  try
    Result := frm.ShowModal = mrOk;
    if Result then begin
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
var
  CellText: string;
begin
  FStringGrid.OnSelectCell := HandleSelectCell;
  FEdit.OnChange := HandleEditChange;
  FEdit.OnEnter := HandleEditEnter;
  FEdit.OnKeyDown := HandleEditKeyDown;

  CellText := FStringGrid.Cells[0, FStringGrid.Row];
  FEdit.Text := Format(FTextFormatStr, [CellText]);
  FEdit.SelStart := FSelStart;
  FEdit.SelLength := FSelLen;
end;

procedure TIfdefTabDefinition.AddGridRow(const _Value, _Desc: string);
begin
  FStringGrid.RowCount := FRow + 1;
  FStringGrid.Cells[0, FRow] := _Value;
  FStringGrid.Cells[1, FRow] := _Desc;
  Inc(FRow);
end;

procedure TIfdefTabDefinition.HandleSelectCell(_Sender: TObject; _Col, _Row: Integer;
  var _CanSelect: Boolean);
var
  CellText: string;
  s: string;
begin
  CellText := FStringGrid.Cells[0, _Row];

  s := FEdit.SelText;
  FEdit.Text := Format(FTextFormatStr, [CellText]);
  FEdit.SelStart := FSelStart;
  FEdit.SelLength := FSelLen;
  if s <> '' then begin
    FEdit.SelText := s;
    FEdit.SelStart := FSelStart;
    FEdit.SelLength := FSelLen;
  end;
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

initialization
  RegisterEditorExpert(TIfDefExpert);
end.
