unit GX_eQuote;

{$I GX_CondDefine.inc}

interface

uses
  GX_EditorExpert, GX_eSelectionEditorExpert, GX_ConfigurationInfo,
  Classes, StdCtrls, Controls, Forms, GX_BaseForm, Graphics;

type
  TQuoteExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
    function GetNoSelectionMode: TNoSelectionMode; override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TPasteQuotedExpert = class(TEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure GetHelpString(List: TStrings); override;
    procedure Execute(Sender: TObject); override;
  end;

  TUnquoteExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
    function GetNoSelectionMode: TNoSelectionMode; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TCopyUnquotedExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
    function GetNoSelectionMode: TNoSelectionMode; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TfmQuoteConfig = class(TfmBaseForm)
    gbxStyle: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblEndOfLine: TLabel;
    cbxEndOfLine: TComboBox;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_OtaUtils, Dialogs, ToolsAPI, GX_eQuoteSupport, Clipbrd,
  GX_GenericUtils;

const
  cDefaultEndOfLine = '''+#13#10+';
  VK_SingleQuote = $DE;

var
  EndOfLine: string = '';

procedure VerifyCompatibleFileType;
var
  FileName: string;
begin
  FileName := GxOtaGetCurrentSourceFile;
  if not IsPascalSourceFile(FileName) then
    raise Exception.Create('This editor expert only supports Delphi source code.');
end;

procedure SaveQuoteSettings(Settings: TGExpertsSettings);
begin
  // Do not localize any of the below items
  Settings.WriteString('Quote', 'EndOfLine', EndOfLine);
end;

procedure LoadQuoteSettings(Settings: TGExpertsSettings);
begin
  // Do not localize any of the below items
  EndOfLine := Settings.ReadString('Quote', 'EndOfLine', cDefaultEndOfLine);
end;

function ConfigureQuote:boolean;
var
  Dlg: TfmQuoteConfig;
begin
  Dlg := TfmQuoteConfig.Create(nil);
  try
    // Possible end of line choices
    Dlg.cbxEndOfLine.Items.Add(' '' +');
    Dlg.cbxEndOfLine.Items.Add('''#13#10+');
    Dlg.cbxEndOfLine.Items.Add('''+#13#10+');
    Dlg.cbxEndOfLine.Items.Add(''' + #13#10 +');
    Dlg.cbxEndOfLine.Items.Add(''' + sLineBreak +');

    Dlg.cbxEndOfLine.Text := EndOfLine;

    Result := Dlg.ShowModal = mrOk;
    if Result then
      EndOfLine := Dlg.cbxEndOfLine.Text;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TQuoteExpert }

procedure TQuoteExpert.Configure;
begin
  if ConfigureQuote then
    SaveSettings;
end;

constructor TQuoteExpert.Create;
begin
  inherited;
  ShortCut := scCtrl + VK_SingleQuote;
end;

function TQuoteExpert.GetDisplayName: string;
resourcestring
  SQuoteName = 'Quote String';
begin
  Result := SQuoteName;
end;

procedure TQuoteExpert.GetHelpString(List: TStrings);
resourcestring
  SQuoteHelp =
    '  This expert quotes a selected block of single or multi-line code. ' +
    'This is useful for converting external code such as HTML or SQL into Delphi string ' +
    'constants.  To use it, select a block of code in the IDE code editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  The column where the selection starts determines how far any subsequent lines are indented.  '+
    'You can configure this expert to use different end of line styles.';
begin
  List.Text := SQuoteHelp;
end;

class function TQuoteExpert.GetName: string;
begin
  Result := 'Quote';
end;

function TQuoteExpert.GetNoSelectionMode: TNoSelectionMode;
begin
  Result := nsmError;
end;

procedure TQuoteExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  LoadQuoteSettings(Settings);
end;

procedure TQuoteExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  SaveQuoteSettings(Settings);
end;

function TQuoteExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  EditView: IOTAEditView;
  BlockStart, BlockEnd: TOTAEditPos;
  SelStart, SelLength: Integer;
begin
  Assert(Assigned(Lines));
  Result := False;

  VerifyCompatibleFileType;
  EditView := GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;

  if not GxOtaGetSelection(EditView, BlockStart, BlockEnd, SelStart, SelLength) then
    Exit;

  QuoteLines(Lines, EndOfLine, BlockStart.Col);

  Result := True;
end;

{ TUnQuoteExpert }

constructor TUnQuoteExpert.Create;
begin
  inherited;
  ShortCut := scCtrl + scAlt + VK_SingleQuote;
end;

function TUnQuoteExpert.GetDisplayName: string;
resourcestring
  SUnquoteName = 'Unquote String';
begin
  Result := SUnquoteName;
end;

procedure TUnQuoteExpert.GetHelpString(List: TStrings);
resourcestring
  SUnquoteHelp = '  This expert unquotes a selected block of Delphi code.  ' +
    'This may be useful when preparing to take external code like HTML ' +
    'or SQL outside of the editor and into an external application. '+
    'To use it, select a block in the IDE code editor and ' +
    'activate this expert.' +
    sLineBreak +
    '  Note: The results may not be correct when activated on ' +
    'complicated strings that contain string concatinations with ' +
    'functions.';
begin
  List.Text := SUnquoteHelp;
end;

class function TUnQuoteExpert.GetName: string;
begin
  Result := 'Unquote';
end;

function TUnquoteExpert.GetNoSelectionMode: TNoSelectionMode;
begin
  Result := nsmError;
end;

function TUnQuoteExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TUnQuoteExpert.ProcessSelected(Lines: TStrings): Boolean;
begin
  VerifyCompatibleFileType;
  Assert(Assigned(Lines));
  UnquoteLines(Lines);
  Result := True;
end;

{ TCopyUnquotedExpert }

constructor TCopyUnquotedExpert.Create;
begin
  inherited;
  //ShortCut := scAlt + scCtrl + scShift + Ord('C');
end;

function TCopyUnquotedExpert.GetDisplayName: string;
resourcestring
  SCopyUnquotedName = 'Copy Unquoted String';
begin
  Result := SCopyUnquotedName;
end;

procedure TCopyUnquotedExpert.GetHelpString(List: TStrings);
resourcestring
  SCopyUnquotedHelp = '  This expert unquotes a selected block of Delphi code, then ' +
    'copies it to the clipboard.  ' +
    'This may be useful for bringing external code like HTML ' +
    'or SQL outside of the editor and into an external application. ' +
    'To use it, select a block in the IDE code editor and ' +
    'activate this expert.' +
    sLineBreak +
    '  Note: The results may not be correct when activated on ' +
    'complicated strings that contain string concatinations with ' +
    'functions.';
begin
  List.Text := SCopyUnquotedHelp;
end;

class function TCopyUnquotedExpert.GetName: string;
begin
  Result := 'CopyUnquoted';
end;

function TCopyUnquotedExpert.GetNoSelectionMode: TNoSelectionMode;
begin
  Result := nsmError;
end;

function TCopyUnquotedExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TCopyUnquotedExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  EditView: IOTAEditView;
  BlockStart, BlockEnd: TOTAEditPos;
  SelStart, SelLength: Integer;
begin
  Assert(Assigned(Lines));
  Result := False;
  VerifyCompatibleFileType;

  EditView := GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;

  if not GxOtaGetSelection(EditView, BlockStart, BlockEnd, SelStart, SelLength) then
    Exit;

  UnquoteLines(Lines, BlockStart.Col);

  Clipboard.AsText := Lines.Text;
end;

{ TPasteQuotedExpert }

procedure TPasteQuotedExpert.Configure;
begin
  if ConfigureQuote then
    SaveSettings;
end;

constructor TPasteQuotedExpert.Create;
begin
  inherited;
  //ShortCut := scAlt + scCtrl + scShift + Ord('V');
end;

procedure TPasteQuotedExpert.Execute(Sender: TObject);
var
  EditView: IOTAEditView;
  Lines: TStringList;
begin
  VerifyCompatibleFileType;
  Lines := TStringList.Create;
  try
    EditView := GxOtaGetTopMostEditView;
    Assert(Assigned(EditView), 'No edit view found');
    Lines.Text := Clipboard.AsText;
    QuoteLines(Lines, EndOfLine, EditView.CursorPos.Col);
    GxOtaInsertTextIntoEditor(Lines.Text);
  finally
    FreeAndNil(Lines);
  end;
end;

function TPasteQuotedExpert.GetDisplayName: string;
resourcestring
  SPasteQuotedName = 'Paste Quoted String';
begin
  Result := SPasteQuotedName;
end;

procedure TPasteQuotedExpert.GetHelpString(List: TStrings);
resourcestring
  SPasteQuotedHelp =
    '  This expert pastes clipboard text with added quotes to create valid Delphi code. ' +
    'This is useful for pasting external code such as HTML and SQL into the ' +
    'editor as string constants. To use it, select a block in the IDE code editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    'You can configure this expert to use different end of line styles.';
begin
  List.Text := SPasteQuotedHelp;
end;

class function TPasteQuotedExpert.GetName: string;
begin
  Result := 'PasteQuoted';
end;

procedure TPasteQuotedExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  LoadQuoteSettings(Settings);
end;

procedure TPasteQuotedExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  SaveQuoteSettings(Settings);
end;

initialization
  RegisterEditorExpert(TQuoteExpert);
  RegisterEditorExpert(TUnquoteExpert);
  RegisterEditorExpert(TCopyUnquotedExpert);
  RegisterEditorExpert(TPasteQuotedExpert);
end.

