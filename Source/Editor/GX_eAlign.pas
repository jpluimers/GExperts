unit GX_eAlign;

{$I GX_CondDefine.inc}

// Original author: Stefan Pettersson <stefpet@gmail.com>

interface

uses
  Classes, Controls, StdCtrls, Forms, Menus;

type
  TfmAlign = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lstTokens: TListBox;
    lblToken: TLabel;
    pmuTokens: TPopupMenu;
    mitConfiguration: TMenuItem;
    cbxMode: TComboBox;
    procedure lstTokensDblClick(Sender: TObject);
    procedure mitConfigurationClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  SysUtils,
  GX_OtaUtils, GX_GenericUtils, GX_ConfigurationInfo,
  GX_EditorExpert, GX_eSelectionEditorExpert, GX_eAlignOptions;

const
  DEFAULT_WHITESPACE = 0;
  DEFAULT_TOKENS: array[0..10] of string =
    (':=', '=', '//', '{', '(*', '''', ':', '+', 'read', 'write', 'in ''');

resourcestring
  SNoTokens = 'No tokens found to align on.';

type
  TGXAlignMode = (gamRightmost, gamFirstToken);

  TAlignExpert = class(TSelectionEditorExpert)
  private
    FWhitespace: Integer;
    FLastToken: string;
    FLastMode: TGXAlignMode;
    FTokens: TStringList;
    FSelectedText: string;
    function QueryUserForAlignToken(var Token: string; var Mode: TGXAlignMode): Boolean;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
    procedure LoadConfiguration(Dialog: TfmAlign);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  end;

var
  AlignExpertInst: TAlignExpert;


{ TAlignExpert }

constructor TAlignExpert.Create;
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('Z');
  FTokens := TStringList.Create;
  AlignExpertInst := Self;
end;

destructor TAlignExpert.Destroy;
begin
  AlignExpertInst := nil;
  FreeAndNil(FTokens);
  inherited;
end;

procedure TAlignExpert.Configure;
var
  Dlg: TfmAlignOptions;
begin
  Dlg := TfmAlignOptions.Create(nil);
  try
    Dlg.edtWhitespace.Text := IntToStr(FWhitespace);
    Dlg.mmoTokens.Lines.Assign(FTokens);

    if Dlg.ShowModal = mrOk then
    begin
      FWhitespace := Min(StrToIntDef(Dlg.edtWhitespace.Text, 1), 100);
      FWhitespace := Max(FWhitespace, 0);
      FTokens.Assign(Dlg.mmoTokens.Lines);

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TAlignExpert.GetDisplayName: string;
resourcestring
  SAlignName = 'Align Lines';
begin
  Result := SAlignName;
end;

procedure TAlignExpert.GetHelpString(List: TStrings);
resourcestring
  SAlignHelp =
    '  This expert aligns the text of the selected lines at the first occurrence of a chosen token in each line.  To use it, select a block of code in the code editor and activate this expert.  '+
    'You may find this feature useful to align the right hand side of variable, field, or constant declarations and other similar lists.' + sLineBreak +
    '  There are two alignment modes.  In the "Align at rightmost token" mode, the rightmost token found in the selected text becomes the column position the other lines are aligned to.  '+
    'In the "Align at first token" mode, the first located token is used to determine the alignment column.  In this second mode, any line whose token prefix is longer than the position of the first token will not be modified.' + sLineBreak +
    '  You can configure the list of tokens to align on as well as the minimum number of space characters that must precede a token that is being aligned.';
begin
  List.Text := SAlignHelp;
end;

class function TAlignExpert.GetName: string;
begin
  Result := 'Align';
end;

function TAlignExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TAlignExpert.ProcessSelected(Lines: TStrings): Boolean;
resourcestring
  STokenListEmpty = 'The token list is empty.' + sLineBreak +
                    'Please add tokens using the configuration dialog.';
var
  TabSize: Integer;
  i: Integer;
  FirstIndex, PosIndex: Integer;
  RowLength, MaxRowLength: Integer;
  AlignIndex: Integer;
  Temp: string;
  MaxPos: Integer;
  LineSuffix: string;
  AlignToken: string;
  AlignMode: TGXAlignMode;
begin
  Assert(Assigned(Lines));
  Result := False;

  if Lines.Count < 2 then
    raise Exception.Create('Please select the lines of code to align first.');

  if FTokens.Count = 0 then
    raise Exception.Create(STokenListEmpty);

  FSelectedText := Lines.Text;
  if not QueryUserForAlignToken(AlignToken, AlignMode) then
    Exit;

  FirstIndex := 0;
  RowLength := 0;
  MaxRowLength := 0;

  TabSize := GxOtaGetTabWidth;

  // Decide at what column to align by
  for i := 0 to Lines.Count - 1 do
  begin
    Temp := ExpandTabsInLine(Lines[i], TabSize);

    PosIndex := Pos(AlignToken, Temp);

    if (PosIndex > 0) and (FirstIndex = 0) then
    begin
      FirstIndex := PosIndex;
      // If first line contains token, only align based on that token
      if AlignMode = gamFirstToken then
        Break;
    end;

    if PosIndex > 0 then
      RowLength := Length(TrimRight(Copy(Temp, 1, PosIndex - 1))) + FWhitespace;

    if RowLength > MaxRowLength then
      MaxRowLength := RowLength;
  end;

  // Exit if nothing to align
  if FirstIndex = 0 then
    raise Exception.Create(SNoTokens);

  // Try to align at column of first found otherwise
  // align after the maximum length of a row
  if FirstIndex > MaxRowLength then
    AlignIndex  := FirstIndex - 1
  else
    AlignIndex := MaxRowLength;

  // Perform alignment
  for i := 0 to Lines.Count - 1 do
  begin
    PosIndex := Pos(AlignToken, Lines[i]);

    if PosIndex > 0 then
    begin
      Temp := TrimRight(Copy(Lines[i], 1, PosIndex - 1));
      MaxPos := Max(AlignIndex - Length(ExpandTabsInLine(Temp, TabSize)), FWhitespace);
      LineSuffix := Copy(Lines[i], PosIndex, Length(Lines[i]));
      Lines[i] := Temp + StringOfChar(' ', MaxPos) + LineSuffix;
    end;
  end;

  Result := True;
end;

procedure TAlignExpert.InternalLoadSettings(Settings: TGExpertsSettings);
var
  i: Integer;
begin
  inherited InternalLoadSettings(Settings);

  // Do not localize any of the below items
  FWhitespace := Settings.ReadInteger(ConfigurationKey, 'Whitespace', DEFAULT_WHITESPACE);
  Settings.ReadStrings(FTokens, ConfigurationKey + '\Tokens', 'Tokens');
  FLastToken := Settings.ReadString(ConfigurationKey, 'Token', '');
  FLastMode := TGXAlignMode(Settings.ReadEnumerated(ConfigurationKey, 'Mode', TypeInfo(TGXAlignMode), Ord(FLastMode)));

  // If no tokens were found, create a default list of tokens
  if FTokens.Count = 0 then
    for i := Low(DEFAULT_TOKENS) to High(DEFAULT_TOKENS) do
      FTokens.Add(DEFAULT_TOKENS[i]);
end;

procedure TAlignExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);

  // Do not localize any of the below items
  Settings.WriteInteger(ConfigurationKey, 'Whitespace', FWhitespace);
  Settings.WriteStrings(FTokens, ConfigurationKey + '\Tokens', 'Tokens');
  Settings.WriteString(ConfigurationKey, 'Token', FLastToken);
  Settings.WriteEnumerated(ConfigurationKey, 'Mode', TypeInfo(TGXAlignMode), Ord(FLastMode));
end;

procedure TfmAlign.lstTokensDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TAlignExpert.QueryUserForAlignToken(var Token: string; var Mode: TGXAlignMode): Boolean;
var
  Dialog: TfmAlign;
begin
  Result := False;
  Mode := gamRightmost;

  Dialog := TfmAlign.Create(nil);
  try
    LoadConfiguration(Dialog);

    if Dialog.ShowModal = mrOk then
    begin
      Token := Dialog.lstTokens.Items[Dialog.lstTokens.ItemIndex];
      if Dialog.cbxMode.ItemIndex > 0 then
        Mode := gamFirstToken;
      FLastToken := Token;
      FLastMode := Mode;
      SaveSettings;
      Result := True;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TfmAlign.mitConfigurationClick(Sender: TObject);
begin
  AlignExpertInst.Configure;
  AlignExpertInst.LoadConfiguration(Self);
end;

procedure TAlignExpert.LoadConfiguration(Dialog: TfmAlign);
var
  LastIndex: Integer;
begin
  AddStringsPresentInString(FTokens, Dialog.lstTokens.Items, FSelectedText);
  if Dialog.lstTokens.Count < 1 then
    raise Exception.Create(SNoTokens);

  LastIndex := Dialog.lstTokens.Items.IndexOf(FLastToken);
  if (FLastToken > '') and (LastIndex > -1) then
    Dialog.lstTokens.ItemIndex := LastIndex
  else
    Dialog.lstTokens.ItemIndex := 0;
  Dialog.cbxMode.ItemIndex := 0;
  if FLastMode <> gamRightmost then
    Dialog.cbxMode.ItemIndex := 1;
end;

initialization
  RegisterEditorExpert(TAlignExpert);
end.

