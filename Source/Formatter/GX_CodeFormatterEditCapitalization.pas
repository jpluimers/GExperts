unit GX_CodeFormatterEditCapitalization;

{$I GX_CondDefine.inc}

{$IFDEF GX_VER200_up}
{$DEFINE SUPPORTS_UNICODE_STRING}
{$ENDIF}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Actions,
  ActnList,
  Buttons,
  ExtCtrls,
  GX_EnhancedEditor,
  GX_GenericUtils;

type
  TfmCodeFormatterEditCapitalization = class(TForm)
    p_Buttons: TPanel;
    TheActionList: TActionList;
    act_AllUpperCase: TAction;
    act_AllLowerCase: TAction;
    act_FirstCharUp: TAction;
    act_ToggleComment: TAction;
    act_Import: TAction;
    act_Export: TAction;
    act_ClearSearch: TAction;
    p_Items: TPanel;
    ed_Search: TEdit;
    b_Clear: TSpeedButton;
    b_UpperCase: TSpeedButton;
    b_LowerCase: TSpeedButton;
    b_FirstCharUp: TSpeedButton;
    b_ToggleComment: TSpeedButton;
    b_OK: TButton;
    b_Cancel: TButton;
    b_Import: TButton;
    b_Export: TButton;
    procedure act_AllUpperCaseExecute(Sender: TObject);
    procedure act_AllLowerCaseExecute(Sender: TObject);
    procedure act_FirstCharUpExecute(Sender: TObject);
    procedure ed_SearchChange(Sender: TObject);
    procedure act_ClearSearchExecute(Sender: TObject);
    procedure act_ToggleCommentExecute(Sender: TObject);
    procedure act_ImportExecute(Sender: TObject);
    procedure act_ExportExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FOrigList: TGXUnicodeString;
    FWords: TGxEnhancedEditor;
    function TryGetLine(out _Idx: Integer; out _Line: TGXUnicodeString): Boolean;
    procedure SetLine(_Idx: Integer; const _Line: TGXUnicodeString);
    procedure HandleOnEnterList(Sender: TObject);
    procedure HandleOnExitList(Sender: TObject);
  public
    constructor Create(_Owner: TComponent); override;
    procedure ListToForm(_Words: TGXUnicodeStringList);
    procedure FormToList(_Words: TGXUnicodeStringList);
    function IsChanged: Boolean;
  end;

implementation

{$R *.DFM}

uses
  GX_dzVclUtils,
  GX_dzClassUtils;

{ TfmCodeFormatterCapitalization }

constructor TfmCodeFormatterEditCapitalization.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  p_Items.BevelOuter := bvNone;
  p_Buttons.BevelOuter := bvNone;

  FWords := TGxEnhancedEditor.Create(Self);
  FWords.Parent := p_Items;
  FWords.HighLighter := gxpNone;
  FWords.Align := alClient;
  FWords.Font.Name := 'Courier New';
  FWords.Font.Size := 10;
  FWords.OnEnter := HandleOnEnterList;
  FWords.OnExit := HandleOnExitList;
  FWords.ActiveLineColor := clYellow;
  FWords.WantTabs := False;
end;

procedure TfmCodeFormatterEditCapitalization.ListToForm(_Words: TGXUnicodeStringList);
begin
  FWords.SetLines(_Words);
  FOrigList := _Words.Text;
end;

procedure TfmCodeFormatterEditCapitalization.FormToList(_Words: TGXUnicodeStringList);
var
  sl: TGXUnicodeStringList;
begin
  sl := TGXUnicodeStringList.Create;
  try
    FWords.GetLines(sl);
    sl.Sort;
    _Words.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.HandleOnEnterList(Sender: TObject);
begin
  FWords.ActiveLineColor := RGB(250, 255, 230);
end;

procedure TfmCodeFormatterEditCapitalization.HandleOnExitList(Sender: TObject);
begin
  FWords.ActiveLineColor := clYellow;
end;

function TfmCodeFormatterEditCapitalization.TryGetLine(out _Idx: Integer; out _Line: TGXUnicodeString): Boolean;
begin
  _Idx := FWords.CaretXY.Y;
  Result := (_Idx >= 0) or (_Idx < FWords.LineCount);
  if Result then
    _Line := FWords.GetLine(_Idx);
end;

procedure TfmCodeFormatterEditCapitalization.SetLine(_Idx: Integer; const _Line: TGXUnicodeString);
var
  pnt: TPoint;
begin
  pnt := FWords.CaretXY;
  FWords.SetLine(_Idx, _Line);
  FWords.CaretXY := pnt;
end;

procedure TfmCodeFormatterEditCapitalization.act_AllUpperCaseExecute(Sender: TObject);
var
  Line: TGXUnicodeString;
  Idx: Integer;
begin
  if TryGetLine(Idx, Line) then
    SetLine(Idx, UpperCase(Line));
end;

procedure TfmCodeFormatterEditCapitalization.act_AllLowerCaseExecute(Sender: TObject);
var
  Line: TGXUnicodeString;
  Idx: Integer;
begin
  if TryGetLine(Idx, Line) then
    SetLine(Idx, LowerCase(Line));
end;

{$IFNDEF SUPPORTS_UNICODE_STRING}

function UpCase(_c: WideChar): WideChar; overload;
begin
  Result := WideChar(System.UpCase(Char(_c)));
end;
{$ENDIF}

procedure TfmCodeFormatterEditCapitalization.act_FirstCharUpExecute(Sender: TObject);
var
  Line: TGXUnicodeString;
  Idx: Integer;
begin
  if TryGetLine(Idx, Line) and (Line <> '') then begin
    Line[1] := UpCase(Line[1]);
    SetLine(Idx, Line);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.ed_SearchChange(Sender: TObject);
var
  s: string;
  sl: TGXUnicodeStringList;
  Idx: Integer;
begin
  s := ed_Search.Text;
  if s = '' then
    Exit;
  sl := TGXUnicodeStringList.Create;
  try
    FWords.GetLines(sl);
    TGXUnicodeStringList_MakeIndex(sl);
    sl.Find(s, Idx);
    FWords.CaretXY := Point(0, Integer(sl.Objects[Idx]) - 1);
  finally
    FreeAndNil(sl);
  end;
end;

function TfmCodeFormatterEditCapitalization.IsChanged: Boolean;
var
  sl: TGXUnicodeStringList;
begin
  sl := TGXUnicodeStringList.Create;
  try
    FWords.GetLines(sl);
    sl.Sort;
    Result := (sl.Text <> FOrigList);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.FormCloseQuery(
  Sender: TObject; var CanClose: Boolean);
begin
  if mrCancel = ModalResult then begin
    if IsChanged and (MessageDlg('Leave without saving changes?', mtInformation,
      [mbYes, mbNo], 0) = ID_No) then
      CanClose := False;
  end;
end;

procedure TfmCodeFormatterEditCapitalization.act_ClearSearchExecute(Sender: TObject);
begin
  ed_Search.Text := '';
end;

procedure TfmCodeFormatterEditCapitalization.act_ToggleCommentExecute(Sender: TObject);
var
  Idx: Integer;
  Line: TGXUnicodeString;
begin
  if TryGetLine(Idx, Line) and (Line <> '') then begin
    if Line[1] = '*' then
      Line := Copy(Line, 2, 255)
    else
      Line := '*' + Line;
    SetLine(Idx, Line);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.act_ExportExecute(Sender: TObject);
var
  fn: string;
begin
  if ShowSaveDialog('Select file to import', 'txt', fn) then
    FWords.SaveToFile(fn);
end;

procedure TfmCodeFormatterEditCapitalization.act_ImportExecute(Sender: TObject);
var
  fn: string;
begin
  if ShowOpenDialog('Select file to import', 'txt', fn) then
    FWords.LoadFromFile(fn);
end;

end.
