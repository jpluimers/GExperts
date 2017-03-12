unit GX_MacroLibraryNamePrompt;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ActnList,
  Menus,
  SynUnicode,
  SynMemo,
  GX_BaseForm,
  GX_GenericUtils;

type
  TfmMacroLibraryNamePrompt = class(TfmBaseForm)
    lblMacroName: TLabel;
    edtMacroName: TEdit;
    chkDoNotShowAgain: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblMacroDesc: TLabel;
    mmoMacroDescription: TMemo;
    pnlMacro: TPanel;
    btnDelete: TButton;
    btnEdit: TButton;
    btnInsert: TButton;
    lblMacroKeystrokes: TLabel;
    btnAppend: TButton;
    TheActionList: TActionList;
    actEdit: TAction;
    actInsert: TAction;
    actDelete: TAction;
    actAppend: TAction;
    pmKeystrokes: TPopupMenu;
    miEdit: TMenuItem;
    miInsert: TMenuItem;
    miDelete: TMenuItem;
    miAppend: TMenuItem;
    procedure actEditExecute(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actAppendExecute(Sender: TObject);
  private
    FMemo: TSynMemo;
    procedure SetData(const AMacroName, AMacroDesc: string; AMacro: TGXUnicodeStringList;
      AShowCheckbox: Boolean);
    procedure GetData(out AMacroName, AMacroDesc: string; AMacro: TGXUnicodeStringList);
    procedure HandleOnSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean;
      var FG, BG: TColor);
    procedure MemoOnEnter(Sender: TObject);
    procedure MemoOnExit(Sender: TObject);
  public
    class function Execute(AOwner: TComponent; AShowCheckbox: Boolean;
      var AMacroName, AMacroDesc: string;
      AMacro: TGXUnicodeStringList; var APromptForName: Boolean): Boolean; overload;
    class function Execute(AOwner: TComponent; var AMacroName, AMacroDesc: string;
      AMacro: TGXUnicodeStringList): Boolean; overload;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  SynEdit,
  GX_dzVclUtils,
  GX_MacroLibraryEditEntry,
  GX_MacroLibrary;

{$R *.dfm}

{ TTfmMacroLibraryNamePrompt }

class function TfmMacroLibraryNamePrompt.Execute(AOwner: TComponent; AShowCheckbox: Boolean;
  var AMacroName, AMacroDesc: string; AMacro: TGXUnicodeStringList;
  var APromptForName: Boolean): Boolean;
var
  Form: TfmMacroLibraryNamePrompt;
begin
  Form := TfmMacroLibraryNamePrompt.Create(AOwner);
  try
    Form.SetData(AMacroName, AMacroDesc, AMacro, AShowCheckbox);
    Result := (Form.ShowModal = mrOk);
    if Result then begin
      Form.GetData(AMacroName, AMacroDesc, AMacro);
    end;
    // The checkbox is always evaluated
    APromptForName := not Form.chkDoNotShowAgain.Checked;
  finally
    FreeAndNil(Form);
  end;
end;

class function TfmMacroLibraryNamePrompt.Execute(AOwner: TComponent; var AMacroName,
  AMacroDesc: string; AMacro: TGXUnicodeStringList): Boolean;
var
  DummBool: Boolean;
begin
  Result := Execute(AOwner, False, AMacroName, AMacroDesc, AMacro, DummBool)
end;

constructor TfmMacroLibraryNamePrompt.Create(AOwner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  pnlMacro.BevelOuter := bvNone;

  FMemo := TSynMemo.Create(Self);
  FMemo.Align := alClient;
  FMemo.Parent := pnlMacro;
  FMemo.Gutter.Visible := False;
  FMemo.ReadOnly := True;
  FMemo.Font.Height := -11;
  FMemo.Font.Name := 'Courier New';
  FMemo.ActiveLineColor := $D0FFFF;
  FMemo.OnEnter := MemoOnEnter;
  fmemo.onexit := MemoOnExit;
  FMemo.OnSpecialLineColors := HandleOnSpecialLineColors;
  FMemo.Options := FMemo.Options - [eoScrollPastEol, eoScrollPastEof, eoEnhanceHomeKey, eoEnhanceEndKey];
  FMemo.PopupMenu := pmKeystrokes;
end;

procedure TfmMacroLibraryNamePrompt.MemoOnEnter(Sender: TObject);
begin
  TheActionList.State := asNormal;
end;

procedure TfmMacroLibraryNamePrompt.MemoOnExit(Sender: TObject);
begin
  TheActionList.State := asSuspended;
end;

procedure TfmMacroLibraryNamePrompt.actInsertExecute(Sender: TObject);
var
  LineIdx: Integer;
  s: TGXUnicodeString;
  Macro: TMacroKey;
begin
  LineIdx := FMemo.CaretY - 1;
  if (LineIdx < 0) or (LineIdx >= FMemo.Lines.Count) then
    Exit;
  s := '';
  Macro.Full := 0;
  if TfmEditMacroItem.Execute(Self, s, Macro) then begin
    if s = '' then
      s := TMacroInfo.MacroKeyToText(Macro);
    FMemo.Lines.Insert(LineIdx, s);
    FMemo.Lines.Objects[LineIdx] := Macro.AsPointer;
  end;
end;

procedure TfmMacroLibraryNamePrompt.actAppendExecute(Sender: TObject);
var
  LineIdx: Integer;
  s: TGXUnicodeString;
  Macro: TMacroKey;
begin
  s := '';
  Macro.Full := 0;
  if TfmEditMacroItem.Execute(Self, s, Macro) then begin
    if s = '' then
      s := TMacroInfo.MacroKeyToText(Macro);
    LineIdx := FMemo.Lines.Count;
    FMemo.Lines.Append(s);
    FMemo.Lines.Objects[LineIdx] := Macro.AsPointer;
  end;
end;

procedure TfmMacroLibraryNamePrompt.actDeleteExecute(Sender: TObject);
var
  LineIdx: Integer;
begin
  LineIdx := FMemo.CaretY - 1;
  if (LineIdx < 0) or (LineIdx >= FMemo.Lines.Count) then
    Exit;
  FMemo.Lines.Delete(LineIdx);
end;

procedure TfmMacroLibraryNamePrompt.actEditExecute(Sender: TObject);
var
  LineIdx: Integer;
  s: TGXUnicodeString;
  Macro: TMacroKey;
begin
  LineIdx := FMemo.CaretY - 1;
  if (LineIdx < 0) or (LineIdx >= FMemo.Lines.Count) then
    Exit;
  s := FMemo.Lines[LineIdx];
  Macro.AsPointer := FMemo.Lines.Objects[LineIdx];
  if TfmEditMacroItem.Execute(Self, s, Macro) then begin
    if s = '' then
      s := TMacroInfo.MacroKeyToText(Macro);
    FMemo.Lines[LineIdx] := s;
    FMemo.Lines.Objects[LineIdx] := Macro.AsPointer;
  end;
end;

procedure TfmMacroLibraryNamePrompt.HandleOnSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
var
  MacroEntry: LongWord;
begin
  Dec(Line); // Line is one-based
  if (Line < 0) or (Line >= FMemo.Lines.Count) then
    Exit;
  MacroEntry := LongWord(FMemo.Lines.Objects[Line]);
  if (MacroEntry and $FFFF0000) <> 0 then begin
    Special := True;
    FG := clBlue;
  end;
end;

procedure TfmMacroLibraryNamePrompt.GetData(out AMacroName, AMacroDesc: string;
  AMacro: TGXUnicodeStringList);
begin
  AMacroName := edtMacroName.Text;
  AMacroDesc := mmoMacroDescription.Lines.Text;
  AMacro.Assign(FMemo.Lines);
end;

procedure TfmMacroLibraryNamePrompt.SetData(const AMacroName, AMacroDesc: string; AMacro: TGXUnicodeStringList;
  AShowCheckbox: Boolean);
begin
  chkDoNotShowAgain.Visible := AShowCheckbox;
  edtMacroName.Text := AMacroName;
  mmoMacroDescription.Lines.Text := AMacroDesc;
  chkDoNotShowAgain.Checked := False;
  FMemo.Lines := AMacro;
end;

end.
