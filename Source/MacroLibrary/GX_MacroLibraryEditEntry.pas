unit GX_MacroLibraryEditEntry;

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
  GX_BaseForm,
  StdCtrls,
  ExtCtrls,
  GX_GenericUtils,
  SynEdit,
  GX_MacroLibrary;

type
  TfmEditMacroItem = class(TfmBaseForm)
    pnlEditGoesHere: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    rbText: TRadioButton;
    rbSpecial: TRadioButton;
    cmbSpecialKey: TComboBox;
    chkModifierShift: TCheckBox;
    chkModifierControl: TCheckBox;
    chkModifierAlt: TCheckBox;
    procedure cmbSpecialKeyEnter(Sender: TObject);
    procedure rbSpecialClick(Sender: TObject);
    procedure rbTextClick(Sender: TObject);
  private
    FEdit: TSynEdit;
    procedure SetData(const _Item: TGXUnicodeString; const _Macro: TMacroKey);
    procedure GetData(out _Item: TGXUnicodeString; out _Macro: TMacroKey);
    procedure SynEditOnEnter(Sender: TObject);
  public
    class function Execute(_Owner: TWinControl; var _Item: TGXUnicodeString; var _Macro: TMacroKey): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_dzVclUtils;

{ TfmEditMacroItem }

class function TfmEditMacroItem.Execute(_Owner: TWinControl;
  var _Item: TGXUnicodeString; var _Macro: TMacroKey): Boolean;
var
  frm: TfmEditMacroItem;
begin
  frm := TfmEditMacroItem.Create(_Owner);
  try
    frm.SetData(_Item, _Macro);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Item, _Macro);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmEditMacroItem.Create(_Owner: TComponent);
var
  mks: TMacroSpecialKey;
  Items: TStrings;
begin
  inherited;
  pnlEditGoesHere.BevelOuter := bvNone;
  FEdit := TSynEdit.Create(Self);
  FEdit.Parent := pnlEditGoesHere;
  FEdit.Align := alClient;
  FEdit.Gutter.Visible := False;
  FEdit.Options := FEdit.Options - [eoScrollPastEol, eoScrollPastEof, eoEnhanceHomeKey, eoEnhanceEndKey];
  FEdit.Font.Height := -11;
  FEdit.Font.Name := 'Courier New';
  FEdit.ScrollBars := ssNone;
  FEdit.OnEnter := SynEditOnEnter;

  Items := cmbSpecialKey.Items;
  Items.Clear;
  for mks := Succ(Low(TMacroSpecialKey)) to High(TMacroSpecialKey) do
    Items.AddObject(MacroSpecialKeyStrings[mks], Pointer(mks));
end;

procedure TfmEditMacroItem.cmbSpecialKeyEnter(Sender: TObject);
begin
  rbSpecial.Checked := True;
end;

procedure TfmEditMacroItem.SynEditOnEnter(Sender: TObject);
begin
  rbText.Checked := True;
end;

procedure TfmEditMacroItem.GetData(out _Item: TGXUnicodeString; out _Macro: TMacroKey);
var
  IntValue: Integer;
begin
  _Macro.Full := 0;
  if rbText.Checked then begin
    _Item := FEdit.Text;
  end else begin
    _Item := '';
    if TComboBox_GetSelectedObject(cmbSpecialKey, IntValue) then
      _Macro := TMacroInfo.EncodeMacroKey(#0, TMacroSpecialKey(IntValue),
        chkModifierControl.Checked, chkModifierShift.Checked, chkModifierAlt.Checked);
  end;
end;

procedure TfmEditMacroItem.rbSpecialClick(Sender: TObject);
begin
  TWinControl_SetFocus(cmbSpecialKey);
end;

procedure TfmEditMacroItem.rbTextClick(Sender: TObject);
begin
  TWinControl_SetFocus(FEdit);
end;

procedure TfmEditMacroItem.SetData(const _Item: TGXUnicodeString; const _Macro: TMacroKey);
var
  wc: WideChar;
  Special: TMacroSpecialKey;
  Ctrl: Boolean;
  Shift: Boolean;
  Alt: Boolean;
begin
  if _Macro.Full = 0 then begin
    FEdit.Text := _Item;
    rbText.Checked := True;
    TWinControl_SetFocus(FEdit);
  end else begin
    FEdit.Text := '';
    rbSpecial.Checked := True;
    TMacroInfo.DecodeMacroKey(_Macro, wc, Special, Ctrl, Shift, Alt);
    TComboBox_SelectByObject(cmbSpecialKey, Integer(Special));
    chkModifierControl.Checked := Ctrl;
    chkModifierShift.Checked := Shift;
    chkModifierAlt.Checked := Alt;
    TWinControl_SetFocus(cmbSpecialKey);
  end;
end;

end.
