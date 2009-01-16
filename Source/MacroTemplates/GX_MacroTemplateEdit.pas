unit GX_MacroTemplateEdit;

interface

uses
  Classes, Controls, Forms, StdCtrls, ComCtrls, GX_MacroFile, GXHotKey,
  GX_BaseForm;

type
  TMacroTemplate = record
    Name: string;
    Description: string;
    ShortCut: TShortCut;
    InsertPos: TTemplateInsertPos;
  end;

  TfmMacroTemplateEdit = class(TfmBaseForm)
    edtName: TEdit;
    edtDescription: TEdit;
    lblName: TLabel;
    lblDescription: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblShortCut: TLabel;
    lblInsertPos: TLabel;
    cbxInsertPos: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    edtShortCut: TGXHotKey;
  end;

function GetMacroTemplate(var VMacroTemplate: TMacroTemplate): Boolean;
function EditMacroObject(AMacroObject: TMacroObject): Boolean;

implementation

uses
  SysUtils, Dialogs;

{$R *.dfm}

function GetMacroTemplate(var VMacroTemplate: TMacroTemplate): Boolean;
begin
  Result := False;
  with TfmMacroTemplateEdit.Create(Application) do
  try
    edtName.Text := VMacroTemplate.Name;
    edtDescription.Text := VMacroTemplate.Description;
    edtShortCut.HotKey := VMacroTemplate.ShortCut;
    cbxInsertPos.ItemIndex := Ord(VMacroTemplate.InsertPos);
    ShowModal;
    if ModalResult = mrOk then
    begin
      Result := True;
      VMacroTemplate.Name := edtName.Text;
      VMacroTemplate.Description := edtDescription.Text;
      VMacroTemplate.ShortCut := edtShortCut.HotKey;
      VMacroTemplate.InsertPos := TTemplateInsertPos(cbxInsertPos.ItemIndex);
    end;
  finally
    Free;
  end;
end;

function EditMacroObject(AMacroObject: TMacroObject): Boolean;
begin
  Result := False;
  with TfmMacroTemplateEdit.Create(Application) do
  try
    edtName.Text := AMacroObject.Name;
    edtDescription.Text := AMacroObject.Desc;
    edtShortCut.HotKey := AMacroObject.ShortCut;
    cbxInsertPos.ItemIndex := Ord(AMacroObject.InsertPos);

    ShowModal;
    if ModalResult = mrOk then
    begin
      Result := True;
      AMacroObject.Name := edtName.Text;
      AMacroObject.Desc := edtDescription.Text;
      AMacroObject.ShortCut := edtShortCut.HotKey;
      AMacroObject.InsertPos := TTemplateInsertPos(cbxInsertPos.ItemIndex);
    end;
  finally
    Free;
  end;
end;

procedure TfmMacroTemplateEdit.btnOKClick(Sender: TObject);
resourcestring
  NoTemplateName = 'All templates require a name';
  InvalidIdentTemplateName = 'Template names must be valid identifiers (no white space or special characters)';
begin
  if edtDescription.Text = '' then
    edtDescription.Text := edtName.Text;
  if (edtName.Text = '') then
    MessageDlg(NoTemplateName, mtError, [mbOK], 0)
  else if (not IsValidIdent(edtName.Text)) then
    MessageDlg(InvalidIdentTemplateName, mtError, [mbOK], 0)
  else
    ModalResult := mrOk;
end;

procedure TfmMacroTemplateEdit.FormCreate(Sender: TObject);
begin
  edtShortCut := TGXHotKey.Create(Self);
  edtShortCut.Parent := Self;
  edtShortCut.SetBounds(edtDescription.Left, lblShortCut.Top - 2, cbxInsertPos.Width, edtDescription.Height);
  edtShortCut.TabOrder := 2;
end;

end.

