unit GX_ProofreaderAutoCorrectEntry;

interface

uses
  Classes, Controls, Forms, StdCtrls,
  GX_ProofreaderUtils;

type
  TfmProofreaderAutoCorrectEntry = class(TForm)
    lblReplaceWhat: TLabel;
    edtReplaceWhat: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblLocation: TLabel;
    lblReplaceWith: TLabel;
    edtReplaceWith: TEdit;
    cbxLocation: TComboBox;
    procedure EditBoxChange(Sender: TObject);
  private
    procedure UpdateButtons;
  public
    class function Execute(var TypedString: string; var ReplaceWhere: TGXWhereReplace;
      var ReplaceWithString: string; Add: Boolean): Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  SysUtils;

{$R *.dfm}

{ TfmProofreaderDictionaryEntry }

class function TfmProofreaderAutoCorrectEntry.Execute(var TypedString: string;
  var ReplaceWhere: TGXWhereReplace; var ReplaceWithString: string; Add: Boolean): Boolean;
resourcestring
  SDlgCaptionAdd = 'Add AutoCorrect Entry';
  SDlgCaptionEdit = 'Edit AutoCorrect Entry';
var
  Dialog: TfmProofreaderAutoCorrectEntry;
begin
  Dialog := TfmProofreaderAutoCorrectEntry.Create(nil);
  try
    if Add then
    begin
      Dialog.Caption := SDlgCaptionAdd;
      Dialog.edtReplaceWhat.Text := '';
      Dialog.cbxLocation.ItemIndex := Ord(wrAnywhere);
      Dialog.edtReplaceWith.Text := '';
    end
    else
    begin
      Dialog.Caption := SDlgCaptionEdit;
      Dialog.edtReplaceWhat.Text := TypedString;
      Dialog.cbxLocation.ItemIndex := Ord(ReplaceWhere);
      Dialog.edtReplaceWith.Text := ReplaceWithString;
    end;

    Dialog.UpdateButtons;
    Result := Dialog.ShowModal = mrOk;
    if Result then
    begin
      Result := Trim(Dialog.edtReplaceWhat.Text) <> '';
      if Result then
      begin
        TypedString := Dialog.edtReplaceWhat.Text;
        ReplaceWhere := TGXWhereReplace(Dialog.cbxLocation.ItemIndex);
        ReplaceWithString := Dialog.edtReplaceWith.Text;
      end;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

constructor TfmProofreaderAutoCorrectEntry.Create(AOwner: TComponent);
var
  WhereReplace: TGXWhereReplace;
begin
  inherited Create(AOwner);
  for WhereReplace := Low(TGXWhereReplace) to High(TGXWhereReplace) do
    cbxLocation.Items.Add(GXWhereReplaceStrings[WhereReplace]);
end;

procedure TfmProofreaderAutoCorrectEntry.UpdateButtons;
begin
  btnOK.Enabled := Trim(edtReplaceWhat.Text) <> ''; // Allow replacing with nothing
end;

procedure TfmProofreaderAutoCorrectEntry.EditBoxChange(Sender: TObject);
begin
  UpdateButtons;
end;

end.

