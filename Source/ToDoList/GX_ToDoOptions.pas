unit GX_ToDoOptions;

// Original Author: AJ Banck <ajbanck@davilex.nl>

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, StdCtrls, Forms, GX_BaseForm;

type
  TfmToDoOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    cbShowTokens: TCheckBox;
    cbAddMessage: TCheckBox;
    cbHideOnGoto: TCheckBox;
    lblPriority: TLabel;
    lblToken: TLabel;
    lstTokens: TListBox;
    btnInsert: TButton;
    btnApply: TButton;
    btnRemove: TButton;
    edToken: TEdit;
    cboPriority: TComboBox;
    gbxSearchFiles: TGroupBox;
    btnBrowse: TButton;
    chkInclude: TCheckBox;
    cboDirectories: TComboBox;
    radScanProj: TRadioButton;
    radScanOpen: TRadioButton;
    radScanDir: TRadioButton;
    radScanProjGroup: TRadioButton;
    procedure btnInsertClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edTokenChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstTokensClick(Sender: TObject);
    procedure cboPriorityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure radScanDirClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure UpdateButtonState;
    procedure DirEnable(New: Boolean);
  end;

// #ToDo:4 Test2

implementation

{$R *.dfm}

uses
  GX_GenericUtils, GX_ToDo, Dialogs, Graphics, SysUtils;

procedure TfmToDoOptions.UpdateButtonState;
var
  HasTokenText: Boolean;
  TokenTextInList: Boolean;
  IsListItemSelected: Boolean;
  TextIsCurrentListItem: Boolean;
begin
  HasTokenText := (edToken.Text <> '');
  TokenTextInList := (lstTokens.Items.IndexOf(edToken.Text) > -1);
  IsListItemSelected := (lstTokens.ItemIndex > -1);

  with lstTokens do
    TextIsCurrentListItem := IsListItemSelected and (edToken.Text = Items[ItemIndex]);

  btnInsert.Enabled := HasTokenText and not TokenTextInList;
  btnRemove.Enabled := IsListItemSelected;
  btnApply.Enabled := HasTokenText and IsListItemSelected and TokenTextInList;

  // #ToDo2 Rewrite this to make it readable!!!
  if TextIsCurrentListItem then
    with lstTokens do
    begin
      if (cboPriority.ItemIndex = Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority)) then
        btnApply.Enabled := False;
    end;
end;

procedure TfmToDoOptions.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cboDirectories.Text;
  if GetDirectory(Temp) then
    cboDirectories.Text := Temp;
end;

procedure TfmToDoOptions.DirEnable(New: Boolean);
begin
  cboDirectories.Enabled := New;
  chkInclude.Enabled   := New;
  btnBrowse.Enabled    := New;
  if not New then
    cboDirectories.Color := clBtnFace
  else
    cboDirectories.Color := clWindow;
end;

procedure TfmToDoOptions.btnInsertClick(Sender: TObject);
resourcestring
  SLeadingDollarNotAllowed = 'A leading "$" character is not allowed in tokens, '+
                             'as this can conflict with Object Pascal compiler options.' + sLineBreak +
                             sLineBreak +
                             'Please choose a different token.';

  SEmptyTokenTextError = 'You cannot insert a token that only consists of white space.';
var
  TokenInfo: TTokenInfo;
  TokenString: string;
begin
  TokenString := Trim(edToken.Text);
  if TokenString <> '' then
  begin
    if TokenString[1] = '$' then
    begin
      MessageDlg(SLeadingDollarNotAllowed, mtError, [mbOk], 0);
      Exit;
    end;

    TokenInfo := TTokenInfo.Create;
    TokenInfo.Token := TokenString;
    TokenInfo.Priority := TTodoPriority(cboPriority.ItemIndex);
    lstTokens.Items.AddObject(TokenInfo.Token, TokenInfo);
  end
  else
  begin
    // Warning message that an empty token is inserted
    MessageDlg(SEmptyTokenTextError, mtError, [mbOK], 0);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnRemoveClick(Sender: TObject);
begin
  with lstTokens do
    if ItemIndex <> -1 then
    begin
      Items.Objects[ItemIndex].Free;
      Items.Delete(ItemIndex);
    end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnApplyClick(Sender: TObject);
var
  TokenText: string;
begin
  with lstTokens do
  begin
    TokenText := edToken.Text;

    if (ItemIndex > -1) and (TokenText <> '') and
       Assigned(Items.Objects[ItemIndex]) then
    begin
      Items[ItemIndex] := TokenText;
      with TTokeninfo(Items.Objects[ItemIndex]) do
      begin
        Token := TokenText;
        Priority := TToDoPriority(cboPriority.ItemIndex);
      end;
    end;
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.edTokenChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormShow(Sender: TObject);
begin
  cboPriority.ItemIndex := 1;
  UpdateButtonState;
end;

procedure TfmToDoOptions.lstTokensClick(Sender: TObject);
begin
  UpdateButtonState;
  if lstTokens.ItemIndex > -1 then
  begin
    with lstTokens do
    begin
      cboPriority.ItemIndex := Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority);
      edToken.Text := Items[ItemIndex]
    end;
  end;
end;

procedure TfmToDoOptions.cboPriorityChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormCreate(Sender: TObject);
begin
  DirEnable(radScanDir.Checked);
end;

procedure TfmToDoOptions.radScanDirClick(Sender: TObject);
begin
  DirEnable(radScanDir.Checked);
end;

end.
