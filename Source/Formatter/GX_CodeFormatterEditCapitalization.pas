// the code formatter dialog for editing the capitalization file
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterEditCapitalization;

{$I GX_CondDefine.inc}

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
  Buttons,
  ExtCtrls;

type
  TCapitalizationAction = (acUpperCase, acLowerCase, acFirstUp, acFirstLow, acCommentOut);

  TfmCodeFormatterEditCapitalization = class(TForm)
    l_FileName: TLabel;
    p_Buttons: TPanel;
    p_Upper: TPanel;
    l_Select: TLabel;
    l_ChangeInto: TLabel;
    lb_Items: TListBox;
    ed_Search: TEdit;
    ed_Change: TEdit;
    b_UpperCase: TButton;
    b_LowerCase: TButton;
    b_FirstCharUp: TButton;
    b_FirstCharLow: TButton;
    b_AddIdentifier: TButton;
    b_Delete: TButton;
    b_ToggleComment: TButton;
    b_Help: TButton;
    b_Ok: TButton;
    b_Cancel: TButton;
    b_Import: TButton;
    b_Export: TButton;
    od_Import: TOpenDialog;
    sd_Export: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure lb_ItemsClick(Sender: TObject);
    procedure ed_ChangeChange(Sender: TObject);
    procedure b_UpperCaseClick(Sender: TObject);
    procedure b_LowerCaseClick(Sender: TObject);
    procedure b_FirstCharUpClick(Sender: TObject);
    procedure ed_SearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure b_FirstCharLowClick(Sender: TObject);
    procedure ed_SearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure b_AddIdentifierClick(Sender: TObject);
    procedure b_ToggleCommentClick(Sender: TObject);
    procedure b_DeleteClick(Sender: TObject);
    procedure b_HelpClick(Sender: TObject);
    procedure b_ImportClick(Sender: TObject);
    procedure b_ExportClick(Sender: TObject);
  private
    FisChanged: Boolean;
    procedure ChangeSelected(AnAction: TCapitalizationAction);
  public
    procedure ListToForm(AWordList: TStrings);
    procedure FormToList(AWordList: TStrings);
    property IsChanged: Boolean read FisChanged write FisChanged;
  end;

implementation

{$R *.DFM}

uses
  GX_CodeFormatterConfig;

procedure TfmCodeFormatterEditCapitalization.FormShow(Sender: TObject);
begin
  IsChanged := False;
end;

procedure TfmCodeFormatterEditCapitalization.ListToForm(AWordList: TStrings);
var
  s: string;
  i: Integer;
begin
//  l_FileName.Caption := AFileName;
  ed_Search.Text := '';
  ed_Change.Text := '';
  lb_Items.Clear;
  lb_Items.Sorted := False;
  for i := 0 to AWordList.Count - 1 do begin
    s := AWordList[i];
    if s <> '' then
      lb_Items.Items.Add(s);
  end;
  lb_Items.Sorted := True;
end;

procedure TfmCodeFormatterEditCapitalization.FormToList(AWordList: TStrings);
var
  i: Integer;
begin
  AWordList.Clear;
  for i := 0 to lb_Items.Items.Count - 1 do begin
    AWordList.Add(lb_Items.Items[i]);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.lb_ItemsClick(Sender: TObject);
begin
  if lb_Items.SelCount <= 1 then begin
    ed_Search.Enabled := True;
    ed_Change.Enabled := True;
    ed_Search.OnChange := nil;
    ed_Change.OnChange := nil;
    ed_Change.Text := lb_Items.Items[lb_Items.ItemIndex];
    ed_Search.Text := ed_Change.Text;
    ed_Search.OnChange := ed_SearchChange;
    ed_Change.OnChange := ed_ChangeChange;
  end else begin
    ed_Search.Enabled := False;
    ed_Change.Enabled := False;
  end
end;

procedure TfmCodeFormatterEditCapitalization.ed_ChangeChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := lb_Items.ItemIndex;
  if Idx < 0 then begin
    lb_Items.Items.Add(ed_Change.Text);
    IsChanged := True;
  end else if not SameText(lb_Items.Items[Idx], ed_Change.Text) then begin
    IsChanged := True;
    lb_Items.Items.Delete(Idx);
    lb_Items.Items.Add(ed_Change.Text);
  end else if lb_Items.Items[Idx] <> ed_Change.Text then begin
    lb_Items.Items[Idx] := ed_Change.Text;
    IsChanged := True;
  end;
  lb_Items.ItemIndex := lb_Items.Items.IndexOf(ed_Change.Text);
end;

procedure TfmCodeFormatterEditCapitalization.ChangeSelected(AnAction: TCapitalizationAction);
var
  i: Integer;

  function Change(s: string): string;
  var
    Ch: Char;
  begin
    Result := s;
    if s <> '' then begin
      case AnAction of
        acUpperCase: Result := UpperCase(s);
        acLowerCase: Result := LowerCase(s);
        acFirstUp: begin
            Result := s;
            Result[1] := UpCase(Result[1]);
          end;
        acFirstLow: begin
            Result := s;
            Ch := Result[1];
            if (Ch >= 'A') and (Ch <= 'Z') then
              Inc(Result[1], 32);
          end;
        acCommentOut:
          if s[1] = '*' then
            Result := Copy(s, 2, Length(s))
          else
            Result := '*' + s;
      end;
    end;
    IsChanged := IsChanged or (s <> Result);
  end;

begin
  IsChanged := True;
  if ed_Change.Enabled then begin
    if ed_Change.SelLength = 0 then
      ed_Change.Text := Change(ed_Change.Text)
    else
      ed_Change.SelText := Change(ed_Change.SelText);
  end else begin
    for i := 0 to lb_Items.Count - 1 do begin
      if lb_Items.Selected[i] then begin
        lb_Items.Items[i] := Change(lb_Items.Items[i]);
        lb_Items.Selected[i] := True;
      end;
    end;
    if (AnAction = acCommentOut) then begin
      lb_Items.Sorted := False;
      lb_Items.Sorted := True; //Combination forces reorder
    end;
  end;
end;

procedure TfmCodeFormatterEditCapitalization.b_UpperCaseClick(Sender: TObject);
begin
  ChangeSelected(acUpperCase);
end;

procedure TfmCodeFormatterEditCapitalization.b_LowerCaseClick(Sender: TObject);
begin
  ChangeSelected(acLowerCase);
end;

procedure TfmCodeFormatterEditCapitalization.b_FirstCharUpClick(Sender: TObject);
begin
  ChangeSelected(acFirstUp);
end;

procedure TfmCodeFormatterEditCapitalization.ed_SearchChange(Sender: TObject);
var
  SearchEditText: string;
  i, j, Lasti: Integer;
begin
  SearchEditText := ed_Search.Text;
  j := 1;
  Lasti := 0;
  for i := 0 to lb_Items.Count - 1 do begin
    if (StrLIComp(PChar(SearchEditText), PChar(lb_Items.Items[i]), j) = 0) then begin
      Inc(j);
      Lasti := i;
    end;
    lb_Items.Selected[i] := False;
  end;
  if Lasti < lb_Items.Count then begin
    lb_Items.OnClick := nil;
    try
      lb_Items.Selected[Lasti] := True;
      if Lasti > 0 then
        lb_Items.TopIndex := Lasti - 1;
    finally
      lb_Items.OnClick := lb_ItemsClick;
    end;
  end;
end;

procedure TfmCodeFormatterEditCapitalization.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if mrCancel = ModalResult then begin
    if IsChanged and (MessageDlg('Leave without saving changes?', mtInformation,
      [mbYes, mbNo], 0) = ID_No) then
      Action := caNone;
  end;
end;

procedure TfmCodeFormatterEditCapitalization.b_FirstCharLowClick(Sender: TObject);
begin
  ChangeSelected(acFirstLow);
end;

procedure TfmCodeFormatterEditCapitalization.ed_SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure HandleUpDown(ADirection: Integer);
  begin
    lb_Items.Selected[lb_Items.ItemIndex] := False;
    lb_Items.ItemIndex := lb_Items.ItemIndex + ADirection;
    lb_Items.Selected[lb_Items.ItemIndex] := True;
    lb_Items.SetFocus;
  end;

begin
  case Key of
    VK_UP:
      HandleUpDown(-1);
    VK_DOWN:
      HandleUpDown(1);
  end;
end;

procedure TfmCodeFormatterEditCapitalization.b_AddIdentifierClick(Sender: TObject);
var
  i: Integer;
begin
  IsChanged := True;
  lb_Items.Items.Add('<New Identifier>');
  lb_Items.ItemIndex := lb_Items.Items.IndexOf('<New Identifier>');
  for i := 0 to lb_Items.Count - 1 do
    lb_Items.Selected[i] := False;
  lb_Items.Selected[lb_Items.ItemIndex] := True;
  lb_ItemsClick(nil);
end;

procedure TfmCodeFormatterEditCapitalization.b_ToggleCommentClick(Sender: TObject);
begin
  ChangeSelected(acCommentOut);
end;

procedure TfmCodeFormatterEditCapitalization.b_DeleteClick(Sender: TObject);
var
  i: Integer;
begin
  for i := lb_Items.Count - 1 downto 0 do
    if lb_Items.Selected[i] then begin
      lb_Items.Items.Delete(i);
      IsChanged := True;
    end;
end;

procedure TfmCodeFormatterEditCapitalization.b_ExportClick(Sender: TObject);
begin
  if not sd_Export.Execute then
    Exit;
  lb_Items.Items.SaveToFile(sd_Export.FileName);
end;

procedure TfmCodeFormatterEditCapitalization.b_HelpClick(Sender: TObject);
//var
//  S: string;
begin
//  with TOptionsDlg(Owner) do
//    if HelpFile <> nil then
//    begin
//      S := 'Edit file dialog';
//      WinHelp(0, PChar(HelpFile), HELP_KEY,
//        Integer(S));
//    end;
end;

procedure TfmCodeFormatterEditCapitalization.b_ImportClick(Sender: TObject);
begin
  if not od_Import.Execute then
    Exit;
  lb_Items.Items.LoadFromFile(od_Import.FileName);
end;

end.

