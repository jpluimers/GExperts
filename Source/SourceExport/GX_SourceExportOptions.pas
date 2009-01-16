unit GX_SourceExportOptions;

{$I GX_CondDefine.inc}

{$IFDEF SYNEDIT}

interface

uses
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ColorGrd,
  SynEdit, Dialogs, GX_BaseForm;

type
  TfmSourceExportOptions = class(TfmBaseForm)
    pnlSettings: TPanel;
    dlgBackground: TColorDialog;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlCode: TPanel;
    gbxAttributes: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    chkStrikeOut: TCheckBox;
    cbxAttributes: TComboBox;
    rbxCopySettings: TRadioGroup;
    btnBackgroundColor: TButton;
    btnLoadIde: TButton;
    lblElement: TLabel;
    procedure AttributeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxAttributesChange(Sender: TObject);
    procedure btnLoadIdeClick(Sender: TObject);
    procedure btnBackgroundColorClick(Sender: TObject);
  private
    FSampleEditor: TSynEdit;
    HiddenGrid: TColorGrid;
    ColorGrid: TColorGrid;
    NoChangeEvents: Boolean;
    function GetColorIndexFromColor(Color: TColor; IsBackground: Boolean): Integer;
    procedure SynEditSelectionChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    BackgroundColor: TColor;
    property SynSampleEditor: TSynEdit read FSampleEditor write FSampleEditor;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, SynEditHighlighter, SynUnicode, GX_SynMemoUtils, GX_VerDepConst, GX_GenericUtils,
  GX_IdeUtils;

{$R *.dfm}

procedure TfmSourceExportOptions.FormCreate(Sender: TObject);
var
  i: Integer;
  GXHighlighter: TGXSyntaxHighlighter;
begin
  // Destroyed with form
  ColorGrid := TColorGrid.Create(Self);
  HiddenGrid := TColorGrid.Create(Self);
  HiddenGrid.Visible := False;
  HiddenGrid.Parent := Self;
  with ColorGrid do
  begin
    Name := 'ColorGrid';
    Parent := gbxAttributes;
    Left := 9;
    Top := 17;
    Width := 100;
    Height := 100;
    ClickEnablesColor := True;
    BackgroundIndex := 15;
    TabOrder := 0;
    TabStop := True;
    OnChange := AttributeChange;
  end;

  FSampleEditor := TSynEdit.Create(Self);
  FSampleEditor.Parent := pnlCode;
  FSampleEditor.ReadOnly := True;
  FSampleEditor.Align := alClient;
  FSampleEditor.Gutter.Width := 0;
  FSampleEditor.Options := FSampleEditor.Options + [eoNoCaret, eoNoSelection, eoHideShowScrollbars, eoAutoSizeMaxScrollWidth] - [eoScrollPastEof, eoScrollPastEol];
  FSampleEditor.OnStatusChange := SynEditSelectionChange;
  GXHighlighter := GetGXHighlighterForCurrentSourceEditor;

  SetSynEditHighlighter(FSampleEditor, GXHighlighter);
  FSampleEditor.Lines.Text := FSampleEditor.Highlighter.SampleSource;
  if Trim(FSampleEditor.Lines.Text) = '' then
    FSampleEditor.Lines.Text := 'No sample source available';
  if GXHighlighter in [gxpHTML, gxpSQL] then
    btnLoadIde.Enabled := False;

  BorderStyle := bsSizeable;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;

  btnLoadIdeClick(Sender);
  for i := 0 to FSampleEditor.Highlighter.AttrCount - 1 do
    cbxAttributes.Items.Add(FSampleEditor.Highlighter.Attribute[i].Name);
  cbxAttributes.ItemIndex := 0;
  cbxAttributesChange(Self);
end;

procedure TfmSourceExportOptions.AttributeChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  AttrStyle: TFontStyles;
begin
  if NoChangeEvents then
    Exit;
  Attr := TSynHighlighterAttributes.Create(cbxAttributes.Items[cbxAttributes.ItemIndex]{$IFDEF UniSynEdit}, cbxAttributes.Items[cbxAttributes.ItemIndex]{$ENDIF});
  try
    AttrStyle := [];
    Attr.Foreground := ColorGrid.ForegroundColor;
    Attr.Background := ColorGrid.BackgroundColor;
    if chkBold.Checked then
      Include(AttrStyle, fsBold);
    if chkItalic.Checked then
      Include(AttrStyle, fsItalic);
    if chkUnderline.Checked then
      Include(AttrStyle, fsUnderline);
    if chkStrikeOut.Checked then
      Include(AttrStyle, fsStrikeOut);
    Attr.Style := AttrStyle;
    FSampleEditor.Highlighter.Attribute[cbxAttributes.ItemIndex].Assign(Attr);
    FSampleEditor.Refresh;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TfmSourceExportOptions.cbxAttributesChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
begin
  Attr := TSynHighlighterAttributes.Create(''{$IFDEF UniSynEdit}, ''{$ENDIF});
  try
    Attr.Assign(FSampleEditor.Highlighter.Attribute[cbxAttributes.ItemIndex]);
    NoChangeEvents := True;
    try
      // Buggy!
      //ColorGrid.ForegroundIndex := ColorGrid.ColorToIndex(Attr.Foreground);
      //ColorGrid.BackgroundIndex := ColorGrid.ColorToIndex(Attr.Background);
      ColorGrid.ForegroundIndex := GetColorIndexFromColor(Attr.Foreground, False);
      ColorGrid.BackgroundIndex := GetColorIndexFromColor(Attr.Background, True);

      chkBold.Checked := (fsBold in Attr.Style);
      chkItalic.Checked := (fsItalic in Attr.Style);
      chkUnderline.Checked := (fsUnderline in Attr.Style);
      chkStrikeOut.Checked := (fsStrikeOut in Attr.Style);
    finally
      NoChangeEvents := False;
      AttributeChange(nil);
    end;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TfmSourceExportOptions.btnLoadIdeClick(Sender: TObject);
resourcestring
  ErrorLoadingIDESettings = 'Error loading IDE editor registry settings.  You may need to customize your editor highlighter settings before they can be loaded from the registry.';
var
  UserSettings: TStringList;
  i: Integer;
begin
  UserSettings := TStringList.Create;
  try
    FSampleEditor.Highlighter.EnumUserSettings(UserSettings);
    for i := UserSettings.Count - 1 downto 0 do
    begin
      if StrBeginsWith(GetIDEVersionID, UserSettings[i]) then
      begin
        if not FSampleEditor.Highlighter.UseUserSettings(i) then
          MessageDlg(ErrorLoadingIDESettings, mtError, [mbOK], 0);
        Break;
      end;
    end;
  finally
    FreeAndNil(UserSettings);
  end;
end;

// Temporary hack, since TColorGrid.ColorToIndex seems buggy?
function TfmSourceExportOptions.GetColorIndexFromColor(Color: TColor; IsBackground: Boolean): Integer;
var
 i: Integer;
begin
  for i := 0 to 15 do
  begin
    HiddenGrid.ForegroundIndex := i;
    if ColorToRGB(HiddenGrid.ForegroundColor) = ColorToRGB(Color) then
    begin
      Result := HiddenGrid.ForegroundIndex;
      Exit;
    end;
  end;
  // Fallback for unknown colors
  {$IFOPT D+}SendDebugError('Source Export: Unknown color requested!');{$ENDIF}

  if IsBackground then
    Result := 15 // White
  else
    Result := 0; // Black
end;

procedure TfmSourceExportOptions.SynEditSelectionChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Token: {$IFDEF UniSynEdit}UnicodeString{$ELSE}string{$ENDIF};
  Attributes: TSynHighlighterAttributes;
  i: Integer;
begin
  FSampleEditor.GetHighlighterAttriAtRowCol(FSampleEditor.CaretXY, Token, Attributes);
  if Attributes = nil then
    Exit;
  for i := 0 to FSampleEditor.Highlighter.AttrCount - 1 do
  begin
    if Attributes.Name = FSampleEditor.Highlighter.Attribute[i].Name then
    begin
      cbxAttributes.ItemIndex := i;
      cbxAttributes.OnChange(Self);
      Break;
    end;
  end;
end;

procedure TfmSourceExportOptions.btnBackgroundColorClick(Sender: TObject);
begin
  dlgBackground.Color := BackgroundColor;
  if dlgBackground.Execute then
    BackgroundColor := dlgBackground.Color;
end;

{$ELSE not SYNEDIT}
interface implementation
{$ENDIF SYNEDIT}

end.

