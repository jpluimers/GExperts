unit FileView;

{$I GX_CondDefine.inc}

interface

uses
  {$IFDEF SYNEDIT}
  SynEdit, GX_SynMemoUtils, // See GX_CondDefine.inc to remove the SynEdit requirement
  {$ENDIF SYNEDIT}
  Classes, ExtCtrls, ComCtrls, GX_GenericUtils;

type
  TFileViewer = class(TCustomPanel)
  private
    FImage: TImage;
    FRichEdit: TRichEdit;
    FLoadedFile: string;
    {$IFDEF SYNEDIT}
    FEditor: TSynEdit;
    FGXSyntaxParser: TGXSyntaxHighlighter;
    {$ENDIF SYNEDIT}
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure Clear;
  published
    property Align;
    property LoadedFile: string read FLoadedFile write FLoadedFile;
  end;

procedure Register;

implementation

uses
  Controls, Forms, Graphics, StdCtrls, SysUtils, GX_OtaUtils;

constructor TFileViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  Color := clWindow;
  if not (csDesigning in ComponentState) then
  begin
    // Image Viewer
    FImage := TImage.Create(nil);
    FImage.Parent := Self;
    FImage.Align := alClient;
    FImage.Visible := False;

    // Rich Text Viewer
    FRichEdit := TRichEdit.Create(nil);

    FRichEdit.Parent := Self;
    FRichEdit.Align := alClient;
    FRichEdit.WordWrap := True;
    FRichEdit.ScrollBars := ssBoth;
    FRichEdit.ReadOnly := True;
    FRichEdit.Visible := False;
    FRichEdit.BorderStyle := bsNone;

    {$IFDEF SYNEDIT}
    // SynEdit Control
    FEditor := TSynEdit.Create(nil);
    with FEditor do
    begin
      Options := Options - [eoScrollPastEof, eoScrollPastEol];
      Parent := Self;
      Align := alClient;
      BorderStyle := bsNone;
      ReadOnly := True;
      Gutter.Width := 0;
      Visible := False;
    end;
    {$ENDIF SYNEDIT}
  end;
end;

destructor TFileViewer.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    FreeAndNil(FImage);
    FreeAndNil(FRichEdit);

    {$IFDEF SYNEDIT}
    FEditor.Highlighter.Free;
    FEditor.Highlighter := nil;
    FreeAndNil(FEditor);
    {$ENDIF SYNEDIT}
  end;

  inherited Destroy;
end;

procedure TFileViewer.Clear;
begin
  LoadedFile := '';
  FImage.Visible := False;
  FRichEdit.Visible := False;
  {$IFDEF SYNEDIT}
  FEditor.Visible := False;
  {$ENDIF SYNEDIT}
end;

procedure TFileViewer.LoadFromFile(const FileName: string);

  {$IFDEF SYNEDIT}
  procedure AssignParser(Parser: TGXSyntaxHighlighter);
  var
    Strings: TGXUnicodeStringList;
  begin
    FRichEdit.Visible := False;
    FImage.Visible := False;
    Strings := TGXUnicodeStringList.Create;
    try
      GxOtaLoadFileToUnicodeStrings(FileName, Strings);
      FEditor.Lines.Text := Strings.Text;
    finally
      FreeAndNil(Strings);
    end;
    begin
      SetSynEditHighlighter(FEditor, Parser);
      FGXSyntaxParser := Parser;
    end;
    FEditor.Visible := True;
  end;
  {$ENDIF SYNEDIT}

var
  Ext: string;
begin
  LoadedFile := FileName;
  {$IFDEF SYNEDIT}
  FEditor.BeginUpdate;
  {$ENDIF SYNEDIT}
  FRichEdit.Lines.BeginUpdate;
  try
    Ext := UpperCase(ExtractFileExt(FileName));

    if (Ext = '.JPG') or
       {$IFDEF GX_VER185_up} (Ext = '.GIF') or {$ENDIF}
       {$IFDEF GX_VER200_up} (Ext = '.PNG') or {$ENDIF}
       (Ext = '.BMP') or (Ext = '.ICO') then
    begin
      FRichEdit.Visible := False;
      {$IFDEF SYNEDIT}
      FEditor.Visible := False;
      {$ENDIF SYNEDIT}
      FImage.Picture.LoadFromFile(FileName);
      FImage.Visible := True;
    end
    else
    if (Ext = '.RTF')
    {$IFNDEF GX_ENHANCED_EDITOR}
        or IsPascalSourceFile(FileName)
        or IsHtml(FileName)
        or IsWebFile(FileName)
        or IsCppSourceModule(FileName)
        or IsSQL(FileName)
        or IsCS(FileName)
        of IsXMLFormat(FileName)
        or IsForm(FileName)
    {$ENDIF GX_ENHANCED_EDITOR}
    then
    begin
      FRichEdit.PlainText := not(Ext = '.RTF');
      FImage.Visible := False;
      {$IFDEF SYNEDIT}
      FEditor.Visible := False;
      {$ENDIF SYNEDIT}
      FRichEdit.Lines.LoadFromFile(FileName);
      FRichEdit.Visible := True;
    end
    {$IFDEF GX_ENHANCED_EDITOR}
    else if IsTextFile(FileName) or (IsWebFile(FileName) and not IsHtml(FileName)) then
    begin
      {$IFDEF SYNEDIT}
      AssignParser(gxpNone)
      {$ENDIF SYNEDIT}
    end
    else
      if IsPascalSourceFile(FileName) or IsForm(FileName) then
        AssignParser(gxpPAS)
      else if IsHtml(FileName) then
        AssignParser(gxpHTML)
      else if IsCppSourceModule(FileName) then
        AssignParser(gxpCPP)
      else if IsSQL(FileName) then
        AssignParser(gxpSQL)
      else if (Ext = '.CS') then
        AssignParser(gxpCS)
      else if IsXMLFormat(FileName) then
        AssignParser(gxpXML)
    {$ENDIF GX_ENHANCED_EDITOR}
    else
      Clear;
  finally
    {$IFDEF SYNEDIT}
    FEditor.EndUpdate;
    {$ENDIF SYNEDIT}
    FRichEdit.Lines.EndUpdate;
  end;
end;

procedure Register;
begin
  RegisterComponents('User', [TFileViewer]);
end;

end.

