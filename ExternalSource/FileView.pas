unit FileView;

{$I GX_CondDefine.inc}

interface

{.$DEFINE HAVEJPEG}

uses
  {$IFDEF SYNEDIT}
  SynEdit, GX_SynMemoUtils, // See GX_CondDefine.inc to remove the SynEdit requirement
  {$ENDIF SYNEDIT}
  {$IFDEF HAVEJPEG}
  jpeg,
  {$ENDIF HAVEJPEG}
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
  Controls, Forms, Graphics, StdCtrls, SysUtils;

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
    Strings: TStrings;
    WasBinary: Boolean;
  begin
    FRichEdit.Visible := False;
    FImage.Visible := False;
    if IsForm(FileName) then
    begin
      Strings := TStringList.Create;
      try
        LoadFormFileToStrings(FileName, Strings, WasBinary);
        FEditor.Lines.Text := Strings.Text;
      finally
        FreeAndNil(Strings);
      end;
    end
    else
      FEditor.Lines.LoadFromFile(FileName); // Slow for large .sql files (4K lines), for example!
    if (FGXSyntaxParser <> Parser) then
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

    if {$IFDEF HAVEJPEG} (Ext = '.JPG') or {$ENDIF HAVEJPEG}
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
        or (Ext = '.TXT') or (Ext = '.ASC') or (Ext = '.ME') or (Ext = '.INI')
        or (Ext = '.PAS') or (Ext = '.DPR') or (Ext = '.INC') or (Ext = '.DPK')
        or (Ext = '.BPG') or (Ext = '.PY') or (Ext = '.BAT') or (Ext = '.BPR')
        or (Ext = '.HTML') or (Ext = '.HTM') or (Ext = '.DIZ')
        or (Ext = '.C') or (Ext = '.CPP') or (Ext = '.H') or (Ext = '.HPP')
        or (Ext = '.SQL') or (Ext = '.XML') or (Ext = '.RC') or (Ext = '.BDSGROUP')
        or (Ext = '.BDSPROJ') or (Ext = '.DPROJ') or (Ext = '.DOF') or (Ext = '.DSK') or (Ext = '.ISS')
        or (Ext = '.BPF') or (Ext = '.KOF') or (Ext = '.DEBUGLOG') or (Ext = '.CFG')
        or (Ext = '.DRC') or (Ext = '.MAP') or (Ext = '.CONF')
        or (Ext = '.LOCAL') or (Ext = '.LOG') or IsForm(FileName)
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
    else
    if (Ext = '.TXT') or (Ext = '.ASC') or (Ext = '.ME') or (Ext = '.DIZ')
      or (Ext = '.INI') or (Ext = '.BPG') or (Ext = '.PY')  or (Ext = '.BAT')
      or (Ext = '.RC')  or (Ext = '.DOF') or (Ext = '.DSK') or (Ext = '.ISS')
      or (Ext = '.KOF') or (Ext = '.DEBUGLOG') or (Ext = '.CFG')
      or (Ext = '.DRC') or (Ext = '.MAP') or (Ext = '.CONF')
      or (Ext = '.LOG') then
    begin
      {$IFDEF SYNEDIT}
      AssignParser(gxpNone)
      {$ENDIF SYNEDIT}
    end
    else
      if (Ext = '.PAS') or (Ext = '.DPR') or (Ext = '.INC') or (Ext = '.DPK') or IsForm(FileName) then
        AssignParser(gxpPAS)
      else if (Ext = '.HTML') or (Ext = '.HTM') or (Ext = '.ASP') then
        AssignParser(gxpHTML)
      else if (Ext = '.C') or (Ext = '.CPP') or (Ext = '.H') or (Ext = '.HPP') or (Ext = '.BPF') then
        AssignParser(gxpCPP)
      else if (Ext = '.SQL') then
        AssignParser(gxpSQL)
      else if (Ext = '.CS') then
        AssignParser(gxpCS)
      else if (Ext = '.XML') or (Ext = '.BDSPROJ') or (Ext = '.DPROJ') or (Ext = '.BDSGROUP') or (Ext = '.LOCAL')  or (Ext = '.BPR') then
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

