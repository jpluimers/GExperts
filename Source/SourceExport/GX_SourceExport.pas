unit GX_SourceExport;

// Original Author: ArentJan Banck <ajbanck@davilex.nl>

{$I GX_CondDefine.inc}

{$IFDEF SYNEDIT}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, ActnList, ComCtrls,
  Menus, StdCtrls, ExtCtrls, ToolWin,
  SynEdit, // This expert requires SynEdit from http://synedit.sf.net/
  GX_Experts, GX_ConfigurationInfo;

type
  TGXCopyFormat = (cfText, cfHTMLFragment, cfRTFFragment);

  TSourceExportExpert = class;

  TfmSourceExport = class(TForm)
    pnlFooter: TPanel;
    dlgSave: TSaveDialog;
    edtTitle: TEdit;
    lblTitle: TLabel;
    pmuCopy: TPopupMenu;
    mitCopyHtml: TMenuItem;
    mitCopyRtf: TMenuItem;
    mitCopy: TMenuItem;
    Actions: TActionList;
    actFileRefresh: TAction;
    actFileSave: TAction;
    actCopy: TAction;
    actCopyTextRtfHtml: TAction;
    actCopyHtmlFragment: TAction;
    actCopyRtfFragment: TAction;
    actFilePrint: TAction;
    actFileConfigure: TAction;
    actHelpHelp: TAction;
    actFileExit: TAction;
    pnlEditor: TPanel;
    pnlButtons: TPanel;
    pnlButtonsRight: TPanel;
    btnClose: TButton;
    btnConfig: TButton;
    btnPrint: TButton;
    btnCopy: TButton;
    btnSave: TButton;
    ToolBar: TToolBar;
    tbnRefresh: TToolButton;
    tbnSave: TToolButton;
    tbnCopy: TToolButton;
    ToolButton3: TToolButton;
    tbnPrint: TToolButton;
    ToolButton2: TToolButton;
    tbnConfigure: TToolButton;
    ToolButton1: TToolButton;
    tbnHelp: TToolButton;
    procedure actHelpExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actConfigureExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actCopyHtmlExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyRtfExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCopyTextRtfHtmlExecute(Sender: TObject);
  private
    FEditor: TSynEdit;
    procedure LoadSettings;
    procedure CopyToClipboard(CopyFormat: TGXCopyFormat);
    function FillEditControlWithIdeData: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSourceExportExpert = class(TGX_Expert)
  private
    // Persistent configuration options
    FDefaultCopyFormat: TGXCopyFormat;
    FSaveDir: string;
    FSaveFilter: Integer;
    FBackgroundColor: TColor;
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Configure; override;
    procedure Click(Sender: TObject); override;
    procedure UpdateAction(Action: TCustomAction); override;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, Clipbrd,
  SynEditExport, SynExportHtml, SynExportRtf, SynEditPrint,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_IdeUtils,
  GX_SynMemoUtils, GX_SourceExportOptions, GX_SharedImages, SynUnicode;

const
  HighlighterDefaultRegKey = '\SourceExport\Highlighters\';

var
  SourceExportExpert: TSourceExportExpert = nil;

function TfmSourceExport.FillEditControlWithIdeData: Boolean;
var
  Lines: TGXUnicodeString;
begin
  Assert(Assigned(FEditor));

  FEditor.ClearAll;
  SetSynEditHighlighter(FEditor, GetGXHighlighterForCurrentSourceEditor);

  Result := GxOtaGetActiveEditorTextAsUnicodeString(Lines);
  FEditor.Lines.Text := Lines;
end;

procedure TfmSourceExport.LoadSettings;
resourcestring
  SCopyText = 'Copy';
  SCopyHTML = 'Copy as HTML';
  SCopyRTF  = 'Copy as RTF';
var
  CaptionText: string;
begin
  Assert(Assigned(SourceExportExpert));
  Assert(Assigned(FEditor));

  FEditor.Highlighter.LoadFromRegistry(HKEY_CURRENT_USER, ConfigInfo.GetGExpertsIdeRootRegistryKey +
        HighlighterDefaultRegKey + FEditor.Highlighter.LanguageName);

  case SourceExportExpert.FDefaultCopyFormat of
    cfText:         CaptionText := SCopyText;
    cfHTMLFragment: CaptionText := SCopyHTML;
    cfRTFFragment:  CaptionText := SCopyRTF;
    else            Assert(False, 'Invalid TGXCopyFormat');
  end;

  btnCopy.Caption := '&' + CaptionText;
  tbnCopy.Hint := CaptionText;
  mitCopy.Default := True;
end;

procedure TfmSourceExport.FormActivate(Sender: TObject);
resourcestring
  SDialogFragmentExportTitle = 'Fragment of %s';
var
  CurrentModuleFileName: string;
  HasBlockSelection: Boolean;
begin
  CurrentModuleFileName := ExtractFileName(GxOtaGetTopMostEditBufferFileName);

  HasBlockSelection := FillEditControlWithIdeData;
  LoadSettings;
  if HasBlockSelection then
    edtTitle.Text := Format(SDialogFragmentExportTitle, [CurrentModuleFileName])
  else
    edtTitle.Text := CurrentModuleFileName;
end;

procedure TfmSourceExport.actRefreshExecute(Sender: TObject);
begin
  FillEditControlWithIdeData;
  LoadSettings;
end;

procedure TfmSourceExport.actSaveExecute(Sender: TObject);
resourcestring
  SDialogTitle = 'Save %s As';
var
  CurrentIdeDir: string;
  Exporter: TSynCustomExporter;
begin
  Assert(Assigned(SourceExportExpert));

  dlgSave.Title := Format(SDialogTitle, [edtTitle.Text]);
  dlgSave.FileName := ChangeFileExt(edtTitle.Text, '');

  dlgSave.InitialDir := SourceExportExpert.FSaveDir;
  dlgSave.FilterIndex := SourceExportExpert.FSaveFilter;

  CurrentIdeDir := GetCurrentDir;
  try
    if GetOpenSaveDialogExecute(dlgSave) then
    begin
      SourceExportExpert.FSaveDir := ExtractFilePath(ExpandFileName(dlgSave.FileName));
      SourceExportExpert.FSaveFilter := dlgSave.FilterIndex;
      if dlgSave.FilterIndex = 1 then
        Exporter := TSynExporterHTML.Create(nil)
      else
        Exporter := TSynExporterRTF.Create(nil);
      try
        Exporter.Title := ExtractFileName(dlgSave.FileName);
        Exporter.UseBackground := True;
        Exporter.Highlighter := FEditor.Highlighter;
        Exporter.ExportAsText := True;
        Exporter.Color := SourceExportExpert.FBackgroundColor;
        Exporter.ExportAll(FEditor.Lines);
        Exporter.SaveToFile(dlgSave.FileName);
      finally
        FreeAndNil(Exporter);
      end;
    end;
  finally
    SetCurrentDir(CurrentIdeDir);
  end;
end;

procedure TfmSourceExport.CopyToClipboard(CopyFormat: TGXCopyFormat);

  procedure ExportToClipboard(Exporter: TSynCustomExporter; AsText: Boolean);
  begin
    if Exporter = nil then
    begin
      Clipboard.AsText := FEditor.Lines.Text;
      Exit;
    end;
    if AsText and (Exporter is TSynExporterHTML) then
      (Exporter as TSynExporterHTML).CreateHTMLFragment := True;
    Exporter.Title := edtTitle.Text;
    Exporter.ExportAsText := AsText;
    Exporter.Highlighter := FEditor.Highlighter;
    Exporter.UseBackground := True;
    Exporter.ExportAll(FEditor.Lines);
    Exporter.CopyToClipboard;
  end;

var
  HtmlExporter: TSynCustomExporter;
  RtfExporter: TSynCustomExporter;
begin
  Assert(Assigned(FEditor));
  HtmlExporter := nil;
  RtfExporter := nil;
  try
    HtmlExporter := TSynExporterHTML.Create(nil);
    RtfExporter := TSynExporterRTF.Create(nil);

    case CopyFormat of
      cfText:
        begin
          Clipboard.Open;
          try
            ExportToClipboard(nil, False);
            ExportToClipboard(HtmlExporter, False);
            ExportToClipboard(RtfExporter, False);
          finally
            Clipboard.Close;
          end;
        end;
      cfHTMLFragment:
        ExportToClipboard(HtmlExporter, True);
      cfRTFFragment:
        ExportToClipboard(RtfExporter, True);
      else
        Assert(False, 'Unknown export type');
    end;
  finally
    FreeAndNil(HtmlExporter);
    FreeAndNil(RtfExporter);
  end;
end;

procedure TfmSourceExport.actCopyHtmlExecute(Sender: TObject);
begin
  CopyToClipboard(cfHTMLFragment);
end;

procedure TfmSourceExport.actCopyRtfExecute(Sender: TObject);
begin
  CopyToClipboard(cfRTFFragment);
end;

procedure TfmSourceExport.actCopyExecute(Sender: TObject);
begin
  Assert(Assigned(SourceExportExpert));
  CopyToClipboard(SourceExportExpert.FDefaultCopyFormat);
end;

procedure TfmSourceExport.actPrintExecute(Sender: TObject);
var
  SynPrint: TSynEditPrint;
  Cursor: IInterface;
begin
  Assert(Assigned(FEditor));
  Assert(Assigned(SourceExportExpert));
  Cursor := TempHourGlassCursor;
  SynPrint := TSynEditPrint.Create(nil);
  try
    SynPrint.SynEdit := FEditor;
    SynPrint.Highlight := (Assigned(FEditor.Highlighter));
    SynPrint.Colors := True;
    SynPrint.TabWidth := 4;
    SynPrint.Wrap := True;
    SynPrint.Title := edtTitle.Text;
    SynPrint.Print
  finally
    FreeAndNil(SynPrint);
  end;
end;

procedure TfmSourceExport.actConfigureExecute(Sender: TObject);
begin
  Assert(Assigned(SourceExportExpert));

  SourceExportExpert.Configure;
  LoadSettings;
end;

procedure TfmSourceExport.actHelpExecute(Sender: TObject);
begin
  GxContextHelp(Self, 26);
end;

procedure TfmSourceExport.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmSourceExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Assert(Assigned(SourceExportExpert));
  SourceExportExpert.SaveSettings;
end;

procedure TfmSourceExport.actCopyTextRtfHtmlExecute(Sender: TObject);
begin
  CopyToClipboard(cfText); // This copies all registered clipboard formats at once
end;

constructor TfmSourceExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetToolbarGradient(ToolBar);
  SetDefaultFont(Self);
  // Destroyed with form
  FEditor := TSynEdit.Create(Self);
  with FEditor do
  begin
    Lines.Clear;
    Parent := pnlEditor;
    Align := alClient;
    TabOrder := 0;
    Gutter.Width := 0;
    TabWidth := 4;
    Options := Options - [eoScrollPastEof, eoScrollPastEol];
  end;

  dlgSave.Options := dlgSave.Options + [ofEnableSizing];
end;

{ TSourceExportExpert }

constructor TSourceExportExpert.Create;
begin
  inherited Create;

  SourceExportExpert := Self;
end;

procedure TSourceExportExpert.Click(Sender: TObject);
var
  Dlg: TfmSourceExport;
begin
  Dlg := TfmSourceExport.Create(nil);
  try
    SetFormIcon(Dlg);
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TSourceExportExpert.InternalLoadSettings(Settings: TGExpertsSettings);
var
  NewCopyFormat: TGXCopyFormat;
begin
  // Do not localize.
  NewCopyFormat := TGXCopyFormat(Settings.ReadEnumerated(ConfigurationKey, 'Copy Format', TypeInfo(TGXCopyFormat), 0));
  Assert(NewCopyFormat in [Low(TGXCopyFormat)..High(TGXCopyFormat)]);
  FDefaultCopyFormat := NewCopyFormat;
  FSaveDir := Settings.ReadString(ConfigurationKey, 'Save Directory', '');
  FSaveFilter := Settings.ReadInteger(ConfigurationKey, 'Save Format', 1);
  FBackgroundColor := Settings.ReadInteger(ConfigurationKey, 'Background', GetIdeEditorBackgroundColor);
end;

procedure TSourceExportExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize.
  Settings.WriteInteger(ConfigurationKey, 'Copy Format', Ord(FDefaultCopyFormat));
  Settings.WriteString(ConfigurationKey, 'Save Directory', FSaveDir);
  Settings.WriteInteger(ConfigurationKey, 'Save Format', FSaveFilter);
  Settings.WriteInteger(ConfigurationKey, 'Background', FBackgroundColor);
end;

procedure TSourceExportExpert.Configure;
var
  Dlg: TfmSourceExportOptions;
  HighlighterRegKey: string;
  NewCopyFormat: TGXCopyFormat;
begin
  Dlg := TfmSourceExportOptions.Create(nil);
  try
    HighlighterRegKey := ConfigInfo.GetGExpertsIdeRootRegistryKey + HighlighterDefaultRegKey
        + Dlg.SynSampleEditor.Highlighter.LanguageName;

    Dlg.rbxCopySettings.ItemIndex := Ord(FDefaultCopyFormat);
    Dlg.SynSampleEditor.Highlighter.LoadFromRegistry(HKEY_CURRENT_USER, HighlighterRegKey);
    Dlg.BackgroundColor := FBackgroundColor;
    if Dlg.ShowModal = mrOk then
    begin
      Dlg.SynSampleEditor.Highlighter.SaveToRegistry(HKEY_CURRENT_USER, HighlighterRegKey);

      NewCopyFormat := TGXCopyFormat(Dlg.rbxCopySettings.ItemIndex);
      Assert(NewCopyFormat in [Low(TGXCopyFormat)..High(TGXCopyFormat)]);
      FDefaultCopyFormat := NewCopyFormat;
      FBackgroundColor := Dlg.BackgroundColor;

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

function TSourceExportExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Source Export...';
begin
  Result := SMenuCaption;
end;

class function TSourceExportExpert.GetName: string;
begin
  Result := 'SourceExport'; // Do not localize.
end;

destructor TSourceExportExpert.Destroy;
begin
  SourceExportExpert := nil;

  inherited Destroy;
end;

procedure TSourceExportExpert.UpdateAction(Action: TCustomAction);
const
  SAllowableFileExtensions = '.pas;.inc;.dpr;.txt;.cpp;.hpp;.c;.h;.sql;.htm;.html;.aspx';
begin
  Action.Enabled := FileMatchesExtensions(GxOtaGetCurrentSourceFile, SAllowableFileExtensions);
end;

initialization
  RegisterGX_Expert(TSourceExportExpert);

{$ELSE not SYNEDIT}
interface implementation
{$ENDIF SYNEDIT}
end.

