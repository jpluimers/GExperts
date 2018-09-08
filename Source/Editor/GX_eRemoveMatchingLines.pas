unit GX_eRemoveMatchingLines;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  GX_Experts,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_BaseForm,
  GX_GenericUtils;

type
  ///<summary>
  /// This editor expert removes lines from the current editor that consist only
  /// of the configured text, optionally padded with spaces.
  /// e.g. those pesky comments the IDE adds automatically
  ///    TForm1 = class(TForm)
  ///    private
  ///    { Private declarations }
  ///  public
  ///    { Public declarations }
  ///  end;
  TGxRemoveMatchingLinesEditorExpert = class(TEditorExpert)
  private
    FLines: TStringList;
  public
    constructor Create; override;
    // optional if HasConfigOptions returns false
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    // optional, but recommended
    function GetHelpString: string; override;
    // Overrride to load any configuration settings
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    // Overrride to save any configuration settings
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  end;

type
  TfmRemoveMatchingLinesExpertConfig = class(TfmBaseForm)
    m_Lines: TMemo;
    b_Ok: TButton;
    b_Cancel: TButton;
    l_Lines: TLabel;
    b_Defaults: TButton;
    procedure b_DefaultsClick(Sender: TObject);
  private
    class procedure SetDefault(_sl: TStrings);
    procedure SetData(_sl: TStrings);
    procedure GetData(_sl: TStrings);
  public
    constructor Create(_Owner: TComponent); override;
    class function Execute(_Owner: TComponent; _Lines: TStrings): Boolean;
  end;

implementation

{$R *.dfm}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  Registry,
  ToolsAPI,

  GX_GExperts,
  GX_dzVclUtils,
  GX_OtaUtils;

{ TGxRemoveMatchingLinesEditorExpert }

procedure TGxRemoveMatchingLinesEditorExpert.Configure;
begin
  if TfmRemoveMatchingLinesExpertConfig.Execute(nil, FLines) then
    SaveSettings;
end;

constructor TGxRemoveMatchingLinesEditorExpert.Create;
begin
  inherited Create;

  FLines := TStringList.Create;

  TfmRemoveMatchingLinesExpertConfig.SetDefault(FLines);

  // LoadSettings is called automatically for the expert upon creation.
end;

procedure TGxRemoveMatchingLinesEditorExpert.Execute(Sender: TObject);
var
  Text: TGXUnicodeStringList;
  LineIdx: Integer;
  s: TGXUnicodeString;
  WasChanged: Boolean;
begin
  Text := TGXUnicodeStringList.Create;
  try
    if not GxOtaGetActiveEditorText(Text, False) then
      Exit; //==>

    WasChanged := False;
    for LineIdx := Text.Count - 1 downto 0 do begin
      s := Trim(Text[LineIdx]);
      if s <> '' then begin
        if FLines.IndexOf(s) <> -1 then begin
          Text.Delete(LineIdx);
          WasChanged := True;
        end;
      end;
    end;
    if WasChanged then
      GxOtaReplaceEditorTextWithUnicodeString(GxOtaGetCurrentSourceEditor, Text.Text);
  finally
    FreeAndNil(Text);
  end;
end;

function TGxRemoveMatchingLinesEditorExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Remove Lines';
begin
  Result := SDisplayName;
end;

function TGxRemoveMatchingLinesEditorExpert.GetHelpString: string;
resourcestring
  SRemoveLinesEditorExpertHelp = 'Removes lines from the current editor that consist only '
    + ' of the configured text, optionally padded with spaces.'
    + ' e.g. those pesky comments the IDE adds automatically.';
begin
  Result := SRemoveLinesEditorExpertHelp;
end;

procedure TGxRemoveMatchingLinesEditorExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);

  Settings.ReadStrings('Lines', FLines);
end;

procedure TGxRemoveMatchingLinesEditorExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);

  Settings.WriteStrings('Lines', FLines);
end;

class procedure TfmRemoveMatchingLinesExpertConfig.SetDefault(_sl: TStrings);
begin
  _sl.Clear;

  // English
  _sl.Add('{ Private declarations }');
  _sl.Add('{ Protected declarations }');
  _sl.Add('{ Public declarations }');
  _sl.Add('{ Published declarations }');

  // French
  _sl.Add('{ déclarations privées }');
  _sl.Add('{ déclarations protégées }');
  _sl.Add('{ déclarations publiques }');
  _sl.Add('{ déclarations publiées }');

  // German
  _sl.Add('{ Private-Deklarationen }');
  _sl.Add('{ Protected-Deklarationen }');
  _sl.Add('{ Public-Deklarationen }');
  _sl.Add('{ Published-Deklarationen }');
end;

class function TfmRemoveMatchingLinesExpertConfig.Execute(_Owner: TComponent; _Lines: TStrings): Boolean;
var
  frm: TfmRemoveMatchingLinesExpertConfig;
begin
  frm := TfmRemoveMatchingLinesExpertConfig.Create(nil);
  try
    frm.SetData(_Lines);
    Result := (frm.ShowModal = mrOk);
    if Result then begin
      frm.GetData(_Lines);
    end;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmRemoveMatchingLinesExpertConfig.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

procedure TfmRemoveMatchingLinesExpertConfig.b_DefaultsClick(Sender: TObject);
begin
  if m_Lines.Lines.Count > 0 then
    if mrYes <> MessageDlg('Do you really want to overwrite the existing lines?', mtConfirmation, [mbYes, mbCancel], 0) then
      Exit; //==>
  SetDefault(m_Lines.Lines);
end;

procedure TfmRemoveMatchingLinesExpertConfig.GetData(_sl: TStrings);
var
  i: Integer;
  s: string;
begin
  _sl.Clear;
  for i := 0 to m_Lines.Lines.Count - 1 do begin
    s := Trim(m_Lines.Lines[i]);
    if s <> '' then
      _sl.Add(s);
  end;
end;

procedure TfmRemoveMatchingLinesExpertConfig.SetData(_sl: TStrings);
begin
  m_Lines.Lines.Assign(_sl);
end;

initialization
  RegisterEditorExpert(TGxRemoveMatchingLinesEditorExpert);
end.
