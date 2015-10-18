unit GX_ePasteAs;

interface

uses
  Windows, SysUtils, Classes, StdCtrls, Controls, Forms,
  GX_BaseForm, GX_eSelectionEditorExpert, GX_EditorExpert, GX_ConfigurationInfo;

type
  TPasteAsExpert = class(TEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TCopyAsExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TReplaceAsExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TfmPasteAsConfig = class(TfmBaseForm)
    gbxPasteAsOptions: TGroupBox;
    lblMaxEntries: TLabel;
    chkCreateQuotedStrings: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbPasteAsType: TComboBox;
    chkAddExtraSpaceAtTheEnd: TCheckBox;
    chkShowOptions: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

uses
  GX_PasteAs, Clipbrd;

{$R *.dfm}

{ TPasteAsExpert }

constructor TPasteAsExpert.Create;
begin
  inherited Create;
  ShortCut := scCtrl + scShift + Ord('V');
end;

function TPasteAsExpert.GetDisplayName: string;
resourcestring
  SPasteAsName = 'Paste As';
begin
  Result := SPasteAsName;
end;

class function TPasteAsExpert.GetName: string;
begin
  Result := 'PasteAs';
end;

procedure TPasteAsExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  PasteAsHandler.LoadSettings(Settings, ConfigurationKey);
end;

procedure TPasteAsExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  PasteAsHandler.SaveSettings(Settings, ConfigurationKey);
end;

procedure TPasteAsExpert.Configure;
begin
  PasteAsHandler.ExecuteConfig(Self, True);
end;

procedure TPasteAsExpert.GetHelpString(List: TStrings);
resourcestring
  SPasteAsHelp =
    '  This expert insert text from clipboard to string constants. ' +
    'To use it, select a block in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different string separators.';
begin
  List.Text := SPasteAsHelp;
end;

procedure TPasteAsExpert.Execute(Sender: TObject);
var
  ALines: TStringList;
begin
  if not Clipboard.HasFormat(CF_TEXT) then
    Exit;

  ALines := TStringList.Create;
  try
    ALines.Text := Clipboard.AsText;
    if PasteAsHandler.ExecuteConfig(Self, False) then
      PasteAsHandler.ConvertToString(ALines, False);
  finally
    ALines.Free;
  end;
end;

{ TCopyAsExpert }

constructor TCopyAsExpert.Create;
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('C');
end;

function TCopyAsExpert.GetDisplayName: string;
resourcestring
  SCopyAsName = 'Copy As';
begin
  Result := SCopyAsName;
end;

class function TCopyAsExpert.GetName: string;
begin
  Result := 'CopyAs';
end;

procedure TCopyAsExpert.GetHelpString(List: TStrings);
resourcestring
  SCopyAsHelp =
    '  This expert copy a selected block of string constants to clipboard as text. ' +
    'To use it, select a block in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different string separators.';
begin
  List.Text := SCopyAsHelp;
end;

function TCopyAsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TCopyAsExpert.ProcessSelected(Lines: TStrings): Boolean;
begin
  Clipboard.AsText := PasteAsHandler.ConvertFromString(Lines, False);
  Result := False;
end;

{ TReplaceAsExpert }

constructor TReplaceAsExpert.Create;
begin
  inherited Create;
  ShortCut := scCtrl + scShift + Ord('R');
end;

function TReplaceAsExpert.GetDisplayName: string;
resourcestring
  SReplaceAsName = 'Replace As';
begin
  Result := SReplaceAsName;
end;

class function TReplaceAsExpert.GetName: string;
begin
  Result := 'ReplaceAs';
end;

procedure TReplaceAsExpert.GetHelpString(List: TStrings);
resourcestring
  SReplaceAsHelp =
    '  This expert replace a selected block of string constants separator. ' +
    '  This expert comments out a selected block of code. ' +
    'To use it, select a block in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different string separators.';
begin
  List.Text := SReplaceAsHelp;
end;

function TReplaceAsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TReplaceAsExpert.ProcessSelected(Lines: TStrings): Boolean;
begin
  Result := PasteAsHandler.ExecuteConfig(Self, False);
  if Result then
  begin
    PasteAsHandler.ConvertFromString(Lines, True);
    PasteAsHandler.ConvertToString(Lines, True);
  end;
end;

{ TfmPasteAsConfig }

procedure TfmPasteAsConfig.FormCreate(Sender: TObject);
begin
  inherited;
  cbPasteAsType.DropDownCount := Integer(High(TPasteAsType)) + 1;
end;

initialization
  RegisterEditorExpert(TPasteAsExpert);
  RegisterEditorExpert(TCopyAsExpert);
  RegisterEditorExpert(TReplaceAsExpert);

end.
