unit GX_ePasteAs;

interface

uses
  Windows, SysUtils, Classes, StdCtrls, Controls, Forms,
  GX_BaseForm, GX_eSelectionEditorExpert, GX_EditorExpert, GX_ConfigurationInfo;

type
  TPasteAsExpert = class(TEditorExpert)
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TCopyRawStringsExpert = class(TSelectionEditorExpert)
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TConvertRawStringsExpert = class(TSelectionEditorExpert)
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDisplayName: string; override;
    procedure GetHelpString(List: TStrings); override;
    procedure Configure; override;
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
  GX_PasteAs, Clipbrd, Dialogs;

{$R *.dfm}

{ TPasteAsExpert }

constructor TPasteAsExpert.Create;
begin
  inherited Create;
  { TODO -oanyone : this shortcut conflicts with the Declare Variable refactoring }
  //  ShortCut := scCtrl + scShift + Ord('V');
end;

function TPasteAsExpert.GetDisplayName: string;
resourcestring
  SPasteAsName = 'Paste Strings As';
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
    '  This expert inserts text lines from the clipboard into the code editor as properly formatted Delphi code.  ' +
    'It can convert things like multi-line SQL statements or long error messages into sets of string constants, '+
    'TStrings.Add() statements, etc.  To use it, copy some text to the clipboard, put the edit cursor in the '+
    'desired location, and execute this editor expert.' + sLineBreak +
    '  You can configure it to ask what type of prefix/suffix to use on each line and whether to ' +
    'add a space to the end of each line''s text.';
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
      PasteAsHandler.ConvertToCode(ALines, False);
  finally
    ALines.Free;
  end;
end;

{ TCopyRawStringsExpert }

constructor TCopyRawStringsExpert.Create;
begin
  inherited Create;
end;

function TCopyRawStringsExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + Ord('C');
end;

function TCopyRawStringsExpert.GetDisplayName: string;
resourcestring
  SCopyRawStringsName = 'Copy Raw Strings';
begin
  Result := SCopyRawStringsName;
end;

class function TCopyRawStringsExpert.GetName: string;
begin
  Result := 'CopyRawStrings';
end;

procedure TCopyRawStringsExpert.GetHelpString(List: TStrings);
resourcestring
  SCopyRawStringsHelp =
    '  This expert copies code lines to the clipboard and removes the prefixes/suffixes around the strings ' +
    'that are used to make them proper Delphi code, leaving you with just the raw strings.  ' +
    'You might use it to take a set of string constants (lines of SQL, for example) and ' +
    'convert them back into the raw text.' + sLineBreak +
    '  To use it, select a block containing the string constants in the Delphi editor and ' +
    'activate this expert.';
begin
  List.Text := SCopyRawStringsHelp;
end;

function TCopyRawStringsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TCopyRawStringsExpert.ProcessSelected(Lines: TStrings): Boolean;
begin
  PasteAsHandler.ExtractRawStrings(Lines, False);
  Clipboard.AsText := Lines.Text;
  Result := False;
end;

{ TConvertRawStringsExpert }

constructor TConvertRawStringsExpert.Create;
begin
  inherited Create;
  { TODO -oanyone : this shortcut conflicts with the Record Macro shortcut }
  // ShortCut := scCtrl + scShift + Ord('R');
end;

function TConvertRawStringsExpert.GetDisplayName: string;
resourcestring
  SConvertRawStringsName = 'Convert Raw Strings';
begin
  Result := SConvertRawStringsName;
end;

class function TConvertRawStringsExpert.GetName: string;
begin
  Result := 'ConvertRawStrings';
end;

procedure TConvertRawStringsExpert.GetHelpString(List: TStrings);
resourcestring
  SConvertRawStringsHelp =
    '  This expert takes the selected code lines and removes the prefixes/suffixes around the strings ' +
    'that are used to make them proper Delphi code, leaving you with just the raw strings.  ' +
    'It then uses the selected string prefix/suffix combination to paste the lines back into the editor.' + sLineBreak +
    '  To use it, select the string constants in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    '  You can configure this expert to use different string prefix/suffix combinations. Note that ' +
    'it shares this configuration with the PasteAs expert.';
begin
  List.Text := SConvertRawStringsHelp;
end;

procedure TConvertRawStringsExpert.Configure;
begin
  PasteAsHandler.ExecuteConfig(Self, True);
end;

function TConvertRawStringsExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TConvertRawStringsExpert.ProcessSelected(Lines: TStrings): Boolean;
begin
  Result := PasteAsHandler.ExecuteConfig(Self, False);
  if Result then
  begin
    PasteAsHandler.ExtractRawStrings(Lines, True);
    PasteAsHandler.ConvertToCode(Lines, True);
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
  RegisterEditorExpert(TCopyRawStringsExpert);
  RegisterEditorExpert(TConvertRawStringsExpert);

end.
