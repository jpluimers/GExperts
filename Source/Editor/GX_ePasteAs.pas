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
  GX_PasteAs, Clipbrd, Dialogs;

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
    '  This expert copies code lines to the clipboard and removes the prefixes/suffixes around the strings ' +
    'that are used to make them proper Delphi code, leaving you with just the raw strings.  ' +
    'You might use it to take a set of string constants (lines of SQL, for example) and ' +
    'convert them back into the raw text.' + sLineBreak +
    'To use it, select a block containing the string constants in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different string prefix/suffix combinations.';
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
    '  This expert copies code lines to the clipboard and removes the prefixes/suffixes around the strings ' +
    'that are used to make them proper Delphi code, leaving you with just the raw strings.  ' +
    'It then uses the selected string prefix/suffix combination to paste the lines back into the editor.' + sLineBreak +
    '  To use it, select the string constants in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different string prefix/suffix combinations.';
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
