unit GX_eDate;

interface

uses
  Classes, Controls, Forms, StdCtrls,
  GX_EditorExpert, GX_ConfigurationInfo;

type
  TDateExpert = class(TEditorExpert)
  private
    FDateFormat: string;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
    property DateFormat: string read FDateFormat write FDateFormat;
  end;

type
  TfmDateFormat = class(TForm)
    lblFormat: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

uses
  SysUtils, GX_OtaUtils;

{$R *.dfm}

{ TDateExpert }

procedure TDateExpert.Configure;
begin
  with TfmDateFormat.Create(nil) do
  try
    cbFormat.Text := DateFormat;
    if ShowModal = mrOk then
      DateFormat := cbFormat.Text;
  finally
    Free;
  end;
end;

constructor TDateExpert.Create;
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('A');
  FDateFormat := ShortDateFormat;
end;

procedure TDateExpert.Execute(Sender: TObject);
resourcestring
  SInvalidDateTimeFormat = 'Invalid date/time format';
var
  InsertString: string;
begin
  try
    InsertString := FormatDateTime(FDateFormat, Date + Time);
  except
    on E: EConvertError do
      InsertString := SInvalidDateTimeFormat;
  end;
  GxOtaInsertLineIntoEditor(InsertString);
end;

function TDateExpert.GetDisplayName: string;
resourcestring
  SDateExpertName = 'Insert Date/Time';
begin
  Result := SDateExpertName;
end;

procedure TDateExpert.GetHelpString(List: TStrings);
resourcestring
  SDateExpertHelp =
    '  This expert inserts the current date/time at the cursor position in ' +
    'the code editor. The format of the date/time text is configurable ' +
    'using standard VCL date format specifiers.  See the FormatDateTime ' +
    'help topic in the VCL documentation for full details.';
begin
  List.Text := SDateExpertHelp;
end;

class function TDateExpert.GetName: string;
begin
  Result := 'DateTime';
end;

procedure TDateExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  DateFormat := Settings.ReadString(ConfigurationKey, 'Format', FDateFormat); // Do not localize
end;

procedure TDateExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  Settings.WriteString(ConfigurationKey, 'Format', DateFormat); // Do not localize
end;

initialization
  RegisterEditorExpert(TDateExpert);
end.

