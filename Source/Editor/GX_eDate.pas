unit GX_eDate;

interface

{$I GX_CondDefine.inc}

uses
  Classes, Controls, Forms, StdCtrls,
  GX_EditorExpert, GX_ConfigurationInfo, GX_BaseForm;

type
  TDateExpert = class(TEditorExpert)
  private
    FDateFormat: string;
  protected
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    property DateFormat: string read FDateFormat write FDateFormat;
  end;

type
  TfmDateFormat = class(TfmBaseForm)
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
  FDateFormat := {$IFDEF GX_VER220_up}FormatSettings.{$ENDIF}ShortDateFormat;
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
  IncCallCount;
end;

function TDateExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + Ord('A');
end;

function TDateExpert.GetDisplayName: string;
resourcestring
  SDateExpertName = 'Insert Date/Time';
begin
  Result := SDateExpertName;
end;

function TDateExpert.GetHelpString: string;
resourcestring
  SDateExpertHelp =
    '  This expert inserts the current date/time at the cursor position in ' +
    'the code editor. The format of the date/time text is configurable ' +
    'using standard VCL date format specifiers.  See the FormatDateTime ' +
    'help topic in the VCL documentation for full details.';
begin
  Result := SDateExpertHelp;
end;

class function TDateExpert.GetName: string;
begin
  Result := 'DateTime';
end;

procedure TDateExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);
  DateFormat := Settings.ReadString('Format', FDateFormat); // Do not localize
end;

procedure TDateExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);
  Settings.WriteString('Format', DateFormat); // Do not localize
end;

initialization
  RegisterEditorExpert(TDateExpert);
end.

