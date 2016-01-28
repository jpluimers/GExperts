// the code formatter expert as an editor expert
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_eCodeFormatter;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_EditorExpert,
  GX_CodeFormatterExpert,
  GX_ConfigurationInfo,
  GX_KbdShortCutBroker;

type
  TeCodeFormatterExpert = class(TEditorExpert)
  private
    FExpert: TCodeFormatterExpert;
    FTwoKeyShortcut: IGxTwoKeyShortCut;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function GetBitmapFileName: string; override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus;

{ TeCodeFormatterExpert }

procedure TeCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TeCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;

  ShortCut := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt]);

  FTwoKeyShortcut := GxKeyboardShortCutBroker.RequestTwoKeyShortCut(Execute, 'F', DisplayName);
end;

destructor TeCodeFormatterExpert.Destroy;
begin
  FreeAndNil(FExpert);
  inherited;
end;

procedure TeCodeFormatterExpert.Execute(Sender: TObject);
begin
  FExpert.Execute;
end;

function TeCodeFormatterExpert.GetBitmapFileName: string;
begin
  Result := 'TCodeFormatterExpert';
end;

function TeCodeFormatterExpert.GetDisplayName: string;
begin
  Result := 'Code Formatter';
end;

procedure TeCodeFormatterExpert.GetHelpString(List: TStrings);
resourcestring
  SFormatterHelp =
    'This expert is the source code formatter formerly known as ' +
    'DelForEx. To switch between different configuration sets, ' +
    'use the tools button.' + sLineBreak +
    'To force a configuration set for a particular unit, add' + sLineBreak +
    '    {GXFormatter.config=<configname>}' + sLineBreak +
    'as the first line to the unit.' + sLineBreak +
    'You can also use a GXFormatter.ini per directory with content ' +
    'like:' + sLineBreak +
    '    [FileSettings]' + sLineBreak +
    '    <mask>=<configname>' + sLineBreak +
    'The formatter will use the first match for the file name. ' +
    'An empty <configname> means that it should use the ' +
    'as configured in this dialog.' + sLineBreak +
    'You can also export your own configuration as' + sLineBreak +
    '    FormatterSettings-<YourName>.ini' + sLineBreak +
    'and put it into the GExperts installation directory.' + sLineBreak +
    'After doing this <YourName> can be used as <configname> as ' +
    'described above.';
begin
  List.Text := SFormatterHelp;
end;

class function TeCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TeCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TeCodeFormatterExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(ConfigurationKey, Settings);
end;

procedure TeCodeFormatterExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(ConfigurationKey, Settings);
end;

initialization
  RegisterEditorExpert(TeCodeFormatterExpert);
end.
