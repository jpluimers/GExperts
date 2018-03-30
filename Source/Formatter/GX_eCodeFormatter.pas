// the code formatter expert as an editor expert
// Original Author:     Thomas Mueller (http://blog.dummzeuch.de)
unit GX_eCodeFormatter;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_EditorExpert,
  GX_CodeFormatterExpert,
  GX_ConfigurationInfo,
  GX_KbdShortCutBroker,
  GX_GenericUtils;

type
  TeCodeFormatterExpert = class(TEditorExpert)
  private
    FExpert: TCodeFormatterExpert;
  protected
    function GetBitmapFileName: string; override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    function IsDefaultActive: Boolean; override;
    procedure AddToCapitalization(const _Identifier: TGXUnicodeString);
  end;

var
  gblCodeFormatter: TeCodeFormatterExpert;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  GX_IdeUtils;

{ TeCodeFormatterExpert }

procedure TeCodeFormatterExpert.AddToCapitalization(const _Identifier: TGXUnicodeString);
var
  GExpertsSettings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  FExpert.AddToCapitalization(_Identifier);

  ExpSettings := nil;
  GExpertsSettings := TGExpertsSettings.Create;
  try
    ExpSettings := GExpertsSettings.CreateExpertSettings(ConfigurationKey);
    InternalSaveSettings(ExpSettings);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(GExpertsSettings);
  end;
end;

procedure TeCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TeCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;
  gblCodeFormatter := Self;
end;

destructor TeCodeFormatterExpert.Destroy;
begin
  gblCodeFormatter := nil;
  FreeAndNil(FExpert);
  inherited;
end;

function TeCodeFormatterExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningRSXEOrGreater;
end;

procedure TeCodeFormatterExpert.Execute(Sender: TObject);
begin
  if FExpert.Execute then
    IncCallCount;
end;

function TeCodeFormatterExpert.GetBitmapFileName: string;
begin
  Result := 'TCodeFormatterExpert';
end;

function TeCodeFormatterExpert.GetDefaultShortCut: TShortCut;
begin
  if RunningRS2010OrGreater then begin
    // Strarting with Delphi 2010 it has a built in code formatter
    // it doesn't work very well in the early incarnations but
    // if the user wants to replace it, he should configure it himself.
    Result := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt])
  end else begin
    // for older versions, hijack the Ctrl+D shortcut that in later versions is
    // used by the built in code formatter
    Result := Menus.ShortCut(Word('D'), [ssCtrl]);
  end;
end;

function TeCodeFormatterExpert.GetDisplayName: string;
begin
  Result := 'Code Formatter';
end;

function TeCodeFormatterExpert.GetHelpString: string;
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
    'An empty <configname> means that it should work ' +
    'as configured in this dialog.' + sLineBreak +
    'You can also export your own configuration as' + sLineBreak +
    '    FormatterSettings-<YourName>.ini' + sLineBreak +
    'and put it into the GExperts installation directory.' + sLineBreak +
    'After doing this <YourName> can be used as <configname> as ' +
    'described above.';
begin
  Result := SFormatterHelp;
end;

class function TeCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TeCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TeCodeFormatterExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(Settings);
end;

procedure TeCodeFormatterExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(Settings);
end;

initialization
  RegisterEditorExpert(TeCodeFormatterExpert);
end.
