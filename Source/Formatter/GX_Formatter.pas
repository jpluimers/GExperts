// the code formatter expert as a regular expert
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_Formatter;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_GenericUtils,
  GX_Experts,
  GX_CodeFormatterExpert,
  GX_ConfigurationInfo;

type
  TGxCodeFormatterExpert = class(TGX_Expert)
  private
    FExpert: TCodeFormatterExpert;
  protected
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    function HasConfigOptions: Boolean; override;
    function HasMenuItem: Boolean; override;
  end;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  GX_GExperts,
  GX_About;

  { TCodeFormatterExpert }

procedure TGxCodeFormatterExpert.Execute(Sender: TObject);
begin
  FExpert.Execute;
end;

procedure TGxCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TGxCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;

  ShortCut := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt]);
end;

destructor TGxCodeFormatterExpert.Destroy;
begin
  FreeAndNil(FExpert);
  inherited;
end;

function TGxCodeFormatterExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Code &Formatter';
begin
  Result := SMenuCaption;
end;

class function TGxCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TGxCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

function TGxCodeFormatterExpert.HasMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxCodeFormatterExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(ConfigurationKey, Settings);
end;

procedure TGxCodeFormatterExpert.InternalSaveSettings(Settings:
  TGExpertsSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(ConfigurationKey, Settings);
end;

function FormatFile(_FileName: PChar): Boolean;
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TGxCodeFormatterExpert;
begin
  Result := false;
  InitSharedResources;
  try
    try
      FormatterStandAlone := TGxCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        Result := FormatterStandAlone.FExpert.FormatFile(_FileName);
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, e.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

{ This is a simple implementation. It originally TStrings.StrictDelimiter
  was used but that only exists in Delphi 2006 and up. }

procedure SplitStr(_InStr: string; _Delimiter: char; _sl: TStrings);
var
  Start: integer;
  i: Integer;
begin
  Assert(Assigned(_sl));

  _InStr := _InStr + ';';
  _sl.Clear;
  Start := 1;
  for i := 1 to Length(_InStr) do begin
    if _InStr[i] = _Delimiter then begin
      _sl.Add(Copy(_InStr, Start, i - Start));
      Start := i + 1;
    end;
  end;
end;

procedure FormatFiles(AFileNames: PChar);
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TGxCodeFormatterExpert;
  FileList: TStringList;
  i: Integer;
begin
  InitSharedResources;
  try
    try
      FormatterStandAlone := TGxCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        FileList := TStringList.Create;
        try
          SplitStr(AFileNames, ';', FileList);
          for i := 0 to Pred(FileList.Count) do
            FormatterStandAlone.FExpert.FormatFile(FileList[i]);
        finally
          FileList.Free;
        end;
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, e.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure ConfigureFormatter();
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TGxCodeFormatterExpert;
begin
  InitSharedResources;
  try
    try
      FormatterStandAlone := TGxCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        FormatterStandAlone.Configure;
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, e.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure AboutFormatter();
{$IFNDEF GX_BCB} export;
var
  frm: TfmAbout;
{$ENDIF GX_BCB}
begin
  InitSharedResources;
  try
    try
      frm := gblAboutFormClass.Create(nil);
      try
        frm.ShowModal;
      finally
        frm.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, e.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

exports
  FormatFile,
  FormatFiles,
  ConfigureFormatter,
  AboutFormatter;

end.

