unit GX_ProofreaderExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_ProofreaderData, GX_ConfigurationInfo, GX_Experts, Classes, Types;

type
  TCodeProofreaderExpert = class(TGX_Expert)
  private
    FProofreaderData: TProofreaderData;
    procedure CopyDefaultsFromResource(const _FileName: string);
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
  end;

var
  CodeProofreaderExpert: TCodeProofreaderExpert = nil;

implementation

uses
  SysUtils, Dialogs, Controls,
  GX_ProofreaderConfig, GX_MessageBox;

type
  TCreateDefaultTablesMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TCreateDefaultTablesMessage }

function TCreateDefaultTablesMessage.GetMessage: string;
resourcestring
  SDefaultTablesCreated = 'Could not find the Code Proofreader data file:'
  + sLineBreak + '%s' + sLineBreak +
  'Would you like to create the default replacement rules and dictionary entries?';
begin
  Result := Format(SDefaultTablesCreated, [FData]);
end;

{ TCodeProofreaderExpert }

procedure TCodeProofreaderExpert.Execute(Sender: TObject);
begin
  Configure;
end;

procedure TCodeProofreaderExpert.Configure;
var
  CreatedDataToConfigure: Boolean;
  Dlg: TfmProofreaderConfig;
  FileName: string;
begin
  CreatedDataToConfigure := False;
  try
    if FProofreaderData = nil then
    begin
      FProofreaderData := TProofreaderData.Create(ConfigurationKey);
      CreatedDataToConfigure := True;
    end;

    FileName := FProofreaderData.XmlFileName;

    if (not FileExists(FileName)) and
      (ShowGxMessageBox(TCreateDefaultTablesMessage, FileName) = mrYes) then
    begin
      CopyDefaultsFromResource(FileName);
      FProofreaderData.ReloadData;
    end;

    Dlg := TfmProofreaderConfig.Create(nil, Self, FProofreaderData);
    try
      SetFormIcon(Dlg);
      Dlg.ShowModal;
    finally
      FreeAndNil(Dlg);
    end;
    SaveSettings;

  finally
    if CreatedDataToConfigure then
      FreeAndNil(FProofreaderData);
  end;
end;

procedure TCodeProofreaderExpert.CopyDefaultsFromResource(const _FileName: string);
var
  ResStream: TResourceStream;
  FileStream: TFileStream;
begin
  FileStream := nil;
  ResStream := TResourceStream.Create(hInstance, 'CodeProofreaderDefault', RT_RCDATA);
  try
    FileStream := TFileStream.Create(_FileName, fmOpenReadWrite or fmCreate);
    FileStream.CopyFrom(ResStream, ResStream.Size);
  finally
    FreeAndNil(FileStream);
    FreeAndNil(ResStream);
  end;
end;

constructor TCodeProofreaderExpert.Create;
begin
  inherited;

  FreeAndNil(CodeProofreaderExpert);
  CodeProofreaderExpert := Self;
end;

destructor TCodeProofreaderExpert.Destroy;
begin
  FreeAndNil(FProofreaderData);
  CodeProofreaderExpert := nil;

  inherited Destroy;
end;

function TCodeProofreaderExpert.GetActionCaption: string;
resourcestring
  SProofMenuCaption = 'Code &Proofreader...';
begin
  Result := SProofMenuCaption;
end;

class function TCodeProofreaderExpert.GetName: string;
begin
  Result := 'CodeProofreader';
end;

procedure TCodeProofreaderExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);

  if Assigned(FProofreaderData) then
    FProofreaderData.SaveSettings(Settings);
end;

procedure TCodeProofreaderExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);

  if Active then
  begin
    if not Assigned(FProofreaderData) then
      FProofreaderData := TProofreaderData.Create(ConfigurationKey);

    FProofreaderData.ReloadData;
  end
  else
    FreeAndNil(FProofreaderData);
end;

initialization
  RegisterGX_Expert(TCodeProofreaderExpert);

end.

