{Search history author: (ERT) Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>}

unit GX_GrepProgress;

interface

uses
  SysUtils, Classes, Controls, ComCtrls, StdCtrls, Forms, GX_BaseForm;

type
  TfmGrepProgress = class(TfmBaseForm)
    Progress: TProgressBar;
    lblProgressDetailText: TLabel;
    ProgressBarDetail: TProgressBar;
    lblProgressText: TLabel;
  private
  public
    procedure Init(const ACaption: String; AMax, ADetailMax: Integer);
    class procedure Start;
    class procedure Progressing(const AStepText: String);
    class procedure Progressed;
    class procedure ProgressingDetail(const AStepDetailText: String);
    class procedure ProgressedDetail;
  end;

var
  fmGrepProgress: TfmGrepProgress;

implementation

{$R *.dfm}

{ TfmGrepProgress }

procedure TfmGrepProgress.Init(const ACaption: String; AMax, ADetailMax: Integer);
begin
  Caption := ACaption;
  Progress.Min := 0;
  Progress.Max := AMax * (ADetailMax + 1);
  Progress.Position := 0;
  lblProgressText.Caption := '';

  ProgressBarDetail.Visible := ADetailMax > 0;
  lblProgressDetailText.Visible := ProgressBarDetail.Visible;
  if ProgressBarDetail.Visible then
  begin
    ProgressBarDetail.Min := 0;
    ProgressBarDetail.Max := ADetailMax;
    ProgressBarDetail.Position := 0;
    lblProgressDetailText.Caption := '';
  end;
end;

class procedure TfmGrepProgress.Start;
begin
  if Assigned(fmGrepProgress) then
    fmGrepProgress.Show;
end;

class procedure TfmGrepProgress.Progressing(const AStepText: String);
begin
  if not Assigned(fmGrepProgress) then
    Exit;

  fmGrepProgress.lblProgressText.Caption := AStepText;
  Application.ProcessMessages;
end;

class procedure TfmGrepProgress.Progressed;
begin
  if Assigned(fmGrepProgress) then
    fmGrepProgress.Progress.StepBy(1);
//  Sleep(400);
end;

class procedure TfmGrepProgress.ProgressingDetail(const AStepDetailText: String);
begin
  if not Assigned(fmGrepProgress) then
    Exit;

  fmGrepProgress.lblProgressDetailText.Caption := AStepDetailText;
  Application.ProcessMessages;
end;

class procedure TfmGrepProgress.ProgressedDetail;
begin
  if not Assigned(fmGrepProgress) then
    Exit;

  fmGrepProgress.ProgressBarDetail.StepBy(1);
  if fmGrepProgress.ProgressBarDetail.Position >= fmGrepProgress.ProgressBarDetail.Max then
    fmGrepProgress.ProgressBarDetail.Position := 0;
  Application.ProcessMessages;
//  Sleep(400);
  Progressed;
end;

initialization
  fmGrepProgress := nil;

end.
