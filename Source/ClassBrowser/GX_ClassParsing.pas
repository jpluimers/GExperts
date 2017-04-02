unit GX_ClassParsing;

interface

uses
  ComCtrls, Controls, Classes, StdCtrls, Forms, GX_BaseForm;

type
  TfmClassParsing = class(TfmBaseForm)
    aniFlashlight: TAnimate;
    lblParsing: TLabel;
    Progress: TProgressBar;
    procedure FormShow(Sender: TObject);
  public
    class function CreateAndShow(Owner: TComponent; FileCount: Integer): TfmClassParsing;
    procedure SetProgress(const Text: string; Position: Integer);
  end;

implementation

{$R *.dfm}

uses GX_GenericUtils;

class function TfmClassParsing.CreateAndShow(Owner: TComponent; FileCount: Integer): TfmClassParsing;
begin
  Result := TfmClassParsing.Create(Owner);
  Result.Progress.Position := 0;
  Result.Progress.Min := 0;
  Result.Progress.Max := FileCount;
  Result.Show;
end;

procedure TfmClassParsing.FormShow(Sender: TObject);
begin
  aniFlashlight.Active := True;
end;

procedure TfmClassParsing.SetProgress(const Text: string; Position: Integer);
begin
  lblParsing.Caption := Text;
  Progress.Position := Position;
end;

end.
