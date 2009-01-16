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
  end;

implementation

{$R *.dfm}

uses GX_GenericUtils;

procedure TfmClassParsing.FormShow(Sender: TObject);
begin
  if not RunningLinux then
    aniFlashlight.Active := True;
end;

end.
