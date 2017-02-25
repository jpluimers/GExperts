unit w_GExpertsFormatterAbout;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TAboutFormatterProc = procedure;

type
  Tf_GExpertsFormatterAbout = class(TForm)
    l_StandAloneAbout: TLabel;
    l_ExperimentalAbout: TLabel;
    b_AboutGExperts: TButton;
    b_Close: TButton;
    l_DelForExp: TLabel;
    procedure b_CloseClick(Sender: TObject);
    procedure b_AboutGExpertsClick(Sender: TObject);
  private
    FAboutFormatterProc: TAboutFormatterProc;
    procedure SetAboutGExperts(_AboutFormatterProc: TAboutFormatterProc);
  public
    class procedure Execute(_Owner: TComponent; _AboutFormatterProc: TAboutFormatterProc);
  end;

implementation

{$R *.dfm}

class procedure Tf_GExpertsFormatterAbout.Execute(_Owner: TComponent;
  _AboutFormatterProc: TAboutFormatterProc);
var
  frm: Tf_GExpertsFormatterAbout;
begin
  frm := Tf_GExpertsFormatterAbout.Create(_Owner);
  try
    frm.SetAboutGExperts(_AboutFormatterProc);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure Tf_GExpertsFormatterAbout.b_AboutGExpertsClick(Sender: TObject);
begin
  if Assigned(FAboutFormatterProc) then
    FAboutFormatterProc;
end;

procedure Tf_GExpertsFormatterAbout.b_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_GExpertsFormatterAbout.SetAboutGExperts(
  _AboutFormatterProc: TAboutFormatterProc);
begin
  FAboutFormatterProc := _AboutFormatterProc;
  b_AboutGExperts.Visible := Assigned(@FAboutFormatterProc);
end;

end.

