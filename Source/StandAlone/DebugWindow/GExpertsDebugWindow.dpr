program GExpertsDebugWindow;

uses
  Forms,
  DebugOptions in 'DebugOptions.pas' {fmDebugOptions},
  DebugWindow in 'DebugWindow.pas' {fmDebug};

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm := ConfigInfo.Start;
  Application.Title := 'GExperts Debug Window'; // Do not localize.
  Application.CreateForm(TfmDebug, fmDebug);
  Application.Run;
end.
