program GExpertsDebugWindow;

uses
  Windows,
  Forms,
  DebugOptions in 'DebugOptions.pas' {fmDebugOptions},
  DebugWindow in 'DebugWindow.pas' {fmDebug};

{$R *_icon.res}

var
  PrvHWND: HWND;

begin
  // Project Relative Search Path:  ..\..\Utils\;..\..\Framework\;..\..\..\ExternalSource\UniSynEdit
  Application.Initialize;
  Application.Title := '';
  PrvHWND := FindWindow('TApplication', 'GExperts Debug Window');
  if PrvHWND <> 0 then
  begin
    if IsIconic(PrvHWND) then
      ShowWindow(PrvHWND, SW_RESTORE);
    SetForegroundWindow(PrvHWND);
    Halt;
  end;
  Application.ShowMainForm := ConfigInfo.Start;
  Application.Title := 'GExperts Debug Window'; // Do not localize.
  Application.CreateForm(TfmDebug, fmDebug);
  Application.Run;
end.
