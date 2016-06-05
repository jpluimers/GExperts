unit GX_LibrarySource;

{$I GX_CondDefine.inc}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF BCB}

interface

uses
  ToolsAPI; // Errors here indicate that you didn't link to the DesignIde package

// This function needs to be interface-visible, otherwise
// C++Builder 5 complains about a missing EXTDEF symbol
// for this export.  Do not call this function yourself.
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  Windows, GX_GxUtils, GX_OtaUtils, GX_VerDepConst,
  GX_GExperts, GX_MessageBox, GX_CodeLib, GX_GrepExpert, GX_ExpertManager,
  GX_PeInformation, GX_GenericUtils;

const
  InvalidIndex = -1;

var
  FExpertIndex: Integer = InvalidIndex;

{ Remove the wizard from the IDE. }
procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if FExpertIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    WizardServices.RemoveWizard(FExpertIndex);

    FExpertIndex := InvalidIndex;
  end;
end;

{ Register the wizard. }
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Result := (BorlandIDEServices <> nil);

  if Result and (not BuiltWithPackages) then
  begin
    Result := False;
    ShowNoPackagesError;
  end;

  if Result then
  begin
    Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);

    Terminate := FinalizeWizard;

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));

    FExpertIndex := WizardServices.AddWizard(TGExperts.Create as IOTAWizard);

    Result := (FExpertIndex >= 0);
  end;
end;

exports
{$IFDEF GX_BCB}
  InitWizard name WizardEntryPoint;
{$ELSE}
  InitWizard name WizardEntryPoint,
  ShowExpertManager,
  ShowPeInfo,
  InstallGExperts,
  ShowCodeLib,
  ShowGrep;
{$ENDIF GX_BCB}

// ---------------------------------------------

type
  TGxMultipleInstancesMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TGxMultipleInstancesMessage }

function TGxMultipleInstancesMessage.GetMessage: string;
resourcestring
  SMultipleGExpertsInstances =
    'Multiple instances of this version of GExperts are currently running.  ' +
    'This is usually caused by multiple instances of the same IDE running at once (check in the Task Manager if you don''t see them all).  ' +
    'Please be aware that the last IDE instance to save any settings ' +
    'will overwrite any previously saved settings from other instances.';
begin
  Result := SMultipleGExpertsInstances;
end;

var
  GXGeneralMutex: THandle;
  GXVersionMutex: THandle;

procedure CreateInstanceMutexes;
begin
  // This mutex signals that at least one copy of GExperts is running
  // The installer uses this to determine if it should allow installation
  GXGeneralMutex := CreateMutex(nil, False, 'GExperts.Addin.For.Borland.IDEs');

  GXVersionMutex := CreateMutex(nil, False, PChar('GExperts.Addin.For.Borland.IDEs.Under.' +
    GxOtaGetIDEProductIdentifier + MajorVersionNumberChar));
  if (GXVersionMutex <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    // The mutex already exists, so there is a good chance that a copy
    // of this version of GExperts is already loaded into memory.
    ShowGxMessageBox(TGxMultipleInstancesMessage);
  end;
end;

procedure DestroyInstanceMutexes;
begin
  if GXGeneralMutex <> 0 then
    CloseHandle(GXGeneralMutex);
  if GXVersionMutex <> 0 then
    CloseHandle(GXVersionMutex);
end;

initialization
  CreateInstanceMutexes;

finalization
  DestroyInstanceMutexes;

end.
