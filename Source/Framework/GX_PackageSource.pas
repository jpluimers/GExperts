unit GX_PackageSource;

interface

{$I GX_CondDefine.inc}

procedure Register;
procedure IDERegister;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  ToolsAPI,
  GX_GExperts;

// Under Delphi 8+ we need to delay register with the IDE so the OTA is ready
procedure IDERegister;
begin
  {$IFOPT D+} SendDebug('GX_PackageSource.IDERegister'); {$ENDIF}
  TGExperts.DelayedRegister;
end;

procedure Register;
begin
  {$IFOPT D+} SendDebug('GX_PackageSource.Register'); {$ENDIF}
  RegisterPackageWizard(TGExperts.Create as IOTAWizard);
end;

end.
