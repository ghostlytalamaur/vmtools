unit vm.ide.main.idereg;

{$I cond_define.inc}

interface

uses
  toolsapi;

function InitWizard(const BIDES: IBorlandIDEServices; RegisterProc: TWizardRegisterProc;
    var Terminate: TWizardTerminateProc): Boolean; stdcall;

implementation

uses
  vm.ide.main, forms, vm.ide.main.wizreg, vm.debug, sysutils;

const
  InvalidIndex = -1;
var
  iWizard: Integer = InvalidIndex;
  Wizard: IOTAWizard;

{ Remove the wizard from the IDE. }
procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  LogEnterLeave('VMTools: FinalizeWizard');
  Logger.d('Has wizard: %s', [BoolToStr(iWizard <> InvalidIndex, True)]);
  if iWizard <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));

    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    Wizard := nil;
    WizardServices.RemoveWizard(iWizard);

    iWizard := InvalidIndex;
  end;
end;

function InitWizard(const BIDES: IBorlandIDEServices; RegisterProc: TWizardRegisterProc;
    var Terminate: TWizardTerminateProc): Boolean;
begin
  LogEnterLeave('VMTools: InitWizard');
  Result := False;
  if (BIDES = nil) or (toolsapi.BorlandIDEServices <> BIDES) then
    Exit;

  Terminate := FinalizeWizard;
  iWizard := (BIDES as IOTAWizardServices).AddWizard(TVMMainWizard.Create(RegisterWizards));
  Result := iWizard >= 0;
end;

procedure Register;
begin
  iWizard := (BorlandIDEServices as IOTAWizardServices).AddWizard(TVMMainWizard.Create(RegisterWizards));
end;


exports
  InitWizard Name WizardEntryPoint;

end.
