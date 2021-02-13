unit vm.ide.main.wizreg;

{$I cond_define.inc}

interface

uses
  vm.ide.main;

procedure RegisterWizards(aRegisterMethod: TWizzardRegisterMethod);

implementation

uses
  vm_wiz_search, vm_wiz_activate_editor, hist_wiz, wiz_openfile, wiz.tabs;

procedure RegisterWizards(aRegisterMethod: TWizzardRegisterMethod);
begin
  if not Assigned(aRegisterMethod) then
    Exit;

  aRegisterMethod(TVMSearchWizard);
  aRegisterMethod(TVMActivateEditorWizard);
  aRegisterMethod(TVMHistoryWizard);
  aRegisterMethod(TVMOpenFileWizard);
  aRegisterMethod(TVMTabsWizard);
end;

end.

