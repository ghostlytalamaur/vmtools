program gui_test;

uses
  FastMM4,
  Forms,
  options_dlg in '..\..\src\options\dlg\options_dlg.pas' {OptForm},
  opt_impl in '..\..\src\options\opt_impl.pas',
  validators in '..\..\src\common\validators.pas',
  opt_editors in '..\..\src\options\opt_editors.pas',
  base_params in '..\..\src\common\base_params.pas',
  baseform in '..\..\src\common\baseform.pas',
  observer in '..\..\src\common\observer.pas',
  vtree_mod in '..\..\src\vtree_mod\vtree_mod.pas',
  weak_ref in '..\..\src\common\weak_ref.pas',
  opt_frame in '..\..\src\options\opt_frame.pas' {OptionsFrame: TFrame},
  openfile_frame in '..\..\src\openfile\openfile_frame.pas' {OpenFileFrame: TFrame},
  search_frame in '..\..\src\search\search_frame.pas' {SearchResultFrame: TFrame},
  vm.debug in '..\..\src\common\vm.debug.pas',
  collections.sets in '..\..\src\common\collections.sets.pas',
  vm.memodlg in '..\..\src\common\vm.memodlg.pas' {MemoDialog},
  search_table in '..\..\src\search\search_table.pas',
  vm.gui.control_updater in '..\..\src\common\vm.gui.control_updater.pas',
  new_file_dlg in '..\..\src\vmtools\new_file\new_file_dlg.pas' {CreateFileDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOptForm, OptForm);
  Application.Run;
end.
