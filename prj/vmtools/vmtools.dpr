library vmtools;

{$I cond_define.inc}
{$E DLL}

{$IFDEF DELPHIX_BERLIN}
  {$LIBSUFFIX '_10.1'}
{$ENDIF}
{$IFDEF DELPHIXE}
  {$LIBSUFFIX '_xe'}
{$ENDIF}

uses
  SysUtils,
  Classes,
  vm.ide.actions.manager in '..\..\src\vmtools\common\vm.ide.actions.manager.pas',
  vm.ide.actions.options_frame in '..\..\src\vmtools\common\vm.ide.actions.options_frame.pas' {ActionManagerFrame: TFrame},
  vm.ide.actions.options_handler in '..\..\src\vmtools\common\vm.ide.actions.options_handler.pas',
  vm.ide.main.idereg in '..\..\src\vmtools\common\vm.ide.main.idereg.pas',
  vm.ide.main in '..\..\src\vmtools\common\vm.ide.main.pas',
  vm.ide.main.wizreg in '..\..\src\vmtools\common\vm.ide.main.wizreg.pas',
  vm.ide.options.handler in '..\..\src\vmtools\common\vm.ide.options.handler.pas',
  vm.ide.options.treehandler in '..\..\src\vmtools\common\vm.ide.options.treehandler.pas',
  vm_basewizard in '..\..\src\vmtools\common\vm_basewizard.pas',
  vm_custom_dock_frm in '..\..\src\vmtools\common\vm_custom_dock_frm.pas',
  vm_internal_int in '..\..\src\vmtools\common\vm_internal_int.pas',
  vm_options_dlg in '..\..\src\vmtools\common\vm_options_dlg.pas' {OptionsDlg},
  vmtools_cst in '..\..\src\vmtools\common\vmtools_cst.pas',
  vtree_mod in '..\..\src\vtree_mod\vtree_mod.pas',
  opt_editors in '..\..\src\options\opt_editors.pas',
  opt_frame in '..\..\src\options\opt_frame.pas' {OptionsFrame: TFrame},
  opt_impl in '..\..\src\options\opt_impl.pas',
  base_params in '..\..\src\common\base_params.pas',
  baseform in '..\..\src\common\baseform.pas',
  collections.array_utils in '..\..\src\common\collections.array_utils.pas',
  collections.common in '..\..\src\common\collections.common.pas',
  collections.deque in '..\..\src\common\collections.deque.pas',
  collections.lists in '..\..\src\common\collections.lists.pas',
  collections.maps in '..\..\src\common\collections.maps.pas',
  collections.rbtree in '..\..\src\common\collections.rbtree.pas',
  collections.tst in '..\..\src\common\collections.tst.pas',
  observer in '..\..\src\common\observer.pas',
  progress in '..\..\src\common\progress.pas',
  progress_ui in '..\..\src\common\progress_ui.pas' {ProgressDlg},
  str_utils in '..\..\src\common\str_utils.pas',
  validators in '..\..\src\common\validators.pas',
  vm.common.updatestack in '..\..\src\common\vm.common.updatestack.pas',
  vm.debug in '..\..\src\common\vm.debug.pas',
  vmsys in '..\..\src\common\vmsys.pas',
  weak_ref in '..\..\src\common\weak_ref.pas',
  openfile_frame in '..\..\src\openfile\openfile_frame.pas' {OpenFileFrame: TFrame},
  openfile_handler in '..\..\src\openfile\openfile_handler.pas',
  search_cst in '..\..\src\search\search_cst.pas',
  search_frame in '..\..\src\search\search_frame.pas' {SearchResultFrame: TFrame},
  search_handler in '..\..\src\search\search_handler.pas',
  search_query_dlg in '..\..\src\search\search_query_dlg.pas' {SearchEngineQueryDlg},
  search_types in '..\..\src\search\search_types.pas',
  vm_wiz_activate_editor in '..\..\src\vmtools\activate_editor\vm_wiz_activate_editor.pas',
  hist_wiz in '..\..\src\vmtools\history\hist_wiz.pas',
  vm_ide_utils in '..\..\src\vmtools\ide_common\vm_ide_utils.pas',
  wiz_openfile in '..\..\src\vmtools\openfile\wiz_openfile.pas',
  wiz_openfile_dockform in '..\..\src\vmtools\openfile\wiz_openfile_dockform.pas',
  wiz_openfile_handler in '..\..\src\vmtools\openfile\wiz_openfile_handler.pas',
  vm_wiz_search in '..\..\src\vmtools\search\vm_wiz_search.pas',
  vm_wiz_search_cst in '..\..\src\vmtools\search\vm_wiz_search_cst.pas',
  vm_wiz_search_dockform in '..\..\src\vmtools\search\vm_wiz_search_dockform.pas',
  csplg_params in '..\..\src\search\engine_codesearch\csplg_params.pas',
  csplg_parser in '..\..\src\search\engine_codesearch\csplg_parser.pas',
  csplg_query_params in '..\..\src\search\engine_codesearch\csplg_query_params.pas',
  csplg_search in '..\..\src\search\engine_codesearch\csplg_search.pas',
  csplg_types in '..\..\src\search\engine_codesearch\csplg_types.pas',
  vm.memodlg in '..\..\src\common\vm.memodlg.pas' {MemoDialog};

{$R *.res}

begin
end.
