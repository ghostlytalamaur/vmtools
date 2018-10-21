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
  templates.dlg in '..\..\src\templates\templates.dlg.pas' {TemplatesParamsDlg},
  vm.debug in '..\..\src\common\vm.debug.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TOptForm, OptForm);
  Application.Run;
end.
