unit vm_wiz_search_cst;

{$I cond_define.inc}

interface

const
  { TAction.Name }
  act_name_Search = 'vm_act_Search';
  act_capt_Search = 'Search...';
  act_name_ShowSearchDlg = 'vm_act_ShowSearchResulsDlg';
  act_capt_ShowSearchDlg = 'Show Search Resuls dialog';
  act_name_ShowSearchNext = 'ShowNextSearchResult';
  act_name_ShowSearchPrev = 'ShowPrevSearchResult';

resourcestring
  rs_err_NoSearchEngine = 'No any registered search engine.';
  rs_err_SearchResIsNil = 'Search engine return empty results.';
  rs_inf_ExecuteSearch  = 'Execute search.';



implementation

end.
