unit vmtools_cst;

{$I cond_define.inc}

interface

const
{ shortcut_manager }
  cstVMKeyBindingName       = 'VMTools_ShortCuts_Name';
  cstVMKeyBindingDispName   = 'VMTools ShortCuts';

{ vm_action_broker }
const
  cstVMActionQualifier      = 'VMToolsAct_';
  cstVMMenuQualifier        = 'VMToolsMenu_';
  cstVMActionCategory       = 'VMTools';
  cstVMBitmapSuffix         = 'VMImage'; // Do not localize.

  cstTransparencyColor      = $00FF00FF; // Defines the background of some bitmaps

  cstActCaption_ShowBrowsingHistory = 'Show browsing history';

resourcestring
  rs_NoButtonCategory       = '(None)';
  rs_AllButtonsCategory     = '(All)';
  rs_DuplicateShortCut      = 'The shortcut "%s" has already been assigned.';

  rs_Msg_RegisterWizard = 'Registration %s.';
  rs_Msg_UnRegisterWizard = 'Unregistration %s.';

const
  cstSearch_GUID = '{9DAEE8D5-C7B6-49D8-BDCE-B0C61C8834EC}';
  cstSearch_Caption = 'Search wizard';
  cstSearch_ParamsAlias = 'Search wizard params';
  cstSearch_ParamsAlias_ExecuteSearch = 'Execute search';
  cstSearch_ParamsAlias_ShowResults = 'Show search results';
  cstSearch_ParamsAlias_GotoNextResult = 'Goto next search result';
  cstSearch_ParamsAlias_GotoPrevResult = 'Goto prev search result';
  cstSearch_Msg_RefreshShortcuts = 'Refreshing search wizard shortcuts.';

  cstOpenFile_GUID = '{0A28079D-78CA-4D78-83C9-CDD563D24B59}';
  cstOpenFile_Caption = 'Open file wizard';
  cstOpenFile_ParamsAlias = 'Open file wizard params';
  cstOpenFile_ParamsAlias_ShowDialog = 'Show open file dialog';
  cstOpenFile_Msg_RefreshShortcuts = 'Refreshing open file wizard shortcuts.';

  cstActivateEditor_GUID = '{C27160A2-12BD-4AA7-A297-FEF82C9BE514}';
  cstActivateEditor_Caption = 'Activate editor wizard';
  cstActivateEditor_ParamsAlias = 'Activate editor wizard params';
  cstActivateEditor_ParamsAlias_ActivateEditor = 'Activate editor';
  cstActivateEditor_Msg_RefreshShortcuts = 'Refreshing activate editor wizard shortcuts.';

  cstHistoryWiz_GUID = '{7FE2043C-69BB-4768-B21A-DC8A58CBAC39}';
  cstHistoryWiz_Caption = 'Browsing history wizard';

implementation


end.
