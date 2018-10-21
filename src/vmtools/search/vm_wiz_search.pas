unit vm_wiz_search;

{$I cond_define.inc}

interface

uses
  vmsys, vm_basewizard, menus,
  toolsapi, forms, inifiles,
  search_types,
  vm_internal_int, generics.collections,
  search_handler, vm_wiz_search_dockform,
  base_params, classes, opt_impl;

type
  TVMSearchWizardParams = class(TBaseParams)
  strict private const
    cstRegSection = 'SearchWizard';
  protected
    function CreateTree: TParamsTree; override;
    procedure RegisterParams; override;
  end;

  TVMSearchWizard = class(TVMBaseWizard)
  private
    FParams: TVMSearchWizardParams;
    FParamsObserver: IParamsChangedObserver;
    FDockForm: IVMSearchResultDockForm;
    FResultsDlg: IObjectHolder<TCustomForm>;
    FHandler: TSearchHandler;

    procedure OnExecuteSearch(aSender: TObject);
    procedure OnShowSearchReultsDlg(aSender: TObject);
    procedure OnShowNextResult(aSender: TObject);
    procedure OnShowPrevResult(aSender: TObject);

    function GetDlg: IObjectHolder<TCustomForm>;
    function GetDockForm: IVMSearchResultDockForm;
    function GetSearchHandler: TSearchHandler;
    function GetParams: TVMSearchWizardParams;

    procedure ParamsChanged;
    procedure RefreshShortCuts;
    procedure RegisterShortCuts;
    procedure UnregisterShortCuts;

    property Dlg: IObjectHolder<TCustomForm> read GetDlg;
    property Handler: TSearchHandler read GetSearchHandler;
    property Params: TVMSearchWizardParams read GetParams;
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    destructor Destroy; override;

    class function GUID: string; override;
    class function Caption: string; override;
  end;

implementation

uses
  dialogs, vm_wiz_search_cst, vm_ide_utils, SysUtils, Windows,
  deskutil, vmtools_cst,
  controls, str_utils;

type
  TVMSearchHandler = class(TSearchHandler)
  private
    FWizzard: TVMSearchWizard;
  protected
    function AppendSearchResult(aSearchInfo: TSearchInfo): Boolean; override;
    function GetIndexSearchPaths: TArray<string>; override;
    function GetProjectPaths: IEnumerable<string>; override;
    function GetQueryText: string; override;
  public
    constructor Create(aWizard: TVMSearchWizard);
    procedure FocusEditor; override;
    procedure OpenFileInEditor(const aFileName: string; aLine: Integer); override;
  end;

destructor TVMSearchWizard.Destroy;
begin
  if FParams <> nil then
    FParams.WriteParams;
  FreeAndNil(FParams);
  FreeAndNil(FHandler);
  inherited;
end;

function TVMSearchWizard.GetDlg: IObjectHolder<TCustomForm>;
begin
  if (FResultsDlg = nil) and (FDockForm <> nil) then
    FResultsDlg := TObjectHolder<TCustomForm>.Create(
      (BorlandIDEServices as INTAServices).CreateDockableForm(FDockForm), False);
  Result := FResultsDlg;
end;

function TVMSearchWizard.GetDockForm: IVMSearchResultDockForm;
begin
  if FDockForm = nil then
    FDockForm := TVMSearchResultDockForm.Create(Handler);
  Result := FDockForm;
end;

function TVMSearchWizard.GetParams: TVMSearchWizardParams;
begin
  if FParams = nil then
  begin
    FParams := TVMSearchWizardParams.Create;
    FParams.ReadParams;
    FParamsObserver := TParamsObserver.Create(
      procedure
      begin
        ParamsChanged;
      end);

    FParams.RegisterObserver(IParamsChangedObserver, FParamsObserver);
  end;
  Result := FParams;
end;

function TVMSearchWizard.GetSearchHandler: TSearchHandler;
begin
  if FHandler = nil then
    FHandler := TVMSearchHandler.Create(Self);
  Result := FHandler;
end;

class function TVMSearchWizard.GUID: string;
begin
  Result := cstSearch_GUID;
end;

class function TVMSearchWizard.Caption: string;
begin
  Result := cstSearch_Caption;
end;

procedure TVMSearchWizard.OnExecuteSearch(aSender: TObject);
begin
  InfoMsg(rs_inf_ExecuteSearch);
  Handler.ExecuteSearch;
end;

procedure TVMSearchWizard.OnShowNextResult(aSender: TObject);
begin
  GetDockForm.ShowNext;
end;

procedure TVMSearchWizard.OnShowPrevResult(aSender: TObject);
begin
  GetDockForm.ShowPrev;
end;

procedure TVMSearchWizard.OnShowSearchReultsDlg(aSender: TObject);
begin
  if (Dlg = nil) or not Dlg.IsAlive then
    Exit;

  if Dlg.Obj.Floating then
  begin
    Dlg.Obj.Show;
    Dlg.Obj.BringToFront
  end
  else
    FocusWindow(Dlg.Obj);
  if Dlg.Obj.CanFocus then
    Dlg.Obj.SetFocus;
end;

procedure TVMSearchWizard.ParamsChanged;
begin
  if IsActive then
    RefreshShortCuts;
  Params.WriteParams;
end;

procedure TVMSearchWizard.RefreshShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    InfoMsg(cstSearch_Msg_RefreshShortcuts);
    UnregisterShortCuts;
    RegisterShortCuts;
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMSearchWizard.RegisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.RegisterInternalMenuAction(act_capt_Search, OnExecuteSearch,
        ShortCut(TextToShortCut('S'), [ssShift, ssAlt]));
    ActionManager.RegisterInternalMenuAction(act_capt_ShowSearchDlg, OnShowSearchReultsDlg,
        ShortCut(TextToShortCut('D'), [ssShift, ssAlt]));
    ActionManager.RegisterInternalAction(act_name_ShowSearchPrev, OnShowPrevResult,
        ShortCut(VK_UP, [ssCtrl, ssAlt, ssShift]));
    ActionManager.RegisterInternalAction(act_name_ShowSearchNext, OnShowNextResult,
        ShortCut(VK_DOWN, [ssCtrl, ssAlt, ssShift]));
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMSearchWizard.UnregisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.UnRegisterInternalMenuAction(act_capt_Search);
    ActionManager.UnRegisterInternalMenuAction(act_capt_ShowSearchDlg);
    ActionManager.UnRegisterInternalAction(act_name_ShowSearchPrev);
    ActionManager.UnRegisterInternalAction(act_name_ShowSearchNext);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMSearchWizard.RegisterWizard;
begin
  inherited;
  RefreshShortCuts;
  (BorlandIDEServices as INTAServices).RegisterDockableForm(GetDockForm);
end;

procedure TVMSearchWizard.UnregisterWizard;
begin
  UnregisterShortCuts;
  if FDockForm <> nil then
    (BorlandIDEServices as INTAServices).UnregisterDockableForm(FDockForm);
  FDockForm := nil;
  FResultsDlg := nil;
  FreeAndNil(FHandler);
  inherited;
end;

{ TVMSearchHandler }

function TVMSearchHandler.AppendSearchResult(aSearchInfo: TSearchInfo): Boolean;
begin
  if (FWizzard <> nil) then
    FWizzard.OnShowSearchReultsDlg(nil);
  Result := inherited;
end;

constructor TVMSearchHandler.Create(aWizard: TVMSearchWizard);
begin
  inherited Create;
  FWizzard := aWizard;
end;

{ TVMSearchHandler }

procedure TVMSearchHandler.FocusEditor;
begin
  TGXOtaUtils.GxOtaFocusCurrentIDEEditControl;
end;

function TVMSearchHandler.GetIndexSearchPaths: TArray<string>;
begin
  SetLength(Result, 2);
  Result[1] := ExtractFilePath(TVMOtaUtils.GetCurrentOpenFileName);
  Result[0] := ExtractFilePath(TVMOtaUtils.GetCurrentOpenProjectFileName);
end;

function TVMSearchHandler.GetProjectPaths: IEnumerable<string>;
begin
  Result := TStrUtils.Words(TVMOtaUtils.GetProjectPaths, [';']);
end;

function TVMSearchHandler.GetQueryText: string;
begin
  Result := TVMOtaUtils.GetWordUnderCursorInCurView2;
end;

procedure TVMSearchHandler.OpenFileInEditor(const aFileName: string; aLine:
    Integer);
begin
  TGXOtaUtils.GxOtaGoToFileLineColumn(aFileName, aLine);
end;

procedure TVMSearchWizardParams.RegisterParams;
begin
  inherited;
end;

function TVMSearchWizardParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(cstRegSection, cstSearch_ParamsAlias);
end;

end.
