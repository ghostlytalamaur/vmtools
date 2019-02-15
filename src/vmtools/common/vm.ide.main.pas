unit vm.ide.main;

{$I cond_define.inc}

interface

uses
  toolsapi, vm_basewizard, generics.collections,
  vm_internal_int, classes, extctrls, base_params, inifiles, opt_impl;

type
  TWizzardRegisterMethod = procedure (aWizard: TVMBaseWizardClass) of object;
  TWizzardsRegisterProc = procedure (aRegisterMethod: TWizzardRegisterMethod);

  TVMMainWizardParams = class(TBaseParams)
  private const
    cstRegSection = 'TVMMainWizardParams';
    cstRegCount   = 'Count';
    cstRegWizard  = 'WizardGUID';
  private
    FDisabledWizzards: TList<string>;
  protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetDefault; override;

    property DisabledWizzards: TList<string> read FDisabledWizzards;
  end;

  TVMMainWizard = class(TNotifierObject, IOTAWizard)
  private
    FParams: TVMMainWizardParams;
    FWizardParamsTree: TParamsTree;
    FWizardParamsTreeObserver: IParamsTreeObserver;
    FWizardsClass: TList<TVMBaseWizardClass>;
    FWizards: TList<TVMBaseWizard>;
    FAddInOptionsList: TList<INTAAddInOptions>;

    FServices: IWizzardsServices;

    procedure BuildWizardParamsTree;

    procedure RegisterAddInOptions;
    procedure UnRegisterAddInOptions;
  public
    constructor Create(aRegisterProc: TWizzardsRegisterProc);
    destructor Destroy; override;

    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    procedure Init;
    procedure RegisterWizard(aWizard: TVMBaseWizardClass);

    { IParamsTreeObserver }
    procedure DataChanged(aTree: TParamsTree);

    property Params: TVMMainWizardParams read FParams;
  end;



implementation

uses
  vmsys, sysutils,
  vm_options_dlg, windows, vm.ide.actions.manager, forms, opt_frame,
  vm.ide.actions.options_frame, vm.ide.options.handler, vm.ide.actions.options_handler,
  vm.ide.options.treehandler, vm.debug, CnDebug;


type
  TCnDebugLogger = class(TAbstractLogger)
  protected
    procedure Log(aType: TAbstractLogger.TLogType; const aMsg: string); override;
  end;

  TInitHelper = class(TObject)
  private
    FInitTimer: TTimer;
    FCallback: TNotifyEvent;
    FInitCount: Integer;
    procedure OnInitTimer(Sender: TObject);
  public
    constructor Create(CallBack: TNotifyEvent);
  end;

  TWizardServices = class(TInterfacedObject, IWizzardsServices)
  private
    FActManager: TVMActionManager;

    FStartingUp: Boolean;
    FInitHelper: TInitHelper;

    procedure DoAfterIDELoading(aSender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    { IWizzardsServices }
    function GetActionManager: TVMActionManager;
    function StartingUp: Boolean;
  end;

  TWizardParam = class(TBooleanParam)
  private
    FWizard: IObjectHolder<TVMBaseWizard>;
  protected
    procedure CopyFrom(aFrom: TBaseParam); override;
  public
    constructor Create(aWizard: TVMBaseWizard);
  end;

{ TWizardServices }

constructor TWizardServices.Create;
begin
  inherited Create;
  FActManager := TVMActionManager.Create;

  FInitHelper := TInitHelper.Create(DoAfterIDELoading);
  FStartingUp := True;
  FActManager.BeginUpdate; // EndUpdate call in DoAfterIDELoading();
end;

destructor TWizardServices.Destroy;
begin
  FreeAndNil(FInitHelper);
  FreeAndNil(FActManager);

  inherited;
end;

procedure TWizardServices.DoAfterIDELoading(aSender: TObject);
begin
  FStartingUp := False;
  FActManager.EndUpdate;
end;

function TWizardServices.GetActionManager: TVMActionManager;
begin
  Result := FActManager;
end;

function TWizardServices.StartingUp: Boolean;
begin
  Result := FStartingUp;
end;

{ TVMMainWizard }

constructor TVMMainWizard.Create(aRegisterProc: TWizzardsRegisterProc);
begin
  LogEnterLeave('TVMMainWizard.Create');
  inherited Create;
  FParams := TVMMainWizardParams.Create;
  FParams.ReadParams;
  FWizardsClass := TList<TVMBaseWizardClass>.Create;
  FWizards := TObjectList<TVMBaseWizard >.Create;
  FAddInOptionsList := TList<INTAAddInOptions>.Create;

  FServices := TWizardServices.Create;
  if Assigned(aRegisterProc) then
    aRegisterProc(RegisterWizard);
  Init;
  BuildWizardParamsTree;
  RegisterAddInOptions;
end;

destructor TVMMainWizard.Destroy;
var
  Wiz: TVMBaseWizard;
begin
  LogEnterLeave('TVMMainWizard.Destroy');
  UnRegisterAddInOptions;
  FreeAndNil(FAddInOptionsList);
  FreeAndNil(FWizardParamsTree);
  FServices.ActionManager.BeginUpdate;
  try
    for Wiz in FWizards do
      Wiz.IsActive := False;
    FreeAndNil(FWizards);
    FreeAndNil(FWizardsClass);
  finally
    FServices.ActionManager.EndUpdate;
  end;

  FServices := nil;

  FParams.WriteParams;
  FreeAndNil(FParams);
  inherited;
end;

procedure TVMMainWizard.RegisterAddInOptions;
var
  Srv: INTAEnvironmentOptionsServices;
  AddIn: INTAAddInOptions;
  Wiz: TVMBaseWizard;
begin
  if not Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, Srv) then
    Exit;

  FAddInOptionsList.Add(TVMOptionsTreeHandler.Create('Wizards', FWizardParamsTree));
  FAddInOptionsList.Add(TVMActionManagerOptionsHandler.Create(FServices.ActionManager));
  for Wiz in FWizards do
  begin
    AddIn := Wiz.CreateOptionsHandler;
    if AddIn <> nil then
      FAddInOptionsList.Add(AddIn);
  end;
  for AddIn in FAddInOptionsList do
    Srv.RegisterAddInOptions(AddIn);
end;

procedure TVMMainWizard.UnRegisterAddInOptions;
var
  Srv: INTAEnvironmentOptionsServices;
  AddIn: INTAAddInOptions;
begin
  if not Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, Srv) then
    Exit;

  for AddIn in FAddInOptionsList do
    Srv.UnregisterAddInOptions(AddIn);
end;

procedure TVMMainWizard.Execute;
begin

end;

function TVMMainWizard.GetIDString: string;
begin
  Result := 'VMDelphiTools';
end;

function TVMMainWizard.GetName: string;
begin
  Result := 'VM Delphi Tools';
end;

function TVMMainWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TVMMainWizard.Init;
var
  wizClass: TVMBaseWizardClass;
  Wiz: TVMBaseWizard;
begin
  FServices.ActionManager.BeginUpdate;
  try
    for WizClass in FWizardsClass do
      FWizards.Add(wizClass.Create(FServices));
    for Wiz in FWizards do
      Wiz.IsActive := not FParams.DisabledWizzards.Contains(Wiz.GUID);
  finally
    FServices.ActionManager.EndUpdate;
  end;
end;

procedure TVMMainWizard.BuildWizardParamsTree;
var
  I: Integer;
begin
  FWizardParamsTree := TParamsTree.Create('VMTools', 'VMTools parameters');
  if FWizards.Count <= 0 then
    Exit;

  for I := 0 to FWizards.Count - 1 do
    FWizardParamsTree.RegisterParam(TWizardParam.Create(FWizards[I]));
  FWizardParamsTreeObserver := TParamsTreeObserver.Create(DataChanged);
  FWizardParamsTree.RegisterListener(FWizardParamsTreeObserver);
end;

procedure TVMMainWizard.DataChanged(aTree: TParamsTree);
var
  I: Integer;
  P: TWizardParam;
begin
  if aTree <> FWizardParamsTree then
    Exit;

  for I := 0 to FWizardParamsTree.ParamsCount - 1 do
  begin
    if not (FWizardParamsTree.ByIndex[I] is TWizardParam) then
      Continue;

    P := TWizardParam(FWizardParamsTree.ByIndex[I]);
    if not P.FWizard.IsAlive then
      Continue;

    if P.Value then
    begin
      if FParams.DisabledWizzards.Contains(P.FWizard.Obj.GUID) then
        FParams.DisabledWizzards.Remove(P.FWizard.Obj.GUID)
    end
    else if not FParams.DisabledWizzards.Contains(P.FWizard.Obj.GUID) then
      FParams.DisabledWizzards.Add(P.FWizard.Obj.GUID);

    P.FWizard.Obj.IsActive := P.Value
  end;
end;

procedure TVMMainWizard.RegisterWizard(aWizard: TVMBaseWizardClass);
begin
  if aWizard = nil then
    Exit;

  FWizardsClass.Add(aWizard);
end;

{ TInitHelper }

constructor TInitHelper.Create(CallBack: TNotifyEvent);
begin
  inherited Create;
  Assert(Assigned(Callback));
  FInitTimer := TTimer.Create(nil);
  FInitTimer.Enabled := False;
  FInitTimer.OnTimer := OnInitTimer;
  FInitTimer.Interval := 400;
  FInitTimer.Enabled := True;
  FInitCount := 0;
  FCallback := CallBack;
end;

procedure TInitHelper.OnInitTimer(Sender: TObject);
begin
  Inc(FInitCount);
  if (FInitCount >= 4) then
  begin
    FInitTimer.Enabled := False;
    FreeAndNil(FInitTimer);
    if Assigned(FCallback) then
      FCallback(Self);
  end;
end;

{ TVMMainWizardParams }

constructor TVMMainWizardParams.Create;
begin
  inherited;
  FDisabledWizzards := TList<string>.Create;
end;

destructor TVMMainWizardParams.Destroy;
begin
  FreeAndNil(FDisabledWizzards);
  inherited;
end;

procedure TVMMainWizardParams.DoReadParams(aIni: TCustomIniFile);
var
  Cnt: Integer;
  I: Integer;
  GUID: string;
begin
  inherited;
  Cnt := aIni.ReadInteger(cstRegSection, cstRegCount, 0);
  for I := 0 to Cnt - 1 do
  begin
    GUID := aIni.ReadString(cstRegSection, Format(cstRegWizard, [I]), '');
    if GUID <> '' then
      FDisabledWizzards.Add(GUID);
  end;
end;

procedure TVMMainWizardParams.DoWriteParams(aIni: TCustomIniFile);
var
  I: Integer;
begin
  inherited;
  aIni.WriteInteger(cstRegSection, cstRegCount, FDisabledWizzards.Count);
  for I := 0 to FDisabledWizzards.Count - 1 do
    aIni.WriteString(cstRegSection, Format(cstRegWizard, [I]), FDisabledWizzards[I]);
end;

procedure TVMMainWizardParams.SetDefault;
begin
  inherited;
  if FDisabledWizzards <> nil then
    FDisabledWizzards.Clear;
end;

{ TWizardParam }

procedure TWizardParam.CopyFrom(aFrom: TBaseParam);
begin
  inherited;
  if aFrom is TWizardParam then
  begin
    if TWizardParam(aFrom).FWizard.IsAlive then
      FWizard := TObjectHolder<TVMBaseWizard>.Create(TWizardParam(aFrom).FWizard.Obj, False)
    else
      FWizard := nil;
  end;
end;

constructor TWizardParam.Create(aWizard: TVMBaseWizard);
begin
  inherited Create(aWizard.Caption, True);
  Value := aWizard.IsActive;
  FWizard := TObjectHolder<TVMBaseWizard>.Create(aWizard, False);
end;

{ TCnDebugLogger }

procedure TCnDebugLogger.Log(aType: TAbstractLogger.TLogType; const aMsg: string);
begin
  case aType of
    lpInfo:       CnDebugger.LogMsg('[Info] ' + aMsg);
    lpWarning:    CnDebugger.LogMsg('[Warning] ' + aMsg);
    lpError:      CnDebugger.LogMsg('[Error] ' + aMsg);
    lpDebug:      CnDebugger.LogMsg('[Debug] ' + aMsg);
    ltGroupStart: CnDebugger.LogEnter(aMsg);
    ltGroupEnd:   CnDebugger.LogLeave(aMsg);
  end;
end;

initialization
  SetLogger(TCnDebugLogger.Create);

end.

