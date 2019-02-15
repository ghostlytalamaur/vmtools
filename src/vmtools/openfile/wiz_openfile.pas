unit wiz_openfile;

interface

uses
  vmsys, vm_basewizard, toolsapi, vm_internal_int, Classes, Menus,
  Windows, wiz_openfile_dockform, forms, wiz_openfile_handler,
  openfile_frame, vm_custom_dock_frm, base_params, opt_impl;

type
  TVMOpenFileWizardParams = class(TOpenFileHandlerParams)
  private const
    cstRegSection = 'OpenFileWizard';
  protected
    function CreateTree: TParamsTree; override;
  end;

  TVMOpenFileWizard = class(TVMBaseWizard)
  private const
    cstActCaption_OpenFile = 'Open file...';
  private
    FParams: TVMOpenFileWizardParams;
    FParamsObserver: IParamsChangedObserver;
    FDockForm: IVMCustomDockForm;
    FDlg: IObjectHolder<TCustomForm>;
    FProvider: TOpenFileHandler;
    FNotifierIdx: Integer;

    procedure OnShowOpenFileDialog(aSender: TObject);
    procedure OnCreateNewFile(aSender: TObject);
    function GetDlg: IObjectHolder<TCustomForm>;
    function GetDockForm: IVMCustomDockForm;
    function GetFrame: TOpenFileFrame;
    function GetHandler: TOpenFileHandler;
    function GetParams: TVMOpenFileWizardParams;

    procedure ParamsChanged;
    procedure RefreshShortCuts;
    procedure RegisterShortCuts;
    procedure UnregisterShortCuts;
    procedure RegisterMenu;
    procedure UnregisterMenu;

    property Dlg: IObjectHolder<TCustomForm> read GetDlg;
    property MainFrame: TOpenFileFrame read GetFrame;
    property Handler: TOpenFileHandler read GetHandler;
    property Params: TVMOpenFileWizardParams read GetParams;
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    destructor Destroy; override;

    function CreateOptionsHandler: INTAAddInOptions; override;

    class function GUID: string; override;
    class function Caption: string; override;
  end;

implementation

uses
  vm_ide_utils, SysUtils, vmtools_cst, ioutils, deskutil, masks,
  vm.ide.options.treehandler, vm.debug, new_file_dlg;

type
  TExtOpenFileHandler = class(TOpenFileHandler)
  private
    FWiz: TVMOpenFileWizard;
  public
    constructor Create(aWiz: TVMOpenFileWizard);
    procedure OpenFile(const aFilePath: string); override;
  end;

  TIDENotifier = class(TInterfacedObject, IOTANotifier, IOTAIDENotifier)
  private
    FHandler: TOpenFileHandler;
  public
    constructor Create(aHandler: TOpenFileHandler);
  { IOTANotifier }
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IOTAWizards }
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IOTAWizard }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IOTAWizards }
    procedure Modified;

  { IOTAIDENotifier }
    { This procedure is called for many various file operations within the
      IDE }
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    { This function is called immediately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

{ TVMOpenFileWizard }

function TVMOpenFileWizard.GetFrame: TOpenFileFrame;
var
  Frame: TCustomFrame;
begin
  Frame := nil;
  if (Dlg <> nil) and (FDockForm <> nil) then
    Frame := FDockForm.GetMainFrame;

  if Frame is TOpenFileFrame then
    Result := TOpenFileFrame(Frame)
  else
    Result := nil;
end;

function TVMOpenFileWizard.CreateOptionsHandler: INTAAddInOptions;
begin
  Result := TVMOptionsTreeHandler.Create('Open file', Params.Tree);
end;

destructor TVMOpenFileWizard.Destroy;
begin
  FreeAndNil(FProvider);
  FreeAndNil(FParams);
  inherited;
end;

function TVMOpenFileWizard.GetDlg: IObjectHolder<TCustomForm>;
begin
  if (FDlg = nil) and (FDockForm <> nil) then
    FDlg := TObjectHolder<TCustomForm>.Create(
        (BorlandIDEServices as INTAServices).CreateDockableForm(FDockForm), False);
  Result := FDlg;
end;

function TVMOpenFileWizard.GetDockForm: IVMCustomDockForm;
begin
  if FDockForm = nil then
    FDockForm := TVMOpenFileDockForm.Create(Handler);
  Result := FDockForm;
end;

function TVMOpenFileWizard.GetHandler: TOpenFileHandler;
begin
  if FProvider = nil then
    FProvider := TExtOpenFileHandler.Create(Self);
  Result := FProvider;
end;

function TVMOpenFileWizard.GetParams: TVMOpenFileWizardParams;
begin
  if FParams = nil then
  begin
    FParams := TVMOpenFileWizardParams.Create;
    FParams.ReadParams;
  end;
  Result := FParams;

  if FParamsObserver = nil then
  begin
    FParamsObserver := TParamsObserver.Create(
      procedure
      begin
        ParamsChanged;
      end);

    Result.RegisterObserver(IParamsChangedObserver, FParamsObserver);
  end;
end;

class function TVMOpenFileWizard.GUID: string;
begin
  Result := cstOpenFile_GUID;
end;

class function TVMOpenFileWizard.Caption: string;
begin
  Result := cstOpenFile_Caption;
end;

procedure TVMOpenFileWizard.OnCreateNewFile(aSender: TObject);
var
  Dlg: TCreateFileDlg;
  OnOpenFile: TProc<string, Integer, Integer>;
begin
  OnOpenFile := procedure (aFilePath: string; aLineNum, aStartColumn: Integer)
    begin
      if not aFilePath.IsEmpty then
      begin
        TGXOtaUtils.GxOtaGoToFileLineColumn(aFilePath, aLineNum, aStartColumn);
        TVMOtaUtils.Reloadfile(aFilePath);
      end;
    end;
  Dlg := TCreateFileDlg.Create(nil, OnOpenFile);
  try
    Dlg.SetPaths(TVMOtaUtils.GetProjectPathsList);
    Dlg.ShowModal;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TVMOpenFileWizard.OnShowOpenFileDialog(aSender: TObject);
begin
  if (Dlg = nil) or (MainFrame = nil) or not Dlg.IsAlive then
    Exit;

  if Dlg.Obj.Floating then
  begin
    Dlg.Obj.Show;
    Dlg.Obj.BringToFront
  end
  else
    FocusWindow(Dlg.Obj);
  if Dlg.Obj.CanFocus then
  begin
    Dlg.Obj.SetFocus;
    MainFrame.cmbFilter.SetFocus;
  end;
end;

procedure TVMOpenFileWizard.ParamsChanged;
begin
  if IsActive then
    RefreshShortCuts;
  Params.WriteParams;
end;

procedure TVMOpenFileWizard.RefreshShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    Logger.i(cstOpenFile_Msg_RefreshShortcuts);
    UnregisterShortCuts;
    RegisterShortCuts;
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMOpenFileWizard.RegisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.RegisterInternalAction(cstActCaption_OpenFile, OnShowOpenFileDialog,
        Shortcut(TextToShortCut('O'), [ssAlt, ssShift]));
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMOpenFileWizard.RegisterWizard;
begin
  inherited;
  RefreshShortcuts;
  RegisterMenu;
  (BorlandIDEServices as INTAServices).RegisterDockableForm(GetDockForm);
  FNotifierIdx := (BorlandIDEServices as IOTAServices50).AddNotifier(TIDENotifier.Create(Handler));
end;

procedure TVMOpenFileWizard.UnregisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    Services.ActionManager.UnRegisterInternalAction(cstActCaption_OpenFile);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMOpenFileWizard.RegisterMenu;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.RegisterInternalMenuAction('Create file in search paths...', OnCreateNewFile, 0);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMOpenFileWizard.UnregisterMenu;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.UnRegisterInternalMenuAction('Create file in search paths...');
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMOpenFileWizard.UnregisterWizard;
begin
  UnregisterShortcuts;
  UnregisterMenu;
  (BorlandIDEServices as IOTAServices50).RemoveNotifier(FNotifierIdx);
  FNotifierIdx := -1;
  if FDockForm <> nil then
    (BorlandIDEServices as INTAServices).UnregisterDockableForm(FDockForm);
  FDockForm := nil;
  FDlg := nil;
  FreeAndNil(FProvider);
  inherited;
end;

{ TExtOpenFileHandler }

constructor TExtOpenFileHandler.Create(aWiz: TVMOpenFileWizard);
begin
  inherited Create(aWiz.Params);
  FWiz := aWiz;
end;

procedure TExtOpenFileHandler.OpenFile(const aFilePath: string);
begin
  LogEnterLeave('TExtOpenFileHandler.OpenFile');
  inherited;
  if (FWiz.Dlg <> nil) and FWiz.Dlg.IsAlive and FWiz.Dlg.Obj.Floating then
    FWiz.Dlg.Obj.Hide;
end;

{ TIDENotifier }

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TIDENotifier.AfterSave;
begin

end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TIDENotifier.BeforeSave;
begin

end;

constructor TIDENotifier.Create(aHandler: TOpenFileHandler);
begin
  inherited Create;
  FHandler := aHandler;
end;

procedure TIDENotifier.Destroyed;
begin

end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);

  function NotifyCodeToString(aCode: TOTAFileNotification): string;
  begin
    case aCode of
      ofnFileOpening: Result := 'ofnFileOpening';
      ofnFileOpened: Result := 'ofnFileOpened';
      ofnFileClosing: Result := 'ofnFileClosing';
      ofnDefaultDesktopLoad: Result := 'ofnDefaultDesktopLoad';
      ofnDefaultDesktopSave: Result := 'ofnDefaultDesktopSave';
      ofnProjectDesktopLoad: Result := 'ofnProjectDesktopLoad';
      ofnProjectDesktopSave: Result := 'ofnProjectDesktopSave';
      ofnPackageInstalled: Result := 'ofnPackageInstalled';
      ofnPackageUninstalled: Result := 'ofnPackageUninstalled';
      ofnActiveProjectChanged: Result := 'ofnActiveProjectChanged';
    else
      Result := IntToStr(Ord(aCode));
    end;
  end;

begin
  Logger.d('%s: FileName: %s', [NotifyCodeToString(NotifyCode), FileName]);
  case NotifyCode of
//    ofnFileOpening, ofnFileOpened, ofnFileClosing,
//    ofnDefaultDesktopLoad, ofnDefaultDesktopSave, ofnProjectDesktopLoad,
//    ofnProjectDesktopSave, ofnPackageInstalled, ofnPackageUninstalled,
    ofnActiveProjectChanged{, ofnProjectOpenedFromTemplate}:
      if FHandler <> nil then
        FHandler.InvalidatePaths;
  end;
end;

procedure TIDENotifier.Modified;
begin

end;

{ TVMOpenFileWizardParams }

function TVMOpenFileWizardParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(cstRegSection, cstOpenFile_ParamsAlias);
end;

end.
