unit wiz.tabs;

interface

uses
  vm_basewizard, collections.lists, vm_internal_int;

type
  TVMTabsWizard = class(TVMBaseWizard)
  type
    ActionNames = record
    public const
      LeastRecentlyUsedTab = 'vmWizTabsLeastRecentlyUsedTab';
      PreviousRecentlyUsedTab = 'vmWizTabsPreviousRecentlyUsedTab';
    end;
  private
    FRecentFiles: IList<string>;
    FNotifierIdx: Integer;
    FIDENotifierIndex: Integer;

    procedure OnActivateRecentlyUsedTab(aSender: TObject);
    procedure OnActivateLeastUsedTab(aSender: TObject);

    procedure OpenFileAt(Index: Integer);
    function SelectFile(InitialIndex: Integer): Integer;

    procedure OnFileOpened(FilePath: string);
    procedure OnFileClosed(FilePath: string);
    
    procedure RefreshShortCuts;
    procedure RegisterShortCuts;
    procedure UnregisterShortCuts;
    procedure FillOpenedFiles;
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    constructor Create(aServices: IWizzardsServices); override;
    class function GUID: string; override;
    class function Caption: string; override;
  end;

implementation

uses
  vcl.menus, classes, windows, ToolsAPI, dockform, system.sysutils, vm_ide_utils, vm.debug,
  wiz.tabs.form, controls, math;

type
  TNTAEditServicesNotifier = class(TInterfacedObject, IOtaNotifier, INTAEditServicesNotifier)
  private
    FOnFileOpened: TProc<string>;
  public
    constructor Create(OnFileOpened: TProc<string>);
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

    { INTAEditServicesNotifier }

    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

  TIDENotifier = class(TInterfacedObject, IOTANotifier, IOTAIDENotifier)
  private
    FOnFileClosed: TProc<string>;
    FOnFileOpened: TProc<string>;
  public
    constructor Create(OnFileClosed: TProc<string>; OnFileOpened: TProc<string>);
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

{ TVMTabsWizard }

class function TVMTabsWizard.GUID: string;
begin
  Result := '{FBED3799-C358-47F3-AD23-6D08E0E3B685}';
end;

class function TVMTabsWizard.Caption: string;
begin
  Result := 'Recent Tabs Wizard';
end;

procedure TVMTabsWizard.RegisterWizard;
begin
  inherited;
  FillOpenedFiles;
  RefreshShortCuts;
  FNotifierIdx := (BorlandIDEServices as IOTAEditorServices).AddNotifier(TNTAEditServicesNotifier.Create(OnFileOpened));
  FIDENotifierIndex := (BorlandIDEServices as IOTAServices50).AddNotifier(TIDENotifier.Create(OnFileClosed, OnFileOpened));
end;

procedure TVMTabsWizard.UnregisterWizard;
begin
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);
  FNotifierIdx := -1;

  (BorlandIDEServices as IOTAServices50).RemoveNotifier(FIDENotifierIndex);
  FIDENotifierIndex := -1;

  UnregisterShortCuts;
  inherited;
end;

procedure TVMTabsWizard.RefreshShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    UnregisterShortCuts;
    RegisterShortCuts;
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMTabsWizard.RegisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.RegisterAction(ActionNames.PreviousRecentlyUsedTab, 'Quick Open Previous Recently Used Tab',
        Shortcut(VK_TAB, [ssCtrl]), OnActivateRecentlyUsedTab);
    ActionManager.RegisterAction(ActionNames.LeastRecentlyUsedTab, 'Quick Open Least Recently Used Tab',
        Shortcut(VK_TAB, [ssCtrl, ssShift]), OnActivateLeastUsedTab);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMTabsWizard.UnregisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.UnRegsiterAction(ActionNames.PreviousRecentlyUsedTab);
    ActionManager.UnRegsiterAction(ActionNames.LeastRecentlyUsedTab);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMTabsWizard.FillOpenedFiles;
var
  ModServicies: IOTAModuleServices;
  I: Integer;
  Module: IOTAModule;
begin
  ModServicies:= BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModServicies.ModuleCount - 1 do
  begin
    Module := ModServicies.Modules[I];
    FRecentFiles.Add(Module.FileName);
  end;
end;

procedure TVMTabsWizard.OnActivateRecentlyUsedTab(aSender: TObject);
begin
  OpenFileAt(SelectFile(Min(FRecentFiles.Count - 1, 1)));
end;

procedure TVMTabsWizard.OnActivateLeastUsedTab(aSender: TObject);
begin
  OpenFileAt(SelectFile(FRecentFiles.Count - 1));
end;

procedure TVMTabsWizard.OpenFileAt(Index: Integer);
var
  FilePath: string;
begin
  if (Index < 0) or (Index >= FRecentFiles.Count) then
    Exit;

  FilePath := FRecentFiles[Index];
  TGXOtaUtils.GxOtaGoToFileLineColumn(FilePath, -1);
end;

function TVMTabsWizard.SelectFile(InitialIndex: Integer): Integer;
var
  Dialog: TTabsListForm;
begin
  Dialog := TTabsListForm.Create(nil, FRecentFiles, InitialIndex);
  try
    if (Dialog.ShowModal = mrOk) then
      Result := Dialog.lvFiles.ItemIndex
    else
      Result := -1;

    if (Result >= FRecentFiles.Count ) then
      Result := -1;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TVMTabsWizard.OnFileOpened(FilePath: string);
var
  Index: Integer;
begin
  Index := FRecentFiles.IndexOf(FilePath);
  if (Index >= 0) then
    FRecentFiles.Delete(Index);
  FRecentFiles.Insert(0, FilePath);
end;

procedure TVMTabsWizard.OnFileClosed(FilePath: string);
begin
  Logger.d('FileClosed: ' + FilePath);
  FRecentFiles.Extract(FilePath);
end;

constructor TVMTabsWizard.Create(aServices: IWizzardsServices);
begin
  inherited Create(aServices);
  FRecentFiles := TListImpl<string>.Create;
  FNotifierIdx := -1;
end;

{ TNTAEditServicesNotifier }

procedure TNTAEditServicesNotifier.AfterSave;
begin

end;

procedure TNTAEditServicesNotifier.BeforeSave;
begin

end;

procedure TNTAEditServicesNotifier.Destroyed;
begin

end;

procedure TNTAEditServicesNotifier.Modified;
begin

end;

procedure TNTAEditServicesNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin

end;

procedure TNTAEditServicesNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin

end;

procedure TNTAEditServicesNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
end;

procedure TNTAEditServicesNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin

end;

procedure TNTAEditServicesNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  FOnFileOpened(TVMOtaUtils.GetCurrentOpenFileName);
end;

procedure TNTAEditServicesNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin

end;

procedure TNTAEditServicesNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TNTAEditServicesNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TNTAEditServicesNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

constructor TNTAEditServicesNotifier.Create(OnFileOpened: TProc<string>);
begin
  inherited Create;
  FOnFileOpened := OnFileOpened;
end;

{ TIDENotifier }

constructor TIDENotifier.Create(OnFileClosed: TProc<string>; OnFileOpened: TProc<string>);
begin
  inherited Create;
  FOnFileOpened := OnFileOpened;
  FOnFileClosed := OnFileClosed;
end;

procedure TIDENotifier.AfterSave;
begin

end;

procedure TIDENotifier.BeforeSave;
begin

end;

procedure TIDENotifier.Destroyed;
begin

end;

procedure TIDENotifier.Modified;
begin

end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName).ToLower;
  if (Ext = '.groupproj') or (Ext = '.dproj') then
    Exit;

  case NotifyCode of
    ofnFileOpened:
      FOnFileOpened(FileName);
    ofnFileClosing:
      FOnFileClosed(FileName);
  end;
end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

initialization
  TVMOtaUtils.RegisterFormClassForTheming(TTabsListForm);

end.
