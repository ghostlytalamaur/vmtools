unit vm.ide.theming.main;

interface

uses
  toolsapi;

type
  TThemingHelper = class(TInterfacedObject)
  private
    FInitialStyleName: string;
  public
    procedure ApplyStyle;
    procedure RemoveStyle;
  end;

  TThemingSupportWizard = class(TInterfacedObject, IOTAWizard)
  private
    FHelper: TThemingHelper;
    FNotifierIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;

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

    { IOTAWizard }
    { Expert UI strings }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    { Launch the AddIn }
    procedure Execute;
  end;

procedure Register;

implementation

uses
  sysutils, vcl.themes, windows;

type
  TNTAIDEThemingServicesNotifier = class(TInterfacedObject, INTAIDEThemingServicesNotifier)
  private
    FHelper: TThemingHelper;
  public
    constructor Create(aHelper: TThemingHelper);
    destructor Destroy; override;
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

    { INTAIDEThemingServicesNotifier }
    { This notifier will be called immediately before the active IDE Theme changes }
    procedure ChangingTheme();
    { This notifier will be called immediately after the active IDE Theme changes }
    procedure ChangedTheme();
  end;

procedure Register;
begin
  if (BorlandIDEServices <> nil) then
    RegisterPackageWizard(TThemingSupportWizard.Create);
end;

procedure SendDebug(const Msg: string); overload;
begin
  OutputDebugString(PWideChar('[vm.ide.theming] ' + Msg));
end;

procedure SendDebug(const Fmt: string; const Args: array of const); overload;
begin
  SendDebug(Format(Fmt, Args));
end;

{ TThemingSupportWizard }

procedure TThemingSupportWizard.AfterSave;
begin

end;

procedure TThemingSupportWizard.BeforeSave;
begin

end;

constructor TThemingSupportWizard.Create;
var
  Srv: IOTAIDEThemingServices;
begin
  inherited;
  FHelper := TThemingHelper.Create;
  FHelper._AddRef;

  if Supports(BorlandIDEServices, IOTAIDEThemingServices, Srv) then
    FNotifierIndex := Srv.AddNotifier(TNTAIDEThemingServicesNotifier.Create(FHelper));
  FHelper.ApplyStyle;
end;

destructor TThemingSupportWizard.Destroy;
begin
  FHelper._Release;
  inherited;
end;

procedure TThemingSupportWizard.Destroyed;
begin

end;

procedure TThemingSupportWizard.Execute;
begin

end;

function TThemingSupportWizard.GetIDString: string;
begin
  Result := '{260D306E-3C7F-4315-93BA-DCFD9781C291}';
end;

function TThemingSupportWizard.GetName: string;
begin
  Result := 'VMTools IDE Theming Support';
end;

function TThemingSupportWizard.GetState: TWizardState;
begin
  Result := [wsEnabled]
end;

procedure TThemingSupportWizard.Modified;
begin

end;

{ TNTAIDEThemingServicesNotifier }

procedure TNTAIDEThemingServicesNotifier.AfterSave;
begin

end;

procedure TNTAIDEThemingServicesNotifier.BeforeSave;
begin

end;

procedure TNTAIDEThemingServicesNotifier.ChangedTheme;
begin
  FHelper.ApplyStyle;
end;

procedure TNTAIDEThemingServicesNotifier.ChangingTheme;
begin

end;

constructor TNTAIDEThemingServicesNotifier.Create(aHelper: TThemingHelper);
begin
  inherited Create;
  FHelper := aHelper;
  FHelper._AddRef;
end;

destructor TNTAIDEThemingServicesNotifier.Destroy;
begin
  FHelper._Release;
  inherited;
end;

procedure TNTAIDEThemingServicesNotifier.Destroyed;
begin

end;

procedure TNTAIDEThemingServicesNotifier.Modified;
begin

end;

procedure TThemingHelper.ApplyStyle;
var
  Srv: IOTAIDEThemingServices;
begin
  try
    if not Supports(BorlandIDEServices, IOTAIDEThemingServices, Srv) then
      Exit;

    if FInitialStyleName = '' then
      FInitialStyleName := TStyleManager.ActiveStyle.Name;
    SendDebug('Active style: %s', [TStyleManager.ActiveStyle.Name]);
    if Srv.IDEThemingEnabled then
    begin
      TStyleManager.SystemHooks := [shDialogs];
      TStyleManager.SetStyle(Srv.StyleServices);
    end
    else
    begin
      SendDebug('IDE theming disabled');
      RemoveStyle;
    end;
    SendDebug('New style: %s', [TStyleManager.ActiveStyle.Name]);
  except
    on E: Exception do
      SendDebug('ApplyStyle: exception <%s> with message <%s>', [E.ClassName, E.Message]);
  end;
end;

procedure TThemingHelper.RemoveStyle;
var
  Srv: IOTAIDEThemingServices;
begin
  try
    if not Supports(BorlandIDEServices, IOTAIDEThemingServices, Srv) then
      Exit;

    SendDebug('RemoveStyle');
    TStyleManager.UnRegisterStyle(Srv.StyleServices);
    if FInitialStyleName <> '' then
    begin
      TStyleManager.SetStyle(FInitialStyleName);
      TStyleManager.SystemHooks := [shMenus, shDialogs, shToolTips];
    end;
  except
    on E: Exception do
      SendDebug('RemoveStyle: exception <%s> with message <%s>', [E.ClassName, E.Message]);
  end;
end;



end.
