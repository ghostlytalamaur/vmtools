unit vm_basewizard;

{$I cond_define.inc}

interface

uses
  vmsys, menus, classes, vm_internal_int, opt_impl, vm.ide.actions.manager, toolsapi;

type
  TVMBaseWizardClass = class of TVMBaseWizard;
  TVMBaseWizard = class(TExtObject)
  private
    FServices: IWizzardsServices;
    FIsActive: Boolean;

    function GetActionManager: TVMActionManager;
    procedure SetIsActive(const Value: Boolean);
  protected
    procedure RegisterWizard; virtual;
    procedure UnregisterWizard; virtual;

    property ActionManager: TVMActionManager read GetActionManager;
    property Services: IWizzardsServices read FServices;
  public
    constructor Create(aServices: IWizzardsServices); virtual;
    destructor Destroy; override;

    function CreateOptionsHandler: INTAAddInOptions; virtual;

    class function GUID: string; virtual;
    class function Caption: string; virtual;

    property IsActive: Boolean read FIsActive write SetIsActive;
  end;

implementation

uses
  dialogs, SysUtils, vmtools_cst, vm.debug;

{ TVMBaseWizard }

constructor TVMBaseWizard.Create(aServices: IWizzardsServices);
begin
  inherited Create;
  FServices := aServices;
end;

function TVMBaseWizard.CreateOptionsHandler: INTAAddInOptions;
begin
  Result := nil;
end;

destructor TVMBaseWizard.Destroy;
begin
  inherited;
end;

function TVMBaseWizard.GetActionManager: TVMActionManager;
begin
  if FServices <> nil then
    Result := FServices.ActionManager
  else
    Result := nil;
end;

class function TVMBaseWizard.GUID: string;
begin
  raise ENotImplemented.Create(ClassName + '.GUID');
end;

class function TVMBaseWizard.Caption: string;
begin
  Result := ClassName;
end;

procedure TVMBaseWizard.RegisterWizard;
begin
  Logger.i(rs_Msg_RegisterWizard, [Caption]);
end;

procedure TVMBaseWizard.SetIsActive(const Value: Boolean);
begin
  if FIsActive = Value then
    Exit;

  FIsActive := Value;
  if FIsActive then
    RegisterWizard
  else
    UnregisterWizard;
end;

procedure TVMBaseWizard.UnregisterWizard;
begin
//  InfoMsg(Format(rs_Msg_UnRegisterWizard, [Caption]));
end;

end.
