unit vm.gui.control_updater;

interface

uses
  extctrls, sysutils, vm.common.updatestack;

type
  TControlUpdater = class(TUpdateStack)
  private
    FOnUpdateControl: TProc;
    FTimer: TTimer;
    FIsValidFlag: Integer;


    procedure DoUpdate;
    procedure DoEndUpdate;
    procedure OnTimer(aSender: TObject);
    function GetIsValid: Boolean;
    procedure SetIsValid(const Value: Boolean);
    property IsValid: Boolean read GetIsValid write SetIsValid;
  public
    constructor Create(aOnUpdateControl: TProc);
    destructor Destroy; override;
    procedure RequestUpdate;
  end;

implementation

uses
  windows, classes;

{ TControlUpdater }

constructor TControlUpdater.Create(aOnUpdateControl: TProc);
begin
  inherited Create(nil, DoEndUpdate);
  FOnUpdateControl := aOnUpdateControl;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FTimer.Interval := 100;
  FTimer.Enabled := False;
end;

destructor TControlUpdater.Destroy;
begin
  TThread.RemoveQueuedEvents(nil, DoUpdate);
  FreeAndNil(FTimer);
  inherited;
end;

procedure TControlUpdater.DoEndUpdate;
begin
  RequestUpdate;
end;

procedure TControlUpdater.DoUpdate;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    FOnUpdateControl;
    InterlockedExchange(FIsValidFlag, 1);
  end
  else
  begin
    TThread.RemoveQueuedEvents(nil, DoUpdate);
    TThread.Queue(nil, DoUpdate);
  end;
end;

procedure TControlUpdater.OnTimer(aSender: TObject);
begin
  if not IsUpdating and not IsValid then
    DoUpdate;

  FTimer.Enabled := False;
end;

procedure TControlUpdater.RequestUpdate;
begin
  FOnUpdateControl;
  exit;

  IsValid := False;
  if IsUpdating then
    Exit;

  if not FTimer.Enabled then
  begin
    DoUpdate;
    FTimer.Enabled := True;
  end
end;

procedure TControlUpdater.SetIsValid(const Value: Boolean);
begin
  InterlockedExchange(FIsValidFlag, Ord(Value));
end;

function TControlUpdater.GetIsValid: Boolean;
begin
  Result := FIsValidFlag = 1;
end;

end.
