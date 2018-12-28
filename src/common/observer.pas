unit observer;

interface

uses
  Generics.Collections, SysUtils, weak_ref, vmsys, vm.common.updatestack, syncobjs, OtlSync;

type
  IBaseObserver = interface(IWeakRefProvider)
  ['{23B09646-8E7C-4CE5-8134-E782C6B482FC}']
    function GetCanObserve: Boolean;

    property CanObserve: Boolean read GetCanObserve;
  end;

  TBaseObservableObject = class(TExtObject)
  private
    FObservers: TList<IWeakRef>;
  protected
    procedure ForEachObserver<T: IBaseObserver>(aIID: TGUID; aProc: TProc<T>);
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterObserver(aIID: TGUID; aObserver: IBaseObserver): Boolean;
    function UnRegisterObserver(aObserver: IBaseObserver): Boolean;
  end;

  IListenersRegistry<T: IInterface> = interface
    procedure RegisterListener(aListener: T);
    procedure RemoveListener(aListener: T);
  end;

  IAnnouncer<T: IInterface> = interface(IListenersRegistry<T>)
    procedure ForEachListener(aAction: TProc<T>);
  end;

  TAnnouncer<T: IInterface> = class(TInterfacedObject, IListenersRegistry<T>, IAnnouncer<T>)
  private
    FListeners: TList<T>;
  public
    destructor Destroy; override;

    procedure RegisterListener(aListener: T);
    procedure RemoveListener(aListener: T);
    procedure ForEachListener(aAction: TProc<T>);
  end;


  IDataObserver<T> = interface
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);

    ///   Called on mainThread when observableData<T> changed.
    procedure OnChanged(aData: T);

    /// Active observer will gets onChanged notification.
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  IObservableData<T> = interface
    function getValue: T;

    procedure RegisterObserver(aObserver: IDataObserver<T>);
    procedure RemoveObserver(aObserver: IDataObserver<T>);

    function HasActiveObservers: Boolean;
    property Value: T read GetValue;
  end;

  IMutableData<T> = interface(IObservableData<T>)
    procedure setValue(const Value: T);
    procedure postValue(const Value: T);
    property Value: T read GetValue write SetValue;
  end;

  TDataObservereCallback<T> = reference to procedure (aData: T);
  TDelegatedDataObserver<T> = class(TInterfacedObject, IDataObserver<T>)
  private
    FCallback: TDataObservereCallback<T>;
    FIsActive: Boolean;

    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  public
    constructor Create(aCallback: TDataObservereCallback<T>);
    procedure OnChanged(aData: T);

    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

  TOnDisposeValue<T> = reference to procedure (var Value: T);
  TObservableData<T> = class(TInterfacedObject, IObservableData<T>, IMutableData<T>)
  private
    FData: T;
    FDataLock: TOmniMREW;
    FPendingData: T;
    FPendingDataLock: TOmniCS;
    FObservers: TList<IDataObserver<T>>;
    FOnDisposeValue: TOnDisposeValue<T>;
    FDispatchInvalidated: Boolean;
    FDispatchingValue: Boolean;

    function getValue: T;
    procedure setValue(const Value: T);
    procedure postValue(const aValue: T);
    procedure DoPostValue(const aValue: T);
    procedure SwapBufData;

    procedure NotifyObserver(aObserver: IDataObserver<T>);
    procedure DispatchingValue(aObserver: IDataObserver<T>);
  public
    constructor Create; overload;
    constructor Create(aOnDisposeValue: TOnDisposeValue<T>); overload;
    destructor Destroy; override;

    procedure RegisterObserver(aObserver: IDataObserver<T>);
    procedure RemoveObserver(aObserver: IDataObserver<T>);

    function HasActiveObservers: Boolean;
  end;

implementation

uses
  Windows, generics.defaults, Classes;


constructor TBaseObservableObject.Create;
begin
  inherited Create;

  FObservers := TList<IWeakRef>.Create;
end;

destructor TBaseObservableObject.Destroy;
begin
  FreeAndNil(FObservers);
  inherited;
end;

procedure TBaseObservableObject.ForEachObserver<T>(aIID: TGUID; aProc: TProc<T>);
var
  Intf: T;
  ObserverRef: IWeakRef;
begin
  if Assigned(aProc) then
    for ObserverRef in FObservers do
      if ObserverRef.IsAlive and (ObserverRef.Intf as IBaseObserver).GetCanObserve and Supports(ObserverRef.Intf, aIID, Intf) then
        aProc(Intf);
end;

function TBaseObservableObject.RegisterObserver(aIID: TGUID; aObserver: IBaseObserver): Boolean;
begin
  Result := False;
  if (aObserver = nil) then
    Exit;

  Assert(Supports(aObserver, aIID));
  if FObservers.Contains(aObserver.WeakRef) then
    Exit;

  Result := FObservers.Add(aObserver.WeakRef) >= 0;
end;

function TBaseObservableObject.UnRegisterObserver(aObserver: IBaseObserver): Boolean;
begin
  Result := (aObserver <> nil) and (FObservers.Remove(aObserver.WeakRef) >= 0);
end;

{ TObservableData<T> }

destructor TObservableData<T>.Destroy;
begin
  TThread.RemoveQueuedEvents(nil, SwapBufData);

  if Assigned(FOnDisposeValue) then
    FOnDisposeValue(FData);

  FreeAndNil(FObservers);
  inherited;
end;

procedure TObservableData<T>.RegisterObserver(aObserver: IDataObserver<T>);
begin
  if aObserver = nil then
    Exit;

  if FObservers = nil then
    FObservers := TList<IDataObserver<T>>.Create;
  if not FObservers.Contains(aObserver) then
  begin
    FObservers.Add(aObserver);
    DispatchingValue(aObserver);
  end;
end;

procedure TObservableData<T>.RemoveObserver(aObserver: IDataObserver<T>);
begin
  if FObservers = nil then
    Exit;

  FObservers.Remove(aObserver);
end;

constructor TObservableData<T>.Create;
begin
  Create(nil);
end;

constructor TObservableData<T>.Create(aOnDisposeValue: TOnDisposeValue<T>);
begin
  inherited Create;
  FOnDisposeValue := aOnDisposeValue;
end;

procedure TObservableData<T>.NotifyObserver(aObserver: IDataObserver<T>);
begin
  if (aObserver = nil) or not aObserver.IsActive then
    Exit;

  aObserver.OnChanged(FData);
end;

procedure TObservableData<T>.DispatchingValue(aObserver: IDataObserver<T>);
var
  O: IDataObserver<T>;
begin
  if FObservers = nil then
    Exit;

  if FDispatchingValue then
  begin
    FDispatchInvalidated := True;
    Exit;
  end;

  FDispatchingValue := True;
  try
    repeat
      if (aObserver <> nil) then
      begin
        NotifyObserver(aObserver);
        aObserver := nil;
      end
      else
      begin
        for O in FObservers do
        begin
          NotifyObserver(O);
          if FDispatchInvalidated then
            Break;
        end;
      end;
    until (not FDispatchInvalidated);
  finally
    FDispatchingValue := False;
  end;
end;

function TObservableData<T>.getValue: T;
begin
  Result := FData;
end;

function TObservableData<T>.HasActiveObservers: Boolean;
var
  O: IDataObserver<T>;
begin
  for O in FObservers do
    if O.IsActive then
      Exit(True);

  Result := False;
end;

procedure TObservableData<T>.SwapBufData;
var
  Buf: T;
begin
  FPendingDataLock.Acquire;
  try
    Buf := FPendingData;
    FPendingData := Default(T);
  finally
    FPendingDataLock.Release;
  end;
  SetValue(Buf);
end;

procedure TObservableData<T>.DoPostValue(const aValue: T);
begin
  FPendingDataLock.Acquire;
  try
    if Assigned(FOnDisposeValue) and not TEqualityComparer<T>.Default.Equals(FPendingData, aValue) then
    begin
      FDataLock.EnterReadLock;
      try
        if not TEqualityComparer<T>.Default.Equals(FPendingData, FData) then
          FOnDisposeValue(FPendingData);
      finally
        FDataLock.ExitReadLock;
      end;
    end;

    FPendingData := aValue;
    TThread.RemoveQueuedEvents(nil, SwapBufData);
    TThread.Queue(nil, SwapBufData);
  finally
    FPendingDataLock.Release;
  end;
end;

procedure TObservableData<T>.postValue(const aValue: T);
begin
  if GetCurrentThreadId <> MainThreadID then
    DoPostValue(aValue)
  else
    setValue(aValue);
end;

procedure TObservableData<T>.setValue(const Value: T);
begin
  FDataLock.EnterWriteLock;
  try
    if Assigned(FOnDisposeValue) and not TEqualityComparer<T>.Default.Equals(FData, Value) then
      FOnDisposeValue(FData);

    FData := Value;
    DispatchingValue(nil);
  finally
    FDataLock.ExitWriteLock;
  end;
end;

{ TDelegatedDataObserver<T> }

constructor TDelegatedDataObserver<T>.Create(aCallback: TDataObservereCallback<T>);
begin
  inherited Create;
  FIsActive := True;
  FCallback := aCallback;
end;

function TDelegatedDataObserver<T>.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TDelegatedDataObserver<T>.OnChanged(aData: T);
begin
  if Assigned(FCallback) then
    FCallback(aData);
end;

procedure TDelegatedDataObserver<T>.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
end;

{ TAnnouncer<T> }

destructor TAnnouncer<T>.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TAnnouncer<T>.ForEachListener(aAction: TProc<T>);
var
  Listener: T;
begin
  if Assigned(aAction) and (FListeners <> nil) then
    for Listener in FListeners do
      aAction(Listener);
end;

procedure TAnnouncer<T>.RegisterListener(aListener: T);
begin
  if aListener = nil then
    Exit;

  if FListeners = nil then
    FListeners := TList<T>.Create;

  if not FListeners.Contains(aListener) then
    FListeners.Add(aListener);
end;

procedure TAnnouncer<T>.RemoveListener(aListener: T);
begin
  if FListeners <> nil then
    FListeners.Remove(aListener);
end;

end.
