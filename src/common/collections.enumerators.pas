unit collections.enumerators;

interface

uses
  vmsys, generics.collections, collections.types, sysutils;

type
  TGenericEnumerator = class(TInterfacedObject, IEnumerator)
  protected
    function DoGetCurrentObj: TObject; virtual;
    function DoMoveNext: Boolean; virtual;
    procedure DoReset; virtual;
  public
    { IEnumerator }
    function IEnumerator.GetCurrent = DoGetCurrentObj;
    function IEnumerator.MoveNext = DoMoveNext;
    procedure Reset;
  end;

type
  TBaseEnumerableImpl = class(TInterfacedObject, IEnumerable)
  protected
    function GetPointerEnumerator: IEnumerator; virtual;
  public
    { IEnumerable }
    function IEnumerable.GetEnumerator = GetPointerEnumerator;
  end;

type
  TEnumeratorImpl<T> = class(TGenericEnumerator, IEnumerator<T>)
  protected
    function DoGetCurrent: T; virtual;
  public
    { IEnumerator<T> }
    function IEnumerator<T>.GetCurrent = DoGetCurrent;
  end;

type
  TEnumerableImpl<T> = class(TBaseEnumerableImpl, IEnumerable<T>)
  protected
    function DoGetEnumerator: IEnumerator<T>; virtual; abstract;
  public
    { IEnumerable<T> }
    function IEnumerable<T>.GetEnumerator = DoGetEnumerator;
  end;

type
  TBaseKeyValueEnumerable<K, V, T> = class(TEnumerableImpl<T>)
  protected type
    TBaseKeyValueEnumerator = class(TEnumeratorImpl<T>)
    private
      FPairs: IEnumerable<TPair<K, V>>;
      FEnumerator: IEnumerator<TPair<K, V>>;
    protected
      function DoMoveNext: Boolean; override;
      procedure DoReset; override;
    public
      constructor Create(aPairs: IEnumerable<TPair<K, V>>);
    end;

  private
    FPairs: IEnumerable<TPair<K, V>>;
  public
    constructor Create(aPairs: IEnumerable<TPair<K, V>>);
  end;

type
  TKeysEnumerable<K, V> = class(TBaseKeyValueEnumerable<K, V, K>)
  private type
    TKeysEnumerator = class(TBaseKeyValueEnumerable<K, V, K>.TBaseKeyValueEnumerator)
    protected
      function DoGetCurrent: K; override;
    end;
  protected
    function DoGetEnumerator: IEnumerator<K>; override;
  end;

type
  TValuesEnumerable<K, V> = class(TBaseKeyValueEnumerable<K, V, V>)
  private type
    TValuesEnumerator = class(TBaseKeyValueEnumerable<K, V, V>.TBaseKeyValueEnumerator)
    protected
      function DoGetCurrent: V; override;
    end;
  protected
    function DoGetEnumerator: IEnumerator<V>; override;
  end;

type
  TWrapEnumerator<T> = class(TEnumeratorImpl<T>)
  private
    FEnumerable: IObjectHolder<TEnumerable<T>>;
    FEnumerator: TEnumerator<T>;
    FOwnEnumerator: Boolean;
  protected
    function DoMoveNext: Boolean; override;
    function DoGetCurrent: T; override;
    procedure DoReset; override;
  public
    constructor Create(aEnumerable: TEnumerable<T>); overload;
    constructor Create(aEnumerable: IObjectHolder<TEnumerable<T>>); overload;
    constructor Create(aEnumerator: TEnumerator<T>; aOwnEnumerator: Boolean); overload;
    destructor Destroy; override;
  end;

type
  TEmptyEnumerator<T> = class(TEnumeratorImpl<T>)
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  end;

type
  TMappingEnumerator<T, R> = class(TEnumeratorImpl<R>)
  private
    FSource: IEnumerable<T>;
    FMapper: TMapper<T, R>;

    FEnumerator: IEnumerator<T>;
    FCurrent: R;
  protected
    function DoGetCurrent: R; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aSource: IEnumerable<T>; aMapper: TMapper<T, R>);
  end;

type
  TIndexedEnumerator<T> = class(TEnumeratorImpl<T>)
  public
    FStart: Integer;
    FStep: Integer;
    FCount: Integer;
    FCurrentIndex: Integer;
    FCurrent: T;
    FOnGetItem: TFunc<Integer, T>;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aStart, aStep, aCount: Integer; aOnGetItem: TFunc<Integer, T>);
  end;

type
  TEnumerableFactory<T> = class(TEnumerableImpl<T>)
  private
    FFactory: TFunc<IEnumerator<T>>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(aFactory: TFunc<IEnumerator<T>>);
  end;

type
  TPipelineEnumerator<T> = class(TEnumeratorImpl<T>)
  private
    FSourceEnumerator: IEnumerator<T>;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aSourceEnumerator: IEnumerator<T>);
  end;

type
  TFilterEnumerator<T> = class(TPipelineEnumerator<T>)
  private
    FPredicate: TFunc<T, Boolean>;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aSourceEnumerator: IEnumerator<T>; aPredicate: TFunc<T, Boolean>);
  end;

type
  TPipelineMappingEnumerator<T, R> = class(TEnumeratorImpl<R>)
  private
    FSourceEnumerator: IEnumerator<T>;
    FMapper: TMapper<T, R>;
    FCurrent: R;
  protected
    function DoGetCurrent: R; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aSource: IEnumerator<T>; aMapper: TMapper<T, R>);
  end;

type
  TPipelineMappingEnumerable<T, R> = class(TEnumerableImpl<R>)
  private
    FSourceEnumerable: IEnumerable<T>;
    FFactory: TFunc<IEnumerator<T>, IEnumerator<R>>;
  protected
    function DoGetEnumerator: IEnumerator<R>; override;
  public
    constructor Create(aSourceEnumerable: IEnumerable<T>; aFactory: TFunc<IEnumerator<T>, IEnumerator<R>>);
  end;

implementation

{ TGenericEnumerator }

function TGenericEnumerator.DoGetCurrentObj: TObject;
begin
  Result := nil;
end;

function TGenericEnumerator.DoMoveNext: Boolean;
begin
  Result := False;
end;

procedure TGenericEnumerator.DoReset;
begin

end;

procedure TGenericEnumerator.Reset;
begin
  DoReset;
end;

{ TGenericEnumerable }

function TBaseEnumerableImpl.GetPointerEnumerator: IEnumerator;
begin
  Result := nil;
end;

{ TEnumeratorImpl<T> }

function TEnumeratorImpl<T>.DoGetCurrent: T;
begin
  Result := Default(T);
end;

{ TBaseKeyValueEnumerable<K, V> }

constructor TBaseKeyValueEnumerable<K, V, T>.Create(aPairs: IEnumerable<TPair<K, V>>);
begin
  inherited Create;
  FPairs := aPairs;
end;

{ TBaseKeyValueEnumerable<K, V>.TBaseKeyValueEnumerator<TT> }

constructor TBaseKeyValueEnumerable<K, V, T>.TBaseKeyValueEnumerator.Create(aPairs: IEnumerable<TPair<K, V>>);
begin
  inherited Create;
  FPairs := aPairs;
  Reset;
end;

function TBaseKeyValueEnumerable<K, V, T>.TBaseKeyValueEnumerator.DoMoveNext: Boolean;
begin
  Result := (FEnumerator <> nil) and FEnumerator.MoveNext;
end;

procedure TBaseKeyValueEnumerable<K, V, T>.TBaseKeyValueEnumerator.DoReset;
begin
  if FPairs <> nil then
    FEnumerator := FPairs.GetEnumerator
  else
    FEnumerator := nil;
  inherited;
end;

{ TKeysEnumerable<K, V> }

function TKeysEnumerable<K, V>.DoGetEnumerator: IEnumerator<K>;
begin
  Result := TKeysEnumerator.Create(FPairs);
end;

{ TKeysEnumerable<K, V>.TKeysEnumerator }

function TKeysEnumerable<K, V>.TKeysEnumerator.DoGetCurrent: K;
begin
  if FEnumerator <> nil then
    Result := FEnumerator.Current.Key
  else
    Result := Default(K);
end;

{ TValuesEnumerable<K, V>.TKeysEnumerator }

function TValuesEnumerable<K, V>.TValuesEnumerator.DoGetCurrent: V;
begin
  if (FEnumerator <> nil) then
    Result := FEnumerator.Current.Value
  else
    Result := Default(V);
end;

{ TValuesEnumerable<K, V> }

function TValuesEnumerable<K, V>.DoGetEnumerator: IEnumerator<V>;
begin
  Result := TValuesEnumerator.Create(FPairs);
end;

{ TWrapEnumerator<T> }

constructor TWrapEnumerator<T>.Create(aEnumerable: TEnumerable<T>);
begin
  Create(TObjectHolder<TEnumerable<T>>.Create(aEnumerable, False));
end;

constructor TWrapEnumerator<T>.Create(aEnumerable: IObjectHolder<TEnumerable<T>>);
begin
  inherited Create;
  FEnumerable := aEnumerable;
  Reset;
end;

constructor TWrapEnumerator<T>.Create(aEnumerator: TEnumerator<T>; aOwnEnumerator: Boolean);
begin
  inherited Create;
  FEnumerator := aEnumerator;
  FOwnEnumerator := aOwnEnumerator;
end;

destructor TWrapEnumerator<T>.Destroy;
begin
  if FOwnEnumerator then
    FreeAndNil(FEnumerator);
  inherited;
end;

function TWrapEnumerator<T>.DoGetCurrent: T;
begin
  if FEnumerator <> nil then
    Result := FEnumerator.Current
  else
    Result := Default(T);
end;

function TWrapEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := (FEnumerator <> nil) and FEnumerator.MoveNext;
end;

procedure TWrapEnumerator<T>.DoReset;
begin
  inherited;
  if FOwnEnumerator then
    FreeAndNil(FEnumerator);
  FEnumerator := nil;
  if (FEnumerable <> nil) and FEnumerable.IsAlive then
  begin
    FEnumerator := FEnumerable.Obj.GetEnumerator;
    FOwnEnumerator := True;
  end;
end;

{ TEmptyEnumerator<T> }

function TEmptyEnumerator<T>.DoGetCurrent: T;
begin
  Result := Default(T);
end;

function TEmptyEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := False;
end;

{ TMappingEnumerator<T, R> }

constructor TMappingEnumerator<T, R>.Create(aSource: IEnumerable<T>; aMapper: TMapper<T, R>);
begin
  inherited Create;
  FSource := aSource;
  FMapper := aMapper;
  Reset;
end;

function TMappingEnumerator<T, R>.DoGetCurrent: R;
begin
  Result := FCurrent;
end;

function TMappingEnumerator<T, R>.DoMoveNext: Boolean;
begin
  Result := (FEnumerator <> nil) and FEnumerator.MoveNext;
  if Result then
    FCurrent := FMapper(FEnumerator.Current)
  else
    FCurrent := Default(R);
end;

procedure TMappingEnumerator<T, R>.DoReset;
begin
  inherited;
  if FSource <> nil then
    FEnumerator := FSource.GetEnumerator
  else
    FEnumerator := nil;
end;

{ TIndexedEnumerator<T> }

constructor TIndexedEnumerator<T>.Create(aStart, aStep, aCount: Integer; aOnGetItem: TFunc<Integer, T>);
begin
  inherited Create;
  FStart := aStart;
  FStep := aStep;
  FCount := aCount;
  FOnGetItem := aOnGetItem;
  Reset;
end;

function TIndexedEnumerator<T>.DoGetCurrent: T;
begin
  Result := FCurrent;
end;

function TIndexedEnumerator<T>.DoMoveNext: Boolean;
var
  NextIdx, Last: Integer;
begin
  NextIdx := FCurrentIndex + FStep;
  if FStep > 0 then
  begin
    Last := FStart + FStep * FCount - 1;
    Result := (NextIdx >= FStart) and (NextIdx <= Last);
  end
  else
  begin
    Last := FStart + FStep * FCount + 1;
    Result := (NextIdx >= Last) and (NextIdx <= FStart);
  end;

  if Result then
  begin
    FCurrentIndex := NextIdx;
    FCurrent := FOnGetItem(FCurrentIndex);
  end;
end;

procedure TIndexedEnumerator<T>.DoReset;
begin
  inherited;
  FCurrentIndex := FStart - FStep;
end;

{ TEnumerableFactory<T> }

constructor TEnumerableFactory<T>.Create(aFactory: TFunc<IEnumerator<T>>);
begin
  inherited Create;
  FFactory := aFactory;
end;

function TEnumerableFactory<T>.DoGetEnumerator: IEnumerator<T>;
begin
  if Assigned(FFactory) then
    Result := FFactory;
  if Result = nil then
    Result := TEmptyEnumerator<T>.Create;
end;

{ TPipelineEnumerator<T> }

constructor TPipelineEnumerator<T>.Create(aSourceEnumerator: IEnumerator<T>);
begin
  inherited Create;
  FSourceEnumerator := aSourceEnumerator;
end;

function TPipelineEnumerator<T>.DoGetCurrent: T;
begin
  if FSourceEnumerator <> nil then
    Result := FSourceEnumerator.Current
  else
    Result := Default(T);
end;

function TPipelineEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := (FSourceEnumerator <> nil) and FSourceEnumerator.MoveNext;
end;

procedure TPipelineEnumerator<T>.DoReset;
begin
  if FSourceEnumerator <> nil then
    FSourceEnumerator.Reset;
end;

{ TFilterEnumerator<T> }

constructor TFilterEnumerator<T>.Create(aSourceEnumerator: IEnumerator<T>; aPredicate: TFunc<T, Boolean>);
begin
  inherited Create(aSourceEnumerator);
  FPredicate := aPredicate;
end;

function TFilterEnumerator<T>.DoGetCurrent: T;
begin
  if FSourceEnumerator <> nil then
    Result := FSourceEnumerator.Current
  else
    Result := Default(T);
end;

function TFilterEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := False;
  if (FSourceEnumerator = nil) or not FSourceEnumerator.MoveNext then
    Exit;

  while not FPredicate(FSourceEnumerator.Current) do
    if not FSourceEnumerator.MoveNext then
      Exit;

  Result := True;
end;

procedure TFilterEnumerator<T>.DoReset;
begin
  inherited;
  if FSourceEnumerator <> nil then
    FSourceEnumerator.Reset;
end;

{ TPipelineMappingEnumerator<T, R> }

constructor TPipelineMappingEnumerator<T, R>.Create(aSource: IEnumerator<T>; aMapper: TMapper<T, R>);
begin
  inherited Create;
  FSourceEnumerator := aSource;
  FMapper := aMapper;
end;

function TPipelineMappingEnumerator<T, R>.DoGetCurrent: R;
begin
  Result := FCurrent;
end;

function TPipelineMappingEnumerator<T, R>.DoMoveNext: Boolean;
begin
  Result := (FSourceEnumerator <> nil) and FSourceEnumerator.MoveNext;
  if Result then
    FCurrent := FMapper(FSourceEnumerator.Current)
  else
    FCurrent := Default(R);
end;

procedure TPipelineMappingEnumerator<T, R>.DoReset;
begin
  inherited;
  if FSourceEnumerator <> nil then
    FSourceEnumerator.Reset;
end;

constructor TPipelineMappingEnumerable<T, R>.Create(aSourceEnumerable: IEnumerable<T>;
  aFactory: TFunc<IEnumerator<T>, IEnumerator<R>>);
begin
  inherited Create;
  FSourceEnumerable := aSourceEnumerable;
  FFactory := aFactory;
end;

function TPipelineMappingEnumerable<T, R>.DoGetEnumerator: IEnumerator<R>;
begin
  Result := nil;
  if Assigned(FFactory) then
  begin
    if (FSourceEnumerable <> nil) then
      Result := FFactory(FSourceEnumerable.GetEnumerator)
    else
      Result := FFactory(nil);
  end;

  if Result = nil then
    Result := TEmptyEnumerator<R>.Create;
end;

end.
