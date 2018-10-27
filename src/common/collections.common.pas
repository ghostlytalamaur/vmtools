unit collections.common;

interface

uses
  generics.collections, sysutils;

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

  TBaseEnumerableImpl = class(TInterfacedObject, IEnumerable)
  protected
    function GetPointerEnumerator: IEnumerator; virtual;
  public
    { IEnumerable }
    function IEnumerable.GetEnumerator = GetPointerEnumerator;
  end;

  TEnumeratorImpl<T> = class(TGenericEnumerator, IEnumerator<T>)
  protected
    function DoGetCurrent: T; virtual;
  public
    { IEnumerator<T> }
    function IEnumerator<T>.GetCurrent = DoGetCurrent;
  end;

  TEnumerableImpl<T> = class(TBaseEnumerableImpl, IEnumerable<T>)
  protected
    function DoGetEnumerator: IEnumerator<T>; virtual; abstract;
  public
    { IEnumerable<T> }
    function IEnumerable<T>.GetEnumerator = DoGetEnumerator;
  end;

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

  TKeysEnumerable<K, V> = class(TBaseKeyValueEnumerable<K, V, K>)
  private type
    TKeysEnumerator = class(TBaseKeyValueEnumerable<K, V, K>.TBaseKeyValueEnumerator)
    protected
      function DoGetCurrent: K; override;
    end;
  protected
    function DoGetEnumerator: IEnumerator<K>; override;
  end;

  TValuesEnumerable<K, V> = class(TBaseKeyValueEnumerable<K, V, V>)
  private type
    TValuesEnumerator = class(TBaseKeyValueEnumerable<K, V, V>.TBaseKeyValueEnumerator)
    protected
      function DoGetCurrent: V; override;
    end;
  protected
    function DoGetEnumerator: IEnumerator<V>; override;
  end;

  TEnumeratorWrapper<T> = class(TEnumeratorImpl<T>)
  private
    FEnumerable: TEnumerable<T>;
    FEnumerator: TEnumerator<T>;
    FOwnEnumerator: Boolean;
  protected
    function DoMoveNext: Boolean; override;
    function DoGetCurrent: T; override;
    procedure DoReset; override;
  public
    constructor Create(aEnumerable: TEnumerable<T>); overload;
    constructor Create(aEnumerator: TEnumerator<T>; aOwnEnumerator: Boolean); overload;
    destructor Destroy; override;
  end;

  TEnumerableWrapper<T> = class(TEnumerableImpl<T>)
  private
    FEnumerable: TEnumerable<T>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(aEnumerable: TEnumerable<T>);
  end;

  TCollectionsUtils = record
  public
    class function FirstThat<T>(aEnumerable: TEnumerable<T>; aPredicate: TPredicate<T>): T; static;
  end;

implementation

{ TGenericEnumerable }

function TBaseEnumerableImpl.GetPointerEnumerator: IEnumerator;
begin
  Result := nil;
end;

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

{ TEnumeratorImpl<T> }

function TEnumeratorImpl<T>.DoGetCurrent: T;
begin
  Result := Default(T);
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


{ TCollectionsUtils }

class function TCollectionsUtils.FirstThat<T>(aEnumerable: TEnumerable<T>; aPredicate: TPredicate<T>): T;
var
  Item: T;
begin
  if (aEnumerable <> nil) and Assigned(aPredicate) then
    for Item in aEnumerable do
      if aPredicate(Item) then
        Exit(Item);
  Result := Default(T);
end;

{ TEnumeratorWrapper<T> }

constructor TEnumeratorWrapper<T>.Create(aEnumerable: TEnumerable<T>);
begin
  inherited Create;
  FEnumerable := aEnumerable;
  Reset;
end;

constructor TEnumeratorWrapper<T>.Create(aEnumerator: TEnumerator<T>; aOwnEnumerator: Boolean);
begin
  inherited Create;
  FEnumerator := aEnumerator;
  FOwnEnumerator := aOwnEnumerator;
end;

destructor TEnumeratorWrapper<T>.Destroy;
begin
  if FOwnEnumerator then
    FreeAndNil(FEnumerator);
  inherited;
end;

function TEnumeratorWrapper<T>.DoGetCurrent: T;
begin
  if FEnumerator <> nil then
    Result := FEnumerator.Current
  else
    Result := Default(T);
end;

function TEnumeratorWrapper<T>.DoMoveNext: Boolean;
begin
  Result := (FEnumerator <> nil) and FEnumerator.MoveNext;
end;

procedure TEnumeratorWrapper<T>.DoReset;
begin
  inherited;
  if FOwnEnumerator then
    FreeAndNil(FEnumerator);
  FEnumerator := nil;
  if FEnumerable <> nil then
  begin
    FEnumerator := FEnumerable.GetEnumerator;
    FOwnEnumerator := True;
  end;
end;

{ TEnumerableWrapper<T> }

constructor TEnumerableWrapper<T>.Create(aEnumerable: TEnumerable<T>);
begin
  inherited Create;
  FEnumerable := aEnumerable;
end;

function TEnumerableWrapper<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorWrapper<T>.Create(FEnumerable);
end;

end.