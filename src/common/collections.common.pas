unit collections.common;

interface

uses
  vmsys, generics.collections, sysutils, Classes;

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

  TEmptyEnumerator<T> = class(TEnumeratorImpl<T>)
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  end;

  TMappingEnumerator<T, R> = class(TEnumeratorImpl<R>)
  private
    FSource: IEnumerable<T>;
    FMapper: TFunc<T, R>;

    FEnumerator: IEnumerator<T>;
    FCurrent: R;
  protected
    function DoGetCurrent: R; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aSource: IEnumerable<T>; aMapper: TFunc<T, R>);
  end;

  TIndexedEnumerator<T> = class(TEnumeratorImpl<T>)
  public
    FFromIndex: Integer;
    FToIndex: Integer;
    FCurrentIndex: Integer;
    FCurrent: T;
    FOnGetItem: TFunc<Integer, T>;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
  public
    constructor Create(aFromIndex, aToIndex: Integer; aOnGetItem: TFunc<Integer, T>);
  end;

  TEnumerableFactory<T> = class(TEnumerableImpl<T>)
  private
    FFactory: TFunc<IEnumerator<T>>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(aFactory: TFunc<IEnumerator<T>>);
  end;

  TCollectionsUtils = record
  private
    class function WrapGetMethod(aList: TStringList): TFunc<Integer, string>; static;
  public
    class function FirstThat<T>(aEnumerable: TEnumerable<T>; aPredicate: TPredicate<T>): T; static;
    class function Map<T, R>(aEnumerable: IEnumerable<T>; aMapper: TFunc<T, R>): IEnumerable<R>; static;
    class function Wrap<T>(aEnumerable: TEnumerable<T>): IEnumerable<T>; overload; static;
    class function Wrap<T>(aEnumerable: IObjectHolder<TEnumerable<T>>): IEnumerable<T>; overload; static;
    class function Wrap(aList: TStringList): IEnumerable<string>; overload; static;
    class function Empty<T>: IEnumerable<T>; static;
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

class function TCollectionsUtils.Empty<T>: IEnumerable<T>;
begin
  Result := TEnumerableFactory<T>.Create(function : IEnumerator<T>
  begin
    Result := TEmptyEnumerator<T>.Create;
  end);
end;

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

class function TCollectionsUtils.Map<T, R>(aEnumerable: IEnumerable<T>; aMapper: TFunc<T, R>): IEnumerable<R>;
begin
  if (aEnumerable = nil) or not Assigned(aMapper) then
  begin
    Result := Empty<R>;
    Exit;
  end;

  Result := TEnumerableFactory<R>.Create(function : IEnumerator<R>
  begin
    Result := TMappingEnumerator<T,R>.Create(aEnumerable, aMapper);
  end);
end;

class function TCollectionsUtils.Wrap<T>(aEnumerable: TEnumerable<T>): IEnumerable<T>;
begin
  Result := Wrap<T>(TObjectHolder<TEnumerable<T>>.Create(aEnumerable, False));
end;

class function TCollectionsUtils.Wrap<T>(aEnumerable: IObjectHolder<TEnumerable<T>>): IEnumerable<T>;
begin
  Result := TEnumerableFactory<T>.Create(function : IEnumerator<T>
  begin
    Result := TWrapEnumerator<T>.Create(aEnumerable);
  end);
end;

class function TCollectionsUtils.WrapGetMethod(aList: TStringList): TFunc<Integer, string>;
begin
  Result := function (aIndex: Integer): string
  begin
    if aList <> nil then
      Result := aList[aIndex]
    else
      Result := '';
  end;
end;

class function TCollectionsUtils.Wrap(aList: TStringList): IEnumerable<string>;
begin
  if aList <> nil then
    Result := TEnumerableFactory<string>.Create(function : IEnumerator<string>
    begin
      Result := TIndexedEnumerator<string>.Create(0, aList.Count - 1, WrapGetMethod(aList));
    end)
  else
    Result := Empty<string>;
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

constructor TMappingEnumerator<T, R>.Create(aSource: IEnumerable<T>; aMapper: TFunc<T, R>);
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

{ TIndexedEnumerator<T> }

constructor TIndexedEnumerator<T>.Create(aFromIndex, aToIndex: Integer; aOnGetItem: TFunc<Integer, T>);
begin
  inherited Create;
  FFromIndex := aFromIndex;
  FToIndex := aToIndex;
  FOnGetItem := aOnGetItem;
  Reset;
end;

function TIndexedEnumerator<T>.DoGetCurrent: T;
begin
  Result := FCurrent;
end;

function TIndexedEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := (FCurrentIndex + 1 >= FFromIndex) and (FCurrentIndex + 1 <= FToIndex) and Assigned(FOnGetItem);
  if Result then
  begin
    Inc(FCurrentIndex);
    FCurrent := FOnGetItem(FCurrentIndex);
  end;
end;

procedure TIndexedEnumerator<T>.DoReset;
begin
  inherited;
  FCurrentIndex := FFromIndex - 1;
end;

end.