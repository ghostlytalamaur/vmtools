unit collections.maps;

interface

uses
  generics.collections,
  generics.defaults,
  collections.rbtree,
  collections.common,
  vmsys;

type
  IMap<K, V> = interface(IEnumerable<TPair<K, V>>)
    function GetCount: Integer;
    function GetItems(aKey: K): V;
    procedure SetItems(aKey: K; aValue: V);

    function GetKeys: IEnumerable<K>;
    function GetValues: IEnumerable<V>;

    function ContainsKey(const aKey: K): Boolean;
    procedure Add(const aKey: K; const aValue: V);
    procedure Remove(const aKey: K);
    procedure Clear;
    function TryGetValue(const aKey: K; out Value: V): Boolean;

    property Keys: IEnumerable<K> read GetKeys;
    property Values: IEnumerable<V> read GetValues;
    property Items[aKey: K]: V read GetItems write SetItems; default;
    property Count: Integer read GetCount;
  end;

  THashMap<K, V> = class(TEnumerableImpl<TPair<K, V>>, IMap<K, V>)
  private
    FDict: IObjectHolder<TDictionary<K,V>>;
    FKeysEnumerable: IEnumerable<K>;
    FValuesEnumerable: IEnumerable<V>;

    function GetCount: Integer;
    function GetItems(aKey: K): V;
    procedure SetItems(aKey: K; aValue: V);

    function GetKeys: IEnumerable<K>;
    function GetValues: IEnumerable<V>;
  protected
    function DoGetEnumerator: IEnumerator<TPair<K, V>>; override;
  public
    constructor Create;
    destructor Destroy; override;
    function ContainsKey(const aKey: K): Boolean;
    procedure Add(const aKey: K; const aValue: V);
    procedure Remove(const aKey: K);
    procedure Clear;
    function TryGetValue(const aKey: K; out Value: V): Boolean;

    property Keys: IEnumerable<K> read GetKeys;
    property Values: IEnumerable<V> read GetValues;
    property Items[aKey: K]: V read GetItems write SetItems; default;
  end;

  TMap<TKey, TValue> = class(TLLRBTree<TKey, TPair<TKey, TValue>>)
  public
    type
      PValue = ^TValue;
      PPair = ^TPair;
      TPair = TPair<TKey, TValue>;
  private
    function GetValue(aKey: TKey): TValue;
  protected
    procedure DisposeT(var aValue: TPair); override;
    procedure DisposeValue(var aValue: TValue); virtual;
  public
    constructor Create(aComparer: IComparer<TKey>); overload;
    constructor Create; overload;
    function Locate(aKey: TKey; aCreateIfNotExists: Boolean): PValue;

    procedure Insert(aKey: TKey; aValue: TValue);

    property Items[aKey: TKey]: TValue read GetValue; default;
  end;

  TObjectMap<TKey; TValue: class> = class(TMap<TKey, TValue>)
  private
    FOwnObjects: Boolean;
  protected
    procedure DisposeValue(var aValue: TValue); override;
  public
    constructor Create(aOwnObjects: Boolean = True);
  end;

implementation

uses
  sysutils;

constructor TObjectMap<TKey, TValue>.Create(aOwnObjects: Boolean = True);
begin
  inherited Create(nil);
  FOwnObjects := aOwnObjects;
end;

procedure TObjectMap<TKey, TValue>.DisposeValue(var aValue: TValue);
begin
  if FOwnObjects then
    FreeAndNil(aValue);
end;

{ TMap<TKey, TValue> }

constructor TMap<TKey, TValue>.Create(aComparer: IComparer<TKey>);
begin
  inherited Create(aComparer);
end;

constructor TMap<TKey, TValue>.Create;
begin
  Create(nil);
end;

procedure TMap<TKey, TValue>.DisposeT(var aValue: TPair);
begin
  DisposeValue(aValue.Value);
end;

procedure TMap<TKey, TValue>.DisposeValue(var aValue: TValue);
begin

end;

function TMap<TKey, TValue>.GetValue(aKey: TKey): TValue;
var
  V: PValue;
begin
  V := Locate(aKey, False);
  if V <> nil then
    Result := V^
  else
    Result := Default(TValue);
end;

procedure TMap<TKey, TValue>.Insert(aKey: TKey; aValue: TValue);
var
  V: PValue;
begin
  V := Locate(aKey, True);
  if V <> nil then
    V^ := aValue;
end;

function TMap<TKey, TValue>.Locate(aKey: TKey; aCreateIfNotExists: Boolean): PValue;
var
  P: TPair;
  wP: PPair;
begin
  wP := Pointer(LocateKey(aKey, aCreateIfNotExists, nil));
  if wP <> nil then
    Result := @(wP.Value)
  else
    Result := nil;
end;


{ THashMap<K, V> }

procedure THashMap<K, V>.Add(const aKey: K; const aValue: V);
begin
  FDict.Obj.AddOrSetValue(aKey, aValue);
end;

procedure THashMap<K, V>.Clear;
begin
  FDict.Obj.Clear;
end;

function THashMap<K, V>.ContainsKey(const aKey: K): Boolean;
begin
  Result := FDict.Obj.ContainsKey(aKey);
end;

constructor THashMap<K, V>.Create;
begin
  inherited Create;
  FDict := TObjectHolder<TDictionary<K,V>>.Create(TDictionary<K,V>.Create, True);
end;

destructor THashMap<K, V>.Destroy;
begin
  inherited;
end;

function THashMap<K, V>.DoGetEnumerator: IEnumerator<TPair<K, V>>;
begin
  Result := TWrapEnumerator<TPair<K, V>>.Create(FDict.Obj.GetEnumerator, True);
end;

function THashMap<K, V>.GetCount: Integer;
begin
  Result := FDict.Obj.Count;
end;

function THashMap<K, V>.TryGetValue(const aKey: K; out Value: V): Boolean;
begin
  Result := FDict.Obj.TryGetValue(aKey, Value);
end;

function THashMap<K, V>.GetItems(aKey: K): V;
begin
  if not FDict.Obj.TryGetValue(aKey, Result) then
    Result := Default(V);
end;

function THashMap<K, V>.GetKeys: IEnumerable<K>;
begin
  if FKeysEnumerable = nil then
    FKeysEnumerable := TCollectionsUtils.Wrap<K>(TMappingObjectHolder<TDictionary<K, V>, TEnumerable<K>>.Create(FDict,
      function (aDict: TDictionary<K, V>): TEnumerable<K>
      begin
        Result := aDict.Keys;
      end));
  Result := FKeysEnumerable;
end;


function THashMap<K, V>.GetValues: IEnumerable<V>;
begin
  if FValuesEnumerable = nil then
    FValuesEnumerable := TCollectionsUtils.Wrap<V>(
      TMappingObjectHolder<TDictionary<K, V>, TEnumerable<V>>.Create(FDict,
      function (aDict: TDictionary<K, V>): TEnumerable<V>
      begin
        Result := aDict.Values;
      end));
  Result := FValuesEnumerable;
end;

procedure THashMap<K, V>.Remove(const aKey: K);
begin
  FDict.Obj.Remove(aKey);
end;

procedure THashMap<K, V>.SetItems(aKey: K; aValue: V);
begin
  FDict.Obj.AddOrSetValue(aKey, aValue);
end;

end.
