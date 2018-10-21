unit collections.maps;

interface

uses
  generics.collections,
  generics.defaults,
  collections.rbtree;

type
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


end.