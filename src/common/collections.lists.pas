unit collections.lists;

interface

uses
  classes,
  generics.defaults,
  generics.collections,
  sysutils,
  collections.enumerators, vmsys;

type
  TCustomList<T> = class(TEnumerable<T>)
  private
    type
      TCustomListEnumerator = class(TEnumerator<T>)
      private
        FList: TCustomList<T>;
        FCurrent: Integer;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(aList: TCustomList<T>);
        destructor Destroy; override;
      end;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  private
    FComparer: IComparer<T>;
    FItems: array of T;
    FCount: Integer;
    FOnNotify: TCollectionNotifyEvent<T>;

    function GetItems(Idx: Integer): T;
    function GetFirst: T;
    function GetLast: T;
    procedure GrowSize(aNewSize: Integer);

    function DoExtract(Idx: Integer; out Item: T): Boolean;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); virtual;
    function Search(Item: T; out Idx: Integer): Boolean; virtual;
    function Compare(Left, Right: T): Integer;

    procedure FreeItem(var Item: T); virtual;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    destructor Destroy; override;

    procedure Add(Item: T);
    procedure Insert(Idx: Integer; Item: T);
    procedure Remove(Idx: Integer); overload;
    procedure Remove(Item: T); overload;
    function Extract(Idx: Integer): T;
    function IndexOf(Item: T): Integer;
    function Has(Item: T): Boolean;
    function ValidIndex(Idx: Integer): Boolean;
    function IsEmpty: Boolean; overload;
    class function IsEmpty(aList: TCustomList<T>): Boolean; overload;

    procedure Clear;
    procedure Exchange(FromIdx, ToIdx: Integer);
    procedure Sort;
    function FirstThat(aFunc: TFunc<T, Boolean>): T;

    property Count: Integer read FCount;
    property First: T read GetFirst;
    property Last: T read GetLast;
    property Items[Idx: Integer]: T read GetItems; default;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
  end;

  TBaseList<T> = class(TCustomList<T>);

  TObjList<T: class> = class(TBaseList<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure FreeItem(var Item: T); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  IList<T> = interface(IEnumerable<T>)
    procedure SetItem(Index: Integer; const Value: T);
    function  GetItem(Index: Integer): T;
    function  GetCount: Integer;

    function  IndexOf(const Value: T): Integer;
    procedure Delete(Index: Integer);
    function  Add(const Value: T): Integer;
    procedure Insert(Index: Integer; const Value: T);
    function  Extract(const Value: T): T;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TListImpl<T> = class(TEnumerableImpl<T>, IList<T>)
  private
    FList: IObjectHolder<TList<T>>;

    procedure SetItem(Index: Integer; const Value: T);
    function  GetItem(Index: Integer): T;
    function  GetCount: Integer;

  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create;
    function  IndexOf(const Value: T): Integer;
    procedure Delete(Index: Integer);
    function  Add(const Value: T): Integer;
    procedure Insert(Index: Integer; const Value: T);
    function  Extract(const Value: T): T;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

implementation


{ TObjList<T> }

constructor TObjList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjList<T>.FreeItem(var Item: T);
begin
  if FOwnsObjects then
    FreeAndNil(Item)
  else
    inherited;
end;

{ TCustomList<T> }

constructor TCustomList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  SetLength(FItems, 2);
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
end;

constructor TCustomList<T>.Create;
begin
  Create(nil);
end;

destructor TCustomList<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCustomList<T>.Add(Item: T);
begin
  Insert(FCount, Item);
end;

function TCustomList<T>.IsEmpty: Boolean;
begin
  Result := IsEmpty(Self);
end;

class function TCustomList<T>.IsEmpty(aList: TCustomList<T>): Boolean;
begin
  Result := (aList = nil) or (aList.Count <= 0);
end;

function TCustomList<T>.DoExtract(Idx: Integer; out Item: T): Boolean;
begin
  Item := Default(T);
  Result := ValidIndex(Idx);
  if not Result then
    Exit;

  Item := FItems[Idx];
  FItems[Idx] := Default(T);
  Dec(FCount);

  if Idx <> FCount then
    Move(FItems[Idx + 1], FItems[Idx], (FCount - Idx) * SizeOf(T));
//  if Length(FItems) > FCount then
//    FillChar(FItems[FCount], (Length(FItems) - FCount - 1) * SizeOf(T), 0);

  if Length(FItems) = (4 * FCount) then
    GrowSize(2 * FCount);
end;

procedure TCustomList<T>.Insert(Idx: Integer; Item: T);
begin
  if Length(FItems) = FCount then
    GrowSize(FCount * 2);

  if Idx <> FCount then
    Move(FItems[Idx], FItems[Idx + 1], (FCount - Idx) * SizeOf(T));

  FItems[Idx] := Item;

  Inc(FCount);

  Notify(Item, cnAdded);
end;

function TCustomList<T>.Extract(Idx: Integer): T;
begin
  if not DoExtract(Idx, Result) then
    Exit;

  Notify(Result, cnExtracted);
end;

procedure TCustomList<T>.Remove(Idx: Integer);
var
  Item: T;
begin
  if not DoExtract(Idx, Item) then
    Exit;

  Notify(Item, cnRemoved);
  FreeItem(Item);
end;

procedure TCustomList<T>.Remove(Item: T);
begin
  Remove(IndexOf(Item));
end;

function TCustomList<T>.IndexOf(Item: T): Integer;
begin
  Search(Item, Result);
end;

function TCustomList<T>.Has(Item: T): Boolean;
begin
  Result := ValidIndex(IndexOf(Item));
end;

function TCustomList<T>.ValidIndex(Idx: Integer): Boolean;
begin
  Result := (Idx >= 0) and (Idx < Count);
end;

procedure TCustomList<T>.FreeItem(var Item: T);
begin
  Item := Default(T);
end;

procedure TCustomList<T>.GrowSize(aNewSize: Integer);
begin
  SetLength(FItems, aNewSize);
end;

procedure TCustomList<T>.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Notify(FItems[I], cnRemoved);
    FreeItem(FItems[I]);
    Dec(FCount);
  end;
  GrowSize(2);
end;

procedure TCustomList<T>.Exchange(FromIdx, ToIdx: Integer);
var
  Buf: T;
begin
  Buf := FItems[FromIdx];
  FItems[FromIdx] := FItems[ToIdx];
  FItems[ToIdx] := Buf;
end;

function TCustomList<T>.GetFirst: T;
begin
  Result := Self[0];
end;

function TCustomList<T>.GetItems(Idx: Integer): T;
begin
  if ValidIndex(Idx) then
    Result := FItems[Idx]
  else
    Result := Default(T);
end;

function TCustomList<T>.GetLast: T;
begin
  Result := Self[Count - 1];
end;

procedure TCustomList<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Value, Action);
end;

function TCustomList<T>.Search(Item: T; out Idx: Integer): Boolean;
var
  I: Integer;
begin
  Idx := -1;
  for I := 0 to Count - 1 do
    if Compare(Self[I], Item) = 0 then
    begin
      Idx := I;
      break;
    end;
  Result := Idx <> -1;
end;

function TCustomList<T>.Compare(Left, Right: T): Integer;
begin
  Result := FComparer.Compare(Left, Right);
end;

procedure TCustomList<T>.Sort;
begin
  TArray.Sort<T>(FItems, FComparer, 0, FCount);
end;

function TCustomList<T>.FirstThat(aFunc: TFunc<T, Boolean>): T;
var
  I: Integer;
  Item: T;
begin
  Result := Default(T);
  for I := 0 to Count - 1 do
  begin
    Item := Self[I];
    if aFunc(Item) then
      Exit(Item);
  end;
end;

function TCustomList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := TCustomListEnumerator.Create(Self);
end;

{ TCustomList<T>.TCustomListEnumerator<T> }

constructor TCustomList<T>.TCustomListEnumerator.Create(aList: TCustomList<T>);
begin
  inherited Create;
  FList := aList;
  FCurrent := -1;
end;

destructor TCustomList<T>.TCustomListEnumerator.Destroy;
begin

  inherited;
end;

function TCustomList<T>.TCustomListEnumerator.DoGetCurrent: T;
begin
  Result := FList[FCurrent];
end;

function TCustomList<T>.TCustomListEnumerator.DoMoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FList.Count;
end;


{ TListImpl<T> }

procedure TListImpl<T>.SetItem(Index: Integer; const Value: T);
begin
  FList.Obj[Index] := Value;
end;

function TListImpl<T>.GetItem(Index: Integer): T;
begin
  Result := FList.Obj[Index];
end;

function TListImpl<T>.GetCount: Integer;
begin
  Result := FList.Obj.Count;
end;

function TListImpl<T>.Add(const Value: T): Integer;
begin
  FList.Obj.Add(Value);
end;

procedure TListImpl<T>.Insert(Index: Integer; const Value: T);
begin
  FList.Obj.Insert(Index, Value);
end;

function TListImpl<T>.Extract(const Value: T): T;
begin
  Result := FList.Obj.Extract(Value);
end;

constructor TListImpl<T>.Create;
begin
  inherited Create;
  FList := TObjectHolder<TList<T>>.Create(TList<T>.Create, True);
end;

function TListImpl<T>.IndexOf(const Value: T): Integer;
begin
  Result := FList.Obj.IndexOf(Value);
end;

procedure TListImpl<T>.Delete(Index: Integer);
begin
  FList.Obj.Delete(Index);
end;

function TListImpl<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TWrapEnumerator<T>.Create(IObjectHolder<TEnumerable<T>>(Pointer(@FList)^))
end;

end.
