unit collections.deque;

interface

uses
  collections.common, collections.enumerators;

type
  TOnDisposeValue<T> = reference to procedure (var Value: T);
  TDeque<T> = class(TObject)
  private type
    TNode = class
    public
      Value: T;
      Next: TNode;
      Prev: TNode;

      constructor Create(aPrev: TNode; const aValue: T; aNext: TNode);
      function AcquireValue: T;
    end;

    TDequeEnumerator = class(TEnumeratorImpl<T>)
    private
      FDeque: TDeque<T>;
      FCurrent: TDeque<T>.TNode;
      FFirstTime: Boolean;
    protected
      function DoGetCurrent: T; override;
      function DoMoveNext: Boolean; override;
      procedure DoReset; override;
    public
      constructor Create(aDeque: TDeque<T>);
    end;

    TDequeEnumerable = class(TEnumerableImpl<T>)
    private
      FDeque: TDeque<T>;
    protected
      function DoGetEnumerator: IEnumerator<T>; override;
    public
      constructor Create(aDeque: TDeque<T>);
    end;
  private
    FCount: Integer;
    FFirst: TNode;
    FLast: TNode;
    FOnDisposeValue: TOnDisposeValue<T>;
    FEnumerable: IEnumerable<T>;

    function GetFirst: T;
    function GetLast: T;

    function DoExtractFirst(aFirst: TNode): TNode;
    function DoExtractLast(aLast: TNode): TNode;
    procedure FreeNode(var Node: TNode);
  public
    constructor Create; overload;
    constructor Create(aOnDisposeValue: TOnDisposeValue<T>); overload;

    destructor Destroy; override;

    procedure AddRangeFirst(const aValues: array of T);
    procedure AddRangeLast(const aValues: array of T);

    procedure AddFirst(const aValue: T);
    procedure AddLast(const aValue: T);

    procedure RemoveFirst;
    procedure RemoveLast;

    function ExtractFirst: T;
    function ExtractLast: T;

    procedure Clear;
    function Contains(const aValue: T): Boolean;

    // Return enumerable from first to last element order.
    function GetEnumerator: IEnumerator<T>;
    function AsEnumerable: IEnumerable<T>;

    property First: T read GetFirst;
    property Last: T read GetLast;
    property Count: Integer read FCount;
  end;

implementation

uses
  generics.defaults, sysutils;


{ TDeque<T> }

procedure TDeque<T>.Clear;
begin
  while Count > 0 do
    RemoveLast;
end;

function TDeque<T>.Contains(const aValue: T): Boolean;
var
  Comparer: IEqualityComparer<T>;
  Node: TNode;
begin
  Node := FFirst;
  Comparer := TEqualityComparer<T>.Default;
  while (Node <> nil) do
  begin
    if Comparer.Equals(aValue, Node.Value) then
      Exit(True);
    Node := Node.Next;
  end;
  Result := False;
end;

constructor TDeque<T>.Create;
begin
  Create(nil);
end;

constructor TDeque<T>.Create(aOnDisposeValue: TOnDisposeValue<T>);
begin
  inherited Create;
  FOnDisposeValue := aOnDisposeValue;
end;

destructor TDeque<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDeque<T>.FreeNode(var Node: TNode);
var
  wNode, NextNextNode: TNode;
begin
  if Node = nil then
    Exit;

  if Assigned(FOnDisposeValue) then
    FOnDisposeValue(Node.Value);

  FreeAndNil(Node);
end;

function TDeque<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := AsEnumerable.GetEnumerator;
end;

function TDeque<T>.AsEnumerable: IEnumerable<T>;
begin
  if FEnumerable = nil then
    FEnumerable := TDequeEnumerable.Create(Self);
  Result := FEnumerable;
end;

function TDeque<T>.GetFirst: T;
begin
  if FFirst <> nil then
    Result := FFirst.Value
  else
    Result := Default(T);
end;

function TDeque<T>.GetLast: T;
begin
  if FLast <> nil then
    Result := FLast.Value
  else
    Result := Default(T);
end;

procedure TDeque<T>.RemoveFirst;
var
  Node: TNode;
begin
  if FFirst = nil then
    Exit;

  Node := DoExtractFirst(FFirst);
  FreeNode(Node);
end;

procedure TDeque<T>.RemoveLast;
var
  Node: TNode;
begin
  if FLast = nil then
    Exit;

  Node := DoExtractLast(FLast);
  FreeNode(Node);
end;

function TDeque<T>.ExtractFirst: T;
var
  Node: TNode;
begin
  if FFirst = nil then
    Exit(Default(T));

  Node := DoExtractFirst(FFirst);
  Result := Node.AcquireValue;
  FreeNode(Node);
end;

function TDeque<T>.ExtractLast: T;
var
  Node: TNode;
begin
  if FLast = nil then
    Exit(Default(T));

  Node := DoExtractLast(FLast);
  Result := Node.AcquireValue;
  FreeNode(Node);
end;

function TDeque<T>.DoExtractFirst(aFirst: TNode): TNode;
var
  Next: TNode;
begin
  Next := aFirst.Next;
  aFirst.Next := nil;
  FFirst := Next;
  if (Next = nil) then
    FLast := nil
  else
    Next.Prev := nil;
  Dec(FCount);
  Result := aFirst;
end;

function TDeque<T>.DoExtractLast(aLast: TNode): TNode;
var
  Prev: TNode;
begin
  Prev := aLast.Prev;
  aLast.Prev := nil;
  FLast := Prev;
  if (Prev = nil) then
    FFirst := nil
  else
    Prev.Next := nil;
  Dec(FCount);
  Result := aLast;
end;

procedure TDeque<T>.AddLast(const aValue: T);
var
  wLast, NewNode: TNode;
begin
  wLast := FLast;
  NewNode := TNode.Create(wLast, aValue, nil);
  FLast := NewNode;
  if (wLast = nil) then
    FFirst := NewNode
  else
    wLast.Next := NewNode;
  Inc(FCount);
end;

procedure TDeque<T>.AddRangeFirst(const aValues: array of T);
var
  V: T;
begin
  for V in aValues do
    AddFirst(V);
end;

procedure TDeque<T>.AddRangeLast(const aValues: array of T);
var
  V: T;
begin
  for V in aValues do
    AddLast(V);
end;

procedure TDeque<T>.AddFirst(const aValue: T);
var
  wFirst, NewNode: TNode;
begin
  wFirst := FFirst;
  NewNode := TNode.Create(nil, aValue, wFirst);
  FFirst := NewNode;
  if (wFirst = nil) then
    FLast := NewNode
  else
    wFirst.Prev := NewNode;
  Inc(FCount);
end;

{ TDeque<T>.TNode }

constructor TDeque<T>.TNode.Create(aPrev: TNode; const aValue: T; aNext: TNode);
begin
  inherited Create;
  Value := aValue;
  Prev := aPrev;
  Next := aNext;
end;

function TDeque<T>.TNode.AcquireValue: T;
begin
  Result := Value;
  Value := Default(T);
end;

{ TDequeEnumerable }

constructor TDeque<T>.TDequeEnumerable.Create(aDeque: TDeque<T>);
begin
  inherited Create;
  FDeque := aDeque;
end;

function TDeque<T>.TDequeEnumerable.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TDequeEnumerator.Create(FDeque);
end;

{ TDequeEnumerator<T> }

constructor TDeque<T>.TDequeEnumerator.Create(aDeque: TDeque<T>);
begin
  inherited Create;
  FDeque := aDeque;
  Reset;
end;

function TDeque<T>.TDequeEnumerator.DoGetCurrent: T;
begin
  if FCurrent <> nil then
    Result := FCurrent.Value
  else
    Result := Default(T);
end;

function TDeque<T>.TDequeEnumerator.DoMoveNext: Boolean;
begin
  Result := False;
  if FDeque = nil then
    Exit;

  if FFirstTime then
  begin
    FCurrent := FDeque.FFirst;
    FFirstTime := False;
  end
  else if FCurrent <> nil then
    FCurrent := FCurrent.Next;
  Result := FCurrent <> nil;
end;

procedure TDeque<T>.TDequeEnumerator.DoReset;
begin
  inherited;
  FCurrent := nil;
  FFirstTime := True;
end;

end.