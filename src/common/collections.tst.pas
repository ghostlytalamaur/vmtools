unit collections.tst;

interface

uses
  sysutils,
  generics.collections,
  collections.common;

type
  TOnDisposeValue<T> = reference to procedure (var aValue: T);
/// <summary>
/// based on implementation from
/// https://algs4.cs.princeton.edu/52trie/
/// </summary>
  TTST<T> = class(TObject)
  // TODO: Implement longestPrefixOf()
  private type
    PT = ^T;
    TNode = class
    public
      Ch: Char;
      Left: TNode;
      Mid: TNode;
      Right: TNode;
      Value: T;
      HasValue: Boolean;
    end;
  private
    FRoot: TNode;
    FCount: Integer;
    FPairsEnumerable: IEnumerable<TPair<string, T>>;
    FKeysEnumerable: IEnumerable<string>;
    FValuesEnumerable: IEnumerable<T>;
    FOnDisposeValue: TOnDisposeValue<T>;

    function LocateNode(aNode: TNode; const aKey: string; aDepth: Integer; aCreateIfNotExists: Boolean;
        var Node: TNode): TNode;
    function DoDelete(aNode: TNode; const aKey: string; aDepth: Integer): TNode;
    procedure FreeNode(var Node: TNode);
    procedure SetValue(aNode: TNode; const aValue: T);
    procedure FreeValue(aNode: TNode);

    function GetPairs: IEnumerable<TPair<string, T>>;
    function GetKeys: IEnumerable<string>;
    function GetValues: IEnumerable<T>;
  public
    constructor Create; overload;
    constructor Create(aOnDisposeValue: TOnDisposeValue<T>); overload;
    destructor Destroy; override;

    function GetValue(const aKey: string): T;
    procedure PutValue(const aKey: string; const aValue: T);
    procedure Delete(const aKey: string);
    function ContainsKey(aKey: string): Boolean;
    procedure Clear;

    function PairsWithPrefix(const aPrefix: string): IEnumerable<TPair<string, T>>;
    function KeysWithPrefix(const aPrefix: string): IEnumerable<string>;
    function ValuesWithPrefix(const aPrefix: string): IEnumerable<T>;

    property Pairs: IEnumerable<TPair<string, T>> read GetPairs;
    property Keys: IEnumerable<string> read GetKeys;
    property Values: IEnumerable<T> read GetValues;
    property Count: Integer read FCount;
  end;


  TVisitedState = (vs_Own, vs_Left, vs_Mid, vs_Right);
  TVisitedStates = set of TVisitedState;
  TVisitedNode<T> = class
  private
    Node: TTST<T>.TNode;
    States: TVisitedStates;
  end;

  TTSTPairsEnumerator<T> = class(TEnumeratorImpl<TPair<string, T>>)
  private type
    TNode = TTST<T>.TNode;
    TVisitedNode = TVisitedNode<T>;
  private
    FTrie: TTST<T>;
    FPrefix: string;
    FNodesStack: TObjectStack<TVisitedNode>;
    FBuilder: TStringBuilder;
    FCurrent: TPair<string, T>;

    function GetVisitedNode(aNode: TNode): TVisitedNode;
  protected
    function DoMoveNext: Boolean; override;
    procedure DoReset; override;
    function DoGetCurrent: TPair<string, T>; override;
  public
    constructor Create(aTrie: TTST<T>; const aPrefix: string);
    destructor Destroy; override;
  end;

  TTSTPairsEnumerable<T> = class(TEnumerableImpl<TPair<string, T>>)
  private
    FTrie: TTST<T>;
    // If prefix not empty will iterate over all nodes with given prefix.
    FPrefix: string;
  protected
    function DoGetEnumerator: IEnumerator<TPair<string, T>>; override;
  public
    constructor Create(aTrie: TTST<T>; const aPrefix: string);
  end;


implementation

{ TTST<T> }

procedure TTST<T>.Delete(const aKey: string);
begin
  if aKey = '' then
    Exit;

  FRoot := DoDelete(FRoot, aKey, 0);
end;

constructor TTST<T>.Create;
begin
  Create(nil);
end;

constructor TTST<T>.Create(aOnDisposeValue: TOnDisposeValue<T>);
begin
  inherited Create;
  FOnDisposeValue := aOnDisposeValue;
end;

destructor TTST<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TTST<T>.PairsWithPrefix(const aPrefix: string): IEnumerable<TPair<string, T>>;
begin
  Result := TTSTPairsEnumerable<T>.Create(Self, aPrefix);
end;

function TTST<T>.KeysWithPrefix(const aPrefix: string): IEnumerable<string>;
begin
  if (aPrefix <> '') then
    Result := TKeysEnumerable<string, T>.Create(PairsWithPrefix(aPrefix))
  else
    Result := TKeysEnumerable<string, T>.Create(Pairs)
end;

function TTST<T>.ValuesWithPrefix(const aPrefix: string): IEnumerable<T>;
begin
  if (aPrefix <> '') then
    Result := TValuesEnumerable<string, T>.Create(PairsWithPrefix(aPrefix))
  else
    Result := TValuesEnumerable<string, T>.Create(Pairs)
end;

function TTST<T>.GetPairs: IEnumerable<TPair<string, T>>;
begin
  if FPairsEnumerable = nil then
    FPairsEnumerable := PairsWithPrefix('');
  Result := FPairsEnumerable;
end;

function TTST<T>.GetKeys: IEnumerable<string>;
begin
  if FKeysEnumerable = nil then
    FKeysEnumerable := KeysWithPrefix('');
  Result := FKeysEnumerable;
end;

function TTST<T>.GetValues: IEnumerable<T>;
begin
  if FValuesEnumerable = nil then
    FValuesEnumerable := ValuesWithPrefix('');
  Result := FValuesEnumerable;
end;

function TTST<T>.GetValue(const aKey: string): T;
var
  Node: TNode;
begin
  Result := Default(T);
  if (aKey = '') then
    Exit;

  Node := nil;
  LocateNode(FRoot, aKey, 0, False, Node);
  if (Node <> nil) and Node.HasValue then
    Result := Node.Value
  else
    Result := Default(T);
end;

procedure TTST<T>.PutValue(const aKey: string; const aValue: T);
var
  Node: TNode;
begin
  if (aKey = '') then
    Exit;

  Node := nil;
  FRoot := LocateNode(FRoot, aKey, 0, True, Node);
  SetValue(Node, aValue);
end;

function TTST<T>.ContainsKey(aKey: string): Boolean;
var
  Node: TNode;
begin
  Node := nil;
  LocateNode(FRoot, aKey, 0, False, Node);
  Result := (Node <> nil) and Node.HasValue;
end;

procedure TTST<T>.Clear;
begin
  FreeNode(FRoot);
end;

function TTST<T>.LocateNode(aNode: TNode; const aKey: string; aDepth: Integer; aCreateIfNotExists: Boolean;
    var Node: TNode): TNode;
var
  Ch: Char;
begin
  Ch := aKey[aDepth + 1];
  if (aNode = nil) and aCreateIfNotExists then
  begin
    aNode := TNode.Create;
    aNode.Ch := Ch;
  end;

  if aNode <> nil then
  begin
    if (Ch < aNode.Ch) then
      aNode.Left := LocateNode(aNode.Left, aKey, aDepth, aCreateIfNotExists, Node)
    else if (Ch > aNode.Ch) then
      aNode.Right := LocateNode(aNode.Right, aKey, aDepth, aCreateIfNotExists, Node)
    else if ((aDepth + 1) < Length(aKey)) then
      aNode.Mid := LocateNode(aNode.Mid, aKey, aDepth + 1, aCreateIfNotExists, Node)
    else
      Node := aNode;
  end;

  Result := aNode;
end;

function TTST<T>.DoDelete(aNode: TNode; const aKey: string; aDepth: Integer): TNode;
var
  Ch: Char;
begin
  Ch := aKey[aDepth + 1];

  if aNode <> nil then
  begin
    if (Ch < aNode.Ch) then
      aNode.Left := DoDelete(aNode.Left, aKey, aDepth)
    else if (Ch > aNode.Ch) then
      aNode.Right := DoDelete(aNode.Right, aKey, aDepth)
    else if ((aDepth + 1) < Length(aKey)) then
      aNode.Mid := DoDelete(aNode.Mid, aKey, aDepth + 1)
    else
      FreeValue(aNode);
  end;
  if not aNode.HasValue and (aNode.Left = nil) and (aNode.Mid = nil) and (aNode.Right = nil) then
    FreeAndNil(aNode);
  Result := aNode;
end;

procedure TTST<T>.FreeNode(var Node: TNode);
begin
  if Node = nil then
    Exit;

  FreeValue(Node);
  FreeNode(Node.Left);
  FreeNode(Node.Mid);
  FreeNode(Node.Right);

  FreeAndNil(Node);
end;

procedure TTST<T>.SetValue(aNode: TNode; const aValue: T);
begin
  if aNode = nil then
    Exit;

  if aNode.HasValue and Assigned(FOnDisposeValue) then
    FOnDisposeValue(aNode.Value);
  if not aNode.HasValue then
    Inc(FCount);
  aNode.Value := aValue;
  aNode.HasValue := True;
end;

procedure TTST<T>.FreeValue(aNode: TNode);
begin
  if (aNode <> nil) and aNode.HasValue then
  begin
    if Assigned(FOnDisposeValue) then
      FOnDisposeValue(aNode.Value);
    aNode.Value := Default(T);
    aNode.HasValue := False;
    Dec(FCount);
  end;
end;

{ TTST<T>.TNode }

function TTSTPairsEnumerable<T>.DoGetEnumerator: IEnumerator<TPair<string, T>>;
begin
  Result := TTSTPairsEnumerator<T>.Create(FTrie, FPrefix);
end;

constructor TTSTPairsEnumerator<T>.Create(aTrie: TTST<T>; const aPrefix: string);
begin
  inherited Create;
  FTrie := aTrie;
  FPrefix := aPrefix;
  Reset;
end;

destructor TTSTPairsEnumerator<T>.Destroy;
begin
  FreeAndNil(FNodesStack);
  FreeAndNil(FBuilder);
  inherited;
end;

function TTSTPairsEnumerator<T>.GetVisitedNode(aNode: TNode): TVisitedNode;
begin
  Result := TVisitedNode.Create;
  Result.Node := aNode;
  Result.States := []
end;

function TTSTPairsEnumerator<T>.DoMoveNext: Boolean;
var
  CurNode: TVisitedNode;
begin
  Result := False;
  if FNodesStack = nil then
    Exit;

  while FNodesStack.Count > 0 do
  begin
    CurNode := FNodesStack.Peek;
    if CurNode.Node.HasValue and not (vs_Own in CurNode.States) then
    begin
      FCurrent := TPair<string, T>.Create(FBuilder.ToString + CurNode.Node.Ch, CurNode.Node.Value);
      Include(CurNode.States, vs_Own);
      Result := True;
      Exit;
    end;

    if (CurNode.Node.Left <> nil) and not (vs_Left in CurNode.States) then
    begin
      Include(CurNode.States, vs_Left);
      FNodesStack.Push(GetVisitedNode(CurNode.Node.Left))
    end
    else if (CurNode.Node.Mid <> nil) and not (vs_Mid in CurNode.States) then
    begin
      FBuilder.Append(CurNode.Node.Ch);
      Include(CurNode.States, vs_Mid);
      FNodesStack.Push(GetVisitedNode(CurNode.Node.Mid));
    end
    else if (CurNode.Node.Right <> nil) and not (vs_Right in CurNode.States) then
    begin
      if (vs_Mid in CurNode.States) then
        FBuilder.Length := FBuilder.Length - 1;
      FNodesStack.Extract;
      FNodesStack.Push(GetVisitedNode(CurNode.Node.Right));
      FreeAndNil(CurNode);
    end
    else
    begin
      if (vs_Mid in CurNode.States) then
        FBuilder.Length := FBuilder.Length - 1;
      FNodesStack.Pop;
    end;
  end;
end;

procedure TTSTPairsEnumerator<T>.DoReset;
var
  StartNode: TNode;
begin
  FreeAndNil(FBuilder);
  FreeAndNil(FNodesStack);
  if (FTrie = nil) then
    Exit;

  StartNode := nil;
  if (FPrefix <> '') then
    FTrie.LocateNode(FTrie.FRoot, FPrefix, 0, False, StartNode)
  else
    StartNode := FTrie.FRoot;

  if StartNode = nil then
    Exit;

  FBuilder := TStringBuilder.Create(FPrefix);
  FNodesStack := TObjectStack<TVisitedNode>.Create;

  FNodesStack.Push(GetVisitedNode(StartNode));
  if (FPrefix <> '') then
  begin
    FNodesStack.Peek.States := [vs_Left, vs_Right];
    FBuilder.Length := FBuilder.Length - 1;
  end;
end;

function TTSTPairsEnumerator<T>.DoGetCurrent: TPair<string, T>;
begin
  Result := FCurrent;
end;

constructor TTSTPairsEnumerable<T>.Create(aTrie: TTST<T>; const aPrefix: string);
begin
  inherited Create;
  FTrie := aTrie;
  FPrefix := aPrefix;
end;

end.