unit collections.rbtree;

interface

uses
  generics.defaults,
  generics.collections,
  classes;
(*

  To use tree as map generic type T must be record like this.
  Important: Key field must be first
  TPair<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  For Set you may define tree as
    TLLRBTree<Integer, Integer>;

 *)

type
  TLLRBTree<TKey, T> = class(TEnumerable<TKey>)
  private
    type
      PT = ^T;
      PKey = ^TKey;
  private
    const
      RED = true;
      BLACK = false;
    type
      TKeyNode = class(TObject)
      private
        Value: T;
        Color: Boolean;
        Size: Integer;

        Left: TKeyNode;
        Right: TKeyNode;

        constructor Create(aKey: TKey; aSize: Integer);
        function GetKey: TKey;
      end;

      // TODO: rewrite without queue?
      TTreeEnumerator = class(TEnumerator<TKey>)
      private
        FQueue: TQueue<TKey>;
        FQueueEnumerator: TEnumerator<TKey>;
        
        procedure FillQueue(aTree: TLLRBTree<TKey, T>; aRoot: TKeyNode; aLow, aHigh: TKey);
      protected
        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(aTree: TLLRBTree<TKey, T>; aLow, aHigh: TKey);
        destructor Destroy; override;
      end;
  private
    FRoot: TKeyNode;
    FComparer: IComparer<TKey>;

    function GetCount: Integer;

    function IsRed(aNode: TKeyNode): Boolean;
    procedure ColorFlip(aNode: TKeyNode);
    function RotateLeft(aNode: TKeyNode): TKeyNode;
    function RotateRight(aNode: TKeyNode): TKeyNode;
    function MoveRedLeft(aNode: TKeyNode): TKeyNode;
    function MoveRedRight(aNode: TKeyNode): TKeyNode;

    function DoLocate(aKey: TKey; aCreateIfNotExists: Boolean; var aRoot: TKeyNode; out aIsNew: Boolean): TKeyNode;
    function DoDelete(aNode: TKeyNode; aKey: TKey): TKeyNode;
    function DoDeleteMin(aNode: TKeyNode): TKeyNode;
    function DoDeleteMax(aNode: TKeyNode): TKeyNode;

    function DoMin(aNode: TKeyNode): TKeyNode;
    function DoMax(aNode: TKeyNode): TKeyNode;
    function Balance(aNode: TKeyNode): TKeyNode;

    function Size(aNode: TKeyNode): Integer;
    function DoFloor(aNode: TKeyNode; aKey: TKey): TKeyNode;
    function DoCeiling(aNode: TKeyNode; aKey: TKey): TKeyNode;

    function DoSelect(aNode: TKeyNode; aRank: Integer): TKeyNode;
    // Return number of key less than aNode.Key in the subtree rooted at aNode
    function DoRank(aNode: TKeyNode; aKey: TKey): Integer;

    procedure DisposeNode(var aNode: TKeyNode);
    function LocateNode(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): TKeyNode;
  private
    function DoIsBalanced(aNode: TKeyNode; aBlack: Integer): Boolean;
    function IsBalanced: Boolean;
    function DoIs23Tree(aNode: TKeyNode): Boolean;
    function Is23Tree: Boolean;
    function IsRankConsistent: Boolean;
    function DoIsSizeConsistent(aNode: TKeyNode): Boolean;
    function IsSizeConsistent: Boolean;
    function DoIsBST(aNode: TKeyNode; Min, Max: PKey): Boolean;
    function IsBST: Boolean;
  protected
    procedure DisposeT(var aValue: T); virtual;

    function LocateKey(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): PT;

    constructor Create(aComparer: IComparer<TKey>); overload;

    function DoGetEnumerator: TEnumerator<TKey>; override;
    function DoGetKeysEnumenator(aRoot: TKeyNode; aLow, aHigh: TKey): TEnumerator<TKey>;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function Locate(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): PT;


    procedure Delete(aKey: TKey);
    procedure DeleteMin;
    procedure DeleteMax;

    function Min: TKey;
    function Max: TKey;
    
    // Returns the largest key in the symbol table less than or equal to aKey
    function Floor(aKey: TKey): TKey;
    // Returns the smallest key in the symbol table greater than or equal to aKey
    function Ceiling(aKey: TKey): TKey;

    function Select(aRank: Integer): TKey;
    function Rank(aKey: TKey): Integer;

    function Check: Boolean;

    function IsEmpty: Boolean;

    property Count: Integer read GetCount;
  end;

implementation

uses
  sysutils;

{ TRBTree<TKey, T>.TNode }

constructor TLLRBTree<TKey, T>.TKeyNode.Create(aKey: TKey; aSize: Integer);
begin
  inherited Create;
  PKey(@Value)^ := aKey;
  Size := aSize;
  Color := RED;
end;

{ TRBTree<TKey, T> }

constructor TLLRBTree<TKey, T>.Create;
begin
  Create(TComparer<TKey>.Default);
end;

constructor TLLRBTree<TKey, T>.Create(aComparer: IComparer<TKey>);
begin
  inherited Create;
  if aComparer = nil then
    FComparer := TComparer<TKey>.Default
  else
    FComparer := aComparer;
end;

destructor TLLRBTree<TKey, T>.Destroy;
begin
  DisposeNode(FRoot);
  inherited;
end;

function TLLRBTree<TKey, T>.LocateNode(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): TKeyNode;
var
  wNode: TKeyNode;
  IsNew: Boolean;
begin
  Result := DoLocate(aKey, aCreateIfNotExists, FRoot, IsNew);

  if IsNew then // ensure that root node is black. make sens only if Result node was create and tree was rebalanced
    FRoot.Color := BLACK;
  if aIsNew <> nil then
    aIsNew^ := IsNew;
end;

procedure TLLRBTree<TKey, T>.Delete(aKey: TKey);
begin
  if not IsRed(FRoot.Left) and not IsRed(FRoot.Right) then
    FRoot.Color := RED;
  FRoot := DoDelete(FRoot, aKey);
  if not IsEmpty then
    FRoot.Color := BLACK;
end;

procedure TLLRBTree<TKey, T>.DeleteMin;
begin
  if not IsRed(FRoot.Left) and not IsRed(FRoot.Right) then
    FRoot.Color := RED;
  FRoot := DoDeleteMin(FRoot);
  if not IsEmpty then
    FRoot.Color := BLACK;
end;

procedure TLLRBTree<TKey, T>.DeleteMax;
begin
  if IsEmpty then
    Exit;

  if not IsRed(FRoot.Left) and not IsRed(FRoot.Right) then
    FRoot.Color := RED;

  FRoot := DoDeleteMax(FRoot);
  if not IsEmpty() then
    FRoot.Color := BLACK;
end;

function TLLRBTree<TKey, T>.Min: TKey;
begin
  if IsEmpty then
    Result := Default(TKey)
  else
    Result := DoMin(FRoot).GetKey;
end;

function TLLRBTree<TKey, T>.Max: TKey;
begin
  if IsEmpty then
    Result := Default(TKey)
  else
    Result := DoMax(FRoot).GetKey;
end;

function TLLRBTree<TKey, T>.Floor(aKey: TKey): TKey;
var
  Node: TKeyNode;
begin
  Node := DoFloor(FRoot, aKey);
  if Node = nil then
    Result := Default(TKey)
  else
    Result := Node.GetKey;
end;

function TLLRBTree<TKey, T>.Ceiling(aKey: TKey): TKey;
var
  wNode: TKeyNode;
begin
  if IsEmpty then
  begin
    Result := Default(TKey);
    Exit;
  end;

  wNode := DoCeiling(FRoot, aKey);
  if wNode <> nil then
    Result := wNode.GetKey
  else
    Result := Default(TKey);
end;

function TLLRBTree<TKey, T>.Select(aRank: Integer): TKey;
begin
  if (aRank < 0) or (aRank >= Count) then
    Result := Default(TKey)
  else
    Result := DoSelect(FRoot, aRank).GetKey;
end;

function TLLRBTree<TKey, T>.Rank(aKey: TKey): Integer;
begin
  Result := DoRank(FRoot, aKey);
end;

function TLLRBTree<TKey, T>.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TLLRBTree<TKey, T>.DoFloor(aNode: TKeyNode; aKey: TKey): TKeyNode;
var
  Cmp: Integer;
  wNode: TKeyNode;
begin
  Result := nil;
  if aNode = nil then
    Exit;

  Cmp := FComparer.Compare(aKey, aNode.GetKey);
  if Cmp = 0 then
    Result := aNode
  else if Cmp < 0 then
    Result := DoFloor(aNode.Left, aKey)
  else
  begin
    wNode := DoFloor(aNode.Right, aKey);
    if wNode <> nil then
      Result := wNode
    else
      Result := aNode;
  end;
end;

function TLLRBTree<TKey, T>.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result := DoGetKeysEnumenator(FRoot, Min, Max);
end;

function TLLRBTree<TKey, T>.DoGetKeysEnumenator(aRoot: TKeyNode; aLow, aHigh: TKey): TEnumerator<TKey>;
begin
  Result := TTreeEnumerator.Create(Self, aLow, aHigh);
end;

function TLLRBTree<TKey, T>.DoCeiling(aNode: TKeyNode; aKey: TKey): TKeyNode;
var
  Cmp: Integer;
  wNode: TKeyNode;
begin
  Result := nil;
  if aNode = nil then
    Exit;

  Cmp := FComparer.Compare(aKey, aNode.GetKey);
  if Cmp = 0 then
    Result := aNode
  else if Cmp > 0 then
    Result := DoCeiling(aNode.Right, aKey)
  else
  begin
    wNode := DoCeiling(aNode.Left, aKey);
    if wNode <> nil then
      Result := aNode
    else
      Result := nil;
  end;
end;

function TLLRBTree<TKey, T>.DoSelect(aNode: TKeyNode; aRank: Integer): TKeyNode;
var
  N: Integer;
begin
  Result := nil;
  if aNode = nil then
    Exit;
  N := Size(aNode.Left);
  if N > aRank then
    Result := DoSelect(aNode.Left, aRank)
  else if N < aRank then
    Result := DoSelect(aNode.Right, aRank - N - 1)
  else
    Result := aNode;
end;

function TLLRBTree<TKey, T>.DoRank(aNode: TKeyNode; aKey: TKey): Integer;
var
  Cmp: Integer;
begin
  Result := 0;
  if aNode = nil then
    Exit;

  Cmp := FComparer.Compare(aKey, aNode.GetKey);
  if Cmp < 0 then
    Result := DoRank(aNode.Left, aKey)
  else if Cmp > 0 then
    Result := 1 + Size(aNode.Left) + DoRank(aNode.Right, aKey)
  else
    Result := Size(aNode.Left);
end;

procedure TLLRBTree<TKey, T>.DisposeNode(var aNode: TKeyNode);
begin
  if aNode = nil then
    Exit;

  DisposeNode(aNode.Left);
  DisposeNode(aNode.Right);
  DisposeT(aNode.Value);
  FreeAndNil(aNode);
end;

procedure TLLRBTree<TKey, T>.DisposeT(var aValue: T);
begin
end;

function TLLRBTree<TKey, T>.LocateKey(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): PT;
var
  Node: TKeyNode;
begin
  Node := LocateNode(aKey, aCreateIfNotExists, aIsNew);
  if Node <> nil then
    Result := @Node.Value
  else
    Result := nil;
end;

function TLLRBTree<TKey, T>.Locate(aKey: TKey; aCreateIfNotExists: Boolean; aIsNew: PBoolean): PT;
begin
  Result := LocateKey(aKey, aCreateIfNotExists, aIsNew);
end;

function TLLRBTree<TKey, T>.GetCount: Integer;
begin
  Result := Size(FRoot);
end;

function TLLRBTree<TKey, T>.IsRed(aNode: TKeyNode): Boolean;
begin
  Result := (aNode <> nil) and (aNode.Color = RED);
end;

procedure TLLRBTree<TKey, T>.ColorFlip(aNode: TKeyNode);
begin
  aNode.Color := not aNode.Color;
  aNode.Left.Color := not aNode.Left.Color;
  aNode.Right.Color := not aNode.Right.Color;
end;

function TLLRBTree<TKey, T>.RotateLeft(aNode: TKeyNode): TKeyNode;
begin
  Result := aNode.Right;
  aNode.Right := Result.Left;
  Result.Left := aNode;
  Result.Color := aNode.Color;

  Result.Size := aNode.Size;
  aNode.Size := 1 + Size(aNode.Left) + Size(aNode.Right);

  aNode.Color := RED;
end;

function TLLRBTree<TKey, T>.RotateRight(aNode: TKeyNode): TKeyNode;
begin
  Result := aNode.Left;
  aNode.Left := Result.Right;
  Result.Right := aNode;
  Result.Color := aNode.Color;

  Result.Size := aNode.Size;
  aNode.Size := 1 + Size(aNode.Left) + Size(aNode.Right);

  aNode.Color := RED;
end;

function TLLRBTree<TKey, T>.MoveRedLeft(aNode: TKeyNode): TKeyNode;
begin
  ColorFlip(aNode);
  if IsRed(aNode.Right.Left) then
  begin
    aNode.Right := RotateRight(aNode.Right);
    aNode := RotateLeft(aNode);
    ColorFlip(aNode);
  end;
  Result := aNode;
end;

function TLLRBTree<TKey, T>.MoveRedRight(aNode: TKeyNode): TKeyNode;
begin
  ColorFlip(aNode);
  if IsRed(aNode.Left.Left) then
  begin
    aNode := RotateRight(aNode);
    ColorFlip(aNode);
  end;
  Result := aNode;
end;

function TLLRBTree<TKey, T>.DoLocate(aKey: TKey; aCreateIfNotExists: Boolean; var aRoot: TKeyNode; out aIsNew: Boolean): TKeyNode;
var
  Cmp: Integer;
begin
  if aRoot = nil then
  begin
    if aCreateIfNotExists then
    begin
      Result := TKeyNode.Create(aKey, 1);

      aRoot := Result; // new node set as root, balance on way up
      aIsNew := True;;
    end
    else
    begin
      Result := nil;
      aIsNew := False;
    end;
  end
  else
  begin
    Cmp := FComparer.Compare(aKey, aRoot.GetKey);
    if Cmp = 0 then
    begin
      Result := aRoot;
      aIsNew := False;
    end
    else if Cmp < 0 then
      Result := DoLocate(aKey, aCreateIfNotExists, aRoot.Left, aIsNew)
    else
      Result := DoLocate(aKey, aCreateIfNotExists, aRoot.Right, aIsNew);

    if aIsNew then
    begin
      if IsRed(aRoot.Right) and not IsRed(aRoot.Left) then
        aRoot := RotateLeft(aRoot);
      if IsRed(aRoot.Left) and IsRed(aRoot.Left.Left) then
        aRoot := RotateRight(aRoot);
      if IsRed(aRoot.Left) and IsRed(aRoot.Right) then
        ColorFlip(aRoot);

      aRoot.Size := 1 + Size(aRoot.Left) + Size(aRoot.Right);
    end;
  end;
end;

{
  if ARoot = nil then
  begin
    if ACreateIfMissing then
    begin
      New(Result);
      FillChar(Result^, SizeOf(Result^), 0);
      ARoot := Result;
      SetRed(ARoot, True);
      AIsNew := True;
    end
    else
    begin
      Result := nil;
      AIsNew := False;
    end;
  end
  else
  begin
    // tree search
    RootNode := GetNode(ARoot);
    c := FComparer.Compare(AKey, PKey(@RootNode.Value)^);
    if c < 0 then
      Result := DoLocate(RootNode.Left, AKey, ACreateIfMissing, AIsNew)
    else if c > 0 then
      Result := DoLocate(RootNode.Right, AKey, ACreateIfMissing, AIsNew)
    else
    begin
      Result := RootNode;
      AIsNew := False;
    end;

    if AIsNew then
    // re-balance
    begin
      if IsRed(RootNode.Right) and not IsRed(RootNode.Left) then
        ARoot := RotateLeft(ARoot);
      if IsRed(GetNode(ARoot).Left) and IsRed(GetNode(GetNode(ARoot).Left).Left) then
        ARoot := RotateRight(ARoot);
      if IsRed(GetNode(ARoot).Left) and IsRed(GetNode(ARoot).Right) then
        FlipColors(ARoot);
    end;
  end;

  balance
  if IsRed(GetNode(ALink).Right) then
    ALink := RotateLeft(ALink);
  if IsRed(GetNode(ALink).Left) and IsRed(GetNode(GetNode(ALink).Left).Left) then
    ALink := RotateRight(ALink);
  if IsRed(GetNode(ALink).Left) and IsRed(GetNode(ALink).Right) then
    FlipColors(ALink);


}

function TLLRBTree<TKey, T>.DoDelete(aNode: TKeyNode; aKey: TKey): TKeyNode;
var
  wNode: TKeyNode;
begin
  if (FComparer.Compare(aKey, aNode.GetKey) < 0) then
  begin
    if not IsRed(aNode.Left) and not IsRed(aNode.Left.Left) then
      aNode := MoveRedLeft(aNode);
    aNode.Left := DoDelete(aNode.Left, aKey);
  end
  else
  begin
    if IsRed(aNode.Left) then
      aNode := RotateRight(aNode);

    if (FComparer.Compare(aKey, aNode.GetKey) = 0) and (aNode.Right = nil) then
    begin
      DisposeNode(aNode);
      Exit(nil);
    end;

    if not IsRed(aNode.Right) and not IsRed(aNode.Right.Left) then
      aNode := MoveRedRight(aNode);

    if FComparer.Compare(aKey, aNode.GetKey) = 0 then
    begin
      wNode := DoMin(aNode.Right);
//      aNode.Value := wNode.Value;
      PKey(@aNode.Value)^ := wNode.GetKey;
      aNode.Right := DoDeleteMin(aNode.Right);
    end
    else
      aNode.Right := DoDelete(aNode.Right, aKey);
  end;

  Result := Balance(aNode);
end;

function TLLRBTree<TKey, T>.DoDeleteMin(aNode: TKeyNode): TKeyNode;
var
  wNode: TKeyNode;
begin
  Result := nil;
  if aNode.Left = nil then
  begin
    DisposeNode(aNode);
    Exit;
  end;

  if not IsRed(aNode.Left) and not IsRed(aNode.Left.Left) then
    aNode := MoveRedLeft(aNode);

  aNode.Left := DoDeleteMin(aNode.Left);

  Result := Balance(aNode);
end;

function TLLRBTree<TKey, T>.DoDeleteMax(aNode: TKeyNode): TKeyNode;
begin
  if IsRed(aNode.Left) then
    aNode := RotateRight(aNode);

  if aNode.Right = nil then
  begin
    DisposeNode(aNode);
    Exit(nil);
  end;

  if not IsRed(aNode.Right) and not IsRed(aNode.Right.Left) then
    aNode := MoveRedRight(aNode);

  aNode.Right := DoDeleteMax(aNode.Right);

  Result := Balance(aNode);
end;

function TLLRBTree<TKey, T>.DoMin(aNode: TKeyNode): TKeyNode;
begin
  if aNode.Left = nil then
    Result := aNode
  else
    Result := DoMin(aNode.Left);
end;

function TLLRBTree<TKey, T>.DoMax(aNode: TKeyNode): TKeyNode;
begin
  if aNode.Right = nil then
    Result := aNode
  else
    Result := DoMax(aNode.Right);
end;

function TLLRBTree<TKey, T>.Balance(aNode: TKeyNode): TKeyNode;
begin
  if IsRed(aNode.Right) then
    aNode := RotateLeft(aNode);
  if IsRed(aNode.Left) and IsRed(aNode.Left.Left) then
    aNode := RotateRight(aNode);
  if IsRed(aNode.Left) and IsRed(aNode.Right) then
    ColorFlip(aNode);

  aNode.Size := 1 + Size(aNode.Left) + Size(aNode.Right);
  Result := aNode;
end;

function TLLRBTree<TKey, T>.Size(aNode: TKeyNode): Integer;
begin
  if aNode = nil then
    Result := 0
  else
    Result := aNode.Size;
end;

function TLLRBTree<TKey, T>.TKeyNode.GetKey: TKey;
begin
  Result := PKey(@Value)^;
end;

   {***************************************************************************
    *  Check integrity of red-black tree data structure.
    ***************************************************************************}
function TLLRBTree<TKey, T>.Check: Boolean;
begin
(*  if not isBST() then StdOut.println("Not in symmetric order");
  if (!isSizeConsistent()) StdOut.println("Subtree counts not consistent");
  if (!isRankConsistent()) StdOut.println("Ranks not consistent");
  if (!is23())             StdOut.println("Not a 2-3 tree");
  if (!isBalanced())       StdOut.println("Not balanced");
*)
  Result := IsBST and IsSizeConsistent and IsRankConsistent and Is23Tree and IsBalanced;
end;

    // does this binary tree satisfy symmetric order?
    // Note: this test also ensures that data structure is a binary tree since order is strict
function TLLRBTree<TKey, T>.IsBST: Boolean;
begin
  Result := DoIsBST(FRoot, nil, nil);
end;

// is the tree rooted at x a BST with all keys strictly between min and max
// (if min or max is null, treat as empty constraint)
// Credit: Bob Dondero's elegant solution
function TLLRBTree<TKey, T>.DoIsBST(aNode: TKeyNode; Min, Max: PKey): Boolean;
var
  wKey: TKey;
begin
  if aNode = nil then
    Exit(True);

  if (Min <> nil) and (FComparer.Compare(aNode.GetKey, Min^) <= 0) then
    Exit(False);

  if (Max <> nil) and (FComparer.Compare(aNode.GetKey, Max^) >= 0) then
    Exit(False);

  wKey := aNode.GetKey;
  Result := DoIsBST(aNode.Left, Min, @wKey) and DoIsBST(aNode.right, @wKey, Max);
end;

// are the size fields correct?
function TLLRBTree<TKey, T>.IsSizeConsistent: Boolean;
begin
  Result := DoIsSizeConsistent(FRoot);
end;

function TLLRBTree<TKey, T>.DoIsSizeConsistent(aNode: TKeyNode): Boolean;
begin
  if aNode = nil then
    Exit(True);

  if aNode.Size <> (size(aNode.Left) + size(aNode.Right) + 1) then
    Exit(False);

  Result := DoIsSizeConsistent(aNode.Left) and DoIsSizeConsistent(aNode.Right);
end;

// check that ranks are consistent
function TLLRBTree<TKey, T>.IsRankConsistent(): Boolean;
var
  I: Integer;
  Key: TKey;
begin
  for I := 0 to Count - 1 do
    if i <> rank(select(i)) then
      Exit(False);

//  for Key in keys()
//    if FComparer.Compare(Key, Select(Rank(Key))) <> 0 then
//      Exit(False);

  Result := True;
end;

// Does the tree have no red right links, and at most one (left)
// red links in a row on any path?
function TLLRBTree<TKey, T>.Is23Tree(): Boolean;
begin
  Result := DoIs23Tree(Froot);
end;

function TLLRBTree<TKey, T>.DoIs23Tree(aNode: TKeyNode): Boolean;
begin
  if aNode = nil then
    Exit(True);

  if isRed(aNode.Right) then
    Exit(False);

  if (aNode <> Froot) and isRed(aNode) and isRed(aNode.Left) then
    Exit(False);

  Result := DoIs23Tree(aNode.Left) and DoIs23Tree(aNode.Right);
end;

// do all paths from root to leaf have same number of black edges?
function TLLRBTree<TKey, T>.IsBalanced(): Boolean;
var
  Black: Integer;
  Node: TKeyNode;
begin
  black := 0;     // number of black links on path from root to min
  Node := FRoot;
  while (Node <> nil) do
  begin
      if not isRed(Node) then
        Inc(Black);

      Node := Node.Left;
  end;

  Result := DoIsBalanced(FRoot, Black);
end;

// does every path from the root to a leaf have the given number of black links?
function TLLRBTree<TKey, T>.DoIsBalanced(aNode: TKeyNode; aBlack: Integer): Boolean;
begin
  if aNode = nil then
    Exit(aBlack = 0);

  if not isRed(aNode) then
    Dec(aBlack);

  Result := DoIsBalanced(aNode.left, aBlack) and DoIsBalanced(aNode.right, aBlack);
end;

{ TLLRBTree<TKey, T>.TTreeEnumerator }

constructor TLLRBTree<TKey, T>.TTreeEnumerator.Create(aTree: TLLRBTree<TKey, T>; aLow, aHigh: TKey);
begin
  inherited Create;
  FQueue := TQueue<TKey>.Create;
  FillQueue(aTree, aTree.FRoot, aLow, aHigh);
  FQueueEnumerator := FQueue.GetEnumerator;
end;

destructor TLLRBTree<TKey, T>.TTreeEnumerator.Destroy;
begin
  FreeAndNil(FQueue);
  FreeAndNil(FQueueEnumerator);
  inherited;
end;

function TLLRBTree<TKey, T>.TTreeEnumerator.DoGetCurrent: TKey;
begin
  Result :=  FQueueEnumerator.Current;
end;

function TLLRBTree<TKey, T>.TTreeEnumerator.DoMoveNext: Boolean;
begin
  Result := FQueueEnumerator.MoveNext;
end;

procedure TLLRBTree<TKey, T>.TTreeEnumerator.FillQueue(aTree: TLLRBTree<TKey, T>; aRoot: TKeyNode; aLow, aHigh: TKey);
var
  CmpLo, CmpHi: Integer;
begin
  if aRoot = nil then
    Exit;
  
  CmpLo := aTree.FComparer.Compare(aLow, aRoot.GetKey);
  CmpHi := aTree.FComparer.Compare(aLow, aRoot.GetKey);  
  if CmpLo < 0 then
    FillQueue(aTree, aRoot.Left, aLow, aHigh);
    
  if (CmpLo <= 0) and (CmpHi >= 0) then
    FQueue.Enqueue(aRoot.GetKey);
  if CmpHi > 0 then
    FillQueue(aTree, aRoot.Right, aLow, aHigh);    
end;

end.