unit tests_collections;
interface
implementation

uses
  testframework, sysutils, classes, generics.collections, strutils, collections.array_utils,
  generics.defaults, collections.tst, collections.deque, collections.common, fluentquery.genericobjects,
  collections.sets;

type
  TTSTTests = class(TTestCase)
  private
    function GetTrie<T>(const aKeys: array of string; const aValues: array of T): TTST<T>;
  published
    procedure TestDelete;
    procedure TestClear;
    procedure TestEnumeratorPairs;
    procedure TestEnumeratorKeys;
    procedure TestEnumeratorValues;
    procedure TestDisposeValue;
  end;

  TDequeTests = class(TTestCase)
  published
    procedure TestDeque;
    procedure TestDequeDispose;
    procedure TestDequeEnumerator;
  end;

  TCollectionsTests = class(TTestCase)
  published
    procedure TestWrapList;
    procedure TestWrapStringList;
    procedure TestMap;
    procedure TestEmpty;
    procedure TestSeq;
    procedure TestReverseSeq;
    procedure TestSeq2;
    procedure TestReverseSeq2;
    procedure TestArrayEnumerator;
  end;

  TPipelineTests = class(TTestCase)
  private
  published
    procedure TestObjectQuery;
    procedure TestFilterMap;
    procedure TestFilter;
    procedure TestMap;
    procedure TestCount;
    procedure TestForEach;
  end;

{ TVMSymTests }


{ TStrUtilsTests }


{ TTSTTests }

const
  cstKeys: array [0..16] of string = (
    'she',
    'sells',
    'sea',
    'shells',
    'are',
    'by',
    'the',
    'shore',
    'test',
    'tst',
    'string',
    'procedure',
    'function',
    'a',
    's',
    'z',
    'zzz'
  );

procedure TTSTTests.TestEnumeratorPairs;

  procedure DoCheckWithPrefix(aTree: TTST<Integer>; aPrefix: string);
  var
    Used: TArray<Boolean>;
    P: TPair<string, Integer>;
    I: Integer;
  begin
    Used := TArrayUtils.GetEmptyArray<Boolean>(Length(cstKeys), False);
    for P in aTree.PairsWithPrefix(aPrefix) do
    begin
      I := IndexStr(P.Key, cstKeys);
      Check((I >= Low(cstKeys)) and (I <= High(cstKeys)), 'Invalid key: ' + P.Key + '. Prefix: ' + aPrefix);
      CheckFalse(Used[I], 'Key already used: ' + P.Key  + '. Prefix: ' + aPrefix);
      Used[I] := True;
    end;

    for I := 0 to High(cstKeys) do
      CheckEquals((aPrefix = '') or StartsStr(aPrefix, cstKeys[I]), Used[I], 'Key not was used: ' + cstKeys[I]);
  end;

var
  Tree: TTST<Integer>;
begin
  Tree := GetTrie<Integer>(cstKeys, TArrayUtils.GetEmptyIntegerArray(Length(cstKeys), 0, 1));
  try
    DoCheckWithPrefix(Tree, '');
    DoCheckWithPrefix(Tree, 'sh');
    DoCheckWithPrefix(Tree, 'a');
    DoCheckWithPrefix(Tree, 'b');
    DoCheckWithPrefix(Tree, 'z');
    DoCheckWithPrefix(Tree, 'se');
  finally
    FreeAndNil(Tree);
  end;
end;

procedure TTSTTests.TestEnumeratorKeys;
var
  Tree: TTST<Integer>;
  I: Integer;
  Used: TArray<Boolean>;
  Key: string;
begin
  Tree := GetTrie<Integer>(cstKeys, TArrayUtils.GetEmptyIntegerArray(Length(cstKeys), 0, 1));
  try
    Used := TArrayUtils.GetEmptyArray<Boolean>(Length(cstKeys), False);
    for Key in Tree.Keys do
    begin
      I := IndexStr(Key, cstKeys);
      Check((I >= Low(cstKeys)) and (I <= High(cstKeys)), 'Invalid key: ' + Key);
      CheckFalse(Used[I], 'Key already used: ' + Key);
      Used[I] := True;
    end;

    for I := 0 to High(Used) do
      Check(Used[I], 'Key not was used: ' + cstKeys[I]);
  finally
    FreeAndNil(Tree);
  end;
end;

procedure TTSTTests.TestEnumeratorValues;
var
  Tree: TTST<Integer>;
  I: Integer;
  Used: TArray<Boolean>;
begin
  Tree := GetTrie<Integer>(cstKeys, TArrayUtils.GetEmptyIntegerArray(Length(cstKeys), 0, 1));
  try
    Used := TArrayUtils.GetEmptyArray<Boolean>(Length(cstKeys), False);
    for I in Tree.Values do
    begin
      CheckFalse(Used[I], 'Value present twice: ' + IntToStr(I));
      Used[I] := True;
    end;

    for I := 0 to High(Used) do
      Check(Used[I], 'Key not was used: ' + cstKeys[I]);
  finally
    FreeAndNil(Tree);
  end;
end;

function TTSTTests.GetTrie<T>(const aKeys: array of string; const aValues: array of T): TTST<T>;
var
  I: Integer;
  Comparer: IEqualityComparer<T>;
begin
  Result := nil;
  CheckEquals(Length(aKeys), Length(aValues), 'Invalid key/values count.');
  Result := TTST<T>.Create;
  Comparer := TEqualityComparer<T>.Default;
  for I := Low(aKeys) to High(aValues) do
  begin
    Result.PutValue(aKeys[I], aValues[I]);
    Check(Result.ContainsKey(aKeys[I]), 'Key should be present in trie after insert: ' + aKeys[I]);
    Check(Comparer.Equals(aValues[I], Result.GetValue(aKeys[I])), 'Incorect value for key: ' + aKeys[I]);
  end;

  for I := Low(aKeys) to High(aKeys) do
  begin
    Check(Result.ContainsKey(aKeys[I]), 'Tree should contains key: ' + aKeys[I]);
    Check(Comparer.Equals(aValues[I], Result.GetValue(aKeys[I])), 'Incorrect value for key: ' + aKeys[I]);
  end;
end;

procedure TTSTTests.TestDelete;
var
  Tree: TTST<Integer>;
  I, J: Integer;
begin
  Tree := GetTrie<Integer>(cstKeys, TArrayUtils.GetEmptyIntegerArray(Length(cstKeys), 0, 1));
  try
    for I := Low(cstKeys) to High(cstKeys) do
    begin
      Tree.Delete(cstKeys[I]);
      CheckFalse(Tree.ContainsKey(cstKeys[I]), 'Tree should not contain key after delete: ' + cstKeys[I]);
      for J := I + 1 to High(cstKeys) do
      begin
        Check(Tree.ContainsKey(cstKeys[J]), 'Tree should contains key: ' + cstKeys[J]);
        CheckEquals(J, Tree.GetValue(cstKeys[J]), 'Incorrect value for key: ' + cstKeys[J]);
      end;
    end;
  finally
    FreeAndNil(Tree);
  end;
end;

type
  TIntegerObject = class
    Value: Integer;
  end;

procedure TTSTTests.TestDisposeValue;
var
  Tree: TTST<TIntegerObject>;
  FreeCount, I: Integer;
  Obj: TIntegerObject;
begin
  Tree := TTST<TIntegerObject>.Create(procedure (var aValue: TIntegerObject)
  begin
    CheckEquals(I, aValue.Value, 'Incorrect value.');
    FreeAndNil(aValue);
    Inc(FreeCount);
  end);
  try
    for I := Low(cstKeys) to High(cstKeys) do
    begin
      Obj := TIntegerObject.Create;
      Obj.Value := I;
      Tree.PutValue(cstKeys[I], Obj);
      CheckEquals(I + 1, Tree.Count, 'Incorrect count after putValue.');
    end;

    // Check overwrite
    for I := Low(cstKeys) to High(cstKeys) do
    begin
      Obj := TIntegerObject.Create;
      Obj.Value := I;
      Tree.PutValue(cstKeys[I], Obj);
      CheckEquals(Length(cstKeys), Tree.Count, 'Incorrect count after replace value.');
    end;
    CheckEquals(FreeCount, Length(cstKeys), Format('Free should call %d times.', [Length(cstKeys)]));

    FreeCount := 0;
    for I := Low(cstKeys) to High(cstKeys) do
    begin
      Tree.Delete(cstKeys[I]);
      CheckEquals(Length(cstKeys) - I - 1, Tree.Count, 'Incorrect count after delete value.');
    end;

    CheckEquals(FreeCount, Length(cstKeys), Format('Free should call %d times.', [Length(cstKeys)]));
  finally
    FreeAndNil(Tree);
  end;
end;

procedure TTSTTests.TestClear;
var
  Tree: TTST<Integer>;
  I: Integer;
begin
  Tree := GetTrie<Integer>(cstKeys, TArrayUtils.GetEmptyIntegerArray(Length(cstKeys), 0, 1));
  try
    for I := Low(cstKeys) to High(cstKeys) do
      Check(Tree.ContainsKey(cstKeys[I]), 'Tree should contains key: ' + cstKeys[I]);
    Tree.Clear;
    for I := Low(cstKeys) to High(cstKeys) do
      CheckFalse(Tree.ContainsKey(cstKeys[I]), 'Tree should not contains key: ' + cstKeys[I]);
  finally
    FreeAndNil(Tree);
  end;
end;

{ TDequeTests }

procedure TDequeTests.TestDeque;
var
  Q: TDeque<string>;
  Key: string;
  Cnt: Integer;
begin
  Q := TDeque<string>.Create;
  try
    // Check addfirst
    Cnt := 0;
    for Key in cstKeys do
    begin
      Q.AddFirst(Key);
      Inc(Cnt);
      CheckEquals(Key, Q.First, 'First element not equal.');
      CheckEquals(Cnt, Q.Count, 'Incorrect count when AddFirst().');
    end;

    // Check extractFrist
    while Cnt > 0 do
    begin
      CheckEquals(Q.ExtractFirst, cstKeys[Cnt - 1], 'Incorrect ExtractFirst element.');
      Dec(Cnt);
      CheckEquals(Q.Count, Cnt, 'Incorrect count when ExtractFirst().');
    end;

    // Check addLast
    Cnt := 0;
    for Key in cstKeys do
    begin
      Q.AddLast(Key);
      Inc(Cnt);
      CheckEquals(Key, Q.Last, 'Last element not equal.');
      CheckEquals(Cnt, Q.Count, 'Incorrect count when AddLast().');
    end;

    // Check extractLast
    while Cnt > 0 do
    begin
      CheckEquals(Q.ExtractFirst, cstKeys[Length(cstKeys) - Cnt], 'Incorrect ExtractLast element.');
      Dec(Cnt);
      CheckEquals(Q.Count, Cnt, 'Incorrect count when ExtractLast().');
    end;


    Cnt := 0;
    for Key in cstKeys do
    begin
      if Cnt mod 2 = 0 then
      begin
        Q.AddFirst(Key);
        CheckEquals(Key, Q.First, 'First element not equal.');
      end
      else
      begin
        Q.AddLast(Key);
        CheckEquals(Key, Q.Last, 'Last element not equal.');
      end;

      Inc(Cnt);
      CheckEquals(Cnt, Q.Count, 'Incorrect count when AddFirst()/AddLast().');
    end;

    while Cnt > 0 do
    begin
      if Cnt mod 2 = 0 then
        Q.RemoveFirst
      else
        Q.RemoveLast;

      Dec(Cnt);
      CheckEquals(Cnt, Q.Count, 'Incorrect count when removeFirst()/removeLast().');
    end;


    // Check clear
    Cnt := 0;
    for Key in cstKeys do
    begin
      Q.AddLast(Key);
      Inc(Cnt);
      CheckEquals(Key, Q.Last, 'Last element not equal.');
      CheckEquals(Cnt, Q.Count, 'Incorrect count when AddLast().');
    end;
    Q.Clear;
    CheckEquals(0, Q.Count, 'Incorrect count when Clear().');
  finally
    FreeAndNil(Q);
  end;
end;

procedure TDequeTests.TestDequeDispose;
var
  Q: TDeque<TObject>;
  Cnt: Integer;
begin
  Q := TDeque<TObject>.Create(procedure (var Value: TObject)
    begin
      FreeAndNil(Value);
      Inc(Cnt);
    end);
  try
    for Cnt := 1 to 10 do
    begin
      Q.AddFirst(TObject.Create);
      CheckEquals(Cnt, Q.Count, 'Incorrect count.');
    end;

    Cnt := 0;
    Q.Clear;
    CheckEquals(10, Cnt, 'OnDispose should be called 10 times.');
  finally
    FreeAndNil(Q);
  end;
end;

procedure TDequeTests.TestDequeEnumerator;
var
  Q: TDeque<string>;
  Key: string;
  Cnt: Integer;
begin
  Q := TDeque<string>.Create;
  try
    // Check addfirst
    Cnt := 0;
    for Key in cstKeys do
    begin
      Q.AddLast(Key);
      Inc(Cnt);
      CheckEquals(Key, Q.Last, 'First element not equal.');
      CheckEquals(Cnt, Q.Count, 'Incorrect count when AddLast().');
    end;

    Cnt := 0;
    for Key in Q do
    begin
      CheckEquals(cstKeys[Cnt], Key, 'Incorrect value with index = ' + IntToStr(Cnt));
      Inc(Cnt);
    end;
    CheckEquals(Length(cstKeys), Cnt, 'Should enumerate ' + IntToStr(Length(cstKeys)) + ' times.');
  finally
    FreeAndNil(Q);
  end;
end;

{ TCollectionsTests }

procedure TCollectionsTests.TestArrayEnumerator;
const
  cstArray: array [-1..3] of Integer = (-10, 20, 30, 40, 70);
var
  Value, Idx: Integer;
begin
  Idx := Low(cstArray) - 1;
  for Value in TCollectionsUtils.Wrap<Integer>(cstArray) do
  begin
    Inc(Idx);
    CheckEquals(cstArray[Idx], Value);
  end;
  CheckEquals(High(cstArray), Idx);
end;

procedure TCollectionsTests.TestEmpty;
var
  K: string;
begin
  for K in TCollectionsUtils.Empty<string> do
    Check(False, 'Empty enumerable should not have any items.');
end;

procedure TCollectionsTests.TestMap;
var
  List: TList<string>;
  Key: string;
  ListEnum: IEnumerable<string>;
  MapEnum: IEnumerable<Integer>;
  I, Idx: Integer;
begin
  List := TList<string>.Create;
  try
    for Key in cstKeys do
      List.Add(Key);

    ListEnum := TCollectionsUtils.Wrap<string>(List);
    MapEnum := TCollectionsUtils.Map<string, Integer>(ListEnum, function (const Item: string): Integer
    begin
      Result := IndexText(Item, cstKeys);
    end);

    Idx := 0;
    for I in MapEnum do
    begin
      CheckEquals(Idx, I, 'Invalid index after map.');
      Inc(Idx);
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TCollectionsTests.TestSeq;
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I in TCollectionsUtils.Seq(0, 1, 10) do
  begin
    Inc(Idx);
    CheckEquals(Idx, I);
  end;
  CheckEquals(9, Idx);
end;

procedure TCollectionsTests.TestSeq2;
var
  I, Idx: Integer;
begin
  Idx := -2;
  for I in TCollectionsUtils.Seq(0, 2, 10) do
  begin
    Inc(Idx, 2);
    CheckEquals(Idx, I);
  end;
  CheckEquals(18, Idx);
end;

procedure TCollectionsTests.TestReverseSeq;
var
  I, Idx: Integer;
begin
  Idx := 1;
  for I in TCollectionsUtils.Seq(0, -1, 10) do
  begin
    Dec(Idx);
    CheckEquals(Idx, I);
  end;
  CheckEquals(-9, Idx);
end;

procedure TCollectionsTests.TestReverseSeq2;
var
  I, Idx: Integer;
begin
  Idx := 2;
  for I in TCollectionsUtils.Seq(0, -2, 10) do
  begin
    Inc(Idx, -2);
    CheckEquals(Idx, I);
  end;
  CheckEquals(-18, Idx);
end;

procedure TCollectionsTests.TestWrapList;

  procedure DoTest(List: TList<string>; ListEnum: IEnumerable<string>);
  var
    Key: string;
    I: Integer;
  begin
    I := 0;
    for Key in ListEnum do
    begin
      CheckEquals(List[I], Key, Format('1. Different key at pos = %d', [I]));
      Inc(I);
    end;
    CheckEquals(List.Count, I, '1. Not all items present in enumerator');
  end;

var
  List: TList<string>;
  Key: string;
  ListEnum: IEnumerable<string>;
begin
  List := TList<string>.Create;
  try
    for Key in cstKeys do
      List.Add(Key);

    ListEnum := TCollectionsUtils.Wrap<string>(List);
    DoTest(List, ListEnum);
    DoTest(List, ListEnum);
  finally
    FreeAndNil(List);
  end;
end;

procedure TCollectionsTests.TestWrapStringList;

  procedure DoTest(List: TStringList; ListEnum: IEnumerable<string>);
  var
    Key: string;
    I: Integer;
  begin
    I := 0;
    for Key in ListEnum do
    begin
      CheckEquals(List[I], Key, Format('1. Different key at pos = %d', [I]));
      Inc(I);
    end;
    CheckEquals(List.Count, I, '1. Not all items present in enumerator');
  end;

var
  List: TStringList;
  Key: string;
  ListEnum: IEnumerable<string>;
begin
  List := TStringList.Create;
  try
    for Key in cstKeys do
      List.Add(Key);

    ListEnum := TCollectionsUtils.Wrap(List);
    DoTest(List, ListEnum);
    DoTest(List, ListEnum);
  finally
    FreeAndNil(List);
  end;
end;

{ TPipelineTests }

type
  TDataObject = class
  public
    Index: Integer;
    Key: string;
  end;

procedure TPipelineTests.TestObjectQuery;
var
  List: TList<TDataObject>;
  I: Integer;
  O: TDataObject;
begin
  List := TObjectList<TDataObject>.Create;
  try
    for I := 0 to High(cstKeys) do
    begin
      List.Add(TDataObject.Create);
      List[I].Index := I;
      List[I].Key := cstKeys[I]
    end;
    for O in ObjectQuery<TDataObject>.Select
        .Skip(1)
        .From(List) do;
  finally
    FreeAndNil(List);
  end;
end;

procedure TPipelineTests.TestFilterMap;
var
  Keys: ISet<Integer>;
  CurKey, Key: string;
  I, Cnt: Integer;
begin
  Keys := THashSet<Integer>.Create;
  for I in TCollectionsUtils.Seq(0, 1, Length(cstKeys)) do
    Keys.Add(I);
  Cnt := 0;
  for Key in Pipeline<Integer>.From(Keys)
      .Filter(function (aValue: Integer): Boolean
      begin
        Result := aValue = 1;
      end)
      .Map<string>(function (aValue: Integer): string
      begin
        Result := cstKeys[aValue];
      end)
      .Enum do
  begin
    Inc(Cnt);
    CurKey := Key;
  end;
  CheckEquals(1, Cnt);
  CheckEquals(cstKeys[1], CurKey);
end;

procedure TPipelineTests.TestMap;
var
  Keys: TList<string>;
  Key: string;
  I, Idx: Integer;
begin
  Keys := TList<string>.Create;
  try
    for Key in cstKeys do
      Keys.Add(Key);
    Idx := -1;
    for I in Pipeline<string>.From(TCollectionsUtils.Wrap<string>(Keys))
        .Map<Integer>(Keys.IndexOf)
        .Enum do
    begin
      Inc(Idx);
      CheckEquals(Idx, I);
    end;
    CheckEquals(Keys.Count, Idx + 1);
  finally
    FreeAndNil(Keys);
  end;
end;

procedure TPipelineTests.TestCount;
var
  Keys: ISet<string>;
  Key: string;
begin
  Keys := THashSet<string>.Create;
  for Key in cstKeys do
    Keys.Add(Key);
  CheckEquals(Length(cstKeys), Pipeline<string>.From(Keys).Count);
end;

procedure TPipelineTests.TestForEach;
var
  Keys: ISet<string>;
  List: TList<string>;
  Key: string;
begin
  Keys := THashSet<string>.Create;
  List := TList<string>.Create;
  try
    for Key in cstKeys do
      Keys.Add(Key);
    Pipeline<string>.From(Keys).ForEach(procedure (const aValue: string)
      begin
        List.Add(aValue);
      end);
    CheckEquals(Keys.Count, List.Count);
  finally
    FreeAndNil(List);
  end;
end;

procedure TPipelineTests.TestFilter;
var
  Keys: ISet<string>;
  CurKey, Key: string;
  Cnt: Integer;
begin
  Keys := THashSet<string>.Create;
  for Key in cstKeys do
    Keys.Add(Key);
  Cnt := 0;
  for Key in Pipeline<string>.From(Keys)
      .Filter(function (aKey: string): Boolean
      begin
        Result := aKey = 'sells';
      end).Enum do
  begin
    Inc(Cnt);
    CurKey := Key;
  end;
  CheckEquals(1, Cnt);
end;

initialization
  TestFramework.RegisterTest('common.collections.tst', TTSTTests.Suite);
  TestFramework.RegisterTest('common.collections.deque', TDequeTests.Suite);
  TestFramework.RegisterTest('common.collections.common', TCollectionsTests.Suite);
  TestFramework.RegisterTest('common.collections.fluentquery', TPipelineTests.Suite);

end.

