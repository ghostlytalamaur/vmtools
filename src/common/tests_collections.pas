unit tests_collections;
interface
implementation

uses
  testframework, sysutils, classes, generics.collections, strutils, collections.array_utils,
  generics.defaults, collections.tst, collections.deque;

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

initialization
  TestFramework.RegisterTest('common.collections.tst', TTSTTests.Suite);
  TestFramework.RegisterTest('common.collections.deque', TDequeTests.Suite);

end.

