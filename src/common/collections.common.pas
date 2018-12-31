unit collections.common;

interface

uses
  vmsys, generics.collections, sysutils, Classes, collections.types;

type
  TCollectionsUtils = record
  private
    class function WrapGetMethod(aList: TStringList): TFunc<Integer, string>; static;
  public
    class function FirstThat<T>(aEnumerable: TEnumerable<T>; aPredicate: TPredicate<T>): T; static;
    class function Map<T, R>(aEnumerable: IEnumerable<T>; aMapper: TMapper<T, R>): IEnumerable<R>; static;
    class function Wrap<T>(aEnumerable: TEnumerable<T>): IEnumerable<T>; overload; static;
    class function Wrap<T>(aEnumerable: IObjectHolder<TEnumerable<T>>): IEnumerable<T>; overload; static;
    class function Wrap<T>(const aArray: array of T): IEnumerable<T>; overload; static;
    class function Wrap(aList: IObjectHolder<TStringList>): IEnumerable<string>; overload; static;
    class function Wrap(aList: TStringList): IEnumerable<string>; overload; static;
    class function Seq(aStart, aStep, aCount: Integer): IEnumerable<Integer>; static;
    class function Empty<T>: IEnumerable<T>; static;
    class function AtValue<T>(aList: TList<T>; aIndex: Integer): T; static;
  end;

  IAccumulator<T> = interface
    procedure Apply(aValue: T);
  end;

  TPipeline<T> = record
  private
    FEnumerable: IEnumerable<T>;
  public
    // operators
    function Filter(aPredicate: TFunc<T, Boolean>): TPipeline<T>;
    function Map<R>(aMapper: TMapper<T, R>): TPipeline<R>; overload;
    function Map<R>(aMapper: TFunc<T, R>): TPipeline<R>; overload;

    // terminal
    function Enum: IEnumerable<T>;
    function Count: Integer;
    procedure ForEach(aAction: TForEachAction<T>);
  end;

  Pipeline<T> = record
  public
    class function From(aEnumerable: IEnumerable<T>): TPipeline<T>; overload; static;
    class function From(aEnumerable: TEnumerable<T>): TPipeline<T>; overload; static;
    class function From(aEnumerable: IObjectHolder<TEnumerable<T>>): TPipeline<T>; overload; static;
    class function From(const aArray: array of T): TPipeline<T>; overload; static;
  end;

implementation

uses
  collections.array_utils, collections.enumerators;


{ TCollectionsUtils }

class function TCollectionsUtils.AtValue<T>(aList: TList<T>; aIndex: Integer): T;
begin
  Result := Default(T);
  if (aList = nil) or (aIndex < 0) or (aIndex >= aList.Count) then
    Exit;

  Result := aList[aIndex];
end;

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

class function TCollectionsUtils.Map<T, R>(aEnumerable: IEnumerable<T>; aMapper: TMapper<T, R>): IEnumerable<R>;
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

class function TCollectionsUtils.Seq(aStart, aStep, aCount: Integer): IEnumerable<Integer>;
begin
  Result := TEnumerableFactory<Integer>.Create(function : IEnumerator<Integer>
  begin
    Result := TIndexedEnumerator<Integer>.Create(aStart, aStep, aCount, function (aIndex: Integer): Integer
    begin
      Result := aIndex;
    end);
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

class function TCollectionsUtils.Wrap<T>(const aArray: array of T): IEnumerable<T>;
var
  Arr: TArray<T>;
begin
  Arr := TArrayUtils.AsArray(aArray);
  Result := TEnumerableFactory<T>.Create(function : IEnumerator<T>
  begin
    Result := TIndexedEnumerator<T>.Create(Low(Arr), 1, Length(Arr), function (aIndex: Integer): T
    begin
      Result := Arr[aIndex];
    end);
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

class function TCollectionsUtils.Wrap(aList: IObjectHolder<TStringList>): IEnumerable<string>;
begin
  if aList <> nil then
    Result := TEnumerableFactory<string>.Create(function : IEnumerator<string>
    begin
      Result := TIndexedEnumerator<string>.Create(0, 1, aList.Obj.Count, WrapGetMethod(aList.Obj));
    end)
  else
    Result := Empty<string>;
end;

class function TCollectionsUtils.Wrap(aList: TStringList): IEnumerable<string>;
begin
  if aList <> nil then
    Result := TEnumerableFactory<string>.Create(function : IEnumerator<string>
    begin
      Result := TIndexedEnumerator<string>.Create(0, 1, aList.Count, WrapGetMethod(aList));
    end)
  else
    Result := Empty<string>;
end;

{ TPipeline<T> }

function TPipeline<T>.Enum: IEnumerable<T>;
begin
  if FEnumerable <> nil then
    Result := FEnumerable
  else
    Result := TCollectionsUtils.Empty<T>;
end;

function TPipeline<T>.Count: Integer;
var
  Item: T;
begin
  Result := 0;
  for Item in Enum do
    Inc(Result);
end;

procedure TPipeline<T>.ForEach(aAction: TForEachAction<T>);
var
  Item: T;
begin
  for Item in Enum do
    aAction(Item);
end;

function TPipeline<T>.Filter(aPredicate: TFunc<T, Boolean>): TPipeline<T>;
var
  Stage: IEnumerable<T>;
begin
  Stage := TPipelineMappingEnumerable<T, T>.Create(FEnumerable,
    function (aSourceEnumerator: IEnumerator<T>): IEnumerator<T>
    begin
      Result := TFilterEnumerator<T>.Create(aSourceEnumerator, aPredicate);
    end);

  Result := Pipeline<T>.From(Stage);
end;

function TPipeline<T>.Map<R>(aMapper: TMapper<T, R>): TPipeline<R>;
var
  Stage: IEnumerable<R>;
begin
  Stage := TPipelineMappingEnumerable<T, R>.Create(FEnumerable,
    function (aSourceEnumerator: IEnumerator<T>): IEnumerator<R>
    begin
      Result := TPipelineMappingEnumerator<T, R>.Create(aSourceEnumerator, aMapper);
    end);

  Result := Pipeline<R>.From(Stage);
end;

function TPipeline<T>.Map<R>(aMapper: TFunc<T, R>): TPipeline<R>;
begin
  Result := Map<R>(
    function (const aValue: T): R
    begin
      Result := aMapper(aValue)
    end);
end;

{ Pipeline<T> }

class function Pipeline<T>.From(aEnumerable: IEnumerable<T>): TPipeline<T>;
begin
  Result.FEnumerable := aEnumerable;
end;

class function Pipeline<T>.From(aEnumerable: TEnumerable<T>): TPipeline<T>;
begin
  Result.FEnumerable := TCollectionsUtils.Wrap<T>(aEnumerable);
end;

class function Pipeline<T>.From(aEnumerable: IObjectHolder<TEnumerable<T>>): TPipeline<T>;
begin
  Result.FEnumerable := TCollectionsUtils.Wrap<T>(aEnumerable);
end;

class function Pipeline<T>.From(const aArray: array of T): TPipeline<T>;
begin
  Result.FEnumerable := TCollectionsUtils.Wrap<T>(aArray);
end;

end.