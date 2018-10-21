unit collections.array_utils;

interface

type
  TArrayUtils = record
  public
    class function Join<T>(const aArr1, aArr2: array of T): TArray<T>; overload; static;
    class function GetEmptyArray<T>(aLen: Integer; const aDefValue: T): TArray<T>; static;
    class function EqualItemsCount<T>(const aArr1, aArr2: array of T): Integer; static;
    class function AsArray<T>(aEnumerable: IEnumerable<T>): TArray<T>; overload; static;
    class function AsArray<T>(const aArr: array of T): TArray<T>; overload; static;

    class function GetEmptyIntegerArray(aLen, aStartValue, aStep: Integer): TArray<Integer>; static;
  end;

implementation

uses
  windows, sysutils, generics.defaults;

{ TArrayUtils }

class function TArrayUtils.Join<T>(const aArr1, aArr2: array of T): TArray<T>;
var
  Idx, I: Integer;
begin
  SetLength(Result, Length(aArr1) + Length(aArr2));
  if {$IFDEF DELPHIX_BERLIN_UP}IsManagedType(T){$ELSE}True{$ENDIF} then
  begin
    Idx := 0;
    for I := Low(aArr1) to High(aArr1) do
    begin
      Result[Idx] := aArr1[I];
      Inc(Idx);
    end;
    for I := Low(aArr1) to High(aArr1) do
    begin
      Result[Idx] := aArr1[I];
      Inc(Idx);
    end;
  end
  else
  begin
    Move(aArr1[Low(aArr1)], Result[0], SizeOf(T) * Length(aArr1));
    Move(aArr2[Low(aArr2)], Result[Length(aArr1)], SizeOf(T) * Length(aArr2));
  end;
end;

class function TArrayUtils.GetEmptyArray<T>(aLen: Integer; const aDefValue: T): TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, aLen);
  for I := 0 to aLen - 1 do
    Result[I] := aDefValue
end;

class function TArrayUtils.GetEmptyIntegerArray(aLen, aStartValue, aStep: Integer): TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, aLen);
  for I := 0 to aLen - 1 do
    Result[I] := aStartValue + I * aStep;
end;

class function TArrayUtils.AsArray<T>(aEnumerable: IEnumerable<T>): TArray<T>;
var
  Item: T;
  Cnt: Integer;
begin
  SetLength(Result, 10);
  Cnt := 0;
  for Item in aEnumerable do
  begin
    if Length(Result) <= Cnt then
      SetLength(Result, Cnt * 2 + 1);
    Result[Cnt] := Item;
    Inc(Cnt);
  end;
  SetLength(Result, Cnt);
end;

class function TArrayUtils.AsArray<T>(const aArr: array of T): TArray<T>;
var
  I, Cnt: Integer;
begin
  SetLength(Result, Length(aArr));
  Cnt := 0;
  for I := Low(aArr) to High(aArr) do
  begin
    Result[Cnt] := aArr[I];
    Inc(Cnt);
  end;
end;

class function TArrayUtils.EqualItemsCount<T>(const aArr1, aArr2: array of T): Integer;
var
  I, J, Len1, Len2: Integer;
  Used: TArray<Boolean>;
  Comparer: IEqualityComparer<T>;
begin
  Result := 0;
  Len1 := Length(aArr1);
  Len2 := Length(aArr2);
  if (Len1 = 0) or (Len2 = 0) then
    Exit;

  Used := GetEmptyArray<Boolean>(Len2, False);
  Comparer := TEqualityComparer<T>.Default;
  for I := 0 to Len1 - 1 do
    for J := 0 to Len2 - 1 do
      if not Used[J] and Comparer.Equals(aArr1[I], aArr2[J]) then
      begin
        Inc(Result);
        Used[J] := True;
        break;
      end;
end;

end.
