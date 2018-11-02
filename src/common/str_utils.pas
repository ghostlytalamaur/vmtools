unit str_utils;

{$I cond_define.inc}

interface

uses
  sysutils, collections.common;

type
  TStrUtils = class
  public
    class function Join(const aStrings: array of string; const aDelim: string): string;
    class function PosI(const SubStr, Str: string): Integer;
    class function StringInArray(const S: string; const SArray: array of string): Boolean;

    class function FileMatchesExtensions(const FileName, FileExtensions: string): Boolean; overload;
    class function FileMatchesExtensions(const FileName: string; FileExtensions: array of string): Boolean; overload;

    class function Words(const aStr: string; aWordDelims: TSysCharSet): IEnumerable<string>;
    class function NGrams(const S: string; aTokenLen: Integer): TArray<string>;
    class function NGramsSimilarity(const aNGrams1, aNGRams2: array of String): Single; overload;
    class function NGramsSimilarity(const S1, S2: string; aNGramsLen: Integer): Single; overload;
    class function Tanimoto(const S1, S2: string; aTokenLen: Integer): Single;
    class function Normalize(const aStr: string): string;
    class function LevenshteinDistance(const S1, S2: string): Integer;
    class function DeleteChars(const aStr: string; aChars: TSysCharSet): string;
    class function MatchesMasks(const aStr: string; const aMasks: array of string): Boolean;
    class function HasChars(const aStr: string; aCharSet: TSysCharSet): Boolean;
    class function FindEscPos(const aText: string; aOffset: Integer; out EscPos: Integer; out EscSeq: string): Boolean;
    class function RemoveEscSeq(const aText: string): string;
  end;

implementation

uses
  math, strutils, masks, collections.array_utils, collections.enumerators;

type
  TStringWordsEnumerable = class(TEnumerableImpl<string>)
  private type
    TWordEnumerator = class(TEnumeratorImpl<string>)
    private
      FSource: string;
      FSourceLen: Integer;
      FDelims: TSysCharSet;
      FCurrent: string;
      FIter: Integer;
    protected
      function DoGetCurrent: string; override;
      function DoMoveNext: Boolean; override;
    public
      constructor Create(const aStr: string; aWordDelims: TSysCharSet);
    end;
  private
    FSource: string;
    FDelims: TSysCharSet;
  protected
    function DoGetEnumerator: IEnumerator<string>; override;
  public
    constructor Create(const aStr: string; aWordDelims: TSysCharSet);
  end;

{ TStrUtils }

class function TStrUtils.FileMatchesExtensions(const FileName, FileExtensions: string): Boolean;
begin
  Result := (PosI(ExtractFileExt(FileName), FileExtensions) <> 0);
end;

class function TStrUtils.DeleteChars(const aStr: string; aChars: TSysCharSet): string;
var
  Ch: Char;
  Cnt: Integer;
begin
  Cnt := 0;
  SetLength(Result, Length(aStr));
  for Ch in aStr do
    if not CharInSet(Ch, aChars) then
    begin
      Inc(Cnt);
      Result[Cnt] := Ch;
    end;

  SetLength(Result, Cnt);
end;

class function TStrUtils.MatchesMasks(const aStr: string; const aMasks: array of string): Boolean;
var
  Mask: string;
begin
  for Mask in aMasks do
  try
    if MatchesMask(aStr, Mask) then
      Exit(True);
  except
  end;
  Result := False;
end;

class function TStrUtils.FileMatchesExtensions(const FileName: string; FileExtensions: array of string): Boolean;
begin
  Result := StringInArray(ExtractFileExt(FileName), FileExtensions);
end;

class function TStrUtils.HasChars(const aStr: string; aCharSet: TSysCharSet): Boolean;
var
  Ch: Char;
begin
  for Ch in aStr do
    if CharInSet(Ch, aCharSet) then
      Exit(True);
  Result := False;
end;

class function TStrUtils.FindEscPos(const aText: string; aOffset: Integer;
    out EscPos: Integer; out EscSeq: string): Boolean;
var
  MPos: Integer;
begin
  Result := False;
  EscPos := PosEx(#$1B, aText, aOffset);
  if EscPos <= 0 then
    Exit;

  {#$1B + '[<n>m'}
  MPos := PosEx('m', aText, EscPos);
  if MPos <= 0 then
    Exit;

  EscSeq := Copy(aText, EscPos, MPos - EscPos + 1);
  Result := True;
end;

class function TStrUtils.RemoveEscSeq(const aText: string): string;
var
  EscOffset, EscPos: Integer;
  EscSeq: string;
begin
  Result := '';
  EscOffset := 1;
  while FindEscPos(aText, EscOffset, EscPos, EscSeq) do
  begin
    Result := Result + Copy(aText, EscOffset, EscPos - EscOffset);
    EscOffset := EscPos + Length(EscSeq);
  end;

  if Length(aText) >= EscOffset then
    Result := Result + Copy(aText, EscOffset, Length(aText) - EscOffset + 1);
end;

class function TStrUtils.Join(const aStrings: array of string; const aDelim: string): string;
var
  S: string;
  ResLen, DelimLen: Integer;
begin
  Result := '';
  if Length(aStrings) <= 0 then
    Exit;

  DelimLen := Length(aDelim);
  for S in aStrings do
  begin
    ResLen := Length(Result);
    if (ResLen > DelimLen) and (Copy(Result, ResLen - DelimLen + 1, DelimLen) <> aDelim) then
      Result := Result + aDelim;

    if (Length(S) > DelimLen) and (Copy(S, 1, DelimLen) = aDelim) then
      Result := Result + Copy(S, DelimLen + 1, Length(S) - DelimLen)
    else
      Result := Result + S;
  end;
end;

class function TStrUtils.PosI(const SubStr, Str: string): Integer;
begin
  Result := Pos(UpperCase(SubStr), UpperCase(Str));
end;

class function TStrUtils.StringInArray(const S: string; const SArray: array of string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(SArray) - 1 do
    if SameText(S, SArray[i]) then begin
      Result := True;
      Break;
    end;
end;

class function TStrUtils.Normalize(const aStr: string): string;
const
  cstCharsToNormalize = ['_', '.', ',', '/', '\', '?', #8, '*', '[', ']', '+', '=', '-', '<', '>', '~', '`'];
begin
  Result := DeleteChars(UpperCase(aStr), cstCharsToNormalize);
end;

class function TStrUtils.NGrams(const S: string; aTokenLen: Integer): TArray<string>;
var
  I, Cnt: Integer;
  wS: string;
begin
  aTokenLen := math.Max(1, aTokenLen);
  wS := S;
  Cnt := math.Max(0, Length(wS) - aTokenLen + 1);
  SetLength(Result, Cnt);
  for I := 0 to Cnt - 1 do
    Result[I] := Copy(wS, I + 1, aTokenLen);
end;

class function TStrUtils.NGramsSimilarity(const aNGrams1, aNGRams2: array of String): Single;
begin
  if (Length(aNGrams1) > 0) and (Length(aNGrams2) > 0) then
    Result := 2 * TArrayUtils.EqualItemsCount<string>(aNGrams1, aNGrams2) / (Length(aNGrams1) + Length(aNGrams2))
  else
    Result := 0;
end;

class function TStrUtils.NGramsSimilarity(const S1, S2: string; aNGramsLen: Integer): Single;
var
  NGrams1: TArray<string>;
begin
  NGrams1 := TStrUtils.NGrams(S1, aNGramsLen);
  if Length(NGrams1) > 0 then
    Result := NGramsSimilarity(NGrams1, NGrams(S2, aNGramsLen))
  else
    Result := 0;
end;

class function TStrUtils.Tanimoto(const S1, S2: string; aTokenLen: Integer): Single;
var
  TokensCnt1, TokensCnt2, Cnt, I: Integer;
  Token: string;
begin
  TokensCnt1 := Length(S1) - aTokenLen;
  TokensCnt2 := Length(S2) - aTokenLen;
  Cnt := 0;
  if TokensCnt2 > 0 then
    for I := 1 to Length(S1) - aTokenLen + 1 do
    begin
      Token := Copy(S1, I, aTokenLen);
      if Pos(Token, S2) > 0 then
        Inc(Cnt);
    end;

  if (TokensCnt1 + TokensCnt2 - Cnt) > 0 then
    Result := Cnt / (TokensCnt1 + TokensCnt2 - Cnt)
  else
    Result := 1;
end;

class function TStrUtils.LevenshteinDistance(const S1, S2: string): Integer;
var
  Matrix: array of array of Integer;
  Len1, Len2, Cost, I, J: Integer;
begin
  Len1 := Length(S1);
  Len2 := Length(S2);

  SetLength(Matrix, Len1 + 1, Len2 + 1);
  for I := 0 to Len1 do
  begin
    SetLength(Matrix[I], Len2);
    Matrix[I, 0] := I;
  end;

  for I := 0 to Len2 do
    Matrix[0, I] := I;

  for I := 1 to Len1 do
  begin
    for J := 1 to Len2 do
    begin
      if S1[I] = S2[J] then
        Cost := 0
      else
        Cost := 1;

      Matrix[I, J] := math.Min(Matrix[I-1, J] + 1, math.Min(Matrix[I, J-1] + 1, Matrix[I-1, J-1] + Cost));
    end;
  end;

  Result := Matrix[Len1, Len2];
end;

class function TStrUtils.Words(const aStr: string; aWordDelims: TSysCharSet): IEnumerable<string>;
begin
  Result := TStringWordsEnumerable.Create(aStr, aWordDelims);
end;

{ TStringWordsEnumerable }

constructor TStringWordsEnumerable.Create(const aStr: string; aWordDelims: TSysCharSet);
begin
  inherited Create;
  FSource := aStr;
  FDelims := aWordDelims;
end;

function TStringWordsEnumerable.DoGetEnumerator: IEnumerator<string>;
begin
  Result := TWordEnumerator.Create(FSource, FDelims);
end;

{ TStringWordsEnumerable.TWordEnumerator }

constructor TStringWordsEnumerable.TWordEnumerator.Create(const aStr: string; aWordDelims: TSysCharSet);
begin
  inherited Create;
  FSource := aStr;
  FDelims := aWordDelims;
  FSourceLen := Length(FSource);
  FIter := 1;
end;

function TStringWordsEnumerable.TWordEnumerator.DoGetCurrent: string;
begin
  Result := FCurrent;
end;

function TStringWordsEnumerable.TWordEnumerator.DoMoveNext: Boolean;
var
  J, K: Integer;
begin
  Result := False;
  FCurrent := '';
  if FSource = '' then
    Exit;

  while FIter <= FSourceLen do
  begin
    J := FIter;
    while CharInSet(FSource[J], FDelims) and (J <= FSourceLen) do Inc(J);

    K := J + 1;
    while not CharInSet(FSource[K], FDelims) and (K <= FSourceLen) do Inc(K);

    FCurrent := Copy(FSource, J, K - J);
    FIter := K + 1;
    if FCurrent <> '' then
      break;
  end;

  Result := FCurrent <> '';
end;

end.

