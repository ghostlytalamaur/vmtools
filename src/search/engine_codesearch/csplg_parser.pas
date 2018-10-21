unit csplg_parser;

interface

uses
  csplg_types, csplg_params, generics.collections, regularexpressionscore;

type
  TSearchItemInfo = class
    FilePath: string;
    LineNum: Integer;
    Text: string;
  end;

  TRatingCalculator = class(TObject)
  strict private type
    TRatingRegExHolder = class
      RegEx: TPerlRegEx;
      Rating: Integer;

      destructor Destroy; override;
    end;
  strict private
    FParams: TRatingCalculatorParams;
    FExpressions: TList<TRatingRegExHolder>;

    procedure SetQuery(const aQuery: string);
  public
    constructor Create(const aQuery: string);
    destructor Destroy; override;
    function Calculate(const aLine: string): Integer;
  end;

  TCodeSearchParser = class
  private
    FQuery: string;
    FRatingCalculator: TRatingCalculator;
  public
    constructor Create(const aQuery: string);
    destructor Destroy; override;

    function Parse(aInfo: TSearchItemInfo): TSearchItem;
  end;

implementation

uses
  windows, sysutils, classes, math, diagnostics, strutils, str_utils;

function CreateRegEx(aRegEx: string): TPerlRegEx;
begin
  try
    Result := TPerlRegEx.Create;
    Result.Options := Result.Options + [preCaseLess];
    Result.State := [preNotEmpty];
    Result.RegEx := aRegEx;
    Result.Compile;
  except
    FreeAndNil(Result);
  end;
end;

function CopySafe(const aStr: string; aFirstIdx, aLastIdx: Integer; out DestStr: string): Boolean;
var
  Cnt, Len: Integer;
begin
  DestStr := '';
  Result := False;
  Len := Length(aStr);
  Cnt := aLastIdx - aFirstIdx + 1;
  if (Len <= 0) or (Cnt <= 0) or (aFirstIdx < 1) or (aFirstIdx > Len) or
      (aLastIdx < 1) or (aLastIdx > Len) or ((aFirstIdx + Cnt - 1) > Len) then
    Exit;

  DestStr := Copy(aStr, aFirstIdx, Cnt);
  Result := True;
end;

function TCodeSearchParser.Parse(aInfo: TSearchItemInfo): TSearchItem;
var
  Rating: Integer;
begin
  Result := nil;
  if aInfo = nil then
    Exit;

  Rating := FRatingCalculator.Calculate(TStrUtils.RemoveEscSeq(aInfo.Text));
  Result := TSearchItem.Create(aInfo.FilePath, aInfo.Text, aInfo.LineNum, Rating);
  Result.RawText := aInfo.Text;
end;

constructor TCodeSearchParser.Create(const aQuery: string);
begin
  inherited Create;
  FQuery := aQuery;
  FRatingCalculator := TRatingCalculator.Create(FQuery);
end;

destructor TCodeSearchParser.Destroy;
begin
  FreeAndNil(FRatingCalculator);
  inherited;
end;

{ TRatingCalculator }

function TRatingCalculator.Calculate(const aLine: string): Integer;
var
  I: Integer;
  H: TRatingRegExHolder;
  SW: TStopwatch;
  IsOk: Boolean;
begin
  Result := 0;
  for I := 0 to FExpressions.Count - 1 do
  begin
    try
      H := FExpressions[I];
      H.RegEx.Subject := aLine;
      SW := TStopwatch.StartNew;
      IsOk := H.RegEx.Match;
      if IsOk then
      begin
        Result := H.Rating;
        Exit;
      end;
    except
    end;
  end;
end;

constructor TRatingCalculator.Create(const aQuery: string);
begin
  inherited Create;
  FParams := TRatingCalculatorParams.Create;
  FParams.ReadParams;
  SetQuery(aQuery);
end;

destructor TRatingCalculator.Destroy;
begin
  FParams.WriteParams;
  FreeAndNil(FParams);
  FreeAndNil(FExpressions);
  inherited;
end;

procedure TRatingCalculator.SetQuery(const aQuery: string);
var
  I: Integer;
  R: TPerlRegEx;
  Holder: TRatingRegExHolder;
begin
  FreeAndNil(FExpressions);
  FExpressions := TObjectList<TRatingRegExHolder>.Create;
  for I := 0 to FParams.PatternsCount - 1 do
  begin
    R := CreateRegEx(Format(FParams.Pattern[I].RegExTemplate, [aQuery]));
    if R = nil then
      Continue;

    Holder := TRatingRegExHolder.Create;
    Holder.RegEx := R;
    Holder.Rating := FParams.Pattern[I].Rating;
    FExpressions.Add(Holder);
  end;
end;

{ TRatingCalculator.TRatingRegExHolder }

destructor TRatingCalculator.TRatingRegExHolder.Destroy;
begin
  FreeAndNil(RegEx);
  inherited;
end;

end.
