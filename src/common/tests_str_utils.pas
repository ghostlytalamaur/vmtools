unit tests_str_utils;
interface
implementation

uses
  testframework, str_utils, sysutils, classes;

type
  TStrUtilsTests = class(TTestCase)
  published
    procedure TestNGrams;
    procedure TestNGramsSimilarity;
  end;
{ TVMSymTests }


{ TStrUtilsTests }

procedure TStrUtilsTests.TestNGrams;

  procedure DoTest(const aStr: string; aTokenLen: Integer; const aExpected: array of string);
  var
    Actual: TArray<string>;
    I: Integer;
  begin
    Actual := TStrUtils.NGrams(aStr, aTokenLen);
    CheckEquals(Length(aExpected), Length(Actual), Format('Str: %s; TokenLen: %d', [aStr, aTokenLen]));
    for I := 0 to High(aExpected) do
    begin
      CheckEquals(aExpected[I], Actual[I], Format('Str: %s; TokenLen: %d', [aStr, aTokenLen]));
    end;
  end;

begin
  DoTest('Test', 3, ['Tes', 'est']);
  DoTest('Test', 2, ['Te', 'es', 'st']);
  DoTest('register', 2, ['re', 'eg', 'gi', 'is', 'st', 'te', 'er']);
  DoTest('register', 3, ['reg', 'egi', 'gis', 'ist', 'ste', 'ter']);
end;


procedure TStrUtilsTests.TestNGramsSimilarity;

  procedure DoTest(const S1, S2: string; aNGramsLen: Integer; aExpected: Single);
  var
    Coef: Single;
  begin
    Coef := TStrUtils.NGramsSimilarity(S1, S2, aNGramsLen);
    CheckEquals(aExpected, Coef, 0.001,
        Format('S1: %s; S2: %s; NGramsLen: %d;', [S1, S2, aNGramsLen]));
  end;

begin
  DoTest('ALEXANDRE', 'ALEKSANDER', 3, 0.266);
end;

initialization
  TestFramework.RegisterTest('common.str_utils', TStrUtilsTests.Suite);

end.
