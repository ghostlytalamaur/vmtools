unit csplg_params;

interface

uses
  base_params, inifiles;

const
  REGEX_ClassInterface                  = '^ *?(?:%s) *?= *?(?:class|interface) *?(?:\([\w\d., ]+\))? *?\;?';
  REGEX_ClassInterfaceSuccessor         = '(?:class|interface) *\(?[\w\d., ]*(?:%s)[\w\d., ]*\)';
  REGEX_ClassMethod                     = '(?:procedure|function|constructor|destructor) +(?:%s)\.[\w\d]+';
  REGEX_Record                          = '^ *(%s) += *(packed)? *record';
  REGEX_Set                             = '^ *(%s) += *(set *of *)?\([\w\d, ]+\)';
  REGEX_Enum                            = '^ *?(?:%s) +={1} *?\([\w\d, .]*\)?';
//  REGEX_Parameter                       = '(?:procedure|function|constructor|destructor) +([\w\d]+\.)?(?:%s) *[\(:;]';
//                                          '(procedure|function|constructor|destructor) +([\w\d]+\.)?%1 *[\(:;]'
  REGEX_FunctionType                    = '(?:%s) *?={1} *?(?:reference to)? *(?:procedure|function)';
  REGEX_CaseStatement                   = '^(?: *?[\w\d.'']+ *,)* *(?:%s) *(?:, *[\w\d.'']+)* *:{1}.*?$';
  REGEX_VariableType                    = ': *?(?:[\w\d]+\.)*(?:%s)(?:$|;| )';
  REGEX_Variable                        = '^ *(?:[\w\d ,])*?(?:%s)(?:[\d\w ,])*?:{1}(?:[\d\w. ])+;';

const
  cstIdentifier                = '([\w]{1}[\w\d]*)';
  cstGenericIdentifier         = cstIdentifier + '(<[\w\d,:\s]*>)*';
  RegEx2_ClassInterfaceRecord  = '^\s*(type\s)?\s*' + cstGenericIdentifier + '\s*=\s*((class|interface)|((packed\s+)?record))';
  RegEx2_TypeConstDefinition   = '^\s*(type|const\s)?\s*' + cstIdentifier + '\s*=';
  RegEx2_FunctionType          = '^\s*' + cstGenericIdentifier + '\s*=\s*(reference\s+to\s+)?(procedure|function)';
  RegEx2_ArrayConst            = '^\s*(type|const\s)?\s*' + cstIdentifier + '\s*:\s*array\s*(\[[\s\w\d\.,]*\])\s+of';
  RegEx2_Function              = '^\s*(class\s)?(constructor|destructor|function|procedure)\s+' + cstIdentifier;
  RegEx2_Variable              = '^\s*(?:var\s)?\s*' + cstIdentifier + '(\s*,{1}\s*(?1))?\s*:\s*' + cstGenericIdentifier;
  RegEx2_Assignment            = '^\s*' + cstIdentifier + '\s*:=';


type
  TRatingPattern = record
    Rating: Integer;
    RegExTemplate: string;
  end;

  TRatingCalculatorParams = class(TBaseParams)
  private
    FPatterns: TArray<TRatingPattern>;
    function GetPattern(aIndex: Integer): TRatingPattern;
    function GetPatternsCount: Integer;
  protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    procedure SetDefault; override;

    property PatternsCount: Integer read GetPatternsCount;
    property Pattern[aIndex: Integer]: TRatingPattern read GetPattern;
  end;

implementation

uses
  sysutils, generics.defaults, generics.collections;

{ TRatingCalculatorParams }

procedure TRatingCalculatorParams.DoReadParams(aIni: TCustomIniFile);
var
  I, Count: Integer;
  Comparer: IComparer<TRatingPattern>;
begin
  inherited;
  Count := aIni.ReadInteger('RatingCalculatorParams', 'TemplatesCount', 0);
  if Count = 0 then
    Exit;

  SetLength(FPatterns, Count);
  for I := 0 to Count - 1 do
  begin
    FPatterns[I].Rating := aIni.ReadInteger('RatingCalculatorParams', 'Rating' + IntToStr(I), 0);
    FPatterns[I].RegExTemplate := aIni.ReadString('RatingCalculatorParams', 'RegExTemplate' + IntToStr(I), '');
  end;

  Comparer := TDelegatedComparer<TRatingPattern>.Create(
    function (const Left, Right: TRatingPattern): Integer
    begin
      Result := Right.Rating - Left.Rating;
    end);
  TArray.Sort<TRatingPattern>(FPatterns, Comparer);
  Comparer := nil;
end;

procedure TRatingCalculatorParams.DoWriteParams(aIni: TCustomIniFile);
var
  I: Integer;
begin
  inherited;
  aIni.WriteInteger('RatingCalculatorParams', 'TemplatesCount', Length(FPatterns));
  for I := 0 to High(FPatterns) do
  begin
    aIni.WriteInteger('RatingCalculatorParams', 'Rating' + IntToStr(I), FPatterns[I].Rating);
    aIni.WriteString('RatingCalculatorParams', 'RegExTemplate' + IntToStr(I), FPatterns[I].RegExTemplate);
  end;
end;

function TRatingCalculatorParams.GetPattern(aIndex: Integer): TRatingPattern;
begin
  if (aIndex >= 0) and (aIndex < Length(FPatterns)) then
    Result := FPatterns[aIndex]
  else
  begin
    Result.Rating := 0;
    Result.RegExTemplate := '';
  end;
end;

function TRatingCalculatorParams.GetPatternsCount: Integer;
begin
  Result := Length(FPatterns);
end;

procedure TRatingCalculatorParams.SetDefault;
const
  ExprsOld: array[0..9] of record
    Rating: integer;
    Template: string;
  end = (
    (Rating: 110; Template: REGEX_ClassInterface),
    (Rating: 106; Template: REGEX_ClassInterfaceSuccessor),
    (Rating: 105; Template: REGEX_ClassMethod),
    (Rating: 100; Template: REGEX_Record),
    (Rating:  90; Template: REGEX_Set),
//    (Rating:  70; Template: REGEX_Parameter),
    (Rating:  60; Template: REGEX_FunctionType),
    (Rating:  50; Template: REGEX_Enum),
    (Rating:  40; Template: REGEX_CaseStatement),
    (Rating:  30; Template: REGEX_VariableType),
    (Rating:  20; Template: REGEX_Variable)
  );
  Exprs: array[0..5] of record
    Rating: integer;
    Template: string;
  end = (
    (Rating: 5; Template: RegEx2_ClassInterfaceRecord),
    (Rating: 4; Template: RegEx2_TypeConstDefinition),
    (Rating: 4; Template: RegEx2_ArrayConst),
    (Rating: 3; Template: RegEx2_Function),
    (Rating: 2; Template: RegEx2_Variable),
    (Rating: 1; Template: RegEx2_Assignment)
  );

{
    (Rating: 5; Template: '^ *%s += *(class|interface) *\([\w\d., ]+\)'),                        //x = class
    (Rating: 5; Template: '^ *%s += *(packed)? *record'),                                        //x = record
    (Rating: 5; Template: '^ *%s += *(set *of *)?\([\w\d, ]+\)'),                                //x = enum
    (Rating: 5; Template: '(procedure|function|constructor|destructor) +([\w\d]+\.)?%1 *[\(:;]'),//function x
    (Rating: 4; Template: '^ *%s += *(procedure|function)'),                                     //x = function(a, b: integer): string
    (Rating: 4; Template: '^ *%s *=( *\$?([\w\d]+\.?|''[^'']*''|#\d+) *\+?)+ *;? *$'),           //x = acdsys.y + $500
    (Rating: 3; Template: '(class|interface) *\([\w\d., ]*%s[\w\d., ]*\)'),                      //y = class(x)
    (Rating: 2; Template: ': *([\w\d]+\.)*%s($|;| )'),                                           //var y: x;
    (Rating: 2; Template: '^( *[\w\d.'']+ *,)* *%s *(, *[\w\d.'']+)* *: *$'),                    //case x, y:
    (Rating: 1; Template: '^ *%s *= *\$?[\d]+ *;'),                                              //x = $500 (dirty)
    (Rating: 1; Template: '^ *%s *: *[\d\w.]+ *;')                                               //x: type
   );
   }
var
  I: Integer;
begin
  inherited;
  SetLength(FPatterns, Length(Exprs));
  for I := 0 to High(Exprs) do
  begin
    FPatterns[I].Rating := Exprs[I].Rating;
    FPatterns[I].RegExTemplate := Exprs[I].Template;
  end;
end;


end.