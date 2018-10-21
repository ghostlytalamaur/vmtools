unit tests_csplg_parser;
interface
implementation

uses
  TestFramework, regularexpressions, strutils, csplg_params, sysutils;

type
  TCodeSearchParserTests = class(TTestCase)
  private
    procedure DoTestMatch(const aRegEx, aText: string; aShouldMatch: Boolean);
  published
    procedure TestREGEX_ClassInterface;
    procedure TestREGEX_ClassInterfaceSuccessor;

    procedure TestMatchClassInterface;
    procedure TestMatchTypeConstDefinition;
    procedure TestMatchFunctionType;
    procedure TestMatchArrayConst;
    procedure TestMatchFunction;
    procedure TestMatchVariable;
    procedure TestMatchAssignment;
  end;

{ TCodeSearchParserTests }

procedure TCodeSearchParserTests.DoTestMatch(const aRegEx, aText: string; aShouldMatch: Boolean);
begin
  CheckEquals(aShouldMatch, TRegEx.IsMatch(aText, aRegEx, [roIgnoreCase]),
      'RegEx ' + aRegEx + ' should ' + IfThen(aShouldMatch, '', 'not') + ' match text: ' + aText);
end;

procedure TCodeSearchParserTests.TestREGEX_ClassInterface;
begin
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = class;', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = class', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = class end;', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = class end;;', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = class(TParentObject)', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject2 = class(TParentObject)', False);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject2 = class(TObject)', False);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = interface;', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = interface(IUnknown); ', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = interface(IUnknown)', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = interface(IUnknown) [''''{D5E327AA-36D2-4690-923F-F622ADD1F803}'''']', True);
  DoTestMatch(Format(REGEX_ClassInterface, ['TObject']), 'TObject = interface(IUnknown)[''''{D5E327AA-36D2-4690-923F-F622ADD1F803}'''']', True);
end;

procedure TCodeSearchParserTests.TestREGEX_ClassInterfaceSuccessor;
begin
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'TSomeObject2 = class(TObject);', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'TSomeObject2 = class(TObject)', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'TObj = class(TObject) end;', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'TSome = class(TObject) end;;', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'TObj = class   (TObject);', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'IInt  = interface(TObject);', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'I1Int = interface(TObject); ', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'I2Int = interface  (TObject)', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'IInt3 = interface(TObject) [''''{D5E327AA-36D2-4690-923F-F622ADD1F803}'''']', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['TObject']), 'IInt1 = interface(TObject)[''''{D5E327AA-36D2-4690-923F-F622ADD1F803}'''']', True);
  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['ISomeInt']), 'TSomeObj = class(TInterfacedObject, IUnknown, ISome3Int, ISomeInt)', True);
//  DoTestMatch(Format(REGEX_ClassInterfaceSuccessor, ['ISomeInt']), 'TSomeObj = class(TInterfacedObject, IUnknown, ISomeInt3)', False);

end;

procedure TCodeSearchParserTests.TestMatchClassInterface;
begin
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TObject = class;', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TObject<T> =class', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TList<T>=class(TObject)', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TObjectList<T: class> = class(TObject)', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TDictionary<K, V> = class(TObject)', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'IEnumerator  = interface', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'IEnumerator<T>  = interface', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TEnumerator= record', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TEnumerator= packed record', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TEnumerator<T> = record', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TEnumerator<T> = packed record', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TEnumerato12 = class', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'TSomeClass_2= class', True);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'type TSomeClass_2= class', True);

  DoTestMatch(RegEx2_ClassInterfaceRecord, 'IEnumerator<T>  = nterface', False);
  DoTestMatch(RegEx2_ClassInterfaceRecord, 'IEnumerator  = nterface', False);
  DoTestMatch(RegEx2_ClassInterfaceRecord, '', False);
end;

procedure TCodeSearchParserTests.TestMatchTypeConstDefinition;
begin
  DoTestMatch(RegEx2_TypeConstDefinition, 'cstSomeConst = 100', True);
  DoTestMatch(RegEx2_TypeConstDefinition, 'const cstSomeConst = 100', True);
  DoTestMatch(RegEx2_TypeConstDefinition, 'cst_Some_const = ''1000''', True);
  DoTestMatch(RegEx2_TypeConstDefinition, 'TSomeEnum = (seItem1, seItem2);', True);
  DoTestMatch(RegEx2_TypeConstDefinition, 'TSomeArray = array [0..3] of Boolean;', True);
  DoTestMatch(RegEx2_TypeConstDefinition, 'type Int_3Number = Integer', True);

  DoTestMatch(RegEx2_TypeConstDefinition, 'if Result <> nil then', False);
  DoTestMatch(RegEx2_TypeConstDefinition, 'if (Result <> nil) then', False);
  DoTestMatch(RegEx2_TypeConstDefinition, '', False);
end;

procedure TCodeSearchParserTests.TestMatchFunctionType;
begin
  DoTestMatch(RegEx2_FunctionType, 'TOnDisposeValue<T> = reference to function(var Value: T): Boolean;', True);
  DoTestMatch(RegEx2_FunctionType, 'TOnDisposeValue<T> = reference to procedure (Value: T)', True);
  DoTestMatch(RegEx2_FunctionType, 'TOnDisposeValue = reference to procedure (var Value: TObject)', True);
  DoTestMatch(RegEx2_FunctionType, 'TSomeCallback = procedure of Object', True);
  DoTestMatch(RegEx2_FunctionType, 'TGlobalFunction = function: Boolean;', True);
  DoTestMatch(RegEx2_FunctionType, 'TGlobalProcedure=procedure;', True);
  DoTestMatch(RegEx2_FunctionType, 'TGlobalParamProc =procedure(aParam: TParam);', True);
  DoTestMatch(RegEx2_FunctionType, '', False);
end;

procedure TCodeSearchParserTests.TestMatchArrayConst;
begin
  DoTestMatch(RegEx2_ArrayConst, 'cstSomeArray: array[Boolean] of Integer=(', True);
  DoTestMatch(RegEx2_ArrayConst, 'cstSomeArray2:array [TSome_Enum3] of Single = (', True);
  DoTestMatch(RegEx2_ArrayConst, 'const cstSomeArray2:array [TSome_Enum3] of Single = (', True);
  DoTestMatch(RegEx2_ArrayConst, 'const cstSomeArray2:array [1..2] of Single = (', True);
  DoTestMatch(RegEx2_ArrayConst, 'cstMatrix: array [0..0, 0..0] of Integer = ((1));', True);

  DoTestMatch(RegEx2_ArrayConst, 'cstSomeArray2:array of Single', False);
  DoTestMatch(RegEx2_ArrayConst, '', False);
end;

procedure TCodeSearchParserTests.TestMatchFunction;
begin
  DoTestMatch(RegEx2_Function, 'function Some_Function3: Boolean;', True);
  DoTestMatch(RegEx2_Function, 'function SomeFunction2(aParam: TParam): TResult;', True);
  DoTestMatch(RegEx2_Function, 'procedure SomeProc;', True);
  DoTestMatch(RegEx2_Function, 'procedure SomeProc2(P1, P2: TParam);', True);
  DoTestMatch(RegEx2_Function, '    class function AsArray<T>(aEnumerable: IEnumerable<T>): TArray<T>; overload; static;', True);
  DoTestMatch(RegEx2_Function, 'class function AsArray<T>(const aArr: array of T): TArray<T>; overload; static;', True);
  DoTestMatch(RegEx2_Function, 'procedure TCodeSearchParserTests.TestMatchFunction;', True);
  DoTestMatch(RegEx2_Function, 'class function TArrayUtils.AsArray<T>(const aArr: array of T): TArray<T>;', True);

  DoTestMatch(RegEx2_Function, 'constructor Create(aOwner: TComponent);', True);
  DoTestMatch(RegEx2_Function, 'destructor Destroy; override;', True);
  DoTestMatch(RegEx2_Function, 'constructor TBaseForm.Create(aOwner: TComponent);', True);
  DoTestMatch(RegEx2_Function, 'destructor TSearchInfo.Destroy;', True);

  DoTestMatch(RegEx2_Function, 'functionSomeFunction FunctionName;', False);
  DoTestMatch(RegEx2_Function, '', False);
end;

procedure TCodeSearchParserTests.TestMatchVariable;
begin
  DoTestMatch(RegEx2_Variable, ' SomeVar: TObject;', True);
  DoTestMatch(RegEx2_Variable, 'List: TList<Integer>;', True);
  DoTestMatch(RegEx2_Variable, 'var SomeVar_2, Some_Var3: Integer;', True);
  DoTestMatch(RegEx2_Variable, 'V2: Single;', True);
  DoTestMatch(RegEx2_Variable, 'V1, V2: Single;', True);
  DoTestMatch(RegEx2_Variable, 'V1 , V2: Single', True);
  DoTestMatch(RegEx2_Function, '', False);
end;

procedure TCodeSearchParserTests.TestMatchAssignment;
begin
  DoTestMatch(RegEx2_Assignment, ' ObjVar1:= TObject.Create(', True);
  DoTestMatch(RegEx2_Assignment, 'NumVar2 := 5;', True);
  DoTestMatch(RegEx2_Assignment, 'str_Var3  :=  ''string''', True);
  DoTestMatch(RegEx2_Assignment, 'realVar:= 5.99;', True);
  DoTestMatch(RegEx2_Assignment, 'hexVar := $0001;', True);
  DoTestMatch(RegEx2_Assignment, 'bool_Var := False', True);
  DoTestMatch(RegEx2_Assignment, 'setVar  := [siFirst, siSecond];', True);
  DoTestMatch(RegEx2_Assignment, '', False);
end;

initialization
  TestFramework.RegisterTest('codesearch.parser', TCodeSearchParserTests.Suite);

end.

