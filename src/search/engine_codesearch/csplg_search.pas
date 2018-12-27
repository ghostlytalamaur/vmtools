unit csplg_search;

interface

uses
  generics.collections, csplg_query_params, regularexpressions, base_params,
  inifiles, sysutils, progress,
  csplg_types, csplg_parser, classes,
  otlcollections, otlparallel, otlsync, otltaskcontrol, JclSynch, otlcommon, otltask;

type
  TOutputLineCallback = reference to procedure(const aLine: string);
  TJCLAppExecutor = class(TObject)
  private
    FAppName: string;
    FCommandLine: string;
    FCancellationToken: IOmniCancellationToken;

    FAbort: Boolean;
    FCallback: TOutputLineCallback;
    FErrorCallback: TOutputLineCallback;

    procedure OnLineCallback(const Text: string);
    procedure OnErrorCallback(const Text: string);
  public
    constructor Create(const aAppName, aCommandLine: string; aCancellationToken: IOmniCancellationToken);
    destructor Destroy; override;

    function Execute(OnNewOutputLine, OnNewErrorLine: TOutputLineCallback; const aPriority: string): Boolean;
  end;

  TCodeSearchEngineParams = class(TBaseParams)
  private const
    cst_Reg_Section = 'TCodeSearchEngineParams';
    cst_Reg_ProcessPriority = 'CSearchProcessPriority';
    cst_Reg_ParseTaskCount = 'ParseTaskCount';
    cst_Reg_UpdateProgressEachItems = 'UpdateProgressEachItems';
  private
    FCSearchProcessPriority: string;
    FParseTaskCount: Integer;
    FUpdateProgressEachItems: Integer;
  protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    procedure SetDefault; override;

    { TJclProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime, ppBelowNormal, ppAboveNormal) }
    property CSearchProcessPriority: string read FCSearchProcessPriority;
    property ParseTaskCount: Integer read FParseTaskCount;
    property UpdateProgressEachItems: Integer read FUpdateProgressEachItems;
  end;

  TCodeSearchEngineError = (csee_Successful, csee_Unknown, csee_InvalidQuery, csee_NoIndex, csee_Cancelled,
      csee_NothingFound);
  TCodeSearchEngine = class(TObject)
  private
    FProgress: TBaseProgress;
    FOwnProgress: Boolean;
    FParams: TCodeSearchEngineParams;
    FValidPaths: TStringList;

    function SearchIndexFile(const aIndexSearchPath: array of string): string;
    function GetProcessExecutor(aParams: TCodeSearchQueryParams; const aIndexFile: string;
        aCancellationToken: IOmniCancellationToken): TJCLAppExecutor;

    function DoSearchMultiThread2(aParams: TCodeSearchQueryParams; aIndexFile: string;
        aCallbacks: TSearchEngineCallbacks): TCodeSearchEngineError;

    function CreateSearchExecutor(aParams: TCodeSearchQueryParams; aIndexFile: string;
        aCallbacks: TSearchEngineCallbacks): TPipelineStageDelegateEx;
    function CreateItemAnalyzer(aParams: TCodeSearchQueryParams): TPipelineStageDelegateEx;
  public
    constructor Create(aProgress: TBaseProgress; aValidPaths: TStringList);
    destructor Destroy; override;

    // aQuery - regexp text to search
    // aCurrentFile - file, that will be used for search index
    function Search(aParams: TCodeSearchQueryParams; const aIndexSearchPath: array of string;
        aCallbacks: TSearchEngineCallbacks): TCodeSearchEngineError;
  end;

implementation

uses
  windows, jclsysutils, generics.defaults, math,
  str_utils, strutils, collections.sets;

type
  TParserState = class
    Parser: TCodeSearchParser;

    constructor Create(aParser: TCodeSearchParser);
    destructor Destroy; override;
  end;

  TCodeSearchMultilineParser = class
  private
    FFoundTextBuffer: TStringList;
    FLineNum: Integer;
    FFilePath: string;
    FResultsQueue: IOmniBlockingCollection;

    procedure FlushBuffer;
    function ExtractFileInfo(const aLine: string;
        out FilePath, FoundText: string; out LineNum: Integer): Boolean;
  public
    constructor Create(aResQueue: IOmniBlockingCollection);
    destructor Destroy; override;
    procedure AppendLine(const aLine: string);
  end;

function JclProcessPriorityToString(aPriority: TJclProcessPriority): string;
const
  cstPriorityStrings: array [TJclProcessPriority] of string = (
    'ppIdle', 'ppNormal', 'ppHigh', 'ppRealTime', 'ppBelowNormal', 'ppAboveNormal'
  );
begin
  Result := cstPriorityStrings[aPriority];
end;

function JclProcessPriorityFromString(aPriorityStr: string): TJclProcessPriority;
var
  P: TJclProcessPriority;
begin
  for P := Low(TJclProcessPriority) to High(TJclProcessPriority) do
    if SameText(JclProcessPriorityToString(P), aPriorityStr) then
      Exit(P);

  Result := ppNormal;
end;

{ TCodeSearchEngine }

constructor TCodeSearchEngine.Create(aProgress: TBaseProgress; aValidPaths: TStringList);
begin
  inherited Create;
  FValidPaths := aValidPaths;
  FParams := TCodeSearchEngineParams.Create;
  FParams.ReadParams;
  FProgress := aProgress;
  if FProgress = nil then
  begin
    FProgress := TProgress.Create(nil);
    FOwnProgress := True;
  end;
end;

destructor TCodeSearchEngine.Destroy;
begin
  FParams.WriteParams;
  if FOwnProgress then
    FreeAndNil(FProgress);
  FreeAndNil(FParams);
  inherited;
end;

function TCodeSearchEngine.GetProcessExecutor(aParams: TCodeSearchQueryParams; const aIndexFile: string;
    aCancellationToken: IOmniCancellationToken): TJCLAppExecutor;
var
  CmdLine: string;
begin
  CmdLine := Format('-n -m %d -M %d', [aParams.MaxResults, aParams.MaxHitsPerFile]);
  if aIndexFile <> '' then
    CmdLine := CmdLine + ' -indexpath "' + aIndexFile + '"';
  if Trim(aParams.FileRegExp) <> '' then
    CmdLine := CmdLine + ' -f ' + '"' + aParams.FileRegExp + '"';

  if (FValidPaths <> nil) and (FValidPaths.Count > 0) then
    CmdLine := CmdLine + ' -ignorepathscase -filepaths "' + TStrUtils.Join(FValidPaths.ToStringArray, '|') + '"';

  if aParams.IgnoreCase then
    CmdLine := CmdLine + ' -i';
  if aParams.AddLines > 0 then
    CmdLine := CmdLine + ' -addlines ' + IntToStr(aParams.AddLines);
  if aParams.UseRe2 then
    CmdLine := CmdLine + ' -re2';
  CmdLine := CmdLine + ' "' + aParams.QueryText + '"';
  Result := TJCLAppExecutor.Create('csearch', CmdLine, aCancellationToken);
end;

{ TCodeSearchEngine }

function TCodeSearchEngine.Search(aParams: TCodeSearchQueryParams; const aIndexSearchPath: array of string;
    aCallbacks: TSearchEngineCallbacks): TCodeSearchEngineError;
var
  IndexFile: string;
begin
  if aCallbacks = nil then
  begin
    Result := csee_Unknown;
    Exit;
  end;

  IndexFile := SearchIndexFile(aIndexSearchPath);
  if IndexFile = '' then
  begin
    Result := csee_NoIndex;
    Exit;
  end;

  Result := DoSearchMultiThread2(aParams, IndexFile, aCallbacks)
end;

function TCodeSearchEngine.CreateSearchExecutor(aParams: TCodeSearchQueryParams;
    aIndexFile: string; aCallbacks: TSearchEngineCallbacks): TPipelineStageDelegateEx;
begin
  Result := procedure (const input, output: IOmniBlockingCollection; const task: IOmniTask)
  var
    Executor: TJCLAppExecutor;
    MultilineParser: TCodeSearchMultilineParser;
  begin
    MultilineParser := nil;
    Executor := GetProcessExecutor(aParams, aIndexFile, Task.CancellationToken);
    try
      MultilineParser := TCodeSearchMultilineParser.Create(output);
      Executor.Execute(
        procedure (const aLine: string)
        begin
          MultilineParser.AppendLine(aLine)
        end,
        procedure (const aLine: string)
        begin
          aCallbacks.Error(aLine);
        end, FParams.CSearchProcessPriority);

      MultilineParser.FlushBuffer;
    finally
      FreeAndNil(Executor);
      FreeAndNil(MultilineParser);
    end;
  end;
end;

function TCodeSearchEngine.CreateItemAnalyzer(aParams: TCodeSearchQueryParams): TPipelineStageDelegateEx;
begin
  Result := procedure (const input, output: IOmniBlockingCollection; const task: IOmniTask)
  var
    Item: TSearchItem;
    Parser: TCodeSearchParser;
    Info: TSearchItemInfo;
    OV, ResOv: TOmniValue;
  begin
    Parser := TCodeSearchParser.Create(aParams.QueryText);
    try
      while Input.Take(OV) do
      begin
        Info := OV.AsOwnedObject as TSearchItemInfo;
        if Info = nil then
          Continue;

        Item := Parser.Parse(Info);
        if Item = nil then
          Exit;

        ResOv.AsOwnedObject := Item;
        output.TryAdd(ResOv);
      end;
    finally
      FreeAndNil(Parser);
    end;
  end;
end;

function TCodeSearchEngine.DoSearchMultiThread2(aParams: TCodeSearchQueryParams; aIndexFile: string;
    aCallbacks: TSearchEngineCallbacks): TCodeSearchEngineError;
var
  Pipeline: IOmniPipeline;
  OmniVal: TOmniValue;
  ResultsCount: Integer;
  TotalFiles: ISet<string>;
begin
  Result := csee_Unknown;

  Pipeline := nil;
  aCallbacks.SearchStarted;
  try
    TotalFiles := THashSet<string>.Create;
    Pipeline := Parallel.Pipeline
        .Stage(CreateSearchExecutor(aParams, aIndexFile, aCallbacks)).NumTasks(1)
        .Stage(CreateItemAnalyzer(aParams)).NumTasks(2)
        .Run;

    ResultsCount := 0;
    while not Pipeline.Output.IsFinalized do
    begin
      if Pipeline.Output.TryTake(OmniVal, 10) and OmniVal.IsOwnedObject then
      begin
        aCallbacks.ItemFound(OmniVal);
        TotalFiles.Add((OmniVal.AsOwnedObject as TSearchItem).FilePath);
        Inc(ResultsCount);
      end;

      if ResultsCount mod math.Max(1, FParams.UpdateProgressEachItems) = 0 then
        FProgress.Info := IntToStr(ResultsCount) + ' results found in ' + IntToStr(TotalFiles.Count) + ' files.';
      if FProgress.Cancelled then
      begin
        Pipeline.Cancel;
        Pipeline.WaitFor(INFINITE);
        break;
      end;
    end;

    if FProgress.Cancelled then
      Result := csee_Cancelled
    else if ResultsCount > 0 then
      Result := csee_Successful
    else
      Result := csee_NothingFound;
  finally
    if Pipeline <> nil then
      Pipeline.WaitFor(INFINITE);
    aCallbacks.SearchFinished;
  end;
end;

function TCodeSearchEngine.SearchIndexFile(const aIndexSearchPath: array of string): string;
const
  cstIndexFile = 'codesearch.index';
var
  SearchPath, Path, ParentPath, IndexFile: String;
begin
  for SearchPath in aIndexSearchPath do
  begin
    ParentPath :=  SearchPath;
    repeat
      Path := ParentPath;
      ParentPath := ExtractFileDir(Path);
      IndexFile := IncludeTrailingPathDelimiter(Path) + cstIndexFile;
      if FileExists(IndexFile) then
      begin
        Result := IndexFile;
        Exit;
      end;
    until (Path = '') or (Path = ParentPath);
  end;
end;

{ TJCLAppExecutor }

constructor TJCLAppExecutor.Create(const aAppName, aCommandLine: string; aCancellationToken: IOmniCancellationToken);
begin
  inherited Create;
  FAppName := aAppName;
  FCommandLine := aCommandLine;
  FCancellationToken := aCancellationToken;  
end;

destructor TJCLAppExecutor.Destroy;
begin
  inherited;
end;

function TJCLAppExecutor.Execute(OnNewOutputLine, OnNewErrorLine: TOutputLineCallback; const aPriority: string): Boolean;
var
  CmdOptions: TJclExecuteCmdProcessOptions;
begin
  try
    CmdOptions := nil;
    FCallback := OnNewOutputLine;
    FErrorCallback := OnNewErrorLine;
    try
      CmdOptions := TJclExecuteCmdProcessOptions.Create(FAppName + ' ' + FCommandLine);
      CmdOptions.AbortPtr := @FAbort;
      CmdOptions.ProcessPriority := JclProcessPriorityFromString(aPriority);
      CmdOptions.RawOutput := True;
      CmdOptions.RawError := True;
      CmdOptions.MergeError := False;
      CmdOptions.OutputLineCallback := OnLineCallback;
      CmdOptions.ErrorLineCallback := OnErrorCallback;
      Result := ExecuteCmdProcess(CmdOptions);
    finally
      FCallback := nil;;
      FErrorCallback := nil;
      FreeAndNil(CmdOptions);
    end;
  except
    Result := False;
  end;
end;

procedure TJCLAppExecutor.OnLineCallback(const Text: string);
begin
  if FCancellationToken.IsSignalled then
    FAbort := True
  else if Assigned(FCallback) then
    FCallback(Text);
end;

procedure TJCLAppExecutor.OnErrorCallback(const Text: string);
begin
  if FCancellationToken.IsSignalled then
    FAbort := True
  else if Assigned(FErrorCallback) then
    FErrorCallback(Text);
end;

{ TCodeSearchEngineParams }

procedure TCodeSearchEngineParams.DoReadParams(aIni: TCustomIniFile);
begin
  inherited;
  FCSearchProcessPriority := aIni.ReadString(cst_Reg_Section, cst_Reg_ProcessPriority, FCSearchProcessPriority);
  FParseTaskCount := aIni.ReadInteger(cst_Reg_Section, cst_Reg_ParseTaskCount, FParseTaskCount);
  FUpdateProgressEachItems := aIni.ReadInteger(cst_Reg_Section, cst_Reg_UpdateProgressEachItems, FUpdateProgressEachItems);
end;

procedure TCodeSearchEngineParams.DoWriteParams(aIni: TCustomIniFile);
begin
  inherited;
  aIni.WriteString(cst_Reg_Section, cst_Reg_ProcessPriority, FCSearchProcessPriority);
  aIni.WriteInteger(cst_Reg_Section, cst_Reg_ParseTaskCount, ParseTaskCount);
  aIni.WriteInteger(cst_Reg_Section, cst_Reg_UpdateProgressEachItems, FUpdateProgressEachItems);
end;

procedure TCodeSearchEngineParams.SetDefault;
begin
  inherited;
  FCSearchProcessPriority := JclProcessPriorityToString(ppNormal);
  FParseTaskCount := 4;
  FUpdateProgressEachItems := 10;
end;

{ TParserState }

constructor TParserState.Create(aParser: TCodeSearchParser);
begin
  inherited Create;
  Parser := aParser;
end;

destructor TParserState.Destroy;
begin
  FreeAndNil(Parser);
  inherited;
end;

{ TCodeSearchParserQueue }

constructor TCodeSearchMultilineParser.Create(aResQueue: IOmniBlockingCollection);
begin
  inherited Create;
  FResultsQueue := aResQueue;
  FFoundTextBuffer := TStringList.Create;
end;

destructor TCodeSearchMultilineParser.Destroy;
begin
  FreeAndNil(FFoundTextBuffer);
  inherited;
end;

function TCodeSearchMultilineParser.ExtractFileInfo(const aLine: string;
    out FilePath, FoundText: string; out LineNum: Integer): Boolean;


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

var
  LineNumberStr: string;
  FilePathIdx, LineNumberIdx: Integer;
begin
  Result := False;
  if (Length(aLine) < 8) or (aLine[2] <> ':') or (aLine[3] <> '\') then // D:\1:1:1
    Exit;

  FilePathIdx := PosEx(':', aLine, 3);
  if not CopySafe(aLine, 1, FilePathIdx - 1, FilePath) then
    Exit;

  LineNumberIdx := PosEx(':', aLine, FilePathIdx + 1);
  if not CopySafe(aLine, FilePathIdx + 1, LineNumberIdx - 1, LineNumberStr) or
      not TryStrToInt(LineNumberStr, LineNum) then
//      not CopySafe(aLine, LineNumberIdx + 1, Length(aLine), FoundText) or (Trim(FoundText) = '')then
    Exit;


  CopySafe(aLine, LineNumberIdx + 1, Length(aLine), FoundText);
  Result := True;
end;

procedure TCodeSearchMultilineParser.AppendLine(const aLine: string);
(*
Sample output line:
{file path}:{line number}:{found text}
D:\acdsrc\acd\cur\sources\src\cfolder\erp\cfe_workflow_actions_wnd.pas:1:unit cfe_workflow_actions_wnd;
*)
var
  FilePath, FoundText: string;
  LineNum: Integer;
begin
  if ExtractFileInfo(aLine, FilePath, FoundText, LineNum) then
  begin
    FlushBuffer;

    FFilePath := FilePath;
    FLineNum := LineNum;
    FFoundTextBuffer.Add(FoundText);
  end
  else if (FFilePath <> '') and (FLineNum > 0) then
    FFoundTextBuffer.Add(aLine);
end;

procedure TCodeSearchMultilineParser.FlushBuffer;
var
  ResItem: TOmniValue;
  ItemInfo: TSearchItemInfo;
begin
  if (FFilePath = '') or (FLineNum <= 0) and (FFoundTextBuffer.Count <= 0) then
    Exit;

  ItemInfo := TSearchItemInfo.Create;
  ItemInfo.FilePath := FFilePath;
  ItemInfo.LineNum := FLineNum;
  ItemInfo.Text := FFoundTextBuffer.Text;
  ResItem.AsOwnedObject := ItemInfo;
  FResultsQueue.TryAdd(ResItem);

  FLineNum := 0;
  FFilePath := '';
  FFoundTextBuffer.Clear;
end;

end.
