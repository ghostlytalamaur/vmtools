unit search_handler;

{$I cond_define.inc}

interface

uses
  vmsys,
  search_types,
  otltask, otltaskcontrol, generics.collections,
  OtlThreadPool, csplg_query_params, OtlParallel, observer, sysutils;

type
  ISearchResultsListener = interface
    procedure SearchInfoAdded(aIndex: Integer);
    procedure SearchInfoRemoved(aIndex: Integer);
  end;

  TSearchHandler = class(TExtObject)
  private
    FSearchWorker: IOmniBackgroundWorker;
    FSearchResults: TList<TSearchInfo>;
    FListeners: IAnnouncer<ISearchResultsListener>;
    FWorkItems: TDictionary<Int64, IOmniWorkItem>;

    function CreateSearchQuery: TCodeSearchQueryParams;
    function CreateSearchWorker: IOmniBackgroundWorker;
    procedure PerformSearch(const workItem: IOmniWorkItem);
    procedure OnSearchRequestDone(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
  protected
    function AppendSearchResult(aSearchInfo: TSearchInfo): Boolean; virtual;
    function GetProjectPaths: IEnumerable<string>; virtual; abstract;
    function GetIndexSearchPaths: TArray<string>; virtual; abstract;
    function GetQueryText: string; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecuteSearch;
    procedure CancellSearch(aSearchId: Int64);

    procedure RemoveSearchInfo(aIndex: Integer);

    procedure FocusEditor; virtual;
    procedure OpenFileInEditor(const aFileName: string; aLine: Integer); virtual;

    property SearchResults: TList<TSearchInfo> read FSearchResults;
    property Listeners: IAnnouncer<ISearchResultsListener> read FListeners;
  end;

  TDelegatedSearchResultsListener = class(TInterfacedObject, ISearchResultsListener)
  private
    FOnSearchInfoAdded: TProc<Integer>;
    FOnSearchInfoRemoved: TProc<Integer>;

    procedure SearchInfoAdded(aIndex: Integer);
    procedure SearchInfoRemoved(aIndex: Integer);
  public
    constructor Create(aOnSearchInfoAdded, aOnSearchInfoRemoved: TProc<Integer>);
  end;

implementation

uses
  dialogs, Classes, Windows,
  search_query_dlg, controls, OtlCommon,
  str_utils, csplg_search, progress, csplg_types, collections.array_utils,
  otlsync, search_cst, diagnostics;

type
  TWorkItemData = class(TObject)
  public
    Params: TCodeSearchQueryParams;
    DestInfo: IObjectHolder<TSearchInfo>;

    constructor Create(aParams: TCodeSearchQueryParams; aDestInfo: TSearchInfo);
    destructor Destroy; override;
  end;

  TWorkItemResult = class
    List: TVMSearchResultsList;
    ErrorCode: TCodeSearchEngineError;
    SearchTime: Cardinal;

    constructor Create(aErrorCode: TCodeSearchEngineError; aList: TVMSearchResultsList);
    destructor Destroy; override;
  end;

  TSearchProgress = class(TProgress)
  private
    FWorkItem: IOmniWorkItem;
    FCaptions: TStack<string>;
    FTotal: TStack<Integer>;
    FCurrent: TStack<Integer>;
    FInfo: string;
    FStatusData: IObservableData<string>;
    FSearchInfo: IObjectHolder<TSearchInfo>;

    procedure UpdateStatusData;
  public
    constructor Create(aWorkItem: IOmniWorkItem);
    destructor Destroy; override;

    procedure SetTotal(aTotal: Integer); override;
    function GetTotal: Integer; override;

    procedure SetCurrent(aCurrent: Integer); override;
    function GetCurrent: Integer; override;

    procedure SetInfo(aInfo: string); override;
    function GetInfo: string; override;

    function GetCancelled: Boolean; override;

    procedure BeginProgress(aCaption: string); override;
    procedure EndProgress; override;
  end;


function GetErrorMsg(aCode: TCodeSearchEngineError): string;
begin
  case aCode of
    csee_Unknown:
      Result := 'Unknown error.';
    csee_InvalidQuery:
      Result := 'Invalid query.';
    csee_NoIndex:
      Result := 'No index file found. Rebuild index and try again.';
    csee_NothingFound:
      Result := 'Nothing found.';
    csee_Cancelled:
      Result := 'Cancelled.';
    csee_Successful:
      Result := 'Done.'
    else
      Result := '';
  end;
end;

function ConvertResults(aRes: TSearchResults): TVMSearchResultsList;
var
  I: Integer;
  Item: TSearchItem;
  DestItem: TVMSearchResultsItem;
begin
  Result := nil;
  if aRes = nil then
    Exit;

  for I := 0 to aRes.Count - 1 do
  begin
    Item := aRes[I];
    if Result = nil then
      Result := TVMSearchResultsList.Create;

    DestItem := TVMSearchResultsItem.Create;
    DestItem.FileName := ExtractFileName(Item.FilePath);
    DestItem.FilePath := ExtractFilePath(Item.FilePath);
    DestItem.Line := Item.LineNumber;
    DestItem.Text := Item.FoundText;
    DestItem.Rating := Item.Rating;
    DestItem.RawText := Item.RawText;

    Result.AddItem(DestItem);
  end;
end;

procedure TSearchHandler.PerformSearch(const workItem: IOmniWorkItem);
var
  Progress: TBaseProgress;
  ValidPaths: TStringList;
  Path: string;
  Engine: TCodeSearchEngine;
  Data: TWorkItemData;
  SearchResults: TSearchResults;
  ErrorCode: TCodeSearchEngineError;
  Res: TOmniValue;
  Stopwatch: TStopwatch;
  WorkItemRes: TWorkItemResult;
begin
  Data := workItem.Data.AsOwnedObject as TWorkItemData;
  if not Data.DestInfo.IsAlive then
    Exit;

  Data.DestInfo.Obj.Status.postValue(ssc_Searching);
  Data.DestInfo.Obj.StatusText.postValue('Searching...');

  Engine := nil;
  SearchResults := nil;
  ValidPaths := nil;
  Stopwatch := TStopwatch.StartNew;
  Progress := TSearchProgress.Create(WorkItem);
  try
    if Data.Params.InProjectPathsOnly then
    begin
      for Path in GetProjectPaths do
        if Path <> '' then
        begin
          if ValidPaths = nil then
            ValidPaths := TStringList.Create;
          ValidPaths.Add(Path);
        end;
    end;

    Engine := TCodeSearchEngine.Create(Progress, ValidPaths);

    ErrorCode := Engine.Search(Data.Params, GetIndexSearchPaths, SearchResults);
    WorkItemRes := TWorkItemResult.Create(ErrorCode, ConvertResults(SearchResults));
    WorkItemRes.SearchTime := Stopwatch.ElapsedMilliseconds;
    Res.AsOwnedObject := WorkItemRes;
    workItem.Result := Res;
  finally
    FreeAndNil(SearchResults);
    FreeAndNil(Engine);
    FreeAndNil(Progress);
    FreeAndNil(ValidPaths);
  end;
end;

procedure TSearchHandler.OnSearchRequestDone(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
var
  Data: TWorkItemData;
  ResData: TWorkItemResult;
begin
  if FWorkItems.ContainsKey(workItem.UniqueID) then
    FWorkItems.Remove(workItem.UniqueID);
  Data := workItem.Data.AsOwnedObject as TWorkItemData;
  if not Data.DestInfo.IsAlive then
    Exit;

  if workItem.IsExceptional then
  begin
    Data.DestInfo.Obj.Status.postValue(ssc_Error);
    Data.DestInfo.Obj.StatusText.postValue('Errror: ' + workItem.FatalException.Message);
  end
  else
  begin
    ResData := workItem.Result.AsOwnedObject as TWorkItemResult;
    if ResData = nil then
    begin
      if workItem.CancellationToken.IsSignalled then
      begin
        Data.DestInfo.Obj.Status.postValue(ssc_Cancelled);
        Data.DestInfo.Obj.StatusText.postValue('Cancelled.');
      end
      else
      begin
        Data.DestInfo.Obj.Status.postValue(ssc_Unknown);
        Data.DestInfo.Obj.StatusText.postValue('Unknown error.');
      end;
    end
    else if workItem.CancellationToken.IsSignalled then
    begin
      Data.DestInfo.Obj.Status.postValue(ssc_Cancelled);
      if ResData.List <> nil then
        Data.DestInfo.Obj.StatusText.postValue(Format('Cancelled. %d results in %d files. Elapsed time: %d ms.', [
            ResData.List.Count, ResData.List.CalculateFilesCount, ResData.SearchTime]))
      else
        Data.DestInfo.Obj.StatusText.postValue(Format('Cancelled. Elapsed time: %d ms.', [ResData.SearchTime]));
    end
    else if ResData.ErrorCode <> csee_Successful then
    begin
      Data.DestInfo.Obj.Status.postValue(ssc_Error);
      Data.DestInfo.Obj.StatusText.postValue(Format('Error: %s. Elapsed time: %d ms.', [
          GetErrorMsg(ResData.ErrorCode), ResData.SearchTime]));
    end
    else
    begin
      Data.DestInfo.Obj.Status.postValue(ssc_Successful);
      Data.DestInfo.Obj.StatusText.postValue(Format('Done. %d results in %d files. Elapsed time: %d ms.', [
          ResData.List.Count, ResData.List.CalculateFilesCount, ResData.SearchTime]));
    end;

    if ResData <> nil then
    begin
      Data.DestInfo.Obj.Results.postValue(ResData.List);
      ResData.List := nil;
    end;
  end;
end;

function TSearchHandler.CreateSearchWorker: IOmniBackgroundWorker;
begin
  Result := Parallel.BackgroundWorker
      .NumTasks(3)
      .Execute(PerformSearch)
      .OnRequestDone(OnSearchRequestDone);
end;

destructor TSearchHandler.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FWorkItems);
  FSearchWorker.CancelAll;
  FSearchWorker.Terminate(INFINITE);
  FSearchWorker := nil;

  for I := FSearchResults.Count - 1 downto 0 do
  begin
    FSearchResults.Delete(I);
    FListeners.ForEachListener(
      procedure (aListener: ISearchResultsListener)
      begin
        aListener.SearchInfoRemoved(I);
      end);
  end;

  FListeners := nil;
  FreeAndNil(FSearchResults);
  inherited;
end;

function TSearchHandler.AppendSearchResult(aSearchInfo: TSearchInfo): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (aSearchInfo = nil) then
    Exit;

  Index := FSearchResults.Add(aSearchInfo);
  FListeners.ForEachListener(
    procedure (aListener: ISearchResultsListener)
    begin
      aListener.SearchInfoAdded(Index);
    end);

  Result := True;
end;

constructor TSearchHandler.Create;
begin
  inherited Create;
  FSearchWorker := CreateSearchWorker;
  FListeners := TAnnouncer<ISearchResultsListener>.Create;
  FSearchResults := TObjectList<TSearchInfo>.Create;
  FWorkItems := TDictionary<Int64, IOmniWorkItem>.Create;
end;

function TSearchHandler.CreateSearchQuery: TCodeSearchQueryParams;
var
  Dlg: TSearchEngineQueryDlg;
begin
  Dlg := nil;
  Result := TCodeSearchQueryParams.Create;
  try
    Result.ReadParams;
    Result.QueryText := GetQueryText;
    Dlg := TSearchEngineQueryDlg.Create(0, Result);
    if Dlg.ShowModal <> mrOk then
    begin
      FreeAndNil(Result);
      Exit;
    end;

    Result.WriteParams;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TSearchHandler.ExecuteSearch;
var
  Query: TCodeSearchQueryParams;
  DestInfo: TSearchInfo;
  OmniData: TOmniValue;
  WorkItem: IOmniWorkItem;
begin
  Query := CreateSearchQuery;
  try
    if Query = nil then
      Exit;

    DestInfo := TSearchInfo.Create(Query.QueryText, nil);
    OmniData.AsOwnedObject := TWorkItemData.Create(Query, DestInfo);
    Query := nil;
    WorkItem := FSearchWorker.CreateWorkItem(OmniData);
    DestInfo.SearchId := WorkItem.UniqueID;
    FWorkItems.AddOrSetValue(WorkItem.UniqueID, WorkItem);

    DestInfo.Status.setValue(ssc_Queued);
    DestInfo.StatusText.setValue(rs_msg_SearchQueued);

    AppendSearchResult(DestInfo);
    FSearchWorker.Schedule(WorkItem);
  finally
    FreeAndNil(Query);
  end;
end;

procedure TSearchHandler.CancellSearch(aSearchId: Int64);
var
  WorkItem: IOmniWorkItem;
begin
  if FWorkItems.TryGetValue(aSearchId, WorkItem) then
    WorkItem.CancellationToken.Signal;
end;

procedure TSearchHandler.FocusEditor;
begin

end;

procedure TSearchHandler.OpenFileInEditor(const aFileName: string; aLine: Integer);
begin

end;

procedure TSearchHandler.RemoveSearchInfo(aIndex: Integer);
begin
  if (aIndex < 0) or (aIndex >= FSearchResults.Count) then
    Exit;

  FSearchResults.Delete(aIndex);
  FListeners.ForEachListener(
    procedure (aListener: ISearchResultsListener)
    begin
      aListener.SearchInfoRemoved(aIndex);
    end);
end;

{ TSearchProgress }

procedure TSearchProgress.BeginProgress(aCaption: string);
begin
  FCaptions.Push(aCaption);
  FTotal.Push(-1);
  FCurrent.Push(-1);
  FInfo := '';
  UpdateStatusData;
end;

constructor TSearchProgress.Create(aWorkItem: IOmniWorkItem);
begin
  inherited Create(nil);
  FWorkItem := aWorkItem;
  FSearchInfo :=   (FWorkItem.Data.AsOwnedObject as TWorkItemData).DestInfo;
  if FSearchInfo.IsAlive then
    FStatusData := FSearchInfo.Obj.StatusText;

  FCaptions := TStack<string>.Create;
  FTotal := TStack<Integer>.Create;
  FCurrent := TStack<Integer>.Create;
end;

destructor TSearchProgress.Destroy;
begin
  FreeAndNil(FCaptions);
  FreeAndNil(FTotal);
  FreeAndNil(FCurrent);
  inherited;
end;

procedure TSearchProgress.EndProgress;
begin
  FCaptions.Pop;
  FTotal.Pop;
  FCurrent.Pop;
  FInfo := '';
  UpdateStatusData;
end;

function TSearchProgress.GetCancelled: Boolean;
begin
  Result := FWorkItem.CancellationToken.IsSignalled or not FSearchInfo.IsAlive
end;

function TSearchProgress.GetCurrent: Integer;
begin
  if FCurrent.Count > 0 then
    Result := FCurrent.Peek
  else
    Result := 0;
end;

function TSearchProgress.GetInfo: string;
begin
  Result := FInfo;
end;

function TSearchProgress.GetTotal: Integer;
begin
  if FTotal.Count > 0 then
    Result := FTotal.Peek
  else
    Result := 0;
end;

procedure TSearchProgress.SetCurrent(aCurrent: Integer);
begin
  FCurrent.Pop;
  FCurrent.Push(aCurrent);
  UpdateStatusData;
end;

procedure TSearchProgress.SetInfo(aInfo: string);
begin
  FInfo := aInfo;
  UpdateStatusData;
end;

procedure TSearchProgress.SetTotal(aTotal: Integer);
begin
  FTotal.Pop;
  FTotal.Push(aTotal);
  UpdateStatusData;
end;

procedure TSearchProgress.UpdateStatusData;
var
  Status: string;
begin
  if FStatusData = nil then
    Exit;

  if FCaptions.Count > 0 then
    Status := FCaptions.Peek
  else
    Status := '';

  Status := TStrUtils.Join([Status, GetInfo], ' ');
  if GetTotal > 0 then
    Status := TStrUtils.Join([Status, IntToStr(GetCurrent) + '/' + IntToStr(GetTotal)], ' ');

  FStatusData.postValue(Status);
end;

{ TWorkItemResult }

constructor TWorkItemResult.Create(aErrorCode: TCodeSearchEngineError; aList: TVMSearchResultsList);
begin
  inherited Create;
  ErrorCode := aErrorCode;
  List := aList;
end;

destructor TWorkItemResult.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

{ TWorkItemData }

constructor TWorkItemData.Create(aParams: TCodeSearchQueryParams; aDestInfo:
    TSearchInfo);
begin
  inherited Create;
  Params := aParams;
  DestInfo := TObjectHolder<TSearchInfo>.Create(aDestInfo, False);
end;

destructor TWorkItemData.Destroy;
begin
  FreeAndNil(Params);
  inherited;
end;

{ TDelegatedSearchResultsListener }

constructor TDelegatedSearchResultsListener.Create(aOnSearchInfoAdded, aOnSearchInfoRemoved: TProc<Integer>);
begin
  inherited Create;
  FOnSearchInfoAdded := aOnSearchInfoAdded;
  FOnSearchInfoRemoved := aOnSearchInfoRemoved;
end;

procedure TDelegatedSearchResultsListener.SearchInfoAdded(aIndex: Integer);
begin
  if Assigned(FOnSearchInfoAdded) then
    FOnSearchInfoAdded(aIndex);
end;

procedure TDelegatedSearchResultsListener.SearchInfoRemoved(aIndex: Integer);
begin
  if Assigned(FOnSearchInfoRemoved) then
    FOnSearchInfoRemoved(aIndex);
end;

end.
