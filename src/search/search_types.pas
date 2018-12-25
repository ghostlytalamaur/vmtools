unit search_types;

{$I cond_define.inc}

interface

uses
  vmsys, generics.collections, generics.defaults, observer, classes, syncobjs, collections.sets;

type
  TVMSearchResultsItem = class(TObject)
  private
    FFileName: string;
    FRating: Integer;
    FLine: Integer;
    FText: string;
    FFilePath: string;
    FRawText: string;
  public
    constructor Copy(aFrom: TVMSearchResultsItem);
    procedure CopyFrom(aFrom: TVMSearchResultsItem);

    property FileName: string read FFileName write FFileName;
    property FilePath: string read FFilePath write FFilePath;
    property Line: Integer read FLine write FLine;
    property Text: string read FText write FText;
    property RawText: string read FRawText write FRawText;
    property Rating: Integer read FRating write FRating;
  end;

  IVMSearchResultsListListener = interface
  ['{DF66D538-C1FC-47B3-B106-22EF9BA09FE6}']
    procedure ItemAdded(aItem: TVMSearchResultsItem);
  end;

  TVMSearchResultsList = class(TObject)
  private
    FList: TList<TVMSearchResultsItem>;
    FListCS: TCriticalSection;
    FAnnouncer: IAnnouncer<IVMSearchResultsListListener>;

    function GetCount: Integer;
    function GetItem(aIdx: Integer): TVMSearchResultsItem;
    function GetListeners: IListenersRegistry<IVMSearchResultsListListener>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(aItem: TVMSearchResultsItem);

    function CalculateFilesCount: Integer;

    property Items[aIdx: Integer]: TVMSearchResultsItem read GetItem;
    property Count: Integer read GetCount;
    property Listeners: IListenersRegistry<IVMSearchResultsListListener> read GetListeners;
  end;

  TSearchStatusCode = (
      ssc_Unknown,
      ssc_Successful,
      ssc_Queued,
      ssc_Searching,
      ssc_Cancelled,
      ssc_Error);

  TSearchInfo = class(TExtObject)
  private
    FSearchText: string;
    FResults: IMutableData<TVMSearchResultsList>;
    FStatus: IMutableData<TSearchStatusCode>;
    FStatusText: IMutableData<string>;
    FErrors: IMutableData<TStringList>;
    FSearchId: Int64;

    function GetResults: IObservableData<TVMSearchResultsList>;
  public
    constructor Create(aSearchText: string);
    destructor Destroy; override;

    property SearchText: string read FSearchText;
    property Results: IObservableData<TVMSearchResultsList> read GetResults;
    property Status: IMutableData<TSearchStatusCode> read FStatus;
    property StatusText: IMutableData<string> read FStatusText;
    property SearchId: Int64 read FSearchId write FSearchId;
    property Errors: IMutableData<TStringList> read FErrors;
  end;

implementation

uses
  SysUtils;

{ TSearchResultsList }

procedure TVMSearchResultsList.AddItem(aItem: TVMSearchResultsItem);
begin
  if aItem = nil then
    Exit;

  FListCS.Acquire;
  try
    FList.Add(aItem);
    FAnnouncer.ForEachListener(procedure (aObserver: IVMSearchResultsListListener)
    begin
      aObserver.ItemAdded(aItem);
    end);
  finally
    FListCS.Release;
  end;
end;

function TVMSearchResultsList.CalculateFilesCount: Integer;
var
  TotalFiles: ISet<string>;
  Item: TVMSearchResultsItem;
begin
  TotalFiles := THashSet<string>.Create;
  FListCS.Acquire;
  try
    for Item in FList do
      TotalFiles.Add(IncludeTrailingPathDelimiter(Item.FilePath) + Item.FileName);
    Result := TotalFiles.Count;
  finally
    FListCS.Release;
  end;
end;

constructor TVMSearchResultsList.Create;
begin
  inherited Create;
  FAnnouncer := TAnnouncer<IVMSearchResultsListListener>.Create;
  FList := TObjectList<TVMSearchResultsItem > .Create;
  FListCS := TCriticalSection.Create;
end;

destructor TVMSearchResultsList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FListCS);
  inherited;
end;

function TVMSearchResultsList.GetCount: Integer;
begin
  FListCS.Acquire;
  try
    Result := FList.Count;
  finally
    FListCS.Release;
  end;
end;

function TVMSearchResultsList.GetItem(aIdx: Integer): TVMSearchResultsItem;
begin
  FListCS.Acquire;
  try
    if (aIdx >= 0) and (aIdx < FList.Count) then
      Result := FList[aIdx]
    else
      Result := nil;
  finally
    FListCS.Release;
  end;
end;

function TVMSearchResultsList.GetListeners: IListenersRegistry<IVMSearchResultsListListener>;
begin
  Result := FAnnouncer;
end;

constructor TSearchInfo.Create(aSearchText: string);
begin
  inherited Create;
  FSearchText := aSearchText;
  FResults := TObservableData<TVMSearchResultsList>.Create(
    procedure (var Value: TVMSearchResultsList)
    begin
      FreeAndNil(Value);
    end);
  FResults.setValue(TVMSearchResultsList.Create);
  FStatusText := TObservableData<string>.Create;
  FStatus := TObservableData<TSearchStatusCode>.Create;
  FStatus.SetValue(ssc_Unknown);
  FErrors := TObservableData<TStringList>.Create(
      procedure (var Value: TStringList)
      begin
        FreeAndNil(Value);
      end);
end;

destructor TSearchInfo.Destroy;
begin
  inherited;
end;

function TSearchInfo.GetResults: IObservableData<TVMSearchResultsList>;
begin
  Result := FResults;
end;

{ TVMSearchResultsItem }

constructor TVMSearchResultsItem.Copy(aFrom: TVMSearchResultsItem);
begin
  Create;
  CopyFrom(aFrom);
end;

procedure TVMSearchResultsItem.CopyFrom(aFrom: TVMSearchResultsItem);
begin
  if aFrom = nil then
    Exit;

  FFileName := aFrom.FFileName;
  FRating := aFrom.FRating;
  FLine  := aFrom.FLine;
  FText := aFrom.FText;
  FFilePath := aFrom.FFilePath;
  FRawText := aFrom.FRawText;
end;

end.
