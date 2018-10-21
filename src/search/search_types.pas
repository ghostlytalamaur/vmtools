unit search_types;

{$I cond_define.inc}

interface

uses
  vmsys, generics.collections, observer;

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

  TVMSearchResultsList = class(TObject)
  private
    FList: TList<TVMSearchResultsItem>;
    function GetCount: Integer;
    function GetItem(aIdx: Integer): TVMSearchResultsItem;
  public
    constructor Create;
    constructor Copy(aFrom: TVMSearchResultsList);

    destructor Destroy; override;
    procedure CopyFrom(aFrom: TVMSearchResultsList);

    procedure AddItem(aItem: TVMSearchResultsItem);

    function CalculateFilesCount: Integer;

    property Items[aIdx: Integer]: TVMSearchResultsItem read GetItem;
    property Count: Integer read GetCount;
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
    FResults: IObservableData<TVMSearchResultsList>;
    FStatus: IObservableData<TSearchStatusCode>;
    FStatusText: IObservableData<string>;
    FSearchId: Int64;

    function GetResults: IObservableData<TVMSearchResultsList>;
  public
    constructor Create(aSearchText: string; aResults: TVMSearchResultsList);
    destructor Destroy; override;
    procedure CopyResults(aFrom: TSearchInfo);

    property SearchText: string read FSearchText;
    property Results: IObservableData<TVMSearchResultsList> read GetResults;
    property Status: IObservableData<TSearchStatusCode> read FStatus;
    property StatusText: IObservableData<string> read FStatusText;
    property SearchId: Int64 read FSearchId write FSearchId;
  end;

implementation

uses
  SysUtils;

{ TSearchResultsList }

procedure TVMSearchResultsList.AddItem(aItem: TVMSearchResultsItem);
begin
  if aItem <> nil then
    FList.Add(aItem);
end;

function TVMSearchResultsList.CalculateFilesCount: Integer;
var
  TotalFiles: TDictionary<string, Pointer>;
  Item: TVMSearchResultsItem;
begin
  TotalFiles := TDictionary<string, Pointer>.Create;
  try
    for Item in FList do
      TotalFiles.AddOrSetValue(IncludeTrailingPathDelimiter(Item.FilePath) + Item.FileName, nil);
    Result := TotalFiles.Count;
  finally
    FreeAndNil(TotalFiles);
  end;
end;

constructor TVMSearchResultsList.Copy(aFrom: TVMSearchResultsList);
begin
  Create;
  CopyFrom(aFrom);
end;

procedure TVMSearchResultsList.CopyFrom(aFrom: TVMSearchResultsList);
var
  I: Integer;
begin
  FList.Clear;
  if aFrom = nil then
    Exit;

  for I := 0 to aFrom.Count - 1 do
    FList.Add(TVMSearchResultsItem.Copy(aFrom.Items[I]));
end;

constructor TVMSearchResultsList.Create;
begin
  inherited Create;
  FList := TObjectList<TVMSearchResultsItem > .Create;
end;

destructor TVMSearchResultsList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TVMSearchResultsList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TVMSearchResultsList.GetItem(aIdx: Integer): TVMSearchResultsItem;
begin
  if (aIdx >= 0) and (aIdx < FList.Count) then
    Result := FList[aIdx]
  else
    Result := nil;
end;

{ TSearchInfo }

procedure TSearchInfo.CopyResults(aFrom: TSearchInfo);
begin
  if (aFrom = nil) or (aFrom.Results.getValue = nil) then
    Results.setValue(nil)
  else
    Results.setValue(TVMSearchResultsList.Copy(aFrom.Results.getValue));
end;

constructor TSearchInfo.Create(aSearchText: string; aResults: TVMSearchResultsList);
begin
  inherited Create;
  FSearchText := aSearchText;
  Results.setValue(aResults);
  FStatusText := TObservableData<string>.Create;
  FStatus := TObservableData<TSearchStatusCode>.Create;
  FStatus.SetValue(ssc_Unknown);
end;

destructor TSearchInfo.Destroy;
begin
  inherited;
end;

function TSearchInfo.GetResults: IObservableData<TVMSearchResultsList>;
begin
  if FResults = nil then
    FResults := TObservableData<TVMSearchResultsList>.Create(
      procedure (var Value: TVMSearchResultsList)
      begin
        FreeAndNil(Value);
      end);
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
