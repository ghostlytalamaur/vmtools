unit openfile_handler;
(*
  Without BorlandIDEServices dependency
*)

interface

uses
  vmsys, classes, observer, otlsync, otlcontainers, otleventmonitor, otltaskcontrol, otlcomm, otltask,
  otlcommon, generics.collections, base_params, inifiles, windows,
  collections.tst, OtlParallel;

type
  TFilePathRec = record
  strict private
    FFilePath: string;
    FFileName: string;
  public
    constructor Create(aFilePath: string);

    property FilePath: string read FFilePath;
    property FileName: string read FFileName;
  end;

  TOpenFileHandlerStatus = (
      ofht_Ready,
      ofht_PendingRefresh,
      ofht_RefreshList,
      ofht_Filtering);
  TBaseOpenFileHandler = class(TExtObject)
  private
    FPathsTrie: TTST<TFilePathRec>;
    FCritSection: TOmniCS;
    FWorker: IOmniBackgroundWorker;
    FStatus: IObservableData<TOpenFileHandlerStatus>;
    FFilteredList: IObservableData<TList<string>>;
    FFilter: string;

    function ValidateFileList: Boolean;

    function GetFilePaths(aWorkItem: IOmniWorkItem) : TTST<TFilePathRec>;
    function GetFilteredPaths(const aFilter: string; aWorkItem: IOmniWorkItem): TList<string>;
    procedure FilterPaths;

    procedure PerformRefresh(const aWorkItem: IOmniWorkItem);
    procedure OnRefreshDone(const Sender: IOmniBackgroundWorker;
        const workItem: IOmniWorkItem);

    procedure PerformFiltering(const workItem: IOmniWorkItem);
    procedure OnFilteringDone(const Sender: IOmniBackgroundWorker;
        const workItem: IOmniWorkItem);
  protected
    function GetDirPaths: IEnumerable<string>; virtual; abstract;
    function GetFileMasks: TArray<string>; virtual;
    function GetAdditionFileList: TArray<string>; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidatePaths;
    procedure OpenFile(const aFilePath: string); virtual;

    procedure SetFilter(const aFilter: string);
    property Status: IObservableData<TOpenFileHandlerStatus> read FStatus;
    property FilteredPaths: IObservableData<TList<string>> read FFilteredList;
  end;

implementation

uses
  SysUtils, ioutils, masks, str_utils, generics.defaults,
  regularexpressionscore,
  OtlCollections, collections.array_utils, opt_impl;

type
  TRefreshWorkItemResult = class
    Trie: TTST<TFilePathRec>;

    constructor Create(aTrie: TTST<TFilePathRec>);
    destructor Destroy; override;
    function AcquireTrie: TTST<TFilePathRec>;
  end;

  TFilterWorkItemResult = class
    List: TList<string>;

    constructor Create(aList: TList<string>);
    destructor Destroy; override;

    function AcquireList: TList<string>;
  end;

{ TFilesProvider }

constructor TBaseOpenFileHandler.Create;
begin
  inherited;
  FStatus := TObservableData<TOpenFileHandlerStatus>.Create;
  FStatus.setValue(ofht_PendingRefresh);
  FFilteredList := TObservableData<TList<string>>.Create(
    procedure (var List: TList<string>)
    begin
      FreeAndNil(List);
    end);

  FWorker := Parallel.BackgroundWorker
      .NumTasks(1)
      .Execute;

  InvalidatePaths;
end;

procedure TBaseOpenFileHandler.PerformFiltering(const workItem: IOmniWorkItem);
var
  Res: TOmniValue;
  List: TList<string>;
begin
  Status.postValue(ofht_Filtering);
  List := GetFilteredPaths(workItem.Data, workItem);
  Res.AsOwnedObject := TFilterWorkItemResult.Create(List);
  WorkItem.Result := Res;
  Status.postValue(ofht_Ready);
end;

procedure TBaseOpenFileHandler.OnFilteringDone(const Sender: IOmniBackgroundWorker;
    const workItem: IOmniWorkItem);
begin
  if workItem.IsExceptional or workItem.CancellationToken.IsSignalled then
    Exit;

  FilteredPaths.postValue((WorkItem.Result.AsOwnedObject as TFilterWorkItemResult).AcquireList);
  ValidateFileList;
end;

procedure TBaseOpenFileHandler.PerformRefresh(const aWorkItem: IOmniWorkItem);
var
  Res: TOmniValue;
begin
  Status.postValue(ofht_RefreshList);
  Res.AsOwnedObject := TRefreshWorkItemResult.Create(GetFilePaths(aworkItem));
  Status.postValue(ofht_Ready);
  aWorkItem.Result := Res;
end;

procedure TBaseOpenFileHandler.OnRefreshDone(const Sender: IOmniBackgroundWorker;
    const workItem: IOmniWorkItem);
begin
  if workItem.IsExceptional or workItem.CancellationToken.IsSignalled then
    Exit;

  FCritSection.Acquire;
  try
    FreeAndNil(FPathsTrie);
    FPathsTrie := (WorkItem.Result.AsOwnedObject as TRefreshWorkItemResult).AcquireTrie;
  finally
    FCritSection.Release;
  end;
  FilterPaths;
end;

destructor TBaseOpenFileHandler.Destroy;
begin
  FStatus := nil;
  FFilteredList := nil;
  FWorker.CancelAll;
  FWorker.Terminate(INFINITE);
  FWorker:= nil;
  FreeAndNil(FPathsTrie);
  inherited;
end;

type
  Void = record end;
function TBaseOpenFileHandler.GetFilePaths(aWorkItem: IOmniWorkItem): TTST<TFilePathRec>;
var
  FilePathsSet: TDictionary<string, Void>;
  FileMasks: TArray<string>;
  V: Void;

  procedure TryAppendFile(const aFilePath: string);
  begin
    if FilePathsSet.ContainsKey(aFilePath) or not TStrUtils.MatchesMasks(aFilePath, FileMasks) then
      Exit;

    FilePathsSet.AddOrSetValue(aFilePath, V);
  end;

var
  DirPath: string;
  FilePath: string;
  DirPaths: IEnumerable<string>;
  Processed: TDictionary<string, Void>;
  Rec: TFilePathRec;
  Size: Int64;
begin
  Result := nil;
  Processed := nil;
  FilePathsSet := nil;
  try
    Size:= 0;
    Processed := TDictionary<string, Void>.Create(10000);
    FilePathsSet := TDictionary<string, Void>.Create(10000);
    DirPaths := GetDirPaths;
    FileMasks := GetFileMasks;
    for DirPath in DirPaths do
    begin
      if aWorkItem.CancellationToken.IsSignalled then
        Exit;

      if Processed.ContainsKey(DirPath) or not TDirectory.Exists(DirPath) then
        Continue;

      Processed.AddOrSetValue(DirPath, V);
      for FilePath in TDirectory.GetFiles(DirPath, '*', TSearchOption.soTopDirectoryOnly) do
      begin
        if aWorkItem.CancellationToken.IsSignalled then
          Exit;

        TryAppendFile(FilePath);
      end;
    end;

    for FilePath in GetAdditionFileList do
      TryAppendFile(FilePath);

    for FilePath in FilePathsSet.Keys do
    begin
      Rec := TFilePathRec.Create(FilePath);
      if Result = nil then
        Result := TTST<TFilePathRec>.Create;
      Result.PutValue(TStrUtils.Normalize(Rec.FileName), Rec);
      Inc(Size, (Length(FilePath) + Length(TStrUtils.Normalize(Rec.FileName))) * SizeOf(FilePath[1]));
      if aWorkItem.CancellationToken.IsSignalled then
      begin
        FreeAndNil(Result);
        Exit;
      end;
    end;
  finally
    FreeAndNil(Processed);
    FreeAndNil(FilePathsSet);
  end;
end;

function TBaseOpenFileHandler.GetAdditionFileList: TArray<string>;
begin
  Result := nil;
end;

function TBaseOpenFileHandler.GetFileMasks: TArray<string>;
begin
  Result := TArrayUtils.AsArray<string>(['*']);
end;

function TBaseOpenFileHandler.GetFilteredPaths(const aFilter: string;
    aWorkItem: IOmniWorkItem): TList<string>;
const
  cstTokenLen = 3;

  function CreateRegEx(aRegExp: string): TPerlRegEx;
  begin
    Result := TPerlRegEx.Create;
    Result.Options := [preCaseLess];
    try
      Result.RegEx := aRegExp;
      Result.Compile;
    except
      Result.RegEx := '.*';
      Result.Compile;
    end;
  end;

  function BuildRegExp(aFilter: string): string;
  var
    I: Integer;
  begin
    Result := '.*?';
    for I := 1 to Length(aFilter) do
    begin
      Result := Result + aFilter[I] + '.*?';
    end;
  end;

var
  Coefs: TDictionary<string, Single>;
  Ratio: Single;
  FilterNGrams: TArray<string>;
  P: TPair<string, TFilePathRec>;
  FuzzyRegExp, FilleRegExp, NormKeyRegExp: TPerlRegEx;
  Comparer: IComparer<string>;
begin
  Result := nil;
  if (aFilter = '') or (Length(aFilter) < 2) then
    Exit;

  FCritSection.Acquire;
  try
    if (FPathsTrie = nil) then
      Exit;
  finally
    FCritSection.Release;
  end;

  FilleRegExp := nil;
  NormKeyRegExp := nil;
  FuzzyRegExp := nil;
  Coefs := nil;
  try
    Coefs := TDictionary<string, Single>.Create;
    FilterNGrams := TStrUtils.NGrams(TStrUtils.Normalize(aFilter), cstTokenLen);
    FilleRegExp := CreateRegEx(aFilter);

    if not TStrUtils.HasChars(aFilter, ['/', '\', ':', '*', '?', '<', '>', '|']) then
    begin
      NormKeyRegExp := CreateRegEx(TStrUtils.Normalize(aFilter));
      FuzzyRegExp := CreateRegEx(BuildRegExp(aFilter));
    end;

    FCritSection.Acquire;
    try
      for P in FPathsTrie.Pairs do
      begin
        if aWorkItem.CancellationToken.IsSignalled then
          Exit;

        try
          if NormKeyRegExp <> nil then
            NormKeyRegExp.Subject := P.Key;
          if (NormKeyRegExp = nil) or not NormKeyRegExp.Match then
          begin
            FilleRegExp.Subject := P.Value.FileName;
            if not FilleRegExp.Match then
            begin
              if FuzzyRegExp <> nil then
                FuzzyRegExp.Subject := P.Value.FileName;
              if (FuzzyRegExp = nil) or not FuzzyRegExp.Match then
                Continue;
            end;
          end;
        except
          Continue;
        end;

        Ratio := TStrUtils.NGramsSimilarity(FilterNGrams, TStrUtils.NGrams(P.Key, cstTokenLen));
        Coefs.AddOrSetValue(P.Value.FilePath, Ratio);
        if Result = nil then
          Result := TList<string>.Create;
        Result.Add(P.Value.FilePath);
      end;
    finally
      FCritSection.Release;
    end;

    if Result <> nil then
    begin
      Comparer := TDelegatedComparer<string>.Create(function(const Left, Right: string): Integer
        begin
          Result := Compare(Coefs[Left], Coefs[Right]);
          if Result = 0 then
            Result := CompareText(Left, Right);
          Result := -Result;
        end);
      Result.Sort(Comparer);
    end;
  finally
    FreeAndNil(Coefs);
    FreeAndNil(FilleRegExp);
    FreeAndNil(NormKeyRegExp);
    FreeAndNil(FuzzyRegExp);
  end;
end;

procedure TBaseOpenFileHandler.FilterPaths;
begin
  FWorker.Schedule(
      FWorker.CreateWorkItem(FFilter),
      FWorker.Config
          .OnExecute(PerformFiltering)
          .OnRequestDone(OnFilteringDone));
end;

procedure TBaseOpenFileHandler.SetFilter(const aFilter: string);
begin
  FFilter := aFilter;
  if ValidateFileList then
    FilterPaths;
end;

procedure TBaseOpenFileHandler.InvalidatePaths;
begin
  if FFilter <> '' then
  begin
    FWorker.CancelAll;
    FStatus.postValue(ofht_PendingRefresh);
    ValidateFileList;
  end
  else
    FStatus.postValue(ofht_PendingRefresh);
end;

procedure TBaseOpenFileHandler.OpenFile(const aFilePath: string);
begin
end;

function TBaseOpenFileHandler.ValidateFileList: Boolean;
begin
  Result := not (Status.getValue in [ofht_PendingRefresh, ofht_RefreshList]);
  if Status.getValue = ofht_PendingRefresh then
  begin
    FWorker.Schedule(
        FWorker.CreateWorkItem(1),
        FWorker.Config
            .OnExecute(PerformRefresh)
            .OnRequestDone(OnRefreshDone));
  end;
end;

{ TFilePathRec }

constructor TFilePathRec.Create(aFilePath: string);
begin
  inherited;
  FFilePath := aFilePath;
  FFileName := ExtractFileName(FFilePath);
end;

{ TFilterWorkItemResult }

constructor TFilterWorkItemResult.Create(aList: TList<string>);
begin
  inherited Create;
  List := aList;
end;

destructor TFilterWorkItemResult.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

function TFilterWorkItemResult.AcquireList: TList<string>;
begin
  Result := List;
  List := nil;
end;

{ TRefreshWorkItemResult }

function TRefreshWorkItemResult.AcquireTrie: TTST<TFilePathRec>;
begin
  Result := Trie;
  Trie := nil;
end;

constructor TRefreshWorkItemResult.Create(aTrie: TTST<TFilePathRec>);
begin
  inherited Create;
  Trie := aTrie;
end;

destructor TRefreshWorkItemResult.Destroy;
begin
  FreeAndNil(Trie);
  inherited;
end;

end.