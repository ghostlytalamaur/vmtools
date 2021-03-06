unit openfile_handler;
(*
  Without BorlandIDEServices dependency
*)

interface

uses
  vmsys, classes, observer, otlsync, otlcontainers, otleventmonitor, otltaskcontrol, otlcomm, otltask,
  otlcommon, generics.collections, base_params, inifiles, windows,
  collections.tst, OtlParallel, collections.maps;

type
  TFilePathRec = record
  strict private
    FFilePath: string;
    FFileName: string;
    FNormalizedFileName: string;
  public
    constructor Create(aFilePath: string);

    property FilePath: string read FFilePath;
    property FileName: string read FFileName;
    property NormalizedFileName: string read FNormalizedFileName;
  end;

  TOpenFileHandlerStatus = (
      ofht_Ready,
      ofht_PendingRefresh,
      ofht_RefreshList,
      ofht_Filtering);
  TBaseOpenFileHandler = class(TExtObject)
  private
    FPathsList: TList<TFilePathRec>;
    FCritSection: TOmniCS;
    FWorker: IOmniBackgroundWorker;
    FStatus: IMutableData<TOpenFileHandlerStatus>;
    FFilteredList: IMutableData<TList<string>>;
    FFilter: string;
    FPriority: IMap<string, Integer>;

    function ValidateFileList: Boolean;

    function GetFilePaths(aWorkItem: IOmniWorkItem) : TList<TFilePathRec>;
    function GetFilteredPaths(const aFilter: string; aWorkItem: IOmniWorkItem): TList<string>;
    procedure FilterPaths;

    procedure PerformRefresh(const aWorkItem: IOmniWorkItem);
    procedure OnRefreshDone(const Sender: IOmniBackgroundWorker;
        const workItem: IOmniWorkItem);

    procedure PerformFiltering(const workItem: IOmniWorkItem);
    procedure OnFilteringDone(const Sender: IOmniBackgroundWorker;
        const workItem: IOmniWorkItem);

    class procedure LoadPriority(aPriority: IMap<string, Integer>);
    class procedure StorePriority(aPriority: IMap<string, Integer>);
    function GetPriority: IMap<string, Integer>;
    function GetStatus: IObservableData<TOpenFileHandlerStatus>;
    function GetFilteredList: IObservableData<TList<string>>;

    property Priority: IMap<string, Integer> read GetPriority;
  protected
    function GetDirPaths: IEnumerable<string>; virtual; abstract;
    function GetFileMasks: TArray<string>; virtual;
    function GetAdditionFileList: IEnumerable<string>; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidatePaths;
    procedure OpenFile(const aFilePath: string); virtual;

    procedure SetFilter(const aFilter: string);
    property Status: IObservableData<TOpenFileHandlerStatus> read GetStatus;
    property FilteredPaths: IObservableData<TList<string>> read GetFilteredList;
  end;

implementation

uses
  SysUtils, ioutils, masks, str_utils, generics.defaults,
  regularexpressionscore,
  OtlCollections, collections.array_utils, opt_impl, collections.sets, collections.common, vm.debug;

type
  TRefreshWorkItemResult = class
  public
    List: TList<TFilePathRec>;

    constructor Create(aList: TList<TFilePathRec>);
    destructor Destroy; override;
    function AcquireList: TList<TFilePathRec>;
  end;

  TFilterWorkItemResult = class
  public
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
  FStatus.postValue(ofht_Filtering);
  List := GetFilteredPaths(workItem.Data, workItem);
  Res.AsOwnedObject := TFilterWorkItemResult.Create(List);
  WorkItem.Result := Res;
  FStatus.postValue(ofht_Ready);
end;

procedure TBaseOpenFileHandler.OnFilteringDone(const Sender: IOmniBackgroundWorker;
    const workItem: IOmniWorkItem);
begin
  if workItem.IsExceptional or workItem.CancellationToken.IsSignalled then
    Exit;

  FFilteredList.postValue((WorkItem.Result.AsOwnedObject as TFilterWorkItemResult).AcquireList);
  ValidateFileList;
end;

class procedure TBaseOpenFileHandler.LoadPriority(aPriority: IMap<string, Integer>);
var
  Ini: TCustomIniFile;
  Priority, FilesCount, I: Integer;
  FilePath: string;
begin
  if aPriority = nil then
    Exit;

  Ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance))) + 'filespriority.ini');
  try
    aPriority.Clear;
    FilesCount := Ini.ReadInteger('OpenFilePriority', 'FilesCount', 0);
    for I := 0 to FilesCount - 1 do
    begin
      FilePath  := Ini.ReadString('OpenFilePriority', 'File' + IntToStr(I), '');
      Priority := Ini.ReadInteger('OpenFilePriority', 'Priority' + IntToStr(I), 0);
      if (Priority = 0) or (FilePath = '') or not FileExists(FilePath) then
        Continue;

      aPriority[FilePath] := Priority;
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

class procedure TBaseOpenFileHandler.StorePriority(aPriority: IMap<string, Integer>);
var
  P: TPair<string, Integer>;
  Ini: TCustomIniFile;
  I: Integer;
begin
  if aPriority = nil then
    Exit;

  LogEnterLeave('TBaseOpenFileHandler.StorePriority');
  Ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance))) + 'filespriority.ini');
  try
    Ini.EraseSection('OpenFilePriority');
    Ini.WriteInteger('OpenFilePriority', 'FilesCount', aPriority.Count);
    I := 0;
    for P in aPriority do
    begin
      Ini.WriteString('OpenFilePriority', 'File' + IntToStr(I), P.Key);
      Ini.WriteInteger('OpenFilePriority', 'Priority' + IntToStr(I), P.Value);
      Inc(I);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TBaseOpenFileHandler.PerformRefresh(const aWorkItem: IOmniWorkItem);
var
  Res: TOmniValue;
begin
  FStatus.postValue(ofht_RefreshList);
  Res.AsOwnedObject := TRefreshWorkItemResult.Create(GetFilePaths(aworkItem));
  FStatus.postValue(ofht_Ready);
  aWorkItem.Result := Res;
end;

procedure TBaseOpenFileHandler.OnRefreshDone(const Sender: IOmniBackgroundWorker;
    const workItem: IOmniWorkItem);
begin
  if workItem.IsExceptional or workItem.CancellationToken.IsSignalled then
    Exit;

  FCritSection.Acquire;
  try
    FreeAndNil(FPathsList);
    FPathsList := (WorkItem.Result.AsOwnedObject as TRefreshWorkItemResult).AcquireList;
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
  FreeAndNil(FPathsList);
  inherited;
end;

function TBaseOpenFileHandler.GetFilePaths(aWorkItem: IOmniWorkItem): TList<TFilePathRec>;
var
  FilePathsSet: ISet<string>;
  UpCaseFilePathsSet: ISet<string>;
  FileMasks: TArray<string>;

  procedure TryAppendFile(const aFilePath: string);
  var
    UpCaseFilePath: string;
  begin
    UpCaseFilePath := UpperCase(aFilePath);
    if UpCaseFilePathsSet.Contains(UpCaseFilePath) or not TStrUtils.MatchesMasks(aFilePath, FileMasks) then
      Exit;

    FilePathsSet.Add(aFilePath);
    UpCaseFilePathsSet.Add(UpCaseFilePath);
  end;

var
  DirPath, UpCaseDirpath: string;
  FilePath: string;
  DirPaths: IEnumerable<string>;
  Processed: ISet<string>;
begin
  Result := nil;
  Processed := THashSet<string>.Create(10000);
  FilePathsSet := THashSet<string>.Create(10000);
  UpCaseFilePathsSet := THashSet<string>.Create(10000);
  DirPaths := GetDirPaths;
  FileMasks := GetFileMasks;
  for DirPath in DirPaths do
  begin
    if aWorkItem.CancellationToken.IsSignalled then
      Exit;

    UpCaseDirpath := UpperCase(DirPath);
    if Processed.Contains(UpCaseDirpath) or not TDirectory.Exists(UpCaseDirpath) then
      Continue;

    Processed.Add(UpCaseDirpath);
    for FilePath in TDirectory.GetFiles(DirPath, '*', TSearchOption.soTopDirectoryOnly) do
    begin
      if aWorkItem.CancellationToken.IsSignalled then
        Exit;

      TryAppendFile(FilePath);
    end;
  end;

  for FilePath in GetAdditionFileList do
    TryAppendFile(FilePath);

  for FilePath in FilePathsSet do
  begin
    if Result = nil then
      Result := TList<TFilePathRec>.Create();
    Result.Add(TFilePathRec.Create(FilePath));
    if aWorkItem.CancellationToken.IsSignalled then
    begin
      FreeAndNil(Result);
      Exit;
    end;
  end;
end;

function TBaseOpenFileHandler.GetAdditionFileList: IEnumerable<string>;
begin
  Result := TCollectionsUtils.Empty<string>;
end;

function TBaseOpenFileHandler.GetFileMasks: TArray<string>;
begin
  Result := TArrayUtils.AsArray<string>(['*']);
end;

function TBaseOpenFileHandler.GetFilteredList: IObservableData<TList<string>>;
begin
  Result := FFilteredList;
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
  Rec: TFilePathRec;
  FuzzyRegExp, FilleRegExp: TPerlRegEx;
  Comparer: IComparer<string>;
  NormFilter: string;
begin
  Result := nil;
  if (aFilter = '') or (Length(aFilter) < 2) then
    Exit;

  FCritSection.Acquire;
  try
    if (FPathsList = nil) then
      Exit;
  finally
    FCritSection.Release;
  end;

  FilleRegExp := nil;
  FuzzyRegExp := nil;
  Coefs := nil;
  try
    Coefs := TDictionary<string, Single>.Create;
    FilterNGrams := TStrUtils.NGrams(TStrUtils.Normalize(aFilter), cstTokenLen);
    FilleRegExp := CreateRegEx(aFilter);
    NormFilter := TStrUtils.Normalize(aFilter);
    if not TStrUtils.HasChars(aFilter, ['/', '\', ':', '*', '?', '<', '>', '|']) then
    begin
      FuzzyRegExp := CreateRegEx(BuildRegExp(aFilter));
    end;

    FCritSection.Acquire;
    try
      for Rec in FPathsList do
      begin
        if aWorkItem.CancellationToken.IsSignalled then
          Exit;

        try
          if Pos(NormFilter, Rec.NormalizedFileName) <= 0 then
          begin
            FilleRegExp.Subject := Rec.FileName;
            if not FilleRegExp.Match then
            begin
              if FuzzyRegExp <> nil then
                FuzzyRegExp.Subject := Rec.FileName;
              if (FuzzyRegExp = nil) or not FuzzyRegExp.Match then
                Continue;
            end;
          end;
        except
          Continue;
        end;

        Ratio := TStrUtils.NGramsSimilarity(FilterNGrams, TStrUtils.NGrams(Rec.NormalizedFileName, cstTokenLen));
        Coefs.AddOrSetValue(Rec.FilePath, Ratio);
        if Result = nil then
          Result := TList<string>.Create;
        Result.Add(Rec.FilePath);
      end;
    finally
      FCritSection.Release;
    end;

    if Result <> nil then
    begin
      Comparer := TDelegatedComparer<string>.Create(function(const Left, Right: string): Integer
        begin
          Result := Priority[Left] - Priority[Right];
          if Result = 0 then
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
    FreeAndNil(FuzzyRegExp);
  end;
end;

function TBaseOpenFileHandler.GetPriority: IMap<string, Integer>;
begin
  if FPriority = nil then
  begin
    FPriority := THashMap<string, Integer>.Create;
    LoadPriority(FPriority);
  end;
  Result := FPriority;
end;

function TBaseOpenFileHandler.GetStatus: IObservableData<TOpenFileHandlerStatus>;
begin
  Result := FStatus;
end;

procedure TBaseOpenFileHandler.FilterPaths;
begin
  if Status.getValue in [ofht_Filtering] then
    FWorker.CancelAll;
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
var
  PriorityCopy: IMap<string, Integer>;
begin
  Priority[aFilePath] := Priority[aFilePath] + 1;
  PriorityCopy := THashMap<string, Integer>.Create(Priority);
  Parallel.Async(procedure
  begin
    StorePriority(PriorityCopy)
  end);
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
  FNormalizedFileName := TStrUtils.Normalize(FFileName);
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

function TRefreshWorkItemResult.AcquireList: TList<TFilePathRec>;
begin
  Result := List;
  List := nil;
end;

constructor TRefreshWorkItemResult.Create(aList: TList<TFilePathRec>);
begin
  inherited Create;
  List := aList;
end;

destructor TRefreshWorkItemResult.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

end.