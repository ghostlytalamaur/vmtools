unit wiz_openfile_handler;

interface

uses
  classes, observer, otlsync, otlcontainers, otleventmonitor, otltaskcontrol, otlcomm, otltask,
  otlcommon, generics.collections, base_params, inifiles, windows,
  openfile_handler, opt_impl;

type
  TOpenFileHandlerParams = class(TBaseParams)
  strict private const
    cst_Reg_Section = 'OpenFileHandlerParams';
    cst_Reg_IncludeBrowsingPaths = 'IncludeBrowsingPaths';
    cst_Reg_IncludeFromProjectGroup = 'IncludeFromProjectGroup';
    cst_Reg_FileMasks = 'FileMasks';
  private
    function GetFileMasks: string;
    function GetIncludeBrowsingPaths: Boolean;
    function GetIncludeFromProjectGroup: Boolean;
  strict protected
    function CreateTree: TParamsTree; override;
    procedure RegisterParams; override;
  public
    property IncludeFromProjectGroup: Boolean read GetIncludeFromProjectGroup;
    property IncludeBrowsingPaths: Boolean read GetIncludeBrowsingPaths;
    property FileMasks: string read GetFileMasks;
  end;

  TOpenFileHandler = class(TBaseOpenFileHandler)
  private
    FParams: TOpenFileHandlerParams;
    FParamsObserver: IParamsChangedObserver;
  protected
    function GetDirPaths: IEnumerable<string>; override;
    function GetFileMasks: TArray<string>; override;
    function GetAdditionFileList: IEnumerable<string>; override;
  public
    constructor Create(aParams: TOpenFileHandlerParams);
    procedure OpenFile(const aFilePath: string); override;
    property Params: TOpenFileHandlerParams read FParams;
  end;

implementation

uses
  vmsys, SysUtils, ioutils, masks, str_utils, generics.defaults, regularexpressionscore,
  otlparallel, OtlCollections, collections.array_utils, vm_ide_utils, collections.sets;

const
  cstValidFileMasks = '*.pas;*.dpr;*.inc;*.def;*.rc';

{ TFilesProvider }

constructor TOpenFileHandler.Create(aParams: TOpenFileHandlerParams);
begin
  inherited Create;
  FParams := aParams;
  FParamsObserver := TParamsObserver.Create(
    procedure
    begin
      InvalidatePaths;
      FParams.WriteParams;
    end);
  FParams.RegisterObserver(IParamsChangedObserver, FParamsObserver)
end;

function TOpenFileHandler.GetAdditionFileList: IEnumerable<string>;
begin
  Result := TVMOtaUtils.GetProjectFileList(Params.IncludeFromProjectGroup);
end;

function TOpenFileHandler.GetDirPaths: IEnumerable<string>;
var
  Paths: ISet<string>;
begin
  Paths := THashSet<string>.Create;
  Result := Paths;
  Paths.Add(TVMOtaUtils.GetProjectPathsList);
  if Params.IncludeBrowsingPaths then
    Paths.Add(TVMOtaUtils.GetBrowsingPathList);
  if Params.IncludeFromProjectGroup then
    Paths.Add(TVMOtaUtils.GetProjectGroupsPathsList);
end;

function TOpenFileHandler.GetFileMasks: TArray<string>;
var
  Masks: string;
begin
  Masks := FParams.FileMasks;
  Result := TArrayUtils.AsArray<string>(TStrUtils.Words(Masks , [';']));
  if Result = nil then
    Result := TArrayUtils.AsArray<string>(TStrUtils.Words(cstValidFileMasks, [';']));
end;

procedure TOpenFileHandler.OpenFile(const aFilePath: string);
begin
  if TVMOtaUtils.GetCurrentOpenFileName <> aFilePath then
    TGXOtaUtils.GxOtaGoToFileLineColumn(aFilePath, -1);
  inherited;
end;

{ TOpenFileHandlerParams }

function TOpenFileHandlerParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(cst_Reg_Section, 'Open file params');
end;

function TOpenFileHandlerParams.GetFileMasks: string;
begin
  Result := (Tree.ByKey[cst_Reg_FileMasks] as TStringParam).Value;
end;

function TOpenFileHandlerParams.GetIncludeBrowsingPaths: Boolean;
begin
  Result := (Tree.ByKey[cst_Reg_IncludeBrowsingPaths] as TBooleanParam).Value;
end;

function TOpenFileHandlerParams.GetIncludeFromProjectGroup: Boolean;
begin
  Result := (Tree.ByKey[cst_Reg_IncludeFromProjectGroup] as TBooleanParam).Value;
end;

procedure TOpenFileHandlerParams.RegisterParams;
begin
  inherited;
  Tree.RegisterParam(TBooleanParam.Create(cst_Reg_IncludeBrowsingPaths, 'Include browsing paths', True));
  Tree.RegisterParam(TBooleanParam.Create(cst_Reg_IncludeFromProjectGroup, 'Include project group paths', True));
  Tree.RegisterParam(TStringParam.Create(cst_Reg_FileMasks, 'File masks', cstValidFileMasks));
end;

end.
