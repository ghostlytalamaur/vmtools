unit vm.debug;

interface

uses
  cndebug;


type
  TAbstractLogger = class
  public type
    TLogType = (
      lpInfo,
      lpWarning,
      lpError,
      lpDebug,
      ltGroupStart,
      ltGroupEnd
    );

  protected
    procedure Log(aPriority: TLogType; const aMsg: string); virtual; abstract;
  public
    procedure startGroup(const aMsg: string);
    procedure endGroup(const aMsg: string);
    procedure i(const aMsg: string); overload;
    procedure i(const aMsg: string; const aArgs: array of const); overload;
    procedure d(const aMsg: string); overload;
    procedure d(const aMsg: string; const aArgs: array of const); overload;
    procedure e(const aMsg: string); overload;
    procedure e(const aMsg: string; const aArgs: array of const); overload;
  end;


function LogEnterLeave(const aMsg: string; aTag: string = ''): IInterface;
function Logger: TAbstractLogger;
procedure SetLogger(aLogger: TAbstractLogger);

implementation

uses
  sysutils;

var
  __Logger: TAbstractLogger = nil;
  __NullLogger: TAbstractLogger = nil;

type
  TNullLogger = class(TAbstractLogger)
  protected
    procedure Log(aPriority: TAbstractLogger.TLogType; const aMsg: string); override;
  end;

  TEnterLeaveLogger = class(TInterfacedObject)
  private
    FMsg: string;
    FTag: string;
  public
    constructor Create(const aMsg, aTag: string);
    destructor Destroy; override;
  end;

{ TEnterLeaveLogger }

constructor TEnterLeaveLogger.Create(const aMsg, aTag: string);
begin
  inherited Create;
  FMsg := aMsg;
  FTag := aTag;
  Logger.startGroup(FMsg);
end;

destructor TEnterLeaveLogger.Destroy;
begin
  Logger.endGroup(FMsg);
  inherited;
end;


function LogEnterLeave(const aMsg: string; aTag: string = ''): IInterface;
begin
  Result := TEnterLeaveLogger.Create(aMsg, aTag);
end;

function Logger: TAbstractLogger;
begin
  if __Logger <> nil then
  begin
    Result := __Logger;
  end
  else
  begin
    if __NullLogger = nil then
      __NullLogger := TNullLogger.Create;
    Result := __NullLogger;
  end;
end;

procedure SetLogger(aLogger: TAbstractLogger);
begin
  if __Logger = aLogger then
    Exit;

  FreeAndNil(__Logger);
  __Logger := aLogger;
end;

{ TAbstractLogger }

procedure TAbstractLogger.d(const aMsg: string);
begin
  Log(lpDebug, aMsg);
end;

procedure TAbstractLogger.d(const aMsg: string; const aArgs: array of const);
begin
  Log(lpDebug, Format(aMsg, aArgs));
end;

procedure TAbstractLogger.e(const aMsg: string);
begin
  Log(lpError, aMsg);
end;

procedure TAbstractLogger.e(const aMsg: string; const aArgs: array of const);
begin
  Log(lpError, Format(aMsg, aArgs));
end;

procedure TAbstractLogger.endGroup(const aMsg: string);
begin
  Log(ltGroupEnd, aMsg);
end;

procedure TAbstractLogger.i(const aMsg: string; const aArgs: array of const);
begin
  Log(lpInfo, Format(aMsg, aArgs));
end;

procedure TAbstractLogger.startGroup(const aMsg: string);
begin
  Log(ltGroupStart, aMsg);
end;

procedure TAbstractLogger.i(const aMsg: string);
begin
  Log(lpInfo, aMsg);
end;

{ TNullLogger }

procedure TNullLogger.Log(aPriority: TAbstractLogger.TLogType; const aMsg: string);
begin
end;

initialization

finalization
  FreeAndNil(__Logger);
  FreeAndNil(__NullLogger);

end.
