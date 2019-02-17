unit vm.debug;

interface

uses
  cndebug, generics.collections;


type
  TAbstractLogger = class
  public type
    TLogLevel = (
      llNone,
      llInfo,
      llWarning,
      llError,
      llDebug
    );
    TLogType = (
      ltInfo,
      ltWarning,
      ltError,
      ltDebug,
      ltGroupStart,
      ltGroupEnd
    );

  private
    FLogLevel: TLogLevel;

    procedure SendLog(aLogType: TAbstractLogger.TLogType; const aMsg: string); overload;
    procedure SendLog(aLogType: TAbstractLogger.TLogType; const aMsg: string; const aArgs: array of const); overload;
  protected
    procedure Log(aLogType: TAbstractLogger.TLogType; const aMsg: string); virtual; abstract;
    function ShouldLog(aLogType: TAbstractLogger.TLogType): Boolean; virtual;
    function FormatMessage(const aMsg: string; const aArgs: array of const): string; virtual;
  public
    constructor Create(aLogLevel: TAbstractLogger.TLogLevel);
    procedure startGroup(const aMsg: string);
    procedure endGroup(const aMsg: string);
    procedure i(const aMsg: string); overload;
    procedure i(const aMsg: string; const aArgs: array of const); overload;
    procedure d(const aMsg: string); overload;
    procedure d(const aMsg: string; const aArgs: array of const); overload;
    procedure e(const aMsg: string); overload;
    procedure e(const aMsg: string; const aArgs: array of const); overload;

    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

  TMultiLogger = class(TAbstractLogger)
  private
    FLoggers: TDictionary<string, TAbstractLogger>;
  protected
    function ShouldLog(aLogType: TAbstractLogger.TLogType): Boolean; override;
    procedure Log(aType: TAbstractLogger.TLogType; const aMsg: string); override;
  public
    procedure AppendLogger(aKey: string; aLogger: TAbstractLogger);
  end;


function LogEnterLeave(const aMsg: string; aTag: string = ''): IInterface;
function Logger: TMultiLogger;

implementation

uses
  sysutils;

var
  __Logger: TMultiLogger = nil;

type
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

function Logger: TMultiLogger;
begin
  if __Logger = nil then
    __Logger := TMultiLogger.Create(llDebug);
  Result := __Logger;
end;

{ TAbstractLogger }

procedure TAbstractLogger.d(const aMsg: string);
begin
  SendLog(ltDebug, aMsg);
end;

constructor TAbstractLogger.Create(aLogLevel: TAbstractLogger.TLogLevel);
begin
  inherited Create;
  FLogLevel := aLogLevel;
end;

procedure TAbstractLogger.d(const aMsg: string; const aArgs: array of const);
begin
  SendLog(ltDebug, aMsg, aArgs);
end;

procedure TAbstractLogger.e(const aMsg: string);
begin
  SendLog(ltError, aMsg);
end;

procedure TAbstractLogger.e(const aMsg: string; const aArgs: array of const);
begin
  SendLog(ltError, aMsg, aArgs);
end;

procedure TAbstractLogger.endGroup(const aMsg: string);
begin
  SendLog(ltGroupEnd, aMsg);
end;

function TAbstractLogger.FormatMessage(const aMsg: string; const aArgs: array of const): string;
begin
  Result := Format(aMsg, aArgs);
end;

procedure TAbstractLogger.i(const aMsg: string; const aArgs: array of const);
begin
  SendLog(ltInfo, aMsg, aArgs);
end;

function TAbstractLogger.ShouldLog(aLogType: TAbstractLogger.TLogType): Boolean;
begin
  case LogLevel of
    llNone:    Result := False;
    llInfo:    Result := aLogType = ltInfo;
    llWarning: Result := aLogType in [ltInfo, ltWarning];
    llError:   Result := aLogType in [ltInfo, ltWarning, ltError];
    llDebug:   Result := aLogType in [ltInfo, ltWarning, ltError, ltDebug, ltGroupStart, ltGroupEnd];
  else         Result := False;
  end;
end;

procedure TAbstractLogger.SendLog(aLogType: TAbstractLogger.TLogType; const aMsg: string);
begin
  if ShouldLog(aLogType) then
    Log(aLogType, aMsg);
end;

procedure TAbstractLogger.SendLog(aLogType: TAbstractLogger.TLogType; const aMsg: string; const aArgs: array of const);
begin
  if ShouldLog(aLogType) then
    Log(aLogType, FormatMessage(aMsg, aArgs));
end;

procedure TAbstractLogger.startGroup(const aMsg: string);
begin
  SendLog(ltGroupStart, aMsg);
end;

procedure TAbstractLogger.i(const aMsg: string);
begin
  SendLog(ltInfo, aMsg);
end;

{ TMultiLogger }

procedure TMultiLogger.AppendLogger(aKey: string; aLogger: TAbstractLogger);
begin
  if aLogger = nil then
    Exit;
  if FLoggers = nil then
    FLoggers := TObjectDictionary<string, TAbstractLogger>.Create([doOwnsValues]);
  FLoggers.AddOrSetValue(aKey, aLogger);
end;

procedure TMultiLogger.Log(aType: TAbstractLogger.TLogType; const aMsg: string);
var
  L: TAbstractLogger;
begin
  if FLoggers <> nil then
    for L in FLoggers.Values do
      L.SendLog(aType, aMsg);
end;

function TMultiLogger.ShouldLog(aLogType: TAbstractLogger.TLogType): Boolean;
begin
  Result := (FLoggers <> nil) and (FLoggers.Count > 0) and inherited;
end;

initialization

finalization
  FreeAndNil(__Logger);

end.
