unit vm.cn_debug;

interface

uses
  vm.debug;

type
  TCnDebugLogger = class(TAbstractLogger)
  protected
    procedure Log(aType: TAbstractLogger.TLogType; const aMsg: string); override;
  end;

implementation

uses
  cndebug;

{ TCnDebugLogger }

procedure TCnDebugLogger.Log(aType: TAbstractLogger.TLogType; const aMsg: string);
begin
  case aType of
    ltInfo:       CnDebugger.LogMsg('[Info] ' + aMsg);
    ltWarning:    CnDebugger.LogMsg('[Warning] ' + aMsg);
    ltError:      CnDebugger.LogMsg('[Error] ' + aMsg);
    ltDebug:      CnDebugger.LogMsg('[Debug] ' + aMsg);
    ltGroupStart: CnDebugger.LogEnter(aMsg);
    ltGroupEnd:   CnDebugger.LogLeave(aMsg);
  end;
end;

end.
