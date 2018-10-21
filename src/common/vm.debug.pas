unit vm.debug;

interface

uses
  cndebug;


function LogEnterLeave(const aMsg: string; aTag: string = ''): IInterface;
function Logger: TCnDebugger;

implementation

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
  CnDebugger.LogEnter(FMsg, FTag);
end;

destructor TEnterLeaveLogger.Destroy;
begin
  CnDebugger.LogLeave(FMsg, FTag);
  inherited;
end;



function LogEnterLeave(const aMsg: string; aTag: string = ''): IInterface;
begin
  Result := TEnterLeaveLogger.Create(aMsg, aTag);
end;

function Logger: TCnDebugger;
begin
  Result := CnDebugger;
end;

end.
