unit vm.common.updatestack;

interface

type
  TUpdateStackEvent = procedure of object;

  TUpdateStack = class
  strict private
    FUpdateCount: Integer;
    FOnBeginUpdate: TUpdateStackEvent;
    FOnEndUpdate: TUpdateStackEvent;

    function GetIsUpdating: Boolean;
  public
    constructor Create(aOnBeginUpdte, aOnEndUpdate: TUpdateStackEvent);

    procedure BeginUpdate;
    procedure EndUpdate;

    property IsUpdating: Boolean read GetIsUpdating;
  end;

implementation

uses
  windows;

{ TUpdateStack }

procedure TUpdateStack.BeginUpdate;
begin
  InterlockedIncrement(FUpdateCount);
  if (FUpdateCount = 1) and Assigned(FOnBeginUpdate) then
    FOnBeginUpdate;
end;

constructor TUpdateStack.Create(aOnBeginUpdte, aOnEndUpdate: TUpdateStackEvent);
begin
  inherited Create;
  FOnBeginUpdate := aOnBeginUpdte;
  FOnEndUpdate := aOnEndUpdate;
end;

procedure TUpdateStack.EndUpdate;
begin
  InterlockedDecrement(FUpdateCount);
  Assert(FUpdateCount >= 0);
  if (FUpdateCount = 0) and Assigned(FOnEndUpdate) then
    FOnEndUpdate;
end;

function TUpdateStack.GetIsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

end.
