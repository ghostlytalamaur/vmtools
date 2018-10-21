unit weak_ref;

interface

type
  IWeakRef = interface
  ['{6163C0FD-C271-47B7-B13B-3F891D205BF6}']
    function GetIntf: IUnknown;
    procedure Clear;
    function IsAlive: Boolean;

    property Intf: IUnknown read GetIntf;
  end;

  IWeakRefProvider = interface
  ['{E2921B18-1C28-4015-907F-40EB97397831}']
    function GetWeakRef: IWeakRef;

    property WeakRef: IWeakRef read GetWeakRef;
  end;

  TWeakRef = class(TInterfacedObject, IWeakRef)
  private
    FRef: IUnknown;
    function GetIntf: IUnknown;
  public
    constructor Create(aIntf: IUnknown);

    procedure Clear;
    function IsAlive: Boolean;

    property Intf: IUnknown read GetIntf;
  end;

implementation

{ TWeakRef }

procedure TWeakRef.Clear;
begin
  FRef := nil;
end;

constructor TWeakRef.Create(aIntf: IUnknown);
begin
  inherited Create;
  Pointer(FRef) := Pointer(aIntf);
//  FRef := Pointer(PT(@aIntf));
//  FRef := Pointer(FRef^);
end;

function TWeakRef.GetIntf: IUnknown;
begin
  Result := FRef;
//  Pointer(Pointer(@Result)^) := FRef;
//  IUnknown(Result)._AddRef;
end;

function TWeakRef.IsAlive: Boolean;
begin
  Result := FRef <> nil;
end;

end.
