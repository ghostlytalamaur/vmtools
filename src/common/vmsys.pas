unit vmsys;

interface

uses
  classes, sysutils, weak_ref, generics.collections;

type
  PComponent = ^TComponent;
  PExtObject = ^TExtObject;
  TExtObject = class(TObject)
  private
    FNotifyComponent: TComponent;
    FNotifiableFields: TList<PObject>;
    function GetNotifyComponent: TComponent;
    function GetNotifiableFields: TList<PObject>;
    procedure DoSetNotifiableObjectProperty(aField: PObject; aObj: TObject);
    function GetNotifyComponentFromField(aField: TObject): TComponent;

    property NotifiableFields: TList<PObject> read GetNotifiableFields;
    property NotifyComponent: TComponent read GetNotifyComponent;
  protected
    procedure SetNotifiableObjectProperty(aField: PExtObject; aObj: TExtObject); overload;
    procedure SetNotifiableObjectProperty(aField: PComponent; aObj: TComponent); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure FieldDestroyed(aField: PObject); virtual;
  public
    destructor Destroy; override;
  end;

  IObjectHolder<T: class> = interface
    function GetObj: T;

    function IsAlive: Boolean;
    property Obj: T read GetObj;
  end;

  TExtInterfacedObject = class(TExtObject, IInterface, IWeakRefProvider)
  private
    FWeakRef: IWeakRef;
    FShouldDestroyItself: Boolean;

    function GetWeakRef: IWeakRef;
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
    property ShouldDestroyItself: Boolean read FShouldDestroyItself write FShouldDestroyItself;
    property WeakRef: IWeakRef read GetWeakRef;
  end;

  TObjectHolder<T: class> = class(TExtInterfacedObject, IObjectHolder<T>)
  private
    FObj: T;
    FOwnObj: Boolean;
    FIsActiveFlag: Integer;

    function GetObj: T;
    function IsAlive: Boolean;
  protected
    procedure FieldDestroyed(aField: PObject); override;
  public
    constructor Create(aObj: T; aOwnObject: Boolean);
    destructor Destroy; override;
  end;

  EBaseInterfacedObjectException = class(Exception);


function Compare(A, B: Single): Integer;

implementation

uses
  windows;

type
  TNotifyComponent = class(TComponent)
  private
    FOwner: TExtObject;
  protected
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner: TExtObject); reintroduce;
  end;

function Compare(A, B: Single): Integer;
begin
  if A > B then
    Result := 1
  else if A < B then
    Result := -1
  else
    Result := 0;
end;

procedure TExtInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TExtInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    raise EBaseInterfacedObjectException.Create('You must release all interfaces before destroy object');
end;

destructor TExtInterfacedObject.Destroy;
begin
  if FWeakRef <> nil then
    FWeakRef.Clear;
  inherited;
end;

function TExtInterfacedObject.GetWeakRef: IWeakRef;
begin
  if FWeakRef = nil then
    FWeakRef := TWeakRef.Create(Self);
  Result := FWeakRef;
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TExtInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TExtInterfacedObject(Result).ShouldDestroyItself := True;
  TExtInterfacedObject(Result).FRefCount := 1;
end;

function TExtInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TExtInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TExtInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) and FShouldDestroyItself then
    Destroy;
end;

{ TExtObject }

destructor TExtObject.Destroy;
begin
  FreeAndNil(FNotifiableFields);
  FNotifyComponent.Free; // dont use FreeAndNil().
  inherited;
end;

function TExtObject.GetNotifiableFields: TList<PObject>;
begin
  if FNotifiableFields = nil then
    FNotifiableFields := TList<PObject>.Create;
  Result := FNotifiableFields;
end;

function TExtObject.GetNotifyComponent: TComponent;
begin
  if FNotifyComponent = nil then
    FNotifyComponent := TNotifyComponent.Create(Self);
  Result := FNotifyComponent;
end;

procedure TExtObject.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  Field: PObject;
begin
  if Operation <> opRemove then
    Exit;

  for I := 0 to NotifiableFields.Count - 1 do
  begin
    Field := NotifiableFields[I];
    if (Field = nil) or (Field^ = nil) then
      Continue;

    if (Field^ is TExtObject) and (TExtObject(Field^).FNotifyComponent = aComponent) or
        (Field^ is TComponent) and (Field^ = aComponent) then
    begin
      Field^ := nil;
      FieldDestroyed(Field);
    end;
  end;
end;

procedure TExtObject.FieldDestroyed(aField: PObject);
begin
end;

function TExtObject.GetNotifyComponentFromField(aField: TObject): TComponent;
begin
  if aField is TComponent then
    Result := TComponent(aField)
  else if aField is TExtObject then
    Result := TExtObject(aField).NotifyComponent
  else
    Result := nil;
end;

procedure TExtObject.DoSetNotifiableObjectProperty(aField: PObject; aObj: TObject);
var
  Idx: Integer;
begin
  if aField = nil then
    Exit;

  Idx := NotifiableFields.IndexOf(aField);
  if (Idx >= 0) and (Idx < NotifiableFields.Count) and (aField^ <> nil) then
  begin
    NotifyComponent.RemoveFreeNotification(GetNotifyComponentFromField(aField^));
  end;

  aField^ := aObj;

  if (aObj <> nil) then
  begin
    NotifyComponent.FreeNotification(GetNotifyComponentFromField(aField^));
    if (Idx < 0) or (Idx >= NotifiableFields.Count) then
      NotifiableFields.Add(aField);
  end;
end;

procedure TExtObject.SetNotifiableObjectProperty(aField: PExtObject; aObj: TExtObject);
begin
  DoSetNotifiableObjectProperty(PObject(aField), aObj);
end;

procedure TExtObject.SetNotifiableObjectProperty(aField: PComponent; aObj: TComponent);
begin
  DoSetNotifiableObjectProperty(PObject(aField), aObj);
end;
{ TNotifyComponent }

constructor TNotifyComponent.Create(aOwner: TExtObject);
begin
  inherited Create(nil);
  FOwner := aOwner;
end;

procedure TNotifyComponent.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if FOwner <> nil then
    FOwner.Notification(aComponent, Operation);
end;

{ TObjectHolder<T> }

constructor TObjectHolder<T>.Create(aObj: T; aOwnObject: Boolean);
begin
  inherited Create;
  FOwnObj := aOwnObject;
  if FOwnObj then
    FObj := aObj
  else if aObj is TExtObject then
    SetNotifiableObjectProperty(@FObj, aObj as TExtObject)
  else if aObj is TComponent then
    SetNotifiableObjectProperty(@FObj, aObj as TComponent)
  else
    FObj := aObj;
  if (FObj <> nil) then
    InterlockedIncrement(FIsActiveFlag);
end;

destructor TObjectHolder<T>.Destroy;
begin
  if FOwnObj then
    FreeAndNil(FObj);
  inherited;
end;

procedure TObjectHolder<T>.FieldDestroyed(aField: PObject);
begin
  inherited;
  if (FIsActiveFlag > 0) then
    InterlockedDecrement(FIsActiveFlag);
end;

function TObjectHolder<T>.GetObj: T;
begin
  Result := FObj;
end;

function TObjectHolder<T>.IsAlive: Boolean;
begin
  Result := FIsActiveFlag > 0;
end;

end.
