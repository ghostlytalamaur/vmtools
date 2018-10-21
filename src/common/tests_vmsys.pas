unit tests_vmsys;
interface
implementation

uses
  testframework, vmsys, sysutils, classes;

type
  TVMSysTests = class(TTestCase)
  published
    procedure TestFreeNotification;
    procedure TestFreeNotificationComponent;
  end;
{ TVMSymTests }

procedure TVMSysTests.TestFreeNotification;
var
  Obj1: TExtObject;
  Holder: IObjectHolder<TExtObject>;
begin
  Obj1 := TExtObject.Create;
  try
    Holder := TObjectHolder<TExtObject>.Create(Obj1, False);
    FreeAndNil(Obj1);
    CheckNull(Holder.Obj, 'Object must be nil.');
  finally
    FreeAndNil(Obj1);
  end;
end;

procedure TVMSysTests.TestFreeNotificationComponent;
var
  Obj1: TComponent;
  Holder: IObjectHolder<TComponent>;
begin
  Obj1 := TComponent.Create(nil);
  try
    Holder := TObjectHolder<TComponent>.Create(Obj1, False);
    FreeAndNil(Obj1);
    CheckNull(Holder.Obj, 'Object must be nil.');
  finally
    FreeAndNil(Obj1);
  end;
end;


initialization
  TestFramework.RegisterTest('common.vmsys', TVMSysTests.Suite);

end.