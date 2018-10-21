unit vm_internal_int;

{$I cond_define.inc}

interface

uses
  classes, actnlist,
{$IFDEF DELPHIX_BERLIN_UP}
  actions,
{$ENDIF}
  sysutils, menus, ImgList, Graphics, observer, weak_ref, vm.ide.actions.manager;

type
  IDebugNotifier = interface
  ['{34300068-2AF0-468E-B706-BA61872AC397}']
    procedure SendError(const aMsg: string);
    procedure SendInfo(const aMsg: string);
  end;

  IWizzardsServices = interface
  ['{1BBFA3FF-C36F-4315-AB84-D69024C28BA0}']
    function GetActionManager: TVMActionManager;

    function StartingUp: Boolean;
    property ActionManager: TVMActionManager read GetActionManager;
  end;


implementation

end.
