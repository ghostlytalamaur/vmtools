unit vm.ide.actions.manager;

{$I cond_define.inc}

interface

uses
  vmsys, classes, actnlist, generics.collections, {$IFDEF DELPHIX_BERLIN_UP}actions, {$ENDIF} vm.common.updatestack, menus,
  base_params, inifiles;

type
  TVMActionManagerParams = class(TBaseParams)
  strict private const
    cst_Reg_Section = 'VMActionManager';
    cst_Reg_ActNamePrefix = 'ActionName';
    cst_Reg_ActShortCutPrefix = 'ActionShortCut';
  strict private
    FActionToShortCut: TDictionary<string, TShortCut>;
  strict protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetDefault; override;

    function GetActionShortCut(aName: string; out ShortCut: TShortCut): Boolean;
    procedure SetActionShortCut(aName: string; aShortCut: TShortCut);
  end;

  TVMActionManager = class(TExtObject)
  private const
    cstInvalidBindingIndex = -1;
  private
    FParams: TVMActionManagerParams;
    FMainMenu: TMenuItem;
    FOwnActionList: TList<TAction>;
    FUpdateStack: TUpdateStack;
    FMenuInstalled: Boolean;
    FInstallingKeyboardBinding: Boolean;
    FKeyboardBindingIndex: Integer;

    function FindAction(aList: TCustomActionList; aShortCut: TShortCut): TContainedAction; overload;
    function FindAction(aList: TCustomActionList; aName: string): TContainedAction; overload;

    function GenerateActionName(const AActionName: string): string;
    function GenerateMenuActionName(const AActionName: string): string;
    function GetValidControlName(const aActionName: string): string;

    function UpdateShortCut(aName: string; aShortCut: TShortCut): TShortCut;
    function DoRegisterAction(aName, aCaption: string; aShortCut: TShortCut; aEvent: TNotifyEvent): TContainedAction;

    procedure InstallKeyboardBindings;
    procedure RemoveKeyboardBindings;
    procedure InstallMenu;
  protected
    function GetIdeActionList: TCustomActionList;
    function GetMainMenu: TMenuItem;

    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterInternalAction(const aCaption: string; aEvent: TNotifyEvent; aShortCut: TShortCut);
    procedure RegisterInternalMenuAction(const aCaption: string; aEvent: TNotifyEvent; aShortCut: TShortCut);
    function UnRegisterInternalAction(const aName: string): Boolean;
    function UnRegisterInternalMenuAction(const aName: string): Boolean;

    function RegisterAction(aName, aCaption: string; aShortCut: TShortCut; aEvent: TNotifyEvent): TContainedAction;
    function RegisterMenuAction(aName, aCaption: string; aShortCut: TShortCut; aEvent: TNotifyEvent): TContainedAction;
    function UnRegsiterAction(aName: string): Boolean;
    function UnRegisterMenuAction(aName: string): Boolean;

    function ChangeShortCut(aName: string; aShortCut: TShortCut): Boolean;
    function IsShortCutAvailable(aShortCut: TShortCut): Boolean; overload;
    function IsShortCutAvailable(aShortCut: TShortCut; out Action: TContainedAction): Boolean; overload;

    function FindAction(aShortCut: TShortCut): TAction; overload;
    function FindAction(aName: string): TAction ; overload;

    procedure BeginUpdate;
    procedure EndUpdate;

    property IdeActionList: TCustomActionList read GetIdeActionList;
    property OwnActionList: TList<TAction> read FOwnActionList;
    property MainMenu: TMenuItem read GetMainMenu;
  end;

implementation

uses
  sysutils, vmtools_cst, vm_ide_utils, ToolsAPI, forms, vm.debug;

type
  TVMKeyboardBinding = class(TNotifierObject, IOTAKeyboardBinding)
  private
    FActionManager: IObjectHolder<TVMActionManager>;

    { IOTAKeyboardBinding }
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    procedure KeyBindingHandler(const Context: IOTAKeyContext; KeyCode: TShortCut;
        var BindingResult: TKeyBindingResult);
  public
    constructor Create(aActionManager: TVMActionManager);
  end;

{ TVMActionManager }

constructor TVMActionManager.Create;
begin
  inherited Create;
  FParams := TVMActionManagerParams.Create;
  FUpdateStack := TUpdateStack.Create(DoBeginUpdate, DoEndUpdate);
  FOwnActionList := TObjectList<TAction>.Create;
end;

destructor TVMActionManager.Destroy;
begin
  RemoveKeyboardBindings;
  while FOwnActionList.Count > 0 do
  begin
    UnRegsiterAction(FOwnActionList.First.Name);
  end;
  FParams.WriteParams;
  FreeAndNil(FOwnActionList);
  FreeAndNil(FMainMenu);
  FreeAndNil(FUpdateStack);
  FreeAndNil(FParams);
  inherited;
end;

procedure TVMActionManager.BeginUpdate;
begin
  FUpdateStack.BeginUpdate;
end;

procedure TVMActionManager.EndUpdate;
begin
  FUpdateStack.EndUpdate;
end;

procedure TVMActionManager.DoBeginUpdate;
begin
  RemoveKeyboardBindings;
end;

procedure TVMActionManager.DoEndUpdate;
begin
  if Assigned(Application) and (csDestroying in Application.ComponentState) then
    Exit;

  InstallMenu;
  InstallKeyboardBindings;
end;

function TVMActionManager.ChangeShortCut(aName: string; aShortCut: TShortCut): Boolean;
var
  Act: TAction;
begin
  BeginUpdate;
  try
    Act := FindAction(aName);
    Result := Act <> nil;
    if Result then
      Act.ShortCut := aShortCut;
  finally
    EndUpdate;
  end;
end;

function TVMActionManager.IsShortCutAvailable(aShortCut: TShortCut): Boolean;
var
  Act: TContainedAction;
begin
  Result := IsShortCutAvailable(aShortCut, Act);
end;

function TVMActionManager.IsShortCutAvailable(aShortCut: TShortCut; out Action: TContainedAction): Boolean;
begin
  if aShortCut = 0 then
  begin
    Action := nil;
    Exit(True);
  end;

  Action := FindAction(IdeActionList, aShortCut);
  if Action = nil then
    Action := FindAction(aShortCut);
  Result := Action = nil;
end;

function TVMActionManager.FindAction(aList: TCustomActionList; aShortCut: TShortCut): TContainedAction;
var
  I: Integer;
begin
  Result := nil;
  if aList = nil then
    Exit;

  for I := 0 to aList.ActionCount - 1 do
    if (aList[I] as TCustomAction).ShortCut = aShortCut then
      Exit(aList[I]);
end;

function TVMActionManager.FindAction(aList: TCustomActionList; aName: string): TContainedAction;
var
  I: Integer;
begin
  Result := nil;
  if aList = nil then
    Exit;

  for I := 0 to aList.ActionCount - 1 do
    if aList[I].Name = aName then
      Exit(aList[I]);
end;

function TVMActionManager.FindAction(aShortCut: TShortCut): TAction;
var
  List: TList<TAction>;
  I: Integer;
begin
  Result := nil;
  List := OwnActionList;
  if List = nil then
    Exit;

  for I := 0 to List.Count - 1 do
    if (List[I] as TCustomAction).ShortCut = aShortCut then
      Exit(List[I]);
end;

function TVMActionManager.FindAction(aName: string): TAction;
var
  List: TList<TAction>;
  I: Integer;
begin
  Result := nil;
  List := OwnActionList;
  if List = nil then
    Exit;

  for I := 0 to List.Count - 1 do
    if List[I].Name = aName then
      Exit(List[I]);
end;

function TVMActionManager.GenerateActionName(const AActionName: string): string;
begin
  Result := cstVMActionQualifier + GetValidControlName(aActionName);
end;

function TVMActionManager.GenerateMenuActionName(const AActionName: string): string;
begin
  Result := cstVMMenuQualifier + GetValidControlName(AActionName);
end;

function TVMActionManager.GetIdeActionList: TCustomActionList;
var
  Srv: INTAServices;
begin
  if Supports(BorlandIDEServices, INTAServices, Srv) then
    Result := Srv.ActionList
  else
    Result := nil;
end;

function TVMActionManager.GetMainMenu: TMenuItem;
begin
  if FMainMenu = nil then
  begin
    FMainMenu := TMenuItem.Create(nil);
    FMainMenu.Name := 'VMDelphiTools_menu';
    FMainMenu.Caption := '&VM Tools';
  end;
  Result := FMainMenu;
end;

function TVMActionManager.GetValidControlName(const aActionName: string): string;
begin
  Result := StringReplace(AActionName, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '', [rfReplaceAll]);
  Result := StringReplace(Result, '!', '', [rfReplaceAll]);
  Result := StringReplace(Result, '.', '', [rfReplaceAll]);
end;

procedure TVMActionManager.InstallKeyboardBindings;
var
  IKeyboardServices: IOTAKeyboardServices;
  IKeyboardBinding: IOTAKeyboardBinding;
  List: TList<TAction>;
begin
  // Starting with Delphi XE3 apparently this gets called again from within
  // the call to IKeyboardServices.AddKeyboardBinding, so FKeyboardBindingIndex
  // isn't set. Therefore this workaround: It prevents the second call
  // and the resulting exception(s)
  if FInstallingKeyboardBinding then
    Exit;

  FInstallingKeyboardBinding := True;
  try
    // XE5, and probably older versions, will AV when you add a keyboard binding
    // (IKeyboardServices.AddKeyboardBinding), when Delphi is shutting down.
    // The AV is in a TMenuItem which is nil.
    if Assigned(Application) and (csDestroying in Application.ComponentState) then
      Exit;

    List := OwnActionList;
    if (List = nil) or (List.Count = 0) then
      Exit;

    Assert(FKeyboardBindingIndex = cstInvalidBindingIndex);

    if not Supports(BorlandIDEServices, IOTAKeyboardServices, IKeyboardServices) then
      Exit;

    IKeyboardBinding := TVMKeyboardBinding.Create(Self);
    try
      FKeyboardBindingIndex := IKeyboardServices.AddKeyboardBinding(IKeyboardBinding);
    except
      on E: Exception do
        raise E.Create('Error registering keyboard shortcuts with IDE: ' + E.Message);
    end;
  finally
    FInstallingKeyboardBinding := False;
  end;
end;

procedure TVMActionManager.InstallMenu;
var
  NTAS: INTAServices;
  ToolsMenu: TMenuItem;
begin
  if FMenuInstalled or (FMainMenu = nil) or (FMainMenu.Count = 0) then
    Exit;

  NTAS := (BorlandIDEServices as INTAServices);
  if (NTAS = nil) or (NTAS.MainMenu = nil) then
    Exit;

  ToolsMenu := NTAS.MainMenu.Items.Find('Tools');
  if ToolsMenu <> nil then
    ToolsMenu.Insert(0, FMainMenu)
  else
    NTAS.MainMenu.Items.Insert(NTAS.MainMenu.Items.Count, FMainMenu);
  FMenuInstalled := True;
end;

procedure TVMActionManager.RegisterInternalAction(const aCaption: string; aEvent: TNotifyEvent; aShortCut: TShortCut);
begin
  RegisterAction(GenerateActionName(aCaption), aCaption, aShortCut, aEvent);
end;

procedure TVMActionManager.RegisterInternalMenuAction(const aCaption: string; aEvent: TNotifyEvent; aShortCut: TShortCut);
begin
  RegisterMenuAction(GenerateMenuActionName(aCaption), aCaption, aShortCut, aEvent);
end;

function TVMActionManager.UnRegisterInternalAction(const aName: string): Boolean;
begin
  Result := UnRegsiterAction(GenerateActionName(aName));
end;

function TVMActionManager.UnRegisterInternalMenuAction(const aName: string): Boolean;
begin
  Result := UnRegisterMenuAction(GenerateMenuActionName(aName));
end;

function TVMActionManager.UpdateShortCut(aName: string; aShortCut: TShortCut): TShortCut;
var
  Act: TContainedAction;
begin
  if not FParams.GetActionShortCut(aName, Result) then
    Result := aShortCut;
  if not IsShortCutAvailable(aShortCut, Act) then
    Result := 0;
end;

function TVMActionManager.DoRegisterAction(aName, aCaption: string; aShortCut: TShortCut;
    aEvent: TNotifyEvent): TContainedAction;
var
  Act: TAction;
begin
  BeginUpdate;
  try
    Act := TAction.Create(nil);
    Act.Name := aName;
    Act.Category := cstVMActionCategory;
    Act.Caption := aCaption;
    Act.ShortCut := aShortCut;
    Act.OnExecute := aEvent;
    Act.ActionList := IdeActionList;

    FOwnActionList.Add(Act);
    Result := Act;
  finally
    EndUpdate;
  end;
end;

function TVMActionManager.RegisterAction(aName, aCaption: string; aShortCut: TShortCut;
    aEvent: TNotifyEvent): TContainedAction;
var
  List: TCustomActionList;
  ShortCut: TShortCut;
begin
  Result := nil;
  List := IdeActionList;
  if (List = nil) or (FindAction(List, aName) <> nil) or (FindAction(aName) <> nil) then
    Exit;

  ShortCut := UpdateShortCut(aName, aShortCut);
  Result := DoRegisterAction(aName, aCaption, ShortCut, aEvent);
  if Result <> nil then
    FParams.SetActionShortCut(aName, ShortCut);
end;

function TVMActionManager.RegisterMenuAction(aName, aCaption: string; aShortCut: TShortCut;
    aEvent: TNotifyEvent): TContainedAction;
var
  Item: TMenuItem;
begin
  Result := nil;
  if (FindAction(GetIdeActionList, aName) <> nil) or (FindAction(aName) <> nil) then
    Exit;

  BeginUpdate;
  try
    aShortCut := UpdateShortCut(aName, aShortCut);

    Result := DoRegisterAction(aName, aCaption, aShortCut, aEvent);
    if Result = nil then
      Exit;

    Item := TMenuItem.Create(MainMenu);
    Item.Name := aName;
    Item.Caption := aCaption;
    Item.ShortCut := aShortCut;
    Item.Action := Result;

    MainMenu.Add(Item);
    FParams.SetActionShortCut(aName, aShortCut);
  finally
    EndUpdate;
  end;
end;

procedure TVMActionManager.RemoveKeyboardBindings;
var
  IKeyboardServices: IOTAKeyboardServices;
begin
  if FKeyboardBindingIndex = cstInvalidBindingIndex then
    Exit;

  if not Supports(BorlandIDEServices, IOTAKeyboardServices, IKeyboardServices) then
    Exit;

  try
    IKeyboardServices.RemoveKeyboardBinding(FKeyboardBindingIndex);
  except
    on E: Exception do
      raise E.Create('Error removing keyboard shortcuts from IDE: ' +E.Message);
  end;
  FKeyboardBindingIndex := cstInvalidBindingIndex;
end;

function TVMActionManager.UnRegsiterAction(aName: string): Boolean;
var
  Act: TAction;
begin
  Act := FindAction(aName);
  Result := Act <> nil;
  if Result then
  begin
    Act.ActionList := nil;
    FOwnActionList.Remove(Act);
  end;
end;

function TVMActionManager.UnRegisterMenuAction(aName: string): Boolean;

  function DoFindMenu(aRoot: TMenuItem; aName: string): TMenuItem;
  var
    I: Integer;
  begin
    Result := nil;
    if aRoot = nil then
      Exit;

    for I := 0 to aRoot.Count - 1 do
      if aRoot[I].Name = aName then
        Exit(aRoot[I])
      else
      begin
        Result := DoFindMenu(aRoot[I], aName);
        if Result <> nil then
          Exit;
      end;
  end;

var
  MenuItem: TMenuItem;
begin
  LogEnterLeave('TVMActionManager.UnRegisterMenuAction');
  MenuItem := DoFindMenu(FMainMenu, aName);
  Result := (MenuItem <> nil) and (MenuItem.Parent <> nil) and UnRegsiterAction(aName);
  if Result then
  begin
    MenuItem.Parent.Remove(MenuItem);
    FreeAndNil(MenuItem);
    Logger.LogFmt('MenuAction %s unregistered', [aName]);
  end;

  if Result and (FMainMenu.Count = 0) and (FMainMenu.Parent <> nil) and FMenuInstalled then
  begin
    FMainMenu.Parent.Remove(FMainMenu);
    FreeAndNil(FMainMenu);
    FMenuInstalled := False;
  end;
end;

{ TGxKeyboardBinding }

constructor TVMKeyboardBinding.Create(aActionManager: TVMActionManager);
begin
  inherited Create;
  FActionManager := TObjectHolder<TVMActionManager>.Create(aActionManager, False);
end;

function TVMKeyboardBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TVMKeyboardBinding.GetDisplayName: string;
begin
  Result := cstVMKeyBindingDispName;
end;

function TVMKeyboardBinding.GetName: string;
begin
  Result := cstVMKeyBindingName;
end;

procedure TVMKeyboardBinding.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
const
  DefaultKeyBindingsFlag = kfImplicitShift + kfImplicitModifier + kfImplicitKeypad;
var
  I: Integer;
  KeyboardName: string;
  ActionList: TList<TAction>;
begin
  LogEnterLeave('TVMKeyboardBinding.BindKeyboard');
  KeyboardName := '';
  if not FActionManager.IsAlive then
    Exit;

  ActionList := FActionManager.Obj.OwnActionList;
  Logger.LogInteger(ActionList.Count, 'ActionList.Count');
  for I := 0 to ActionList.Count - 1 do
  begin
    Logger.LogFmt('Aciton %s; Shortcut: %s; HasActionList: %d', [ActionList[I].Name,
        ShortCutToText((ActionList[I] as TCustomAction).ShortCut), Ord(ActionList[I].ActionList <> nil)]);

    if ActionList[I].ActionList = nil then
      Continue;

    if (ActionList[I] as TCustomAction).ShortCut <> 0 then
      BindingServices.AddKeyBinding([(ActionList[I] as TCustomAction).ShortCut], KeyBindingHandler, nil,
          DefaultKeyBindingsFlag, KeyboardName);
  end;
end;

procedure TVMKeyboardBinding.KeyBindingHandler(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  Action: TContainedAction;
begin
  BindingResult := krUnhandled;

  if not FActionManager.IsAlive then
    Exit;

  Action := FActionManager.Obj.FindAction(KeyCode);
  Logger.LogFmt('TVMKeyboardBinding.KeyBindingHandler: KeyCode: %s; HasAction: %s', [
      ShortCutToText(KeyCode), BoolToStr(Action <> nil, True)]);
  if Action <> nil then
  begin
    BindingResult := krHandled;
    try
      Action.Execute;
    except
      on E: Exception do
      begin
        // If we don't handle these, the hotkey is passed to the editor (inserts
        // a character) or another expert (may show another error, dialog, etc.)
        ApplicationShowException(E);
      end;
    end;
  end;
end;

{ TVMActionManagerParams }

constructor TVMActionManagerParams.Create;
begin
  inherited Create;
  FActionToShortCut := TDictionary<string, TShortCut>.Create;
end;

destructor TVMActionManagerParams.Destroy;
begin
  FreeAndNil(FActionToShortCut);
  inherited;
end;

procedure TVMActionManagerParams.SetDefault;
begin
  if FActionToShortCut <> nil then
    FActionToShortCut.Clear;
end;

procedure TVMActionManagerParams.DoReadParams(aIni: TCustomIniFile);
var
  Names, ShortCuts: TArray<string>;
  I: Integer;
begin
  inherited;
  Names := ReadStrings(aIni, cst_Reg_Section, cst_Reg_ActNamePrefix);
  ShortCuts := ReadStrings(aIni, cst_Reg_Section, cst_Reg_ActShortCutPrefix);
  if Length(Names) <> Length(ShortCuts) then
    Exit;

  for I := 0 to High(Names) do
    SetActionShortCut(Names[I], TextToShortCut(ShortCuts[I]));
end;

procedure TVMActionManagerParams.DoWriteParams(aIni: TCustomIniFile);
var
  ShortCuts: TArray<string>;
  ShortCut: TShortCut;
  I: Integer;
begin
  inherited;

  SetLength(ShortCuts, FActionToShortCut.Count);
  I := 0;
  for ShortCut in FActionToShortCut.Values do
  begin
    ShortCuts[I] := ShortCutToText(ShortCut);
    Inc(I);
  end;

  WriteStrings(aIni, cst_Reg_Section, cst_Reg_ActNamePrefix, FActionToShortCut.Keys.ToArray);
  WriteStrings(aIni, cst_Reg_Section, cst_Reg_ActNamePrefix, ShortCuts);
end;

function TVMActionManagerParams.GetActionShortCut(aName: string; out ShortCut: TShortCut): Boolean;
begin
  Result := FActionToShortCut.TryGetValue(aName, ShortCut);
end;

procedure TVMActionManagerParams.SetActionShortCut(aName: string; aShortCut: TShortCut);
begin
  FActionToShortCut.AddOrSetValue(aName, aShortCut);
end;

end.
