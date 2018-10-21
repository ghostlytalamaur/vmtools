unit vm.ide.actions.options_handler;

{$I cond_define.inc}

interface

uses
  forms, vm.ide.actions.manager, vm.ide.options.handler, actnlist,
  {$IFDEF DELPHIX_BERLIN_UP}actions, {$ENDIF} generics.collections,
  vm.ide.actions.options_frame, classes;

type
  TVMActionManagerOptionsHandler = class(TVMOptionsHandler)
  private
    FActionManager: TVMActionManager;
    FData: TList<TActionShortCutData>;

    function IsShortcutAvailable(aShortCut: TShortCut; out Action: TContainedAction): Boolean;
    procedure UpdateData;
  protected
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
    function ValidateContents: Boolean; override;
  public
    constructor Create(aActionManager: TVMActionManager);
    destructor Destroy; override;
  end;

implementation

uses
  sysutils;

{ TVMActionManagerOptionsHandler }

constructor TVMActionManagerOptionsHandler.Create(aActionManager: TVMActionManager);
begin
  inherited Create('ShortCuts', TActionManagerFrame);
  SetNotifiableObjectProperty(@FActionManager, aActionManager);
end;

destructor TVMActionManagerOptionsHandler.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TVMActionManagerOptionsHandler.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  if aFrame is TActionManagerFrame then
  begin
    UpdateData;
    TActionManagerFrame(aFrame).SetData(FData, IsShortcutAvailable);
  end;
end;

procedure TVMActionManagerOptionsHandler.DialogClosed(Accepted: Boolean);
var
  ActData: TActionShortCutData;
begin
  if not Accepted or (FData = nil) or (FActionManager = nil) then
  begin
    FreeAndNil(FData);
    Exit;
  end;

  FActionManager.BeginUpdate;
  try
    for ActData in FData do
      FActionManager.ChangeShortCut(ActData.Name, ActData.ShortCut);
  finally
    FActionManager.EndUpdate;
  end;
end;

function TVMActionManagerOptionsHandler.ValidateContents: Boolean;
var
  ActData: TActionShortCutData;
  Act: TContainedAction;
begin
  Result := True;
  if (FData = nil) or (FActionManager = nil) then
    Exit;

  for ActData in FData do
  begin
    if not IsShortcutAvailable(ActData.ShortCut, Act) and (Act <> nil) and (ActData.Name <> Act.Name) then
      Exit(False);
  end;
end;

function TVMActionManagerOptionsHandler.IsShortcutAvailable(aShortCut: TShortCut; out Action: TContainedAction): Boolean;
begin
  Result := (FActionManager <> nil) and FActionManager.IsShortCutAvailable(aShortCut, Action);
end;

procedure TVMActionManagerOptionsHandler.UpdateData;
var
  Act: TAction;
  Data: TActionShortCutData;
begin
  if FData = nil then
    FData := TObjectList<TActionShortCutData>.Create;
  FData.Clear;

  for Act in FActionManager.OwnActionList do
  begin
    Data := TActionShortCutData.Create;
    Data.Caption := Act.Caption;
    Data.Name := Act.Name;
    Data.ShortCut := Act.ShortCut;
    FData.Add(Data);
  end;
end;

end.
