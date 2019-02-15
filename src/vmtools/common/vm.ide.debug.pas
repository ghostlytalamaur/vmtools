unit vm.ide.debug;

interface

uses
  vm.debug, ToolsAPI;

type
  IMessageGroupProvider = interface
  ['{5F105505-6D20-46F5-A1CA-B03E9CC5117A}']
    function GetMessageGroup: IOTAMessageGroup;
    property MessageGroup: IOTAMessageGroup read GetMessageGroup;
  end;

  TIDELogger = class(TAbstractLogger)
  private
    FMessageGroupProvider: IMessageGroupProvider;
    FNotifierIndex: Integer;

    function GetMessageGroup: IOTAMessageGroup;
  protected
    procedure Log(aType: TAbstractLogger.TLogType; const aMsg: string); override;
  public
    destructor Destroy; override;
  end;


implementation

uses
  sysutils;

type
  TOTAMessageNotifier = class(TInterfacedObject, IOTANotifier, IOTAMessageNotifier, IMessageGroupProvider)
  private
    FMessageGroup: IOTAMessageGroup;

    { IMessageGroupProvider }
    function GetMessageGroup: IOTAMessageGroup;
  public
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAMessageNotifier }
    procedure MessageGroupAdded(const Group: IOTAMessageGroup);
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup);
  end;

destructor TIDELogger.Destroy;
var
  Srv: IOTAMessageServices;
begin
  if (FNotifierIndex >= 0) and Supports(BorlandIDEServices, IOTAMessageServices, Srv) then
    Srv.RemoveNotifier(FNotifierIndex);
  inherited;
end;

{ TIDELogger }

function TIDELogger.GetMessageGroup: IOTAMessageGroup;
var
  Srv: IOTAMessageServices;
begin
  if (FMessageGroupProvider = nil) and Supports(BorlandIDEServices, IOTAMessageServices, Srv) then
  begin
    FMessageGroupProvider := TOTAMessageNotifier.Create;
    FNotifierIndex := Srv.AddNotifier(FMessageGroupProvider as IOTAMessageNotifier)
  end;
  if FMessageGroupProvider <> nil then
    Result := FMessageGroupProvider.MessageGroup
  else
    Result := nil;
end;

procedure TIDELogger.Log(aType: TAbstractLogger.TLogType; const aMsg: string);
var
  Srv: IOTAMessageServices;
  Prefix: string;
  LineRef: Pointer;
begin
  try
    if (GetMessageGroup = nil) or not Supports(BorlandIDEServices, IOTAMessageServices, Srv) then
      Exit;

    case aType of
      lpInfo:       Prefix := 'Info';
      lpWarning:    Prefix := 'Warning';
      lpError:      Prefix := 'Error';
      lpDebug:      Prefix := 'Debug';
      ltGroupStart: Prefix := '[Start]';
      ltGroupEnd:   Prefix := '[End]';
    end;

    if aType in [ltGroupStart, ltGroupEnd] then
      Srv.AddTitleMessage(Prefix + ' ' + aMsg, GetMessageGroup)
    else
      Srv.AddToolMessage('', aMsg, Prefix, 0, 0, nil, LineRef, GetMessageGroup);
  except
  end;
end;

{ TOTAMessageNotifier }

procedure TOTAMessageNotifier.AfterSave;
begin
end;

procedure TOTAMessageNotifier.BeforeSave;
begin
end;

procedure TOTAMessageNotifier.Destroyed;
begin
end;

function TOTAMessageNotifier.GetMessageGroup: IOTAMessageGroup;
var
  Srv: IOTAMessageServices;
begin
  if FMessageGroup = nil then
  begin
    if Supports(BorlandIDEServices, IOTAMessageServices, Srv) then
    try
      FMessageGroup := Srv.AddMessageGroup('VM Tools');
    except
      FMessageGroup := nil;
    end;
  end;
  Result := FMessageGroup;
end;

procedure TOTAMessageNotifier.Modified;
begin
end;

procedure TOTAMessageNotifier.MessageGroupAdded(const Group: IOTAMessageGroup);
begin
end;

procedure TOTAMessageNotifier.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
  if (Group = FMessageGroup) then
    FMessageGroup := nil;
end;

//initialization
//  SetLogger(TIDELogger.Create);

end.
