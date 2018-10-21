unit opt_impl;

interface

uses
  vmsys, sysutils, generics.collections, windows, Classes, IniFiles, observer;

type
  TParamFlag = (pfRuntine, pfInvisible);
  TParamFlags = set of TParamFlag;

  TBaseParam = class(TExtObject)
  strict private
    FKey: string;
    FAlias: string;
    FFlags: TParamFlags;

  protected
    function GetIsChanged: Boolean; virtual;
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); virtual;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); virtual;

    procedure ReadIni(aIni: TCustomIniFile; const aSection: string);
    procedure WriteIni(aIni: TCustomIniFile; const aSection: string);
    procedure CopyFrom(aFrom: TBaseParam); virtual;
  public
    constructor Create(const aKey: string); overload;
    constructor Create(const aKey: string; const aAlias: string); overload;
    constructor Create(const aKey: string; const aAlias: string; const aFlags: TParamFlags); overload;

    function Duplicate: TBaseParam; virtual;
    procedure CopyData(aFrom: TBaseParam); virtual; abstract;

    procedure SetDefault; virtual;

    property Key: string read FKey;
    property Alias: string read FAlias;
    property Flags: TParamFlags read FFlags;
    property IsChanged: Boolean read GetIsChanged;
  end;

  TSimpleParam<T> = class(TBaseParam)
  private
    FValue: T;
    FDefValue: T;

  protected
    function GetIsChanged: Boolean; override;

    procedure CopyFrom(aFrom: TBaseParam); override;
  public
    constructor Create(const aKey: string; const aDefValue: T); overload;
    constructor Create(const aKey: string; const aAlias: string; const aDefValue: T); overload;
    constructor Create(const aKey: string; const aAlias: string; const aFlags: TParamFlags; const aDefValue: T); overload;

    procedure CopyData(aFrom: TBaseParam); override;

    procedure SetDefault; override;

    property Value: T read FValue write FValue;
    property DefValue: T read FDefValue;
  end;

  TIntegerParam = class(TSimpleParam<Integer>)
  protected
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  end;

  TSingleParam = class(TSimpleParam<Single>)
  protected
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  end;

  TBooleanParam = class(TSimpleParam<Boolean>)
  protected
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  end;

  TStringParam = class(TSimpleParam<string>)
  protected
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  end;

  TShortCutParam = class(TSimpleParam<TShortCut>)
  protected
    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  public
    constructor Create(const aKey: string; const aDefShortCutText: string); overload;
    constructor Create(const aKey: string; const aAlias: string; const aDefShortCutText: string); overload;
    constructor Create(const aKey: string; const aAlias: string; const aFlags: TParamFlags; const aDefShortCutText: string); overload;

    constructor Create(const aKey: string; const aDefValue: TShortCut); overload;
    constructor Create(const aKey: string; const aAlias: string; const aDefValue: TShortCut); overload;
    constructor Create(const aKey: string; const aAlias: string; const aFlags: TParamFlags; const aDefValue: TShortCut); overload;
  end;

  TParamsGroup = class(TBaseParam)
  strict private
    FOwnParams: TList<TBaseParam>;

    function GetParamsCount: Integer;
    function GetByIndex(aIndex: Integer): TBaseParam;
    function GetByKey(aKey: string): TBaseParam;
  protected
    FParams: TList<TBaseParam>;

    procedure CopyFrom(aFrom: TBaseParam); override;
    function GetIsChanged: Boolean; override;

    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
  public
    destructor Destroy; override;
    procedure CopyData(aFrom: TBaseParam); override;

    procedure SetDefault; override;
    procedure RegisterParam(aParam: TBaseParam; aOwnParam: Boolean = True);
    procedure RemoveParams;

    property ByKey[aKey: string]: TBaseParam read GetByKey;
    property ByIndex[aIndex: Integer]: TBaseParam read GetByIndex;
    property ParamsCount: Integer read GetParamsCount;
  end;

  TActiveParamsGroup = class(TParamsGroup)
  private
    FIsActive: Boolean;
    FDefIsActive: Boolean;

  protected
    function GetIsActive: Boolean; virtual;
    procedure SeIsActive(const Value: Boolean); virtual;
    function GetIsChanged: Boolean; override;

    procedure DoReadIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure DoWriteIni(aIni: TCustomIniFile; const aSection: string); override;
    procedure CopyFrom(aFrom: TBaseParam); override;
  public
    constructor Create(const aKey: string); overload;
    constructor Create(const aKey: string; aDefIsActive: Boolean); overload;
    constructor Create(const aKey: string; const aAlias: string; aDefIsActive: Boolean); overload;
    constructor Create(const aKey: string; const aAlias: string; const aFlags: TParamFlags; aDefIsActive: Boolean); overload;
    procedure CopyData(aFrom: TBaseParam); override;
    procedure SetDefault; override;

    property IsActive: Boolean read GetIsActive write SeIsActive;
    property DefIsActive: Boolean read FDefIsActive;
  end;

  TParamsTree = class;
  IParamsTreeObserver = interface
  ['{48ED0BDA-50CA-4452-AD64-E2F3B81874FE}']
    procedure DataChanged(aTree: TParamsTree);
  end;

  TParamsTreeObserver = class(TInterfacedObject, IParamsTreeObserver)
  private
    FOnDataChanged: TProc<TParamsTree>;
  public
    constructor Create(aOnDataChanged: TProc<TParamsTree>);

    procedure DataChanged(aTree: TParamsTree);
  end;

  TParamsTree = class(TParamsGroup)
  private
    FAnnouncer: IAnnouncer<IParamsTreeObserver>;
  public
    destructor Destroy; override;
    procedure CopyData(aFrom: TBaseParam); override;

    procedure ReadTree(aIni: TCustomIniFile);
    procedure WriteTree(aIni: TCustomIniFile);

    procedure RegisterListener(aListener: IParamsTreeObserver);
    procedure RemoveListener(aListener: IParamsTreeObserver);
    procedure DataChanged;
  end;

implementation

uses
  Generics.Defaults, Menus;

constructor TSimpleParam<T>.Create(const aKey, aAlias: string; const aDefValue: T);
begin
  Create(aKey, aAlias, [], aDefValue);
end;

procedure TSimpleParam<T>.CopyFrom(aFrom: TBaseParam);
begin
  if aFrom is TSimpleParam<T> then
  begin
    inherited;
    FValue := TSimpleParam<T>(aFrom).FValue;
    FDefValue := TSimpleParam<T>(aFrom).FDefValue;
  end;
end;

constructor TSimpleParam<T>.Create(const aKey, aAlias: string; const aFlags: TParamFlags; const aDefValue: T);
begin
  inherited Create(aKey, aAlias, aFlags);
  FDefValue := aDefValue;
  FValue := FDefValue;
end;

procedure TSimpleParam<T>.CopyData(aFrom: TBaseParam);
begin
  if aFrom is TSimpleParam<T> then
  begin
    inherited;
    Value := (aFrom as TSimpleParam<T>).Value;
  end;
end;

constructor TSimpleParam<T>.Create(const aKey: string; const aDefValue: T);
begin
  Create(aKey, '', aDefValue);
end;

function TSimpleParam<T>.GetIsChanged: Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  Comparer := TEqualityComparer<T>.Default;
  Result := not Comparer.Equals(FDefValue, FValue);
end;

procedure TSimpleParam<T>.SetDefault;
begin
  FValue := FDefValue;
end;

{ TBaseParam }

constructor TBaseParam.Create(const aKey: string);
begin
  Create(aKey, '');
end;

constructor TBaseParam.Create(const aKey, aAlias: string);
begin
  Create(aKey, aAlias, []);
end;

procedure TBaseParam.CopyFrom(aFrom: TBaseParam);
begin
  FKey := aFrom.FKey;
  FAlias := aFrom.FAlias;
  FFlags := aFrom.FFlags;
end;

constructor TBaseParam.Create(const aKey, aAlias: string; const aFlags: TParamFlags);
begin
  inherited Create;
  FKey := aKey;
  FAlias := aAlias;
  FFlags := aFlags;
end;

function TBaseParam.GetIsChanged: Boolean;
begin
  Result := False;
end;

procedure TBaseParam.ReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  if not (pfRuntine in Flags) then
    DoReadIni(aIni, aSection);
end;

procedure TBaseParam.WriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  if not (pfRuntine in Flags) then
    DoWriteIni(aIni, aSection);
end;

procedure TBaseParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin

end;

procedure TBaseParam.SetDefault;
begin

end;

procedure TBaseParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin

end;

function TBaseParam.Duplicate: TBaseParam;
begin
  Result := ClassType.Create as TBaseParam;
  Result.CopyFrom(Self);
end;

{ TParamsGroup }

procedure TParamsGroup.CopyData(aFrom: TBaseParam);
var
  I: Integer;
begin
  if (FParams <> nil) and (aFrom is TParamsGroup) and (TParamsGroup(aFrom).FParams <> nil) and
      (TParamsGroup(aFrom).FParams.Count = FParams.Count) then
  begin
    inherited;
    for I := 0 to FParams.Count - 1 do
      FParams[I].CopyData(TParamsGroup(aFrom).FParams[I]);
  end;
end;

destructor TParamsGroup.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FOwnParams);
  inherited;
end;

procedure TParamsGroup.CopyFrom(aFrom: TBaseParam);
var
  P: TBaseParam;
begin
  if aFrom is TParamsGroup then
  begin
    inherited;
    RemoveParams;
    if TParamsGroup(aFrom).FParams <> nil then
      for P in TParamsGroup(aFrom).FParams do
        RegisterParam(P.Duplicate);
  end;
end;

function TParamsGroup.GetByIndex(aIndex: Integer): TBaseParam;
begin
  if (FParams <> nil) and (aIndex >= 0) and (aIndex < FParams.Count) then
    Result := FParams[aIndex]
  else
    Result := nil;
end;

function TParamsGroup.GetByKey(aKey: string): TBaseParam;

  function DoGetByKey(const aKey: string; aParam: TBaseParam): TBaseParam;
  var
    P: TBaseParam;
  begin
    Result := nil;
    if (aParam <> Self) and SameStr(aKey, aParam.Key) then
      Result := aParam
    else if (aParam is TParamsGroup) and (TParamsGroup(aParam).FParams <> nil) then
      for P in TParamsGroup(aParam).FParams do
      begin
        Result := DoGetByKey(aKey, P);
        if Result <> nil then
          Exit;
      end;
  end;

begin
  Result := DoGetByKey(aKey, Self);
end;

function TParamsGroup.GetIsChanged: Boolean;
var
  P: TBaseParam;
begin
  if FParams <> nil then
    for P in FParams do
      if P.IsChanged then
        Exit(True);
  Result := False;
end;

function TParamsGroup.GetParamsCount: Integer;
begin
  if FParams <> nil then
    Result := FParams.Count
  else
    Result := 0;
end;

procedure TParamsGroup.RegisterParam(aParam: TBaseParam; aOwnParam: Boolean = True);
begin
  if aParam = nil then
    Exit;

  if FOwnParams = nil then
    FOwnParams := TObjectList<TBaseParam>.Create;
  if FParams = nil then
    FParams := TList<TBaseParam>.Create;

  FParams.Add(aParam);
  if aOwnParam then
    FOwnParams.Add(aParam);
end;

procedure TParamsGroup.RemoveParams;
begin
  FreeAndNil(FParams);
  FreeAndNil(FOwnParams);
end;

procedure TParamsGroup.SetDefault;
var
  P: TBaseParam;
begin
  inherited;
  if FParams <> nil then
    for P in FParams do
      P.SetDefault;
end;

procedure TParamsGroup.DoReadIni(aIni: TCustomIniFile; const aSection: string);
var
  P: TBaseParam;
  Section: string;
begin
  if FParams = nil then
    Exit;

  if aSection <> '' then
    Section := aSection + '\' + Key
  else
    Section := Key;
  for P in FParams do
    P.ReadIni(aIni, Section);
end;

procedure TParamsGroup.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
var
  P: TBaseParam;
  Section: string;
begin
  if FParams = nil then
    Exit;

  if aSection <> '' then
    Section := aSection + '\' + Key
  else
    Section := Key;
  for P in FParams do
    P.WriteIni(aIni, Section);
end;

{ TActiveParamsGroup }

constructor TActiveParamsGroup.Create(const aKey: string);
begin
  Create(aKey, True);
end;

constructor TActiveParamsGroup.Create(const aKey: string; aDefIsActive: Boolean);
begin
  Create(aKey, '', aDefIsActive);
end;

constructor TActiveParamsGroup.Create(const aKey, aAlias: string; aDefIsActive: Boolean);
begin
  Create(aKey, aAlias, [], aDefIsActive);
end;

procedure TActiveParamsGroup.CopyFrom(aFrom: TBaseParam);
begin
  if aFrom is TActiveParamsGroup then
  begin
    inherited;
    FIsActive := TActiveParamsGroup(aFrom).IsActive;
    FDefIsActive := TActiveParamsGroup(aFrom).DefIsActive;
  end;
end;

constructor TActiveParamsGroup.Create(const aKey, aAlias: string; const aFlags: TParamFlags; aDefIsActive: Boolean);
begin
  inherited Create(aKey, aAlias, aFlags);
  FDefIsActive := aDefIsActive;
  FIsActive := FDefIsActive;
end;

{ TActiveParamsGroup }

procedure TActiveParamsGroup.CopyData(aFrom: TBaseParam);
begin
  if (aFrom is TActiveParamsGroup) then
  begin
    inherited;
    IsActive := TActiveParamsGroup(aFrom).IsActive;
  end;
end;

function TActiveParamsGroup.GetIsChanged: Boolean;
begin
  Result := (IsActive <> FDefIsActive) or inherited;
end;

function TActiveParamsGroup.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TActiveParamsGroup.SeIsActive(const Value: Boolean);
begin
  FIsActive := Value;
end;

procedure TActiveParamsGroup.SetDefault;
begin
  inherited;
  IsActive := FDefIsActive;
end;

procedure TActiveParamsGroup.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  IsActive := aIni.ReadBool(aSection + '\' + Key, 'IsActive', DefIsActive);
end;

procedure TActiveParamsGroup.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  aIni.WriteBool(aSection + '\' + Key, 'IsActive', IsActive);
end;

{ TIntegerParam }

procedure TIntegerParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  Value := aIni.ReadInteger(aSection, Key, DefValue);
end;

procedure TIntegerParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  aIni.WriteInteger(aSection, Key, Value);
end;

{ TSingleParam }

procedure TSingleParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  Value := aIni.ReadFloat(aSection, Key, DefValue);
end;

procedure TSingleParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  aIni.WriteFloat(aSection, Key, Value);
end;

{ TBooleanParam }

procedure TBooleanParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  Value := aIni.ReadBool(aSection, Key, DefValue);
end;

procedure TBooleanParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  aIni.WriteBool(aSection, Key, Value);
end;

{ TStringParam }

procedure TStringParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  Value := aIni.ReadString(aSection, Key, DefValue);
end;

procedure TStringParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  aIni.WriteString(aSection, Key, Value);
end;

{ TShortCutParam }

constructor TShortCutParam.Create(const aKey, aDefShortCutText: string);
begin
  Create(aKey, '', aDefShortCutText);
end;

constructor TShortCutParam.Create(const aKey: string; const aDefValue:
    TShortCut);
begin
  inherited;
end;

constructor TShortCutParam.Create(const aKey, aAlias: string; const aDefValue: TShortCut);
begin
  inherited;
end;

constructor TShortCutParam.Create(const aKey, aAlias: string; const aFlags: TParamFlags; const aDefValue: TShortCut);
begin
  inherited;
end;

constructor TShortCutParam.Create(const aKey, aAlias, aDefShortCutText: string);
begin
  Create(aKey, aAlias, [], aDefShortCutText);
end;

constructor TShortCutParam.Create(const aKey, aAlias: string; const aFlags: TParamFlags;
  const aDefShortCutText: string);
begin
  Create(aKey, aAlias, aFlags, TextToShortCut(aDefShortCutText));
end;

procedure TShortCutParam.DoReadIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  Value := aIni.ReadInteger(aSection, Key, DefValue);
end;

procedure TShortCutParam.DoWriteIni(aIni: TCustomIniFile; const aSection: string);
begin
  inherited;
  aIni.WriteInteger(aSection, Key, Value);
end;

{ TParamsTree }

procedure TParamsTree.CopyData(aFrom: TBaseParam);
begin
  if (aFrom is TParamsTree) then
  begin
    inherited;
  end;
end;

procedure TParamsTree.ReadTree(aIni: TCustomIniFile);
begin
  if (aIni <> nil) then
    ReadIni(aIni, '');
end;

procedure TParamsTree.RegisterListener(aListener: IParamsTreeObserver);
begin
  if aListener = nil then
    Exit;

  if FAnnouncer = nil then
    FAnnouncer := TAnnouncer<IParamsTreeObserver>.Create;
  FAnnouncer.RegisterListener(aListener);
end;

procedure TParamsTree.RemoveListener(aListener: IParamsTreeObserver);
begin
  if (aListener = nil) or (FAnnouncer = nil) then
    Exit;

  FAnnouncer.RemoveListener(aListener);
end;

procedure TParamsTree.DataChanged;

  procedure TryNotifyListeners(aParam: TBaseParam);
  var
    P: TBaseParam;
  begin
    if aParam is TParamsTree then
    begin
      TParamsTree(aParam).DataChanged
    end
    else if (aParam is TParamsGroup) and (TParamsGroup(aParam).FParams <> nil) then
      for P in TParamsGroup(aParam).FParams do
        TryNotifyListeners(P);
  end;

var
  P: TBaseParam;
begin
  if  FAnnouncer <> nil then
    FAnnouncer.ForEachListener(
      procedure (aListener: IParamsTreeObserver)
      begin
        aListener.DataChanged(Self);
      end);

  if FParams <> nil then
    for P in FParams do
      TryNotifyListeners(P);
end;

destructor TParamsTree.Destroy;
begin
  FAnnouncer := nil;
  inherited;
end;

procedure TParamsTree.WriteTree(aIni: TCustomIniFile);
begin
  if (aIni <> nil) then
    WriteIni(aIni, '');
end;

{ TParamsTreeObserver }

constructor TParamsTreeObserver.Create(aOnDataChanged: TProc<TParamsTree>);
begin
  inherited Create;
  FOnDataChanged := aOnDataChanged;
end;

procedure TParamsTreeObserver.DataChanged(aTree: TParamsTree);
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(aTree);
end;

end.
