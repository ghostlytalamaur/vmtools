unit base_params;

interface

uses
  vmsys, IniFiles, observer, classes, opt_impl, sysutils;

type
  IParamsChangedObserver = interface(IBaseObserver)
  ['{8518DA8F-8161-471C-AEBF-DC7312311BB0}']
    procedure ParamsChanged;
  end;

  TBaseParamsObserver = class(TExtInterfacedObject, IBaseObserver, IParamsChangedObserver)
  protected
    function GetCanObserve: Boolean; virtual;
    procedure ParamsChanged; virtual;
  end;

  TParamsObserver = class(TExtInterfacedObject, IBaseObserver, IParamsChangedObserver)
  private
    FOnParamsChanged: TProc;
  public
    constructor Create(aOnParamsChanged: TProc);

    function GetCanObserve: Boolean;
    procedure ParamsChanged;
  end;

  TBaseParams = class(TBaseObservableObject)
  strict private
    FTree: TParamsTree;

    procedure ReadWriteParams(IsRead: Boolean);
    class procedure RemoveStrings(aIni: TCustomIniFile; aSection, aPrefix: string);
  strict protected
    procedure DoReadParams(aIni: TCustomIniFile); virtual;
    procedure DoWriteParams(aIni: TCustomIniFile); virtual;

    function CreateTree: TParamsTree; virtual;
    procedure RegisterParams; virtual;

    class procedure WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aList: TStringList); overload;
    class procedure ReadStrings(aIni: TCustomIniFile; aSection, aPrefix: string; DestList: TStringList); overload;

    class procedure WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aList: TArray<string>); overload;
    class function ReadStrings(aIni: TCustomIniFile; aSection, aPrefix: string): TArray<string>; overload;

    class procedure WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aEnumerable: IEnumerable<string>); overload;
  strict private
    function GetParamsTree: TParamsTree;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetDefault; virtual;
    // Read/Write all params in DoReadParams()/DoWriteParams
    procedure ReadParams;
    procedure WriteParams;

    procedure ParamsChanged;

    property Tree: TParamsTree read GetParamsTree;
  end;

implementation

uses
  StrUtils;

{ TBaseParams }

const
  cstIniFileName = 'settings.ini';

type
  TParamsTreeObserver = class(TInterfacedObject, IParamsTreeObserver)
  private
    FOwner: TBaseParams;
  public
    constructor Create(aOwner: TBaseParams);
    procedure DataChanged(aTree: TParamsTree);
  end;

constructor TBaseParams.Create;
begin
  inherited Create;
  SetDefault;
end;

function TBaseParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(ClassName);
end;

destructor TBaseParams.Destroy;
begin
  FreeAndNil(FTree);
  inherited;
end;

function TBaseParams.GetParamsTree: TParamsTree;
begin
  if FTree = nil then
  begin
    FTree := CreateTree;
    RegisterParams;
    FTree.RegisterListener(TParamsTreeObserver.Create(Self));
  end;
  Result := FTree;
end;

procedure TBaseParams.ParamsChanged;
begin
  ForEachObserver<IParamsChangedObserver>(IParamsChangedObserver, procedure (O: IParamsChangedObserver)
  begin
    O.ParamsChanged;
  end);
end;

procedure TBaseParams.ReadParams;
begin
  ReadWriteParams(True);
end;

procedure TBaseParams.DoReadParams(aIni: TCustomIniFile);
begin
  Tree.ReadTree(aIni);
end;

procedure TBaseParams.DoWriteParams(aIni: TCustomIniFile);
begin
  Tree.WriteTree(aIni);
end;

procedure TBaseParams.ReadWriteParams(IsRead: Boolean);
var
  Ini: TCustomIniFile;
  Path: string;
begin
  Path := ExtractFilePath(GetModuleName(HInstance));
  Ini := TIniFile.Create(IncludeTrailingPathDelimiter(Path) + cstIniFileName);
  try
    if IsRead then
    begin
      SetDefault;
      DoReadParams(Ini)
    end
    else
      DoWriteParams(Ini);
    Ini.UpdateFile;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TBaseParams.RegisterParams;
begin

end;

procedure TBaseParams.SetDefault;
begin
  Tree.SetDefault;
end;

procedure TBaseParams.WriteParams;
begin
  ReadWriteParams(False);
end;

class procedure TBaseParams.RemoveStrings(aIni: TCustomIniFile; aSection, aPrefix: string);
var
  Count: Integer;
  I: Integer;
begin
  if aIni = nil then
    Exit;

  Count := aIni.ReadInteger(aSection, aPrefix + 'Count', 0);
  for I := 0 to Count - 1 do
    aIni.DeleteKey(aSection, aPrefix + IntToStr(I));
end;

class procedure TBaseParams.WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aList: TArray<string>);
var
  I: Integer;
begin
  RemoveStrings(aIni, aSection, aPrefix);
  if (aList = nil) or (aIni = nil) then
    Exit;

  aIni.WriteInteger(aSection, aPrefix + 'Count', Length(aList));
  for I := 0 to High(aList) do
    aIni.WriteString(aSection, aPrefix + IntToStr(I), aList[I]);
end;

class function TBaseParams.ReadStrings(aIni: TCustomIniFile; aSection, aPrefix: string): TArray<string>;
var
  I, Count: Integer;
begin
  Result := nil;
  if aIni = nil then
    Exit;

  Count := aIni.ReadInteger(aSection, aPrefix + 'Count', 0);
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := aIni.ReadString(aSection, aPrefix + IntToStr(I), '');
end;

class procedure TBaseParams.WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aEnumerable: IEnumerable<string>);
var
  I: Integer;
  S: string;
begin
  RemoveStrings(aIni, aSection, aPrefix);
  if (aIni = nil) or (aEnumerable = nil) then
    Exit;

  I := 0;
  for S in aEnumerable do
  begin
    aIni.WriteString(aSection, aPrefix + IntToStr(I), S);
    Inc(I);
  end;
  aIni.WriteInteger(aSection, aPrefix + 'Count', I);
end;

class procedure TBaseParams.ReadStrings(aIni: TCustomIniFile; aSection, aPrefix: string; DestList: TStringList);
begin
  if DestList = nil then
    Exit;

  DestList.Clear;
  DestList.AddStrings(ReadStrings(aIni, aSection, aPrefix));
end;

class procedure TBaseParams.WriteStrings(aIni: TCustomIniFile; aSection, aPrefix: string; aList: TStringList);
begin
  if aList <> nil then
    WriteStrings(aIni, aSection, aPrefix, aList.ToStringArray);
end;

{ TBaseParamsObserver }

function TBaseParamsObserver.GetCanObserve: Boolean;
begin
  Result := True;
end;

procedure TBaseParamsObserver.ParamsChanged;
begin

end;

{ TParamsTreeObserver }

constructor TParamsTreeObserver.Create(aOwner: TBaseParams);
begin
  inherited Create;
  FOwner := aOwner;
end;

procedure TParamsTreeObserver.DataChanged(aTree: TParamsTree);
begin
  if (FOwner <> nil) then
    FOwner.ParamsChanged;
end;

{ TParamsObserver }

constructor TParamsObserver.Create(aOnParamsChanged: TProc);
begin
  inherited Create;
  FOnParamsChanged := aOnParamsChanged;
end;

function TParamsObserver.GetCanObserve: Boolean;
begin
  Result := Assigned(FOnParamsChanged);
end;

procedure TParamsObserver.ParamsChanged;
begin
  if Assigned(FOnParamsChanged) then
    FOnParamsChanged;
end;

end.
