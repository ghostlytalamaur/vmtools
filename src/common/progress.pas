unit progress;

interface

uses
  generics.collections;

type
  TProgressInfo = class
  strict private
    FCaption: string;
    FCurrent: Integer;
    FTotal: Integer;
    FInfo: string;

    function GetCurrent: Integer;
    function GetInfo: string;
    function GetTotal: Integer;
    procedure SetCurrent(const Value: Integer);
    procedure SetInfo(const Value: string);
    procedure SetTotal(const Value: Integer);
  public
    constructor Create(aCaption: string);

    property Total: Integer read GetTotal write SetTotal;
    property Current: Integer read GetCurrent write SetCurrent;
    property Info: string read GetInfo write SetInfo;
  end;

  TProgressLink = class
  protected
    function GetCancelled: Boolean; virtual; abstract;
  public
    procedure BeginProgress(aInfo: TProgressInfo); virtual; abstract;
    procedure EndProgress(aNewInfo: TProgressInfo); virtual; abstract;
    procedure InfoChanged(aInfo: TProgressInfo); virtual; abstract;

    property Cancelled: Boolean read GetCancelled;
  end;

  TBaseProgress = class
  protected
    procedure SetTotal(aTotal: Integer); virtual; abstract;
    function GetTotal: Integer; virtual; abstract;

    procedure SetCurrent(aCurrent: Integer); virtual; abstract;
    function GetCurrent: Integer; virtual; abstract;

    procedure SetInfo(aInfo: string); virtual; abstract;
    function GetInfo: string; virtual; abstract;

    function GetCancelled: Boolean; virtual; abstract;
  public
    procedure BeginProgress(aCaption: string); virtual; abstract;
    procedure EndProgress; virtual; abstract;

    property Total: Integer read GetTotal write SetTotal;
    property Current: Integer read GetCurrent write SetCurrent;
    property Info: string read GetInfo write SetInfo;
    property Cancelled: Boolean read GetCancelled;
  end;

  TProgress = class(TBaseProgress)
  strict private
    FCancelled: Boolean;
    FProgressStack: TStack<TProgressInfo>;
    FLink: TProgressLink;

  protected
    procedure SetTotal(aTotal: Integer); override;
    function GetTotal: Integer; override;

    procedure SetCurrent(aCurrent: Integer); override;
    function GetCurrent: Integer; override;

    procedure SetInfo(aInfo: string); override;
    function GetInfo: string; override;

    function GetCancelled: Boolean; override;
  public
    constructor Create(aLink: TProgressLink);
    destructor Destroy; override;

    procedure BeginProgress(aCaption: string); override;
    procedure EndProgress; override;
  end;

implementation

uses
  sysutils;

{ TProgress }

procedure TProgress.BeginProgress(aCaption: string);
begin
  FCancelled := False;
  FProgressStack.Push(TProgressInfo.Create(aCaption));
  if FLink <> nil then
    FLink.BeginProgress(FProgressStack.Peek);
end;

constructor TProgress.Create(aLink: TProgressLink);
begin
  inherited Create;

  FLink := aLink;
  FProgressStack := TObjectStack<TProgressInfo>.Create;
end;

destructor TProgress.Destroy;
begin
  if FLink <> nil then
    FLink.EndProgress(nil);
  FreeAndNil(FProgressStack);
  inherited;
end;

procedure TProgress.EndProgress;
var
  NewInfo, OldInfo: TProgressInfo;
begin
  Assert(FProgressStack.Count <> 0);
  NewInfo := nil;
  OldInfo := FProgressStack.Extract;
  try
    if FProgressStack.Count > 0 then
      NewInfo := FProgressStack.Peek;
    if FLink <> nil then
      FLink.EndProgress(NewInfo);
  finally
    FreeAndNil(OldInfo);
  end;
end;

function TProgress.GetCurrent: Integer;
begin
  if FProgressStack.Count > 0 then
    Result := FProgressStack.Peek.Current
  else
    Result := 0;
end;

function TProgress.GetInfo: string;
begin
  if FProgressStack.Count > 0 then
    Result := FProgressStack.Peek.Info
  else
    Result := '';
end;

function TProgress.GetCancelled: Boolean;
begin
  Result := (FLink <> nil) and FLink.Cancelled;
end;

function TProgress.GetTotal: Integer;
begin
  if FProgressStack.Count > 0 then
    Result := FProgressStack.Peek.Total
  else
    Result := 0;
end;

procedure TProgress.SetCurrent(aCurrent: Integer);
var
  Info: TProgressInfo;
begin
  if FProgressStack.Count > 0 then
  begin
    Info := FProgressStack.Peek;
    Info.Current := aCurrent;
    if FLink <> nil then
      FLink.InfoChanged(Info);
  end;
end;

procedure TProgress.SetInfo(aInfo: string);
var
  Info: TProgressInfo;
begin
  if FProgressStack.Count > 0 then
  begin
    Info := FProgressStack.Peek;
    Info.Info := aInfo;
    if FLink <> nil then
      FLink.InfoChanged(Info);
  end;
end;

procedure TProgress.SetTotal(aTotal: Integer);
var
  Info: TProgressInfo;
begin
  if FProgressStack.Count > 0 then
  begin
    Info := FProgressStack.Peek;
    Info.Total := aTotal;
    if FLink <> nil then
      FLink.InfoChanged(Info);
  end;
end;

{ TProgressInfo }

constructor TProgressInfo.Create(aCaption: string);
begin
  inherited Create;
  FCaption := aCaption;
end;

function TProgressInfo.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

function TProgressInfo.GetInfo: string;
begin
  Result := FInfo;
end;

function TProgressInfo.GetTotal: Integer;
begin
  Result := FTotal;
end;

procedure TProgressInfo.SetCurrent(const Value: Integer);
begin
  FCurrent := Value;
end;

procedure TProgressInfo.SetInfo(const Value: string);
begin
  FInfo := Value;
end;

procedure TProgressInfo.SetTotal(const Value: Integer);
begin
  FTotal := Value;
end;

end.
