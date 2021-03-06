unit hist_wiz;

interface

uses
  vmsys, vm_basewizard, toolsapi, observer, Classes,
  Windows, ExtCtrls, base_params, opt_impl;

type
  TBaseOTANotifier = class(TInterfacedObject, IOTANotifier)
  protected
    { IOTANotifier }
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
  end;

  TVMHistoryItem = class(TBaseOTANotifier, IOTAHistoryItem)
  private
    FFileName: string;
    FLine: Integer;
    FCol: Integer;
  public
    constructor Create(aFileName: string; aLine, aCol: Integer);
    { Execute is called when this particular history item is invoked }
    procedure Execute;
    { GetItemCaption is called if the UI needs to display the history item in
      a menu or other type of list.  The implementor should return a human
      readable string that uniquely identifies this item. }
    function GetItemCaption: string;
    { IsEqual is called when inserting this item into the history stack to
      determine if this item is the same location or position as the current
      top item.  This will keep the stack from filling with adjacent duplicate
      entries. }
    function IsEqual(const Item: IOTAHistoryItem): Boolean;
  end;

  TLineData = record
    FileName: string;
    Line: Integer;
    Col: Integer;
  end;

  TEditLineChangedNotifier = class(TObject)
  private
    FTimer: TTimer;
    // last stored pos
    FPrevFileName: string;
    FPrevLine: Integer;
    FPrevCol: Integer;
    FEditLineData: IMutableData<TLineData>;

    procedure OnTimer(aSender: TObject);
    function GetEditLineData: IObservableData<TLineData>;
  public
    constructor Create;
    destructor Destroy; override;

    property EditLineData: IObservableData<TLineData> read GetEditLineData;
  end;

  TVMHistoryWizardParams = class(TBaseParams)
  private
    function GetMinDistanceInLines: Integer;
    function GetMaxBackwardCount: Integer;
  public
    procedure RegisterParams; override;
  public
    property MinDistanceInLines: Integer read GetMinDistanceInLines;
    property MaxBackwardCount: Integer read GetMaxBackwardCount;
  end;

  TVMHistoryWizard = class(TVMBaseWizard)
  private
    FNotifier: TEditLineChangedNotifier;
    FParams: TVMHistoryWizardParams;
    FParamsObserver: IParamsChangedObserver;

    procedure ParamsChanged;
    procedure EditLineChanged(const aFileName: string; aCol, aLine: Integer);
    function GetDistanceInLines(aItem: IOTAHistoryItem; const aFileName: string; aLine: Integer): Integer;
    function GetParams: TVMHistoryWizardParams;
    property Params: TVMHistoryWizardParams read GetParams;
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    destructor Destroy; override;

    function CreateOptionsHandler: INTAAddInOptions; override;

    class function GUID: string; override;
    class function Caption: string; override;
  end;

  TOtaEditLineChangedNotifier = class(TExtInterfacedObject, IOTAEditLineNotifier)
  private
    FBufferId: Integer;
    FFileName: string;
  public
    constructor Create(aBufferId: Integer; aFileName: string);
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAEditLineNotifier }
    procedure LineChanged(OldLine, NewLine: Integer; Data: Integer);
  end;


implementation

uses
  vm_ide_utils, SysUtils, vmtools_cst, vm.ide.options.treehandler, vm.debug, math;

procedure TVMHistoryWizard.RegisterWizard;
begin
  inherited;

  FNotifier := TEditLineChangedNotifier.Create;
  FNotifier.EditLineData.RegisterObserver(TDelegatedDataObserver<TLineData>.Create(
    procedure (LineData: TLineData)
    begin
      if LineData.FileName <> '' then
        EditLineChanged(LineData.FileName, LineData.Col, LineData.Line);
    end));
end;

procedure TVMHistoryWizard.UnregisterWizard;
begin
  FreeAndNil(FNotifier);
  inherited;
end;

{ TBaseOTANotifier }

procedure TBaseOTANotifier.AfterSave;
begin

end;

procedure TBaseOTANotifier.BeforeSave;
begin

end;

procedure TBaseOTANotifier.Destroyed;
begin

end;

procedure TBaseOTANotifier.Modified;
begin

end;

{ TVMHistoryItem }

constructor TVMHistoryItem.Create(aFileName: string; aLine, aCol: Integer);
begin
  inherited Create;
  FFileName := aFileName;
  FLine := aLine;
  FCol := aCol;
end;

procedure TVMHistoryItem.Execute;
begin
  TGXOtaUtils.GxOtaGoToFileLineColumn(FFileName, FLine, FCol);
end;

function TVMHistoryItem.GetItemCaption: string;
begin
  Result := FFileName + ' ' + IntToStr(FLine);
end;

function TVMHistoryItem.IsEqual(const Item: IOTAHistoryItem): Boolean;
begin
  Result := SameText(GetItemCaption, Item.GetItemCaption);
end;

function TVMHistoryWizard.CreateOptionsHandler: INTAAddInOptions;
begin
  Result := TVMOptionsTreeHandler.Create('History', Params.Tree);
end;

destructor TVMHistoryWizard.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FNotifier);
  inherited;
end;

class function TVMHistoryWizard.GUID: string;
begin
  Result := cstHistoryWiz_GUID;
end;

class function TVMHistoryWizard.Caption: string;
begin
  Result := cstHistoryWiz_Caption;
end;

procedure TVMHistoryWizard.EditLineChanged(const aFileName: string; aCol,
    aLine: Integer);

  function GetAsString(aItem: IOTAHistoryItem): string;
  begin
    if aItem = nil then
      Result := 'none'
    else
      Result := aItem.GetItemCaption;
  end;
var
  HstSrv: IOTAHistoryServices;
  Top, NewItem: IOTAHistoryItem;
  ShouldAdd: Boolean;
  Dist: Integer;
begin
  LogEnterLeave('TVMHistoryWizard.EditLineChanged');

  HstSrv := (BorlandIDEServices as IOTAHistoryServices);
  // when we add first item onto stack HstSrv.BackwardCount = 0
  try Top := HstSrv.BackwardItems[HstSrv.BackwardCount] except on Exception do Top := nil; end;
  Dist := GetDistanceInLines(Top, aFileName, aLine);
  ShouldAdd:= (Dist <> 0) and ((Dist < 0) or (Params.MinDistanceInLines <= 0) or (Dist > Params.MinDistanceInLines));
  if ShouldAdd then
  begin
    NewItem := TVMHistoryItem.Create(aFileName, aLine, aCol);
    HstSrv.AddHistoryItem(Top, NewItem);
    Logger.d('Adding history entry');
  end
  else
    Logger.d('Skipping history entry');
  Logger.d('Top: %s; aFileName: %s; aLine: %d', [GetAsString(Top), aFileName, aLine]);

  while HstSrv.BackwardCount > Math.Max(1, Params.MaxBackwardCount) do
    HstSrv.RemoveHistoryItem(HstSrv.BackwardItems[0]);
end;

function TVMHistoryWizard.GetDistanceInLines(aItem: IOTAHistoryItem; const aFileName: string; aLine: Integer): Integer;

  function ParseCaption(aCaption: string; out FileName: string; out Line: Integer): Boolean;
  var
    wPos: Integer;
    LineStr: string;
  begin
    Result := False;
    FileName := '';
    Line := -1;
    wPos := aCaption.LastIndexOf(' ');
    if wPos < 0 then
      Exit;

    LineStr := aCaption.Substring(wPos);
    if TryStrToInt(aCaption.Substring(wPos), Line) then
      FileName := aCaption.Substring(0, wPos);
    Result := FileName <> '';
  end;

var
  FileName: string;
  Line: Integer;
begin
  // Caption: FFileName + ' ' + IntToStr(FLine);
  if (aItem <> nil) and
      ParseCaption(aItem.GetItemCaption.Trim, FileName, Line) and
     (aFileName = FileName) then
    Result := Abs(aLine - Line)
  else
    Result := -1;
end;

function TVMHistoryWizard.GetParams: TVMHistoryWizardParams;
begin
  if FParams = nil then
  begin
    FParams := TVMHistoryWizardParams.Create;
    FParams.ReadParams;
  end;
  Result := FParams;

  if FParamsObserver = nil then
  begin
    FParamsObserver := TParamsObserver.Create(ParamsChanged);
    Result.RegisterObserver(IParamsChangedObserver, FParamsObserver);
  end;
end;

procedure TVMHistoryWizard.ParamsChanged;
begin
  Params.WriteParams;
end;

constructor TEditLineChangedNotifier.Create;
begin
  inherited Create;
  FEditLineData := TObservableData<TLineData>.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

destructor TEditLineChangedNotifier.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TEditLineChangedNotifier.GetEditLineData: IObservableData<TLineData>;
begin
  Result := FEditLineData;
end;

procedure TEditLineChangedNotifier.OnTimer(aSender: TObject);
var
  EditView: IOTAEditView;
  CurFileName: string;
  CurCol, CurLine: Integer;
  Data: TLineData;
begin
  EditView := TGXOtaUtils.GxOtaGetTopMostEditView;
  if EditView = nil then
    Exit;

  CurFileName := EditView.Buffer.FileName;
  CurCol := EditView.CursorPos.Col;
  CurLine := EditView.CursorPos.Line;
  if (CurFileName = '') or SameText(CurFileName, FPrevFileName) and (CurCol = FPrevCol) and (CurLine = FPrevLine) then
    Exit;

  FPrevFileName := CurFileName;
  FPrevCol := CurCol;
  FPrevLine := CurLine;

  Data.FileName := CurFileName;
  Data.Line := CurLine;
  Data.Col := CurCol;
  FEditLineData.setValue(Data);
end;

{ TOtaEditLineChangedNotifier }

procedure TOtaEditLineChangedNotifier.AfterSave;
begin
  Logger.d('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'AfterSave']);
end;

procedure TOtaEditLineChangedNotifier.BeforeSave;
begin
  Logger.d('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'BeforeSave']);
end;

constructor TOtaEditLineChangedNotifier.Create(aBufferId: Integer; aFileName: string);
begin
  inherited Create;

  FBufferId := aBufferId;
  FFileName := aFileName;
end;

procedure TOtaEditLineChangedNotifier.Destroyed;
begin
  Logger.d('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'Destroyed']);
end;

procedure TOtaEditLineChangedNotifier.LineChanged(OldLine, NewLine, Data: Integer);
begin
  Logger.d('Instance: %d; File: %s; Action: LineChanged: OldLine: %d; NewLine: %d; Data: %d', [
      FBufferid, FFileName, OldLine, NewLine, Data]);
end;

procedure TOtaEditLineChangedNotifier.Modified;
begin
  Logger.d('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'Modified']);
end;

{ TVMHistoryWizardParams }

function TVMHistoryWizardParams.GetMaxBackwardCount: Integer;
begin
  Result := (Tree.ByKey['MaxBackwardCount'] as TIntegerParam).Value;
end;

function TVMHistoryWizardParams.GetMinDistanceInLines: Integer;
begin
  Result := (Tree.ByKey['MinDistanceInLines'] as TIntegerParam).Value;
end;

procedure TVMHistoryWizardParams.RegisterParams;
begin
  inherited;
  Tree.RegisterParam(TIntegerParam.Create('MaxBackwardCount', 'Maximum elements in backward stack', 40));
  Tree.RegisterParam(TIntegerParam.Create('MinDistanceInLines', 'Minimum distance between lines', 5));
end;

end.
