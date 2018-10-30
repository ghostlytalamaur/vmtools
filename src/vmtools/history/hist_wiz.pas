unit hist_wiz;

interface

uses
  vmsys, vm_basewizard, toolsapi, observer, Classes,
  Windows, ExtCtrls;

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

  TVMHistoryWizard = class(TVMBaseWizard)
  private
    FNotifier: TEditLineChangedNotifier;

    procedure EditLineChanged(const aFileName: string; aCol, aLine: Integer);
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    destructor Destroy; override;

    class function GUID: string; override;
    class function Caption: string; override;
  end;

  TOtaEditLineChangedNotifier = class(TExtInterfacedObject, IOTAEditLineNotifier)
  private
    FWizard: TVMHistoryWizard;
    FBufferId: Integer;
    FFileName: string;
  public
    constructor Create(aWizard: TVMHistoryWizard; aBufferId: Integer; aFileName: string);
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
  vm_ide_utils, SysUtils, vmtools_cst;

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

destructor TVMHistoryWizard.Destroy;
begin
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
const
  cstMaxBackwardCount = 40;
var
  HstSrv: IOTAHistoryServices;
  CurItem, NewItem, LastItem, NextItem: IOTAHistoryItem;
begin
  LastItem := nil;
  CurItem := nil;
  NextItem := nil;

  HstSrv := (BorlandIDEServices as IOTAHistoryServices);
  if HstSrv.BackwardCount > 0 then
    LastItem := HstSrv.BackwardItems[HstSrv.BackwardCount - 1];

  // when we add first item onto stack HstSrv.BackwardCount = 0
  try CurItem := HstSrv.BackwardItems[HstSrv.BackwardCount] except on Exception do CurItem := nil; end;

  if HstSrv.ForwardCount > 0 then
    NextItem := HstSrv.ForwardItems[0];

  NewItem := TVMHistoryItem.Create(aFileName, aLine, aCol);

  if (CurItem = nil) and (NextItem = nil) or
      (CurItem <> nil) and not CurItem.IsEqual(NewItem) or
      (CurItem = nil) and (NextItem <> nil) and not NextItem.IsEqual(NewItem) then
  begin
    if (LastItem <> nil) and (CurItem <> nil) and LastItem.IsEqual(CurItem) then
      CurItem := nil;

    if (CurItem = nil) or not NewItem.IsEqual(CurItem) then
      HstSrv.AddHistoryItem(CurItem, NewItem);
  end;

  while HstSrv.BackwardCount > cstMaxBackwardCount do
    HstSrv.RemoveHistoryItem(HstSrv.BackwardItems[0]);
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
  if FWizard = nil then
    Exit;

  FWizard.InfoMsg(Format('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'AfterSave']));
end;

procedure TOtaEditLineChangedNotifier.BeforeSave;
begin
  if FWizard = nil then
    Exit;

  FWizard.InfoMsg(Format('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'BeforeSave']));
end;

constructor TOtaEditLineChangedNotifier.Create(aWizard: TVMHistoryWizard; aBufferId: Integer; aFileName: string);
begin
  inherited Create;

  FBufferId := aBufferId;
  FFileName := aFileName;
  SetNotifiableObjectProperty(@FWizard, aWizard);
end;

procedure TOtaEditLineChangedNotifier.Destroyed;
begin
  if FWizard = nil then
    Exit;

  FWizard.InfoMsg(Format('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'Destroyed']));
end;

procedure TOtaEditLineChangedNotifier.LineChanged(OldLine, NewLine, Data: Integer);
begin
  if FWizard = nil then
    Exit;

  FWizard.InfoMsg(Format('Instance: %d; File: %s; Action: LineChanged: OldLine: %d; NewLine: %d; Data: %d', [
      FBufferid, FFileName, OldLine, NewLine, Data]));
end;

procedure TOtaEditLineChangedNotifier.Modified;
begin
  if FWizard = nil then
    Exit;

  FWizard.InfoMsg(Format('Instance: %d; File: %s; Action: %s', [FBufferid, FFileName, 'Modified']));
end;

end.
