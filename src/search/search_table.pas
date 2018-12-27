unit search_table;

interface

uses
  vtree_mod, search_types, generics.defaults, classes, virtualtrees, observer, graphics, types,
  vm.gui.control_updater;

type
  TInfoComparer = class
  private
    FSortOrder: TArray<Integer>;
  public
    function Compare(aColumn: Integer; const L, R: TVMSearchResultsItem): Integer;
    procedure SetSortColumn(aColumn: Integer);
  end;

  TFormatTextDrawer = class(TObject)
  private const
    cstEscReset        = #$1B + '[0m';
    cstEscBold         = #$1B + '[1m';
  private
    function TextExtent(aCanvas: TCanvas; aText: TStringList): TSize;
    procedure SetCanvasStyle(aCanvas: TCanvas; const aEscSeq: string);
  public
    function Draw(const aText: string; const aCanvas: TCanvas; aRect: TRect): Boolean;
  end;

  TSearchResultFrameSettings = class(TExtVirtualTreeParams);

  TSearchResultsVirtualTree = class(TExtVirtualStringTree)
  private
    FInfoComparer: TInfoComparer;
    FTextDrawer: TFormatTextDrawer;

    FCurList: TVMSearchResultsList;
    FCurListData: IObservableData<TVMSearchResultsList>;

    FDataObserver: IDataObserver<TVMSearchResultsList>;
    FListObserver: IVMSearchResultsListListener;

    FListUpdater: TControlUpdater;

    procedure UpdateList;

    procedure OnNewList(aData: TVMSearchResultsList);
    procedure OnListChanged;
  protected
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure DoCanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex;
        var Allowed: Boolean); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    function DoNodeHeightTracking(Node: PVirtualNode; Column: TColumnIndex;  Shift: TShiftState;
        var TrackPoint: TPoint; P: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNodeData(aNode: PVirtualNode): TVMSearchResultsItem;
    procedure SetData(aListData: IObservableData<TVMSearchResultsList>);
  end;

implementation

uses
  SysUtils, Math, str_utils, VirtualTrees.Utils;

type
  TVMSearchResultsListListener = class(TInterfacedObject, IVMSearchResultsListListener)
  private
    FOnListChanged: TProc;
  public
    constructor Create(aOnListChanged: TProc);
    procedure ListChanged;
  end;

const
  cstFirstColumn = 0;
  cstColText     = 0;
  cstColFile     = 1;
  cstColLine     = 2;
  cstColRating   = 3;
  cstColPath     = 4;
  cstLastColumnt = 4;


{ TInfoComparer }

procedure TInfoComparer.SetSortColumn(aColumn: Integer);

  procedure AppendOrder(var Order: TArray<Integer>; var Idx: Integer; aCurCol: Integer);
  begin
    if aColumn <> aCurCol then
    begin
      if Idx >= Length(Order) then
        SetLength(Order, Idx + 1); // for safety
      Order[Idx] := aCurCol;
      Inc(Idx);
    end;
  end;

var
  I: Integer;
begin
  if (FSortOrder <> nil) and (FSortOrder[0] = aColumn) then
    Exit;

  SetLength(FSortOrder, cstLastColumnt + 1);
  FSortOrder[0] := aColumn;
  I := 1;
  AppendOrder(FSortOrder, I, cstColRating);
  AppendOrder(FSortOrder, I, cstColPath);
  AppendOrder(FSortOrder, I, cstColFile);
  AppendOrder(FSortOrder, I, cstColLine);
  AppendOrder(FSortOrder, I, cstColText);
end;

function TInfoComparer.Compare(aColumn: Integer; const L, R: TVMSearchResultsItem): Integer;
var
  Col: Integer;
begin
  Result := 0;
  if (L = nil) or (R = nil) then
    Exit;

  SetSortColumn(aColumn);
  for Col in FSortOrder do
  begin
    case Col of
      cstColFile: Result := CompareText(L.FileName, R.FileName);
      cstColPath: Result := CompareText(L.FilePath, R.FilePath);
      cstColText: Result := CompareText(L.Text, R.Text);
      cstColLine: Result := L.Line - R.Line;
      cstColRating: Result := L.Rating - R.Rating;
    end;
    if Result <> 0 then
      Exit;
  end;
end;

{ TSearchResultsVirtualTree }

constructor TSearchResultsVirtualTree.Create(AOwner: TComponent);
begin
  inherited;
  FInfoComparer := TInfoComparer.Create;
  FTextDrawer := TFormatTextDrawer.Create;
  FDataObserver := TDelegatedDataObserver<TVMSearchResultsList>.Create(OnNewList);
  FListObserver := TVMSearchResultsListListener.Create(OnListChanged);
  FListUpdater := TControlUpdater.Create(UpdateList);
end;

destructor TSearchResultsVirtualTree.Destroy;
begin
  SetData(nil);
  FreeAndNil(FListUpdater);
  FreeAndNil(FInfoComparer);
  FreeAndNil(FTextDrawer);
  inherited;
end;

procedure TSearchResultsVirtualTree.DoCanSplitterResizeNode(P: TPoint; Node: PVirtualNode; Column: TColumnIndex;
    var Allowed: Boolean);
begin
  Allowed := Node <> nil;
end;

function TSearchResultsVirtualTree.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
begin
  Result := FInfoComparer.Compare(Column, GetNodeData(Node1), GetNodeData(Node2));
end;

procedure TSearchResultsVirtualTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var
  Item: TVMSearchResultsItem;
begin
  inherited;
  Item := GetNodeData(pEventArgs.Node);
  if Item = nil then
    Exit;

  case pEventArgs.Column of
    cstColFile: pEventArgs.CellText := Item.FileName;
    cstColPath: pEventArgs.CellText := Item.FilePath;
    cstColText: pEventArgs.CellText := Item.Text;
    cstColLine: pEventArgs.CellText := IntToStr(Item.Line);
    cstColRating: pEventArgs.CellText := IntToStr(Item.Rating);
  end;
end;

procedure TSearchResultsVirtualTree.DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates);
begin
  Node.SetData<Integer>(Node.Index);
end;

function TSearchResultsVirtualTree.DoNodeHeightTracking(Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;
    var TrackPoint: TPoint; P: TPoint): Boolean;
begin
  if P.Y - TrackPoint.Y < TExtVirtualTreeParams.cstMinNodeHeight then
    TrackPoint.Y := P.Y - TExtVirtualTreeParams.cstMinNodeHeight;
  UpdateNodesHeight(Max(TExtVirtualTreeParams.cstMinNodeHeight, P.Y - TrackPoint.Y));
  Result := True;
end;

procedure TSearchResultsVirtualTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect;
  DrawFormat: Cardinal);
var
  Item: TVMSearchResultsItem;
  DefaultDraw: Boolean;
begin
  DefaultDraw := True;
  Item := GetNodeData(PaintInfo.Node);
  if Item <> nil then
  begin
    case PaintInfo.Column of
      cstColText: DefaultDraw := not FTextDrawer.Draw(Item.RawText, PaintInfo.Canvas, CellRect);
    end;
  end;
  if DefaultDraw then
    inherited;
end;

function TSearchResultsVirtualTree.GetNodeData(aNode: PVirtualNode): TVMSearchResultsItem;
begin
  if FCurList <> nil then
    Result := FCurList.Items[aNode.GetData<Integer>]
  else
    Result := nil;
end;

procedure TSearchResultsVirtualTree.SetData(aListData: IObservableData<TVMSearchResultsList>);
begin
  if FCurListData = aListData then
    Exit;

  FListUpdater.BeginUpdate;
  try
    if FCurListData <> nil then
    begin
      OnNewList(nil); // Remove list observer
      FCurListData.RemoveObserver(FDataObserver);
    end;
    FCurListData := aListData;
    if FCurListData <> nil then
      FCurListData.RegisterObserver(FDataObserver);
  finally
    FListUpdater.EndUpdate;
  end;
end;

procedure TSearchResultsVirtualTree.UpdateList;
begin
  if (FCurList = nil) or (FCurList.Count <= 0) then
  begin
    Clear;
    Exit;
  end;

  BeginUpdate;
  try
    RootNodeCount := FCurList.Count;
  finally
    EndUpdate;
  end;
end;

procedure TSearchResultsVirtualTree.OnNewList(aData: TVMSearchResultsList);
begin
  if FCurList = aData then
    Exit;

  if FCurList <> nil then
    FCurList.Listeners.RemoveListener(FListObserver);
  FCurList := aData;
  if FCurList <> nil then
    FCurList.Listeners.RegisterListener(FListObserver);
  FListUpdater.RequestUpdate;
end;

procedure TSearchResultsVirtualTree.OnListChanged;
begin
  FListUpdater.RequestUpdate;
end;

{ TFormatTextDrawer }

function TFormatTextDrawer.TextExtent(aCanvas: TCanvas; aText: TStringList): TSize;
var
  Line: string;
  CurSize: TSize;
begin
  Result.cx := 0;
  Result.cY := 0;
  for Line in aText do
  begin
    CurSize := aCanvas.TextExtent(Line);
    Result.cx := Max(Result.cx, CurSize.cx);
    Result.cy := Result.cy + CurSize.cy;
  end;
end;

procedure TFormatTextDrawer.SetCanvasStyle(aCanvas: TCanvas; const aEscSeq: string);
begin
  if aEscSeq = cstEscBold then
    aCanvas.Font.Style := [fsBold]
  else if aEscSeq = cstEscReset then
    aCanvas.Font.Style := [];
end;

function TFormatTextDrawer.Draw(const aText: string; const aCanvas: TCanvas; aRect: TRect): Boolean;
const
  cstMargin = 16;
var
  Lines: TStringList;
  Len, AlreadyDrawedLen, DrawLen, RectW, EscOffset, EscPos, I, X, Y: Integer;
  TotalTextSize, TextSize: TSize;
  Line, CleanLine, EscSeq: string;
begin
  Result := True;
  Lines := TStringList.Create;
  try
    Lines.Text := aText;
    TotalTextSize := TextExtent(aCanvas, Lines);
    if RectHeight(aRect) > TotalTextSize.cy then
      Y := (aRect.Top + aRect.Bottom) div 2 - TotalTextSize.cy div 2
    else
      Y := aRect.Top;
    RectW := RectWidth(aRect);
    for I := 0 to Lines.Count - 1 do
    begin
      X := aRect.Left;
      Line := Lines[I];
      CleanLine := TStrUtils.RemoveEscSeq(Line);
      DrawLen := Length(ShortenString(aCanvas.Handle, CleanLine, RectW - 2 * cstMargin));
      AlreadyDrawedLen := 0;

      TextSize := aCanvas.TextExtent(Lines[I]);

      EscOffset := 1;
      while TStrUtils.FindEscPos(Line, EscOffset, EscPos, EscSeq) do
      begin
        if AlreadyDrawedLen < DrawLen then
        begin
          Len := Min(DrawLen - AlreadyDrawedLen, EscPos - EscOffset);
          aCanvas.TextOut(X, Y, Copy(Line, EscOffset, Len));
          X := aCanvas.PenPos.X;

          AlreadyDrawedLen := AlreadyDrawedLen + Len;
        end;

        EscOffset := EscPos + Length(EscSeq);
        SetCanvasStyle(aCanvas, EscSeq);
      end;


      if AlreadyDrawedLen < DrawLen then
      begin
        aCanvas.TextOut(X, Y, Copy(Line, EscOffset, DrawLen - AlreadyDrawedLen));
        X := aCanvas.PenPos.X;
      end;

      if DrawLen <> Length(CleanLine) then
        aCanvas.TextOut(X, Y, '...');

      Inc(Y, TextSize.cy);
      if Y + TextSize.Cy > aRect.Bottom then
        Exit;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

{ TVMSearchResultsListListener }

constructor TVMSearchResultsListListener.Create(aOnListChanged: TProc);
begin
  inherited Create;
  FOnListChanged := aOnListChanged;
end;

procedure TVMSearchResultsListListener.ListChanged;
begin
  if Assigned(FOnListChanged) then
    FOnListChanged;
end;

end.
