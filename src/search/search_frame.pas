unit search_frame;

{$I cond_define.inc}

interface

uses
  vmsys, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls, Grids, Menus, vtree_mod, ComCtrls,
  search_types, generics.collections, IniFiles,
  Tabs, Types, VirtualTrees, observer, search_handler,
  ActnList, Buttons, ExtCtrls
{$IFNDEF DELPHIXE}, System.Actions{$ENDIF}
  ;

type
  TVirtualStringTree = class(TExtVirtualStringTree);
  TSearchResultFrameSettings = class(TExtVirtualTreeParams)
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

  TResulsInfoData = class(TExtObject)
  strict private
    FInfo: TSearchInfo;
  public
    SelectedIdx: Cardinal;

    constructor Create(aSelIdx: Integer; aInfo: TSearchInfo);
    property Info: TSearchInfo read FInfo;
  end;

  TSearchResultFrameClass = class of TSearchResultFrame;
  TSearchResultFrame = class(TFrame)
    pmTabs: TPopupMenu;
    miCloseTab: TMenuItem;
    actlst: TActionList;
    actCloseTab: TAction;
    pmTreeView: TPopupMenu;
    miShowHeader: TMenuItem;
    vstResults: TVirtualStringTree;
    tbcTabs: TTabSet;
    pnlStatus: TFlowPanel;
    btnShowErrors: TSpeedButton;
    btnCancel: TSpeedButton;
    lblStatus: TLabel;
    procedure vstResultsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure actCloseTabExecute(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnShowErrorsClick(Sender: TObject);
    procedure vstResultsDblClick(Sender: TObject);
    procedure vstResultsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vstResultsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FrameEnter(Sender: TObject);
    procedure miCancellCurrentClick(Sender: TObject);
    procedure miShowHeaderClick(Sender: TObject);
    procedure pmTreeViewPopup(Sender: TObject);
    procedure tbcTabsChange(Sender: TObject; NewTab: Integer; var AllowChange:
        Boolean);
    procedure vstResultsCanSplitterResizeNode(Sender: TBaseVirtualTree; P: TPoint; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstResultsNodeHeightTracking(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      Shift: TShiftState; var TrackPoint: TPoint; P: TPoint; var Allowed: Boolean);
    procedure vstResultsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    FHandler: IObjectHolder<TSearchHandler>;
    FHandlerListener: ISearchResultsListener;
    FResults: TList<TResulsInfoData>;
    FTextDrawer: TFormatTextDrawer;
    FCurInfoIndex: Integer;
    FStatusObservers: TList<IDataObserver<TSearchStatusCode>>;
    FStatusTextObserver: IDataObserver<string>;
    FResultsObserver: IDataObserver<TVMSearchResultsList>;
    FErrorsObserver: IDataObserver<TStringList>;

    procedure OnSearchInfoAdded(aIndex: Integer);
    procedure OnSearchInfoRemoved(aIndex: Integer);
    procedure OnStatusTextChanged(aStatusText: string);
    procedure OnNewResults(aList: TVMSearchResultsList);
    procedure OnErrorsChanged(aErrors: TStringList);

    procedure RemoveStatusObservers;
    procedure UpdateResults;
    procedure SetCurInfo(aIndex: Integer);
    function GetCurInfo: TResulsInfoData;
    property CurInfo: TResulsInfoData read GetCurInfo;
    procedure OnKeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenCurrentFile;
    procedure CloseCurrentTab;
    function TabIndexBySearchInfo(aInfo: TSearchInfo): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowResultItem(aIsNext: Boolean);

    procedure SetHandler(aHandler: TSearchHandler);
  end;

implementation

uses
  Math, RichEdit, StrUtils, str_utils, virtualtrees.utils, collections.common, vm.memodlg;

{$R *.dfm}

const
  cstFirstColumn = 0;
  cstColText     = 0;
  cstColFile     = 1;
  cstColLine     = 2;
  cstColRating   = 3;
  cstColPath     = 4;
  cstLastColumnt = 4;

type
  TStatusObserver = class(TDelegatedDataObserver<TSearchStatusCode>)
  private
    FSearchId: Int64;
    FFrame: TSearchResultFrame;
    procedure OnChanged(aData: TSearchStatusCode);
  public
    constructor Create(aFrame: TSearchResultFrame; aSearchId: Int64);
  end;

function GetTabText(aInfo: TSearchInfo): string;
var
  Prefix: string;
begin
  Result := '';
  if aInfo = nil then
    Exit;

  case aInfo.Status.getValue of
    ssc_Successful: Prefix := '[R]';
    ssc_Queued:     Prefix := '[Q]';
    ssc_Searching:  Prefix := '[S]';
    ssc_Error:      Prefix := '[E]';
    ssc_Cancelled:  Prefix := '[C]';
    else            Prefix := '[U]';
  end;
  Result := Prefix + ' ' + aInfo.SearchText;
end;

{ TSearchResultFrame }

procedure TSearchResultFrame.actCloseTabExecute(Sender: TObject);
begin
  CloseCurrentTab;
end;

function TSearchResultFrame.TabIndexBySearchInfo(aInfo: TSearchInfo): Integer;
var
  I: Integer;
begin
  Result := -1;
  if aInfo = nil then
    Exit;

  for I := 0 to FResults.Count - 1 do
    if FResults[I].Info = aInfo then
      Exit(I);
end;

procedure TSearchResultFrame.OnSearchInfoAdded(aIndex: Integer);
var
  List: TList<TSearchInfo>;
  StatusObserver: IDataObserver<TSearchStatusCode>;
  Info: TSearchInfo;
  FrameHolder: IObjectHolder<TSearchResultFrame>;
begin
  if (FHandler = nil) or not FHandler.IsAlive then
    Exit;

  List := FHandler.Obj.SearchResults;
  if (aIndex < 0) or (aIndex >= List.Count) then
    Exit;

  Info := List[aIndex];
  FResults.Insert(aIndex, TResulsInfoData.Create(0, Info));

  tbcTabs.Visible := True;
  tbcTabs.Tabs.Insert(aIndex, GetTabText(Info));
  tbcTabs.TabIndex := aIndex;

  FrameHolder := TObjectHolder<TSearchResultFrame>.Create(Self, False);
  StatusObserver := TStatusObserver.Create(Self, Info.SearchId);
  FStatusObservers.Insert(aIndex, StatusObserver);
  Info.Status.RegisterObserver(StatusObserver);
end;

procedure TSearchResultFrame.OnSearchInfoRemoved(aIndex: Integer);
var
  NewTabIndex, OldTabIndex: Integer;
begin
  if (FHandler = nil) or not FHandler.IsAlive then
    Exit;

  OldTabIndex := tbcTabs.TabIndex;
  if (aIndex >= OldTabIndex) then
    tbcTabs.TabIndex := -1;
  FResults.Delete(aIndex);
  FStatusObservers.Delete(aIndex);
  tbcTabs.Tabs.Delete(aIndex);
  tbcTabs.Visible := tbcTabs.Tabs.Count > 0;
  if (aIndex >= OldTabIndex) then
  begin
    NewTabIndex := OldTabIndex - 1;
    if (NewTabIndex < 0) and (FResults.Count > 0) then
      NewTabIndex := 0;

    tbcTabs.TabIndex := NewTabIndex;
  end;
end;

procedure TSearchResultFrame.OnStatusTextChanged(aStatusText: string);
begin
  lblStatus.Caption := aStatusText;
end;

procedure TSearchResultFrame.OnNewResults(aList: TVMSearchResultsList);
begin
  UpdateResults;
end;

procedure TSearchResultFrame.OnErrorsChanged(aErrors: TStringList);
begin
  if (aErrors = nil) or (aErrors.Count <= 0) then
    btnShowErrors.Visible := False
  else
    btnShowErrors.Visible := True;
end;

type
  TTabControlFriend = class(TTabSet);

procedure TSearchResultFrame.CloseCurrentTab;
begin
  FHandler.Obj.RemoveSearchInfo(tbcTabs.TabIndex);
end;

constructor TSearchResultFrame.Create(AOwner: TComponent);
begin
  inherited;
  FCurInfoIndex := -1;
  FResults := TObjectList<TResulsInfoData>.Create;
  FStatusObservers := TList<IDataObserver<TSearchStatusCode>>.Create;
  FHandlerListener := TDelegatedSearchResultsListener.Create(OnSearchInfoAdded, OnSearchInfoRemoved);
  FStatusTextObserver := TDelegatedDataObserver<string>.Create(OnStatusTextChanged);
  FResultsObserver := TDelegatedDataObserver<TVMSearchResultsList>.Create(OnNewResults);
  FErrorsObserver := TDelegatedDataObserver<TStringList>.Create(OnErrorsChanged);

  FTextDrawer := TFormatTextDrawer.Create;
  vstResults.Clear;
  vstResults.Colors.TreeLineColor := vstResults.Colors.BackGroundColor;
  vstResults.DefaultNodeHeight := 21;
  tbcTabs.Tabs.Clear;
  tbcTabs.Visible := False;
  OnKeyDown := OnKeyDownEvent;
  vstResults.OnKeyDown := OnKeyDownEvent;
  TTabControlFriend(tbcTabs).OnKeyDown := OnKeyDownEvent;

  vstResults.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort,
      toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale];
  vstResults.TreeOptions.MiscOptions := [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick,
      toWheelPanning, toNodeHeightResize, toEditOnClick];
  vstResults.TreeOptions.PaintOptions := [toHideFocusRect, toShowButtons, toShowDropmark, toThemeAware,
      toUseBlendedImages, toShowTreeLines, toShowVertGridLines, toFullVertGridLines, toShowHorzGridLines];
  vstResults.TreeOptions.SelectionOptions := [toFullRowSelect];
  vstResults.Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoFullRepaintOnResize, hoHeaderClickAutoSort];
  lblStatus.Caption := '';
end;

destructor TSearchResultFrame.Destroy;
begin
  // Remove observers
  SetCurInfo(-1);
  SetHandler(nil);
  FreeAndNil(FStatusObservers);
  FreeAndNil(FTextDrawer);
  FreeAndNil(FResults);
  inherited;
end;

procedure TSearchResultFrame.btnCancelClick(Sender: TObject);
begin
  miCancellCurrentClick(Sender);
end;

procedure TSearchResultFrame.btnShowErrorsClick(Sender: TObject);
begin
  if (CurInfo = nil) or (CurInfo.Info = nil) or
      (CurInfo.Info.Errors.getValue = nil) or (CurInfo.Info.Errors.getValue.Count <= 0) then
    Exit;

  TMemoDialog.Execute('Errors', CurInfo.Info.Errors.getValue.Text);
end;

procedure TSearchResultFrame.RemoveStatusObservers;
var
  List: TList<TSearchInfo>;
  I: Integer;
begin
  if (FHandler <> nil) and FHandler.IsAlive then
  begin
    List := FHandler.Obj.SearchResults;
    for I := 0 to List.Count - 1 do
      List[I].Status.RemoveObserver(FStatusObservers[I]);
  end;
end;

procedure TSearchResultFrame.FrameEnter(Sender: TObject);
begin
  if vstResults.CanFocus then
    vstResults.SetFocus;
end;

function TSearchResultFrame.GetCurInfo: TResulsInfoData;
begin
  if (FCurInfoIndex >= 0) and (FCurInfoIndex < FResults.Count) then
    Result := FResults[FCurInfoIndex]
  else
    Result := nil;
end;

procedure TSearchResultFrame.UpdateResults;
var
  CurNode, Node: PVirtualNode;
begin
  vstResults.Clear;
  if (CurInfo = nil) or (CurInfo.Info = nil) or (CurInfo.Info.Results.getValue = nil) then
    Exit;

  vstResults.RootNodeCount := CurInfo.Info.Results.getValue.Count;
  if CurInfo.Info.Results.getValue.Count <= 0 then
    Exit;

  Node := nil;
  if CurInfo.SelectedIdx <= 0 then
    Node := vstResults.GetFirstChild(vstResults.RootNode)
  else
    for CurNode in vstResults.Nodes do
      if CurInfo.SelectedIdx = CurNode.Index then
      begin
        Node := CurNode;
        break;
      end;

  if Node <> nil then
    vstResults.Selected[Node] := True;
end;

procedure TSearchResultFrame.SetCurInfo(aIndex: Integer);
begin
  if (aIndex >= 0) and (FCurInfoIndex = aIndex) then
    Exit;

  if (CurInfo <> nil) and (CurInfo.Info <> nil) then
  begin
    CurInfo.Info.Results.RemoveObserver(FResultsObserver);
    CurInfo.Info.StatusText.RemoveObserver(FStatusTextObserver);
    CurInfo.Info.Errors.RemoveObserver(FErrorsObserver);
  end;

  FCurInfoIndex := aIndex;
  if (CurInfo <> nil) and (CurInfo.Info <> nil) then
  begin
    CurInfo.Info.Results.RegisterObserver(FResultsObserver);
    CurInfo.Info.StatusText.RegisterObserver(FStatusTextObserver);
    CurInfo.Info.Errors.RegisterObserver(FErrorsObserver);
  end
  else
  begin
    lblStatus.Visible := False;
    btnShowErrors.Visible := False;
  end;

  UpdateResults;
end;

procedure TSearchResultFrame.SetHandler(aHandler: TSearchHandler);
var
  I: Integer;
begin
  if (FHandler <> nil) and (FHandler.Obj = aHandler) then
    Exit;

  if (FHandler <> nil) and FHandler.IsAlive then
  begin
    FHandler.Obj.Listeners.RemoveListener(FHandlerListener);
    RemoveStatusObservers;
    SetCurInfo(-1)
  end;

  if aHandler <> nil then
    FHandler := TObjectHolder<TSearchHandler>.Create(aHandler, False)
  else
    FHandler := nil;

  FResults.Clear;
  tbcTabs.Tabs.Clear;
  tbcTabs.TabIndex := -1;

  if FHandler <> nil then
  begin
    for I := 0 to FHandler.Obj.SearchResults.Count - 1 do
      OnSearchInfoAdded(I);
    FHandler.Obj.Listeners.RegisterListener(FHandlerListener);
  end;
end;

procedure TSearchResultFrame.vstResultsCanSplitterResizeNode(Sender: TBaseVirtualTree; P: TPoint; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Node <> nil;
end;

procedure TSearchResultFrame.vstResultsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (Node <> nil) and vstResults.Selected[Node] and (CurInfo <> nil) then
     CurInfo.SelectedIdx := Node.Index;
end;

procedure TSearchResultFrame.vstResultsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Item1, Item2: TVMSearchResultsItem;
begin
  Item1 := vstResults.GetNodeData<TVMSearchResultsItem>(Node1);
  Item2 := vstResults.GetNodeData<TVMSearchResultsItem>(Node2);
  if (Item1 = nil) or (Item2 = nil) then
    Exit;

  case Column of
    cstColFile: Result := CompareText(Item1.FileName, Item2.FileName);
    cstColText: Result := CompareText(Item1.Text, Item2.Text);
    cstColLine: Result := Item1.Line - Item2.Line;
    cstColRating: Result := Item1.Rating - Item2.Rating;
  end;
end;

procedure TSearchResultFrame.vstResultsDblClick(Sender: TObject);
begin
  OpenCurrentFile;
end;

procedure TSearchResultFrame.miCancellCurrentClick(Sender: TObject);
begin
  if FHandler.IsAlive and (CurInfo <> nil) and (CurInfo.Info <> nil) then
    FHandler.Obj.CancellSearch(CurInfo.Info.SearchId);
end;

procedure TSearchResultFrame.vstResultsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Item: TVMSearchResultsItem;
begin
  Item := vstResults.GetNodeData<TVMSearchResultsItem>(Node);
  if Item = nil then
    Exit;

  case Column of
    cstColText:
    begin
      DefaultDraw := not FTextDrawer.Draw(Item.RawText, TargetCanvas, CellRect);
    end;
  end;
end;

procedure TSearchResultFrame.vstResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Item: TVMSearchResultsItem;
begin
  Item := vstResults.GetNodeData<TVMSearchResultsItem>(Node);
  if Item = nil then
    Exit;

  case Column of
    cstColFile: CellText := Item.FileName;
    cstColPath: CellText := Item.FilePath;
    cstColText: CellText := Item.Text;
    cstColLine: CellText := IntToStr(Item.Line);
    cstColRating: CellText := IntToStr(Item.Rating);
  end;
end;

procedure TSearchResultFrame.vstResultsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  if (CurInfo <> nil) and (CurInfo.Info <> nil) and (CurInfo.Info.Results.getValue <> nil) then
    Node.SetData<TVMSearchResultsItem>(CurInfo.Info.Results.getValue.Items[Node.Index]);
end;

procedure TSearchResultFrame.vstResultsNodeHeightTracking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; Shift: TShiftState; var TrackPoint: TPoint; P: TPoint; var Allowed: Boolean);
begin
  if P.Y - TrackPoint.Y < TExtVirtualTreeParams.cstMinNodeHeight then
    TrackPoint.Y := P.Y - TExtVirtualTreeParams.cstMinNodeHeight;
  vstResults.UpdateNodesHeight(Max(TExtVirtualTreeParams.cstMinNodeHeight, P.Y - TrackPoint.Y));
end;

procedure TSearchResultFrame.OnKeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure ChangeTab(IsMoveNext: Boolean);
  var
    Ind: Integer;
  begin
    if IsMoveNext then
      Ind := tbcTabs.TabIndex + 1
    else
      Ind := tbcTabs.TabIndex - 1;

    if Ind < 0 then
      Ind := tbcTabs.Tabs.Count - 1
    else if (Ind >= tbcTabs.Tabs.Count) then
      Ind := 0;

    if (Ind >= tbcTabs.Tabs.Count) then
      Ind := -1;

    tbcTabs.TabIndex := Ind;
  end;
begin
  case Key of
    VK_TAB:
      if ssCtrl in Shift then
        ChangeTab(not (ssShift in Shift));
    VK_ESCAPE:
    begin
      if Parent <> nil then
        Parent.Hide;
      if (FHandler <> nil) and FHandler.IsAlive then
        FHandler.Obj.FocusEditor;
    end;
    Ord('W'),
    VK_F4:
      if ssCtrl in Shift then
        CloseCurrentTab;
    Ord('E'),
    VK_RETURN:
      OpenCurrentFile;
  end
end;

procedure TSearchResultFrame.OpenCurrentFile;
var
  Item: TVMSearchResultsItem;
begin
  Item := vstResults.GetNodeData<TVMSearchResultsItem>(vstResults.FocusedNode);
  if Item = nil then
    Exit;

  if (FHandler <> nil) and FHandler.IsAlive then
    FHandler.Obj.OpenFileInEditor(IncludeTrailingPathDelimiter(Item.FilePath) + Item.FileName, Item.Line);
end;

procedure TSearchResultFrame.pmTreeViewPopup(Sender: TObject);
begin
  miShowHeader.Checked := hoVisible in vstResults.Header.Options;
end;

procedure TSearchResultFrame.miShowHeaderClick(Sender: TObject);
begin
  vstResults.ShowHeader(not (hoVisible in vstResults.Header.Options));
end;

procedure TSearchResultFrame.ShowResultItem(aIsNext: Boolean);
var
  NewNode: PVirtualNode;
begin
  if (CurInfo = nil) or (CurInfo.Info = nil) or (CurInfo.Info.Results.getValue = nil) or
      (CurInfo.Info.Results.getValue.Count <= 0) then
    Exit;

  if vstResults.FocusedNode = nil then
    NewNode := vstResults.GetFirst
  else if aIsNext and (vstResults.FocusedNode.NextSibling <> nil) then
    NewNode := vstResults.FocusedNode.NextSibling
  else if not aIsNext and (vstResults.FocusedNode.PrevSibling <> nil) then
    NewNode := vstResults.FocusedNode.PrevSibling
  else
    NewNode := nil;

  if NewNode <> nil then
  begin
    vstResults.Selected[NewNode] := True;
    OpenCurrentFile;
  end
end;

procedure TSearchResultFrame.tbcTabsChange(Sender: TObject; NewTab: Integer;
    var AllowChange: Boolean);
begin
  SetCurInfo(NewTab);
end;

{ TResulsInfoData }

constructor TResulsInfoData.Create(aSelIdx: Integer; aInfo: TSearchInfo);
begin
  inherited Create;
  SelectedIdx := aSelIdx;
  SetNotifiableObjectProperty(@FInfo, aInfo);
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

{ TStatusObserver }

constructor TStatusObserver.Create(aFrame: TSearchResultFrame; aSearchId: Int64);
begin
  inherited Create(OnChanged);
  FSearchId := aSearchId;
  FFrame := aFrame;
end;

procedure TStatusObserver.OnChanged(aData: TSearchStatusCode);
var
  TabIndex: Integer;
  Info: TSearchInfo;
begin
  FFrame.btnCancel.Visible := aData in [ssc_Queued, ssc_Searching];
  if not FFrame.FHandler.IsAlive then
    Exit;

  Info := TCollectionsUtils.FirstThat<TSearchInfo>(FFrame.FHandler.Obj.SearchResults,
    function (aInfo: TSearchInfo): Boolean
    begin
      Result := aInfo.SearchId = FSearchId;
    end);

  TabIndex := FFrame.TabIndexBySearchInfo(Info);
  if TabIndex < 0 then
    Exit;

  if Info <> nil then
    FFrame.tbcTabs.Tabs[TabIndex] := GetTabText(Info);
end;

end.

