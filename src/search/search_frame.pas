unit search_frame;

{$I cond_define.inc}

interface

uses
  vmsys, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls, Menus, vtree_mod,
  search_types, generics.collections, generics.defaults, IniFiles,
  Tabs, Types, VirtualTrees, observer, search_handler,
  ActnList, Buttons, ExtCtrls, search_table
{$IFNDEF DELPHIXE}, System.Actions{$ENDIF}
  , vm.gui.control_updater;

type
  TVirtualStringTree = class(TSearchResultsVirtualTree);
  TResulsInfoData = class(TExtObject)
  strict private
    FInfo: TSearchInfo;
  public
    SelectedIdx: Integer;

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
    procedure actCloseTabExecute(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnShowErrorsClick(Sender: TObject);
    procedure vstResultsDblClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure miCancellCurrentClick(Sender: TObject);
    procedure miShowHeaderClick(Sender: TObject);
    procedure pmTreeViewPopup(Sender: TObject);
    procedure tbcTabsChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
    procedure vstResultsAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements:
        THeaderPaintElements);
    procedure vstResultsCanSplitterResizeNode(Sender: TBaseVirtualTree; P: TPoint; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstResultsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstResultsHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements:
        THeaderPaintElements);
    procedure vstResultsStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
  private
    { Private declarations }
    FHandler: IObjectHolder<TSearchHandler>;
    FHandlerListener: ISearchResultsListener;
    FResults: TList<TResulsInfoData>;
    FCurInfoIndex: Integer;
    FStatusObservers: TList<IDataObserver<TSearchStatusCode>>;
    FStatusTextObserver: IDataObserver<string>;
    FErrorsObserver: IDataObserver<TStringList>;
    FStatusTextUpdater: TControlUpdater;

    procedure OnSearchInfoAdded(aIndex: Integer);
    procedure OnSearchInfoRemoved(aIndex: Integer);
    procedure OnStatusTextChanged(aStatusText: string);
    procedure OnErrorsChanged(aErrors: TStringList);

    procedure RemoveStatusObservers;
    procedure UpdateStatus;
    procedure SetCurInfo(aIndex: Integer);
    procedure SelectNode(aIndex: Integer);
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

  case aInfo.Status.Value of
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
  FStatusTextUpdater.RequestUpdate;
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
  FErrorsObserver := TDelegatedDataObserver<TStringList>.Create(OnErrorsChanged);

  FStatusTextUpdater := TControlUpdater.Create(UpdateStatus);

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
  vstResults.Header.Options := vstResults.Header.Options + [hoColumnResize, hoDrag, hoShowSortGlyphs, hoFullRepaintOnResize, hoHeaderClickAutoSort];
end;

destructor TSearchResultFrame.Destroy;
begin
  // Remove observers
  SetCurInfo(-1);
  SetHandler(nil);
  FreeAndNil(FStatusObservers);
  FreeAndNil(FStatusTextUpdater);
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

  TMemoDialog.Execute('Errors', CurInfo.Info.Errors);
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

procedure TSearchResultFrame.UpdateStatus;
begin
  if CurInfo <> nil then
    lblStatus.Caption := CurInfo.Info.StatusText.Value
  else
    lblStatus.Caption := '';
  lblStatus.Visible := lblStatus.Caption <> '';
end;

procedure TSearchResultFrame.SelectNode(aIndex: Integer);
var
  Node, CurNode: PVirtualNode;
begin
  Node := nil;
  if CurInfo.SelectedIdx <= 0 then
    Node := vstResults.GetFirstChild(vstResults.RootNode)
  else
    for CurNode in vstResults.Nodes do
      if CurInfo.SelectedIdx = CurNode.GetData<Integer> then
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
    CurInfo.Info.StatusText.RemoveObserver(FStatusTextObserver);
    CurInfo.Info.Errors.RemoveObserver(FErrorsObserver);
  end;

  FCurInfoIndex := aIndex;
  if (CurInfo <> nil) and (CurInfo.Info <> nil) then
  begin
    CurInfo.Info.StatusText.RegisterObserver(FStatusTextObserver);
    CurInfo.Info.Errors.RegisterObserver(FErrorsObserver);
    btnCancel.Visible := CurInfo.Info.Status.getValue in [ssc_Queued, ssc_Searching];

    vstResults.SetData(CurInfo.Info.Results);
    SelectNode(CurInfo.SelectedIdx);
  end
  else
  begin
    vstResults.SetData(nil);
    FStatusTextUpdater.RequestUpdate;
    btnShowErrors.Visible := False;
    btnCancel.Visible := False;
  end;
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
     CurInfo.SelectedIdx := Node.GetData<Integer>;
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
  Item := vstResults.GetNodeData(vstResults.FocusedNode);
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

procedure TSearchResultFrame.vstResultsAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const
    Elements: THeaderPaintElements);
begin
  PaintInfo.TargetCanvas.Brush.Color := vstResults.Color;
  PaintInfo.TargetCanvas.Font.Color := vstResults.Font.Color;
  PaintInfo.TargetCanvas.FillRect(PaintINfo.PaintRectangle);
end;

procedure TSearchResultFrame.vstResultsHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var
    Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground]
end;

procedure TSearchResultFrame.vstResultsStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
begin
end;

{ TResulsInfoData }

constructor TResulsInfoData.Create(aSelIdx: Integer; aInfo: TSearchInfo);
begin
  inherited Create;
  SelectedIdx := aSelIdx;
  SetNotifiableObjectProperty(@FInfo, aInfo);
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

  if FFrame.tbcTabs.TabIndex = TabIndex then
    FFrame.btnCancel.Visible := aData in [ssc_Queued, ssc_Searching];

  if Info <> nil then
    FFrame.tbcTabs.Tabs[TabIndex] := GetTabText(Info);
end;

end.


