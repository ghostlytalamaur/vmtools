unit vtree_mod;

interface

uses
  VirtualTrees, graphics, windows, controls, classes, inifiles, stdctrls, messages;

type
  TExtVirtualTreeParams = class
  public const
    cstMinNodeHeight = 19;
  private const
    cstSortDirectionAscStr  = 'sdAscending';
    cstSortDirectionDescStr = 'sdDescending';
  private
    FSortColumn: Integer;
    FColCount: Integer;
    FColPos: TArray<Integer>;
    FColWidth: TArray<Integer>;
    FVisibleHeader: Boolean;
    FNodeHeight: Integer;
    FSortDirection: TSortDirection;

    function GetColPos(aIdx: Integer): Integer;
    function GetColWidth(aIdx: Integer): Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetColPos(aIdx: Integer; const Value: Integer);
    procedure SetColWidth(aIdx: Integer; const Value: Integer);

    function SortDirectionFromStr(const aStr: string): TSortDirection;
    function SortDirectionToStr(const aDirection: TSortDirection): string;
  public
    constructor Create;
    procedure WriteSettings(aIni: TCustomIniFile; const aSection: string);
    procedure ReadSettings(aIni: TCustomIniFile; const aSection: string);

    property ColCount: Integer read FColCount write SetColCount;
    property ColPos[aIdx: Integer]: Integer read GetColPos write SetColPos;
    property ColWidth[aIdx: Integer]: Integer read GetColWidth write SetColWidth;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property VisibleHeader: Boolean read FVisibleHeader write FVisibleHeader;
    property NodeHeight: Integer read FNodeHeight write FNodeHeight;
  end;

  TExtVirtualStringTree = class(TVirtualStringTree)
  private
    FSearchEdit: TEdit;
    FLastHitNode: PVirtualNode;
  protected
    function DoEndEdit: Boolean; override;
    function DoCancelEdit: Boolean; override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer); override;
    procedure DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer; UseSelectedBkColor: Boolean = False); override;
    procedure UpdateNodesHeight(aNewHeight: Integer);

    procedure DoStateChange(Enter: TVirtualTreeStates; Leave: TVirtualTreeStates = []); override;
    procedure UpdateSearchEditVisibility(aShouldShow: Boolean);
    procedure SetParent(AParent: TWinControl); override;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    function FindIncrementalNode(aStartFrom: TVTSearchStart; aSkipFirst, aIsForward: Boolean; aText: string): PVirtualNode;
  private
    procedure OnEditKeyPress(Sender: TObject; var Key: Char);
    procedure OnEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnEditChange(Sender: TObject);
    procedure OnEditExit(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure SortTree(Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); override;
    procedure LoadSetting(aParams: TExtVirtualTreeParams);
    procedure StoreSetting(aParams: TExtVirtualTreeParams);
    procedure ShowHeader(aIsShow: Boolean);
  end;

implementation

uses
  sysutils, math;

{ TExtVirtualStringTree }

// Usage  NewColor:= Blend(Color1, Color2, blending level 0 to 100);
function Blend(Color1, Color2: TColor; A: Byte): TColor;
var
  c1, c2: LongInt;
  r, g, b, v1, v2: byte;
begin
  A:= Round(2.55 * A);
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= A * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;

constructor TExtVirtualStringTree.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Visible := False;
  FSearchEdit.OnKeyDown := OnEditKeyDown;
  FSearchEdit.OnKeyPress := OnEditKeyPress;
  FSearchEdit.OnChange := OnEditChange;
  FSearchEdit.OnExit := OnEditExit;
end;

procedure TExtVirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if AbsoluteIndex(Node) mod 2 = 0 then
  begin
    Canvas.Brush.Color := Blend(Colors.BackGroundColor, $000000, 90);
    Canvas.FillRect(CellRect);
  end;

  inherited;
end;

function TExtVirtualStringTree.DoEndEdit: Boolean;
begin
  Result := inherited;
  if Result and CanFocus then
    SetFocus;
end;

function TExtVirtualStringTree.DoCancelEdit: Boolean;
begin
  Result := inherited;
  if Result and CanFocus then
    SetFocus;
end;

function TExtVirtualStringTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
begin
  Result := False;
  case CharCode of
    VK_ESCAPE:
      if Shift = [] then
        Result := CancelEditNode;
    VK_RETURN:
      if IsEditing then
        EndEditNode
      else if CanEdit(FocusedNode, FocusedColumn) then
        EditNode(FocusedNode, FocusedColumn);
  end;
  if not Result then
    Result := inherited;
end;

function TExtVirtualStringTree.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  KeyCode: Integer;
begin
  // Change focused node on mouse wheel by simulating Up/Down KeyPress.
  if WheelDelta < 0 then
    KeyCode := VK_DOWN
  else
    KeyCode := VK_UP;
  Perform(WM_KEYDOWN, KeyCode, 1);

  inherited;

  // We need return true, otherwise tree will srolled in TBaseVirtualTree.CMMouseWheel()
  Result := True;
end;

procedure TExtVirtualStringTree.UpdateSearchEditVisibility(aShouldShow: Boolean);
begin
  if FSearchEdit = nil then
    Exit;

  if FSearchEdit.Visible = aShouldShow then
    Exit;

  FSearchEdit.Visible := aShouldShow;
  if FSearchEdit.Visible then
  begin
    FSearchEdit.Top := Top;
    FSearchEdit.Left := Left;
    FSearchEdit.Width := Width;
    Top := Top + FSearchEdit.Height;
    Height := Height - FSearchEdit.Height;
    FSearchEdit.SetFocus;
  end
  else
  begin
    Top := Top - FSearchEdit.Height;
    Height := Height + FSearchEdit.Height;
    SetFocus;
    DoStateChange([], [tsIncrementalSearching]);
  end;
  FSearchEdit.Text := '';
  FLastHitNode := nil;
end;

procedure TExtVirtualStringTree.DoStateChange(Enter, Leave: TVirtualTreeStates);
begin
  inherited;
  if FSearchEdit <> nil then
  begin
    if (tsIncrementalSearchPending in Enter) and (IncrementalSearch <> isNone) then
      UpdateSearchEditVisibility(True)
  end;
end;

procedure TExtVirtualStringTree.WMChar(var Message: TWMChar);
begin
  if (FSearchEdit <> nil) and (IncrementalSearch <> isNone) and (Message.CharCode > 0) and (tsIncrementalSearchPending in TreeStates) then
  begin
    FSearchEdit.Dispatch(Message);
    DoStateChange([], [tsIncrementalSearchPending]);
  end
  else
    inherited
end;

function TExtVirtualStringTree.FindIncrementalNode(aStartFrom: TVTSearchStart; aSkipFirst, aIsForward: Boolean; aText: string): PVirtualNode;

  function SelectNext(aNode: PVirtualNode): PVirtualNode;

    function SelectSibling(aNode: PVirtualNode): PVirtualNode;
    begin
      if aIsForward then
        Result := aNode.NextSibling
      else
        Result := aNode.PrevSibling;
    end;

  begin
    Result := nil;
    if aNode = nil then
      Exit;

    case IncrementalSearch of
      isAll: Result := SelectSibling(aNode);
      isNone: Result := nil;
      isInitializedOnly:
      begin
        aNode := SelectSibling(aNode);
        while (aNode <> nil) and not (vsInitialized in aNode.States) do
          aNode := SelectSibling(aNode);
        if (aNode <> nil) and (vsInitialized in aNode.States) then
          Result := aNode;
      end;
      isVisibleOnly:
      begin
        if aIsForward then
          Result := GetNextVisible(aNode)
        else
          Result := GetPreviousVisible(aNode);
      end;
    end;
  end;

var
  Node: PVirtualNode;
begin
  Result := nil;
  Node := nil;
  case aStartFrom of
    ssAlwaysStartOver:
    begin
      if IncrementalSearch = isVisibleOnly then
      begin
        if aIsForward then
          Node := GetFirstVisibleChild(RootNode)
        else
          Node := GetLastVisibleChild(RootNode);
      end
      else if aIsForward then
        Node := GetFirstChild(RootNode)
      else
        Node := GetLastChild(RootNode);
    end;
    ssLastHit:
    begin
      Node := FLastHitNode;
      if Node = nil then
        Node := FocusedNode;
    end;
    ssFocusedNode: Node := FocusedNode;
  end;

  if Node = nil then
    Exit;

  if aSkipFirst then
    Node := SelectNext(Node);

  while (Node <> nil) and (DoIncrementalSearch(Node, aText) <> 0) do
    Node := SelectNext(Node);
  if (Node <> nil) and (DoIncrementalSearch(Node, aText) = 0) then
  begin
    FLastHitNode := Node;
    Result := Node;
  end;
end;

procedure TExtVirtualStringTree.OnEditKeyPress(Sender: TObject; var Key: Char);
var
  Node: PVirtualNode;
begin
  if Ord(Key) >= 32 then
  begin
    Node := FindIncrementalNode(IncrementalSearchStart, False, IncrementalSearchDirection = sdForward, FSearchEdit.Text + Key);
    if Node = nil then
      Key := #0
    else
      Selected[Node] := True;
  end;
end;

procedure TExtVirtualStringTree.OnEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  Node := nil;
  case Key of
    VK_DOWN,
    VK_UP:
    begin
      Node := FindIncrementalNode(ssFocusedNode, True, Key = VK_DOWN, FSearchEdit.Text);
      Key := 0;
    end;
    VK_ESCAPE:
    begin
      FSearchEdit.Text := '';
      UpdateSearchEditVisibility(False);
    end;
  end;

  if Node <> nil then
    Selected[Node] := True;
end;

procedure TExtVirtualStringTree.OnEditChange(Sender: TObject);
begin
  if FSearchEdit = nil then
    Exit;

  UpdateSearchEditVisibility(FSearchEdit.Text <> '')
end;

procedure TExtVirtualStringTree.OnEditExit(Sender: TObject);
begin
  if FSearchEdit = nil then
    Exit;

  UpdateSearchEditVisibility(False)
end;

procedure TExtVirtualStringTree.DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer);
begin
  inherited;
end;

procedure TExtVirtualStringTree.DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer;
  UseSelectedBkColor: Boolean);
begin
  inherited;
end;

procedure TExtVirtualStringTree.SetParent(AParent: TWinControl);
begin
  inherited;
  if FSearchEdit <> nil then
    FSearchEdit.Parent := aParent;
end;

procedure TExtVirtualStringTree.ShowHeader(aIsShow: Boolean);
begin
  if aIsShow then
    Header.Options := Header.Options + [hoVisible]
  else
    Header.Options := Header.Options - [hoVisible]
end;

procedure TExtVirtualStringTree.UpdateNodesHeight(aNewHeight: Integer);
var
  CurNode: PVirtualNode;
begin
  BeginUpdate;
  try
    aNewHeight := math.Max(TExtVirtualTreeParams.cstMinNodeHeight, aNewHeight);
    DefaultNodeHeight := aNewHeight;
    for CurNode in Nodes do
      NodeHeight[CurNode] := aNewHeight;
  finally
    EndUpdate;
  end;
end;

procedure TExtVirtualStringTree.SortTree(Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean);
var
  OldR, NewR: TRect;
  Run: PVirtualNode;
begin
  if Assigned(FocusedNode) then
  begin
    // make sure all parents of the node are expanded
    Run := FocusedNode.Parent;
    while Run <> RootNode do
    begin
      if not (vsExpanded in Run.States) then
        ToggleNode(Run);
      Run := Run.Parent;
    end;
    OldR := GetDisplayRect(FocusedNode, Header.MainColumn, True);
  end;

  inherited;

  if Assigned(FocusedNode) then
  begin
    NewR := GetDisplayRect(FocusedNode, Header.MainColumn, True);
    OffsetY := OffsetY - (NewR.Top - OldR.Top);
  end;
end;

procedure TExtVirtualStringTree.LoadSetting(aParams: TExtVirtualTreeParams);
var
  I: Integer;
begin
  if (aParams = nil) or (aParams.ColCount <> Header.Columns.Count) then
    Exit;

  BeginUpdate;
  try
    for I := 0 to Header.Columns.Count - 1 do
    begin
      Header.Columns[I].Position := aParams.ColPos[I];
      Header.Columns[I].Width := aParams.ColWidth[I];
    end;
    Header.SortColumn := aParams.SortColumn;
    Header.SortDirection := aParams.SortDirection;
    ShowHeader(aParams.VisibleHeader);
    UpdateNodesHeight(aParams.NodeHeight);
  finally
    EndUpdate;
  end;
end;

procedure TExtVirtualStringTree.StoreSetting(aParams: TExtVirtualTreeParams);
var
  I: Integer;
begin
  if aParams = nil then
    Exit;

  aParams.ColCount := Header.Columns.Count;
  for I := 0 to Header.Columns.Count - 1 do
  begin
    aParams.ColPos[I] := Header.Columns[I].Position;
    aParams.ColWidth[I] := Header.Columns[I].Width;
  end;
  aParams.SortColumn := Header.SortColumn;
  aParams.SortDirection := Header.SortDirection;
  aParams.VisibleHeader := hoVisible in Header.Options;
  aParams.NodeHeight := DefaultNodeHeight;
end;

{ TExtVirtualTreeParams }

constructor TExtVirtualTreeParams.Create;
begin
  inherited Create;
end;

function TExtVirtualTreeParams.GetColPos(aIdx: Integer): Integer;
begin
  Result := 0;
  if (aIdx >= 0) and (aIdx < ColCount) then
    Result := FColPos[aIdx]
end;

function TExtVirtualTreeParams.GetColWidth(aIdx: Integer): Integer;
begin
  Result := 50;
  if (aIdx >= 0) and (aIdx < ColCount) then
    Result := FColWidth[aIdx]
end;

procedure TExtVirtualTreeParams.SetColCount(const Value: Integer);
begin
  FColCount := Value;
  SetLength(FColWidth, ColCount);
  SetLength(FColPos, ColCount);
end;

procedure TExtVirtualTreeParams.SetColPos(aIdx: Integer; const Value: Integer);
begin
  if (aIdx >= 0) and (aIdx < ColCount) then
    FColPos[aIdx] := Value;
end;

procedure TExtVirtualTreeParams.SetColWidth(aIdx: Integer; const Value: Integer);
begin
  if (aIdx >= 0) and (aIdx < ColCount) then
    FColWidth[aIdx] := Value;
end;

function TExtVirtualTreeParams.SortDirectionFromStr(const aStr: string): TSortDirection;
begin
  if SameText(aStr, cstSortDirectionAscStr) then
    Result := sdAscending
  else if SameText(aStr, cstSortDirectionDescStr) then
    Result := sdDescending
  else
    Result := sdAscending;
end;

function TExtVirtualTreeParams.SortDirectionToStr(const aDirection: TSortDirection): string;
begin
  case aDirection of
    sdAscending:  Result := cstSortDirectionAscStr;
    sdDescending: Result := cstSortDirectionDescStr;
  else            Result := cstSortDirectionAscStr;
  end;
end;

procedure TExtVirtualTreeParams.ReadSettings(aIni: TCustomIniFile; const aSection: string);
var
  I: Integer;
begin
  ColCount := aIni.ReadInteger(aSection, 'ColCount', 0);
  for I := 0 to ColCount - 1 do
  begin
    FColPos[I] := aIni.ReadInteger(aSection, 'ColPos' + IntToStr(I), I);
    FColWidth[I] := aIni.ReadInteger(aSection, 'ColWidth' + IntToStr(I), 100);
  end;
  SortColumn := aIni.ReadInteger(aSection, 'SortColumn', 0);
  SortDirection := SortDirectionFromStr(aIni.ReadString(aSection, 'SortDirection', 'sdAscending'));
  VisibleHeader := aIni.ReadBool(aSection, 'VisibleHeader', True);
  NodeHeight := aIni.ReadInteger(aSection, 'NodeHeight', cstMinNodeHeight);
end;

procedure TExtVirtualTreeParams.WriteSettings(aIni: TCustomIniFile; const aSection: string);
var
  I: Integer;
begin
  aIni.WriteInteger(aSection, 'ColCount', Length(FColWidth));
  for I := 0 to Length(FColWidth) - 1 do
  begin
    aIni.WriteInteger(aSection, 'ColPos' + IntToStr(I), ColPos[I]);
    aIni.WriteInteger(aSection, 'ColWidth' + IntToStr(I), ColWidth[I]);
  end;
  aIni.WriteInteger(aSection, 'SortColumn', SortColumn);
  aIni.WriteString(aSection, 'SortDirection', SortDirectionToStr(SortDirection));
  aIni.WriteBool(aSection, 'VisibleHeader', VisibleHeader);
  aIni.WriteInteger(aSection, 'NodeHeight', NodeHeight);
end;


end.
