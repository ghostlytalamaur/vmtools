unit vtree_mod;

interface

uses
  VirtualTrees, graphics, windows, controls, classes, inifiles;

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
  public
    procedure SortTree(Column: TColumnIndex; Direction: TSortDirection; DoInit: Boolean = True); override;
    procedure LoadSetting(aParams: TExtVirtualTreeParams);
    procedure StoreSetting(aParams: TExtVirtualTreeParams);
    procedure ShowHeader(aIsShow: Boolean);
  end;

implementation

uses
  messages, sysutils, math;

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

procedure TExtVirtualStringTree.DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer);
begin
  inherited;
end;

procedure TExtVirtualStringTree.DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer;
  UseSelectedBkColor: Boolean);
begin
  inherited;
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
