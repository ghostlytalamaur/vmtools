unit openfile_frame;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, extctrls,
  classes, inifiles, VirtualTrees, vtree_mod, Buttons,
  generics.collections, observer, vmsys, openfile_handler,
  base_params, collections.deque;

type
  TOpenFileFrameParams = class(TBaseParams)
  strict private
    FLastFiles: TDeque<string>;
  strict protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property LastFiles: TDeque<string> read FLastFiles;
  end;

  TVirtualStringTree = class(TExtVirtualStringTree);
  TOpenFileFrame = class(TFrame)
    Panel1: TPanel;
    vstFiles: TVirtualStringTree;
    btnRebuild: TSpeedButton;
    StatusBar1: TStatusBar;
    cmbFilter: TComboBoxEx;
    procedure vstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vstFilesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilesNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure btnRebuildClick(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
    procedure vstFilesIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result:
        Integer);
  strict private
    FFrameParams: TOpenFileFrameParams;
    FHandlerHolder: IObjectHolder<TBaseOpenFileHandler>;
    FFilteredList: TList<string>;
    FStatusObserver: IDataObserver<TOpenFileHandlerStatus>;
    FPathsObserver: IDataObserver<TList<string>>;

    procedure UpdateFilterCombo;
    procedure SetupControls;
    function OpenCurrentFile: Boolean;
    function TryOpenFileAt(aIndex: Integer): Boolean;
    procedure OnKeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetupTree;
    procedure UpdateStatus(aStatus: TOpenFileHandlerStatus);
    function GetHandler: TBaseOpenFileHandler;
    property Handler: TBaseOpenFileHandler read GetHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetHandler(aHandler: TBaseOpenFileHandler);
  end;

implementation

{$R *.dfm}

uses
  regularexpressions, strutils, str_utils, generics.defaults,
  collections.array_utils;

type
  TNodeData = class(TObject)
  strict private
    FFullPath: string;
    FFileName: string;
    FFilePath: string;
    function GetFileName: string;
    function GetFilePath: string;
  public
    constructor Create(const aFullPath: string);

    property FileName: string read GetFileName;
    property FilePath: string read GetFilePath;
    property FullPath: string read FFullPath;
  end;

const
  cstColumnFirst = 0;
  cstColumnNum   = 0;
  cstColumnFile  = 1;
  cstColumnPath  = 2;
  cstColumnLast  = 2;
  cstColumns: array [cstColumnFirst..cstColumnLast] of string = (
    '#',
    'Name',
    'Path'
  );
  cstColumnsWidth: array [cstColumnFirst..cstColumnLast] of Integer = (
    20,
    300,
    350
  );

{ TOpenFileFrame }

procedure TOpenFileFrame.btnRebuildClick(Sender: TObject);
begin
  if Handler <> nil then
    Handler.InvalidatePaths;
end;

constructor TOpenFileFrame.Create(AOwner: TComponent);
var
  FrameHolder: IObjectHolder<TOpenFileFrame>;
begin
  inherited;

  FFrameParams := TOpenFileFrameParams.Create;
  FFrameParams.ReadParams;
  FrameHolder := TObjectHolder<TOpenFileFrame>.Create(Self, False);
  FStatusObserver := TDelegatedDataObserver<TOpenFileHandlerStatus>.Create(
    procedure (aStatus: TOpenFileHandlerStatus)
    begin
      if not FrameHolder.IsAlive then
        Exit;

      FrameHolder.Obj.UpdateStatus(aStatus);
    end);

  FPathsObserver := TDelegatedDataObserver<TList<string>>.Create(
    procedure (aPaths: TList<string>)
    begin
      if not FrameHolder.IsAlive then
        Exit;

      FrameHolder.Obj.FFilteredList.Clear;
      if aPaths <> nil then
        FrameHolder.Obj.FFilteredList.AddRange(aPaths);
      FrameHolder.Obj.SetupControls;
    end);

  FFilteredList := TList<string>.Create;
  SetupTree;
  vstFiles.OnKeyDown := OnKeyDownEvent;
  cmbFilter.OnKeyDown := OnKeyDownEvent;
  UpdateFilterCombo;
  SetupControls;
end;

procedure TOpenFileFrame.SetupTree;
var
  I: Integer;
  Col: TVirtualTreeColumn;
begin
  vstFiles.Header.Columns.Clear;
  for I := Low(cstColumns) to High(cstColumns) do
  begin
    Col := vstFiles.Header.Columns.Add;
    Col.Text := cstColumns[I];
    Col.Width := cstColumnsWidth[I];
  end;
  vstFiles.Header.Columns[cstColumnNum].Options := vstFiles.Header.Columns[cstColumnNum].Options + [coSmartResize];
  vstFiles.Header.Options := vstFiles.Header.Options + [hoVisible, hoAutoResize];
  vstFiles.TreeOptions.AutoOptions := vstFiles.TreeOptions.AutoOptions + [toAutoScroll] - [toDisableAutoscrollOnFocus];
  vstFiles.TreeOptions.SelectionOptions := vstFiles.TreeOptions.SelectionOptions + [toFullRowSelect, toAlwaysSelectNode];
  vstFiles.TreeOptions.PaintOptions := vstFiles.TreeOptions.PaintOptions - [toShowRoot] +
      [toShowHorzGridLines, toHideFocusRect];
end;

procedure TOpenFileFrame.UpdateStatus(aStatus: TOpenFileHandlerStatus);
var
  StatusText: string;
begin
  case aStatus of
    ofht_Ready:          StatusText := 'Ready.';
    ofht_PendingRefresh: StatusText := 'Pending refresh.';
    ofht_RefreshList:    StatusText := 'Refreshing list...';
    ofht_Filtering:      StatusText := 'Filtering...';
  end;

  StatusBar1.Panels[0].Text := 'Status: ' + StatusText;
end;

destructor TOpenFileFrame.Destroy;
begin
  if Handler <> nil then
  begin
    Handler.Status.RemoveObserver(FStatusObserver);
    Handler.FilteredPaths.RemoveObserver(FPathsObserver);
  end;

  FreeAndNil(FFilteredList);
  FreeAndNil(FFrameParams);
  inherited;
end;

procedure TOpenFileFrame.cmbFilterChange(Sender: TObject);
begin
  if Handler <> nil then
    Handler.SetFilter(cmbFilter.Text)
  else
    FFilteredList.Clear;
end;

function TOpenFileFrame.GetHandler: TBaseOpenFileHandler;
begin
  if FHandlerHolder <> nil then
    Result := FHandlerHolder.Obj
  else
    Result := nil;
end;

procedure TOpenFileFrame.OnKeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure DoOpenFileAt(aIndex: Integer);
  begin
    if [ssAlt] = Shift then
      TryOpenFileAt(aIndex);
  end;
begin
  case Key of
    Ord('R'):
    begin
      if [ssCtrl, ssShift] = Shift then
      begin
        btnRebuildClick(Self);
        Key := Ord(#0);
      end;
    end;
    VK_RETURN: OpenCurrentFile;
    Ord('1'): DoOpenFileAt(0);
    Ord('2'): DoOpenFileAt(1);
    Ord('3'): DoOpenFileAt(2);
    Ord('4'): DoOpenFileAt(3);
    Ord('5'): DoOpenFileAt(4);
    Ord('6'): DoOpenFileAt(5);
    Ord('7'): DoOpenFileAt(6);
    Ord('8'): DoOpenFileAt(7);
    Ord('9'): DoOpenFileAt(8);
    Ord('0'): DoOpenFileAt(9);
    VK_DOWN:
    begin
      if not vstFiles.Focused and vstFiles.CanFocus and (vstFiles.RootNodeCount > 0) and
        not (cmbFilter.Focused and ([ssCtrl] = Shift)) then
      begin
        vstFiles.SetFocus;
        vstFiles.Selected[vstFiles.GetFirst] := True;
        Key := Ord(#0);
      end;
    end;
    VK_UP:
    begin
      if vstFiles.Focused and (vstFiles.GetFirst = vstFiles.FocusedNode) and cmbFilter.CanFocus then
      begin
        cmbFilter.SetFocus;
        Key := Ord(#0);
      end;
    end;
  end;
end;

function TOpenFileFrame.OpenCurrentFile: Boolean;
begin
  if vstFiles.FocusedNode <> nil then
    Result := TryOpenFileAt(vstFiles.FocusedNode.Index)
  else
    Result := False;
end;

function TOpenFileFrame.TryOpenFileAt(aIndex: Integer): Boolean;
begin
  Result := False;
  if (Handler = nil) or (aIndex < 0) or (aIndex >= FFilteredList.Count) then
    Exit;

  Handler.OpenFile(FFilteredList[aIndex]);

  if FFrameParams.LastFiles.First <> cmbFilter.Text then
  begin
    FFrameParams.LastFiles.AddFirst(cmbFilter.Text);
    while FFrameParams.LastFiles.Count > 10 do
      FFrameParams.LastFiles.RemoveLast;
    FFrameParams.WriteParams;
  end;
  UpdateFilterCombo;

  cmbFilter.Text := '';
end;

procedure TOpenFileFrame.UpdateFilterCombo;
begin
  cmbFilter.Items.Clear;
  cmbFilter.Items.AddStrings(TArrayUtils.AsArray<string>(FFrameParams.LastFiles.AsEnumerable));
  cmbFilter.ItemIndex := -1;
end;

procedure TOpenFileFrame.SetHandler(aHandler: TBaseOpenFileHandler);
begin
  if Handler <> nil then
  begin
    Handler.Status.RemoveObserver(FStatusObserver);
    Handler.FilteredPaths.RemoveObserver(FPathsObserver);
  end;

  FHandlerHolder := TObjectHolder<TBaseOpenFileHandler>.Create(aHandler, False);

  if Handler <> nil then
  begin
    Handler.Status.RegisterObserver(FStatusObserver);
    Handler.FilteredPaths.RegisterObserver(FPathsObserver);
  end;
  cmbFilter.Text := '';
end;

procedure TOpenFileFrame.SetupControls;
begin
  vstFiles.Clear;
  vstFiles.RootNodeCount := FFilteredList.Count;
end;

procedure TOpenFileFrame.vstFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: TNodeData;
begin
  NodeData := vstFiles.GetNodeData<TNodeData>(Node);
  FreeAndNil(NodeData);
  vstFiles.SetNodeData(Node, nil);
end;

procedure TOpenFileFrame.vstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: TNodeData;
begin
  NodeData := vstFiles.GetNodeData<TNodeData>(Node);
  if NodeData = nil then
    Exit;

  case Column of
    cstColumnNum:  CellText := IntToStr(vstFiles.AbsoluteIndex(Node) + 1);
    cstColumnFile: CellText := NodeData.FileName;
    cstColumnPath: CellText := NodeData.FilePath
  end;
end;

procedure TOpenFileFrame.vstFilesIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText:
    string; var Result: Integer);
var
  NodeData: TNodeData;
begin
  Result := 0;
  NodeData := vstFiles.GetNodeData<TNodeData>(Node);
  if NodeData = nil then
    Exit;

  if TStrUtils.PosI(SearchText, IncludeTrailingPathDelimiter(NodeData.FilePath) + NodeData.FileName) > 0 then
    Result := 1;
end;

procedure TOpenFileFrame.vstFilesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  FullPath: string;
begin
  if (Node.Index < Cardinal(FFilteredList.Count)) then
    FullPath := FFilteredList[Node.Index];

  if FullPath <> '' then
    Node.SetData<TNodeData>(TNodeData.Create(FullPath));
end;

procedure TOpenFileFrame.vstFilesNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  OpenCurrentFile;
end;

{ TNodeData }

constructor TNodeData.Create(const aFullPath: string);
begin
  inherited Create;
  FFullPath := aFullPath;
end;

function TNodeData.GetFileName: string;
begin
  if FFileName = '' then
    FFileName := ExtractFileName(FFullPath);
  Result := FFileName;
end;

function TNodeData.GetFilePath: string;
begin
  if FFilePath = '' then
    FFilePath := ExtractFilePath(FFullPath);
  Result := FFilePath;
end;

{ TOpenFileFrameParams }

constructor TOpenFileFrameParams.Create;
begin
  inherited Create;
  FLastFiles := TDeque<string>.Create;
end;

destructor TOpenFileFrameParams.Destroy;
begin
  FreeAndNil(FLastFiles);
  inherited;
end;

procedure TOpenFileFrameParams.DoReadParams(aIni: TCustomIniFile);
begin
  inherited;
  FLastFiles.AddRangeLast(ReadStrings(aIni, 'LastOpenFileFilterList', 'Filter'));
end;

procedure TOpenFileFrameParams.DoWriteParams(aIni: TCustomIniFile);
begin
  inherited;
  WriteStrings(aIni, 'LastOpenFileFilterList', 'Filter', FLastFiles.AsEnumerable);
end;

end.
