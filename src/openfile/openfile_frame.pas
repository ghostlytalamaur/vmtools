unit openfile_frame;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, extctrls,
  classes, inifiles, VirtualTrees, vtree_mod, Buttons,
  generics.collections, observer, vmsys, openfile_handler,
  base_params, collections.deque, Vcl.Grids;

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

  TOpenFileFrame = class(TFrame)
    Panel1: TPanel;
    btnRebuild: TSpeedButton;
    StatusBar1: TStatusBar;
    cmbFilter: TComboBox;
    lvFiles: TListView;
    procedure vstFilesNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure btnRebuildClick(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
    procedure lvFilesData(Sender: TObject; Item: TListItem);
    procedure OnKeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    FFrameParams: TOpenFileFrameParams;
    FHandlerHolder: IObjectHolder<TBaseOpenFileHandler>;
    FFilteredList: TList<TNodeData>;
    FStatusObserver: IDataObserver<TOpenFileHandlerStatus>;
    FPathsObserver: IDataObserver<TList<string>>;

    procedure UpdateFilterCombo;
    procedure SetupControls;
    function OpenCurrentFile: Boolean;
    function TryOpenFileAt(aIndex: Integer): Boolean;
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
  collections.array_utils, math, collections.common;

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
    var
      Nodes: IEnumerable<TNodeData>;
    begin
      if not FrameHolder.IsAlive then
        Exit;

      FrameHolder.Obj.FFilteredList.Clear;
      Nodes := Pipeline<string>.From(aPaths)
        .Map<TNodeData>(function (Path: string): TNodeData
          begin
            Result := TNodeData.Create(Path);
          end)
        .Enum;

      FrameHolder.Obj.FFilteredList.AddRange(Nodes);
      FrameHolder.Obj.SetupControls;
    end);

  FFilteredList := TObjectList<TNodeData>.Create;
  UpdateFilterCombo;
  SetupControls;
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

procedure TOpenFileFrame.lvFilesData(Sender: TObject; Item: TListItem);
var
  Node: TNodeData;
begin
  if (Item.Index >= 0) and (Item.Index < FFilteredList.Count) then
    Node := FFilteredList[Item.Index]
  else
    Node := nil;

  Item.Caption := IntToStr(Item.Index + 1);
  if Node <> nil then
  begin
    Item.SubItems.Add(Node.FileName);
    Item.SubItems.Add(Node.FilePath);
  end;
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
      if not lvFiles.Focused and lvFiles.CanFocus and (lvFiles.Items.Count > 0) and
        not (cmbFilter.Focused and ([ssCtrl] = Shift)) then
      begin
        lvFiles.SetFocus;

        if lvFiles.ItemIndex > 0 then
          lvFiles.ItemIndex := math.Max(lvFiles.Items.Count - 1, lvFiles.ItemIndex + 1);
        Key := Ord(#0);
      end;
    end;
    VK_UP:
    begin
      if lvFiles.Focused and (lvFiles.ItemIndex = 0) and cmbFilter.CanFocus then
      begin
        cmbFilter.SetFocus;
        Key := Ord(#0);
      end;
    end;
  end;
end;

function TOpenFileFrame.OpenCurrentFile: Boolean;
begin
  if lvFiles.ItemIndex >= 0 then
    Result := TryOpenFileAt(lvFiles.ItemIndex)
  else
    Result := False;
end;

function TOpenFileFrame.TryOpenFileAt(aIndex: Integer): Boolean;
begin
  Result := False;
  if (Handler = nil) or (aIndex < 0) or (aIndex >= FFilteredList.Count) then
    Exit;

  Handler.OpenFile(FFilteredList[aIndex].FullPath);

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
  lvFiles.Items.BeginUpdate;
  try
    lvFiles.Items.Clear;
    lvFiles.Items.Count := FFilteredList.Count;

    if lvFiles.Items.Count > 0 then
      lvFiles.ItemIndex := 0;
  finally
    lvFiles.Items.EndUpdate;
  end;
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
