unit options_dlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, opt_frame, opt_impl, baseform, StdCtrls, Buttons, ExtCtrls,
  ComCtrls,
  openfile_handler, openfile_frame, search_frame, search_handler;


type
  TOptForm = class(TBaseForm)
    pnlOptions: TPanel;
    pnlTop: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlFiles: TPanel;
    OptionsFrame1: TOptionsFrame;
    OpenFileFrame1: TOpenFileFrame;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    btnApplyOptions: TButton;
    btnDefaultOptions: TButton;
    tsSearch: TTabSheet;
    pnlSearch: TPanel;
    SearchResultFrame1: TSearchResultFrame;
    pnlSearchRight: TPanel;
    btnSearch: TButton;
    procedure btnApplyOptionsClick(Sender: TObject);
    procedure btnDefaultOptionsClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnTestActionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOptions: TParamsTree;
    FCopyParams: TParamsTree;
    FOpenFileHandler: TBaseOpenFileHandler;
    FSearchHandler: TSearchHandler;
  protected
    function CanCloseByDialogKey(aKeyCode: Word): Boolean; override;
    procedure SetupControls; override;
    procedure UpdateData; override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  OptForm: TOptForm;

implementation

uses
  str_utils, ioutils, Types, Menus, Rtti, TypInfo,
  generics.collections, Vcl.ActnList, vmsys;

{$R *.dfm}

type
  TTestOpenFileHandler = class(TBaseOpenFileHandler)
  protected
    function GetDirPaths: IEnumerable<string>; override;
  end;

  TActionManager = class
  private
    FActions: TList<TAction>;
    FOnExecute: TProc<TObject>;
    procedure DoExecute(aSender: TObject);
  public
    procedure RegisterAction(aName: string; aOnExecute: TProc<TObject>);
    procedure ExecuteAction(aName: string);
  end;

  TSomeHandler = class(TExtObject)
  private
    FDestoryed: Boolean;
  public
    constructor Create(aActionManager: TActionManager);
    destructor Destroy; override;
  end;

  TTestSearchHandler = class(TSearchHandler)
  public
    function GetProjectPaths: IEnumerable<string>; override;
    function GetIndexSearchPaths: TArray<string>; override;
    function GetQueryText: string; override;
  end;

{ TOptForm }

function TOptForm.CanCloseByDialogKey(aKeyCode: Word): Boolean;
begin
  Result := not OptionsFrame1.vstOptions.IsEditing;
end;

constructor TOptForm.Create(aOwner: TComponent);
var
  G, G2: TParamsGroup;
begin
  inherited Create(aOwner);

  FSearchHandler := TTestSearchHandler.Create;
  SearchResultFrame1.SetHandler(FSearchHandler);

  FOpenFileHandler := TTestOpenFileHandler.Create;
  OpenFileFrame1.SetHandler(FOpenFileHandler);

  FOptions := TParamsTree.Create('SampleParams');
  FOptions.RegisterParam(TShortCutParam.Create('ShortCut 1', ShortCut(Ord('S'), [ssAlt, ssShift])));
  FOptions.RegisterParam(TShortCutParam.Create('ShortCut 2', 0));

  FOptions.RegisterParam(TIntegerParam.Create('Integer param', 100));
  FOptions.RegisterParam(TIntegerParam.Create('Integer param 2'));
  FOptions.RegisterParam(TSingleParam.Create('Single param', 0.5));
  FOptions.RegisterParam(TBooleanParam.Create('Boolean param', True));
  FOptions.RegisterParam(TStringParam.Create('String param', 'Some string'));

  G := TParamsGroup.Create('Params group with invisible param');
  G.RegisterParam(TIntegerParam.Create('Integer param 2', 200));
  G.RegisterParam(TSingleParam.Create('Single param 2', 15.5));
  G.RegisterParam(TBooleanParam.Create('Boolean param 2', False));
  G.RegisterParam(TStringParam.Create('String param 2', 'Default string'));
  G.RegisterParam(TStringParam.Create('Not editable param', '', [pfInvisible], 'not visible'));


  G2 := TActiveParamsGroup.Create('Checkable Params sub group', True);
  G2.RegisterParam(TIntegerParam.Create('Integer param 5', 200));
  G2.RegisterParam(TSingleParam.Create('Single param 5', 10.10));
  G2.RegisterParam(TBooleanParam.Create('Boolean param 5', True));
  G.RegisterParam(G2);

  FOptions.RegisterParam(G);

  G := TActiveParamsGroup.Create('Checkable Params group', False);
  G.RegisterParam(TIntegerParam.Create('Integer param 3', 200));
  G.RegisterParam(TSingleParam.Create('Single param 3', 0));
  G.RegisterParam(TBooleanParam.Create('Boolean param 3', False));
  G.RegisterParam(TStringParam.Create('String param 3', ''));
  FOptions.RegisterParam(G);

  FCopyParams := FOptions.Duplicate as TParamsTree;
  TOptionsFrame(OptionsFrame1).SetParams(FOptions);
end;

destructor TOptForm.Destroy;
begin
  FreeAndNil(FCopyParams);
  FreeAndNil(FOptions);
  FreeAndNil(FOpenFileHandler);
  FreeAndNil(FSearchHandler);
  inherited;
end;

procedure TOptForm.btnApplyOptionsClick(Sender: TObject);
begin
  UpdateData;
  SetupControls;
end;

procedure TOptForm.btnDefaultOptionsClick(Sender: TObject);
begin
  FCopyParams.SetDefault;
  OptionsFrame1.SetParams(FCopyParams);
end;

procedure TOptForm.btnSearchClick(Sender: TObject);
begin
  FSearchHandler.ExecuteSearch;
end;

procedure TOptForm.btnTestActionClick(Sender: TObject);
var
  Handler: TSomeHandler;
  ActMan: TActionManager;
begin
  Handler := nil;
  ActMan := TActionManager.Create;
  try
    Handler := TSomeHandler.Create(ActMan);
    FreeAndNil(Handler);
    ActMan.ExecuteAction('SomeAction');
  finally
    FreeAndNil(Handler);
    FreeAndNil(ActMan);
  end;
end;

procedure TOptForm.FormShow(Sender: TObject);
begin
  if OpenFileFrame1.cmbFilter.CanFocus then
    OpenFileFrame1.cmbFilter.SetFocus;
end;

procedure TOptForm.SetupControls;
begin
  if InSetupControls then
    Exit;

  BeginSetupControls;
  try
    inherited;

    FreeAndNil(FCopyParams);
    FCopyParams := FOptions.Duplicate as TParamsTree;

    OptionsFrame1.SetParams(FCopyParams);
  finally
    EndSetupControls;
  end;
end;

procedure TOptForm.UpdateData;
begin
  inherited;
  if (FCopyParams <> nil) and (FCopyParams.IsChanged or FOptions.IsChanged) then
  begin
    OptionsFrame1.SetParams(nil);
    FOptions.CopyData(FCopyParams);
    FOptions.DataChanged;
    FreeAndNil(FCopyParams);
  end;
end;

{ TTestOpenFileHandler }

function TTestOpenFileHandler.GetDirPaths: IEnumerable<string>;
var
  Dirs: TStringDynArray;
  DirsStr, Dir: string;
begin
  Dirs := TDirectory.GetDirectories('D:\dev\delphi', '*.*', TSearchOption.soAllDirectories);
  DirsStr := '';
  for Dir in Dirs do
    DirsStr := DirsStr + ';' + Dir;
  Result := TStrUtils.Words(DirsStr, [';']);
end;

{ TActionManager }

procedure TActionManager.DoExecute(aSender: TObject);
begin
  FOnExecute(aSender);
end;

procedure TActionManager.ExecuteAction(aName: string);
begin
  if FActions = nil then
    Exit;

  FActions.First.Execute;
end;

procedure TActionManager.RegisterAction(aName: string; aOnExecute: TProc<TObject>);
var
  Act: TAction;
begin
  if FActions = nil then
    FActions := TObjectList<TAction>.Create;
  Act := TAction.Create(nil);
  Act.Name := aName;
  Act.OnExecute := DoExecute;
  FOnExecute := aOnExecute;
  FActions.Add(Act);
end;

{ TSomeHandler }

constructor TSomeHandler.Create(aActionManager: TActionManager);
var
  Inst: IObjectHolder<TSomeHandler>;
begin
  inherited Create;
  Inst := TObjectHolder<TSomeHandler>.Create(Self, False);
  aActionManager.RegisterAction('SomeAction', procedure (aSender: TObject)
  begin
    Showmessage(Format('Instance: %d', [NativeInt(Inst.Obj)]));

  end);
end;

destructor TSomeHandler.Destroy;
begin
  FDestoryed := True;
  inherited;
end;

{ TTestSearchHandler }

function TTestSearchHandler.GetIndexSearchPaths: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'D:\dev\delphi\vmtools';
end;

function TTestSearchHandler.GetProjectPaths: IEnumerable<string>;
begin
  Result := TStrUtils.Words('', []);
end;

function TTestSearchHandler.GetQueryText: string;
begin
  Result := 'string';
end;

end.
