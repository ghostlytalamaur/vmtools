unit wiz_openfile_dockform;

interface

uses
  SysUtils,
  Forms,
  vm_custom_dock_frm, inifiles, vtree_mod, wiz_openfile_handler,
  vmsys, openfile_frame;

type
  TVMOpenFileDockForm = class(TVMCustomDockForm)
  private
    FParams: TExtVirtualTreeParams;
    FHandlerHolder: IObjectHolder<TOpenFileHandler>;

    function GetParams: TExtVirtualTreeParams;
    function GetHandler: TOpenFileHandler;
    function DoGetMainFrame: TOpenFileFrame;

    property Params: TExtVirtualTreeParams read GetParams;
    property Handler: TOpenFileHandler read GetHandler;
  public
    constructor Create(aHandler: TOpenFileHandler);
    destructor Destroy; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;

    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean); override;
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string); override;

    property MainFrame: TOpenFileFrame read DoGetMainFrame;
  end;

implementation

uses
  toolsapi;


{ TVMSearchResultDockForm }

constructor TVMOpenFileDockForm.Create(aHandler: TOpenFileHandler);
begin
  inherited Create (TOpenFileFrame, 'Open file', 'TVMOpenFileDockForm');
  FHandlerHolder := TObjectHolder<TOpenFileHandler>.Create(aHandler, False);
end;

destructor TVMOpenFileDockForm.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TVMOpenFileDockForm.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  if MainFrame <> nil then
  begin
    (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(aFrame);
//    MainFrame.vstFiles.LoadSetting(Params);
    MainFrame.SetHandler(Handler);
  end;
end;

function TVMOpenFileDockForm.GetHandler: TOpenFileHandler;
begin
  if FHandlerHolder <> nil then
    Result := FHandlerHolder.Obj
  else
    Result := nil;
end;

function TVMOpenFileDockForm.DoGetMainFrame: TOpenFileFrame;
var
  F: TCustomFrame;
begin
  F := GetMainFrame;
  if F is TOpenFileFrame then
    Result := TOpenFileFrame(F)
  else
    Result := nil;
end;

procedure TVMOpenFileDockForm.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin
  inherited;
  Params.ReadSettings(Desktop, Section);
//  if MainFrame <> nil then
//    MainFrame.vstFiles.LoadSetting(Params);
end;

procedure TVMOpenFileDockForm.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin
  inherited;
  if MainFrame <> nil then
  begin
//    MainFrame.vstFiles.StoreSetting(Params);
    Params.WriteSettings(Desktop, Section);
  end;
end;

function TVMOpenFileDockForm.GetParams: TExtVirtualTreeParams;
begin
  if FParams = nil then
    FParams := TExtVirtualTreeParams.Create;
  Result := FParams;
end;

end.
