unit vm_wiz_search_dockform;
interface

uses
  vmsys, Forms, search_frame, inifiles, vm_custom_dock_frm, search_handler, search_table;

type
  IVMSearchResultDockForm = interface(IVMCustomDockForm)
  ['{0513148A-E5DA-443B-A614-F660BF1442C5}']
    procedure ShowNext;
    procedure ShowPrev;
  end;

  TVMSearchResultDockForm = class(TVMCustomDockForm, IVMSearchResultDockForm)
  private
    FHandler: IObjectHolder<TSearchHandler>;
    FParams: TSearchResultFrameSettings;
    function GetParams: TSearchResultFrameSettings;
    function DoGetMainFrame: TSearchResultFrame;

    property Params: TSearchResultFrameSettings read GetParams;
  public
    constructor Create(aHandler: TSearchHandler);
    destructor Destroy; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;

    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean); override;
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string); override;

    { IVMSearchResultDockForm }
    procedure ShowNext;
    procedure ShowPrev;

    property MainFrame: TSearchResultFrame read DoGetMainFrame;
  end;

implementation

uses
  sysutils;

{ TVMSearchResultDockForm }

constructor TVMSearchResultDockForm.Create(aHandler: TSearchHandler);
begin
  inherited Create(TSearchResultFrame, 'Search results', 'TVMSearchResultDockForm');
  FHandler := TObjectHolder<TSearchHandler>.Create(aHandler, False);
end;

destructor TVMSearchResultDockForm.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TVMSearchResultDockForm.DoGetMainFrame: TSearchResultFrame;
var
  Frame: TCustomFrame;
begin
  Frame := GetMainFrame;
  if Frame is TSearchResultFrame then
    Result := Frame as TSearchResultFrame
  else
    Result := nil;
end;

procedure TVMSearchResultDockForm.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  if MainFrame = nil then
    Exit;

  MainFrame.vstResults.LoadSetting(Params);
  if FHandler.IsAlive then
    MainFrame.SetHandler(FHandler.Obj);
end;

function TVMSearchResultDockForm.GetParams: TSearchResultFrameSettings;
begin
  if FParams = nil then
    FParams := TSearchResultFrameSettings.Create;
  Result := FParams;
end;

procedure TVMSearchResultDockForm.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin
  inherited;
  Params.ReadSettings(Desktop, Section);
  if MainFrame <> nil then
    MainFrame.vstResults.LoadSetting(Params);
end;

procedure TVMSearchResultDockForm.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin
  inherited;
  if MainFrame = nil then
    Exit;

  MainFrame.vstResults.StoreSetting(Params);
  Params.WriteSettings(Desktop, Section);
end;

procedure TVMSearchResultDockForm.ShowNext;
begin
  if MainFrame = nil then
    Exit;

  MainFrame.ShowResultItem(True);
end;

procedure TVMSearchResultDockForm.ShowPrev;
begin
  if MainFrame = nil then
    Exit;

  MainFrame.ShowResultItem(False);
end;

end.
