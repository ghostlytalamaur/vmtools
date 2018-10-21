unit vm_wiz_activate_editor;

interface

uses
  vm_basewizard, base_params, Classes, opt_impl, wiz_openfile;

type
  TVMActivateEditorWizardParams = class(TBaseParams)
  private const
    cstRegSection = 'ActivateEditorWizard';
  protected
    function CreateTree: TParamsTree; override;
  end;

  TVMActivateEditorWizard = class(TVMBaseWizard)
  private const
    act_name_ActivateEditor = 'ActivateEditor';
  private
    FParams: TVMActivateEditorWizardParams;
    FParamsObserver: IParamsChangedObserver;

    procedure OnActivateEditor(aSender: TObject);
    procedure ParamsChanged;
    procedure RefreshShortCuts;
    procedure RegisterShortCuts;
    procedure UnregisterShortCuts;
    function GetParams: TVMActivateEditorWizardParams;

    property Params: TVMActivateEditorWizardParams read GetParams;
  protected
    procedure RegisterWizard; override;
    procedure UnregisterWizard; override;
  public
    destructor Destroy; override;
    class function GUID: string; override;
    class function Caption: string; override;
  end;

implementation

uses
  Menus, vm_ide_utils, vmtools_cst, SysUtils;

{ TVMActivateEditorWizard }

class function TVMActivateEditorWizard.Caption: string;
begin
  Result := cstActivateEditor_Caption;
end;

destructor TVMActivateEditorWizard.Destroy;
begin
  if FParams <> nil then
    FParams.WriteParams;
  FreeAndNil(FParams);
  inherited;
end;

function TVMActivateEditorWizard.GetParams: TVMActivateEditorWizardParams;
begin
  if FParams = nil then
  begin
    FParams := TVMActivateEditorWizardParams.Create;
    FParams.ReadParams;
    FParamsObserver := TParamsObserver.Create(
      procedure
      begin
        ParamsChanged;
      end);

    FParams.RegisterObserver(IParamsChangedObserver, FParamsObserver);
  end;
  Result := FParams;
end;

class function TVMActivateEditorWizard.GUID: string;
begin
  Result := cstActivateEditor_GUID;
end;

procedure TVMActivateEditorWizard.OnActivateEditor(aSender: TObject);
begin
  TGXOtaUtils.GxOtaFocusCurrentIDEEditControl;
end;

procedure TVMActivateEditorWizard.ParamsChanged;
begin
  if IsActive then
    RefreshShortCuts;
  Params.WriteParams;
end;

procedure TVMActivateEditorWizard.RefreshShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    InfoMsg(cstActivateEditor_Msg_RefreshShortcuts);
    UnregisterShortCuts;
    RegisterShortCuts;
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMActivateEditorWizard.RegisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    ActionManager.RegisterInternalAction(act_name_ActivateEditor, OnActivateEditor,
        ShortCut(TextToShortCut('E'), [ssAlt]));
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMActivateEditorWizard.RegisterWizard;
begin
  inherited;
  RefreshShortCuts;
end;

procedure TVMActivateEditorWizard.UnregisterShortCuts;
begin
  ActionManager.BeginUpdate;
  try
    Services.ActionManager.UnRegisterInternalAction(act_name_ActivateEditor);
  finally
    ActionManager.EndUpdate;
  end;
end;

procedure TVMActivateEditorWizard.UnregisterWizard;
begin
  UnregisterShortCuts;
  inherited;
end;

{ TVMActivateEditorWizardParams }

function TVMActivateEditorWizardParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(cstRegSection, cstActivateEditor_ParamsAlias);
end;

end.
