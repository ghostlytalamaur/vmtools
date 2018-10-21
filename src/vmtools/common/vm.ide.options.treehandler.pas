unit vm.ide.options.treehandler;

interface

uses
  vm.ide.options.handler, opt_impl, base_params, forms;

type
  TVMOptionsTreeHandler = class(TVMOptionsHandler)
  private
    FParamsTree: TParamsTree;
    FParamsTreeCopy: TParamsTree;
  protected
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
  public
    constructor Create(aCaption: string; aParamsTree: TParamsTree);
    destructor Destroy; override;
  end;

implementation

uses
  sysutils, opt_frame;

{ TVMOptionsTreeHandler }

constructor TVMOptionsTreeHandler.Create(aCaption: string; aParamsTree: TParamsTree);
begin
  inherited Create(aCaption, TOptionsFrame);
  SetNotifiableObjectProperty(@FParamsTree, aParamsTree);
end;

destructor TVMOptionsTreeHandler.Destroy;
begin
  FreeAndNil(FParamsTreeCopy);
  inherited;
end;

procedure TVMOptionsTreeHandler.DialogClosed(Accepted: Boolean);
begin
  inherited;
  if Accepted and (FParamsTreeCopy <> nil) then
  begin
    FParamsTree.CopyData(FParamsTreeCopy);
    FParamsTree.DataChanged;
    FreeAndNil(FParamsTreeCopy);
  end;
end;

procedure TVMOptionsTreeHandler.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  if not (aFrame is TOptionsFrame) or (FParamsTree = nil) then
    Exit;

  FreeAndNil(FParamsTreeCopy);
  FParamsTreeCopy := FParamsTree.Duplicate as TParamsTree;
  TOptionsFrame(aFrame).SetParams(FParamsTreeCopy);
end;

end.
