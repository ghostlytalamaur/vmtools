unit opt_frame;
{$I cond_define.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, opt_impl, Types, vtree_mod, opt_editors, Menus, ExtCtrls, StdCtrls, ActnList, ToolWin, ActnMan,
  ActnCtrls, ComCtrls;


type
  TVirtualStringTree = class(TExtVirtualStringTree);

  TOptionsFrame = class(TFrame)
    vstOptions: TVirtualStringTree;
    pmContextMenu: TPopupMenu;
    mnuSetDefault: TMenuItem;
    procedure mnuSetDefaultClick(Sender: TObject);
    procedure vstOptionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstOptionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstOptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vstOptionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      out EditLink: IVTEditLink);
    procedure vstOptionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure vstOptionsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstOptionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstOptionsContextPopup(Sender: TObject; MousePos: TPoint; var
        Handled: Boolean);
  private
    FParams: TParamsTree;
    FEditorManager: TVTreeEditorManager;

    function GetParamString(aParam: TBaseParam): string;

    // TODO: Calculate visible params at once
    function GetParamsVisibleParamsCount(aParamsGroup: TParamsGroup): Integer;
    function GetVisibleParam(aParamsGroup: TParamsGroup; aIndex: Integer): TBaseParam;
    function IsVisibleParam(aParamsGroup: TParamsGroup; aIndex: Integer): Boolean;
    procedure NodeChanged(aNode: PVirtualNode);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParams(aParams: TParamsTree);
    procedure UpdateData;
  end;

implementation

uses
  validators
{$IFDEF DELPHIX_BERLIN_UP}
  , uitypes
{$ENDIF}  , SysUtils;

{$R *.dfm}

const
  cstCheckState: array [Boolean] of TCheckState = (csUncheckedPressed, csCheckedPressed);

type
  POptionsNodeData = ^TOptionsNodeData;
  TOptionsNodeData = class
  strict private
    FParamItem: TBaseParam;
  public

    constructor Create(aParamItem: TBaseParam);
    destructor Destroy; override;
    property ParamItem: TBaseParam read FParamItem;
  end;

{ TOptionsFrame }

constructor TOptionsFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FEditorManager := TVTreeEditorManager.Create;
  FEditorManager.RegisterFactory(TVTreeIntegerEditorFactory.Create);
  FEditorManager.RegisterFactory(TVTreeShortCutEditorFactory.Create);
  FEditorManager.RegisterFactory(TVTreeSingleEditorFactory.Create);
  FEditorManager.RegisterFactory(TVTreeBooleanEditorFactory.Create);
  FEditorManager.RegisterFactory(TVTreeStringEditorFactory.Create);

  vstOptions.TreeOptions.MiscOptions := vstOptions.TreeOptions.MiscOptions + [toEditOnDblClick, toCheckSupport];
  vstOptions.TreeOptions.SelectionOptions := vstOptions.TreeOptions.SelectionOptions + [toFullRowSelect];
  vstOptions.DefaultNodeHeight := 21;
end;

function TOptionsFrame.GetParamsVisibleParamsCount(aParamsGroup: TParamsGroup):
    Integer;
var
  I: Integer;
begin
  Result := 0;
  if aParamsGroup <> nil then
    for I := 0 to aParamsGroup.ParamsCount - 1 do
      if IsVisibleParam(aParamsGroup, I) then
        Inc(Result);
end;

function TOptionsFrame.GetVisibleParam(aParamsGroup: TParamsGroup; aIndex:
    Integer): TBaseParam;
var
  CurIdx, I: Integer;

begin
  CurIdx := 0;
  Result := nil;
  if aParamsGroup <> nil then
    for I := 0 to aParamsGroup.ParamsCount - 1 do
      if IsVisibleParam(aParamsGroup, I) then
      begin
        if CurIdx = aIndex then
          Exit(aParamsGroup.ByIndex[I]);
        Inc(CurIdx);
      end;
  Assert(Result <> nil, 'Invalid index for visible param');
end;

function TOptionsFrame.IsVisibleParam(aParamsGroup: TParamsGroup; aIndex: Integer): Boolean;
begin
  Result := not (pfInvisible in aParamsGroup.ByIndex[aIndex].Flags);
end;

destructor TOptionsFrame.Destroy;
begin
  FreeAndNil(FEditorManager);
  inherited;
end;

procedure TOptionsFrame.NodeChanged(aNode: PVirtualNode);
begin
  while (aNode <> nil) and (aNode.Parent <> vstOptions.RootNode) do
    aNode := aNode.Parent;
  vstOptions.ResetNode(aNode);
end;

function TOptionsFrame.GetParamString(aParam: TBaseParam): string;
begin
  if (aParam is TIntegerParam) then
    Result := IntToStr(TIntegerParam(aParam).Value)
  else if (aParam is TSingleParam) then
    Result := FloatToStr(TSingleParam(aParam).Value)
  else if (aParam is TStringParam) then
    Result := TStringParam(aParam).Value
  else if (aParam is TBooleanParam) then
    Result := BoolToStr(TBooleanParam(aParam).Value, True)
  else if (aParam is TParamsGroup) then
    Result := ''
  else if (aParam is TShortCutParam) then
    Result := ShortCutToText(TShortCutParam(aParam).Value)
  else
    Result := 'Not supported param.';
end;

procedure TOptionsFrame.mnuSetDefaultClick(Sender: TObject);
var
  NodeData: TOptionsNodeData;
begin
  if vstOptions.FocusedNode = nil then
    Exit;

  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(vstOptions.FocusedNode);
  if (NodeData <> nil) and NodeData.ParamItem.IsChanged then
  begin
    NodeData.ParamItem.SetDefault;
    NodeChanged(vstOptions.FocusedNode);
  end;
end;

procedure TOptionsFrame.SetParams(aParams: TParamsTree);
begin
  if aParams <> FParams then
    FParams := aParams;
  vstOptions.Clear;
  if FParams <> nil then
  begin
    vstOptions.RootNodeCount := GetParamsVisibleParamsCount(FParams);
  end;
end;

procedure TOptionsFrame.UpdateData;
begin
  vstOptions.EndEditNode;
end;

procedure TOptionsFrame.vstOptionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: TOptionsNodeData;
  G: TActiveParamsGroup;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  if (NodeData = nil) or not (NodeData.ParamItem is TActiveParamsGroup) then
    Exit;

  G := NodeData.ParamItem as TActiveParamsGroup;
  if cstCheckState[G.IsActive] = Node.CheckState then
    Exit;

  G.IsActive := Node.CheckState = cstCheckState[True];
  NodeChanged(Node);
end;

procedure TOptionsFrame.vstOptionsContextPopup(Sender: TObject; MousePos:
    TPoint; var Handled: Boolean);
var
  Pt: TPoint;
  Node: PVirtualNode;
begin
  Node := vstOptions.GetNodeAt(MousePos);
  if Node <> nil then
  begin
    vstOptions.FocusedNode := Node;
    vstOptions.Selected[Node] := True;
    Pt := vstOptions.ClientToScreen(MousePos);
    Handled := True;
    pmContextMenu.Popup(Pt.X, Pt.Y);
  end;
end;

procedure TOptionsFrame.vstOptionsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
var
  NodeData: TOptionsNodeData;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  if NodeData = nil then
    Exit;

  EditLink := FEditorManager.CreateEditor(NodeData.ParamItem);
end;

procedure TOptionsFrame.vstOptionsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var
  NodeData: TOptionsNodeData;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  Allowed := (Column = 1) and (NodeData <> nil) and (Node.ChildCount = 0) and
      not (NodeData.ParamItem is TParamsGroup) and FEditorManager.HasEditor(NodeData.ParamItem);
end;

procedure TOptionsFrame.vstOptionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: TOptionsNodeData;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  vstOptions.SetNodeData(Node, nil);
  FreeAndNil(NodeData);
end;

procedure TOptionsFrame.vstOptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: TOptionsNodeData;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  if NodeData = nil then
    Exit;

  case Column of
    0:
    begin
      if NodeData.ParamItem.Alias <> '' then
        CellText := NodeData.ParamItem.Alias
      else
        CellText := NodeData.ParamItem.Key
    end;
    1: CellText := GetParamString(NodeData.ParamItem);
  end;
end;

procedure TOptionsFrame.vstOptionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  NodeData: TOptionsNodeData;
  Group: TParamsGroup;
  ParentData: TOptionsNodeData;
begin
  if (FParams = nil) then
    Exit;

  if ParentNode <> nil then
    ParentData := vstOptions.GetNodeData<TOptionsNodeData>(ParentNode)
  else
    ParentData := nil;
  if ParentData <> nil then
  begin
    Assert(ParentData.ParamItem is TParamsGroup);
    Group := ParentData.ParamItem as TParamsGroup
  end
  else
    Group := FParams;

  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  vstOptions.SetNodeData<TOptionsNodeData>(Node, nil);
  FreeAndNil(NodeData);

  NodeData := TOptionsNodeData.Create(GetVisibleParam(Group, Node.Index));
  vstOptions.SetNodeData<TOptionsNodeData>(Node, NodeData);

  if (NodeData.ParamItem is TActiveParamsGroup) then
  begin
    vstOptions.CheckType[Node] := ctCheckBox;
    vstOptions.CheckState[Node] := cstCheckState[(NodeData.ParamItem as TActiveParamsGroup).IsActive];
    if (NodeData.ParamItem as TActiveParamsGroup).IsActive then
    begin
      vstOptions.ChildCount[Node] := GetParamsVisibleParamsCount((NodeData.ParamItem as TParamsGroup));
      vstOptions.Expanded[Node] := vstOptions.ChildCount[Node] > 0;
    end;
  end
  else if (NodeData.ParamItem is TParamsGroup) then
  begin
    vstOptions.ChildCount[Node] := GetParamsVisibleParamsCount((NodeData.ParamItem as TParamsGroup));
    vstOptions.Expanded[Node] := vstOptions.ChildCount[Node] > 0;
  end;
end;

procedure TOptionsFrame.vstOptionsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  NodeData: TOptionsNodeData;
begin
  NodeData := vstOptions.GetNodeData<TOptionsNodeData>(Node);
  if NodeData.ParamItem.IsChanged then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];
  if (NodeData <> nil) and (NodeData.ParamItem is TParamsGroup) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
end;

{ TOptionsNodeData }

constructor TOptionsNodeData.Create(aParamItem: TBaseParam);
begin
  inherited Create;
  FParamItem := aParamItem;
end;

destructor TOptionsNodeData.Destroy;
begin
  inherited;
end;

end.


