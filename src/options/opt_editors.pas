unit opt_editors;
{$I cond_define.inc}

interface

uses
  VirtualTrees, validators, classes, Windows, stdctrls, Controls, Messages,
  SysUtils, opt_impl, generics.collections, comctrls;

type
  TValidatorEdit = class(TEdit)
  strict private
    FValidator: TEditValidator;
  strict private
    function GetValidator: TEditValidator;
    procedure SetValidator(const Value: TEditValidator);
  strict protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(aOwner: TComponent; aValidator: TEditValidator); reintroduce;
    destructor Destroy; override;
    property Validator: TEditValidator read GetValidator write SetValidator;
  end;

  TVTreeControlEditLink<EditType: TWinControl; ParamType: TBaseParam> = class(TInterfacedObject, IVTEditLink)
  private type
    TWinControlFriend = class(TWinControl);
  strict private
    FEditor: EditType;
    FOldEditorWndProc: TWndMethod;
    FReleasing: Boolean;

    FStopping: Boolean;
    FParam: ParamType;

    FTree: TBaseVirtualTree;
    FNode: PVirtualNode;
    FColumn: TColumnIndex;

    function GetEditor: EditType;

    procedure OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorWndProc(var Message: TMessage);
    function DoDispatchToEditor(var Message: TMessage): Boolean;
  strict protected
    function CreateEditor: EditType; virtual; abstract;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; abstract;
    procedure DoEndEdit; virtual; abstract;

    property Editor: EditType read GetEditor;
    property Param: ParamType read FParam;
  public
    constructor Create(aParam: ParamType);
    destructor Destroy; override;

    { IVTEditLink }
    function BeginEdit: Boolean; stdcall;                  // Called when editing actually starts.
    function CancelEdit: Boolean; stdcall;                 // Called when editing has been cancelled by the tree.
    function EndEdit: Boolean; stdcall;                    // Called when editing has been finished by the tree. Returns True if successful, False if edit mode is still active.
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
                                                           // Called after creation to allow a setup.
    function GetBounds: TRect; stdcall;                    // Called to get the current size of the edit window
                                                           // (only important if the edit resizes itself).
    procedure ProcessMessage(var Message: TMessage); stdcall;
                                                           // Used to forward messages to the edit window(s)-
    procedure SetBounds(R: TRect); stdcall;                // Called to place the editor.
  end;

  TVTreeEditorFactory = class
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; virtual; abstract;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; virtual; abstract;
  end;

  TVTreeIntegerEditLink = class(TVTreeControlEditLink<TValidatorEdit, TIntegerParam>)
  strict protected
    function CreateEditor: TValidatorEdit; override;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoEndEdit; override;
  end;

  TVTreeShortCutEditLink = class(TVTreeControlEditLink<THotkey, TShortCutParam>)
  strict protected
    function CreateEditor: THotkey; override;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoEndEdit; override;
  end;

  TVTreeShortCutEditorFactory = class(TVTreeEditorFactory)
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; override;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; override;
  end;

  TVTreeIntegerEditorFactory = class(TVTreeEditorFactory)
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; override;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; override;
  end;

  TVTreeSingleEditLink = class(TVTreeControlEditLink<TValidatorEdit, TSingleParam>)
  strict protected
    function CreateEditor: TValidatorEdit; override;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoEndEdit; override;
  end;

  TVTreeSingleEditorFactory = class(TVTreeEditorFactory)
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; override;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; override;
  end;

  TVTreeBooleanEditLink = class(TVTreeControlEditLink<TComboBox, TBooleanParam>)
  strict protected
    function CreateEditor: TComboBox; override;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoEndEdit; override;
  end;

  TVTreeBooleanEditorFactory = class(TVTreeEditorFactory)
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; override;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; override;
  end;

  TVTreeStringEditLink = class(TVTreeControlEditLink<TEdit, TStringParam>)
  strict protected
    function CreateEditor: TEdit; override;
    function DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoEndEdit; override;
  end;

  TVTreeStringEditorFactory = class(TVTreeEditorFactory)
  public
    function IsSuitableParam(aParam: TBaseParam): Boolean; override;
    function CreateEditor(aParam: TBaseParam): IVTEditLink; override;
  end;

  TVTreeEditorManager = class(TObject)
  strict private
    FFactories: TList<TVTreeEditorFactory>;

    function GetFactory(aParam: TBaseParam): TVTreeEditorFactory;
  public
    destructor Destroy; override;

    function RegisterFactory(aFactory: TVTreeEditorFactory): Boolean;
    function HasEditor(aParam: TBaseParam): Boolean;
    function CreateEditor(aParam: TBaseParam): IVTEditLink;
  end;


implementation

uses
  Forms;


{ TValidatorEdit }

constructor TValidatorEdit.Create(aOwner: TComponent; aValidator: TEditValidator);
begin
  inherited Create(aOwner);
  FValidator := aValidator;
end;

destructor TValidatorEdit.Destroy;
begin
  FreeAndNil(FValidator);
  inherited;
end;

function TValidatorEdit.GetValidator: TEditValidator;
begin
  if FValidator = nil then
    FValidator := TEditValidator.Create;
  Result := FValidator;
end;

procedure TValidatorEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TValidatorEdit.KeyPress(var Key: Char);
var
  CurText: string;
  SelL, CurPos: Integer;

begin
  inherited;

  if (Key = #0) or
     (Word(Key) = VK_ESCAPE) or
     (Word(Key) = VK_TAB) or
     (Word(Key) = VK_BACK) or
     (Word(Key) = VK_CLEAR) or
     (Word(Key) = VK_RETURN) or
     (GetKeyState(VK_CONTROL) < 0) then
     Exit;

  CurText := Text;
  CurPos := SelStart;
  SelL := SelLength;
  if SelL > 0 then
    Delete(CurText, CurPos + 1, SelL);
  Insert(string(Key), CurText, CurPos + 1);

  if not Validator.ValidInput(CurText) then
    Key := #0;
end;

procedure TValidatorEdit.SetValidator(const Value: TEditValidator);
begin
  if FValidator = Value then
    Exit;

  if FValidator <> nil then
    FreeAndNil(FValidator);

  FValidator := Value;
end;


{ TVTreeControlEditLink }

function TVTreeControlEditLink<EditType, ParamType>.BeginEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  begin
    Editor.Show;
    if Editor.CanFocus then
      Editor.SetFocus;
  end;
end;

function TVTreeControlEditLink<EditType, ParamType>.CancelEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    if FEditor <> nil then
      FEditor.Hide;
  end;
end;

constructor TVTreeControlEditLink<EditType, ParamType>.Create(aParam: ParamType);
begin
  inherited Create;
  FParam := aParam;
end;

destructor TVTreeControlEditLink<EditType, ParamType>.Destroy;
begin
  // Don't change to FreeAndNil() because Editor must process messages on own destroy
  FEditor.Free;
  FEditor := nil;
  FOldEditorWndProc := nil;

  inherited;
end;

function TVTreeControlEditLink<EditType, ParamType>.DoDispatchToEditor(var Message: TMessage): Boolean;
begin
  Result := (FEditor <> nil);
  if Result then
  begin
    if Assigned(FOldEditorWndProc) then
      FOldEditorWndProc(Message)
    else
      TWinControl(FEditor).Dispatch(Message);
  end
end;


procedure TVTreeControlEditLink<EditType, ParamType>.EditorWndProc(var Message: TMessage);
var
  Res: Integer;
begin
  Res := 1;
  case Message.Msg of
    CM_RELEASE:
    begin
      FReleasing := True;
      _Release;
    end;
    WM_DESTROY:
    begin
      if FReleasing then
        Res := 0 // Pass message to Editor
      else if DoDispatchToEditor(Message) then
      begin
        FEditor.WindowProc := FOldEditorWndProc;
        FEditor := nil; // Editor already destroyed, clear reference and call release
        _Release;
      end;
    end;
    else
      Res := 0;
  end;

  if (Res = 0) and not DoDispatchToEditor(Message) then
    Message.Result := Res;
end;

function TVTreeControlEditLink<EditType, ParamType>.EndEdit: Boolean;
begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    DoEndEdit;

    if FEditor <> nil then
      FEditor.Hide;

    if (FEditor <> nil) and FEditor.HandleAllocated then
      PostMessage(FEditor.Handle, CM_RELEASE, 0, 0);
  except
    FStopping := False;
    raise;
  end;
end;

function TVTreeControlEditLink<EditType, ParamType>.GetBounds: TRect;
begin
  Result := Editor.BoundsRect;
end;

function TVTreeControlEditLink<EditType, ParamType>.GetEditor: EditType;
begin
  if FEditor = nil then
  begin
    FEditor := CreateEditor;
    TWinControlFriend(FEditor).OnKeyDown := OnEditorKeyDown;
    FOldEditorWndProc := FEditor.WindowProc;
    FEditor.WindowProc := EditorWndProc;

    // Keep Alive when CancelEditNode() or EndEditNode() called.
    // _Release called on CM_RELEASE in EditorWndProc();
    _AddRef;
  end;
  Result := FEditor;
end;

procedure TVTreeControlEditLink<EditType, ParamType>.OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      if Shift = [] then
        FTree.CancelEditNode;
    VK_RETURN:
      if FTree.IsEditing then
        FTree.EndEditNode;
  end;
end;

function TVTreeControlEditLink<EditType, ParamType>.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  WasBounds: TRect;
begin
  Result := Tree is TBaseVirtualTree;
  if Result then
  begin
    if Editor = nil then
      Exit(False);

    FTree := Tree;
    FNode := Node;
    FColumn := Column;
    Editor.Parent := Tree;
    Editor.HandleNeeded;

    Result := DoPrepareEdit(Tree, Node, Column);
  end;
end;

procedure TVTreeControlEditLink<EditType, ParamType>.ProcessMessage(var Message: TMessage);
begin
  EditorWndProc(Message);
end;

procedure TVTreeControlEditLink<EditType, ParamType>.SetBounds(R: TRect);
begin
  Editor.BoundsRect := R;
end;

{ TVTreeEditorManager }

function TVTreeEditorManager.CreateEditor(aParam: TBaseParam): IVTEditLink;
var
  F: TVTreeEditorFactory;
begin
  F := GetFactory(aParam);
  if F <> nil then
    Result := F.CreateEditor(aParam)
  else
    Result := nil;
end;

destructor TVTreeEditorManager.Destroy;
begin
  FreeAndNil(FFactories);
  inherited;
end;

function TVTreeEditorManager.GetFactory(aParam: TBaseParam):
    TVTreeEditorFactory;
var
  F: TVTreeEditorFactory;
begin
  Result := nil;
  if FFactories = nil then
    Exit;

  for F in FFactories do
    if F.IsSuitableParam(aParam) then
      Exit(F);
end;

function TVTreeEditorManager.HasEditor(aParam: TBaseParam): Boolean;
begin
  Result := GetFactory(aParam) <> nil;
end;

function TVTreeEditorManager.RegisterFactory(aFactory: TVTreeEditorFactory): Boolean;
begin
  Result := False;
  if aFactory = nil then
    Exit;

  if FFactories = nil then
    FFactories := TObjectList<TVTreeEditorFactory>.Create;

  if not FFactories.Contains(aFactory) then
  begin
    FFactories.Add(aFactory);
    Result := True;
  end;
end;

{ TVTreeIntegerEditLink }

function TVTreeIntegerEditLink.CreateEditor: TValidatorEdit;
begin
  Result := TValidatorEdit.Create(nil, TIntegerValidator.Create);
  Result.Visible := False;
end;

procedure TVTreeIntegerEditLink.DoEndEdit;
begin
  Param.Value := StrToIntDef(Editor.Text, Param.Value);
end;

function TVTreeIntegerEditLink.DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Editor.Text := IntToStr(Param.Value);
  Result := True;
end;

{ TVTreeSingleEditLink }

function TVTreeSingleEditLink.CreateEditor: TValidatorEdit;
begin
  Result := TValidatorEdit.Create(nil, TSingleValidator.Create);
  Result.Visible := False;
end;

procedure TVTreeSingleEditLink.DoEndEdit;
begin
  inherited;
  Param.Value := StrToFloatDef(Editor.Text, Param.Value);
end;

function TVTreeSingleEditLink.DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Editor.Text := FloatToStr(Param.Value);
  Result := True;
end;

{ TVTreeBooleanEditLink }

function TVTreeBooleanEditLink.CreateEditor: TComboBox;
begin
  Result := TComboBox.Create(nil);
  Result.Visible := False;
  Result.Style := csDropDownList;
  Result.AutoDropDown := False;
end;

procedure TVTreeBooleanEditLink.DoEndEdit;
begin
  inherited;
  Param.Value := Editor.ItemIndex = 1;
end;

function TVTreeBooleanEditLink.DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Editor.Items.Add(BoolToStr(False, True));
  Editor.Items.Add(BoolToStr(True, True));
  Editor.ItemIndex := Ord(Param.Value);
  Result := True;
end;

{ TVTreeStringEditLink }

function TVTreeStringEditLink.CreateEditor: TEdit;
begin
  Result := TEdit.Create(nil);
  Result.Visible := False;
end;

procedure TVTreeStringEditLink.DoEndEdit;
begin
  inherited;
  Param.Value := Editor.Text;
end;

function TVTreeStringEditLink.DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Editor.Text := Param.Value;
  Result := True;
end;

{ TVTreeIntegerEditorFactory }

function TVTreeIntegerEditorFactory.CreateEditor(aParam: TBaseParam): IVTEditLink;
begin
  Result := TVTreeIntegerEditLink.Create(aParam as TIntegerParam);
end;

function TVTreeIntegerEditorFactory.IsSuitableParam(aParam: TBaseParam): Boolean;
begin
  Result := aParam is TIntegerParam;
end;

{ TVTreeSingleEditorFactory }

function TVTreeSingleEditorFactory.CreateEditor(aParam: TBaseParam): IVTEditLink;
begin
  Result := TVTreeSingleEditLink.Create(aParam as TSingleParam);
end;

function TVTreeSingleEditorFactory.IsSuitableParam(aParam: TBaseParam): Boolean;
begin
  Result := aParam is TSingleParam;
end;

{ TVTreeBooleanEditorFactory }

function TVTreeBooleanEditorFactory.CreateEditor(aParam: TBaseParam): IVTEditLink;
begin
  Result := TVTreeBooleanEditLink.Create(aParam as TBooleanParam);
end;

function TVTreeBooleanEditorFactory.IsSuitableParam(aParam: TBaseParam): Boolean;
begin
  Result := aParam is TBooleanParam;
end;

{ TVTreeStringEditorFactory }

function TVTreeStringEditorFactory.CreateEditor(aParam: TBaseParam): IVTEditLink;
begin
  Result := TVTreeStringEditLink.Create(aParam as TStringParam);
end;

function TVTreeStringEditorFactory.IsSuitableParam(aParam: TBaseParam): Boolean;
begin
  Result := aParam is TStringParam;
end;

{ TVTreeShortCutEditLink }

function TVTreeShortCutEditLink.CreateEditor: THotkey;
begin
  Result := THotKey.Create(nil);
  Result.Visible := False;
end;

procedure TVTreeShortCutEditLink.DoEndEdit;
begin
  inherited;
  Param.Value := Editor.HotKey;
end;

function TVTreeShortCutEditLink.DoPrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Editor.HotKey := Param.Value;
  Result := True;
end;

{ TVTreeShortCutEditorFactory }

function TVTreeShortCutEditorFactory.CreateEditor(aParam: TBaseParam): IVTEditLink;
begin
  Result := TVTreeShortCutEditLink.Create(aParam as TShortCutParam);
end;

function TVTreeShortCutEditorFactory.IsSuitableParam(aParam: TBaseParam): Boolean;
begin
  Result := aParam is TShortCutParam;
end;

end.
