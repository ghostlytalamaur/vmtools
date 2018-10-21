unit vm.ide.actions.options_frame;

{$I cond_define.inc}

interface

uses
  windows, messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  generics.collections,  {$IFDEF DELPHIX_BERLIN_UP}actions, {$ENDIF} actnlist;

type
  TOnShortcutIsAvailable = function (aShortCut: TShortCut; out Act: TContainedAction): Boolean of object;

  TActionShortCutData = class
    Caption: string;
    Name: string;
    ShortCut: TShortCut;
  end;

  TActionManagerFrame = class(TFrame)
    pnlAssign: TPanel;
    lvShortCuts: TListView;
    hkAssign: THotKey;
    btnAssign: TButton;
    lblError: TLabel;
    procedure btnAssignClick(Sender: TObject);
    procedure hkAssignChange(Sender: TObject);
    procedure lvShortCutsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    FData : TList<TActionShortCutData>;
    FOnShortcutIsAvailable: TOnShortcutIsAvailable;
    procedure SetupControls;
    procedure EnableControls;
    procedure SetupListView;
    function CurAction: TActionShortCutData;
  public
    procedure SetData(aData: TList<TActionShortCutData>; aOnShortcutIsAvailable: TOnShortcutIsAvailable);
  end;

implementation

{$R *.dfm}

uses
  menus;

procedure TActionManagerFrame.btnAssignClick(Sender: TObject);
begin
  if CurAction <> nil then
    CurAction.ShortCut := hkAssign.HotKey;
  SetupListView;
  SetupControls;
end;

{ TActionManagerFrame }

procedure TActionManagerFrame.EnableControls;
var
  Act: TContainedAction;
begin
  btnAssign.Enabled := (CurAction <> nil) and
      Assigned(FOnShortcutIsAvailable) and (FOnShortcutIsAvailable(hkAssign.HotKey, Act) or (Act.Name = CurAction.Name));
end;

procedure TActionManagerFrame.hkAssignChange(Sender: TObject);
var
  Act: TContainedAction;
begin
  if not Assigned(FOnShortcutIsAvailable) or FOnShortcutIsAvailable(hkAssign.HotKey, Act) or
      (CurAction <> nil) and (Act <> nil) and (Act.Name = CurAction.Name) then
    lblError.Caption := ''
  else
    lblError.Caption := Format('ShortCut already used by action "%s"', [(Act as TAction).Caption]);
  EnableControls;
end;

procedure TActionManagerFrame.lvShortCutsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  SetupControls;
end;

procedure TActionManagerFrame.SetData(aData: TList<TActionShortCutData>; aOnShortcutIsAvailable:
    TOnShortcutIsAvailable);
begin
  FData := aData;
  FOnShortcutIsAvailable := aOnShortcutIsAvailable;
  SetupListView;
end;

procedure TActionManagerFrame.SetupControls;
var
  ActData: TActionShortCutData;
begin
  ActData := CurAction;
  if ActData <> nil then
    hkAssign.HotKey := ActData.ShortCut
  else
    hkAssign.HotKey := 0;
  hkAssignChange(Self);
  EnableControls;
end;

function TActionManagerFrame.CurAction: TActionShortCutData;
begin
  if (lvShortCuts.ItemIndex >= 0) and (lvShortCuts.ItemIndex < lvShortCuts.Items.Count) then
    Result := FData[lvShortCuts.ItemIndex]
  else
    Result := nil;
end;

procedure TActionManagerFrame.SetupListView;
var
  Item: TListItem;
  ActData: TActionShortCutData;
  WasIndex: Integer;
begin
  WasIndex := lvShortCuts.ItemIndex;
  lvShortCuts.Clear;
  if FData = nil then
    Exit;

  for ActData in FData do
  begin
    Item := lvShortCuts.Items.Add;
    Item.Caption := ActData.Caption;
    Item.SubItems.Add(ShortCutToText(ActData.ShortCut));
  end;

  if (WasIndex >= 0) and (WasIndex <= lvShortCuts.Items.Count) then
    lvShortCuts.ItemIndex := WasIndex
  else if lvShortCuts.Items.Count > 0 then
    lvShortCuts.ItemIndex := 0;

  SetupControls;
end;

end.
