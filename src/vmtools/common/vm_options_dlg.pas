unit vm_options_dlg;

interface

uses
  Windows, Messages, SysUtils, variants, classes, graphics,
  controls, forms, dialogs, stdctrls, checkLst, extctrls, baseform,
  vm_internal_int, comctrls, opt_frame, opt_impl, vm.ide.actions.manager,
  generics.collections;

type
  TOptionsDlg = class(TBaseForm)
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    pnlList: TPanel;
    lstFrames: TListBox;
    pnlFrame: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure lstFramesClick(Sender: TObject);
  private
    FFrames: TDictionary<string, TFrame>;
    FCurFrame: TFrame;
  protected
    procedure SetupControls; override;
    function CanCloseByDialogKey(aKeyCode: Word): Boolean; override;
  public
    constructor Create(aOwner: TComponent; aFrames: TDictionary<string, TFrame>); reintroduce;
  end;

implementation

{$R *.dfm}

{ TOptionsDlg }

procedure TOptionsDlg.btnApplyClick(Sender: TObject);
begin
  UpdateData;
  SetupControls;
end;

constructor TOptionsDlg.Create(aOwner: TComponent; aFrames: TDictionary<string, TFrame>);
begin
  inherited Create(aOwner);
  FFrames := aFrames;
end;

procedure TOptionsDlg.SetupControls;
var
  FrameCaption: string;
begin
  if InSetupControls then
    Exit;

  BeginSetupControls;
  try
    inherited;
    lstFrames.Clear;
    for FrameCaption in FFrames.Keys do
      lstFrames.Items.Add(FrameCaption);
    if lstFrames.Items.Count > 0 then
    begin
      lstFrames.ItemIndex := 0;
      lstFramesClick(Self);
    end;
  finally
    EndSetupControls;
  end;
end;

function TOptionsDlg.CanCloseByDialogKey(aKeyCode: Word): Boolean;
begin
  Result := False; //not OptionsFrame1.vstOptions.IsEditing;
end;

procedure TOptionsDlg.lstFramesClick(Sender: TObject);
begin
  if FCurFrame <> nil then
  begin
    FCurFrame.Visible := False;
    FCurFrame.Parent := nil;
  end;
  FCurFrame := nil;
  if lstFrames.ItemIndex >= 0 then
    FFrames.TryGetValue(lstFrames.Items[lstFrames.ItemIndex], FCurFrame);
  if FCurFrame <> nil then
  begin
    FCurFrame.Parent := pnlFrame;
    FCurFrame.Align := alClient;
    FCurFrame.Visible := True;
  end;
end;

end.
