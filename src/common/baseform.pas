unit baseform;
{$I cond_define.inc}
interface

uses
  windows, forms, classes, Messages, controls, base_params, inifiles, SysUtils, StdCtrls,
  Dialogs;

type
  TBaseForm = class(TForm)
  private type
    TDialogParams = class(TBaseParams)
    private
      FSection: string;
    protected
      procedure DoReadParams(aIni: TCustomIniFile); override;
      procedure DoWriteParams(aIni: TCustomIniFile); override;
    public
      Pos: TPoint;
      Height: Integer;
      Width: Integer;

      constructor Create(aSection: string); reintroduce;
    end;

  private
    FSetupControlsCount: Integer;
    FCloseByEscape: Boolean;
    FBtnOk: TCustomButton;
    FBtnCancel: TCustomButton;

    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure RestoreParams;
    procedure SaveParams;
    function FindButtonWithModalResult(aModalResult: TModalResult): TCustomButton;
    procedure CreateInternal;
  protected
    procedure BeginSetupControls;
    procedure EndSetupControls;
    function InSetupControls: Boolean;

    function CanCloseByDialogKey(aKeyCode: Word): Boolean; virtual;
    procedure PrepareDialog; virtual;

    procedure EnableControls; virtual;
    procedure SetupControls; virtual;
    procedure SetupControlsStatic; virtual;
    procedure UpdateData; virtual;
    procedure DoShow; override;
    procedure DoHide; override;
  public
    constructor Create(aOwner: TComponent); override;
    constructor CreateParented(aParent: HWND);

    function ShowModal: Integer; override;

    property CloseByEscape: Boolean read FCloseByEscape write FCloseByEscape;
  end;

implementation

uses
  Math;

const
  cstEmptyPos = Low(Integer);

{ TBaseForm }

procedure TBaseForm.BeginSetupControls;
begin
  Inc(FSetupControlsCount);
end;

function TBaseForm.FindButtonWithModalResult(aModalResult: TModalResult): TCustomButton;

  function DoSearch(aControl: TControl): TCustomButton;
  var
    I: Integer;
  begin
    Result := nil;
    if (aControl is TCustomButton) and (TCustomButton(aControl).ModalResult = aModalResult) then
      Result := TCustomButton(aControl)
    else if (aControl is TWinControl) then
      for I := 0 to TWinControl(aControl).ControlCount - 1 do
      begin
        Result := DoSearch(TWinControl(aControl).Controls[I]);
        if Result <> nil then
          Exit;
      end;
  end;

begin
  Result := DoSearch(Self);
end;

function TBaseForm.CanCloseByDialogKey(aKeyCode: Word): Boolean;
begin
  Result := True;
end;

procedure TBaseForm.CMDialogKey(var Msg: TCMDialogKey);

  procedure DoCloseQuery(aModalResult: TModalResult);
  begin
    if not CanCloseByDialogKey(Msg.CharCode) then
      Exit;

    if fsModal in FormState then
    begin
      ModalResult := aModalResult;
      Msg.Result := 1;
    end
    else
      Msg.Result := Integer(PostMessage(Handle, WM_CLOSE, 0, 0));
  end;

var
  FocusWnd: HWND;
begin
  case Msg.CharCode of
    VK_ESCAPE:
      if (KeyDataToShiftState(Msg.KeyData) = []) and CloseByEscape then
        DoCloseQuery(mrCancel);

    VK_RETURN:
      if (KeyDataToShiftState(Msg.KeyData) = []) and (fsModal in FormState) then
      begin
        FocusWnd := GetFocus;
        if (FBtnCancel <> nil) and (FBtnCancel.Handle = FocusWnd) then
          DoCloseQuery(mrCancel)
        else if (FBtnOk <> nil) then
          DoCloseQuery(mrOk)
      end;
  else
    inherited;
  end;
end;

procedure TBaseForm.CMShowingChanged(var Message: TMessage);
begin
  if Showing then
    RestoreParams
  else
    SaveParams;
  inherited;
end;

constructor TBaseForm.Create(aOwner: TComponent);
begin
  inherited;
  CreateInternal;
end;

constructor TBaseForm.CreateParented(aParent: HWND);
begin
  inherited CreateParented(aParent);
  CreateInternal;
end;

procedure TBaseForm.CreateInternal;
begin
  FBtnOk := FindButtonWithModalResult(mrOk);
  FBtnCancel := FindButtonWithModalResult(mrCancel);

  CloseByEscape := True;
//  RestoreParams;
end;

procedure TBaseForm.DoShow;
begin
  inherited;
  SetupControlsStatic;
  SetupControls;
end;

procedure TBaseForm.DoHide;
begin
  inherited;
//  SaveParams;
end;

procedure TBaseForm.EnableControls;
//var
//  ParentProcessID: DWORD;
begin
//  Inc(FDisableLevel);
//  if FDisableLevel <> 1 then
//    Exit;
//
//  FParentAppWnd := Application.Handle;
//  if ParentWindow <> 0 then
//  begin
//    GetWindowThreadProcessID(ParentWindow, @ParentProcessID);
//    if GetCurrentProcessID <> ParentProcessID then
//      FParentAppWnd := FindApplicationWindow(ParentProcessID)
//  end;
end;

procedure TBaseForm.EndSetupControls;
begin
  Dec(FSetupControlsCount);
  Assert(FSetupControlsCount >= 0);
end;

function TBaseForm.InSetupControls: Boolean;
begin
  Result := FSetupControlsCount > 0;
end;

procedure TBaseForm.PrepareDialog;
begin
  SetupControlsStatic;
  SetupControls;
  EnableControls;
end;

procedure TBaseForm.RestoreParams;
var
  DesktopRect: TRect;

  procedure UpdateWinRect(var aRect: TRect);
  var
    DX, DY: Integer;
  begin
    DX := 0;
    DY := 0;
    if aRect.Left < DesktopRect.Left then
      DX := DesktopRect.Left - aRect.Left
    else if aRect.Right > DesktopRect.Right then
      DX := DesktopRect.Right - aRect.Right;

    if aRect.Top < DesktopRect.Top then
      DY := DesktopRect.Top - aRect.Top
    else if aRect.Bottom > DesktopRect.Bottom then
      DY := DesktopRect.Bottom - aRect.Bottom;
    OffsetRect(aRect, DX, DY);
  end;

var
  Params: TDialogParams;
  NewLeft, NewTop, NewWidth, NewHeight: Integer;
  WinRect, NewWinRect: TRect;
begin
  if not HandleAllocated then
    Exit;

  Params := TDialogParams.Create(ClassName);
  try
    Params.ReadParams;

    GetWindowRect(Handle, WinRect);
    DesktopRect := Screen.MonitorFromRect(WinRect).WorkareaRect;

    Newleft := IfThen(Params.Pos.X = cstEmptyPos, (DesktopRect.Left + DesktopRect.Right) div 2 - Width div 2, Params.Pos.X);
    NewTop := IfThen(Params.Pos.Y = cstEmptyPos, (DesktopRect.Top + DesktopRect.Bottom) div 2 - Height div 2, Params.Pos.Y);
    NewWidth := WinRect.Right - WinRect.Left;
    NewHeight := WinRect.Bottom - WinRect.Top;
    if (BorderStyle = bsSizeable) or (BorderStyle = bsSizeToolWin) then
    begin
      if Params.Height > 0 then
        NewHeight := Params.Height;
      if Params.Width > 0 then
        NewWidth := Params.Width;
    end;

    NewWinRect := Rect(NewLeft, NewTop, NewLeft + NewWidth, NewTop + NewHeight);
    UpdateWinRect(NewWinRect);
    if not EqualRect(WinRect, NewWinRect) then
    begin
      Position := poDesigned;
      Left := NewWinRect.Left;
      Top := NewWinRect.Top;
      SetWindowPos(Handle, HWND_TOP, NewWinRect.Left, NewWinRect.Top,
          NewWinRect.Right - NewWinRect.Left, NewWinRect.Bottom - NewWinRect.Top, SWP_NOZORDER or SWP_NOACTIVATE);
    end;
  finally
    FreeAndNil(Params);
  end;
end;

procedure TBaseForm.SaveParams;
var
  Params: TDialogParams;
  WinRect: TRect;
begin
  if not HandleAllocated then
    Exit;

  Params := TDialogParams.Create(ClassName);
  try
    GetWindowRect(Handle, WinRect);
    Params.Pos.X := WinRect.Left;
    Params.Pos.Y := WinRect.Top;
    Params.Height := WinRect.Bottom - WinRect.Top;
    Params.Width := WinRect.Right - WinRect.Left;

    Params.WriteParams;
  finally
    FreeAndNil(Params);
  end;
end;

procedure TBaseForm.SetupControls;
begin

end;

procedure TBaseForm.SetupControlsStatic;
begin

end;

function TBaseForm.ShowModal: Integer;
begin
  PrepareDialog;
  Result := inherited;
  if Result = mrOk then
    UpdateData;
end;

procedure TBaseForm.UpdateData;
begin

end;

{ TBaseForm.TDialogParams }

constructor TBaseForm.TDialogParams.Create(aSection: string);
begin
  inherited Create;
  FSection := aSection;
end;

procedure TBaseForm.TDialogParams.DoReadParams(aIni: TCustomIniFile);
begin
  inherited;
  Pos.X := aIni.ReadInteger(FSection, 'PosX', cstEmptyPos);
  Pos.Y := aIni.ReadInteger(FSection, 'PosY', cstEmptyPos);
  Height := aIni.ReadInteger(FSection, 'Height', -1);
  Width := aIni.ReadInteger(FSection, 'Width', -1);
end;

procedure TBaseForm.TDialogParams.DoWriteParams(aIni: TCustomIniFile);
begin
  inherited;
  aIni.WriteInteger(FSection, 'PosX', Pos.X);
  aIni.WriteInteger(FSection, 'PosY', Pos.Y);
  aIni.WriteInteger(FSection, 'Width', Width);
  aIni.WriteInteger(FSection, 'Height', Height);
end;

end.
