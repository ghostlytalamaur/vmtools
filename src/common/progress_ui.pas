unit progress_ui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, progress, baseform;

type
  TProgressDlg = class(TBaseForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    lblInfo: TLabel;
    btnCancel: TButton;
    pbBar: TProgressBar;
    procedure btnCancelClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FCancelled: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  TSimpleProgressLink = class(TProgressLink)
  private
    FDlg: TProgressDlg;
    FParent: HWND;
    FActiveWindow: HWND;
    FInfo: TProgressInfo;
    FTaskWindows: TTaskWindowList;
    FFocusState: TFocusState;

    procedure SetupDialog;
    procedure EnableWindows;
    procedure DisableWindows;
  protected
    function GetCancelled: Boolean; override;
  public
    constructor Create(aParent: HWND);
    destructor Destroy; override;

    procedure BeginProgress(aInfo: TProgressInfo); override;
    procedure EndProgress(aNewInfo: TProgressInfo); override;

    procedure InfoChanged(aInfo: TProgressInfo); override;
  end;

implementation

uses
  math;

{$R *.dfm}

{ TSimpleProgressLink }

procedure TSimpleProgressLink.BeginProgress(aInfo: TProgressInfo);
begin
  inherited;
  FInfo := aInfo;
  if not FDlg.Visible then
  begin
    DisableWindows;
    SetupDialog;
    FDlg.Show;
    SendMessage(FDlg.Handle, CM_ACTIVATE, 0, 0)
  end
  else
    SetupDialog;
end;

constructor TSimpleProgressLink.Create(aParent: HWND);
begin
  inherited Create;
  FParent := aParent;
  FDlg := TProgressDlg.Create(nil);
  FDlg.CloseByEscape := False;
end;

destructor TSimpleProgressLink.Destroy;
begin
  EnableWindows;
  FreeAndNil(FDlg);
  inherited;
end;

procedure TSimpleProgressLink.DisableWindows;
begin
  FFocusState := SaveFocusState;
  FActiveWindow := GetActiveWindow;
  Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
  Screen.FocusedForm := FDlg;
  FTaskWindows := DisableTaskWindows(0);
end;

procedure TSimpleProgressLink.EnableWindows;
begin
  if FTaskWindows <> nil then
  begin
    EnableTaskWindows(FTaskWindows);
    FTaskWindows := nil;
  end;
  if Screen.SaveFocusedList.Count > 0 then
  begin
    Screen.FocusedForm := TCustomForm(Screen.SaveFocusedList.First);
    Screen.SaveFocusedList.Remove(Screen.FocusedForm);
  end
  else
    Screen.FocusedForm := nil;
//  if (FDlg <> nil) and (GetActiveWindow <> FDlg.Handle) then
//    FActiveWindow := 0;
  if (FActiveWindow <> 0) and not IsWindow(FActiveWindow) then
    FActiveWindow := 0;//FindTopMostWindow(0);
  if FActiveWindow <> 0 then
    SetActiveWindow(FActiveWindow);
  if FFocusState <> nil then
    RestoreFocusState(FFocusState);
  FFocusState := nil;
end;

procedure TSimpleProgressLink.EndProgress(aNewInfo: TProgressInfo);
begin
  inherited;
  FInfo := aNewInfo;
  if (FInfo = nil) and (FDlg <> nil) then
  begin
    SendMessage(FDlg.Handle, CM_DEACTIVATE, 0, 0);
    FDlg.Hide;
    EnableWindows;
  end
  else
    SetupDialog;
end;

function TSimpleProgressLink.GetCancelled: Boolean;
begin
  if Application <> nil then
    Application.ProcessMessages;
  Result := (FDlg <> nil) and (FDlg.FCancelled);
end;

procedure TSimpleProgressLink.InfoChanged(aInfo: TProgressInfo);
begin
  if aInfo = FInfo then
    SetupDialog;
end;

procedure TSimpleProgressLink.SetupDialog;
begin
  if FDlg = nil then
    Exit;

  if (FInfo <> nil) then
  begin
    FDlg.lblInfo.Caption := FInfo.Info;
    FDlg.pbBar.Max := math.Max(0, FInfo.Total);
    FDlg.pbBar.Position := FInfo.Current;
    FDlg.pbBar.Visible := FDlg.pbBar.Max <> 0;
    FDlg.ClientHeight := FDlg.pnlMain.Height + FDlg.pnlButtons.Height;
  end;
end;

procedure TProgressDlg.btnCancelClick(Sender: TObject);
begin
  FCancelled := True;
end;

procedure TProgressDlg.FormHide(Sender: TObject);
begin
  FCancelled := True;
end;

end.
