unit search_query_dlg;
{$I cond_define.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  baseform, base_params, inifiles, Mask, Spin, opt_frame,
  csplg_query_params, comctrls, collections.deque;

type
  TSearchEngineQueryDlgParams = class(TBaseParams)
  strict private
    FLastQueryList: TDeque<string>;
    FLastFileRegExp: TDeque<string>;

  strict protected
    procedure DoReadParams(aIni: TCustomIniFile); override;
    procedure DoWriteParams(aIni: TCustomIniFile); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property LastQueryList: TDeque<string> read FLastQueryList;
    property LastFileRegExp: TDeque<string> read FLastFileRegExp;
  end;

  TSearchEngineQueryDlg = class(TBaseForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    lblQuery: TLabel;
    cmbQuery: TComboBoxEx;
    OptionsFrame: TOptionsFrame;
    cmbFileRegExp: TComboBoxEx;
    lblFileRegExp: TLabel;
    procedure btnOkClick(Sender: TObject);
  private
    FQuery: TCodeSearchQueryParams;
    FParams: TSearchEngineQueryDlgParams;

  protected
    procedure SetupControls; override;
    procedure UpdateData; override;
    function CanCloseByDialogKey(aKeyCode: Word): Boolean; override;
  public
    constructor Create(aParent: HWND; aQuery: TCodeSearchQueryParams); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  str_utils, collections.array_utils;

{$R *.dfm}

{ TCodeSearchQueryDlg }

procedure TSearchEngineQueryDlg.btnOkClick(Sender: TObject);
begin
  UpdateData;
  Hide;
end;

function TSearchEngineQueryDlg.CanCloseByDialogKey(aKeyCode: Word): Boolean;
begin
  Result := not OptionsFrame.vstOptions.IsEditing;
end;

constructor TSearchEngineQueryDlg.Create(aParent: HWND; aQuery:
    TCodeSearchQueryParams);
begin
  inherited CreateParented(aParent);
  CloseByEscape := True;
  FQuery := aQuery;
  FParams := TSearchEngineQueryDlgParams.Create;
  FParams.ReadParams;
  OptionsFrame.SetParams(aQuery.Tree);
end;

destructor TSearchEngineQueryDlg.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TSearchEngineQueryDlg.SetupControls;
begin
  if InSetupControls then
    Exit;

  BeginSetupControls;
  try
    inherited;

    cmbQuery.Items.Clear;
    cmbQuery.Items.AddStrings(TArrayUtils.AsArray<string>(FParams.LastQueryList.AsEnumerable));
    cmbQuery.Text := FQuery.QueryText;

    cmbFileRegExp.Items.Clear;
    cmbFileRegExp.Items.AddStrings(TArrayUtils.AsArray<string>(FParams.LastFileRegExp.AsEnumerable));
    cmbFileRegExp.Text := FQuery.FileRegExp;
  finally
    EndSetupControls;
  end;
end;

procedure TSearchEngineQueryDlg.UpdateData;
begin
  if FParams.LastQueryList.First <> cmbQuery.Text then
  begin
    FParams.LastQueryList.AddFirst(cmbQuery.Text);
    while FParams.LastQueryList.Count > 10 do
      FParams.LastQueryList.RemoveLast;
  end;

  if FParams.LastFileRegExp.First <> cmbFileRegExp.Text then
  begin
    FParams.LastFileRegExp.AddFirst(cmbFileRegExp.Text);
    while FParams.LastFileRegExp.Count > 10 do
      FParams.LastFileRegExp.RemoveLast;
  end;

  FQuery.QueryText := cmbQuery.Text;
  FQuery.FileRegExp := cmbFileRegExp.Text;
  FParams.WriteParams;
end;

constructor TSearchEngineQueryDlgParams.Create;
begin
  inherited Create;
  FLastQueryList := TDeque<string>.Create;
  FLastFileRegExp := TDeque<string>.Create;
end;

destructor TSearchEngineQueryDlgParams.Destroy;
begin
  FreeAndNil(FLastQueryList);
  FreeAndNil(FLastFileRegExp);
  inherited;
end;

procedure TSearchEngineQueryDlgParams.DoReadParams(aIni: TCustomIniFile);
begin
  inherited;
  FLastQueryList.AddRangeLast(ReadStrings(aIni, 'LastSearchQueryList', 'Query'));
  FLastFileRegExp.AddRangeLast(ReadStrings(aIni, 'LastFileRegExp', 'FileRegExp'));
end;

procedure TSearchEngineQueryDlgParams.DoWriteParams(aIni: TCustomIniFile);
begin
  inherited;
  WriteStrings(aIni, 'LastSearchQueryList', 'Query', FLastQueryList.AsEnumerable);
  WriteStrings(aIni, 'LastFileRegExp', 'FileRegExp', FLastFileRegExp.AsEnumerable);
end;

end.
