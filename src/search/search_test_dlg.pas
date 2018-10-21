unit search_test_dlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, search_frame, search_handler, ExtCtrls;

type
  TSearchTestDlg = class(TForm)
    btnSearch: TButton;
    SearchResultFrame1: TSearchResultFrame;
    Panel1: TPanel;
    btnPrev: TButton;
    btnNext: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FHandler: TSearchHandler;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  str_utils;

{$R *.dfm}


type
  TTestSearchHandler = class(TSearchHandler)
  public
    function GetProjectPaths: IEnumerable<string>; override;
    function GetIndexSearchPaths: TArray<string>; override;
    function GetQueryText: string; override;
  end;


{ TTestSearchHandler }

function TTestSearchHandler.GetIndexSearchPaths: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'D:\dev\delphi\vmtools';
end;

function TTestSearchHandler.GetProjectPaths: IEnumerable<string>;
begin
  Result := TStrUtils.Words('', []);
end;

function TTestSearchHandler.GetQueryText: string;
begin
  Result := 'string';
end;

procedure TSearchTestDlg.btnSearchClick(Sender: TObject);
begin
  FHandler.ExecuteSearch;
end;

constructor TSearchTestDlg.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHandler := TTestSearchHandler.Create;
  SearchResultFrame1.SetHandler(FHandler);
end;

destructor TSearchTestDlg.Destroy;
begin
  SearchResultFrame1.SetHandler(nil);
  FreeAndNil(FHandler);
  inherited;
end;

procedure TSearchTestDlg.btnNextClick(Sender: TObject);
begin
  SearchResultFrame1.ShowResultItem(True);
end;

procedure TSearchTestDlg.btnPrevClick(Sender: TObject);
begin
  SearchResultFrame1.ShowResultItem(False);
end;

end.
