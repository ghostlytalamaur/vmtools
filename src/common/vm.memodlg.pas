unit vm.memodlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, baseform, observer;

type
  TMemoDialog = class(TBaseForm)
    pnlButtons: TPanel;
    mmoMsg: TMemo;
    btnOk: TButton;
  private
    FStirngs: IObservableData<TStringList>;
    FStringsObserver: IDataObserver<TStringList>;
    procedure OnStringsChanged(aList: TStringList);
  public
    constructor Create(aOwner: TComponent; aCaption: string); reintroduce; overload;
    constructor Create(aOwner: TComponent; aCaption, aText: string); reintroduce; overload;
    constructor Create(aOwner: TComponent; aCaption: string; aStrings: IObservableData<TStringList>); reintroduce; overload;
    destructor Destroy; override;
    class function Execute(aCaption, aText: string): Boolean; overload;
    class function Execute(aCaption: string; aStrings: IObservableData<TStringList>): Boolean; overload;
  end;

implementation

{$R *.dfm}

{ TMemoDialog }

constructor TMemoDialog.Create(aOwner: TComponent; aCaption, aText: string);
begin
  Create(aOwner, aCaption);
  mmoMsg.Text := aText;
end;

class function TMemoDialog.Execute(aCaption, aText: string): Boolean;
var
  Dlg: TMemoDialog;
begin
  Dlg := TMemoDialog.Create(nil, aCaption, aText);
  try
    Result := Dlg.ShowModal = mrOk;
  finally
    FreeAndNil(Dlg);
  end;
end;

constructor TMemoDialog.Create(aOwner: TComponent; aCaption: string; aStrings: IObservableData<TStringList>);
begin
  Create(aOwner, aCaption);
  FStirngs := aStrings;
  if FStirngs <> nil then
  begin
    FStringsObserver := TDelegatedDataObserver<TStringList>.Create(OnStringsChanged);
    FStirngs.RegisterObserver(FStringsObserver);
  end;
end;

constructor TMemoDialog.Create(aOwner: TComponent; aCaption: string);
begin
  inherited Create(aOwner);
  Caption := aCaption;
end;

destructor TMemoDialog.Destroy;
begin
  if FStirngs <> nil then
    FStirngs.RemoveObserver(FStringsObserver);
  inherited;
end;

class function TMemoDialog.Execute(aCaption: string; aStrings: IObservableData<TStringList>): Boolean;
var
  Dlg: TMemoDialog;
begin
  Dlg := TMemoDialog.Create(nil, aCaption, aStrings);
  try
    Result := Dlg.ShowModal = mrOk;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TMemoDialog.OnStringsChanged(aList: TStringList);
var
  I: Integer;
  OldPos: TPoint;
begin
  mmoMsg.Lines.BeginUpdate;
  try
    OldPos := mmoMsg.CaretPos;
    if aList = nil then
      mmoMsg.Lines.Clear
    else
    begin
      while mmoMsg.Lines.Count > aList.Count do
        mmoMsg.Lines.Delete(mmoMsg.Lines.Count - 1);
      for I := 0 to aList.Count - 1 do
        if I = mmoMsg.Lines.Count then
          mmoMsg.Lines.Add(aList[I])
        else if mmoMsg.Lines[I] <> aList[I] then
          mmoMsg.Lines[I] := aList[I];
    end;

    mmoMsg.CaretPos := OldPos;
  finally
    mmoMsg.Lines.EndUpdate;
  end;
end;

end.
