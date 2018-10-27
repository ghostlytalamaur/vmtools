unit vm.memodlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, baseform;

type
  TMemoDialog = class(TBaseForm)
    pnlButtons: TPanel;
    mmoMsg: TMemo;
    btnOk: TButton;
  public
    constructor Create(aOwner: TComponent; aCaption, aText: string); reintroduce;
    class function Execute(aCaption, aText: string): Boolean;
  end;

implementation

{$R *.dfm}

{ TMemoDialog }

constructor TMemoDialog.Create(aOwner: TComponent; aCaption, aText: string);
begin
  inherited Create(aOwner);
  Caption := aCaption;
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

end.
