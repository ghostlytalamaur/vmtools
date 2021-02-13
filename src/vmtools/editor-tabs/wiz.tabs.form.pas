unit wiz.tabs.form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, collections.lists, baseform;

type
  TTabsListForm = class(TBaseForm)
    lvFiles: TListView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lvFilesData(Sender: TObject; Item: TListItem);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FList: IList<string>;
    FActiveIndex: Integer;

    procedure SelectPrevFile;
    procedure SelectNextFile;
    procedure SelectFileAt(Index: Integer);

  protected
    procedure SetupControls; override;
  public
    constructor Create(aOwner: TComponent; aList: IList<string>; aActiveIndex: Integer); reintroduce;
  end;

implementation

uses
  system.math;

{$R *.dfm}

{ TTabsListForm }

constructor TTabsListForm.Create(aOwner: TComponent; aList: IList<string>; aActiveIndex: Integer);
begin
  inherited Create(aOwner);
  CloseByEscape := True;
  FList := aList;
  FActiveIndex := aActiveIndex;
  SetupControls;
end;

procedure TTabsListForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TTabsListForm.lvFilesData(Sender: TObject; Item: TListItem);
var
  FilePath: string;
begin
  FilePath := FList[Item.Index];
  Item.Caption := ExtractFileName(FilePath);
  Item.SubItems.Add(ExtractFilePath(FilePath));
end;

procedure TTabsListForm.lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not lvFiles.Focused and lvFiles.CanFocus then
    lvFiles.SetFocus;

  case Key of
    VK_RETURN:
    begin
      ModalResult := mrOk;
      CloseModal;
      Key := Ord(#0);
    end;

    VK_TAB:
    begin
      if ssShift in Shift then
        SelectPrevFile
      else
        SelectNextFile;
      Key := Ord(#0);
    end;

    VK_DOWN:
    begin
      SelectNextFile;
      Key := Ord(#0);
    end;

    VK_UP:
    begin
      SelectPrevFile;
      Key := Ord(#0);
    end;

  end;
end;

procedure TTabsListForm.lvFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_CONTROL:
    begin
      ModalResult := mrOk;
      CloseModal;
    end;
  end;
end;

procedure TTabsListForm.SetupControls;
begin
  lvFiles.Items.BeginUpdate;
  try
    lvFiles.Items.Clear;
    lvFiles.Items.Count := FList.Count;

    SelectFileAt(FActiveIndex);
  finally
    lvFiles.Items.EndUpdate;
  end;
end;

procedure TTabsListForm.SelectPrevFile;
begin
  if (lvFiles.ItemIndex - 1) >= 0 then
    SelectFileAt(lvFiles.ItemIndex - 1)
  else
    SelectFileAt(lvFiles.Items.Count - 1);
end;

procedure TTabsListForm.SelectNextFile;
begin
  if (lvFiles.ItemIndex + 1) < lvFiles.Items.Count then
    SelectFileAt(lvFiles.ItemIndex + 1)
  else
    SelectFileAt(0);
end;

procedure TTabsListForm.SelectFileAt(Index: Integer);
begin
  if (Index >= 0) and (Index < lvFiles.Items.Count) and (lvFiles.ItemIndex <> Index) then
  begin
    lvFiles.ItemIndex := Index;
    lvFiles.Items[Index].MakeVisible(False);
  end;
end;

end.
