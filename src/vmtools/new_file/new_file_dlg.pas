unit new_file_dlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, baseform, Vcl.CheckLst, generics.collections,
  collections.sets, Vcl.ComCtrls;

type
  TFileTemplate = class
  strict private
    FName: string;
    FFiles: ISet<string>;
  public
    constructor Create(aName: string);
    property Name: string read FName;
    property Files: ISet<string> read FFiles;
  end;

  TFileToSave = class
  public
    FilePath: string;
    Line: Integer;
    Column: Integer;
    ShouldOpen: Boolean;
    Lines: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  EFileExistsException = class(Exception);
  TTemplateProcessor = class
  private
    FOnOpenFile: TProc<string, Integer, Integer>;
    FOnFileExists: TFunc<string, Boolean>;

    function ProcessTemplateFile(const aTemplateFile, aDirPath, aFileNameWithoutExt: string): TFileToSave;
  public
    constructor Create(aOnOpenFile: TProc<string, Integer, Integer>; aOnFileExists: TFunc<string, Boolean>);

    function GetTemplates: TList<TFileTemplate>;
    function CreateFileFromTemplate(const aFullFileName: string; aTemplate: TFileTemplate): Boolean;
  end;

  TCreateFileDlg = class(TBaseForm)
    pnlButtons: TPanel;
    pnlMain: TPanel;
    edtFileName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    lstPaths: TListBox;
    lstTemplates: TListBox;
    procedure btnOkClick(Sender: TObject);
    procedure DoEnableControls(Sender: TObject);
    procedure lstTemplatesKeyPress(Sender: TObject; var Key: Char);
  private
    FTemplateProc: TTemplateProcessor;
    FPaths: IEnumerable<string>;
    FTemplates: TList<TFileTemplate>;

  protected
    procedure SetupControls; override;
    procedure EnableControls; override;
    function CanCloseByDialogKey(aKeyCode: Word): Boolean; override;
  public
    constructor Create(aOwner: TComponent; aOnOpenFile: TProc<string, Integer, Integer>); reintroduce;
    destructor Destroy; override;
    procedure SetPaths(aPaths: IEnumerable<string>);
  end;

implementation

uses
  Ioutils, Types, collections.maps, collections.common, uitypes, generics.defaults;

{$R *.dfm}

{ TCreateFileDlg }

function TCreateFileDlg.CanCloseByDialogKey(aKeyCode: Word): Boolean;
begin
  Result := False;
end;

constructor TCreateFileDlg.Create(aOwner: TComponent; aOnOpenFile: TProc<string, Integer, Integer>);
begin
  inherited Create(aOwner);
  FTemplateProc := TTemplateProcessor.Create(aOnOpenFile,
    function (aFilePath: string): Boolean
    begin
      Result := MessageDlg(Format('File exists: %s' + #13#10 + 'Overwrite?', [aFilePath]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    end);
  FTemplates := FTemplateProc.GetTemplates;
end;

destructor TCreateFileDlg.Destroy;
begin
  FreeAndNil(FTemplates);
  inherited;
end;

procedure TCreateFileDlg.btnOkClick(Sender: TObject);
begin
  if (lstPaths.Count = 0) or (lstTemplates.Count = 0) and (lstPaths.ItemIndex < 0) or
      (lstTemplates.ItemIndex < 0) and (Trim(edtFileName.Text) = '') then
    Exit;

  FTemplateProc.CreateFileFromTemplate(TPath.Combine(lstPaths.Items[lstPaths.ItemIndex], edtFileName.Text),
      FTemplates[lstTemplates.ItemIndex]);
end;

procedure TCreateFileDlg.DoEnableControls(Sender: TObject);
begin
  EnableControls;
end;

procedure TCreateFileDlg.EnableControls;
begin
  inherited;
  btnOk.Enabled := (lstPaths.Count > 0) and (lstTemplates.Count > 0) and (lstPaths.ItemIndex >= 0) and
      (lstTemplates.ItemIndex >= 0) and (Trim(edtFileName.Text) <> '') and
      TPath.HasValidPathChars(edtFileName.Text, False) and
      TPath.HasValidPathChars(lstPaths.Items[lstPaths.ItemIndex], False);
end;

procedure TCreateFileDlg.lstTemplatesKeyPress(Sender: TObject; var Key: Char);
begin
  EnableControls;
end;

procedure TCreateFileDlg.SetPaths(aPaths: IEnumerable<string>);
begin
  FPaths := aPaths;
  SetupControls;
end;

procedure TCreateFileDlg.SetupControls;

  procedure FillListBox(aListBox: TListBox; aStrings: IEnumerable<string>);
  var
    S: string;
  begin
    aListBox.Items.BeginUpdate;
    try
      aListBox.Items.Clear;
      if FPaths <> nil then
        for S in aStrings do
          aListBox.Items.Add(S);
      if aListBox.Items.Count > 0 then
        aListBox.ItemIndex := 0;
    finally
      aListBox.Items.EndUpdate;
    end;
  end;

begin
  inherited;

  FillListBox(lstTemplates,
    Pipeline<TFileTemplate>.From(FTemplates).Map<string>(function (const aTemplate: TFileTemplate): string
    begin
      Result := Format('%s (%d files)', [aTemplate.Name, aTemplate.Files.Count])
    end).Enum);

  FillListBox(lstPaths, FPaths);
end;

{ TFileTemplate }

constructor TFileTemplate.Create(aName: string);
begin
  inherited Create;
  FName := aName;
  FFiles := THashSet<string>.Create(3);
end;

{ TTemplateProcessor }

function TTemplateProcessor.ProcessTemplateFile(const aTemplateFile, aDirPath, aFileNameWithoutExt: string): TFileToSave;
var
  I: Integer;
  Lines: TStringList;
  Line: string;
  CursorPos: Integer;
begin
  Result := TFileToSave.Create;
  Result.FilePath := TPath.Combine(aDirPath, aFileNameWithoutExt + TPath.GetExtension(aTemplateFile));

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(aTemplateFile);
    Result.Lines := TStringList.Create;

    Result.Column := -1;
    Result.Line := -1;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      Line := Line.Replace('{%FileName%}', aFileNameWithoutExt, [rfReplaceAll, rfIgnoreCase]);
      if not Result.ShouldOpen then
      begin
        Result.ShouldOpen := Line.Contains('{%Open%}');
        if Result.ShouldOpen then
          Continue;
      end;

      if Result.Column < 0 then
      begin
        CursorPos := Line.IndexOf('{%Cursor%}', 0);
        if CursorPos >= 0 then
        begin
          Line := Line.Replace('{%Cursor%}', '', [rfReplaceAll, rfIgnoreCase]);
          Result.Column := CursorPos + 1;
          Result.Line := Result.Lines.Count + 1;
        end;
      end;

      Result.Lines.Add(Line);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

constructor TTemplateProcessor.Create(aOnOpenFile: TProc<string, Integer, Integer>;
    aOnFileExists: TFunc<string, Boolean>);
begin
  inherited Create;
  FOnOpenFile := aOnOpenFile;
  FOnFileExists := aOnFileExists;
end;

function TTemplateProcessor.CreateFileFromTemplate(const aFullFileName: string; aTemplate: TFileTemplate): Boolean;
var
  FileNameWithoutExtension, TemplateFile, DirPath: string;
  FileToSave: TFileToSave;
  FilesToSave: TList<TFileToSave>;
begin
  Result := False;
  if aFullFileName.Trim.IsEmpty or (aTemplate = nil) or (aTemplate.Files.Count = 0) then
    Exit;

  DirPath := TPath.GetDirectoryName(aFullFileName);
  TDirectory.CreateDirectory(DirPath);
  FileNameWithoutExtension := TPath.GetFileNameWithoutExtension(aFullFileName);

  FilesToSave := TObjectList<TFileToSave>.Create;
  try
    for TemplateFile in aTemplate.Files do
      FilesToSave.Add(ProcessTemplateFile(TemplateFile, DirPath, FileNameWithoutExtension));

    for FileToSave in FilesToSave do
    begin
      if not TFile.Exists(FileToSave.FilePath) or
          Assigned(FOnFileExists) and FOnFileExists(FileToSave.FilePath) then
      begin
        FileToSave.Lines.SaveToFile(FileToSave.FilePath);
        if Assigned(FOnOpenFile) and FileToSave.ShouldOpen then
          FOnOpenFile(FileToSave.FilePath, FileToSave.Line, FileToSave.Column);
      end;
    end;
  finally
    FreeAndNil(FilesToSave);
  end;
end;

function TTemplateProcessor.GetTemplates: TList<TFileTemplate>;
var
  Files: TStringDynArray;
  TemplatesPath, FilePath, Name: string;
  Template: TFileTemplate;
  Name2Template: TDictionary<string, TFileTemplate>;
  Comparer: IComparer<TFileTemplate>;
begin
  Result := nil;
  TemplatesPath := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'templates');
  if not TDirectory.Exists(TemplatesPath) then
    Exit;

  Files := TDirectory.GetFiles(TemplatesPath);
  Name2Template := TDictionary<string, TFileTemplate>.Create;
  try
    for FilePath in Files do
    begin
      Name := TPath.GetFileNameWithoutExtension(FilePath);
      if not Name2Template.TryGetValue(Name, Template) then
      begin
        Template := TFileTemplate.Create(Name);
        Name2Template.Add(Name, Template);
      end;
      Template.Files.Add(FilePath);
    end;

    Result := TObjectList<TFileTemplate>.Create;
    Result.AddRange(Name2Template.Values);
    Comparer := TDelegatedComparer<TFileTemplate>.Create(function (const L, R: TFileTemplate): Integer
    begin
      Result := CompareStr(L.Name, R.Name);
    end);
    Result.Sort(Comparer);
  finally
    FreeAndNil(Name2Template);
  end;
end;


{ TFileToSave }

constructor TFileToSave.Create;
begin
  inherited Create;
  Lines := TStringList.Create;
  Column := -1;
end;

destructor TFileToSave.Destroy;
begin
  FreeAndNil(Lines);
  inherited;
end;

end.
