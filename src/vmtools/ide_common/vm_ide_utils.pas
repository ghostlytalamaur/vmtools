unit vm_ide_utils;

{$I cond_define.inc}

interface

uses
  toolsapi, classes, Windows, Controls, Forms, ActnList;

type
  TGXOtaUtils = class
  private
    class function GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
    class function GxOtaGetOpenModuleCount: Integer;

    class function GxOtaGetEditorServices: IOTAEditorServices;
    class function GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView; overload;
    class function GxOtaGetCurrentSourceEditor: IOTASourceEditor;
    class function GxOtaGetCurrentModule: IOTAModule;

    class function GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
    class function GxOtaHaveCPPSupport: Boolean;
    class function GxOtaGetCurrentIDEEditControl: TWinControl;
    class function GxOtaGetBaseModuleFileName(const FileName: string): string;
    class function GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean = False): Boolean;
    class function GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean = False): Boolean;
    class function IsStandAlone: Boolean;
    class function GxOtaMakeSourceVisible(const FileName: string): Boolean;
    class function GxOtaOpenFile(const FileName: string): Boolean;
    class function GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
    class function GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
    class function GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
    class function GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
    class function GxOtaGetModule(const FileName: string): IOTAModule;
    class function GxOtaGetEditorLine(View: IOTAEditView; LineNo: Integer): UTF8String;
    class function GxOtaConvertColumnCharsToBytes(LineData: UTF8String; CharIndex: Integer; EndByte: Boolean): Integer;
  public
    class procedure GxOtaGoToFileLineColumn(const FileName: string; Line: Integer; StartColumn: Integer = 0;
        StopColumn: Integer = 0; ShowInMiddle: Boolean = True);
    class function GxOtaFocusCurrentIDEEditControl: Boolean;

    class function GxOtaGetTopMostEditView: IOTAEditView; overload;
    class function GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
  end;

  TGXIdeUtils = class
  public
    class function GetIDEEditControl(Form: TCustomForm): TWinControl;
    class function GetIdeMainForm: TCustomForm;
  end;

  TGXGenericUtils = class
  private
    class function IsForm(const FileName: string): Boolean;
    class function TryFocusControl(Control: TWinControl): Boolean;
  end;

  TVMOtaUtils = class
  private
    class function NormalizeDelphiPath(const Path: string): string;
    class function GetDelphiRootPath: string;
    class function GetProjectOptions: IOTAProjectOptions;
    class function GetEnvironmentOptions: IOTAEnvironmentOptions;
    class function GetTopEditView: IOTAEditView;
    class function GetWordUnderCursorInCurView: string;
    class function GetActiveProject: IOTAProject;

    class function GetProjectGroupFileList: IEnumerable<string>;
    class function GetActiveProjectFileList: IEnumerable<string>;

    // may return relative or macro paths
    class function GetProjectPaths: string;
    class function GetBrowsingPath: string;

    class function NormalizePaths(aProject: IOTAProject; Paths: IEnumerable<string>): IEnumerable<string>;
  public
    class function GetIdeActionList: TCustomActionList;
    class function GetKeyboardServices: IOTAKeyboardServices;

    class function GetCurrentOpenFileName: string;
    class function GetCurrentOpenProjectFileName: string;
    class function GetBrowsingPathList: IEnumerable<string>;
    class function GetProjectPathsList: IEnumerable<string>;
    class function GetProjectGroupsPathsList: IEnumerable<string>;
    class function GetProjectFileList(aFromProjectGroup: Boolean): IEnumerable<string>;
    class function GetWordUnderCursorInCurView2: string;
  end;


implementation

uses
  sysutils, Registry, str_utils, collections.array_utils, collections.common, collections.sets, vmsys;



class function TVMOtaUtils.GetDelphiRootPath: string;
const
  cstRootDir = 'RootDir';
var
  LocalPath: string;
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    LocalPath := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
    Delete(LocalPath, 1, 1);
    if Reg.OpenKey(LocalPath, False) then
      Result := Reg.ReadString(cstRootDir);
  finally
    Reg.Free;
  end;
end;

class function TVMOtaUtils.NormalizeDelphiPath(const Path: string): string;
var
  MacroEndPos: integer;
begin
  Result := LowerCase(Trim(Path));
  if Result[1] = '$' then
  begin
    MacroEndPos := Pos(')', Result);
    if MacroEndPos <> 0 then
      Result :=  TStrUtils.Join([GetDelphiRootPath, Copy(Result, MacroEndPos + 1, Length(Result) - MacroEndPos)], '\');
  end;
end;

class function TVMOtaUtils.GetBrowsingPath: string;
var
  Res, TempPaths, CurPath: string;
begin
  Result := '';
  if (GetEnvironmentOptions <> nil) then
  try
    Res := '';
    TempPaths := GetEnvironmentOptions.Values['BrowsingPath'];
    for CurPath in TStrUtils.Words(TempPaths, [';']) do
      Res := Res + NormalizeDelphiPath(CurPath) + ';';

    Result := Res;
  except
  end;
end;

class function TVMOtaUtils.GetBrowsingPathList: IEnumerable<string>;
begin
  Result := TStrUtils.Words(GetBrowsingPath, [';']);
end;

class function TVMOtaUtils.GetProjectPaths: string;
begin
  Result := '';
  if GetProjectOptions <> nil then
  try
    Result := LowerCase(GetProjectOptions.Values['SrcDir']);
  except
  end;
end;

class function TVMOtaUtils.GetProjectPathsList: IEnumerable<string>;
begin
  Result := NormalizePaths(GetActiveProject, TStrUtils.Words(GetProjectPaths, [';']));
end;

class function TVMOtaUtils.NormalizePaths(aProject: IOTAProject; Paths: IEnumerable<string>): IEnumerable<string>;
var
  ProjectPath: string;
begin
  if aProject <> nil then
    ProjectPath := IncludeTrailingBackslash(ExtractFilePath(aProject.FileName))
  else
    ProjectPath := '';

  Result := Pipeline<string>.From(Paths)
      .Map<string>(function (aPath: string): string
          begin
            Result := NormalizeDelphiPath(aPath);
            if (ProjectPath <> '') and IsRelativePath(Result) then
              Result := ExpandFileName(ProjectPath + Result);
          end)
      .Enum;
end;

class function TVMOtaUtils.GetProjectGroupsPathsList: IEnumerable<string>;
var
  ModuleServices: IOTAModuleServices;
  Group: IOTAProjectGroup;
  Project: IOTAProject;
  ProjOptions: IOTAProjectOptions;
  I: Integer;
  Paths: ISet<string>;
begin
  Paths := THashSet<string>.Create;
  Result := Paths;
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    Exit;

  Group := ModuleServices.GetMainProjectGroup;
  if Group = nil then
    Exit;

  for I := 0 to Group.ProjectCount - 1 do
  begin
    Project := Group.Projects[I];
    if Project = nil then
      Exit;

    ProjOptions := Project.ProjectOptions;
    if ProjOptions <> nil then
      Paths.Add(NormalizePaths(Project, TStrUtils.Words(LowerCase(ProjOptions.Values['SrcDir']), [';'])));
  end;
end;

class function TVMOtaUtils.GetProjectGroupFileList: IEnumerable<string>;
var
  ModuleServices: IOTAModuleServices;
  Group: IOTAProjectGroup;
  Project: IOTAProject;
  I: Integer;
  List: TStringList;
  Files: ISet<string>;
  ListEnum: IEnumerable<string>;
begin
  Files := THashSet<string>.Create;
  Result := Files;
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    Exit;

  Group := ModuleServices.GetMainProjectGroup;
  if Group = nil then
    Exit;

  List := TStringList.Create;
  try
    ListEnum := TCollectionsUtils.Wrap(List);
    for I := 0 to Group.ProjectCount - 1 do
    begin
      Project := Group.Projects[I];
      if Project = nil then
        Exit;

      List.Clear;
      Project.GetCompleteFileList(List);
      Files.Add(NormalizePaths(Project, ListEnum));
    end;
  finally
    ListEnum := nil;
    FreeAndNil(List);
  end;
end;

class function TVMOtaUtils.GetActiveProjectFileList: IEnumerable<string>;
var
  Project: IOTAProject;
  List: IObjectHolder<TStringList>;
begin
  Result := nil;
  List := TObjectHolder<TStringList>.Create(TStringList.Create, True);
  Project := GetActiveProject;
  if Project <> nil then
  begin
    Project.GetCompleteFileList(List.Obj);
    Result := NormalizePaths(Project, TCollectionsUtils.Wrap(List));
  end
  else
    Result := TCollectionsUtils.Empty<string>;
end;

class function TVMOtaUtils.GetProjectFileList(aFromProjectGroup: Boolean): IEnumerable<string>;
begin
  if aFromProjectGroup then
    Result := GetProjectGroupFileList
  else
    Result := GetActiveProjectFileList;
end;

class function TVMOtaUtils.GetActiveProject: IOTAProject;
var
  ModuleServices: IOTAModuleServices;
  {$ifdef VER130}
  i: Integer;
  Dummy: IUnknown;
  {$endif}
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices <> nil then
  begin
    {$ifndef VER130}
    Result := ModuleServices.GetActiveProject
    {$else}
    for i := 0 to ModuleServices.ModuleCount - 1 do
      if ModuleServices.Modules[i].QueryInterface(IOTAProjectGroup, Dummy) = S_OK then
      begin
        Result := (ModuleServices.Modules[i] as IOTAProjectGroup).ActiveProject;
        exit;
      end;
    for i := 0 to ModuleServices.ModuleCount - 1 do
      if ModuleServices.Modules[i].QueryInterface(IOTAProject, Dummy) = S_OK then
      begin
        Result := ModuleServices.Modules[i] as IOTAProject;
        exit;
      end;
    {$endif}
  end
  else
    Result := nil;
end;

class function TVMOtaUtils.GetProjectOptions: IOTAProjectOptions;
var
  OTAProject: IOTAProject;
begin
  Result := nil;
  if (BorlandIDEServices <> nil) then
  try
    OTAProject := GetActiveProject;
    if OTAProject <> nil then
      Result := OTAProject.ProjectOptions;
  except
  end
end;

class function TVMOtaUtils.GetEnvironmentOptions: IOTAEnvironmentOptions;
var
  OTAServices: IOTAServices;
begin
  Result := nil;
  if (BorlandIDEServices <> nil) then
  try
    OTAServices := BorlandIDEServices as IOTAServices;
    if OTAServices <> nil then
      Result := OTAServices.GetEnvironmentOptions;
  except
  end
end;

class function TVMOtaUtils.GetTopEditView: IOTAEditView;
var
  EdSrv: IOTAEditorServices;
begin
  try
    EdSrv := BorlandIDEServices as IOTAEditorServices;
    if Assigned(EdSrv) then
      Result := EdSrv.TopView;
  except
    Result := nil;
  end;
end;

class function TVMOtaUtils.GetWordUnderCursorInCurView: string;
var
  EditView: IOTAEditView;
  Reader: IOTAEditReader;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  Pos: Longint;
  StartPos: Longint;
  Buffer: AnsiString;
begin
  Result := '';

  EditView := GetTopEditView;
  if nil = EditView then
    Exit;

  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True, EditPos, CharPos);
  Pos := EditView.CharPosToPos(CharPos);

  if Pos < 0 then
    Exit;

  Reader := EditView.Buffer.CreateReader;

  if nil = Reader then
    Exit;

  SetLength(Buffer, 512);
  StartPos := Pos - 256;
  if StartPos < 0 then
    StartPos := 0;
  SetLength(Buffer, Reader.GetText(StartPos, PAnsiChar(Buffer), 512));
  Pos := Pos - StartPos + 1;
  StartPos := Pos;
  while (StartPos > 1) and (Buffer[StartPos - 1] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) do
    Dec(StartPos);
  while (Pos <= Length(Buffer)) and (Buffer[Pos] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) do
    Inc(Pos);
  Result := Copy(Buffer, StartPos, Pos - StartPos);
end;

class function TVMOtaUtils.GetCurrentOpenFileName: string;
var
  ModServicies: IOTAModuleServices;
begin
  Result:= '';
  ModServicies:= BorlandIDEServices as IOTAModuleServices;
  if (ModServicies <> nil) and (ModServicies.CurrentModule <> nil) then
    Result:= ModServicies.CurrentModule.FileName
end;

class function TVMOtaUtils.GetCurrentOpenProjectFileName: string;
var
  Proj: IOTAProject;
begin
  Proj := GetActiveProject;
  if Proj <> nil then
    Result := Proj.FileName
  else
    Result := '';
end;

class function TVMOtaUtils.GetWordUnderCursorInCurView2: string;
var
  iView : IOTAEditView;
begin
  iView := GetTopEditView;
  if Assigned(iView) and (iView.Block <> nil) then
    Result:= iView.Block.Text
  else
    Result:= '';

  if Result = '' then
    Result:= GetWordUnderCursorInCurView;
end;


{ TGXOtaUtils }

class function TGXOtaUtils.GxOtaConvertColumnCharsToBytes(LineData: UTF8String; CharIndex: Integer;
    EndByte: Boolean): Integer;
var
  UString: string;
  FinalUChar: string;
  UTF8Str: UTF8String;
begin
  UString := UTF8ToUnicodeString(LineData);
  UString := Copy(UString, 1, CharIndex);
  UTF8Str := UTF8String(UTF8Encode(UString));
  Result := Length(UTF8Str);
  if not EndByte then
  begin
    if Length(UString) = 0 then
      Result := 0
    else
    begin
      FinalUChar := UString[Length(UString)];
      UTF8Str := Utf8String(UTF8Encode(FinalUChar));
      Result := Result - (Length(UTF8Str)) + 1;
    end;
  end;
end;

class function TGXOtaUtils.GxOtaFileOrModuleExists(const AFileName: string; UseBase: Boolean): Boolean;
begin
  Result := FileExists(AFileName) or GxOtaIsFileOpen(AFileName, UseBase);
end;

class function TGXOtaUtils.GxOtaFocusCurrentIDEEditControl: Boolean;
var
  EditControl: TWinControl;
begin
  Result := False;
  EditControl := GxOtaGetCurrentIDEEditControl;
  TGXGenericUtils.TryFocusControl(EditControl)
end;

class function TGXOtaUtils.GxOtaGetBaseModuleFileName(const FileName: string): string;
var
  AltName: string;
begin
  Result := FileName;
  if TGXGenericUtils.IsForm(FileName) then
  begin
    if GxOtaHaveCPPSupport then
    begin
      AltName := ChangeFileExt(FileName, '.cpp');
      if GxOtaFileOrModuleExists(AltName) then
        Result := AltName;
    end;
    AltName := ChangeFileExt(FileName, '.pas');
    if GxOtaFileOrModuleExists(AltName) then
      Result := AltName;
  end;
end;

class function TGXOtaUtils.GxOtaGetCurrentIDEEditControl: TWinControl;
var
  EditView: IOTAEditView;
  EditWindow: INTAEditWindow;
  EditForm: TCustomForm;
begin
  Result := nil;
  EditView := GxOtaGetTopMostEditView;
  if Assigned(EditView) then
  begin
    EditWindow := EditView.GetEditWindow;
    if Assigned(EditWindow) then
    begin
      EditForm := EditWindow.Form;
      if Assigned(EditForm) then
        Result := TGXIdeUtils.GetIDEEditControl(EditForm);
    end;
  end;
end;

class function TGXOtaUtils.GxOtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.CurrentModule;
end;

class function TGXOtaUtils.GxOtaGetCurrentSourceEditor: IOTASourceEditor;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := nil;
  EditBuffer := GxOtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and (EditBuffer.FileName <> '') then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule, EditBuffer.FileName);
  if Result = nil then
    Result := GxOtaGetSourceEditorFromModule(GxOtaGetCurrentModule);
end;

class function TGXOtaUtils.GxOtaGetEditActionsFromModule(Module: IOTAModule): IOTAEditActions;
var
  i: Integer;
  EditView: IOTAEditView;
  SourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if Module = nil then
    Exit;
  SourceEditor := GxOtaGetSourceEditorFromModule(Module);
  if SourceEditor = nil then
    Exit;
  for i := 0 to SourceEditor.GetEditViewCount - 1 do
  begin
    EditView := SourceEditor.GetEditView(i);
    if Supports(EditView, IOTAEditActions, Result) then
      Exit;
  end;
  Result := nil;
end;

class function TGXOtaUtils.GxOtaGetEditorLine(View: IOTAEditView; LineNo: Integer): UTF8String;
const
  MaxEditorCol = High(SmallInt);
var
  Buffer: IOTAEditBuffer;
  LineStartByte: Integer;
  LineEndByte: Integer;
  Pos: TOTACharPos;
  LineData: AnsiString;
  Reader: IOTAEditReader;
  LineBytes: Integer;
begin
  Assert(Assigned(View));
  Result := '';
  Buffer := View.Buffer;
  if (LineNo > Buffer.GetLinesInBuffer) or (LineNo < 1) then
    Exit;
  Pos.CharIndex := 0;
  Pos.Line := LineNo;
  LineStartByte := View.CharPosToPos(Pos);
  Pos.CharIndex := MaxEditorCol;
  LineEndByte := View.CharPosToPos(Pos);
  Reader := Buffer.CreateReader;
  Assert(Assigned(Reader));
  LineBytes := LineEndByte - LineStartByte;
  SetLength(LineData, LineBytes);
  Reader.GetText(LineStartByte, PAnsiChar(LineData), LineBytes);
  Result := UTF8String(LineData);
end;

class function TGXOtaUtils.GxOtaGetEditorServices: IOTAEditorServices;
begin
  Result := (BorlandIDEServices as IOTAEditorServices);
  Assert(Assigned(Result));
end;

class function TGXOtaUtils.GxOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

class function TGXOtaUtils.GxOtaGetFormEditorFromModule(const Module: IOTAModule): IOTAFormEditor;
var
  i: Integer;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    Editor := GxOtaGetFileEditorForModule(Module, i);
    if Supports(Editor, IOTAFormEditor, FormEditor) then
    begin
      Assert(not Assigned(Result));
      Result := FormEditor;
      // In order to assert our assumptions that only one form
      // is ever associated with a module, do not call Break; here.
    end;
  end;
end;

class function TGXOtaUtils.GxOtaGetModule(const FileName: string): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));

  Result := ModuleServices.FindModule(FileName);
end;

class function TGXOtaUtils.GxOtaGetOpenModuleCount: Integer;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.ModuleCount;
end;

class function TGXOtaUtils.GxOtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount-1 do
  begin
    IEditor := GxOtaGetFileEditorForModule(Module, i);

    if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
    begin
      if Assigned(ISourceEditor) then
      begin
        if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then
        begin
          Result := ISourceEditor;
          Break;
        end;
      end;
    end;
  end;
end;

class function TGXOtaUtils.GxOtaGetTopMostEditView: IOTAEditView;
begin
  Result := nil;
  // Bug: Delphi 5/6 crash when calling TopView with no files open
  if GxOtaGetOpenModuleCount = 0 then
    Exit;
  Result := GxOTAGetEditorServices.TopView;
end;

class function TGXOtaUtils.GxOtaGetTopMostEditBuffer: IOTAEditBuffer;
begin
  Result := GxOTAGetEditorServices.TopBuffer;
end;

class function TGXOtaUtils.GxOtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView;
begin
  if SourceEditor = nil then
    SourceEditor := GxOtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
    Result := SourceEditor.EditViews[0]
  else
    Result := nil;
end;

class procedure TGXOtaUtils.GxOtaGoToFileLineColumn(const FileName: string; Line, StartColumn, StopColumn: Integer;
  ShowInMiddle: Boolean);
var
  EditView: IOTAEditView;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  CurPos: TOTAEditPos;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
  MatchLength: Integer;
  LineData: UTF8String;
resourcestring
  SCouldNotOpenFile = 'Could not open file %s';
begin
  // Force the source editor to show the right file (cpp, pas, dfm, xfm, etc.)
  if not GxOtaMakeSourceVisible(FileName) then
    raise Exception.CreateFmt(SCouldNotOpenFile, [FileName]);

  Module := GxOtaGetModule(FileName);
  if not Assigned(Module) then
    Exit;

  SourceEditor := GxOtaGetSourceEditorFromModule(Module, FileName);
  if not Assigned(SourceEditor) then
    Exit;

  EditView := GxOtaGetTopMostEditView(SourceEditor);
  if not Assigned(EditView) then
    Exit;

  SourceEditor.Show;
  EditView.GetEditWindow.Form.Update;
  if Line < 0 then
    Exit;

  EditView.Block.SetVisible(False);

  // Set the top line of the edit view
  CurPos.Col := 1;
  CurPos.Line := Line;
  if ShowInMiddle then
    CurPos.Line := CurPos.Line - (EditView.ViewSize.cy div 2);
  if CurPos.Line < 1 then
   CurPos.Line := 1;
  EditView.TopPos := CurPos;

  Application.ProcessMessages;

  GxOtaFocusCurrentIDEEditControl;

  LineData := GxOtaGetEditorLine(EditView, Line);
  StartColumn := GxOtaConvertColumnCharsToBytes(LineData, StartColumn, False);
  StopColumn := GxOtaConvertColumnCharsToBytes(LineData, StopColumn, True);

  // Position the cursor to the line and column of the match
  CharPos.CharIndex := StartColumn - 1;
  CharPos.Line := Line;
  EditView.ConvertPos(False, EditPos, CharPos);
  EditView.CursorPos := EditPos;
  // This is disabled since it causes crashes in D2007 jumping to matches in data modules with no opened project
  // inside EdScript.TOTAEditView.unElideNearestBlock
  //GxOtaUnfoldNearestRegion(EditView);
  EditView.CursorPos := EditPos;
  if StopColumn > StartColumn then
  begin
    EditView.Block.BeginBlock;
    MatchLength := StopColumn - StartColumn + 1;
    // This calculation doesn't work when there are tabs inside the match text (rare)
    EditPos.Col := EditPos.Col + MatchLength;
    EditView.CursorPos := EditPos;
    EditView.Block.EndBlock;
  end;
  EditView.Block.SetVisible(True);
  EditView.Paint;
end;


class function TGXOtaUtils.GxOtaHaveCPPSupport: Boolean;
begin
  Result := GxOtaIdeSupportsPersonality(sCBuilderPersonality);
end;

class function TGXOtaUtils.GxOtaIdeSupportsPersonality(const Personality: string): Boolean;
begin
  Result := TStrUtils.StringInArray(Personality, [sDelphiPersonality, sCBuilderPersonality])
end;

class function TGXOtaUtils.GxOtaIsFileOpen(const AFileName: string; UseBase: Boolean): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
  FileName: string;
begin
  Result := False;

  if IsStandAlone then
    Exit;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  FileName := AFileName;
  if UseBase then
    FileName := GxOtaGetBaseModuleFileName(FileName);

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for i := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := GxOtaGetFileEditorForModule(Module, i);
      Assert(Assigned(FileEditor));

      Result := SameFileName(FileName, FileEditor.FileName);
      if Result then
        Exit;
    end;
  end;
end;

class function TGXOtaUtils.GxOtaMakeSourceVisible(const FileName: string): Boolean;
var
  EditActions: IOTAEditActions;
  Module: IOTAModule;
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  FileEditor: IOTAEditor;
  i: Integer;
  BaseFileName: string;
begin
  BaseFileName := GxOtaGetBaseModuleFileName(FileName);
  Module := GxOtaGetModule(BaseFileName);
  if Module = nil then
    Module := GxOtaGetModule(FileName);
  if Module = nil then
    Module := GxOtaGetModule(ChangeFileExt(FileName, '.dfm'));

  if Module <> nil then
  begin
    if TGXGenericUtils.IsForm(FileName) then
    begin
      if not GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module, BaseFileName);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
        begin
          FormEditor := GxOtaGetFormEditorFromModule(Module);
          FormEditor.Show;
          EditActions.SwapSourceFormView;
        end;
      end
    end
    else // We are focusing a regular text file, not a form
    begin
      if GxOtaModuleIsShowingFormSource(Module) then
      begin
        SourceEditor := GxOtaGetSourceEditorFromModule(Module);
        if Assigned(SourceEditor) then
          SourceEditor.Show;
        SourceEditor := nil;
        EditActions := GxOtaGetEditActionsFromModule(Module);
        if EditActions <> nil then
          EditActions.SwapSourceFormView;
      end;
    end;
  end;

  // D5/BDS 2006 sometimes delay opening the file until messages are processed
  Application.ProcessMessages;

  if not (GxOtaIsFileOpen(BaseFileName) or GxOtaIsFileOpen(FileName)) then
    Result := GxOtaOpenFile(FileName)
  else
    Result := True;

  if Result then
  begin
    {$IFDEF VER160}
    // Delphi 8 can not open both the module and the form text, so stop here
    if IsForm(FileName) then
      Exit;
    {$ENDIF VER160}
    Module := GxOtaGetModule(BaseFileName);
    if Module = nil then
      Module := GxOtaGetModule(FileName);
    if Module <> nil then
    begin
      for i := 0 to Module.GetModuleFileCount-1 do
      begin
        FileEditor := Module.GetModuleFileEditor(i);
        Assert(Assigned(FileEditor));

        if SameFileName(FileEditor.FileName, FileName) then
        begin
          FileEditor.Show;
          Exit;
        end;
      end;
    end;
    Result := False;
  end;
end;

class function TGXOtaUtils.GxOtaModuleIsShowingFormSource(Module: IOTAModule): Boolean;
var
  Editor: IOTAEditor;
  i: Integer;
begin
  Result := False;
  if not Assigned(Module) then
    Exit;
  for i := 0 to Module.GetModuleFileCount - 1 do
  begin
    Editor := Module.ModuleFileEditors[i];
    if Assigned(Editor) and TGXGenericUtils.IsForm(Editor.FileName) and Supports(Editor, IOTASourceEditor) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TGXOtaUtils.GxOtaOpenFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
begin
  Result := False;
  if FileName = '' then
    Exit;

  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  Result := ActionServices.OpenFile(FileName);
end;

class function TGXOtaUtils.IsStandAlone: Boolean;
begin
  Result := (BorlandIDEServices = nil);
end;

{ TGXGenericUtils }

class function TGXGenericUtils.IsForm(const FileName: string): Boolean;
begin
  Result := TStrUtils.FileMatchesExtensions(FileName, ['.dfm', '.xfm', '.nfm', '.fmx']);
end;

class function TGXGenericUtils.TryFocusControl(Control: TWinControl): Boolean;
begin
  Result := False;
  if Assigned(Control) then begin
    if Control.CanFocus and Control.Visible then begin
      try
        Control.SetFocus;
        Result := True;
      except //FI:W501
        // Ignore focus errors
      end;
    end;
  end;
end;

{ TGXIdeUtils }

class function TGXIdeUtils.GetIDEEditControl(Form: TCustomForm): TWinControl;
const
  EditorControlName = 'Editor';
var
  Component: TComponent;
begin
  Assert(Assigned(Form));
  Result :=  nil;
  Component := (Form.FindComponent(EditorControlName) as TWinControl);
  if Assigned(Component) then
    if Component is TWinControl then
      Result := Component as TWinControl;
end;


class function TGXIdeUtils.GetIdeMainForm: TCustomForm;
begin
  Assert(Assigned(Application));
  Result := Application.FindComponent('AppBuilder') as TCustomForm;
end;

{ TVMOtaUtils }

class function TVMOtaUtils.GetIdeActionList: TCustomActionList;
var
  NTAServices: INTAServices;
begin
  Assert(Assigned(BorlandIDEServices));
  NTAServices := BorlandIDEServices as INTAServices;

  Assert(Assigned(NTAServices));
  Result := NTAServices.ActionList;

  Assert(Assigned(Result));
end;

class function TVMOtaUtils.GetKeyboardServices: IOTAKeyboardServices;
begin
  Result := BorlandIDEServices as IOTAKeyboardServices;
end;

end.
