unit csplg_query_params;

interface

uses
  base_params, inifiles, opt_impl;

type
  TCodeSearchQueryParams = class(TBaseParams)
  private const
    cst_Reg_Section           = 'CodeSearchQueryParams';
    cst_Reg_QueryText         = 'QueryText';
    cst_Reg_FileRegExp        = 'FileRegExp';
    cst_Reg_IgnoreCase        = 'IgnoreCase';
    cst_Reg_MaxResults        = 'MaxResults';
    cst_Reg_MaxHitsPerFile    = 'MaxHitsPerFile';
    cst_Reg_InProjectPathsOnly= 'InProjectPathsOnly';
    cst_Reg_AddLines          = 'AddLines';
    cst_Reg_UseRe2            = 'UseNativeGoRegexp';
  private
    function GetFileRegExp: string;
    function GetIgnoreCase: Boolean;
    function GetInProjectPathsOnly: Boolean;
    function GetMaxHitsPerFile: Integer;
    function GetMaxResults: Integer;
    function GetQueryText: string;
    function GetAddLines: Integer;
    function GetUseRe2: Boolean;
    procedure SetFileRegExp(const Value: string);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetInProjectPathsOnly(const Value: Boolean);
    procedure SetMaxHitsPerFile(const Value: Integer);
    procedure SetMaxResults(const Value: Integer);
    procedure SetQueryText(const Value: string);
    procedure SetAddLines(const Value: Integer);
    procedure SetUseRe2(const Value: Boolean);
  protected
    procedure RegisterParams; override;
    function CreateTree: TParamsTree; override;
  public
    property QueryText: string read GetQueryText write SetQueryText;
    property FileRegExp: string read GetFileRegExp write SetFileRegExp;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property MaxResults: Integer read GetMaxResults write SetMaxResults;
    property MaxHitsPerFile: Integer read GetMaxHitsPerFile write SetMaxHitsPerFile;
    property InProjectPathsOnly: Boolean read GetInProjectPathsOnly write SetInProjectPathsOnly;
    property AddLines: Integer read GetAddLines write SetAddLines;
    property UseRe2: Boolean read GetUseRe2 write SetUseRe2;
  end;

implementation

function TCodeSearchQueryParams.CreateTree: TParamsTree;
begin
  Result := TParamsTree.Create(cst_Reg_Section, 'Query params');
end;

function TCodeSearchQueryParams.GetAddLines: Integer;
begin
  Result := (Tree.ByKey[cst_Reg_AddLines] as TIntegerParam).Value;
end;

function TCodeSearchQueryParams.GetUseRe2: Boolean;
begin
  Result := (Tree.ByKey[cst_Reg_UseRe2] as TBooleanParam).Value;
end;

function TCodeSearchQueryParams.GetFileRegExp: string;
begin
  Result := (Tree.ByKey[cst_Reg_FileRegExp] as TStringParam).Value;
end;

function TCodeSearchQueryParams.GetIgnoreCase: Boolean;
begin
  Result := (Tree.ByKey[cst_Reg_IgnoreCase] as TBooleanParam).Value;
end;

function TCodeSearchQueryParams.GetInProjectPathsOnly: Boolean;
begin
  Result := (Tree.ByKey[cst_Reg_InProjectPathsOnly] as TBooleanParam).Value;
end;

function TCodeSearchQueryParams.GetMaxHitsPerFile: Integer;
begin
  Result := (Tree.ByKey[cst_Reg_MaxHitsPerFile] as TIntegerParam).Value;
end;

function TCodeSearchQueryParams.GetMaxResults: Integer;
begin
  Result := (Tree.ByKey[cst_Reg_MaxResults] as TIntegerParam).Value;
end;

function TCodeSearchQueryParams.GetQueryText: string;
begin
  Result := (Tree.ByKey[cst_Reg_QueryText] as TStringParam).Value;
end;

procedure TCodeSearchQueryParams.RegisterParams;
begin
  inherited;

  Tree.RegisterParam(TStringParam.Create(cst_Reg_QueryText, '', [pfInvisible], ''));
  Tree.RegisterParam(TStringParam.Create(cst_Reg_FileRegExp, 'File reg exp', [pfInvisible], ''));
  Tree.RegisterParam(TBooleanParam.Create(cst_Reg_IgnoreCase, 'Ignore case', True));
  Tree.RegisterParam(TBooleanParam.Create(cst_Reg_InProjectPathsOnly, 'In project paths only', True));
  Tree.RegisterParam(TIntegerParam.Create(cst_Reg_MaxResults, 'Max results', 0));
  Tree.RegisterParam(TIntegerParam.Create(cst_Reg_MaxHitsPerFile, 'Max hits per file', 0));
  Tree.RegisterParam(TIntegerParam.Create(cst_Reg_AddLines, 'Add lines of code', 0));
  Tree.RegisterParam(TBooleanParam.Create(cst_Reg_UseRe2, 'Use Re2 engine', False));
end;

procedure TCodeSearchQueryParams.SetAddLines(const Value: Integer);
begin
  (Tree.ByKey[cst_Reg_AddLines] as TIntegerParam).Value := Value;
end;

procedure TCodeSearchQueryParams.SetUseRe2(const Value: Boolean);
begin
  (Tree.ByKey[cst_Reg_UseRe2] as TBooleanParam).Value := Value;;
end;

procedure TCodeSearchQueryParams.SetFileRegExp(const Value: string);
begin
  (Tree.ByKey[cst_Reg_FileRegExp] as TStringParam).Value := Value;;
end;

procedure TCodeSearchQueryParams.SetIgnoreCase(const Value: Boolean);
begin
  (Tree.ByKey[cst_Reg_IgnoreCase] as TBooleanParam).Value := Value;
end;

procedure TCodeSearchQueryParams.SetInProjectPathsOnly(const Value: Boolean);
begin
  (Tree.ByKey[cst_Reg_InProjectPathsOnly] as TBooleanParam).Value := Value;
end;

procedure TCodeSearchQueryParams.SetMaxHitsPerFile(const Value: Integer);
begin
  (Tree.ByKey[cst_Reg_MaxHitsPerFile] as TIntegerParam).Value := Value;
end;

procedure TCodeSearchQueryParams.SetMaxResults(const Value: Integer);
begin
  (Tree.ByKey[cst_Reg_MaxResults] as TIntegerParam).Value := Value;
end;

procedure TCodeSearchQueryParams.SetQueryText(const Value: string);
begin
  (Tree.ByKey[cst_Reg_QueryText] as TStringParam).Value := Value;
end;

end.
