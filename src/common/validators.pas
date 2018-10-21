unit validators;
{$I cond_define.inc}
interface

type
  TEditValidator = class(TObject)
  public
    constructor Create;
    function ValidInput(var ValidText: string): Boolean; virtual;
  end;

  TIntegerValidator = class(TEditValidator)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
  public
    constructor Create;
    function ValidInput(var ValidText: string): Boolean; override;

    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
  end;

  TSingleValidator = class(TEditValidator)
  private
    FMinValue: Single;
    FMaxValue: Single;
  public
    constructor Create;
    function ValidInput(var ValidText: string): Boolean; override;

    property MinValue: Single read FMinValue write FMinValue;
    property MaxValue: Single read FMaxValue write FMaxValue;
  end;

implementation

uses
  SysUtils, Math;


{ TEditValidator }

constructor TEditValidator.Create;
begin
  inherited Create;
end;

function TEditValidator.ValidInput(var ValidText: string): Boolean;
begin
  Result := True;
end;

{ TIntegerValidator }

constructor TIntegerValidator.Create;
begin
  inherited;
  MinValue := Low(Integer);
  MaxValue := High(Integer);
end;

function TIntegerValidator.ValidInput(var ValidText: string): Boolean;
var
  CurV: Integer;
begin
  Result := True;
  if (ValidText = '') or (Length(ValidText) = 1) and (ValidText[1] = '-') then
    Exit;
  try
    Result := TryStrToInt(ValidText, CurV) and (CurV >= MinValue) and (CurV <= MaxValue);
  except
    on EConvertError do
      Result := False;
  end;
end;

{ TSingleValidator }

constructor TSingleValidator.Create;
begin
  inherited Create;
  FMinValue := -MaxSingle;
  FMaxValue := MaxSingle;
end;

function TSingleValidator.ValidInput(var ValidText: string): Boolean;
var
  CurV: Single;
begin
  Result := True;
  if (ValidText = '') or (Length(ValidText) = 1) and (ValidText[1] = '-') then
    Exit;
  try
    Result := TryStrToFloat(ValidText, CurV) and (CurV >= MinValue) and (CurV <= MaxValue);
  except
    on EConvertError do
      Result := False;
  end;
end;

end.
