unit csplg_types;

interface

uses
  generics.collections;

type
  TSearchItem = class(TObject)
  private
    FFilePath: string;
    FLineNumber: Integer;
    FFoundText: string;
    FRating: Integer;
    FRawText: string;
  public
    constructor Create(const aFilePath, aFoundText: string; aLineNumber, aRating: Integer);

    property FilePath: string read FFilePath;
    property LineNumber: Integer read FLineNumber;
    property FoundText: string read FFoundText;
    property Rating: Integer read FRating;
    property RawText: string read FRawText write FRawText;
  end;

  PSearchResults = ^TSearchResults;
  TSearchResults = class(TObject)
  private
    FItems: TList<TSearchItem>;
    function GetCount: Integer;
    function GetItems(aIndex: Integer): TSearchItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(aItem: TSearchItem);
    property Items[aIndex: Integer]: TSearchItem read GetItems; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  sysutils;

{ TSearchItem }

constructor TSearchItem.Create(const aFilePath, aFoundText: string; aLineNumber, aRating: Integer);
begin
  inherited Create;
  FFilePath := aFilePath;
  FLineNumber := aLineNumber;
  FFoundText := aFoundText;
  FRating := aRating;
end;

{ TSearchResults }

procedure TSearchResults.Add(aItem: TSearchItem);
begin
  if aItem <> nil then
    FItems.Add(aItem);
end;

constructor TSearchResults.Create;
begin
  inherited Create;
  FItems := TObjectList<TSearchItem>.Create;
end;

destructor TSearchResults.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TSearchResults.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSearchResults.GetItems(aIndex: Integer): TSearchItem;
begin
  if (aIndex >= 0) and (aIndex < FItems.Count) then
    Result := FItems[aIndex]
  else
    Result := nil;
end;

end.
