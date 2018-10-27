unit collections.sets;

interface

uses
  collections.common, generics.collections;

type
  ISet<T> = interface(IEnumerable<T>)
  ['{F9153EFF-CE30-46BF-9643-BB47A9D448BC}']
    function GetCount: Integer;

    function Add(const aItem: T): Boolean;
    function Remove(const aItem: T): Boolean;
    function Contains(const aItem: T): Boolean;

    property Count: Integer read GetCount;
  end;


  THashSet<T> = class(TEnumerableImpl<T>, IEnumerable<T>, ISet<T>)
  private type
    TVoid = record end;
  private
    FDict: TDictionary<T,TVoid>;
  protected
    function DoGetEnumerator: IEnumerator<T>; override;
  public
    constructor Create(aCapacity: Integer = 0);
    destructor Destroy; override;
    {ISet<T>}
    function GetCount: Integer;

    function Add(const aItem: T): Boolean;
    function Remove(const aItem: T): Boolean;
    function Contains(const aItem: T): Boolean;

    property Count: Integer read GetCount;
  end;
implementation

uses
  sysutils;

{ THashSet<T> }

constructor THashSet<T>.Create(aCapacity: Integer = 0);
begin
  inherited Create;
  FDict := TDictionary<T,TVoid>.Create(aCapacity);
end;

destructor THashSet<T>.Destroy;
begin
  FreeAndNil(FDict);
  inherited;
end;

function THashSet<T>.Add(const aItem: T): Boolean;
var
  V: TVoid;
begin
  FDict.AddOrSetValue(aItem, V);
end;

function THashSet<T>.Contains(const aItem: T): Boolean;
begin
  Result := FDict.ContainsKey(aItem);
end;

function THashSet<T>.DoGetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorWrapper<T>.Create(FDict.Keys);
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := FDict.Count;
end;

function THashSet<T>.Remove(const aItem: T): Boolean;
begin
  Result := FDict.ContainsKey(aItem);
  if Result then
    FDict.Remove(aItem);
end;

end.
