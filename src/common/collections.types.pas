unit collections.types;

interface

type
  TMapper<T, R> = reference to function (const aValue: T): R;
  TForEachAction<T> = reference to procedure (const aValue: T);

implementation

end.
