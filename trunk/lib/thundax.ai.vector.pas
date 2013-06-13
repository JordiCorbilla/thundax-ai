unit thundax.ai.vector;

interface

type
  IVector<T> = interface
    function GetCell(x: Integer): T;
    procedure SetCell(x: Integer; const Value: T);
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    property Cell[x: Integer]: T read GetCell write SetCell;
  end;

  TVector<T> = class(TInterfacedObject, IVector<T>)
  private
    FColumns: Integer;
    FMultiArray: Array of T;
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    function GetCell(x: Integer): T;
    procedure SetCell(x: Integer; const Value: T);
  public
    property Cell[x: Integer]: T read GetCell write SetCell;
    property Columns: Integer read GetColumns write SetColumns;
    constructor Create(Columns: Integer); overload;
  end;

implementation

{ TMatrix }

constructor TVector<T>.Create(Columns: Integer);
begin
  FColumns := Columns;
  SetLength(FMultiArray, FColumns);
end;

function TVector<T>.GetCell(x: Integer): T;
begin
  result := FMultiArray[x];
end;

function TVector<T>.GetColumns: Integer;
begin
  result := FColumns;
end;

procedure TVector<T>.SetCell(x: Integer; const Value: T);
begin
  FMultiArray[x] := Value;
end;

procedure TVector<T>.SetColumns(Value: Integer);
begin
  FColumns := Value;
end;

end.
