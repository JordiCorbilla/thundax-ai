unit thundax.ai.matrix.columns;

interface

type
  TArrayDouble = array of Double;

  IColumn = interface
    procedure SetValues(const Value: TArrayDouble);
    function GetValues() : TArrayDouble;
    property Values : TArrayDouble read GetValues write SetValues;
  end;

  TColumn = class(TInterfacedObject, IColumn)
  private
    FValues: TArrayDouble;
    procedure SetValues(const Value: TArrayDouble);
    function GetValues() : TArrayDouble;
  public
    constructor Create(values: array of Double);
    property Values : TArrayDouble read GetValues write SetValues;
  end;

implementation

{ TColumn }

constructor TColumn.Create(values: array of Double);
var
  i : integer;
begin
  SetLength(FValues, High(values)+1);
  for I := 0 to High(values) do
    FValues[i] := values[i];
end;

function TColumn.GetValues: TArrayDouble;
begin
  Result := FValues;
end;

procedure TColumn.SetValues(const Value: TArrayDouble);
begin
  FValues := Value;
end;

end.
