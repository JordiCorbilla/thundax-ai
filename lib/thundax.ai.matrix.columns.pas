unit thundax.ai.matrix.columns;

interface

type
  TArrayDouble = array of Double;

  TColumn = class(TObject)
  private
    FValues: TArrayDouble;
    procedure SetValues(const Value: TArrayDouble);
  public
    constructor Create(values: array of Double);
    property Values : TArrayDouble read FValues write SetValues;
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

procedure TColumn.SetValues(const Value: TArrayDouble);
begin
  FValues := Value;
end;

end.
