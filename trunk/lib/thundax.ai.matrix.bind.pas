unit thundax.ai.matrix.bind;

interface

uses
  thundax.ai.matrix.columns, thundax.ai.matrix, thundax.ai.matrix.arraytypes;

type
  IBind = interface
    function matrix() : IMatrix;
  end;

  TBind = class(TInterfacedObject, IBind)
  private
    Fcolumns: TArrayColumns;
    procedure Setcolumns(const Value: TArrayColumns);
  public
    constructor Create(values : array of IColumn);
    class function New(values : array of IColumn) : IBind;
    function matrix() : IMatrix;
    property columns : TArrayColumns read Fcolumns write Setcolumns;
  end;
implementation

uses
  SysUtils;

{ TBind }

constructor TBind.Create(values: array of IColumn);
var
  i : integer;
begin
  SetLength(Fcolumns, High(values)+1);
  for I := 0 to High(values) do
    Fcolumns[i] := values[i];
end;

function TBind.matrix: IMatrix;
begin
  result := TMatrix.Create(Self.Fcolumns);
end;

class function TBind.New(values: array of IColumn): IBind;
var
  i : integer;
  Size, initialSize : integer;
  bind : IBind;
begin
  bind := Create(values);
  Size := 0;
  initialSize := 0;
  for I := 0 to High(values) do
  begin
    if Size = 0 then
    begin
      Size := High(values[i].Values);
      initialSize := Size;
    end
    else
    begin
      Size := High(values[i].Values);
      if Size <> initialSize then
        raise Exception.Create('Columns does not have the same dimension');
    end;
  end;
  if Size = 0 then
    raise Exception.Create('Size is zero');
  //Check that the size of every column is exactly the same
  result := bind;
end;

procedure TBind.Setcolumns(const Value: TArrayColumns);
begin
  Fcolumns := Value;
end;

end.
