// Copyright (c) 2012-2015, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit thundax.ai.matrix;

interface

uses
  thundax.ai.matrix.arraytypes, Generics.collections, Contnrs, thundax.ai.vector;

type
  TMultiArray<T> = Array of array of T;

  IMatrix = interface
    function GetCell(x, y: Integer): Double;
    procedure SetCell(x, y: Integer; const Value: Double);
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    function GetRows(): Integer;
    procedure SetRows(Value: Integer);
    function ToString(): string;
    function Transpose(): IMatrix;
    property Cell[x, y: Integer]: Double read GetCell write SetCell;
    property Rows: Integer read GetRows write SetRows;
    property Columns: Integer read GetColumns write SetColumns;
    function Add(matrix: IMatrix): IMatrix;
    function Subtract(matrix: IMatrix): IMatrix;
    function Multiply(matrix: IMatrix): IMatrix; overload;
    function Multiply(vector : IVector<Double>) : IVector<Double>; overload;
    function Mean(): IMatrix;
    function Covariance(): IMatrix;
    function Standardisation() : IMatrix;
    function Distance() : IMatrix;
    function GetCovarianceValue(Column, Row: Integer): Double;
    function GetDistance(Column, Row : Integer) : Double;
    function Identity() : IMatrix;
    function Clone() : IMatrix;
  end;

  TMatrix = class(TInterfacedObject, IMatrix)
  private
    FColumns: Integer;
    FRows: Integer;
    FMultiArray: Array of array of Double;
    function GetCell(x, y: Integer): Double;
    procedure SetCell(x, y: Integer; const Value: Double);
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    function GetRows(): Integer;
    procedure SetRows(Value: Integer);
  public
    constructor Create(Columns, Rows: Integer); overload;
    constructor Create(Columns: TArrayColumns); overload;
    constructor Create(filename : string; delimiter : char); overload;
    procedure Initialise();
    property Columns: Integer read GetColumns write SetColumns;
    property Rows: Integer read GetRows write SetRows;
    function Transpose(): IMatrix;
    property Cell[x, y: Integer]: Double read GetCell write SetCell;
    function Add(matrix: IMatrix): IMatrix;
    function Subtract(matrix: IMatrix): IMatrix;
    function Multiply(matrix: IMatrix): IMatrix; overload;
    function Multiply(vector : IVector<Double>) : IVector<Double>; overload;
    function Mean(): IMatrix;
    function GetCovarianceValue(Column, Row: Integer): Double;
    function GetDistance(Column, Row : Integer) : Double;
    function Covariance(): IMatrix;
    function Standardisation() : IMatrix;
    function Distance() : IMatrix;
    function Identity() : IMatrix;
    function Clone() : IMatrix;
    function ToString(): string; override;
  end;

implementation

uses
  SysUtils, thundax.ai.matrix.Columns, thundax.ai.Maths, thundax.ai.layout.text,
  System.Classes;

{ TMatrix }

constructor TMatrix.Create(Columns, Rows: Integer);
var
  i: Integer;
begin
  FRows := Rows;
  FColumns := Columns;
  SetLength(FMultiArray, FColumns);
  for i := 0 to FColumns - 1 do
    SetLength(FMultiArray[i], FRows);
end;

function TMatrix.Add(matrix: IMatrix): IMatrix;
var
  i, j: Integer;
  newMatrix: IMatrix;
begin
  if (Self.FRows <> matrix.Rows) or (Self.FColumns <> matrix.Columns) then
    raise Exception.Create('Dimension matrix are different');

  newMatrix := TMatrix.Create(matrix.Columns, matrix.Rows);
  for i := 0 to FColumns - 1 do
    for j := 0 to FRows - 1 do
      newMatrix.Cell[i, j] := Self.Cell[i, j] + matrix.Cell[i, j];

  result := newMatrix;
end;

function TMatrix.Clone: IMatrix;
var
  i, j: Integer;
  newMatrix: IMatrix;
begin
  newMatrix := TMatrix.Create(FColumns, FColumns);

  for i := 0 to newMatrix.Columns - 1 do
  begin
    for j := 0 to newMatrix.Rows - 1 do
    begin
      newMatrix.Cell[i, j] := self.Cell[i,j];
    end;
  end;
  result := newMatrix;
end;

function TMatrix.Covariance: IMatrix;
var
  i, j: Integer;
  newMatrix, meanMatrix: IMatrix;
  cov: Double;
begin
  newMatrix := TMatrix.Create(FColumns, FColumns);
  meanMatrix := Self.Mean;

  for i := 0 to newMatrix.Columns - 1 do
  begin
    for j := 0 to newMatrix.Rows - 1 do
    begin
      cov := meanMatrix.GetCovarianceValue(i, j);
      newMatrix.Cell[i, j] := cov;
    end;
  end;
  result := newMatrix;
end;

constructor TMatrix.Create(filename: string; delimiter : char);
var
   myFile : TextFile;
   LineText   : string;
   OutPutList: TStringList;
   i, j: Integer;
begin
  AssignFile(myFile, filename);
  Reset(myFile);
  while not Eof(myFile) do
  begin
    ReadLn(myFile, LineText);
    OutPutList := TStringList.Create;
    OutPutList.Clear;
    OutPutList.Delimiter := delimiter;
    OutputList.StrictDelimiter := true;
    OutputList.DelimitedText := LineText;
    SetLength(FMultiArray, OutPutList.count);
    FColumns := OutPutList.count;
    for i := 0 to OutPutList.Count -1 do
    begin
      SetLength(FMultiArray[i], Length(FMultiArray[i])+1);
    end;
    for j := 0 to OutPutList.Count - 1 do
      SetCell(j,Length(FMultiArray[j])-1, OutPutList[j].ToDouble());
    OutPutList.Free;
  end;
  FRows := Length(FMultiArray[0]);
end;

constructor TMatrix.Create(Columns: TArrayColumns);
var
  i, j: Integer;
begin
  FRows := High(Columns[0].Values) + 1;
  FColumns := High(Columns) + 1;
  SetLength(FMultiArray, FColumns);
  for i := 0 to FColumns - 1 do
    SetLength(FMultiArray[i], FRows);

  for i := 0 to FColumns - 1 do
    for j := 0 to FRows - 1 do
      SetCell(i, j, Columns[i].Values[j]);
end;

function TMatrix.Distance: IMatrix;
var
  i, j, k : Integer;
  newMatrix : IMatrix;
  dist : double;
begin
  newMatrix := TMatrix.Create(FRows-1, FRows-1);
  k := 0;
  for i := 0 to FColumns-1 do
  begin
    Inc(k);
    j := k;
    while j < FRows do
    begin
      dist := GetDistance(i, j);
      newMatrix.Cell[i, j-1] := dist;
      Inc(j);
    end;
  end;
  result := newMatrix;
end;

function TMatrix.GetCell(x, y: Integer): Double;
begin
  result := FMultiArray[x, y];
end;

function TMatrix.GetColumns: Integer;
begin
  result := FColumns;
end;

function TMatrix.GetCovarianceValue(Column, Row: Integer): Double;
var
  i: Integer;
  sum, cov: Double;
begin
  sum := 0;
  for i := 0 to FRows - 1 do
  begin
    sum := sum + (Self.Cell[Column, i] * Self.Cell[Row, i]);
  end;
  cov := sum / (FRows - 1);
  result := cov;
end;

function TMatrix.GetDistance(Column, Row: Integer): Double;
var
  i: Integer;
  sum, dist: Double;
begin
  sum := 0;
  for i := 0 to FColumns - 1 do
  begin
    sum := sum + Sqr((Self.Cell[i, Column] - Self.Cell[i, Row]));
  end;
  dist := Sqrt(sum);
  result := dist;
end;

function TMatrix.GetRows: Integer;
begin
  result := FRows;
end;

function TMatrix.Identity: IMatrix;
var
  i, j : integer;
begin
  for i := 0 to FColumns - 1 do
  begin
    for j := 0 to FRows - 1 do
    begin
        Self.Cell[i, j] := 1.0;
    end;
  end;

  result := Self;
end;

procedure TMatrix.Initialise;
var
  i: Integer;
begin
  for i := 0 to High(FMultiArray) - 1 do
  begin
    FMultiArray[0, i] := i;
    FMultiArray[i, 0] := i;
  end;
end;

function TMatrix.Mean(): IMatrix;
var
  i, j: Integer;
  sum, Mean: Double;
  newMatrix: IMatrix;
begin
  newMatrix := TMatrix.Create(FColumns, FRows);
  for i := 0 to FColumns - 1 do
  begin
    sum := 0;
    for j := 0 to FRows - 1 do
    begin
      sum := sum + Self.Cell[i, j]
    end;
    Mean := sum / FRows;
    for j := 0 to newMatrix.Rows - 1 do
    begin
      newMatrix.Cell[i, j] := Self.Cell[i, j] - Mean;
    end;
  end;
  result := newMatrix;
end;

function TMatrix.Multiply(vector: IVector<Double>): IVector<Double>;
var
  resvector: IVector<double>;
  i, j: Integer;
begin
  if (Self.FColumns <> vector.Length) then
    raise Exception.Create('Dimension matrix are different');
  resvector := TVector<double>.create(Self.FColumns);
  for i := 0 to FRows - 1 do
  begin
    for j := 0 to FColumns - 1 do
    begin
        resvector.Cell[i] := resvector.Cell[i] + (Self.Cell[i, j] * vector.Cell[j]);
    end;
  end;
  result := resvector;
end;

function TMatrix.Multiply(matrix: IMatrix): IMatrix;
var
  i, j, k: Integer;
  newMatrix: IMatrix;
  sum: Double;
begin
  if (Self.FRows <> matrix.Rows) or (Self.FColumns <> matrix.Columns) then
    raise Exception.Create('Dimension matrix are different');

  newMatrix := TMatrix.Create(matrix.Columns, matrix.Rows);
  for i := 0 to FColumns - 1 do
  begin
    for j := 0 to matrix.Rows - 1 do
    begin
      sum := 0;
      for k := 0 to FRows - 1 do
      begin
        sum := sum + Self.Cell[k, j] * matrix.Cell[i, k];
      end;
      newMatrix.Cell[i, j] := sum;
    end;
  end;

  result := newMatrix;
end;

procedure TMatrix.SetCell(x, y: Integer; const Value: Double);
begin
  FMultiArray[x, y] := Value;
end;

procedure TMatrix.SetColumns(Value: Integer);
begin
  FColumns := Value;
end;

procedure TMatrix.SetRows(Value: Integer);
begin
  FRows := Value;
end;

function TMatrix.Standardisation: IMatrix;
var
  average : array of double;
  std : array of double;
  standard : IMatrix;
  i, j : integer;
begin
  //Normalization: substract mean and divide by std deviation
  standard := TMatrix.Create(Self.FColumns, Self.FRows);
  SetLength(average, FColumns);
  SetLength(std, FColumns);
  for i := 0 to FColumns - 1 do
  begin
    average[i] := 0.0;
    std[i] := 0.0;
  end;

  //Calculate the average
  for i := 0 to FColumns - 1 do
  begin
    for j := 0 to FRows - 1 do
    begin
      average[i] := average[i] + Self.Cell[i, j];
    end;
    average[i] := average[i] / (FRows);
  end;

  //Calculate the Standard deviation
  for i := 0 to FColumns - 1 do
  begin
    for j := 0 to FRows - 1 do
    begin
      std[i] := std[i] + sqr(average[i] - Self.Cell[i, j]);
    end;
    std[i] := sqrt(std[i] / (FRows));
  end;

  //Recalculate the content of the properties
  for i := 0 to FColumns - 1 do
  begin
    for j := 0 to FRows - 1 do
    begin
      standard.Cell[i, j] := (Self.Cell[i, j] - average[i]) / std[i];
    end;
  end;
  result := standard;
end;

function TMatrix.Subtract(matrix: IMatrix): IMatrix;
var
  i, j: Integer;
  newMatrix: IMatrix;
begin
  if (Self.FRows <> matrix.Rows) or (Self.FColumns <> matrix.Columns) then
    raise Exception.Create('Dimension matrix are different');

  newMatrix := TMatrix.Create(matrix.Columns, matrix.Rows);
  for i := 0 to FColumns - 1 do
    for j := 0 to FRows - 1 do
      newMatrix.Cell[i, j] := Self.Cell[i, j] - matrix.Cell[i, j];

  result := newMatrix;
end;

function TMatrix.ToString: string;
begin
  Result := TMatrixLayoutDouble.New.FormatText(Self.FRows, Self.FColumns, FMultiArray);
end;

function TMatrix.Transpose: IMatrix;
var
  trasposed: IMatrix;
  i, j: Integer;
begin
  trasposed := TMatrix.Create(Self.FRows, Self.FColumns);
  for i := 0 to Self.FRows - 1 do
    for j := 0 to Self.FColumns - 1 do
      trasposed.Cell[i, j] := FMultiArray[j, i];
  result := trasposed;
end;

end.
