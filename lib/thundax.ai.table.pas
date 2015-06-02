// Copyright (c) 2013, Jordi Corbilla
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

unit thundax.ai.table;

interface

uses
  Classes;

type
  TMultiArrayString = array of array of string;

  TListIds = class(TStringList)
    function ExistsID(id: string): Boolean;
  end;

  ITable = interface
    function GetCell(col, row: Integer): String;
    procedure SetCell(col, row: Integer; const Value: String);
    property Cell[col, row: Integer]: string read GetCell write SetCell;
    function filter(ColumnName: string; Value: string; operation: string): ITable;
    function ToString(): string;
  end;

  //This class builds a table in memory using a matrix.
  //Then it uses a filter method to perform a query on the original data and return
  //a new table.
  TTable = class(TInterfacedObject, ITable)
  private
    FColumns: Integer;
    FRows: Integer;
    FMultiArray: TMultiArrayString;
    function GetCell(col, row: Integer): String;
    procedure SetCell(col, row: Integer; const Value: String);
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    function GetRows(): Integer;
    procedure SetRows(Value: Integer);
  public
    property Columns: Integer read GetColumns write SetColumns;
    property Rows: Integer read GetRows write SetRows;
    property Cell[col, row: Integer]: string read GetCell write SetCell;
    constructor Create(Columns, Rows: Integer);
    function ToString(): string; override;
    function filter(ColumnName: string; Value: string; operation: string): ITable;
  end;

implementation

uses
  SysUtils, thundax.ai.maths, thundax.ai.layout.text;

{ TTable }

constructor TTable.Create(Columns, Rows: Integer);
var
  i: Integer;
begin
  FRows := Rows;
  FColumns := Columns;
  SetLength(FMultiArray, FColumns);
  for i := 0 to FColumns - 1 do
    SetLength(FMultiArray[i], FRows);
end;

//Filter method based on the attributes.
function TTable.filter(ColumnName, Value, operation: string): ITable;
var
  i, j, p, k: Integer;
  found: Boolean;
  Column: Integer;
  countRows: Integer;
  table: ITable;
  listIds: TListIds;
begin
  // Locate column
  i := 0;
  found := false;
  while (not found) and (i < Self.FColumns) do
  begin
    found := FMultiArray[i, 1] = ColumnName;
    Inc(i);
  end;

  if not found then
    raise Exception.Create('Column not found ' + ColumnName);

  // This is the column to loop through
  Column := i - 1;
  countRows := 2; // We need to add the column names as well. Starting from 1.

  listIds := TListIds.Create;
  listIds.Add(IntToStr(0));
  listIds.Add(IntToStr(1));
  try
    for i := 2 to Self.FRows - 1 do
    begin
      if operation = '=' then
      begin
        if FMultiArray[Column, i] = Value then
        begin
          countRows := countRows + 1;
          listIds.Add(IntToStr(i));
        end;
      end
      else
      begin
        if TMathHelper.Compare(StrToFloat(FMultiArray[Column, i]), StrToFloat(Value), operation) then
        begin
          countRows := countRows + 1;
          listIds.Add(IntToStr(i));
        end;
      end;
    end;

    table := TTable.Create(Self.FColumns - 1, countRows); // new table, with one less column and new number of rows
    k := 0;
    for i := 0 to Self.FRows - 1 do
    begin
      if (listIds.ExistsID(IntToStr(i))) then
      begin
        for j := 0 to Self.FColumns - 1 do
        begin
          p := j;
          if j <> Column then
          begin
            if j > Column then
              p := j - 1;
            table.Cell[p, k] := FMultiArray[j, i];
          end;
        end;
        Inc(k);
      end;
    end;
  finally
    FreeAndNil(listIds);
  end;
  Result := table;
end;

function TTable.GetCell(col, row: Integer): String;
begin
  Result := FMultiArray[col, row];
end;

function TTable.GetColumns: Integer;
begin
  Result := FColumns;
end;

function TTable.GetRows: Integer;
begin
  Result := FRows;
end;

procedure TTable.SetCell(col, row: Integer; const Value: String);
begin
  FMultiArray[col, row] := Value;
end;

procedure TTable.SetColumns(Value: Integer);
begin
  FColumns := Value;
end;

procedure TTable.SetRows(Value: Integer);
begin
  FRows := Value;
end;

function TTable.ToString: string;
begin
  Result := TMatrixLayoutDouble.New.FormatText(Self.FRows, Self.FColumns, FMultiArray);
end;

{ TListIds }

function TListIds.ExistsID(id: string): Boolean;
var
  i: Integer;
  found: Boolean;
begin
  found := false;
  i := 0;
  while (not found) and (i < Self.count) do
  begin
    found := Self[i] = id;
    Inc(i);
  end;
  Result := found;
end;

end.
