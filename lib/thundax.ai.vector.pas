// Copyright (c) 2012-2013, Jordi Corbilla
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

unit thundax.ai.vector;

interface

type
  IVector<T> = interface
    function GetCell(x: Integer): T;
    procedure SetCell(x: Integer; const Value: T);
    function GetColumns(): Integer;
    procedure SetColumns(Value: Integer);
    property Cell[x: Integer]: T read GetCell write SetCell;
    property Length: Integer read GetColumns write SetColumns;
    function Add(values : array of T): IVector<T>;
    function ToString(): string;
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
    class function New(Columns: Integer) : IVector<T>;
    property Cell[x: Integer]: T read GetCell write SetCell;
    property Length: Integer read GetColumns write SetColumns;
    constructor Create(Columns: Integer); overload;
    function Add(values : array of T): IVector<T>;
    function ToString(): string; override;
  end;

implementation

uses
  thundax.ai.layout.text;

{ TMatrix }

function TVector<T>.Add(values: array of T): IVector<T>;
var
  i : integer;
begin
  for I := 0 to High(values) do
    FMultiArray[i] := values[i];
  result := Self;
end;

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

class function TVector<T>.New(Columns: Integer): IVector<T>;
begin
  result := Create(Columns);
end;

procedure TVector<T>.SetCell(x: Integer; const Value: T);
begin
  FMultiArray[x] := Value;
end;

procedure TVector<T>.SetColumns(Value: Integer);
begin
  FColumns := Value;
end;

function TVector<T>.ToString: string;
begin
  Result := TVectorLayoutString.New.FormatText(Self.Length, FMultiArray);
end;

end.
