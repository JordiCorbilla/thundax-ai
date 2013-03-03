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

unit thundax.ai.matrix;

interface

type
  TMultiArray = Array of array of Double;

  TMatrix = class(TObject)
  private
    FSize: Integer;
    FMultiArray: TMultiArray;
    function GetCell(x, y: Integer): Double;
    procedure SetCell(x, y: Integer; const Value: Double);
  public
    constructor Create(size: Integer);
    procedure Initialise();
    property size: Integer read FSize write FSize;
    function Transpose(): TMatrix;
    property Cell[x, y: Integer]: Double read GetCell write SetCell;
  end;

implementation

{ TMatrix }

constructor TMatrix.Create(size: Integer);
var
  i: Integer;
begin
  FSize := size;
  SetLength(FMultiArray, size);
  for i := 0 to size - 1 do
    SetLength(FMultiArray[i], size);
end;

function TMatrix.GetCell(x, y: Integer): Double;
begin
  Result := FMultiArray[x, y];
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

procedure TMatrix.SetCell(x, y: Integer; const Value: Double);
begin
  FMultiArray[x, y] := Value;
end;

function TMatrix.Transpose: TMatrix;
var
  trasposed: TMatrix;
  i, j: Integer;
begin
  trasposed := TMatrix.Create(Self.FSize);
  for i := 0 to Self.FSize - 1 do
    for j := 0 to Self.FSize - 1 do
      trasposed.Cell[i, j] := FMultiArray[j, i];
  result := trasposed;
end;

end.
