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

unit thundax.ai.spanningTree;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.coordenate, thundax.ai.matrix;

type
  TArg = reference to procedure(const p: TCoordenate);

  TSpanningTree = class(TObject)
  private
    Fvertices: TObjectList<TCoordenate>;
    Fmatrix: TMatrix;
    procedure Setvertices(const Value: TObjectList<TCoordenate>);
  public
    property vertices: TObjectList<TCoordenate>read Fvertices write Setvertices;
    constructor Create();
    destructor Destroy(); override;
    procedure AddVertice(x, y: Integer);
    procedure CalculateAdjacentList();
    function Minimum(CallBackP1: TArg; CallBackP2: TArg): Double;
  end;

implementation

uses
  SysUtils, thundax.ai.maths;

{ TSpanningTree }

procedure TSpanningTree.AddVertice(x, y: Integer);
begin
  Fvertices.Add(TCoordenate.Create(x, y));
end;

procedure TSpanningTree.CalculateAdjacentList;
var
  i, j: Integer;
  p1, p2: TCoordenate;
  FAdjacentMatrix: TMatrix;
begin
  FAdjacentMatrix := TMatrix.Create(Fvertices.count);
  try
    for i := 0 to Fvertices.count - 1 do
    begin
      for j := 0 to Fvertices.count - 1 do
      begin
        p1 := Fvertices.Items[i];
        p2 := Fvertices.Items[j];
        FAdjacentMatrix.Cell[i, j] := TMathHelper.EuclideanDistance(p2, p1)
      end;
    end;
  finally
    Fmatrix := FAdjacentMatrix.Transpose;
    FreeAndNil(FAdjacentMatrix);
  end;
end;

constructor TSpanningTree.Create;
begin
  Fvertices := TObjectList<TCoordenate>.Create;
end;

destructor TSpanningTree.Destroy;
begin
  FreeAndNil(Fvertices);
  if Assigned(Fmatrix) then
    FreeAndNil(Fmatrix);
  inherited;
end;

function TSpanningTree.Minimum(CallBackP1: TArg; CallBackP2: TArg): Double;
var
  m, i, k, j: Integer;
  min, euclidianDistance: Double;
  distance: Double;
  nearestVertex: Integer;
  distanceList: array of Double;
  nearestPoint: array of Integer;
  origin: TCoordenate;
begin
  SetLength(distanceList, Fmatrix.Size);
  SetLength(nearestPoint, Fmatrix.Size);

  distance := 0.0;
  nearestVertex := 0;
  for i := 0 to Fmatrix.Size - 1 do
  begin
    nearestPoint[i] := 1;
    distanceList[i] := Fmatrix.Cell[1, i];
  end;

  origin := Fvertices.Items[0];
  CallBackP1(origin);
  j := 0;
  for k := 0 to Fmatrix.Size - 1 do
  begin
    min := High(Integer);
    for i := 0 to Fmatrix.Size - 1 do
    begin
      if (distanceList[i] >= 0) and (TMathHelper.Compare(min, distanceList[i], '>')) then
      begin
        min := distanceList[i];
        nearestVertex := i;
      end;
    end;

    euclidianDistance := Fmatrix.Cell[nearestVertex, nearestPoint[nearestVertex]];
    CallBackP2(Fvertices.Items[nearestVertex]);

    m := 0;
    for i := 0 to Fmatrix.Size - 1 do
    begin
      if (TMathHelper.Compare(Fmatrix.Cell[i, nearestVertex], euclidianDistance, '=')) then
      begin
        j := i;
        Inc(m);
      end;
    end;
    if m > 1 then
      j := nearestVertex;

    distance := distance + TMathHelper.EuclideanDistance(Fvertices.Items[j], Fvertices.Items[nearestVertex]);

    origin := Fvertices.Items[j];
    CallBackP1(origin);

    distanceList[nearestVertex] := -1;
    for i := 0 to Fmatrix.Size - 1 do
      if TMathHelper.Compare(Fmatrix.Cell[i, nearestVertex], distanceList[i], '<') then
      begin
        distanceList[i] := Fmatrix.Cell[i, nearestVertex];
        nearestPoint[i] := nearestVertex;
      end;
  end;
  result := distance;
end;

procedure TSpanningTree.Setvertices(const Value: TObjectList<TCoordenate>);
begin
  Fvertices := Value;
end;

end.
