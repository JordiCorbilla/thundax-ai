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

unit thundax.ai.convexHull;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.coordenate;

type
  TConvexHull = class(TObject)
  private
    Fvertices: TObjectList<TCoordenate>;
    function isLeft(P0, P1, P2: TCoordenate): double;
    procedure Setvertices(const Value: TObjectList<TCoordenate>);
    function Compare(Item1: TCoordenate; Item2: TCoordenate): Integer;
  public
    property vertices: TObjectList<TCoordenate>read Fvertices write Setvertices;
    procedure AddVertice(x, y: Integer);
    constructor Create();
    destructor Destroy(); override;
    function Chain(): TObjectList<TCoordenate>;
  end;

implementation

uses
  SysUtils, Generics.Defaults;

{ TConvexHull }

procedure TConvexHull.AddVertice(x, y: Integer);
begin
  Fvertices.Add(TCoordenate.Create(x, y));
  Fvertices.Sort(TComparer<TCoordenate>.Construct(
   function (const L, R: TCoordenate): integer
   begin
     result := Compare(L, R);
   end));
end;

function TConvexHull.Compare(Item1: TCoordenate; Item2: TCoordenate): Integer;
begin
  if (Item1.x > Item2.x) then
    Result := 1
  else if (Item1.x = Item2.x) then
    if (Item1.y > Item2.y) then
      Result := 1
    else if (Item1.y = Item2.y) then
      Result := 0
    else
      Result := -1
  else
    Result := -1
end;

function TConvexHull.Chain: TObjectList<TCoordenate>;
var
  H: TObjectList<TCoordenate>;
  bot, i, minmin, minmax, maxmin, maxmax, n: Integer;
  xmin, xmax: double;
  output: boolean;
begin
  H := TObjectList<TCoordenate>.Create(false);
  minmin := 0;
  xmin := Fvertices[0].x;
  n := Fvertices.Count - 1;

  for i := 1 to n - 1 do
    if Fvertices[i].x <> xmin then
      break;

  minmax := i - 1;
  output := false;
  if (minmax = n - 1) then
  begin
    H.Add(Fvertices[minmin]);
    if (Fvertices[minmax].y <> Fvertices[minmin].y) then
      H.Add(Fvertices[minmax]);
    H.Add(Fvertices[minmin]);
    output := true;
  end;

  if not output then
  begin
    maxmax := n - 1;
    xmax := Fvertices[n - 1].x;
    for i := n - 2 downto 0 do
      if (Fvertices[i].x <> xmax) then
        break;

    maxmin := i + 1;
    H.Add(Fvertices[minmin]);
    i := minmax;
    while (i <= maxmin) do
    begin
      inc(i);
      if ((isLeft(Fvertices[minmin], Fvertices[maxmin], Fvertices[i]) >= 0) and (i < maxmin)) then
        continue;

      while (H.Count > 1) do
        if (isLeft(H[H.Count - 2], H[H.Count - 1], Fvertices[i]) > 0) then
          break
        else
          H.Remove(H[H.Count - 1]);
      H.Add(Fvertices[i]);
    end;

    if (maxmax <> maxmin) then
      H.Add(Fvertices[maxmax]);
    bot := H.Count - 1;
    i := maxmin;
    while (i > minmax) do
    begin
      dec(i);
      if ((isLeft(Fvertices[maxmax], Fvertices[minmax], Fvertices[i]) >= 0) and (i > minmax)) then
        continue;

      while (H.Count > bot) do
        if (isLeft(H[H.Count - 2], H[H.Count - 1], Fvertices[i]) > 0) then
          break
        else
          H.Remove(H[H.Count - 1]);
      H.Add(Fvertices[i]);

    end;
    if (minmax <> minmin) then
      H.Add(Fvertices[minmin]);
  end;
  Result := H;
end;

constructor TConvexHull.Create;
begin
  Fvertices := TObjectList<TCoordenate>.Create;
end;

destructor TConvexHull.Destroy;
begin
  FreeAndNil(Fvertices);
  inherited;
end;

function TConvexHull.isLeft(P0, P1, P2: TCoordenate): double;
begin
  Result := (P1.x - P0.x) * (P2.y - P0.y) - (P2.x - P0.x) * (P1.y - P0.y);
end;

procedure TConvexHull.Setvertices(const Value: TObjectList<TCoordenate>);
begin
  Fvertices := Value;
end;

end.
