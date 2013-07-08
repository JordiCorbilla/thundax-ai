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
