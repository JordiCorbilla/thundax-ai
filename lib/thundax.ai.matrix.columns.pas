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
