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

unit thundax.ai.attribute.average.list;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.attribute.average;

type
  // This Class will contain the list of items from the class above.
  // This class will have the maxGoodness method which retrieves the best goodness.
  TAverageAttributeList = class(TObjectList<TAverageAttribute>)
  private
    FName: string;
    procedure SetName(const Value: string);
  public
    function maxGoodnessOfFit(): Double; overload;
    function maxGoodnessOfFit(var item: integer): Double; overload;
    function Exists(attribute: TAverageAttribute): Boolean;
    function Add(const Value: TAverageAttribute): integer; overload;
    function ToString(): string; override;
    function SimpleOutput: string;
    constructor Create(name: string);
    destructor Destroy(); override;
    property Name: string read FName write SetName;
  end;

implementation

uses
  SysUtils;

{ TAverageAttributeList }

function TAverageAttributeList.Add(const Value: TAverageAttribute): integer;
begin
  result := 0;
  if not Exists(Value) then
    result := inherited Add(Value);
end;

constructor TAverageAttributeList.Create(name: string);
begin
  SetName(name);
  inherited Create(True);
end;

destructor TAverageAttributeList.Destroy;
begin

  inherited;
end;

function TAverageAttributeList.Exists(attribute: TAverageAttribute): Boolean;
var
  found: Boolean;
  i: integer;
begin
  i := 0;
  found := False;
  while (not found) and (i < Self.Count) do
  begin
    found := attribute.Value = Self[i].Value;
    Inc(i);
  end;
  result := found;
end;

function TAverageAttributeList.maxGoodnessOfFit(var item: integer): Double;
var
  i: integer;
  max: Double;
begin
  max := 0;
  item := -1;
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].AttributeList.GetGoodnessOfFit > max then
    begin
      max := Self[i].AttributeList.GoodnessOfFit;
      item := i;
    end;
  end;
  result := max;
end;

function TAverageAttributeList.maxGoodnessOfFit: Double;
var
  i: integer;
  max: Double;
begin
  max := 0;
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].AttributeList.GetGoodnessOfFit > max then
      max := Self[i].AttributeList.GetGoodnessOfFit;
  end;
  result := max;
end;

procedure TAverageAttributeList.SetName(const Value: string);
begin
  FName := Value;
end;

function TAverageAttributeList.SimpleOutput: string;
begin
  result := '{ Goodness of fit = ' + FloatToStr(maxGoodnessOfFit) + ' }';
end;

function TAverageAttributeList.ToString: string;
var
  i: integer;
  s: string;
begin
  s := '{ ' + sLineBreak;
  for i := 0 to Self.Count - 1 do
    s := s + Self[i].ToString + sLineBreak;
  s := s + '   Max Goodness of fit = ' + FloatToStr(maxGoodnessOfFit) + sLineBreak;
  result := s + '}';
end;

end.


