// Copyright (c) 2015, Jordi Corbilla
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

unit thundax.ai.classInstance.list;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.classInstance;

type
  // This will have a list of ClassIntances, example:
  // Value -1 will have {'O' 3}  - 3 instances of the class O
  // {'N' 2}  - 2 instances of the class N
  // Value 1 will have {'O' 2}  - 2 instances of the class O
  // {'N' 5}  - 5 instances of the class N
  //
  // Max value will get the higher number from every instance.
  TListClassInstances = class(TObjectList<TClassInstances>)
    function Exists(classInstances: TClassInstances; var currentInstance: TClassInstances): Boolean;
    function Add(const Value: TClassInstances): integer; overload;
    function ToString(): string; override;
    function getMaxValue(): integer;
    function getBetterClass(): string;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  SysUtils;

{ TListClassInstances }

function TListClassInstances.Add(const Value: TClassInstances): integer;
var
  currentInstance: TClassInstances;
begin
  result := 0;
  if Exists(Value, currentInstance) then
  begin
    currentInstance.numInstances := currentInstance.numInstances + 1;
    Value.Free;
  end
  else
    result := inherited Add(Value);
end;

constructor TListClassInstances.Create;
begin
  inherited Create(True);
end;

destructor TListClassInstances.Destroy;
begin
  inherited;
end;

function TListClassInstances.Exists(classInstances: TClassInstances; var currentInstance: TClassInstances): Boolean;
var
  found: Boolean;
  i: integer;
begin
  i := 0;
  found := False;
  currentInstance := nil;
  while (not found) and (i < Self.Count) do
  begin
    found := classInstances.yClass = Self[i].yClass;
    if found then
      currentInstance := Self[i];
    Inc(i);
  end;
  result := found;
end;

function TListClassInstances.getBetterClass: string;
var
  i: integer;
  max: integer;
  betterClass: string;
begin
  max := 0;
  for i := 0 to Self.Count - 1 do
    if Self[i].numInstances > max then
    begin
      max := Self[i].numInstances;
      betterClass := Self[i].yClass;
    end;
  result := betterClass;
end;

function TListClassInstances.getMaxValue: integer;
var
  i: integer;
  max: integer;
begin
  max := 0;
  for i := 0 to Self.Count - 1 do
    if Self[i].numInstances > max then
      max := Self[i].numInstances;
  result := max;
end;

function TListClassInstances.ToString: string;
var
  i: integer;
  Text: string;
begin
  Text := '';
  for i := 0 to Self.Count - 1 do
  begin
    if i <> Self.Count - 1 then
      Text := Text + '      {''' + Self[i].yClass + ''' ' + IntToStr(Self[i].numInstances) + '}' + sLineBreak
    else
      Text := Text + '      {''' + Self[i].yClass + ''' ' + IntToStr(Self[i].numInstances) + '}'
  end;
  result := Text;
end;

end.
