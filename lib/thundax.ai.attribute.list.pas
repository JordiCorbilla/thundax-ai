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

unit thundax.ai.attribute.list;

interface
uses
  Contnrs, Generics.Collections, thundax.ai.attribute.ordinal, thundax.ai.attribute.dictionary;

type
  // Initial list of Ordinal Attributes
  // The purpose of this list is to scan the items from the grid and the ability to work on them
  TAttributeList = class(TObjectList<TOrdinalAttribute>)
  private
    FTotalNumberOfItems: integer;
    FName: string;
    procedure SetTotalNumberOfItems(const Value: integer);
    procedure SetName(const Value: string);
  public
    function Exists(attribute: TOrdinalAttribute): Boolean;
    function Add(const Value: TOrdinalAttribute): integer; overload;
    function ToString(): string; override;
    function GetListOfIndividualItems(): TDictionaryAttributes;
    constructor Create(name: string);
    property TotalNumberOfItems: integer read FTotalNumberOfItems write SetTotalNumberOfItems;
    property Name: string read FName write SetName;
  end;

implementation

uses
  thundax.ai.classInstance.list, thundax.ai.classInstance;

{ TAttributeList }

function TAttributeList.Add(const Value: TOrdinalAttribute): integer;
begin
  result := 0;
  if not Exists(Value) then
    result := inherited Add(Value);
end;

constructor TAttributeList.Create(name: string);
begin
  SetName(name);
end;

function TAttributeList.Exists(attribute: TOrdinalAttribute): Boolean;
begin
  result := False;
end;

//This method returns the list of dictionary attributes {Class - Value}
function TAttributeList.GetListOfIndividualItems: TDictionaryAttributes;
var
  dictionary: TDictionaryAttributes;
  i, j: integer;
  outClassInstances: TListClassInstances;
  key: string;
begin
  dictionary := TDictionaryAttributes.Create();
  dictionary.Name := self.Name;
  dictionary.numTotalInstances := self.FTotalNumberOfItems;
  for i := 0 to self.Count - 1 do
    if not dictionary.ContainsKey(self[i].Value) then
      dictionary.Add(self[i].Value, TListClassInstances.Create);
  for key in dictionary.Keys do
  begin
    dictionary.TryGetValue(key, outClassInstances);
    for j := 0 to self.Count - 1 do
      if key = self[j].Value then
        outClassInstances.Add(TClassInstances.Create(self[j].yClass, 1));
  end;
  result := dictionary;
end;

procedure TAttributeList.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TAttributeList.SetTotalNumberOfItems(const Value: integer);
begin
  FTotalNumberOfItems := Value;
end;

function TAttributeList.ToString: string;
var
  i: integer;
  s: string;
begin
  for i := 0 to self.Count - 1 do
    s := s + self[i].ToString + sLineBreak;
  result := s;
end;

end.
