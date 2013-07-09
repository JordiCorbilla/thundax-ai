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

unit thundax.ai.attribute.average.list.sorted;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.attribute.numerical, thundax.ai.attribute.average.list;

type
  // This class will analyse the list
  // It will sort the values in ascending order
  // It will remove any duplicates
  // It will get the average list with all the Paired-values items.
  TDistinctSortedAttributeList = class(TObjectList<TNumericalAttribute>)
  private
    FDefaultList: TObjectList<TNumericalAttribute>;
    FName: string;
    procedure SetName(const Value: string);
  public
    constructor Create(name: string);
    destructor Destroy(); override;
    function Exists(attribute: TNumericalAttribute): Boolean;
    function Add(const Value: TNumericalAttribute): integer; overload;
    function AddDefault(const Value: TNumericalAttribute): integer;
    function ToString(): string; override;
    function GetAverageList(): TAverageAttributeList;
    procedure SortList();
    property Name: string read FName write SetName;
  end;

implementation

uses
  SysUtils, Generics.Defaults, Math, thundax.ai.attribute.average, thundax.ai.classInstance, thundax.ai.classInstance.list;

{ TDistinctSortedAttributeList }

function TDistinctSortedAttributeList.Add(const Value: TNumericalAttribute): integer;
begin
  result := 0;
  if not Exists(Value) then
    result := inherited Add(Value);
end;

function TDistinctSortedAttributeList.AddDefault(const Value: TNumericalAttribute): integer;
begin
  result := FDefaultList.Add(Value);
end;

constructor TDistinctSortedAttributeList.Create(name: string);
begin
  SetName(name);
  FDefaultList := TObjectList<TNumericalAttribute>.Create;
end;

destructor TDistinctSortedAttributeList.Destroy;
begin
  FDefaultList.Free;
  inherited;
end;

function TDistinctSortedAttributeList.Exists(attribute: TNumericalAttribute): Boolean;
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

//This methos builds the list of average values to work on.
// Example
//    > {N 3}
//      {O 1}        It will give the number of times it finds a class.
//    <={N 2}
//      {O 2}
//
function TDistinctSortedAttributeList.GetAverageList: TAverageAttributeList;
var
  i, j: integer;
  firstValue: Double;
  secondValue: Double;
  averageList: TAverageAttributeList;
  key: string;
  outClassInstances: TListClassInstances;
begin
  averageList := TAverageAttributeList.Create(Self.Name);
  for i := 0 to Self.Count - 2 do
  begin
    firstValue := Self[i].Value;
    secondValue := Self[i + 1].Value;
    averageList.Add(TAverageAttribute.Create(((firstValue + secondValue) / 2)));
  end;
  // Work on the average List:
  for i := 0 to averageList.Count - 1 do
  begin
    for j := 0 to Self.FDefaultList.Count - 1 do
    begin
      if Self.FDefaultList[j].Value <= averageList[i].Value then
      begin
        if not averageList[i].AttributeList.ContainsKey('<=') then
        begin
          averageList[i].AttributeList.Name := Self.Name;
          averageList[i].AttributeList.isNumerical := true;
          averageList[i].AttributeList.Value := averageList[i].Value;
          averageList[i].AttributeList.Add('<=', TListClassInstances.Create);
          averageList[i].AttributeList.numTotalInstances := Self.FDefaultList.Count;
        end;
      end
      else
      begin
        if not averageList[i].AttributeList.ContainsKey('>') then
        begin
          averageList[i].AttributeList.Name := Self.Name;
          averageList[i].AttributeList.isNumerical := true;
          averageList[i].AttributeList.Value := averageList[i].Value;
          averageList[i].AttributeList.Add('>', TListClassInstances.Create);
          averageList[i].AttributeList.numTotalInstances := Self.FDefaultList.Count;
        end;
      end;
    end;
  end;

  for i := 0 to averageList.Count - 1 do
  begin
    for key in averageList[i].AttributeList.Keys do
    begin
      averageList[i].AttributeList.TryGetValue(key, outClassInstances);
      for j := 0 to Self.FDefaultList.Count - 1 do
      begin
        if (key = '<=') and (Self.FDefaultList[j].Value <= averageList[i].Value) then
          outClassInstances.Add(TClassInstances.Create(Self.FDefaultList[j].yClass, 1));
      end;
    end;
    for key in averageList[i].AttributeList.Keys do
    begin
      averageList[i].AttributeList.TryGetValue(key, outClassInstances);
      for j := 0 to Self.FDefaultList.Count - 1 do
      begin
        if (key = '>') and (Self.FDefaultList[j].Value > averageList[i].Value) then
          outClassInstances.Add(TClassInstances.Create(Self.FDefaultList[j].yClass, 1));
      end;
    end;
  end;
  result := averageList;
end;

procedure TDistinctSortedAttributeList.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDistinctSortedAttributeList.SortList;
begin
  Self.Sort(TComparer<TNumericalAttribute>.Construct( function(const L, r: TNumericalAttribute): integer begin result := CompareValue(L.Value, r.Value, 0.000001); end));
end;

function TDistinctSortedAttributeList.ToString: string;
var
  i: integer;
  s: string;
begin
  for i := 0 to Self.Count - 1 do
    s := s + Self[i].ToString + sLineBreak;
  result := s;
end;

end.
