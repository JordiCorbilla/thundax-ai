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

unit thundax.ai.attribute.dictionary;

interface

uses
  Contnrs, Generics.Collections, thundax.ai.classInstance.list;

type
  // Dictionary for Pair-values
  // Each item in the dictionary will have associated a full list of Class Instances.

  // Column1
  // [
  // '1'
  // ['N' 2]
  // ['O' 1]
  // '0'
  // ['O' 2]
  // ['N' 1]
  // Goodness = 0.666666666666667
  // ]

  TDictionaryAttributes = class(TDictionary<string, TListClassInstances>)
  private
    FnumTotalInstances: integer;
    FGoodnessOfFit: Double;
    FName: string;
    FminValue: Double;
    Fvalue: Double;
    FisNumerical: Boolean;
    procedure SetnumTotalInstances(const Value: integer);
    procedure SetGoodness(const Value: Double);
    procedure SetName(const Value: string);
    procedure SetisNumerical(const Value: Boolean);
    procedure SetminValue(const Value: Double);
    procedure Setvalue(const Value: Double);
  public
    function GetGoodnessOfFit(): Double;
    function ToString(): string; override;
    function SimpleOutput(): string;
    function CountInstances: integer;
    property numTotalInstances: integer read FnumTotalInstances write SetnumTotalInstances;
    property GoodnessOfFit: Double read GetGoodnessOfFit write SetGoodness;
    destructor Destroy(); override;
    property Name: string read FName write SetName;
    property isNumerical: Boolean read FisNumerical write SetisNumerical;
    property Value: Double read Fvalue write Setvalue;
    property minValue: Double read FminValue write SetminValue;
  end;

implementation

uses
  SysUtils;

{ TDictionaryAttributes }

function TDictionaryAttributes.CountInstances: integer;
var
  key: string;
  outList: TListClassInstances;
  num: integer;
begin
  num := 0;
  for key in Self.Keys do
  begin
    Self.TryGetValue(key, outList);
    num := num + outList.getMaxValue;
  end;
  result := num;
end;

destructor TDictionaryAttributes.Destroy;
var
  key: string;
  outClassInstances: TListClassInstances;
begin
  for key in Self.Keys do
  begin
    Self.TryGetValue(key, outClassInstances);
    outClassInstances.Free;
  end;
  inherited;
end;

function TDictionaryAttributes.GetGoodnessOfFit: Double;
begin
  FGoodnessOfFit := Self.CountInstances / Self.FnumTotalInstances;
  result := FGoodnessOfFit;
end;

procedure TDictionaryAttributes.SetGoodness(const Value: Double);
begin
  FGoodnessOfFit := Value;
end;

procedure TDictionaryAttributes.SetisNumerical(const Value: Boolean);
begin
  FisNumerical := Value;
end;

procedure TDictionaryAttributes.SetminValue(const Value: Double);
begin
  FminValue := Value;
end;

procedure TDictionaryAttributes.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDictionaryAttributes.SetnumTotalInstances(const Value: integer);
begin
  FnumTotalInstances := Value;
end;

procedure TDictionaryAttributes.Setvalue(const Value: Double);
begin
  Fvalue := Value;
end;

function TDictionaryAttributes.SimpleOutput: string;
begin
  result := '{ Goodness of Fit = ' + FloatToStr(GoodnessOfFit) + ' }';
end;

function TDictionaryAttributes.ToString: string;
var
  key: string;
  outClassInstance: TListClassInstances;
  Text: string;
begin
  Text := '  {' + sLineBreak;
  for key in Self.Keys do
  begin
    Self.TryGetValue(key, outClassInstance);
    Text := Text + '    ''' + key + '''' + sLineBreak;
    Text := Text + '' + outClassInstance.ToString + sLineBreak;
  end;
  Text := Text + '    Goodness of Fit : ' + IntToStr(Self.CountInstances) + ' / ' + IntToStr(Self.FnumTotalInstances) + ' = ' + FloatToStr(GoodnessOfFit) + sLineBreak;
  result := Text + '  }';
end;

end.
