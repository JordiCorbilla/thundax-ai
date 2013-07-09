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

unit thundax.ai.attribute.average;

interface

uses
  thundax.ai.attribute.dictionary;

type
  // Class for the Numerical attributes.
  // As we need to build the average list, this class will contain a list of attributes
  // where the average has been built into it.

  // example
  // 0.25  -> 0.28 = (0.25 + 0.31 / 2)
  // 0.31  -> 0.345 = (0.31 + 0.38 / 2)
  // 0.38  -> 0.44 = (0.31 + 0.38 / 2)
  // 0.5

  TAverageAttribute = class(TObject)
  private
    FAttributeList: TDictionaryAttributes;
    FValue: Double;
    procedure SetAttributeList(const Value: TDictionaryAttributes);
    procedure SetValue(const Value: Double);
  public
    property Value: Double read FValue write SetValue;
    property AttributeList: TDictionaryAttributes read FAttributeList write SetAttributeList;
    constructor Create(Value: Double);
    destructor Destroy(); override;
    function ToString(): string; override;
  end;

implementation

uses
  SysUtils, Generics.Defaults, Math;

{ TAverageAttribute }

constructor TAverageAttribute.Create(Value: Double);
begin
  SetValue(Value);
  FAttributeList := TDictionaryAttributes.Create();
end;

destructor TAverageAttribute.Destroy;
begin
  FAttributeList.Free;
  inherited;
end;

procedure TAverageAttribute.SetAttributeList(const Value: TDictionaryAttributes);
begin
  FAttributeList := Value;
end;

procedure TAverageAttribute.SetValue(const Value: Double);
begin
  FValue := Value;
end;

function TAverageAttribute.ToString: string;
begin
  result := 'Value: ' + FloatToStr(FValue) + sLineBreak;
  result := result + FAttributeList.ToString + sLineBreak;
end;

end.
