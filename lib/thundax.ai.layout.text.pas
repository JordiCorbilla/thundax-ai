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

unit thundax.ai.layout.text;

interface

uses
  thundax.ai.matrix;

type
  TMultiArrayDouble = Array of array of Double;
  TMultiArrayString = Array of array of String;

  ITextLayout = interface
    function FormatText(Rows : integer; columns: integer; MultiArrayIn : variant) : string;
  end;

  TTextLayoutDouble = class(TInterfacedObject, ITextLayout)
    function FormatText(Rows : integer; columns: integer; MultiArrayIn : variant) : string;
    class function New: ITextLayout;
  end;

  TTextLayoutString = class(TInterfacedObject, ITextLayout)
    function FormatText(Rows : integer; columns: integer; MultiArrayIn : variant) : string;
    class function New: ITextLayout;
  end;

implementation

uses
  thundax.ai.vector, thundax.ai.Maths, SysUtils, thundax.ai.stringHelper;

{ TTextLayout }

function TTextLayoutDouble.FormatText(Rows: integer; columns: integer; MultiArrayIn : variant): string;
var
  i, j: Integer;
  sLine, sRowLine: string;
  maxLength: Integer;
  vector: IVector<Integer>;
  newString: string;
  MultiArray : TMultiArrayDouble;
begin
  MultiArray := TMultiArrayDouble(MultiArrayIn);
  sRowLine := '';
  // First get the max colum length
  vector := TVector<Integer>.Create(columns);
  for j := 0 to columns - 1 do
  begin
    maxLength := 0;
    for i := 0 to Rows - 1 do
    begin
      if TMathHelper.Compare(MultiArray[j, i], 0, '>=') then
        newString := '+' + FloatToStr(MultiArray[j, i])
      else
        newString := FloatToStr(MultiArray[j, i]);

      if Length(newString) > maxLength then
        maxLength := Length(newString);
    end;
    vector.Cell[j] := maxLength;
  end;

  for i := 0 to Rows - 1 do
  begin
    sLine := '';
    for j := 0 to columns - 1 do
    begin
      if TMathHelper.Compare(MultiArray[j, i], 0, '>=') then
        newString := '+' + FloatToStr(MultiArray[j, i])
      else
        newString := FloatToStr(MultiArray[j, i]);
      sLine := sLine + newString + StringOfChar(' ', 1 + (vector.Cell[j] - Length(newString)));
    end;
    sRowLine := sRowLine + sLine + sLineBreak;
  end;
  sRowLine := StringReplace(sRowLine, '+', ' ', [rfReplaceAll, rfIgnoreCase]);
  result := sRowLine;
end;

{ TTextLayoutString }

function TTextLayoutString.FormatText(Rows, columns: integer; MultiArrayIn: variant): string;
var
  i, j: Integer;
  sLine, sRowLine: string;
  maxLength: Integer;
  vector: IVector<Integer>;
  newString: string;
  MultiArray : TMultiArrayString;
  outValue : double;
begin
  MultiArray := TMultiArrayString(MultiArrayIn);
  sRowLine := '';
  // First get the max colum length
  vector := TVector<Integer>.Create(columns);
  for j := 0 to columns - 1 do
  begin
    maxLength := 0;
    for i := 0 to Rows - 1 do
    begin
      if TStringHelper.TryFloatStrToFloat(MultiArray[j, i], outValue) then
      begin
        if TMathHelper.Compare(outValue, 0, '>=') then
          newString := '+' + FloatToStr(outValue)
        else
          newString := FloatToStr(outValue);
      end
      else
        newString := MultiArray[j, i];

      if Length(newString) > maxLength then
        maxLength := Length(newString);
    end;
    vector.Cell[j] := maxLength;
  end;

  for i := 0 to Rows - 1 do
  begin
    sLine := '';
    for j := 0 to columns - 1 do
    begin
      if TStringHelper.TryFloatStrToFloat(MultiArray[j, i], outValue) then
      begin
        if TMathHelper.Compare(outValue, 0, '>=') then
          newString := '+' + FloatToStr(outValue)
        else
          newString := FloatToStr(outValue);
      end
      else
        newString := MultiArray[j, i];
      sLine := sLine + newString + StringOfChar(' ', 1 + (vector.Cell[j] - Length(newString)));
    end;
    sRowLine := sRowLine + sLine + sLineBreak;
  end;
  sRowLine := StringReplace(sRowLine, '+', ' ', [rfReplaceAll, rfIgnoreCase]);
  result := sRowLine;
end;

class function TTextLayoutDouble.New: ITextLayout;
begin
  result := Create;
end;

class function TTextLayoutString.New: ITextLayout;
begin
  result := Create;
end;

end.
