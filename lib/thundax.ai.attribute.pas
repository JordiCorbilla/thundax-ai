// Copyright (c) 2012-2015, Jordi Corbilla
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

unit thundax.ai.attribute;

interface

type
  IAttribute<T> = interface
    function GetValue: T;
    function GetyClass: String;
    procedure SetValue(const Value: T);
    procedure SetyClass(const Value: string);
    property Value: T read GetValue write SetValue;
    property yClass: string read GetyClass write SetyClass;
  end;
  // Generic class for any attribute
  TAttribute<T> = class(TInterfacedObject, IAttribute<T>)
  private
    FValue: T;
    FyClass: string;
    function GetValue: T;
    function GetyClass: String;
    procedure SetValue(const Value: T);
    procedure SetyClass(const Value: string);
  public
    property Value: T read GetValue write SetValue;
    property yClass: string read GetyClass write SetyClass;
    constructor Create(Value: T; yClass: string);
  end;

implementation

{ TAttribute<T> }

constructor TAttribute<T>.Create(Value: T; yClass: string);
begin
  SetValue(Value);
  SetyClass(yClass);
end;

function TAttribute<T>.GetValue: T;
begin
  Result := FValue;
end;

function TAttribute<T>.GetyClass: String;
begin
  Result := FyClass;
end;

procedure TAttribute<T>.SetValue(const Value: T);
begin
  FValue := Value;
end;

procedure TAttribute<T>.SetyClass(const Value: string);
begin
  FyClass := Value;
end;

end.
