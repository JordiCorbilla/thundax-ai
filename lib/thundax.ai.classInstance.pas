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

unit thundax.ai.classInstance;

interface

type
  // Each individual class, will have a number of instances associated to it
  // for example
  // Value 5 will have {'O' 5}  - 5 instances of the class O
  // {'N' 1}  - 1 instance of the class N
  TClassInstances = class(TObject)
  private
    FyClass: string;
    FnumInstances: integer;
    procedure SetnumInstances(const Value: integer);
    procedure SetyClass(const Value: string);
  public
    property yClass: string read FyClass write SetyClass;
    property numInstances: integer read FnumInstances write SetnumInstances;
    constructor Create(yClass: string; numInstances: integer);
    destructor Destroy(); override;
  end;

implementation

{ TClassInstances }

constructor TClassInstances.Create(yClass: string; numInstances: integer);
begin
  SetyClass(yClass);
  SetnumInstances(numInstances);
end;

destructor TClassInstances.Destroy;
begin

  inherited;
end;

procedure TClassInstances.SetnumInstances(const Value: integer);
begin
  FnumInstances := Value;
end;

procedure TClassInstances.SetyClass(const Value: string);
begin
  FyClass := Value;
end;

end.
