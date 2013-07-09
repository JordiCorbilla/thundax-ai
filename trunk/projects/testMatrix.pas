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

unit testMatrix;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure Display(s: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  thundax.ai.matrix, thundax.ai.matrix.columns, thundax.ai.matrix.bind, thundax.ai.table;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  matrix: IMatrix;
  c1, c2, c3: IColumn;
  bind, bind1, bind2: IBind;
begin
  Memo1.Lines.Clear;
  c1 := TColumn.create([1, 2]);
  c2 := TColumn.create([4, 5]);
  c3 := TColumn.create([7, 8]);

  bind := TBind.New([c1, c2, c3]);
  matrix := bind.matrix;

  Memo1.Lines.Add(matrix.ToString);
  Memo1.Lines.Add(matrix.transpose.ToString);

  bind1 := TBind.New([TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4])]);
  bind2 := TBind.New([TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4]), TColumn.create([1, 2, 3, 4])]);
  Memo1.Lines.Add(bind1.matrix.ToString);
  Memo1.Lines.Add(bind2.matrix.transpose.ToString);
  Memo1.Lines.Add(bind1.matrix.Multiply(bind2.matrix.transpose).ToString);

  bind1 := TBind.New([TColumn.create([1, 2]), TColumn.create([3, 4])]);
  bind2 := TBind.New([TColumn.create([5, 6]), TColumn.create([7, 8])]);

  Memo1.Lines.Add(bind1.matrix.ToString);
  Memo1.Lines.Add(bind2.matrix.ToString);
  Memo1.Lines.Add(bind1.matrix.Multiply(bind2.matrix).ToString);
  Memo1.Lines.Add(bind1.matrix.Add(bind2.matrix).ToString);
  Memo1.Lines.Add(bind1.matrix.Subtract(bind2.matrix).ToString);

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  matrix: IMatrix;
  c1, c2, c3, c4, c5: IColumn;
  bind: IBind;
begin
  Memo1.Lines.Clear;
  c1 := TColumn.create([1, 0, 1, 1, 0, 0]);
  c2 := TColumn.create([1, 1, 1, 0, 1, 1]);
  c3 := TColumn.create([-1.645497224, -0.645497224, 1.290994449, -0.845497224, 1.290994449, -0.645557224]);
  c4 := TColumn.create([2.597414305, 1.792842914, -0.697614305, -0.797614305, -0.597614305, -0.797614305]);
  c5 := TColumn.create([1.89738712, -0.817753946, -0.808876973, 0.220626409, -0.208876973, -0.882505637]);

//  c3 := TColumn.create([-0.645497224, -0.645497224, 1.290994449, -0.645497224, 1.290994449, -0.645497224]);
//  c4 := TColumn.create([0.597614305, 1.792842914, -0.597614305, -0.597614305, -0.597614305, -0.597614305]);
//  c5 := TColumn.create([1.89738712, -0.617753946, -0.308876973, 0.220626409, -0.308876973, -0.882505637]);

  bind := TBind.New([c1, c2, c3, c4, c5]);
  matrix := bind.matrix;

  Display('Matrix:');
  Display(matrix.ToString);
  Display('Mean:');
  Display(matrix.Mean.ToString);
  Display('Covariance:');
  Display(matrix.Covariance.ToString);
  Display('Distance:');
  Display(matrix.Distance.ToString);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  matrix1, matrix2: IMatrix;
begin
  matrix1 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  matrix2 := TBind.New([TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1]), TColumn.create([1, 1, 1])]).matrix;
  Display(matrix1.Add(matrix2).ToString);

  Display(matrix1.Multiply(matrix2).ToString);

  matrix1 := TBind.New([TColumn.create([1, 7, 4]), TColumn.create([8, 6, 3]), TColumn.create([12, 8, 6])]).matrix;
  Display(matrix1.Transpose.ToString);
  Display(matrix1.Mean.ToString);
  Display(matrix1.Covariance.ToString);
  Display(matrix1.Distance.ToString);

  matrix1 := TBind.New([TColumn.create([4, 4.2, 3.9, 4.3, 4.1]), TColumn.create([2, 2.1, 2, 2.1, 2.2]), TColumn.create([0.60, 0.59, 0.58, 0.62, 0.63])]).matrix;
  Display(matrix1.Covariance.ToString);

end;

procedure TForm1.Button4Click(Sender: TObject);
var
  table: ITable;
  i, j: Integer;
begin
  table := TTable.Create(5, 10);

  for i := 0 to 4 do
  begin
      table.Cell[i, 0] := IntToStr(i);
      table.Cell[i, 1] := ' Col ' + IntToStr(i);
  end;

  for i := 0 to 4 do
  begin
    for j := 2 to 9 do
    begin
      table.Cell[i, j] := IntToStr((Random(2)*-1) + Random(10000)) + '.' + IntToStr(Random(20));
    end;
  end;
  Display(table.ToString);

  Display('filtering -> Col 3 >= 1000');
  table := table.filter(' Col 3', '100', '>=');
  Display(table.ToString);

  Display('filtering -> Col 1 < 5000');
  table := table.filter(' Col 1', '5000', '<');
  Display(table.ToString);
end;

procedure TForm1.Display(s : string);
begin
  Memo1.Lines.Add(s);
end;

end.
