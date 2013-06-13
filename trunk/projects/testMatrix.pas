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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  thundax.ai.matrix, thundax.ai.matrix.columns, thundax.ai.matrix.bind;

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

procedure TForm1.Display(s : string);
begin
  Memo1.Lines.Add(s);
end;

end.
