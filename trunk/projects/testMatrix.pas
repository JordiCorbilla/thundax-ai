unit testMatrix;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
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
  matrix : TMatrix;
  c1, c2, c3 : TColumn;
  bind : TMatrix;
begin
  //Create simple matrix and display it
  matrix := TMatrix.Create(3,3);

  c1 := TColumn.create([1,2,3]);
  c2 := TColumn.create([4,5,6]);
  c3 := TColumn.Create([7,8,9]);

  bind := TBind.New([c1,c2,c3]);
  Memo1.Lines.Add(bind.ToString);

//  matrix.Cell[0,0] := 1;
//  matrix.Cell[1,0] := 2;
//  matrix.Cell[2,0] := 3;
//  matrix.Cell[0,1] := 4;
//  matrix.Cell[1,1] := 5;
//  matrix.Cell[2,1] := 6;
//  matrix.Cell[0,2] := 7;
//  matrix.Cell[1,2] := 8;
//  matrix.Cell[2,2] := 9;

//  Memo1.Lines.Add(matrix.ToString);
//  Memo1.Lines.Add(matrix.Transpose.ToString);
end;

end.
