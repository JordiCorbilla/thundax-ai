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
  matrix : IMatrix;
  c1, c2, c3 : IColumn;
  bind : IBind;
begin
  c1 := TColumn.create([1,2]);
  c2 := TColumn.create([4,5]);
  c3 := TColumn.Create([7,8]);

  bind := TBind.New([c1,c2,c3]);
  matrix := bind.matrix;

  Memo1.Lines.Add(matrix.ToString);
  Memo1.Lines.Add(matrix.transpose.ToString);

  Memo1.Lines.Add(TBind.New([TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5])]).matrix.ToString);
  Memo1.Lines.Add(TBind.New([TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5]), TColumn.create([1,2,3,4,5])]).matrix.transpose.ToString);
end;

end.
