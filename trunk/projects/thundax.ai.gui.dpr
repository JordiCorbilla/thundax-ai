program thundax.ai.gui;

{$APPTYPE CONSOLE}

uses
  SysUtils, StrUtils, Windows, graphics,
  thundax.ai.matrix, thundax.ai.matrix.columns, thundax.ai.matrix.bind;

var
  s : string;
  matrix: IMatrix;
  c1, c2, c3, c4, c5: IColumn;
  bind: IBind;
  procedure SetTextColour(colour : TColor = clwhite);
  begin
    case colour of
      clred:
      begin
        SetConsoleTextAttribute(GetStdHandle(
                              STD_OUTPUT_HANDLE),
                              FOREGROUND_RED or
                              FOREGROUND_INTENSITY);
      end;
      clGreen:
      begin
        SetConsoleTextAttribute(GetStdHandle(
                              STD_OUTPUT_HANDLE),
                              FOREGROUND_GREEN or
                              FOREGROUND_INTENSITY);
      end;
      clwhite:
      begin
        SetConsoleTextAttribute(GetStdHandle(
                              STD_OUTPUT_HANDLE),
                              FOREGROUND_RED or
                              FOREGROUND_GREEN or
                              FOREGROUND_BLUE);
      end;
    end;
  end;
begin
  try
    SetTextColour(clGreen);
    Writeln('This is thundax.ai.gui');
    SetTextColour(clWhite);
    Readln(s);
    while s <> 'exit' do
    begin
      SetTextColour(clWhite);
      Readln(s);
      if s = 'example' then
      begin
        c1 := TColumn.create([1, 0, 1, 1, 0, 0]);
        c2 := TColumn.create([1, 1, 1, 0, 1, 1]);
        c3 := TColumn.create([-1.645497224, -0.645497224, 1.290994449, -0.845497224, 1.290994449, -0.645557224]);
        c4 := TColumn.create([2.597414305, 1.792842914, -0.697614305, -0.797614305, -0.597614305, -0.797614305]);
        c5 := TColumn.create([1.89738712, -0.817753946, -0.808876973, 0.220626409, -0.208876973, -0.882505637]);

        bind := TBind.New([c1, c2, c3, c4, c5]);
        matrix := bind.matrix;

        SetTextColour(clRed);
        Writeln('Matrix:');
        Writeln(matrix.ToString);
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
