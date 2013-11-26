program thundax.ai.testMatrix;

uses
  Forms,
  testMatrix in 'testMatrix.pas' {frmTestMatrix};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := true;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestMatrix, frmTestMatrix);
  Application.Run;
end.
