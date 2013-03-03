program thundax.ai.testSpanningTree;

uses
  Forms,
  testSpanningTree in 'testSpanningTree.pas' {frmSpanning};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := true;
  Application.Title := 'test minimum spanning tree';
  Application.CreateForm(TfrmSpanning, frmSpanning);
  Application.Run;
end.
