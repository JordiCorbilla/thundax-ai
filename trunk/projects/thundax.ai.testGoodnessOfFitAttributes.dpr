program thundax.ai.testGoodnessOfFitAttributes;

uses
  Forms,
  testGoodnessOfFitAttributes in 'testGoodnessOfFitAttributes.pas' {frmGoodness};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGoodness, frmGoodness);
  Application.Run;
end.
