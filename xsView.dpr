program xsView;

uses
  Forms,
  xsViewMain in 'xsViewMain.pas' {frmXsViewMain},
  stratoDecl in 'stratoDecl.pas',
  stratoSphere in 'stratoSphere.pas',
  stratoDebug in 'stratoDebug.pas',
  stratoTools in 'stratoTools.pas',
  stratoTokenizer in 'stratoTokenizer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmXsViewMain, frmXsViewMain);
  Application.Run;
end.
