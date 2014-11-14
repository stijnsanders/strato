program xsView;

uses
  Forms,
  xsViewMain in 'xsViewMain.pas' {Form1},
  stratoDecl in 'stratoDecl.pas',
  stratoSphere in 'stratoSphere.pas',
  stratoDebug in 'stratoDebug.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
