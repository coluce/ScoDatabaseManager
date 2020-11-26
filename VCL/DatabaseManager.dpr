program DatabaseManager;

uses
  Vcl.Forms,
  View.Principal in 'View\View.Principal.pas' {ViewPrincipal},
  Controller.Principal in 'Controller\Controller.Principal.pas',
  Model.Connection.SQLite in 'Model\Model.Connection.SQLite.pas',
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Model.Table in 'Model\Model.Table.pas',
  Model.Script in 'Model\Model.Script.pas',
  Model.Types in 'Model\Model.Types.pas',
  Model.Updater in 'Model\Model.Updater.pas';

{$R *.res}

var
  vUpdater: IModelStructureUpdater;
begin

  Model.Types.ConnectionType := TModelConnectionType.SQLite;

  vUpdater := TModelStructureUpdaterFactory.New;
  vUpdater.Execute;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewPrincipal, ViewPrincipal);
  Application.Run;
end.
