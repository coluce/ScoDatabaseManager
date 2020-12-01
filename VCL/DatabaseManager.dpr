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
  Model.Updater in 'Model\Model.Updater.pas',
  View.Database.Manager in 'View\View.Database.Manager.pas' {ViewDatabaseManager},
  Controller.DataBase in 'Controller\Controller.DataBase.pas',
  Controller.Interfaces in 'Controller\Controller.Interfaces.pas',
  Controller.Factory in 'Controller\Controller.Factory.pas',
  Model.Connection.Firebird in 'Model\Model.Connection.Firebird.pas',
  View.Layout in 'View\View.Layout.pas' {ViewLayout},
  Controller.Layout in 'Controller\Controller.Layout.pas',
  View.Ini in 'View\View.Ini.pas' {ViewIni},
  Controller.Ini in 'Controller\Controller.Ini.pas',
  Controller.Param in 'Controller\Controller.Param.pas',
  View.Default in 'View\View.Default.pas' {ViewDefault},
  Controller.Window in 'Controller\Controller.Window.pas',
  View.Server in 'View\View.Server.pas' {ViewServer},
  View.Database.Register in 'View\View.Database.Register.pas' {ViewRegisterDatabase},
  Model.Database.Manager in 'Model\Model.Database.Manager.pas';

{$R *.res}

var
  vUpdater: IModelStructureUpdater;
begin

  ReportMemoryLeaksOnShutdown := True;

  Model.Types.ConnectionType := TModelConnectionType.SQLite;

  vUpdater := TModelFactory.Updater;
  vUpdater.Execute;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewPrincipal, ViewPrincipal);
  Application.Run;
end.
