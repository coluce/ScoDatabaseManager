program DatabaseManager;

uses
  Vcl.Forms,
  View.Principal in 'View\View.Principal.pas' {ViewPrincipal},
  Controller.Imp.Principal in 'Controller\Controller.Imp.Principal.pas',
  Model.Imp.Connection.SQLite in 'Model\Model.Imp.Connection.SQLite.pas',
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Model.Imp.Table in 'Model\Model.Imp.Table.pas',
  Model.Imp.Script in 'Model\Model.Imp.Script.pas',
  Model.Types in 'Model\Model.Types.pas',
  Model.Imp.Updater in 'Model\Model.Imp.Updater.pas',
  View.Database.Data in 'View\View.Database.Data.pas' {ViewDatabaseData},
  Controller.Imp.DataBase.Data in 'Controller\Controller.Imp.DataBase.Data.pas',
  Controller.Interfaces in 'Controller\Controller.Interfaces.pas',
  Controller.Factory in 'Controller\Controller.Factory.pas',
  Model.Imp.Connection.Firebird in 'Model\Model.Imp.Connection.Firebird.pas',
  View.Layout in 'View\View.Layout.pas' {ViewLayout},
  Controller.Imp.Layout in 'Controller\Controller.Imp.Layout.pas',
  View.Ini in 'View\View.Ini.pas' {ViewIni},
  Controller.Imp.Ini in 'Controller\Controller.Imp.Ini.pas',
  Controller.Imp.Param in 'Controller\Controller.Imp.Param.pas',
  View.Default in 'View\View.Default.pas' {ViewDefault},
  Controller.Imp.Window in 'Controller\Controller.Imp.Window.pas',
  View.Server in 'View\View.Server.pas' {ViewServer},
  View.Database.Register in 'View\View.Database.Register.pas' {ViewRegisterDatabase},
  Model.Imp.Database.Backup in 'Model\Model.Imp.Database.Backup.pas',
  View.Database.Backup in 'View\View.Database.Backup.pas' {ViewDataBaseBackup},
  Controller.Imp.DataBase.Backup in 'Controller\Controller.Imp.DataBase.Backup.pas';

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
