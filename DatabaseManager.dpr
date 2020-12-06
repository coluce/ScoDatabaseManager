program DatabaseManager;

uses
  Vcl.Forms,
  View.Main in 'View\View.Main.pas' {ViewMain},
  Controller.Imp.Main in 'Controller\Controller.Imp.Main.pas',
  Model.Imp.Connection.SQLite in 'Model\Model.Imp.Connection.SQLite.pas',
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Model.Imp.Table in 'Model\Model.Imp.Table.pas',
  Model.Imp.Script in 'Model\Model.Imp.Script.pas',
  Model.Types in 'Model\Model.Types.pas',
  Model.Imp.Updater in 'Model\Model.Imp.Updater.pas',
  View.Query in 'View\View.Query.pas' {ViewQuery},
  Controller.Imp.Query in 'Controller\Controller.Imp.Query.pas',
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
  View.Backup.Manager in 'View\View.Backup.Manager.pas' {ViewBackupManager},
  Controller.Imp.Backup.Manager in 'Controller\Controller.Imp.Backup.Manager.pas',
  Controller.Imp.Param.Manager in 'Controller\Controller.Imp.Param.Manager.pas',
  View.Param.Manager in 'View\View.Param.Manager.pas' {ViewParamManager},
  View.Wait in 'View\View.Wait.pas' {ViewWait},
  View.Query.Param in 'View\View.Query.Param.pas' {ViewQueryParam};

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
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
