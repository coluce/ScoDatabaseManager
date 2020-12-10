program DatabaseManager;

uses
  Vcl.Forms,

  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas',
  Model.Types in 'Model\Model.Types.pas',

  Model.Imp.Connection.SQLite in 'Model\Imp\Model.Imp.Connection.SQLite.pas',
  Model.Imp.Connection.Firebird in 'Model\Imp\Model.Imp.Connection.Firebird.pas',
  Model.Imp.Table in 'Model\Imp\Model.Imp.Table.pas',
  Model.Imp.Script in 'Model\Imp\Model.Imp.Script.pas',
  Model.Imp.Updater in 'Model\Imp\Model.Imp.Updater.pas',
  Model.Imp.Firebird.Functions in 'Model\Imp\Model.Imp.Firebird.Functions.pas',
  Model.Imp.Database.Backup in 'Model\Imp\Model.Imp.Database.Backup.pas',

  View.Main in 'View\View.Main.pas' {ViewMain},
  View.Default in 'View\View.Default.pas' {ViewDefault},
  View.Query in 'View\View.Query.pas' {ViewQuery},
  View.Layout in 'View\View.Layout.pas' {ViewLayout},
  View.Ini in 'View\View.Ini.pas' {ViewIni},
  View.Server in 'View\View.Server.pas' {ViewServer},
  View.Database.Register in 'View\View.Database.Register.pas' {ViewRegisterDatabase},
  View.Backup.Manager in 'View\View.Backup.Manager.pas' {ViewBackupManager},
  View.Param.Manager in 'View\View.Param.Manager.pas' {ViewParamManager},
  View.Wait in 'View\View.Wait.pas' {ViewWait},
  View.Query.Param in 'View\View.Query.Param.pas' {ViewQueryParam},

  Controller.Interfaces in 'Controller\Controller.Interfaces.pas',
  Controller.Factory in 'Controller\Controller.Factory.pas',
  Controller.Imp.Query in 'Controller\Imp\Controller.Imp.Query.pas',
  Controller.Imp.Layout in 'Controller\Imp\Controller.Imp.Layout.pas',
  Controller.Imp.Ini in 'Controller\Imp\Controller.Imp.Ini.pas',
  Controller.Imp.Param in 'Controller\Imp\Controller.Imp.Param.pas',
  Controller.Imp.Window in 'Controller\Imp\Controller.Imp.Window.pas',
  Controller.Imp.Backup.Manager in 'Controller\Imp\Controller.Imp.Backup.Manager.pas',
  Controller.Imp.Param.Manager in 'Controller\Imp\Controller.Imp.Param.Manager.pas',
  Controller.Imp.Main in 'Controller\Imp\Controller.Imp.Main.pas';

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
