program MyConfig;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Principal in 'View\Form.Principal.pas' {FormPrincipal},
  View.Config in 'View\View.Config.pas' {FormConfig},
  View.DataBase in 'View\View.DataBase.pas' {ViewDataBase},
  View.Default in 'View\View.Default.pas' {FormPadrao},
  View.FileLayout in 'View\View.FileLayout.pas' {ViewFileLayout},
  Model.Config in 'Model\Model.Config.pas',
  Model.Conexao in 'Model\Model.Conexao.pas',
  Controller.Conexao in 'Controller\Controller.Conexao.pas',
  Model.FileLayout in 'Model\Model.FileLayout.pas',
  SCO.FMX.MainLayout in 'View\Componentes\SCO.FMX.MainLayout.pas',
  Controller.Principal in 'Controller\Controller.Principal.pas',
  View.Menu in 'View\View.Menu.pas' {ViewMenu},
  Controller.Menu in 'Controller\Controller.Menu.pas',
  Model.Interfaces in 'Model\Model.Interfaces.pas',
  Model.Factory in 'Model\Model.Factory.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
