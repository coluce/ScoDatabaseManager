program MyConfig;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Principal in 'View\Form.Principal.pas' {FormPrincipal},
  View.Config in 'View\View.Config.pas' {FormConfig},
  View.Query in 'View\View.Query.pas' {FormQuery},
  View.Default in 'View\View.Default.pas' {FormPadrao},
  View.FileLayout in 'View\View.FileLayout.pas' {ViewFileLayout},
  Model.Config in 'Model\Model.Config.pas',
  Model.Conexao in 'Model\Model.Conexao.pas',
  Controller.Conexao in 'Controller\Controller.Conexao.pas',
  Model.FileLayout in 'Model\Model.FileLayout.pas',
  Model.DAO in 'Model\Model.DAO.pas',
  SCO.FMX.MainLayout in 'View\Componentes\SCO.FMX.MainLayout.pas',
  Controller.Principal in 'Controller\Controller.Principal.pas',
  View.Menu in 'View\View.Menu.pas' {ViewMenu};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.CreateForm(TFormPadrao, FormPadrao);
  Application.CreateForm(TViewFileLayout, ViewFileLayout);
  Application.CreateForm(TViewMenu, ViewMenu);
  Application.Run;
end.
