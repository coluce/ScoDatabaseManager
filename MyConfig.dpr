program MyConfig;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Principal in 'View\Form.Principal.pas' {FormPrincipal},
  Form.Config in 'View\Form.Config.pas' {FormConfig},
  Form.Query in 'View\Form.Query.pas' {FormQuery},
  Form.Padrao in 'View\Form.Padrao.pas' {FormPadrao},
  Form.FileLayout in 'View\Form.FileLayout.pas' {FormFileLayout},
  Model.Config in 'Model\Model.Config.pas',
  Model.Conexao in 'Model\Model.Conexao.pas',
  Controller.Conexao in 'Controller\Controller.Conexao.pas',
  Model.FileLayout in 'Model\Model.FileLayout.pas',
  Model.DAO in 'Model\Model.DAO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.CreateForm(TFormPadrao, FormPadrao);
  Application.CreateForm(TFormFileLayout, FormFileLayout);
  Application.Run;
end.
