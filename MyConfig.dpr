program MyConfig;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Principal in 'Form.Principal.pas' {FormPrincipal},
  Form.Config in 'Form.Config.pas' {FormConfig},
  Form.Query in 'Form.Query.pas' {FormQuery},
  Model.Config in 'Model.Config.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
