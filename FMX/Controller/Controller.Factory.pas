unit Controller.Factory;

interface

uses
  Model.Interfaces,
  Controller.Interfaces;

type

  TControllerFactory = class
  public
    class function ControllerConexao(const ACofig: IDataBaseConfig): IControllerConexao;
  end;

implementation

uses
  Controller.Conexao;

{ TControllerFactory }

class function TControllerFactory.ControllerConexao(const ACofig: IDataBaseConfig): IControllerConexao;
begin
  Result := TControllerConexao.Create(ACofig);
end;

end.
