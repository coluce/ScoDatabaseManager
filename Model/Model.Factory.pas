unit Model.Factory;

interface

uses
  Model.Interfaces;

type

  TModelFactory = class
  public
    class function Config: IDataBaseConfig;
    class function Dao: IDao<IDataBaseConfig>;
    class function Conexao(const AConfig: IDataBaseConfig): IConexao;
  end;

implementation

uses
  Model.Config,
  Model.Conexao;

{ TConficFactory }

class function TModelFactory.Dao: IDao<IDataBaseConfig>;
begin
  Result := TDataBaseConfigDao.Create;
end;

class function TModelFactory.Conexao(const AConfig: IDataBaseConfig): IConexao;
begin
  Result := TConexao.Create(AConfig);
end;

class function TModelFactory.Config: IDataBaseConfig;
begin
  Result := TDataBaseConfig.Create;
end;

end.
