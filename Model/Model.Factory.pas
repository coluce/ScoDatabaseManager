unit Model.Factory;

interface

uses
  Model.Interfaces;

type

  TDataBaseConfigFactory = class
  public
    class function Config: IDataBaseConfig;
    class function Dao: IDao<IDataBaseConfig>;
  end;

implementation

uses
  Model.Config;

{ TConficFactory }

class function TDataBaseConfigFactory.Dao: IDao<IDataBaseConfig>;
begin
  Result := TDataBaseConfigDao.Create;
end;

class function TDataBaseConfigFactory.Config: IDataBaseConfig;
begin
  Result := TDataBaseConfig.Create;
end;

end.
