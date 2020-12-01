unit Model.Factory;

interface

uses
  Model.Interfaces, Model.Types;

type

  TModelFactory = class
  public
    class function SQLite: IModelConnection;
    class function Firebird(ADataBase: TDatabase): IModelConnection;
    class function MainConnection: IModelConnection;
    class function Updater: IModelStructureUpdater;
    class function Table(const ATableName: string): IModelTable;
    class function DataBaseManager(ADataBaseInfo: TDataBase): IModelDatabaseManager;
  end;

implementation

uses
  Model.Connection.SQLite, System.SysUtils, Model.Updater,
  Model.Script, Model.Table, Model.Connection.Firebird,
  Model.DataBase.Manager;

{ TModelFactory }

class function TModelFactory.MainConnection: IModelConnection;
var
  vFirebirdDatabase: TDataBase;
begin

  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := TModelFactory.SQLite;
    TModelConnectionType.Firebird:
    begin
      {esses dados abaixo não são utilizados}
      {estão aqui como exemplo, para futura implementação para a possibilidade de usar o Firebird para guardar as configurações}
      vFirebirdDatabase.ID := TGUID.NewGuid.ToString;
      vFirebirdDatabase.Server := TServer.Create(TGUID.NewGuid.ToString, 'Localhost', '127.0.0.1)');
      vFirebirdDatabase.Name := 'Local';
      vFirebirdDatabase.Path := 'E:\Database\config.db';
      vFirebirdDatabase.UserName := 'SYSDBA';
      vFirebirdDatabase.Password := 'MASTERKEY';
      Result := TModelFactory.Firebird(vFirebirdDatabase);
    end;
  end;

end;

class function TModelFactory.SQLite: IModelConnection;
begin
  Result := TModelConnectionSQLite.Create;
end;

class function TModelFactory.Table(
  const ATableName: string): IModelTable;
begin
  Result := TModelTable.Create(ATableName);
end;

class function TModelFactory.Updater: IModelStructureUpdater;

  { TODO : criar uma classe de implementacao pra isso }
  function CreateSQLite: IModelStructureUpdater;
  begin
    Result := TModelStrcutureUpdater.Create;
    Result.AddScript(TModelScript.Create('create table if not exists TSERVER (ID text primary key, NAME text, IP text)'));
    Result.AddScript(TModelScript.Create('create table if not exists TDATABASE (ID text primary key, ID_SERVER text, NAME text, PATH text, USERNAME text, PASSWORD text)'));
    Result.AddScript(TModelScript.Create('create table if not exists TLAYOUT (ID text primary key, NAME varchar(50), LAYOUT varchar(5000))'));
    Result.AddScript(TModelScript.Create('create table if not exists TPARAM (SESSION varchar(100), KEY varchar(100), VALUE varchar(5000))'));
  end;

  function CreateFirebird: IModelStructureUpdater;
  begin
    Result := nil;
    raise Exception.Create('Updater Firebird não implementado');
  end;

begin
  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := CreateSQLite;
    TModelConnectionType.Firebird: Result := CreateFirebird;
  end;
end;

class function TModelFactory.DataBaseManager(ADataBaseInfo: TDataBase): IModelDatabaseManager;
begin
  Result := TModelDataBaseManager.Create(ADataBaseInfo);
end;

class function TModelFactory.Firebird(ADataBase: TDatabase): IModelConnection;
begin
  Result := TModelConnectionFirebird.Create(ADataBase);
end;

end.
