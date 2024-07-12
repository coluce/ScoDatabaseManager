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
    class function Script(const ATableName, AFieldName, AFieldType: string): IModelScript;
    class function DataBaseBackup(ADataBaseInfo: TDatabase;
      const ADllDatabasePath: string): IModelDatabaseBackup;
  end;

implementation

uses
  Model.Imp.Connection.SQLite, System.SysUtils, Model.Imp.Updater,
  Model.Imp.Script, Model.Imp.Table, Model.Imp.Connection.Firebird,
  Model.Imp.DataBase.Backup;

{ TModelFactory }

class function TModelFactory.MainConnection: IModelConnection;
var
  vFirebirdDatabase: TDatabase;
begin

  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := TModelFactory.SQLite;
    TModelConnectionType.Firebird:
      begin
        { esses dados abaixo n�o s�o utilizados }
        { est�o aqui como exemplo, para futura implementa��o para a possibilidade de usar o Firebird para guardar as configura��es }
        vFirebirdDatabase.ID := TGUID.NewGuid.ToString;
        vFirebirdDatabase.Server := TServer.Create(
          TGUID.NewGuid.ToString,
          'Localhost',
          '127.0.0.1',
          3050,
          'SYSDBA',
          'masterkey'
        );
        vFirebirdDatabase.Name := 'Local';
        vFirebirdDatabase.Path := 'E:\Database\config.db';
        Result := TModelFactory.Firebird(vFirebirdDatabase);
      end;
  end;

end;

class function TModelFactory.Script(const ATableName, AFieldName,
  AFieldType: string): IModelScript;
begin
  Result := TModelScript.Create('alter table ' + ATableName + ' add ' + AFieldName + ' ' + AFieldType);
end;

class function TModelFactory.SQLite: IModelConnection;
begin
  Result := TModelConnectionSQLite.Create;
end;

class function TModelFactory.Table(const ATableName: string): IModelTable;
begin
  Result := TModelTable.Create(ATableName);
end;

class function TModelFactory.Updater: IModelStructureUpdater;

{ TODO : criar uma classe de implementacao pra isso }
  function CreateSQLite: IModelStructureUpdater;
  begin
    Result := TModelStrcutureUpdater.Create;
    Result.AddScript(TModelScript.Create('create table if not exists TSERVER (ID text primary key, NAME text, IP text, PORT int, USERNAME text, PASSWORD text)'));
    Result.AddScript(TModelScript.Create('create table if not exists TDATABASE (ID text primary key, ID_SERVER text, NAME text, PATH text)'));
    Result.AddScript(TModelScript.Create('create table if not exists TLAYOUT (ID text primary key, NAME varchar(50), LAYOUT varchar(5000))'));
    Result.AddScript(TModelScript.Create('create table if not exists TPARAM (SESSION varchar(100), KEY varchar(100), VALUE varchar(5000))'));
    Result.AddScript(TModelScript.Create('create table if not exists TQUERY_HISTORY (ID text primary key, DATA datetime, QUERY varchar(5000))'));
  end;

  function CreateFirebird: IModelStructureUpdater;
  begin
    Result := nil;
    raise Exception.Create('Updater Firebird n�o implementado');
  end;

begin
  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite:
      Result := CreateSQLite;
    TModelConnectionType.Firebird:
      Result := CreateFirebird;
  end;
end;

class function TModelFactory.DataBaseBackup(ADataBaseInfo: TDatabase;
  const ADllDatabasePath: string): IModelDatabaseBackup;
begin
  Result := TModelDataBaseBackup.Create(ADataBaseInfo, ADllDatabasePath);
end;

class function TModelFactory.Firebird(ADataBase: TDatabase): IModelConnection;
begin
  Result := TModelConnectionFirebird.Create(ADataBase);
end;

end.
