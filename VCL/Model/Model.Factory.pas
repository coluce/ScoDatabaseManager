unit Model.Factory;

interface

uses
  Model.Interfaces, Model.Types;

type

  TModelConnectionFactory = class
  private
  public
    class function SQLite: IModelConnection;
    class function Firebird(ADataBase: TDatabase): IModelConnection;
    class function New: IModelConnection;
  end;

  TModelStructureUpdaterFactory = class
  private
    class function SQLite: IModelStructureUpdater;
    class function Firebird: IModelStructureUpdater;
  public
    class function New: IModelStructureUpdater;
  end;

  TModelTableFactory = class
  public
    class function New(const ATableName: string): IModelTable;
  end;

implementation

uses
  Model.Connection.SQLite, System.SysUtils, Model.Updater,
  Model.Script, Model.Table, Model.Connection.Firebird;

{ TModelConnectionFactory }

class function TModelConnectionFactory.New: IModelConnection;
var
  vFirebirdDatabase: TDataBase;
begin

  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := TModelConnectionFactory.SQLite;
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
      Result := TModelConnectionFactory.Firebird(vFirebirdDatabase);
    end;
  end;

end;

class function TModelConnectionFactory.SQLite: IModelConnection;
begin
  Result := TModelConnectionSQLite.Create;
end;

class function TModelConnectionFactory.Firebird(ADataBase: TDatabase): IModelConnection;
begin
  Result := TModelConnectionFirebird.Create(ADataBase);
end;

{ TModelStructureUpdaterFactory }

class function TModelStructureUpdaterFactory.SQLite: IModelStructureUpdater;
begin
  Result := TModelStrcutureUpdater.Create;
  Result.AddScript(TModelScript.Create('create table if not exists TSERVER (ID text primaty key, NAME text, IP text)'));
  Result.AddScript(TModelScript.Create('create table if not exists TDATABASE (ID text primaty key, ID_SERVER text, NAME text, PATH text, USERNAME text, PASSWORD text)'));
  Result.AddScript(TModelScript.Create('create table if not exists TLAYOUT (ID text primaty key, NAME varchar(50), LAYOUT text)'));
end;

class function TModelStructureUpdaterFactory.Firebird: IModelStructureUpdater;
begin
  Result := nil;
  raise Exception.Create('Updater Firebird não implementado');
end;

class function TModelStructureUpdaterFactory.New: IModelStructureUpdater;
begin
  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := TModelStructureUpdaterFactory.SQLite;
    TModelConnectionType.Firebird: Result := TModelStructureUpdaterFactory.Firebird;
  end;
end;

{ TModelTableFactory }

class function TModelTableFactory.New(const ATableName: string): IModelTable;
begin
  Result := TModelTable.Create(ATableName);
end;

end.
