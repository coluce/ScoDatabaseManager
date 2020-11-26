unit Model.Factory;

interface

uses
  Model.Interfaces, Model.Types;

type

  TModelConnectionFactory = class
  private
    class function SQLite: IModelConnection;
    class function Firebird: IModelConnection;    
  public
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
  Model.Script, Model.Table;

{ TModelConnectionFactory }

class function TModelConnectionFactory.New: IModelConnection;
begin

  case Model.Types.ConnectionType of
    TModelConnectionType.SQLite: Result := TModelConnectionFactory.SQLite;
    TModelConnectionType.Firebird: Result := TModelConnectionFactory.Firebird;
  end;

end;

class function TModelConnectionFactory.SQLite: IModelConnection;
begin
  Result := TModelConnectionSQLite.Create;
end;

class function TModelConnectionFactory.Firebird: IModelConnection;
begin
  Result := nil;
  raise Exception.Create('Conexão ao banco Firebird não implementada!');
end;

{ TModelStructureUpdaterFactory }

class function TModelStructureUpdaterFactory.SQLite: IModelStructureUpdater;
begin
  Result := TModelStrcutureUpdater.Create;
  Result.AddScript(TModelScript.Create('create table if not exists TSERVER (ID text primaty key, NAME text, IP text)'));
  Result.AddScript(TModelScript.Create('create table if not exists TDATABASE (ID text primaty key, ID_SERVER text, NAME text, PATH text, USERNAME text, PASSWORD text)'));
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
