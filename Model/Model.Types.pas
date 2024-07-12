unit Model.Types;

interface

type
  TModelConnectionType = (SQLite, Firebird);

  TServer = record
    ID: string;
    Name: string;
    IP: string;
    Port: Integer;
    UserName: string;
    Password: string;
  public
    constructor Create(const AID, AName, AIP: string; const APort: Integer; const AUserName, APassword: string);
  end;

  TDataBase = record
    ID: string;
    Server: TServer;
    Name: string;
    Path: string;
  public
    constructor Create(const AID, AName, APath: string; AServer: TServer);
  end;

  TTableParam = record
    FieldName: string;
    FieldValue: string;
    constructor Create(const AFieldName, AFieldValue: string);
  end;

var
  ConnectionType: TModelConnectionType;

implementation

{ TServer }

constructor TServer.Create(const AID, AName, AIP: string; const APort: Integer; const AUserName, APassword: string);
begin
  Self.ID := AID;
  Self.Name := AName;
  Self.IP := AIP;
  Self.Port := APort;
  Self.UserName := AUserName;
  Self.Password := APassword;
end;

{ TDataBase }

constructor TDataBase.Create(const AID, AName, APath: string; AServer: TServer);
begin
  Self.ID := AID;
  Self.Name := AName;
  Self.Path := APath;
  Self.Server := AServer;
end;

{ TTableParam }

constructor TTableParam.Create(const AFieldName, AFieldValue: string);
begin
  Self.FieldName := AFieldName;
  Self.FieldValue := AFieldValue;
end;

end.
