unit Model.Types;

interface

type
  TModelConnectionType = (SQLite, Firebird);

  TServer = record
    ID: string;
    Name: string;
    IP: string;
    Port: Integer;
  public
    constructor Create(const AID, AName, AIP: string; const APort: Integer);
  end;

  TDataBase = record
    ID: string;
    Server: TServer;
    Name: string;
    Path: string;
    UserName: string;
    Password: string;
  public
    constructor Create(const AID, AName, APath, AUserName, APassword: string;
      AServer: TServer);
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

constructor TServer.Create(const AID, AName, AIP: string; const APort: Integer);
begin
  Self.ID := AID;
  Self.Name := AName;
  Self.IP := AIP;
  Self.Port := APort;
end;

{ TDataBase }

constructor TDataBase.Create(const AID, AName, APath, AUserName,
  APassword: string; AServer: TServer);
begin
  Self.ID := AID;
  Self.Name := AName;
  Self.Path := APath;
  Self.UserName := AUserName;
  Self.Password := APassword;
  Self.Server := AServer;
end;

{ TTableParam }

constructor TTableParam.Create(const AFieldName, AFieldValue: string);
begin
  Self.FieldName := AFieldName;
  Self.FieldValue := AFieldValue;
end;

end.
