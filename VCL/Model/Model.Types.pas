unit Model.Types;

interface

type
  TModelConnectionType = (SQLite, Firebird);

  TServer = record
    ID: string;
    Name: string;
    IP: string;
  public
    constructor Create(const AID, AName, AIP: string);
  end;

var
  ConnectionType: TModelConnectionType;

implementation

{ TServer }

constructor TServer.Create(const AID, AName, AIP: string);
begin
  Self.ID := AID;
  Self.Name := AName;
  Self.IP := AIP;
end;

end.
