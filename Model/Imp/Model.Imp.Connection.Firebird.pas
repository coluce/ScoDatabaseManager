unit Model.Imp.Connection.Firebird;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.DApt,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  Model.Interfaces,
  Model.Types;

type

  { TODO : criar classe ancestral, tem muito codigo repetido }

  TModelConnectionFirebird = class(TInterfacedObject, IModelConnection)
  private
    FConection: TFDCustomConnection;
    FDataBase: TDataBase;

    function GetActive: boolean;
    procedure SetActive(const Value: boolean);

  public
    constructor Create(ADataBase: TDataBase);
    destructor Destroy; override;

    function GetConnection: TFDCustomConnection;
    function ExecScript(const AScript: IModelScript): boolean;

    procedure Close;
    procedure Open;

    property Active: boolean read GetActive write SetActive;

  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TModelConnectionFirebird }

procedure TModelConnectionFirebird.Close;
begin
  FConection.Close;
end;

constructor TModelConnectionFirebird.Create(ADataBase: TDataBase);
var
  LConnectionString: string;
begin
  FDataBase := ADataBase;
  FConection := TFDCustomConnection.Create(nil);
  FConection.LoginPrompt := False;

  LConnectionString :=
     'Database=' + FDataBase.DatabaseFile + ';' +
     'User_Name=' + FDataBase.Server.UserName + ';' +
     'Password=' + FDataBase.Server.Password + ';' +
     'Server=' + FDataBase.Server.IP + ';' +
     'Port=' + FDataBase.Server.Port.ToString + ';' +
     'Protocol=TCPIP;' +
     'CharacterSet=UTF8;' +
     'DriverID=FB';

  if FConection.Connected then
    FConection.Close;

  FConection.ConnectionString := LConnectionString;

end;

destructor TModelConnectionFirebird.Destroy;
begin
  FreeAndNil(FConection);
  inherited;
end;

function TModelConnectionFirebird.ExecScript(const AScript
  : IModelScript): boolean;

  function IsAlreadyExecuted(const AID: string): boolean;
  begin
    { verificar se ja existe na tabela de scripts }
    Result := False;
  end;

  procedure RegisterScript(const AID: string);
  begin
    { gravar ID na tabela de scripts na tabela de scripts }
  end;

begin
  Result := False;
  if not IsAlreadyExecuted(AScript.ID) then
  begin
    FConection.ExecSQL(AScript.SQL.Text);
    RegisterScript(AScript.ID);
    Result := True;
  end;
end;

function TModelConnectionFirebird.GetActive: boolean;
begin
  Result := FConection.Connected;
end;

function TModelConnectionFirebird.GetConnection: TFDCustomConnection;
begin
  Result := FConection;
end;

procedure TModelConnectionFirebird.Open;
begin
  FConection.Open;
end;

procedure TModelConnectionFirebird.SetActive(const Value: boolean);
begin
  if Value then
  begin
    Self.Open;
  end
  else
  begin
    Self.Close;
  end;
end;

end.
