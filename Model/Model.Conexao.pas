unit Model.Conexao;

interface

uses
  Model.Interfaces,
  FireDAC.Comp.Client;

type

  TConexao = class(TInterfacedObject, IConexao)
  private
    FConexao: TFDConnection;
    FConfig: IDataBaseConfig;
    function GetBanco: TFDConnection;
    procedure SetupConnection;
  public
    constructor Create(const AConfig: IDataBaseConfig);
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  published
    property Banco: TFDConnection read GetBanco;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TConexao }

procedure TConexao.Close;
begin
  FConexao.Close;
end;

constructor TConexao.Create(const AConfig: IDataBaseConfig);
begin
  FConfig := AConfig;
  FConexao := TFDConnection.Create(nil);
end;

destructor TConexao.Destroy;
begin
  if FConexao.Connected then
  begin
    FConexao.Close;
  end;
  FreeAndNil(FConexao);
  inherited;
end;

function TConexao.GetBanco: TFDConnection;
begin
  Result := FConexao;
end;

procedure TConexao.Open;
begin
  SetupConnection;
  FConexao.Open;
end;

procedure TConexao.SetupConnection;
begin
  FConexao.DriverName := 'FB';
  FConexao.LoginPrompt := False;
  FConexao.Params.Database := TPath.Combine(FConfig.DataBase,'ALTERDB.IB');
  FConexao.Params.Values['Server'] := FConfig.ServerName;
  FConexao.Params.UserName := 'SYSDBA';
  FConexao.Params.Password := 'masterkey';
end;

end.
