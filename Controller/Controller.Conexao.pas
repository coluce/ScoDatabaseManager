unit Controller.Conexao;

interface

uses
  Controller.Interfaces,
  Model.Interfaces;

type

  TControllerConexao = class(TInterfacedObject, IControllerConexao)
  private
    FConfig: IDataBaseConfig;
    FConexao: IConexao;
    function GetConexao: IConexao;
  public
    constructor Create(const AConfig: IDataBaseConfig);
    function TestConnection: boolean;
  published
    property Conexao: IConexao read GetConexao;
  end;

implementation

uses
  Model.Factory;

{ TControllerConexao }

constructor TControllerConexao.Create(const AConfig: IDataBaseConfig);
begin
  FConfig := AConfig;
  FConexao := TModelFactory.Conexao(FConfig);
end;

function TControllerConexao.GetConexao: IConexao;
begin
  Result := FConexao;
end;

function TControllerConexao.TestConnection: boolean;
begin
  Result := False;
  try
    try
      FConexao.SetupConnection;
      FConexao.Open;
      Result := FConexao.Banco.Connected;
    except
      Result := False;
    end;
  finally
    FConexao.Close;
  end;
end;

end.
