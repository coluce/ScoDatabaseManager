unit Model.Conexao;

interface

uses
  FireDAC.Comp.Client;

type
  IConexao = interface
    ['{0B477AC4-2578-4BFC-9846-846088A22A9B}']

    function GetBanco: TFDConnection;
    function GetFileName: string;

    property Banco: TFDConnection read GetBanco;
    property FileName: string read GetFileName;
  end;

  TConexao = class(TInterfacedObject, IConexao)
  private
    FConexao: TFDConnection;
    function GetBanco: TFDConnection;
    function GetFileName: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  published
    property Banco: TFDConnection read GetBanco;
    property FileName: string read GetFileName;
  end;

implementation

uses
  System.SysUtils;

{ TConexao }

constructor TConexao.Create(const AFileName: string);
begin
  FConexao := TFDConnection.Create(nil);
  FConexao.DriverName := 'SQLite';
  FConexao.LoginPrompt := False;
  FConexao.Params.Database := AFileName;
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

function TConexao.GetFileName: string;
begin
  Result := FConexao.Params.Database;
end;

end.
