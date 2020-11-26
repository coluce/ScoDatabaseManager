unit Model.Connection.SQLite;

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.DApt,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  Model.Interfaces;

type
  TModelConnectionSQLite = class(TInterfacedObject, IModelConnection)
  private
    FConection: TFDCustomConnection;

    function GetActive: boolean;
    procedure SetActive(const Value: boolean);

  public
    constructor Create;
    destructor Destroy; override;

    function GetConnection: TFDCustomConnection;
    function ExecScript(const AScript: IModelScript): boolean;

    procedure Close;
    procedure Open;

    property Active: boolean read GetActive write SetActive;

  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TModelConexao }

procedure TModelConnectionSQLite.Close;
begin
  FConection.Close;
end;

constructor TModelConnectionSQLite.Create;
begin
  FConection := TFDCustomConnection.Create(nil);
  FConection.DriverName := 'SQLite';
  FConection.Params.Database := TPath.ChangeExtension(ParamStr(0),'.sqlite');
end;

destructor TModelConnectionSQLite.Destroy;
begin
  FreeAndNil(FConection);
  inherited;
end;

function TModelConnectionSQLite.ExecScript(const AScript: IModelScript): boolean;
  function IsAlreadyExecuted(const AID: string): boolean;
  begin
    {verificar se ja existe na tabela de scripts}
    Result := False;
  end;

  procedure RegisterScript(const AID: string);
  begin
    {gravar ID na tabela de scripts na tabela de scripts}
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

function TModelConnectionSQLite.GetActive: boolean;
begin

end;

function TModelConnectionSQLite.GetConnection: TFDCustomConnection;
begin
  Result := FConection;
end;

procedure TModelConnectionSQLite.Open;
begin
  FConection.Open;
end;

procedure TModelConnectionSQLite.SetActive(const Value: boolean);
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
