unit Model.Table;

interface

uses
  Model.Interfaces, FireDAC.Comp.Client, Data.DB;

type
  TModelTable = class(TInterfacedObject, IModelTable)
  private
    FTableName: string;
    FConnection: IModelConnection;
    FQuery: TFDQuery;
    function GetDataSet: TDataSet;
  public
    constructor Create(const ATableName: string);
    destructor Destroy; override;
    function ApplyUpdates: boolean;
    procedure Open(const AWhere: string);
    function Delete(const AID: string): boolean;
  published
    property DataSet: TDataset read GetDataSet;
  end;

implementation

uses
  Model.Factory, System.SysUtils;

{ TModelTable }

function TModelTable.ApplyUpdates: boolean;
begin
  Result := FQuery.ApplyUpdates(0) = 0;
end;

constructor TModelTable.Create(const ATableName: string);
begin
  FTableName := ATableName;
  FConnection := TModelConnectionFactory.New;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection.GetConnection;
  FQuery.CachedUpdates := True;
  FQuery.SQL.Text := 'select * from ' + FTableName;
  FQuery.Open;
end;

function TModelTable.Delete(const AID: string): boolean;
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add('delete from ' + FTableName);
  FQuery.SQL.Add('where');
  FQuery.SQL.Add('  ID = :ID');
  FQuery.ParamByName('ID').AsString := AID;
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected > 0;
end;

destructor TModelTable.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

function TModelTable.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TModelTable.Open(const AWhere: string);
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add('select * from ' + FTableName);
  if not AWhere.Trim.IsEmpty then
  begin
    FQuery.SQL.Add('where');
    FQuery.SQL.Add(AWhere);
  end;
end;

end.
