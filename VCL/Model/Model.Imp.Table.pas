unit Model.Imp.Table;

interface

uses
  Model.Interfaces, FireDAC.Comp.Client, Data.DB, Model.Types;

type
  TModelTable = class(TInterfacedObject, IModelTable)
  private
    FTableName: string;
    FConnection: IModelConnection;
    FQuery: TFDQuery;
    function GetDataSet: TDataSet;
    procedure DoAfterPost(DataSet: TDataSet);
    procedure DoOnNewRecord(DataSet: TDataSet);
    function ApplyUpdates: boolean;
  public
    constructor Create(const ATableName: string);
    destructor Destroy; override;
    procedure Open(const AWhere: string = '');
    function Delete(const AID: string): boolean;
    procedure Find(const AID: string); overload;
    procedure Find(AParams: TArray<TTableParam>); overload;
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
  FConnection := TModelFactory.MainConnection;
  FConnection.Open;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection.GetConnection;
  //FQuery.CachedUpdates := True;
  FQuery.AfterPost := DoAfterPost;
  FQuery.OnNewRecord := DoOnNewRecord;
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

procedure TModelTable.DoAfterPost(DataSet: TDataSet);
begin
  //Self.ApplyUpdates;
end;

procedure TModelTable.DoOnNewRecord(DataSet: TDataSet);
begin
  if Assigned(DataSet.FindField('ID')) then
  begin
    DataSet.FieldByName('ID').AsString := TGuid.NewGuid.ToString;
  end;
end;

procedure TModelTable.Find(AParams: TArray<TTableParam>);
var
  vParam: TTableParam;
  vWhere: string;
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add('select * from ' + FTableName);
  FQuery.SQL.Add('where');
  vWhere := EmptyStr;
  for vParam in AParams do
  begin
    if not vWhere.Trim.IsEmpty then
    begin
      vWhere := vWhere + ' and' +chr(13);
    end;
    vWhere := vWhere + '  ' + UpperCase(vParam.FieldName) + ' = ' + QuotedStr(vParam.FieldValue);
  end;
  if not vWhere.Trim.IsEmpty then
  begin
    FQuery.SQL.Add(vWhere);
  end;
  FQuery.Open;
end;

procedure TModelTable.Find(const AID: string);
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add('select * from ' + FTableName);
  FQuery.SQL.Add('where');
  FQuery.SQL.Add('  ID = ' + QuotedStr(AID));
  FQuery.Open;
end;

function TModelTable.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TModelTable.Open(const AWhere: string = '');
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add('select * from ' + FTableName);
  if not AWhere.Trim.IsEmpty then
  begin
    FQuery.SQL.Add('where');
    FQuery.SQL.Add(AWhere);
  end;
  FQuery.Open;
end;

end.
