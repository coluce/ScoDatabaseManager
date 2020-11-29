unit Controller.DataBase;

interface

uses
  View.DataBase, Model.Types, Controller.Interfaces, Model.Interfaces,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type

  TControllerDataBase = class(TInterfacedObject, IControllerDataBase)
  private
    FView: TViewDataBase;
    FDataBase: TDataBase;
    FConnection: IModelConnection;
    FQuery: TFDQuery;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);

    procedure UpdateStatusBar;
    procedure LogAdd(const AMessage: string);

  public
    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure FillTableNames;
    procedure ExecuteQuery(const ASQL: string);

  published
    property Connected: boolean read GetConnected write SetConnected;
  end;

implementation

uses
  System.Classes, Model.Factory, Vcl.ComCtrls, System.SysUtils;

{ TControllerDataBase }

constructor TControllerDataBase.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewDatabase.Create(Self);
  FConnection := TModelConnectionFactory.Firebird(FDataBase);
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection.GetConnection;
  FView.DataSource1.DataSet := FQuery;
  FView.MemoLog.Clear;
end;

destructor TControllerDataBase.Destroy;
begin
  FQuery.Free;
  FView.Free;
  inherited;
end;

procedure TControllerDataBase.ExecuteQuery(const ASQL: string);
begin
  if not FConnection.GetConnection.Connected then
    Exit;

  FQuery.Close;

  if not ASQL.Trim.IsEmpty then
  begin
    FQuery.SQL.Text := ASQL;
    if
      UpperCase(ASQL).Contains('UPDATE') or
      UpperCase(ASQL).Contains('DELETE') or
      UpperCase(ASQL).Contains('CREATE') or
      UpperCase(ASQL).Contains('ALTER') or
      UpperCase(ASQL).Contains('DROP') or
      UpperCase(ASQL).Contains('INSERT')
    then
    begin
      FQuery.ExecSQL;
      LogAdd('Exec SQL: ' + FQuery.RowsAffected.ToString);
    end
    else
    begin
      FQuery.Open;
      LogAdd('Select SQL: ' + FQuery.RecordCount.ToString);
    end;

  end;

end;

procedure TControllerDataBase.FillTableNames;

  procedure FillTriggers(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Triggers');
    vNodeTitulo.ImageIndex := 3;
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda').ImageIndex := 3;
  end;

  procedure FillContraints(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Constraints');
    vNodeTitulo.ImageIndex := 1;
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda').ImageIndex := 1;
  end;

  procedure FillFields(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
    vFields: TStrings;
    vField: string;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Fields');
    vNodeTitulo.ImageIndex := 4;
    vFields := TStringList.Create;
    try
      FConnection.GetConnection.GetFieldNames('','', ATableName, '', vFields);
      for vField in vFields do
      begin
        FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, vField).ImageIndex := 4;;
      end;
    finally
      vFields.Free;
    end;
  end;

var
  vList: TStringList;
  vTable: string;
  vNode: TTreeNode;
begin
  vList := TStringList.Create;
  try
    FConnection.GetConnection.GetTableNames('','','', vList);

    FView.SynSQLSyn1.TableNames := vList;

    FView.TreeViewTabelas.Items.BeginUpdate;
    try
      for vTable in vList do
      begin
        vNode := FView.TreeViewTabelas.Items.Add(nil, vTable);
        vNode.ImageIndex := 0;

        FillFields(vNode, vTable);
        FillTriggers(vNode, vTable);
        FillContraints(vNode, vTable);
      end;
    finally
      FView.TreeViewTabelas.Items.EndUpdate;
    end;

    LogAdd('Quantidade tabelas: ' + FView.TreeViewTabelas.Items.Count.ToString);

  finally
    vList.Free;
  end;
end;

function TControllerDataBase.GetConnected: boolean;
begin
  Result := FConnection.Active;
  UpdateStatusbar;
end;

procedure TControllerDataBase.LogAdd(const AMessage: string);
begin
  FView.MemoLog.Lines.Add(AMessage);
end;

procedure TControllerDataBase.SetConnected(const Value: boolean);
begin
  FConnection.Active := Value;
  UpdateStatusbar;
end;

procedure TControllerDataBase.Show;
begin
  FView.Show;
  UpdateStatusBar;
end;

procedure TControllerDataBase.ShowModal;
begin
  FView.ShowModal;
  UpdateStatusBar;
end;

procedure TControllerDataBase.UpdateStatusBar;
begin
  FView.StatusBar1.Panels[1].Text := FDataBase.Server.IP + ':' + FDataBase.Path;
  if FConnection.Active then
  begin
    FView.StatusBar1.Panels[0].Text := 'Conectado';
  end
  else
  begin
    FView.StatusBar1.Panels[0].Text := 'Desconectado';
  end;
end;

end.
