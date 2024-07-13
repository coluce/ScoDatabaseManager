unit Controller.Imp.Query;

interface

uses
  View.Query, Model.Types, Controller.Interfaces, Model.Interfaces,

  {TODO : criar classe model}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type

  TControllerQuery = class(TInterfacedObject, IControllerQuery)
  private
    FView: TViewQuery;
    FDataBase: TDataBase;
    FConnection: IModelConnection;
    FQuery: TFDQuery;
    FParams: TFDMemTable;
    FModelHistory: IModelTable;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);

    procedure UpdateActions;
    procedure UpdateTabs;
    procedure UpdateStatusBar;
    procedure UpdateTableList;
    procedure UpdateQueryEditor;
    procedure LogAdd(const AMessage: string);
    procedure PrepareScreen;
    function PrepareQuery: boolean;
  public
    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure FillSQLFromTreeView;
    procedure FillTableNames;
    procedure UpdateToogleColor;
    procedure ToogleSwitchClick;
    procedure ExecuteQuery;
    procedure ExportData;
    procedure ImportData;
    procedure RegisterHistoryQuery(const AQuery: string);
    procedure SelectHistoryQuery;
    procedure SetDataBaseInUse;

    // published
    property Connected: boolean read GetConnected write SetConnected;
  end;

implementation

uses
  System.Classes, Model.Factory, Vcl.ComCtrls, System.SysUtils, Vcl.Graphics,
  System.IOUtils, System.Types, Data.DB, View.Query.Param, System.UITypes,
  Controller.Factory;

{ TControllerDataBase }

constructor TControllerQuery.Create(const ADataBase: TDataBase);

  procedure PrepareQuery;
  begin
    FConnection := TModelFactory.Firebird(FDataBase);
    FQuery := TFDQuery.Create(nil);
    FQuery.Connection := FConnection.GetConnection;
    FView.DataSourceQuery.DataSet := FQuery;
    FView.MemoLog.Clear;
  end;

  procedure PrepareHistory;
  begin
    FModelHistory := TModelFactory.Table('TQUERY_HISTORY');

    FView.DataSourceHistory.DataSet := FModelHistory.DataSet;
    FView.GridHistory.Columns.Clear;
    FView.GridHistory.Columns.Add.Field :=
      FModelHistory.DataSet.FieldByName('DATA');
  end;

begin
  FDataBase := ADataBase;
  FView := TViewQuery.Create(Self);
  FView.Caption := FDataBase.Name;

  FParams := TFDMemTable.Create(nil);
  FParams.FieldDefs.Add('KEY', ftString, 50);
  FParams.FieldDefs.Add('VALUE', ftString, 1000);
  FParams.CreateDataSet;

  FParams.FieldByName('KEY').DisplayWidth := 10;
  FParams.FieldByName('VALUE').DisplayWidth := 30;

  PrepareQuery;
  PrepareHistory;

end;

destructor TControllerQuery.Destroy;
begin
  FParams.Free;
  FQuery.Free;
  FView.Free;
  inherited;
end;

procedure TControllerQuery.ExecuteQuery;
var
  vQuery: string;
begin
  if not FConnection.GetConnection.Connected then
    Exit;

  vQuery := FView.synSource.SelText.Trim;
  if vQuery.IsEmpty then
    vQuery := FView.synSource.Text;

  FQuery.Close;

  if not vQuery.Trim.IsEmpty then
  begin
    FQuery.SQL.Text := vQuery;

    if not Self.PrepareQuery then
    begin
      Exit;
    end;

    try
      if UpperCase(vQuery).Contains('UPDATE') or UpperCase(vQuery)
        .Contains('DELETE') or UpperCase(vQuery).Contains('CREATE') or
        UpperCase(vQuery).Contains('ALTER') or UpperCase(vQuery)
        .Contains('DROP') or UpperCase(vQuery).Contains('INSERT') then
      begin
        FQuery.ExecSQL;
        LogAdd('Exec SQL: ' + FQuery.RowsAffected.ToString);
        FView.PageControlMain.ActivePage := FView.TabSheetLog;
      end
      else
      begin
        FQuery.Open;
        LogAdd('Select SQL: ' + FQuery.RecordCount.ToString);
        FView.PageControlMain.ActivePage := FView.TabSheetResult;
      end;

      Self.RegisterHistoryQuery(vQuery);

    except
      on E: Exception do
      begin
        LogAdd('Erro: ' + E.Message);
        FView.PageControlMain.ActivePage := FView.TabSheetLog;
      end;
    end;

  end;

end;

procedure TControllerQuery.ExportData;
begin
  if FQuery.Active then
  begin
    if FQuery.RecordCount > 0 then
    begin
      if FView.SaveDataDialog.Execute then
      begin
        FQuery.SaveToFile(FView.SaveDataDialog.FileName,
          TFDStorageFormat.sfXML);
      end;
    end;
  end;
end;

procedure TControllerQuery.FillSQLFromTreeView;
begin
  if FView.TreeViewTabelas.Selected.Level = 0 then
  begin
    FView.synSource.Clear;
    FView.synSource.Lines.Add('select');
    FView.synSource.Lines.Add('  *');
    FView.synSource.Lines.Add('from');
    FView.synSource.Lines.Add('  ' + FView.TreeViewTabelas.Selected.Text);
  end;
end;

procedure TControllerQuery.FillTableNames;

  procedure FillTriggers(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Triggers');
    vNodeTitulo.ImageIndex := 3;
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda')
      .ImageIndex := 3;
  end;

  procedure FillContraints(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Constraints');
    vNodeTitulo.ImageIndex := 1;
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda')
      .ImageIndex := 1;
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
      FConnection.GetConnection.GetFieldNames('', '', ATableName, '', vFields);
      for vField in vFields do
      begin
        FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, vField)
          .ImageIndex := 4;;
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
    FConnection.GetConnection.GetTableNames('', '', '', vList);

    FView.SynSQLSyn1.TableNames := vList;

    FView.TreeViewTabelas.Items.Clear;
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

function TControllerQuery.GetConnected: boolean;
begin
  Result := FConnection.Active;
end;

procedure TControllerQuery.ImportData;
var
  vQueryOrigem: TFDMemTable;
  vField: TField;
begin
  if FQuery.Active then
  begin
    if FView.OpenDataDialog.Execute then
    begin
      vQueryOrigem := TFDMemTable.Create(nil);
      try
        vQueryOrigem.LoadFromFile(FView.SaveDataDialog.FileName,
          TFDStorageFormat.sfXML);

        vQueryOrigem.First;
        while not vQueryOrigem.Eof do
        begin
          FQuery.Append;
          for vField in FQuery.Fields do
          begin
            if Assigned(vQueryOrigem.FindField(vField.FieldName)) then
            begin
              try
                vField.Value := vQueryOrigem.FieldByName
                  (vField.FieldName).Value;
              except
                on E: Exception do
                begin
                  vField.Clear;
                  LogAdd('Não foi possivel importar o campo: ' +
                    vField.FieldName);
                  LogAdd(E.Message);
                end;
              end;
            end
            else
            begin
              LogAdd('Campo ' + vField.FieldName + ' não encontrado!');
            end;
          end;
          FQuery.Post;
        end;

      finally
        vQueryOrigem.Free;
      end;
    end;
  end;
end;

procedure TControllerQuery.LogAdd(const AMessage: string);
begin
  FView.MemoLog.Lines.Add(AMessage);
end;

function TControllerQuery.PrepareQuery: boolean;
var
  vView: TViewQueryParam;
  I: Integer;
begin
  if FQuery.Params.Count = 0 then
  begin
    Result := True;
    Exit;
  end;

  vView := TViewQueryParam.Create(nil);
  try
    vView.DataSourceParam.DataSet := FParams;
    FParams.EmptyDataSet;
    for I := 0 to FQuery.Params.Count - 1 do
    begin
      FParams.Append;
      FParams.FieldByName('KEY').AsString := FQuery.Params[I].Name;
      FParams.Post;
    end;

    Result := vView.ShowModal = mrOK;

    if Result then
    begin
      if FParams.State in dsEditModes then
      begin
        FParams.Post;
      end;
      for I := 0 to FQuery.Params.Count - 1 do
      begin
        FParams.RecNo := I + 1;
        FQuery.Params[I].Value := FParams.FieldByName('VALUE').AsVariant;
      end;
    end;

  finally
    vView.Free;
  end;
end;

procedure TControllerQuery.PrepareScreen;
begin
  Self.UpdateStatusBar;
  Self.UpdateToogleColor;
  Self.UpdateTabs;
  Self.UpdateActions;
  Self.UpdateQueryEditor;
  FModelHistory.Open;
  FView.PageControlMain.ActivePageIndex := 0;
end;

procedure TControllerQuery.RegisterHistoryQuery(const AQuery: string);
begin
  FModelHistory.DataSet.Append;
  FModelHistory.DataSet.FieldByName('DATA').AsDateTime := Now;
  FModelHistory.DataSet.FieldByName('QUERY').AsString := AQuery;
  FModelHistory.DataSet.Post;
end;

procedure TControllerQuery.SelectHistoryQuery;
begin
  FView.synSource.Clear;
  FView.synSource.Text := FModelHistory.DataSet.FieldByName('QUERY').Value;
  FView.PageControlMain.ActivePage := FView.TabSheetQuery;
end;

procedure TControllerQuery.SetConnected(const Value: boolean);
begin
  FConnection.Active := Value;
  Self.UpdateStatusBar;
  Self.UpdateTabs;
  Self.UpdateActions;
  Self.UpdateTableList;
  Self.UpdateQueryEditor;
end;

procedure TControllerQuery.SetDataBaseInUse;
var
  vController: IControllerIniFile;
begin
  vController := TControllerFactory.ExportIniFile(FDataBase);
  vController.Show;
end;

procedure TControllerQuery.Show;
begin
  FView.Show;
  PrepareScreen;
end;

procedure TControllerQuery.ShowModal;
begin
  FView.ShowModal;
  PrepareScreen;
end;

procedure TControllerQuery.ToogleSwitchClick;
begin
  try
    Self.Connected := FView.ToggleSwitch1.IsOn;
  finally
    Self.UpdateToogleColor;
  end;
end;

procedure TControllerQuery.UpdateActions;
begin
  FView.acnQueryRun.Enabled := FConnection.GetConnection.Connected;
end;

procedure TControllerQuery.UpdateQueryEditor;
begin
  FView.synSource.Enabled := FConnection.GetConnection.Connected;
end;

procedure TControllerQuery.UpdateStatusBar;
begin
  FView.StatusBar1.Panels[1].Text := FDataBase.Server.IP + ':' + FDataBase.DatabaseFile;
  if FConnection.Active then
  begin
    FView.StatusBar1.Panels[0].Text := 'Conectado';
  end
  else
  begin
    FView.StatusBar1.Panels[0].Text := 'Desconectado';
  end;
end;

procedure TControllerQuery.UpdateTableList;
begin
  if FConnection.GetConnection.Connected then
  begin
    Self.FillTableNames;
  end
  else
  begin
    FView.TreeViewTabelas.Items.Clear;
  end;
end;

procedure TControllerQuery.UpdateTabs;
begin
  FView.TabSheetResult.TabVisible := FConnection.Active;
  FView.TabSheetLog.TabVisible := FConnection.Active;
  FView.TabSheetHistory.TabVisible := FConnection.Active;
end;

procedure TControllerQuery.UpdateToogleColor;
begin
  if FConnection.GetConnection.Connected then
  begin
    FView.ToggleSwitch1.FrameColor := clGreen;
    FView.ToggleSwitch1.ThumbColor := clGreen;
  end
  else
  begin
    FView.ToggleSwitch1.FrameColor := clRed;
    FView.ToggleSwitch1.ThumbColor := clRed;
  end;
end;

end.
