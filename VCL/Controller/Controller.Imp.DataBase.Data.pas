unit Controller.Imp.DataBase.Data;

interface

uses
  View.DataBase.Data, Model.Types, Controller.Interfaces, Model.Interfaces,

  { TODO : criar classe model }
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type

  TControllerDataBase = class(TInterfacedObject, IControllerDataBaseData)
  private
    FView: TViewDatabaseData;
    FDataBase: TDataBase;
    FConnection: IModelConnection;
    FQuery: TFDQuery;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);

    procedure UpdateStatusBar;
    procedure LogAdd(const AMessage: string);
    procedure PrepareScreen;
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

  published
    property Connected: boolean read GetConnected write SetConnected;
  end;

implementation

uses
  System.Classes, Model.Factory, Vcl.ComCtrls, System.SysUtils, Vcl.Graphics,
  System.IOUtils, System.Types, Data.DB;

{ TControllerDataBase }

constructor TControllerDataBase.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewDatabaseData.Create(Self);
  FView.Caption := FDatabase.Name;
  FConnection := TModelFactory.Firebird(FDataBase);
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

procedure TControllerDataBase.ExecuteQuery;
var
  vQuery: string;
begin
  if not FConnection.GetConnection.Connected then
    Exit;

  vQuery := FView.MemoQuery.SelText.Trim;

  if vQuery.IsEmpty then
  begin
    vQuery := FView.MemoQuery.Text;
  end;

  FQuery.Close;

  if not vQuery.Trim.IsEmpty then
  begin
    FQuery.SQL.Text := vQuery;
    try
      if
        UpperCase(vQuery).Contains('UPDATE') or
        UpperCase(vQuery).Contains('DELETE') or
        UpperCase(vQuery).Contains('CREATE') or
        UpperCase(vQuery).Contains('ALTER') or
        UpperCase(vQuery).Contains('DROP') or
        UpperCase(vQuery).Contains('INSERT')
      then
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
    except on E: Exception do
      begin
        LogAdd('Erro: ' + E.Message);
        FView.PageControlMain.ActivePage := FView.TabSheetLog;
      end;
    end;

  end;

end;

procedure TControllerDataBase.ExportData;
begin
  if FQuery.Active then
  begin
    if FQuery.RecordCount > 0 then
    begin
      if FView.SaveDataDialog.Execute then
      begin
        FQuery.SaveToFile(FView.SaveDataDialog.FileName, TFDStorageFormat.sfXML);
      end;
    end;
  end;
end;

procedure TControllerDataBase.FillSQLFromTreeView;
begin
  if FView.TreeViewTabelas.Selected.Level = 0 then
  begin
    FView.MemoQuery.Clear;
    FView.MemoQuery.Lines.Add('select');
    FView.MemoQuery.Lines.Add('  *');
    FView.MemoQuery.Lines.Add('from');
    FView.MemoQuery.Lines.Add('  ' + FView.TreeViewTabelas.Selected.Text);
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

function TControllerDataBase.GetConnected: boolean;
begin
  Result := FConnection.Active;
  Self.UpdateStatusbar;
end;

procedure TControllerDataBase.ImportData;
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
        vQueryOrigem.LoadFromFile(FView.SaveDataDialog.FileName, TFDStorageFormat.sfXML);

        vQueryOrigem.First;
        while not vQueryOrigem.Eof do
        begin
          FQuery.Append;
          for vField in FQuery.Fields do
          begin
            if Assigned(vQueryOrigem.FindField(vField.FieldName)) then
            begin
              try
                vField.Value := vQueryOrigem.FieldByName(vField.FieldName).Value;
              except on E: exception do
                begin
                  vField.Clear;
                  LogAdd('Não foi possivel importar o campo: ' + vField.FieldName);
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

procedure TControllerDataBase.LogAdd(const AMessage: string);
begin
  FView.MemoLog.Lines.Add(AMessage);
end;

procedure TControllerDataBase.PrepareScreen;
begin
  Self.UpdateStatusBar;
  Self.UpdateToogleColor;
  FView.PageControlMain.ActivePageIndex := 0;
end;

procedure TControllerDataBase.SetConnected(const Value: boolean);
begin
  FConnection.Active := Value;
  Self.UpdateStatusbar;
end;

procedure TControllerDataBase.Show;
begin
  FView.Show;
  PrepareScreen;
end;

procedure TControllerDataBase.ShowModal;
begin
  FView.ShowModal;
  PrepareScreen;
end;

procedure TControllerDataBase.ToogleSwitchClick;
begin
  try
    Self.Connected := FView.ToggleSwitch1.IsOn;
    if Self.Connected then
    begin
      Self.FillTableNames;
    end;
  finally
    Self.UpdateToogleColor;
  end;
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

procedure TControllerDataBase.UpdateToogleColor;
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
