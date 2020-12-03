unit Controller.DataBase;

interface

uses
  View.DataBase.Manager, Model.Types, Controller.Interfaces, Model.Interfaces,


  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type

  TControllerDataBase = class(TInterfacedObject, IControllerDataBase)
  private
    FView: TViewDataBaseManager;
    FDataBase: TDataBase;
    FConnection: IModelConnection;
    FQuery: TFDQuery;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);

    procedure UpdateStatusBar;
    procedure LogAdd(const AMessage: string);
    procedure CreateBackupDirectory;
    function GetBackupDirectory: string;
    function GetBackupFileName: string;
    procedure PrepareScreen;
  public
    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure FillSQLFromTreeView;
    procedure FillTableNames;
    procedure FillBackupFiles;
    procedure UpdateToogleColor;
    procedure ToogleSwitchClick;
    procedure ExecuteQuery;

    procedure Backup;
    procedure Restore;

  published
    property Connected: boolean read GetConnected write SetConnected;
  end;

implementation

uses
  System.Classes, Model.Factory, Vcl.ComCtrls, System.SysUtils, Vcl.Graphics,
  System.IOUtils, System.Types;

const
  BACKUP_LEVEL_FULL: integer = 0;

{ TControllerDataBase }

procedure TControllerDataBase.Backup;
var
  vModelManager: IModelDatabaseManager;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  Self.Connected := False;

  CreateBackupDirectory;
  vModelManager := TModelFactory.DataBaseManager(FDataBase);
  vModelManager.Backup(TPath.Combine(GetBackupDirectory, GetBackupFileName), BACKUP_LEVEL_FULL);

  Self.FillBackupFiles;

end;

constructor TControllerDataBase.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewDataBaseManager.Create(Self);
  FView.Caption := FDatabase.Name;
  FConnection := TModelFactory.Firebird(FDataBase);
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection.GetConnection;
  FView.DataSource1.DataSet := FQuery;
  FView.MemoLog.Clear;
end;

procedure TControllerDataBase.CreateBackupDirectory;
var
  vDirectoryName: string;
begin
  vDirectoryName := GetBackupDirectory;
  if not DirectoryExists(vDirectoryName) then
  begin
    ForceDirectories(vDirectoryName);
  end;
  if not DirectoryExists(vDirectoryName) then
  begin
    raise Exception.Create('Não foi poss[ivel criar o diretório: ' + vDirectoryName);
  end;
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
      end
      else
      begin
        FQuery.Open;
        LogAdd('Select SQL: ' + FQuery.RecordCount.ToString);
      end;
    except on E: Exception do
      LogAdd('Erro: ' + E.Message);
    end;

  end;

end;

procedure TControllerDataBase.FillBackupFiles;
var
  vList: TStringDynArray;
  vDirectory: string;
  i: integer;
begin
  FView.TreeViewBackupFiles.Items.Clear;

  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  vDirectory := TPath.Combine(FDataBase.Path, 'backup');
  if DirectoryExists(vDirectory) then
  begin
    vList := TDirectory.GetFiles(vDirectory, '*.backup');
    for I := Low(vList) to High(vList) do
    begin
      FView.TreeViewBackupFiles.Items.Add(nil, ExtractFileName(vList[i]));
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

function TControllerDataBase.GetBackupDirectory: string;
begin
  Result := TPath.Combine(FDataBase.Path, 'backup');
end;

function TControllerDataBase.GetBackupFileName: string;
begin
  Result := 'backup_' + FormatDateTime('yyyy_mm_dd_hh_nn_ss_zzz', Now) + '.backup';
end;

function TControllerDataBase.GetConnected: boolean;
begin
  Result := FConnection.Active;
  Self.UpdateStatusbar;
end;

procedure TControllerDataBase.LogAdd(const AMessage: string);
begin
  FView.MemoLog.Lines.Add(AMessage);
end;

procedure TControllerDataBase.Restore;
var
  vModelManager: IModelDatabaseManager;
begin
  if not FDataBase.Server.IP.Equals('127.0.0.1') then
    Exit;

  Self.Connected := False;

  { TODO : apagar banco anterior }

  vModelManager := TModelFactory.DataBaseManager(FDataBase);
  vModelManager.Restore(FView.TreeViewBackupFiles.Selected.Text);
end;

procedure TControllerDataBase.PrepareScreen;
begin
  Self.UpdateStatusBar;
  Self.UpdateToogleColor;

  FView.PageControlMain.ActivePageIndex := 0;
  FView.tabManager.Visible := False;
  if FDataBase.Server.IP.Equals('127.0.0.1') then
  begin
    FView.tabManager.Visible := True;
    Self.FillBackupFiles;
  end;
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
