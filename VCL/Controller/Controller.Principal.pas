unit Controller.Principal;

interface

uses
  View.Principal, Model.Interfaces, Model.Types, System.Generics.Collections,
  Vcl.ComCtrls;

type
  TControllerPrincipal = class
  private
    FView: TViewPrincipal;
    FModelServer: IModelTable;
    FModelDataBase: IModelTable;

    FServers: TDictionary<TTreeNode, TServer>;
    FDatabases: TDictionary<TTreeNode, TDataBase>;

  public
    constructor Create(const AView: TViewPrincipal);
    destructor Destroy; override;
    procedure FillList;
    procedure NewServer;
    procedure DeleteServer(const ATreeNode: TTreeNode);
    procedure NewDataBase(const ATreeNode: TTreeNode);
    procedure DeleteDataBase(const ATreeNode: TTreeNode);
    procedure ShowDataBase(const ATreeNode: TTreeNode);
  end;

var
  ControllerPrincipal: TControllerPrincipal;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils, Controller.Interfaces,
  Controller.Factory;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AView: TViewPrincipal);
begin
  FView := AView;
  FModelServer := TModelTablefactory.New('TSERVER');
  FModelDataBase := TModelTablefactory.New('TDATABASE');
  FServers := TDictionary<TTreeNode, TServer>.Create;
  FDatabases := TDictionary<TTreeNode, TDataBase>.Create;
end;

procedure TControllerPrincipal.NewDataBase(const ATreeNode: TTreeNode);
var
  vServer: TServer;
  vNode: TTreeNode;
  vName: string;
  vPath: string;
begin
  if FServers.TryGetValue(ATreeNode, vServer) then
  begin
    vName := InputBox('Database', 'Nome', 'Meu banco');
    vPath := InputBox('Database', 'Local', 'E:\DataBases\[pasta]\');
    FModelDataBase.DataSet.Append;
    FModelDataBase.DataSet.FieldByName('ID').AsString := TGUID.NewGuid.ToString;
    FModelDataBase.DataSet.FieldByName('ID_SERVER').AsString := vServer.ID;
    FModelDataBase.DataSet.FieldByName('NAME').AsString := vName;
    FModelDataBase.DataSet.FieldByName('PATH').AsString := vPath;
    FModelDataBase.DataSet.Post;
    FModelDataBase.ApplyUpdates;
  end;
end;

procedure TControllerPrincipal.NewServer;
var
  vName: string;
  vIP: string;
begin
  vName := InputBox('Novo Server', 'Nome', 'Localhost');
  vIP := InputBox('Novo Server', 'IP', '127.0.0.1');
  FModelServer.DataSet.Append;
  FModelServer.DataSet.FieldByName('ID').AsString := TGUID.NewGuid.ToString;
  FModelServer.DataSet.FieldByName('NAME').AsString := vName;
  FModelServer.DataSet.FieldByName('IP').AsString := vIP;
  FModelServer.DataSet.Post;
  FModelServer.ApplyUpdates;
end;

procedure TControllerPrincipal.ShowDataBase(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
  vController: IControllerDataBase;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    vController := TControllerFactory.DataBase(vDataBase);
    vController.Show;
  end;
end;

procedure TControllerPrincipal.DeleteDataBase(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    FModelDataBase.Delete(vDataBase.ID);
  end;
end;

procedure TControllerPrincipal.DeleteServer(const ATreeNode: TTreeNode);
var
  vServer: TServer;
begin
  if FServers.TryGetValue(ATreeNode, vServer) then
  begin
    FModelServer.Delete(vServer.ID);
    FModelDataBase.Open('ID_SERVER = ' + QuotedStr(vServer.ID));
    while not FModelDataBase.DataSet.Eof do
    begin
      FModelDataBase.DataSet.Delete;
    end;
  end;
end;

destructor TControllerPrincipal.Destroy;
begin
  FServers.Clear;
  FServers.Free;

  FDatabases.Clear;
  FDatabases.Free;

  inherited;
end;

procedure TControllerPrincipal.FillList;
  procedure AddDataBasesToTree(const ATreeNode: TTreeNode);
  var
    vServer: TServer;
    vNode: TTreeNode;
    vItem: TTreeNode;
  begin
    if FServers.TryGetValue(ATreeNode, vServer) then
    begin
      FModelDataBase.Open('ID_SERVER = ' + QuotedStr(vServer.ID));
      while not FModelDataBase.DataSet.Eof do
      begin
        vItem := FView.TreeView1.Items.AddChild(ATreeNode, FModelDataBase.DataSet.FieldByName('NAME').AsString);

        FDatabases.Add(
          vItem,
          TDataBase.Create(
            FModelDataBase.DataSet.FieldByName('ID').AsString,
            FModelDataBase.DataSet.FieldByName('NAME').AsString,
            FModelDataBase.DataSet.FieldByName('PATH').AsString,
            FModelDataBase.DataSet.FieldByName('USERNAME').AsString,
            FModelDataBase.DataSet.FieldByName('PASSWORD').AsString,
            vServer
          )
        );

        FModelDataBase.DataSet.Next;
      end;
    end;
  end;
var
  vItem: TTreeNode;
begin

  FServers.Clear;
  FServers.TrimExcess;

  FView.TreeView1.Items.Clear;
  FModelServer.Open;

  FView.TreeView1.Items.BeginUpdate;
  try
    while not FModelServer.DataSet.Eof do
    begin
      vItem := FView.TreeView1.Items.Add(nil, FModelServer.DataSet.FieldByName('IP').AsString + ' | ' + FModelServer.DataSet.FieldByName('NAME').AsString);
      FServers.Add(
        vItem,
        TServer.Create(
          FModelServer.DataSet.FieldByName('ID').AsString,
          FModelServer.DataSet.FieldByName('Name').AsString,
          FModelServer.DataSet.FieldByName('IP').AsString
        )
      );

      AddDataBasesToTree(vItem);

      FModelServer.DataSet.Next;
    end;
  finally
    FView.TreeView1.Items.EndUpdate;
  end;
end;

end.
