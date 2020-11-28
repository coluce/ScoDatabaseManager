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

    procedure SetStatusBar(const AServer, APath: string);

  public
    constructor Create(const AView: TViewPrincipal);
    destructor Destroy; override;
    procedure FillList;
    procedure RegisterServer;
    procedure UnregisterServer(const ATreeNode: TTreeNode);
    procedure RegisterDatabase(const ATreeNode: TTreeNode);
    procedure UnregisterDataBase(const ATreeNode: TTreeNode);
    procedure ShowDataBase(const ATreeNode: TTreeNode);
    procedure ExportToDrive(const ATreeNode: TTreeNode);
    procedure IrParaCadastroLayout;
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

procedure TControllerPrincipal.RegisterDatabase(const ATreeNode: TTreeNode);
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

procedure TControllerPrincipal.RegisterServer;
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

procedure TControllerPrincipal.SetStatusBar(const AServer, APath: string);
begin
  FView.StatusBar1.Panels[0].Text := AServer;
  FView.StatusBar1.Panels[1].Text := APath;
end;

procedure TControllerPrincipal.ShowDataBase(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
  vController: IController;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    vController := TControllerFactory.DataBase(vDataBase);
    vController.Show;
  end;
end;

procedure TControllerPrincipal.UnregisterDataBase(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    FModelDataBase.Delete(vDataBase.ID);
  end;
end;

procedure TControllerPrincipal.UnregisterServer(const ATreeNode: TTreeNode);
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

procedure TControllerPrincipal.ExportToDrive(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
  vController: IController;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    vController := TControllerFactory.Exportini(vDataBase);
    vController.Show;
  end;
end;

procedure TControllerPrincipal.FillList;

  function GetLastID: string;
  var
    vControllerParam: IControllerParam;
  begin
    vControllerParam := TControllerFactory.Param;
    Result := vControllerParam.GetParam('INI', 'LAST_IN', EmptyStr);
  end;

  procedure AddDataBasesToTree(const ATreeNode: TTreeNode);
  var
    vServer: TServer;
    vNode: TTreeNode;
    vItem: TTreeNode;
    vLastID: string;
  begin
    vLastID := GetLastID;

    if FServers.TryGetValue(ATreeNode, vServer) then
    begin
      FModelDataBase.Open('ID_SERVER = ' + QuotedStr(vServer.ID));
      while not FModelDataBase.DataSet.Eof do
      begin
        vItem := FView.TreeView1.Items.AddChild(ATreeNode, FModelDataBase.DataSet.FieldByName('NAME').AsString);
        vItem.ImageIndex := 1;

        if vLastID.Equals(FModelDataBase.DataSet.FieldByName('ID').AsString) then
        begin
          SetStatusBar(vServer.IP, FModelDataBase.DataSet.FieldByName('PATH').AsString);
        end;

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
      vItem.ImageIndex := 0;

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

procedure TControllerPrincipal.IrParaCadastroLayout;
var
  vController: IController;
begin
  vController := TControllerFactory.Layout;
  vController.Show;
end;

end.
