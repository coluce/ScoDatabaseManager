unit Controller.Principal;

interface

uses
  View.Principal, Model.Interfaces, Model.Types, System.Generics.Collections,
  Vcl.ComCtrls, Controller.Interfaces;

type
  TControllerPrincipal = class(TInterfacedObject, IControllerPrincipal)
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
    procedure FindInUse;
    procedure FillList;
    procedure RegisterServer;
    procedure UnregisterServer(const ATreeNode: TTreeNode);
    procedure EditServer(const ATreeNode: TTreeNode);
    procedure RegisterDatabase(const ATreeNode: TTreeNode);
    procedure UnregisterDataBase(const ATreeNode: TTreeNode);
    procedure EditDataBase(const ATreeNode: TTreeNode);
    procedure ShowDataBase(const ATreeNode: TTreeNode);
    procedure ExportToDrive(const ATreeNode: TTreeNode);
    procedure IrParaCadastroLayout;
  end;

var
  ControllerPrincipal: TControllerPrincipal;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils,
  Controller.Factory, View.Server, View.Database.Register, System.UITypes;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AView: TViewPrincipal);
begin
  FView := AView;
  FModelServer := TModelFactory.Table('TSERVER');
  FModelDataBase := TModelFactory.Table('TDATABASE');
  FServers := TDictionary<TTreeNode, TServer>.Create;
  FDatabases := TDictionary<TTreeNode, TDataBase>.Create;
end;

procedure TControllerPrincipal.RegisterDatabase(const ATreeNode: TTreeNode);
var
  vServer: TServer;
  vNode: TTreeNode;
  vView: TViewRegisterDatabase;
begin
  if FServers.TryGetValue(ATreeNode, vServer) then
  begin
    vView := TViewRegisterDatabase.Create(nil);
    try
      vView.EditNome.Text := 'Meu banco';
      vView.EditLocal.Text := 'E:\DataBases\[pasta]\';
      vView.ShowModal;
      if vView.Resultado = mrOK then
      begin
        FModelDataBase.DataSet.Append;
        FModelDataBase.DataSet.FieldByName('ID_SERVER').AsString := vServer.ID;
        FModelDataBase.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
        FModelDataBase.DataSet.FieldByName('PATH').AsString := vView.EditLocal.Text;
        FModelDataBase.DataSet.Post;
      end;
    finally
      vView.Free;
    end;
  end;
end;

procedure TControllerPrincipal.RegisterServer;
var
  vView: TViewServer;
begin
  vView := TViewServer.Create(nil);
  try
    vView.EditNome.Text := 'Localhost';
    vView.EditLocal.Text := '127.0.0.1';
    vView.ShowModal;
    if vView.Resultado = mrOK then
    begin
      FModelServer.DataSet.Append;
      FModelServer.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
      FModelServer.DataSet.FieldByName('IP').AsString := vView.EditLocal.Text;
      FModelServer.DataSet.Post;
    end;
  finally
    vView.Free;
  end;
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

procedure TControllerPrincipal.EditDataBase(const ATreeNode: TTreeNode);
var
  vDataBase: TDataBase;
  vView: TViewRegisterDatabase;
begin
  if FDatabases.TryGetValue(ATreeNode, vDataBase) then
  begin
    FModelDataBase.Find(vDataBase.ID);
    if not FModelDataBase.DataSet.IsEmpty then
    begin
      vView := TViewRegisterDatabase.Create(nil);
      try
        vView.EditNome.Text := FModelDataBase.DataSet.FieldByName('NAME').AsString;
        vView.EditLocal.Text := FModelDataBase.DataSet.FieldByName('PATH').AsString;
        vView.ShowModal;
        if vView.Resultado = mrOK then
        begin
          FModelDataBase.DataSet.Edit;
          FModelDataBase.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
          FModelDataBase.DataSet.FieldByName('PATH').AsString := vView.EditLocal.Text;
          FModelDataBase.DataSet.Post;
        end;
      finally
        vView.Free;
      end;
    end;
  end;
end;

procedure TControllerPrincipal.EditServer(const ATreeNode: TTreeNode);
var
  vServer: TServer;
  vView: TViewServer;
begin
  if FServers.TryGetValue(ATreeNode, vServer) then
  begin
    FModelServer.Find(vServer.ID);
    if not FModelServer.DataSet.IsEmpty then
    begin
      vView := TViewServer.Create(nil);
      try
        vView.EditNome.Text := FModelServer.DataSet.FieldByName('NAME').AsString;
        vView.EditLocal.Text := FModelServer.DataSet.FieldByName('IP').AsString;
        vView.ShowModal;
        if vView.Resultado = mrOK then
        begin
          FModelServer.DataSet.Edit;
          FModelServer.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
          FModelServer.DataSet.FieldByName('IP').AsString := vView.EditLocal.Text;
          FModelServer.DataSet.Post;
        end;
      finally
        vView.Free;
      end;
    end;
  end;
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
        vItem.ImageIndex := 1;

        FDatabases.AddOrSetValue(
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

  FModelServer.Open;

  FView.TreeView1.Items.Clear;
  FView.TreeView1.Items.BeginUpdate;
  try
    while not FModelServer.DataSet.Eof do
    begin
      vItem := FView.TreeView1.Items.Add(nil, FModelServer.DataSet.FieldByName('IP').AsString + ' | ' + FModelServer.DataSet.FieldByName('NAME').AsString);
      vItem.ImageIndex := 0;

      FServers.AddOrSetValue(
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

procedure TControllerPrincipal.FindInUse;

  function GetLastID: string;
  var
    vControllerParam: IControllerParam;
  begin
    vControllerParam := TControllerFactory.Param;
    Result := vControllerParam.GetParam('INI', 'LAST_ID', EmptyStr);
  end;

var
  vLastID: string;
begin
  SetStatusBar(EmptyStr, EmptyStr);
  vLastID := GetLastID;
  FModelDataBase.Find(vLastID);
  if not FModelDataBase.DataSet.IsEmpty then
  begin
    SetStatusBar(FModelDataBase.DataSet.FieldByName('NAME').AsString, FModelDataBase.DataSet.FieldByName('PATH').AsString);
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
