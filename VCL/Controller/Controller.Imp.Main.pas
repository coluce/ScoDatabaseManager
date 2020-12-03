unit Controller.Imp.Main;

interface

uses
  View.Principal, Model.Interfaces, Model.Types, System.Generics.Collections,
  Vcl.ComCtrls, Controller.Interfaces;

type
  TControllerMain = class(TInterfacedObject, IControllerMain)
  private
    FView: TViewMain;
    FModelServer: IModelTable;
    FModelDataBase: IModelTable;

    FServers: TDictionary<TTreeNode, TServer>;
    FDatabases: TDictionary<TTreeNode, TDataBase>;

    procedure SetStatusBar(const AServer, APath: string);
    procedure EditServer;
    procedure EditDataBase;
    procedure UnregisterServer;
    procedure UnregisterDataBase;
    procedure RegisterDatabase;
    procedure RegisterServer;
  public
    constructor Create(const AView: TViewMain);
    destructor Destroy; override;
    procedure FindInUse;
    procedure FillList;
    procedure ShowDataBase;
    procedure ExportToDrive;
    procedure PreparePopUp;
    procedure CallLayoutManager;
    procedure CallBackupManager;
    procedure CallEdit;
    procedure CallUnregister;
    procedure CallRegister;
  end;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils,
  Controller.Factory, View.Server, View.Database.Register, System.UITypes;

{ TControllerPrincipal }

constructor TControllerMain.Create(const AView: TViewMain);
begin
  FView := AView;
  FModelServer := TModelFactory.Table('TSERVER');
  FModelDataBase := TModelFactory.Table('TDATABASE');
  FServers := TDictionary<TTreeNode, TServer>.Create;
  FDatabases := TDictionary<TTreeNode, TDataBase>.Create;
end;

procedure TControllerMain.RegisterDatabase;
var
  vServer: TServer;
  vNode: TTreeNode;
  vView: TViewRegisterDatabase;
begin
  if FView.TreeView1.Selected.Level = 0 then
  begin
    if FServers.TryGetValue(FView.TreeView1.Selected, vServer) then
    begin
      vView := TViewRegisterDatabase.Create(nil);
      try
        vView.EditNome.Text := 'Meu banco';
        vView.EditLocal.Text := 'E:\DataBases\[pasta]\';
        vView.EditUserName.Text := 'sysdba';
        vView.EditPassword.Text := 'masterkey';
        vView.ShowModal;
        if vView.Resultado = mrOK then
        begin
          FModelDataBase.DataSet.Append;
          FModelDataBase.DataSet.FieldByName('ID_SERVER').AsString := vServer.ID;
          FModelDataBase.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
          FModelDataBase.DataSet.FieldByName('PATH').AsString := vView.EditLocal.Text;
          FModelDataBase.DataSet.FieldByName('USERNAME').AsString := vView.EditUserName.Text;
          FModelDataBase.DataSet.FieldByName('PASSWORD').AsString := vView.EditPassword.Text;
          FModelDataBase.DataSet.Post;
        end;
      finally
        vView.Free;
      end;
    end;
  end;
end;

procedure TControllerMain.RegisterServer;
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

procedure TControllerMain.SetStatusBar(const AServer, APath: string);
begin
  FView.StatusBar1.Panels[0].Text := AServer;
  FView.StatusBar1.Panels[1].Text := APath;
end;

procedure TControllerMain.ShowDataBase;
var
  vDataBase: TDataBase;
  vController: IControllerView;
begin
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      vController := TControllerFactory.DataBaseData(vDataBase);
      vController.Show;
    end;
  end;
end;

procedure TControllerMain.UnregisterDataBase;
var
  vDataBase: TDataBase;
begin
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      FModelDataBase.Delete(vDataBase.ID);
    end;
  end;
end;

procedure TControllerMain.UnregisterServer;
var
  vServer: TServer;
begin
  if FView.TreeView1.Selected.Level = 0 then
  begin
    if FServers.TryGetValue(FView.TreeView1.Selected, vServer) then
    begin
      FModelServer.Delete(vServer.ID);
      FModelDataBase.Open('ID_SERVER = ' + QuotedStr(vServer.ID));
      while not FModelDataBase.DataSet.Eof do
      begin
        FModelDataBase.DataSet.Delete;
      end;
    end;
  end;
end;

destructor TControllerMain.Destroy;
begin
  FServers.Clear;
  FServers.Free;

  FDatabases.Clear;
  FDatabases.Free;

  inherited;
end;

procedure TControllerMain.EditDataBase;
var
  vDataBase: TDataBase;
  vView: TViewRegisterDatabase;
begin
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      FModelDataBase.Find(vDataBase.ID);
      if not FModelDataBase.DataSet.IsEmpty then
      begin
        vView := TViewRegisterDatabase.Create(nil);
        try
          vView.EditNome.Text     := FModelDataBase.DataSet.FieldByName('NAME').AsString;
          vView.EditLocal.Text    := FModelDataBase.DataSet.FieldByName('PATH').AsString;
          vView.EditUserName.Text := FModelDataBase.DataSet.FieldByName('USERNAME').AsString;
          vView.EditPassword.Text := FModelDataBase.DataSet.FieldByName('PASSWORD').AsString;
          vView.ShowModal;
          if vView.Resultado = mrOK then
          begin
            FModelDataBase.DataSet.Edit;
            FModelDataBase.DataSet.FieldByName('NAME').AsString     := vView.EditNome.Text;
            FModelDataBase.DataSet.FieldByName('PATH').AsString     := vView.EditLocal.Text;
            FModelDataBase.DataSet.FieldByName('USERNAME').AsString := vView.EditUserName.Text;
            FModelDataBase.DataSet.FieldByName('PASSWORD').AsString := vView.EditPassword.Text;
            FModelDataBase.DataSet.Post;
          end;
        finally
          vView.Free;
        end;
      end;
    end;
  end;
end;

procedure TControllerMain.EditServer;
var
  vServer: TServer;
  vView: TViewServer;
begin
  if FView.TreeView1.Selected.Level = 0 then
  begin
    if FServers.TryGetValue(FView.TreeView1.Selected, vServer) then
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
end;

procedure TControllerMain.ExportToDrive;
var
  vDataBase: TDataBase;
  vController: IControllerView;
begin
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      vController := TControllerFactory.Exportini(vDataBase);
      vController.Show;
    end;
  end;
end;

procedure TControllerMain.FillList;

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

procedure TControllerMain.FindInUse;

  function GetLastID: string;
  var
    vControllerParam: IControllerParam;
  begin
    vControllerParam := TControllerFactory.ParamManager;
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

procedure TControllerMain.PreparePopUp;
begin
  FView.acnPopupMenuRegistrarBanco.Visible := FView.TreeView1.Selected.Level = 0;
  FView.acnPopupMenuExport.Visible         := FView.TreeView1.Selected.Level = 1;
  FView.acnPopupMenuShowData.Visible       := FView.TreeView1.Selected.Level = 1;
  FView.acnPopupMenuBackup.Visible         := FView.TreeView1.Selected.Level = 1;
end;

procedure TControllerMain.CallBackupManager;
var
  vController: IControllerView;
  vDataBase: TDataBase;
begin
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      if not vDataBase.Server.IP.Equals('127.0.0.1') then
      begin
        raise Exception.Create('Backup apenas para IP "127.0.0.1"');
      end;

      vController := TControllerFactory.DataBaseBackup(vDataBase);
      vController.Show;
    end;
  end;
end;

procedure TControllerMain.CallEdit;
begin
  case FView.TreeView1.Selected.Level of
    0: Self.EditServer;
    1: Self.EditDataBase;
  end;
  Self.FillList;
end;

procedure TControllerMain.CallLayoutManager;
var
  vController: IControllerView;
begin
  vController := TControllerFactory.LayoutManager;
  vController.Show;
end;

procedure TControllerMain.CallRegister;
begin
  case FView.TreeView1.Selected.Level of
    0: Self.RegisterServer;
    1: Self.RegisterDatabase;
  end;
  Self.FillList;
end;

procedure TControllerMain.CallUnregister;
begin
  case FView.TreeView1.Selected.Level of
    0: Self.UnregisterServer;
    1: Self.UnregisterDataBase;
  end;
  Self.FillList;
end;

end.
