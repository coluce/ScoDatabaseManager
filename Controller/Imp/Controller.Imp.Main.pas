unit Controller.Imp.Main;

interface

uses
  View.Main, Model.Interfaces, Model.Types, System.Generics.Collections,
  Vcl.ComCtrls, Controller.Interfaces, Vcl.Forms;

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
    function UnregisterServer: TTreeNode;
    function UnregisterDataBase: TTreeNode;
    procedure RegisterDatabase;
    procedure RegisterServer;
  public
    constructor Create(const AView: TForm);
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
    procedure CallParamManager;
  end;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils,
  Controller.Factory, View.Server, View.Database.Register, System.UITypes,
  Data.DB;

{ TControllerPrincipal }

constructor TControllerMain.Create(const AView: TForm);
begin
  FView := AView as TViewMain;
  FModelServer := TModelFactory.Table('TSERVER');
  FModelDataBase := TModelFactory.Table('TDATABASE');
  FServers := TDictionary<TTreeNode, TServer>.Create;
  FDatabases := TDictionary<TTreeNode, TDataBase>.Create;
end;

procedure TControllerMain.RegisterDatabase;
var
  LServer: TServer;
  LView: TViewRegisterDatabase;
begin
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  if FView.TreeView1.Selected.Level = 0 then
  begin
    if FServers.TryGetValue(FView.TreeView1.Selected, LServer) then
    begin
      LView := TViewRegisterDatabase.Create(nil);
      try
        LView.EditNome.Text := 'Meu banco';
        LView.EditLocal.Text := 'E:\DataBases\[pasta]\';
        LView.ShowModal;
        if LView.Resultado = mrOK then
        begin
          FModelDataBase.DataSet.Append;
          FModelDataBase.DataSet.FieldByName('ID_SERVER').AsString := LServer.ID;
          FModelDataBase.DataSet.FieldByName('NAME').AsString := LView.EditNome.Text;
          FModelDataBase.DataSet.FieldByName('DATABASE_FILE').AsString := LView.EditLocal.Text;
          FModelDataBase.DataSet.FieldByName('BACKUP_FOLDER').AsString := LView.edtBackupFolder.Text;
          FModelDataBase.DataSet.Post;
        end;
      finally
        LView.Free;
      end;
    end;
  end;
end;

procedure TControllerMain.RegisterServer;
var
  LView: TViewServer;
begin
  LView := TViewServer.Create(nil);
  try
    LView.EditNome.Text := 'Localhost';
    LView.EditLocal.Text := '127.0.0.1';
    LView.spnPort.Value := 3050;
    LView.edtUserName.Text := 'sysdba';
    LView.edtPassword.Text := 'masterkey';
    LView.ShowModal;
    if LView.Resultado = mrOK then
    begin
      FModelServer.DataSet.Append;
      FModelServer.DataSet.FieldByName('NAME').AsString := LView.EditNome.Text;
      FModelServer.DataSet.FieldByName('IP').AsString := LView.EditLocal.Text;
      FModelServer.DataSet.FieldByName('PORT').AsInteger := LView.spnPort.Value;
      FModelServer.DataSet.FieldByName('USERNAME').AsString := LView.edtUserName.Text;
      FModelServer.DataSet.FieldByName('PASSWORD').AsString := LView.edtPassword.Text;
      FModelServer.DataSet.Post;
    end;
  finally
    LView.Free;
  end;
end;

procedure TControllerMain.SetStatusBar(const AServer, APath: string);
begin
  FView.StatusBar1.Panels[0].Text := AServer;
  FView.StatusBar1.Panels[1].Text := APath;
end;

procedure TControllerMain.ShowDataBase;
var
  LDataBase: TDataBase;
  LController: IControllerView;
begin
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, LDataBase) then
    begin
      LController := TControllerFactory.Query(LDataBase);
      LController.Show;
    end;
  end;
end;

function TControllerMain.UnregisterDataBase: TTreeNode;
var
  LDataBase: TDataBase;
begin
  Result := nil;
  if not Assigned(FView.TreeView1.Selected) then
    Exit;
  if FView.TreeView1.Selected.Level = 1 then
    if FDatabases.TryGetValue(FView.TreeView1.Selected, LDataBase) then
    begin
      FModelDataBase.Delete(LDataBase.ID);
      Result := FView.TreeView1.Selected;
    end;
end;

function TControllerMain.UnregisterServer: TTreeNode;
var
  LServer: TServer;
begin
  Result := nil;
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  if FView.TreeView1.Selected.Level = 0 then
    if FServers.TryGetValue(FView.TreeView1.Selected, LServer) then
    begin
      FModelServer.Delete(LServer.ID);
      Result := FView.TreeView1.Selected;
      FModelDataBase.Open('ID_SERVER = ' + QuotedStr(LServer.ID));
      while not FModelDataBase.DataSet.Eof do
        FModelDataBase.DataSet.Delete;
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
  if not Assigned(FView.TreeView1.Selected) then
  begin
    Exit;
  end;
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      FModelDataBase.Find(vDataBase.ID);
      if not FModelDataBase.DataSet.IsEmpty then
      begin
        vView := TViewRegisterDatabase.Create(nil);
        try
          vView.EditNome.Text := FModelDataBase.DataSet.FieldByName('NAME').AsString;
          vView.EditLocal.Text := FModelDataBase.DataSet.FieldByName('DATABASE_FILE').AsString;
          vView.edtBackupFolder.Text := FModelDataBase.DataSet.FieldByName('BACKUP_FOLDER').AsString;
          vView.ShowModal;
          if vView.Resultado = mrOK then
          begin
            FModelDataBase.DataSet.Edit;
            FModelDataBase.DataSet.FieldByName('NAME').AsString := vView.EditNome.Text;
            FModelDataBase.DataSet.FieldByName('DATABASE_FILE').AsString := vView.EditLocal.Text;
            FModelDataBase.DataSet.FieldByName('BACKUP_FOLDER').AsString := vView.edtBackupFolder.Text;
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
  LServer: TServer;
  LView: TViewServer;
begin
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  if FView.TreeView1.Selected.Level = 0 then
  begin
    if FServers.TryGetValue(FView.TreeView1.Selected, LServer) then
    begin
      FModelServer.Find(LServer.ID);
      if not FModelServer.DataSet.IsEmpty then
      begin
        LView := TViewServer.Create(nil);
        try
          LView.EditNome.Text := FModelServer.DataSet.FieldByName('NAME').AsString;
          LView.EditLocal.Text := FModelServer.DataSet.FieldByName('IP').AsString;
          LView.spnPort.Value := FModelServer.DataSet.FieldByName('PORT').AsInteger;
          LView.edtUserName.Text := FModelServer.DataSet.FieldByName('USERNAME').AsString;
          LView.edtPassword.Text := FModelServer.DataSet.FieldByName('PASSWORD').AsString;
          LView.ShowModal;
          if LView.Resultado = mrOK then
          begin
            FModelServer.DataSet.Edit;
            FModelServer.DataSet.FieldByName('NAME').AsString := LView.EditNome.Text;
            FModelServer.DataSet.FieldByName('IP').AsString := LView.EditLocal.Text;
            FModelServer.DataSet.FieldByName('PORT').AsInteger := LView.spnPort.Value;
            FModelServer.DataSet.FieldByName('USERNAME').AsString := LView.edtUserName.Text;
            FModelServer.DataSet.FieldByName('PASSWORD').AsString := LView.edtPassword.Text;
            FModelServer.DataSet.Post;
          end;
        finally
          LView.Free;
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
  if not Assigned(FView.TreeView1.Selected) then
  begin
    Exit;
  end;
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      vController := TControllerFactory.ExportIniFile(vDataBase);
      vController.Show;
    end;
  end;
end;

procedure TControllerMain.FillList;

  procedure AddDataBasesToTree(const ATreeNode: TTreeNode);
  var
    LServer: TServer;
    LDataBaseNode: TTreeNode;
  begin

    if FServers.TryGetValue(ATreeNode, LServer) then
    begin
      FModelDataBase.Open('ID_SERVER = ' + QuotedStr(LServer.ID));
      while not FModelDataBase.DataSet.Eof do
      begin
        LDataBaseNode := FView.TreeView1.Items.AddChild(ATreeNode,
          FModelDataBase.DataSet.FieldByName('NAME').AsString);

        FDatabases.AddOrSetValue(
          LDataBaseNode,
          TDataBase.Create(
            FModelDataBase.DataSet.FieldByName('ID').AsString,
            FModelDataBase.DataSet.FieldByName('NAME').AsString,
            FModelDataBase.DataSet.FieldByName('DATABASE_FILE').AsString,
            FModelDataBase.DataSet.FieldByName('BACKUP_FOLDER').AsString,
            LServer
          )
        );
        FModelDataBase.DataSet.Next;
      end;
    end;
  end;

var
  LServerNode: TTreeNode;
begin

  FServers.Clear;
  FServers.TrimExcess;

  FModelServer.Open;

  FView.TreeView1.Items.Clear;
  FView.TreeView1.Items.BeginUpdate;
  try
    while not FModelServer.DataSet.Eof do
    begin
      LServerNode := FView.TreeView1.Items.Add(
        nil,
        FModelServer.DataSet.FieldByName('IP').AsString + ' | ' +
        FModelServer.DataSet.FieldByName('NAME').AsString
      );

      FServers.AddOrSetValue(
        LServerNode,
        TServer.Create(
          FModelServer.DataSet.FieldByName('ID').AsString,
          FModelServer.DataSet.FieldByName('NAME').AsString,
          FModelServer.DataSet.FieldByName('IP').AsString,
          FModelServer.DataSet.FieldByName('PORT').AsInteger,
          FModelServer.DataSet.FieldByName('USERNAME').AsString,
          FModelServer.DataSet.FieldByName('PASSWORD').AsString
        )
      );

      AddDataBasesToTree(LServerNode);

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
    vControllerParam := TControllerFactory.IniManager;
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
    SetStatusBar(
      FModelDataBase.DataSet.FieldByName('NAME').AsString,
      FModelDataBase.DataSet.FieldByName('DATABASE_FILE').AsString
    );
  end;
end;

procedure TControllerMain.PreparePopUp;
begin
  if not Assigned(FView.TreeView1.Selected) then
  begin
    Exit;
  end;
  FView.acnPopupMenuServerEditar.Visible := FView.TreeView1.Selected.Level = 0;
  FView.acnPopUpMenuDataBaseEdit.Visible := FView.TreeView1.Selected.Level = 1;
  FView.acnPopupMenuDataBaseRegister.Visible := FView.TreeView1.Selected.Level = 0;

  FView.acnPopupMenuExport.Visible := FView.TreeView1.Selected.Level = 1;
  FView.acnPopupMenuShowData.Visible := FView.TreeView1.Selected.Level = 1;
  FView.acnPopupMenuBackup.Visible := FView.TreeView1.Selected.Level = 1;
end;

procedure TControllerMain.CallBackupManager;
var
  vController: IControllerView;
  vDataBase: TDataBase;
begin
  if not Assigned(FView.TreeView1.Selected) then
  begin
    Exit;
  end;
  if FView.TreeView1.Selected.Level = 1 then
  begin
    if FDatabases.TryGetValue(FView.TreeView1.Selected, vDataBase) then
    begin
      if not vDataBase.Server.IP.Equals('127.0.0.1') then
      begin
        raise Exception.Create('Backup apenas para IP "127.0.0.1"');
      end;

      vController := TControllerFactory.BackupManager(vDataBase);
      vController.Show;
    end;
  end;
end;

procedure TControllerMain.CallEdit;
begin
  if not Assigned(FView.TreeView1.Selected) then
  begin
    Exit;
  end;
  case FView.TreeView1.Selected.Level of
    0:
      Self.EditServer;
    1:
      Self.EditDataBase;
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

procedure TControllerMain.CallParamManager;
var
  vController: IControllerView;
begin
  vController := TControllerFactory.ParamManager;
  vController.Show;
end;

procedure TControllerMain.CallRegister;
begin
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  case FView.TreeView1.Selected.Level of
    0: Self.RegisterServer;
    1: Self.RegisterDatabase;
  end;
  Self.FillList;
end;

procedure TControllerMain.CallUnregister;
var
  LNode: TTreeNode;
begin
  if not Assigned(FView.TreeView1.Selected) then
    Exit;

  LNode := nil;
  case FView.TreeView1.Selected.Level of
    0: LNode := Self.UnregisterServer;
    1: LNode := Self.UnregisterDataBase;
  end;
  if Assigned(LNode) then
    FView.TreeView1.Items.Delete(LNode);
end;

end.
