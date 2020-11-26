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
    FServers: TDictionary<TTreeNode, TServer>;
  public
    constructor Create(const AView: TViewPrincipal);
    destructor Destroy; override;
    procedure FillList;
    procedure NewServer;
    procedure DeleteServer(const ATreeNode: TTreeNode);
  end;

var
  ControllerPrincipal: TControllerPrincipal;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AView: TViewPrincipal);
begin
  FView := AView;
  FModelServer := TModelTablefactory.New('TSERVER');
  FServers := TDictionary<TTreeNode, TServer>.Create;
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

procedure TControllerPrincipal.DeleteServer(const ATreeNode: TTreeNode);
var
  vServer: TServer;
begin
  if FServers.TryGetValue(ATreeNode, vServer) then
  begin
    FModelServer.Delete(vServer.ID);
    //FModelDataBase.Delete(vServer.ID);
  end;
end;

destructor TControllerPrincipal.Destroy;
begin
  FServers.Clear;
  FServers.Free;
  inherited;
end;

procedure TControllerPrincipal.FillList;
var
  vItem: TTreeNode;
begin

  FServers.Clear;
  FServers.TrimExcess;

  FView.TreeView1.Items.Clear;
  FModelServer.Open;

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
    FModelServer.DataSet.Next;
  end;

end;

end.
